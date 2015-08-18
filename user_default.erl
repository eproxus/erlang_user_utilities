-module(user_default).

-include_lib("kernel/include/file.hrl").

-export([reload/0]).

-export([dbg/0]).
-export([dbg/1]).
-export([dbg/2]).
-export([dbg/3]).
-export([dbg/4]).

-export([tc_avg/4]).

-compile(inline).

%--- Code Reload --------------------------------------------------------------

reload() ->
	Lib = code:lib_dir(),
	[load(M) || {M, P} <- code:all_loaded(),
        is_list(P),        % Filter out 'preloaded' atoms
        is_prefix(Lib, P), % Is in lib dir?
        is_modified(M)     % Is it modified?
    ].

%--- Debugging ----------------------------------------------------------------
dbg()                           -> dbg:tracer().

dbg(c)                          -> dbg:stop_clear();
dbg(M)                          -> dbge({M, '_', '_'}, []).

dbg(M, c)                       -> dbgc({M, '_', '_'});
dbg(M, r)                       -> dbge({M, '_', '_'}, dbg_rt());
dbg(M, l)                       -> dbgl({M, '_', '_'}, []);
dbg(M, lr)                      -> dbgl({M, '_', '_'}, dbg_rt());
dbg(M, F) when is_atom(F)       -> dbge({M,   F, '_'}, []);
dbg(M, O)                       -> dbge({M, '_', '_'}, O).

dbg(M, F, c)                    -> dbgc({M,   F, '_'});
dbg(M, F, l)                    -> dbgl({M,   F, '_'}, dbg_rt());
dbg(M, F, r)                    -> dbge({M,   F, '_'}, dbg_rt());
dbg(M, F, lr)                   -> dbgl({M,   F, '_'}, dbg_rt());
dbg(M, F, A) when is_integer(A) -> dbge({M,   F,   A}, []);
dbg(M, F, O)                    -> dbge({M,   F, '_'}, O).

dbg(M, F, A, c)                 -> dbgc({M,   F,   A});
dbg(M, F, A, r)                 -> dbge({M,   F,   A}, dbg_rt());
dbg(M, F, A, l)                 -> dbgl({M,   F,   A}, dbg_rt());
dbg(M, F, A, lr)                -> dbgl({M,   F,   A}, dbg_rt());
dbg(M, F, A, O)                 -> dbge({M,   F,   A}, O).

%--- Benchmarking -------------------------------------------------------------

tc_avg(M, F, A, N) when N > 1 ->
    L = tl(lists:reverse(tc_loop(M, F, A, N, []))),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
              "Median: ~b mics~n"
              "Average: ~b mics~n",
              [Min, Max, Med, Avg]),
    Med.

%--- Private ------------------------------------------------------------------

tc_loop(_M, _F, _A, 0, List) ->
    List;
tc_loop(M, F, A, N, List) ->
    case timer:tc(M, F, A) of
        {_T, {'EXIT', Reason}} -> exit(Reason);
        {T, _Result} -> tc_loop(M, F, A, N - 1, [T|List])
    end.

dbgc(MFA)    -> dbg:ctp(MFA).
dbge(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tp(MFA, O).
dbgl(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tpl(MFA, O).
dbg_rt() -> x.

is_prefix(Prefix, String) -> string:str(String, Prefix) =:= 0.

is_modified(Module) ->
    Compile = proplists:get_value(compile, Module:module_info()),
    {Y, M, D, H, Mm, S} = proplists:get_value(time, Compile),
    Time = {{Y, M, D}, {H, Mm, S}},
    Source = proplists:get_value(source, Compile),
    case file:read_file_info(Source) of
        {ok, Info} when Info#file_info.type == regular ->
            Info#file_info.mtime > calendar:universal_time_to_local_time(Time);
        _ ->
            false
    end.

load(Module) ->
    case code:which(Module) of
        Path when is_list(Path) ->
            true = code:soft_purge(Module),
            File = string:sub_string(Path, 1, length(Path) - 5),
            {module, Module} = code:load_abs(File),
            Module;
        Other ->
            {not_loaded, Module, Other}
    end.
