-module(user_default).

-export([reload/0]).

-export([dbg/0]).
-export([dbg/1]).
-export([dbg/2]).
-export([dbg/3]).
-export([dbg/4]).

-export([tc_avg/4]).

-compile(inline).

%% Reload code
reload() ->
	LibExclude = base_lib_path(),
	Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, LibExclude) =:= 0],
	[shell_default:l(M) || M <- Modules].

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

tc_loop(_M, _F, _A, 0, List) ->
    List;
tc_loop(M, F, A, N, List) ->
    case timer:tc(M, F, A) of
        {_T, {'EXIT', Reason}} -> exit(Reason);
        {T, _Result} -> tc_loop(M, F, A, N - 1, [T|List])
    end.

%%%% Private Functions

run_command(CommandList) ->
	Result = os:cmd(string:join(CommandList, " ")),
	io:format("~s~n", [Result]).

dbgc(MFA)    -> dbg:ctp(MFA).
dbge(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tp(MFA, O).
dbgl(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tpl(MFA, O).
dbg_rt() -> x.

base_lib_path() ->
	KernAppPath = code:where_is_file("kernel.app"),
	string:substr(KernAppPath, 1, string:str(KernAppPath,"kernel") - 1).
