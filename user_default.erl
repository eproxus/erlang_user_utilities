-module(user_default).

-export ([sync/0,make/0,git/1,reload/0,reload_then/1]).

-export([dbg/0]).
-export([dbg/1]).
-export([dbg/2]).
-export([dbg/3]).
-export([dbg/4]).

-compile(inline).

%% Reload code
reload() ->
	LibExclude = base_lib_path(),
	Modules = [M || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, LibExclude) =:= 0],
	[shell_default:l(M) || M <- Modules].

%% Reload code then exec F
reload_then(F) ->
	reload(),
	F().

%% Compiles all files in Emakefile and load into current shell.
sync() ->
	make:all([load]).

%% Run the make command in shell.
make() ->
	run_command(["make", "all"]).

%% Run git command in shell.
git(Command) ->
	CommandList = ["git", Command],
	run_command(CommandList).

dbg()                           -> dbg:tracer().

dbg(c)                          -> dbg:stop_clear();
dbg(M)                          -> dbge({M, '_', '_'}, []).

dbg(M, c)                       -> dbgc({M, '_', '_'});
dbg(M, r)                       -> dbge({M, '_', '_'}, dbg_rt());
dbg(M, l)                       -> dbgl({M, '_', '_'}, []);
dbg(M, lr)                      -> dbgl({M, '_', '_'}, dbg_rt());
dbg(M, O) when is_list(O)       -> dbge({M, '_', '_'}, O);
dbg(M, F) when is_atom(F)       -> dbge({M,   F, '_'}, []).

dbg(M, F, c)                    -> dbgc({M,   F, '_'});
dbg(M, F, l)                    -> dbgl({M,   F, '_'}, dbg_rt());
dbg(M, F, r)                    -> dbge({M,   F, '_'}, dbg_rt());
dbg(M, F, lr)                   -> dbgl({M,   F, '_'}, dbg_rt());
dbg(M, F, O) when is_list(O)    -> dbge({M,   F, '_'}, O);
dbg(M, F, A) when is_integer(A) -> dbge({M,   F,   A}, []).

dbg(M, F, A, c)                 -> dbgc({M,   F,   A});
dbg(M, F, A, r)                 -> dbge({M,   F,   A}, dbg_rt());
dbg(M, F, A, l)                 -> dbgl({M,   F,   A}, dbg_rt());
dbg(M, F, A, lr)                -> dbgl({M,   F,   A}, dbg_rt());
dbg(M, F, A, O)                 -> dbge({M,   F,   A}, O).

%%%% Private Functions

run_command(CommandList) ->
	Result = os:cmd(string:join(CommandList, " ")),
	io:format("~s~n", [Result]).

dbgc(MFA)    -> dbg:ctp(MFA).
dbge(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tp(MFA, O).
dbgl(MFA, O) -> dbg:tracer(), dbg:p(all, call), dbg:tpl(MFA, O).
dbg_rt() -> [{'_',[],[{return_trace}]}].

base_lib_path() ->
	KernAppPath = code:where_is_file("kernel.app"),
	string:substr(KernAppPath, 1, string:str(KernAppPath,"kernel") - 1).
