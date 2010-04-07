A collection of handy user utilities for the Erlang shell. Read [this blog post](http://medevyoujane.com/blog/2010/1/3/erlang-quick-tip-the-user_default-module.html) for background.

    sync()              %   compiles all files in Emakefile and load into
                        %     current shell
    reload()            %   reload loaded modules
    reload_then(Fun)    %   reload then exec a function
    git(String)         %   run git command
    tc_avg(M, F, A, N)  %   run timer:tc(M, F, A) N times and return range,
                        %     average and median results skipping the first
                        %     run.

Debugger helper functions:

    dbg()               %   start the debugger (done automatically for all
                        %     calls)
    dbg(c)              %   clear all traces

    dbg(M)              %   trace on module M
    dbg(M, O)           %   trace on module M with options O
    dbg(M, F)           %   trace on function M:F
    dbg(M, F, O)        %   trace on function M:F with options O
    dbg(M, F, A)        %   trace on function M:F(A)
    dbg(M, F, A, O)     %   trace on function M:F(A) with options O
    
The options for debugging are:

    c                   %   clear
    r                   %   show return trace (including exceptions).
    l                   %   trace on local functions
    lr                  %   trace on local functions with return trace 
                        %     (including expections)

Add the following to ~/.erlang to enable them:

    code:load_abs("/Path/To/erlang_user_utilities/user_default").



