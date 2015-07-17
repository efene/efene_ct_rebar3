-module(rebar3_efene_ct).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, ct).
-define(DEPS, [{default, app_discovery}, {efene, compile}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},           % The 'user friendly' name of the task
            {module, ?MODULE},           % The module implementation of the task
            {namespace, efene},
            {bare, false},
            {deps, ?DEPS},               % The list of dependencies
            {example, "rebar efene ct"}, % How to use the plugin
            {opts, []},                  % list of options understood by the plugin
            {short_desc, "efene rebar3 common test plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    run_tests(State),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Private API
%% ===================================================================

compile(Path, DestPath, ErlOpts) ->
    io:format("Compiling ~s~n", [Path]),
    case efene:compile(Path, DestPath, ErlOpts) of
        {error, _}=Error ->
            efene:print_errors([Error], "errors");
        {error, Errors, Warnings} ->
            efene:print_errors(Errors, "errors"),
            efene:print_errors(Warnings, "warnings"),
            ok;
        {ok, CompileInfo} ->
            efene:print_errors(proplists:get_value(warnings, CompileInfo, []), "warnings"),
            ok;
        Other ->
            io:format("unknown result: ~p~n", [Other]),
            Other
    end.

run_tests(State) ->
    compile_sources(State).


compile_sources(State) ->
    Path = filename:join(rebar_state:dir(State), "test"),
    DestPath = filename:join(rebar_state:dir(State), "test"),
    ok = filelib:ensure_dir(filename:join(DestPath, "a")),
    Mods = find_source_files(Path),
    ErlOpts = rebar_utils:erl_opts(State),
    lists:foreach(fun (ModPath) ->
                          compile(ModPath, DestPath, ErlOpts)
                  end, Mods),
    ok.

find_source_files(Path) ->
    [filename:join(Path, Mod) || Mod <- filelib:wildcard("*.fn", Path)].
