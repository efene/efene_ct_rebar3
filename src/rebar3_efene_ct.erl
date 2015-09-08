-module(rebar3_efene_ct).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, ct).
-define(DEPS, [{default, app_discovery}]).

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
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         OutDir = rebar_app_info:out_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "test"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.fn\$"),

         CompileFun = fun(Source, Opts1) ->
                              ErlOpts = rebar_opts:erl_opts(Opts1),
                              compile_source(State, ErlOpts, Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

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

compile_source(_State, ErlOpts, Source, DestPath) ->
    NewDestPath = filename:join(DestPath, "test"),
    ok = filelib:ensure_dir(filename:join(NewDestPath, "a")),
    io:format("Compiling ~s~n", [Source]),
    compile(Source, NewDestPath, ErlOpts),
    ok.
