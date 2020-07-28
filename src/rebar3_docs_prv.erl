-module(rebar3_docs_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, erl_doc).
-define(DEPS, [app_discovery]).


-type options() :: #{output_dir := file:filename()}.

%%==============================================================================
%% Public API
%%==============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
  Options = [ {name, ?PROVIDER}
            , {module, ?MODULE}
            , {bare, true}
            , {deps, ?DEPS}
            , {example, "rebar3 erl_doc"}
            , {opts, []}
            , {short_desc, "Generates nice looking documentation"}
            , {desc, "Generates nice looking documentation"}
            ],
  Provider = providers:create(Options),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  OutDir = "doc",
  Opts0 = #{ output_dir => OutDir
           , version => "1.0.0"
           },

  ensure_output_dir(Opts0),
  setup_templates(Opts0),

  Files = rebar_utils:find_files("src", ".erl$"),
  Modules = [parse_doc(Path) || Path <- Files],
  Opts = Opts0#{modules => [M || {M, _} <- lists:sort(Modules)]},

  [generate(module_dtl, M, Info, Opts) || {M, Info} <- Modules],

  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec parse_doc(string()) -> {module(), map()}.
parse_doc(Path) ->
  {M, Edoc} = edoc:get_doc(Path, [{preprocess, true}, {includes, ["include"]}]),
  Internal  = xmerl:export_simple([Edoc], rebar3_docs_xmerl),
  {M, Internal}.

-spec ensure_output_dir(options()) -> ok.
ensure_output_dir(#{output_dir := Dir}) ->
  Dirs = [[Dir], [Dir, "css"], [Dir, "js"]],
  [ ok = filelib:ensure_dir(filename:join(D ++ ["dummy"]))
    || D <- Dirs
  ],
  ok.

-spec setup_templates(options()) -> ok.
setup_templates(#{output_dir := OutDir}) ->
  PrivDir = code:priv_dir(rebar3_docs),
  TemplatePath = filename:join(PrivDir, "module.dtl"),
  {ok, module_dtl} = erlydtl:compile(TemplatePath, module_dtl),
  copy_files(PrivDir, OutDir, [["js", "main.js"], ["css", "main.css"]]).

-spec copy_files(file:filename(), file:filename(), [[string()]]) -> ok.
copy_files(From, To, Paths) ->
  [ {ok, _} = file:copy(filename:join([From | P]), filename:join([To | P]))
    || P <- Paths
  ],
  ok.

-spec generate(module(), module(), map(), options()) -> ok.
generate(Template, Module, _Info, Opts) ->
  #{ output_dir := Dir
   , version := Version
   , modules := Modules
   } = Opts,
  Variables = [ {application, "clojerl"}
              , {module, Module}
              , {version, Version}
              , {modules, Modules}
              ],
  {ok, Content} = Template:render(Variables),
  Filename = atom_to_list(Module) ++ ".html",
  file:write_file(filename:join(Dir, Filename), Content).
