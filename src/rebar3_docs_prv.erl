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
  Opts0 = #{ application => "clojerl"
           , output_dir => OutDir
           , version => "1.0.0"
           },

  ensure_output_dir(Opts0),
  setup_templates(Opts0),


  Files = rebar_utils:find_files("src", ".erl$"),
  Modules1 = [parse_doc(Path) || Path <- Files],
  Modules  = lists:sort(fun sort_modules/2, Modules1),
  Opts1 = Opts0#{modules => Modules},
  Sidenav = generate(sidenav_dtl, undefined, Opts1),

  Opts = Opts1#{sidenav => Sidenav},

  [generate(module_dtl, M, Opts) || M <- Modules],

  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec sort_modules(any(), any()) -> boolean().
sort_modules(X, Y) ->
  proplists:get_value(name, X) < proplists:get_value(name, Y).

-spec parse_doc(string()) -> map().
parse_doc(Path) ->
  {_M, Edoc} = edoc:get_doc(Path, [{preprocess, true}, {includes, ["include"]}]),
  xmerl:export_simple([Edoc], rebar3_docs_xmerl).

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
  Templates = ["module", "sidenav"],
  [ begin
      TemplatePath = filename:join(PrivDir, T ++ ".dtl"),
      Module = list_to_atom(T ++ "_dtl"),
      {ok, Module} = erlydtl:compile(TemplatePath, Module)
    end
    || T <- Templates
  ],
  copy_files(PrivDir, OutDir, [["js", "main.js"], ["css", "main.css"]]).

-spec copy_files(file:filename(), file:filename(), [[string()]]) -> ok.
copy_files(From, To, Paths) ->
  [ {ok, _} = file:copy(filename:join([From | P]), filename:join([To | P]))
    || P <- Paths
  ],
  ok.

-spec generate(module(), map(), options()) -> ok.
generate(module_dtl, Module, #{output_dir := Dir} = Opts) ->
  Name          = proplists:get_value(name, Module),
  Variables     = maps:to_list(Opts#{module => Module}),
  {ok, Content} = module_dtl:render(Variables),
  Filename      = atom_to_list(Name) ++ ".html",
  file:write_file(filename:join(Dir, Filename), Content);
generate(sidenav_dtl, _, Opts) ->
  Variables     = maps:to_list(Opts),
  {ok, Content} = sidenav_dtl:render(Variables),
  Content.
