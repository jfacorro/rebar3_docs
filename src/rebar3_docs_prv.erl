-module(rebar3_docs_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, docs).
-define(DEPS, [app_discovery]).

-define(INCLUDE, "include").

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
            , {example, "rebar3 docs"}
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
  Modules  = lists:sort(fun sort_by_name/2, Modules1),
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

-spec sort_by_name(any(), any()) -> boolean().
sort_by_name(X, Y) ->
  proplists:get_value(name, X) < proplists:get_value(name, Y).

-spec parse_doc(string()) -> map().
parse_doc(Path) ->
  Opts = [{preprocess, true}, {includes, [?INCLUDE]}],
  {_M, Edoc} = edoc:get_doc(Path, Opts),
  Source = edoc:read_source(Path, Opts),
  Docs = xmerl:export_simple([Edoc], rebar3_docs_xmerl),
  add_function_specs(Docs, Source).

-spec add_function_specs([any()], [any()]) -> [any()].
add_function_specs(Docs, Source) ->
  FoldFun = fun(X, Acc) ->
                case erl_syntax:type(X) of
                  attribute ->
                    case erl_syntax_lib:analyze_attribute(X) of
                      {spec, {spec, {{F, A}, _}}} ->
                        Data = pretty_print_spec(X),
                        Acc#{{F, A} => Data};
                      _  -> Acc
                    end;
                  _ -> Acc
                end
            end,
  Source1 = lists:foldl(FoldFun, #{}, Source),

  Functions0 = proplists:get_value(functions, Docs, []),

  F = fun(Function) ->
          Name = proplists:get_value(name, Function),
          Arity = proplists:get_value(arity, Function),
          Spec = maps:get({Name, Arity}, Source1, none),
          [{spec, Spec} | Function]
      end,

  [{functions, lists:map(F, Functions0)} | Docs].

-spec pretty_print_spec(any()) -> string().
pretty_print_spec(Tree) ->
  try
    erl_prettypr:format(Tree)
  catch _:_:_ ->
      ""
  end.

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
