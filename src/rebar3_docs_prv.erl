-module(rebar3_docs_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, docs).
-define(DEPS, [app_discovery]).

-define(INCLUDE, "include").

-type options() :: #{ application := string()
                    , filename => string()
                    , include_dirs := [file:filename()]
                    , output_dir := file:filename()
                    , version := string()
                    }.

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
            , {opts, [{out, $o, "out", string, "Output directory"}]}
            , {short_desc, "Generates nice looking documentation"}
            , {desc, "Generates nice looking documentation"}
            ],
  Provider = providers:create(Options),
  {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  [AppInfo | _] = rebar_state:project_apps(State),
  AppName = rebar_utils:to_list(rebar_app_info:name(AppInfo)),
  OriginalVsn = rebar_app_info:original_vsn(AppInfo),
  AppVersion = rebar_utils:vcs_vsn(AppInfo, OriginalVsn, State),

  Opts = #{ application => AppName
          , output_dir => output_dir(State)
          , include_dirs => include_dirs(AppInfo)
          , version => AppVersion
          },

  ensure_output_dir(Opts),
  setup_templates(Opts),

  Files = rebar_utils:find_files("src", ".erl$"),
  Modules1 = [parse_doc(Path, Opts) || Path <- Files],
  Modules2 = [M || M <- Modules1, proplists:get_value(name, M) =/= undefined],
  Modules  = lists:sort(fun sort_by_name/2, Modules2),
  Sidenav = generate(sidenav_dtl, [{modules, Modules}], Opts),

  [ generate(module_dtl, [{module, M}, {sidenav, Sidenav}], Opts)
    || M <- Modules
  ],

  IndexVars = [{content, <<>>}, {title, AppName}, {sidenav, Sidenav}],
  ok = generate(landing_dtl, IndexVars, Opts#{filename => "index.html"}),

  generate_nav_tree(Modules, Opts),

  {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec output_dir(rebar_state:t()) -> string().
output_dir(State) ->
  {Args, _} = rebar_state:command_parsed_args(State),
  proplists:get_value(out, Args, "docs").

-spec include_dirs(rebar_app_info:t()) -> [string()].
include_dirs(AppInfo) ->
  OutDir = rebar_app_info:out_dir(AppInfo),
  BaseDir = rebar_app_info:dir(AppInfo),
  RebarOpts = rebar_app_info:opts(AppInfo),
  ErlOpts = rebar_opts:erl_opts(RebarOpts),
  ErlOptIncludes = proplists:get_all_values(i, ErlOpts),
  [ filename:join([BaseDir, "include"])
  , filename:join(OutDir, "..")
  | lists:map(fun(Incl) -> filename:absname(Incl) end, ErlOptIncludes)
  ].

-spec sort_by_name(any(), any()) -> boolean().
sort_by_name(X, Y) ->
  proplists:get_value(name, X) < proplists:get_value(name, Y).

-spec parse_doc(string(), options()) -> [any()].
parse_doc(Path, #{include_dirs := IncludeDirs}) ->
  Opts = [{preprocess, true}, {includes, IncludeDirs}],
  try
    {_M, Edoc} = edoc:get_doc(Path, Opts),
    Source = edoc:read_source(Path, Opts),
    Docs = xmerl:export_simple([Edoc], rebar3_docs_xmerl),
    specs_and_types(Docs, Source)
  catch _:_ ->
      rebar_api:error("Failed to process docs for ~s", [Path]),
      [{functions, []}, {types, []}]
  end.

-spec specs_and_types([any()], [any()]) -> [any()].
specs_and_types(Docs, Source) ->
  #{ specs := Specs
   , types := TypesDesc
   } = lists:foldl( fun extract_specs_and_types/2
                  , #{specs => #{}, types => #{}}
                  , Source
                  ),

  Functions = [ begin
                  Name = proplists:get_value(name, Function),
                  Arity = proplists:get_value(arity, Function),
                  Spec = maps:get({Name, Arity}, Specs, none),
                  [{spec, Spec} | Function]
                end
                || Function <- proplists:get_value(functions, Docs, [])
              ],

  Types = [ begin
              Name = proplists:get_value(name, Type),
              Arity = proplists:get_value(arity, Type),
              Desc = maps:get({Name, Arity}, TypesDesc, none),
              [{description, Desc} | Type]
            end
            || Type <- proplists:get_value(types, Docs, [])
          ],

  [{functions, Functions}, {types, Types} | Docs].

extract_specs_and_types(Tree, #{specs := Specs, types:= Types} = M) ->
  case erl_syntax:type(Tree) of
    attribute ->
      case erl_syntax_lib:analyze_attribute(Tree) of
        {spec, {spec, {{F, A}, _}}} ->
          Data = pretty_print(Tree),
          M#{specs := Specs#{{F, A} => Data}};
        {type, {type, {Type, _, Args}}} ->
          M#{types := Types#{{Type, length(Args)} => pretty_print(Tree)}};
        _  -> M
      end;
    _ -> M
  end.

-spec pretty_print(any()) -> string().
pretty_print(Tree) ->
  erl_pp:attribute(Tree).

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
  Templates = ["module", "sidenav", "landing"],
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

-spec generate(module(), list(), options()) -> ok | binary().
generate(module_dtl, Variables, #{output_dir := Dir} = Opts) ->
  Module = proplists:get_value(module, Variables),
  Name = proplists:get_value(name, Module),
  Vars = Variables ++ [{title, Name} | maps:to_list(Opts)],
  Filename = atom_to_list(Name) ++ ".html",
  Path = filename:join(Dir, Filename),
  rebar_api:debug("Generating ~s", [Path]),
  {ok, Content} = module_dtl:render(Vars),
  ok = file:write_file(Path, unicode:characters_to_binary(Content));
generate(sidenav_dtl, Variables, Opts) ->
  Vars = Variables ++ maps:to_list(Opts),
  {ok, Content} = sidenav_dtl:render(Vars),
  Content;
generate(landing_dtl, Variables, Opts) ->
  #{output_dir := Dir, filename := Filename} = Opts,
  Vars = Variables ++ maps:to_list(Opts),
  Path = filename:join(Dir, Filename),
  {ok, Content} = landing_dtl:render(Vars),
  ok = file:write_file(Path, unicode:characters_to_binary(Content)).

-spec generate_nav_tree([any()], options()) -> ok.
generate_nav_tree(Modules, Opts) ->
  #{output_dir := Dir} = Opts,
  NavTree = build_nav_tree(Modules),
  JSON = jsx:encode(NavTree),
  Path = filename:join([Dir, "js", "nav-tree.js"]),
  ok = file:write_file(Path, ["navTree = ", JSON, ";"]).

%% @doc Builds a map where the keys are the module names and the
%% values are the list of functions and types in the modules.
%%
%% This map is meant to be used for displaying items in the side
%% navigation bar and also in the quick item search.
-spec build_nav_tree([list()]) -> map().
build_nav_tree(Modules) ->
  maps:from_list(build_nav_tree(modules, Modules)).

-spec build_nav_tree(modules | children, [list()]) -> list().
build_nav_tree(modules, Modules) ->
  [ begin
      Name = proplists:get_value(name, M),
      Functions = proplists:get_value(functions, M, []),
      Types = proplists:get_value(types, M, []),
      { Name, #{ functions => build_nav_tree(children, Functions)
               , types => build_nav_tree(children, Types)
               }
      }
    end
    || M <- Modules
  ];
build_nav_tree(children, Children) ->
  [ begin
      Name = proplists:get_value(name, Child),
      Arity = proplists:get_value(arity, Child),
      unicode:characters_to_binary( [ atom_to_binary(Name, utf8)
                                    , "/"
                                    , integer_to_list(Arity)
                                    ]
                                  )
    end
    || Child <- Children
  ].
