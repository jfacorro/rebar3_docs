-module(rebar3_docs_xmerl).

%% xmerl:simple_export/2 API

-export(['#root#'/4,
         '#element#'/5,
         '#text#'/1,
         '#xml-inheritance#'/0]).

-export_type([xml_element_content/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @type xml_element_content(). `#xmlElement.content' as defined by `xmerl.hrl'.
-type xml_element_content() :: [#xmlElement{} | #xmlText{} | #xmlPI{} | #xmlComment{} | #xmlDecl{}].

-define(il2b(IOList), iolist_to_binary(IOList)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%%
%%' xmerl:simple_export/2 API
%%

-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> docsh_internal:t().
'#root#'([#xmlElement{name = module} = Module], _, _, _) ->
  [ {name, get_module_name(Module)},
    {description, get_module_description(Module)},
    {functions, get_functions(Module)},
    {types, get_types(Module)}
  ].

-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(_, _, _, _, E) -> E.

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

%%.
%%' xmerl:simple_export/2 helpers
%%

get_module_name(#xmlElement{attributes = Attrs}) ->
  ?l2ea(find_attribute(name, Attrs)).

get_module_description(#xmlElement{name = module} = M) ->
  get_description(M).

-spec get_functions(#xmlElement{}) -> [docsh_internal:item()].
get_functions(#xmlElement{name = module} = M) ->
  get_content(functions, [], fun get_functions/1, M);
get_functions(#xmlElement{name = functions, content = Content}) ->
  Functions = [ get_function(Function)
                || #xmlElement{name = function} = Function <- Content
              ],
  lists:sort(fun sort_by_name/2, Functions).

-spec sort_by_name(any(), any()) -> boolean().
sort_by_name(X, Y) ->
  proplists:get_value(name, X) < proplists:get_value(name, Y).

-spec get_function(#xmlElement{}) -> [tuple()].
get_function(#xmlElement{attributes = Attrs} = Function) ->
  [ {kind        , 'function'}
  , {name        , ?l2ea(find_attribute(name, Attrs))}
  , {arity       , ?l2i(find_attribute(arity, Attrs))}
  , {exported    , list_to_boolean(find_attribute(exported, Attrs))}
  , {description , get_function_description(Function)}
  , {args        , get_arguments(Function)}
  , {spec        , get_typespec(Function)}
  ].

-spec get_arguments(#xmlElement{}) -> [tuple()].
get_arguments(#xmlElement{name = function, content = Elements}) ->
  case find_elements(args, Elements) of
    [#xmlElement{content = Args}] ->
      [get_argument(Arg) || Arg <- Args];
    [] -> []
  end.

-spec get_argument(#xmlElement{}) -> [tuple()].
get_argument(#xmlElement{name = arg, content = Arg}) ->
  case find_elements(argName, Arg) of
    [#xmlElement{content = Content}] ->
      [{name, format_text(Content)}];
    [] -> []
  end.

-spec get_typespec(#xmlElement{}) -> [tuple()].
get_typespec(#xmlElement{name = function, content = Elements}) ->
  case find_elements([typespec, type], Elements) of
    [#xmlElement{content = Types}] ->
      [ {args, get_argtypes(Types)}
      , {return, get_returntype(Types)}
      ];
    [] -> none
  end.

-spec get_argtypes(#xmlElement{}) -> [tuple()].
get_argtypes(Types) ->
  case find_elements([argtypes, type], Types) of
    []       -> [];
    ArgTypes -> [parse_type(T) || T <- ArgTypes]
  end.

-spec get_returntype(#xmlElement{}) -> [tuple()].
get_returntype(Types) ->
  case find_elements(type, Types) of
    [Type] -> parse_type(Type);
    []     -> []
  end.

parse_type(_Type) ->
  [{name, "any()"}].

-spec get_types(#xmlElement{}) -> [docsh_internal:item()].
get_types(#xmlElement{name = module} = M) ->
  get_content(typedecls, [], fun get_types/1, M);
get_types(#xmlElement{name = typedecls, content = Content}) ->
  [ get_type(Type) || #xmlElement{name = typedecl} = Type <- Content ].

-spec get_type(#xmlElement{}) -> docsh_internal:item().
get_type(#xmlElement{name = typedecl} = Type) ->
  [ {kind        , 'type'}
  , {name        , get_type_name(Type)}
  , {arity       , get_type_arity(Type)}
    %% TODO: really always true? anyway, we want the structure for functions and types
    %% to be the same
  , {exported    , true}
  , {description , get_type_description(Type)}
  ].

get_function_description(#xmlElement{name = function} = Function) ->
  get_description(Function).

get_type_name(#xmlElement{name = typedecl} = Type) ->
  get_type_def(fun get_type_name/1, Type);
get_type_name(#xmlElement{name = typedef} = TypeDef) ->
  case get_content(erlangName, {error, no_erlang_name}, fun get_type_name/1, TypeDef) of
    {error, no_erlang_name} -> erlang:error({not_found, erlangName});
    TypeName -> TypeName
  end;
get_type_name(#xmlElement{name = erlangName, attributes = Attrs}) ->
  ?l2ea(find_attribute(name, Attrs)).

get_type_arity(#xmlElement{name = typedecl} = Type) ->
  get_type_def(fun get_type_arity/1, Type);
get_type_arity(#xmlElement{name = typedef} = TypeDef) ->
  case get_content(argtypes, {error, no_argtypes}, fun get_type_arity/1, TypeDef) of
    {error, no_argtypes} -> erlang:error({not_found, argtypes});
    TypeArity -> TypeArity
  end;
get_type_arity(#xmlElement{name = argtypes, content = Content}) ->
  count_args(Content).

count_args(Args) ->
  length([ Arg || #xmlElement{name = type} = Arg <- Args ]).

get_type_description(#xmlElement{name = typedecl} = Type) ->
  get_description(Type).

get_content(Name, Default, ContinueFun, #xmlElement{content = Content}) ->
  case lists:keyfind(Name, #xmlElement.name, Content) of
    false -> Default;
    #xmlElement{} = Found -> ContinueFun(Found)
  end.

get_description(#xmlElement{} = Element) ->
  get_content(description, none, fun get_full_description/1, Element).

get_full_description(#xmlElement{name = description} = D) ->
  get_content(fullDescription, none, fun get_full_description/1, D);
get_full_description(#xmlElement{name = fullDescription, content = Content}) ->
  %% See xmerl.hrl for the definition of #xmlElement.content:
  %%   content = [#xmlElement()|#xmlText()|#xmlPI()|#xmlComment()|#xmlDecl()]
  format_text(Content).

format_text(XmlElements) when is_list(XmlElements)->
  [format_text(X) || X <- XmlElements];
format_text(#xmlElement{name = Name, content = Content}) ->
  NameStr = atom_to_list(Name),
  ["<", NameStr, ">", format_text(Content), "</", NameStr, ">"];
format_text(#xmlText{value = Value}) ->
  Value;
format_text(#xmlPI{value = Value}) ->
  Value;
format_text(#xmlComment{}) ->
  [];
format_text(#xmlDecl{}) ->
  [].

get_type_def(ContinueFun, #xmlElement{name = typedecl} = Type) ->
  case get_content(typedef, {error, no_typedef}, ContinueFun, Type) of
    {error, no_typedef} -> erlang:error({not_found, typedef, Type});
    ContinuationResult -> ContinuationResult
  end.

list_to_boolean("yes") -> true;
list_to_boolean("no")  -> false.

find_attribute(Attr, Attrs) ->
  case xmerl_lib:find_attribute(Attr, Attrs) of
    false -> {error, no_attr, Attr};
    {value, Value} -> Value
  end.

find_elements([], Elements) ->
  Elements;
find_elements([Name | Rest], Elements) ->
  case find_elements(Name, Elements) of
    [#xmlElement{content = E}] -> find_elements(Rest, E);
    Found when Rest =:= [] -> Found;
    _ -> []
  end;
find_elements(Name, Elements) ->
  [ Element
   || #xmlElement{name = N} = Element <- Elements, Name =:= N
  ].
