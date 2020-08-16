-module(rebar3_docs_xmerl).

%% xmerl:simple_export/2 API

-export([ '#root#'/4
        , '#element#'/5
        , '#text#'/1
        , '#xml-inheritance#'/0
        ]).

-export_type([xml_element_content/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @type xml_element_content(). `#xmlElement.content' as defined by `xmerl.hrl'.
-type xml_element_content() :: [ #xmlElement{}
                               | #xmlText{}
                               | #xmlPI{}
                               | #xmlComment{}
                               | #xmlDecl{}
                               ].

-define(il2b(IOList), unicode:characters_to_binary(IOList)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).

%%
%%' xmerl:simple_export/2 API
%%

-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> [any()].
'#root#'([#xmlElement{name = module} = Module], _, _, _) ->
  [ {name, name(Module)},
    {description, description(Module)},
    {functions, functions(Module)},
    {types, types(Module)}
  ].

-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(_, _, _, _, E) -> E.

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

%%.
%%' xmerl:simple_export/2 helpers
%%

name(#xmlElement{attributes = Attrs}) ->
  ?l2ea(find_attribute(name, Attrs)).

-spec functions(#xmlElement{}) -> [any()].
functions(#xmlElement{name = module} = M) ->
  content(functions, [], fun functions/1, M);
functions(#xmlElement{name = functions, content = Content}) ->
  Functions = [ function(Function)
                || #xmlElement{name = function} = Function <- Content
              ],
  lists:sort(fun sort_by_name_arity/2, Functions).

-spec sort_by_name_arity(any(), any()) -> boolean().
sort_by_name_arity(X, Y) ->
  NameX = proplists:get_value(name, X),
  NameY = proplists:get_value(name, Y),
  if
    NameX < NameY -> true;
    NameX =:= NameY ->
      proplists:get_value(arity, X) < proplists:get_value(arity, Y);
    true -> false
  end.

-spec function(#xmlElement{}) -> [tuple()].
function(#xmlElement{attributes = Attrs} = Function) ->
  [ {kind        , 'function'}
  , {name        , name(Function)}
  , {arity       , ?l2i(find_attribute(arity, Attrs))}
  , {exported    , list_to_boolean(find_attribute(exported, Attrs))}
  , {synopsis    , synopsis(Function)}
  , {description , description(Function)}
  , {args        , arguments(Function)}
  , {spec        , typespec(Function)}
  ].

-spec arguments(#xmlElement{}) -> [any()].
arguments(#xmlElement{name = function, content = Elements}) ->
  case find_elements(args, Elements) of
    [#xmlElement{content = Args}] ->
      [argument(Arg) || Arg <- Args];
    [] -> []
  end.

-spec argument(#xmlElement{}) -> [tuple()].
argument(#xmlElement{name = arg, content = Arg}) ->
  case find_elements(argName, Arg) of
    [#xmlElement{content = Content}] ->
      [{name, format_text(Content)}];
    [] -> []
  end.

-spec typespec(#xmlElement{}) -> [tuple()].
typespec(#xmlElement{name = function, content = Elements}) ->
  case find_elements([typespec, type], Elements) of
    [#xmlElement{content = Types}] ->
      [ {args, argument_types(Types)}
      , {return, return_type(Types)}
      ];
    [] -> none
  end.

-spec argument_types(#xmlElement{}) -> [any()].
argument_types(Types) ->
  case find_elements([argtypes, type], Types) of
    []       -> [];
    ArgTypes -> [parse_type(T) || T <- ArgTypes]
  end.

-spec return_type(#xmlElement{}) -> [tuple()].
return_type(Types) ->
  case find_elements(type, Types) of
    [Type] -> parse_type(Type);
    []     -> []
  end.

parse_type(_Type) ->
  [{name, "any()"}].

-spec types(#xmlElement{}) -> [any()].
types(#xmlElement{name = module} = M) ->
  content(typedecls, [], fun types/1, M);
types(#xmlElement{name = typedecls, content = Content}) ->
  Types = [ type(Type) || #xmlElement{name = typedecl} = Type <- Content ],
  lists:sort(fun sort_by_name_arity/2, Types).

-spec type(#xmlElement{}) -> [any()].
type(#xmlElement{name = typedecl} = Type) ->
  [ {kind        , 'type'}
  , {name        , type_name(Type)}
  , {arity       , type_arity(Type)}
  ].

type_name(#xmlElement{name = typedecl} = Type) ->
  type_def(fun type_name/1, Type);
type_name(#xmlElement{name = typedef} = TypeDef) ->
  case content(erlangName, {error, no_erlang_name}, fun type_name/1, TypeDef) of
    {error, no_erlang_name} -> erlang:error({not_found, erlangName});
    TypeName -> TypeName
  end;
type_name(#xmlElement{name = erlangName, attributes = Attrs}) ->
  ?l2ea(find_attribute(name, Attrs)).

type_arity(#xmlElement{name = typedecl} = Type) ->
  type_def(fun type_arity/1, Type);
type_arity(#xmlElement{name = typedef} = TypeDef) ->
  case content(argtypes, {error, no_argtypes}, fun type_arity/1, TypeDef) of
    {error, no_argtypes} -> erlang:error({not_found, argtypes});
    TypeArity -> TypeArity
  end;
type_arity(#xmlElement{name = argtypes, content = Content}) ->
  count_args(Content).

count_args(Args) ->
  length([ Arg || #xmlElement{name = type} = Arg <- Args ]).

content(Name, Default, ContinueFun, #xmlElement{content = Content}) ->
  case lists:keyfind(Name, #xmlElement.name, Content) of
    false -> Default;
    #xmlElement{} = Found -> ContinueFun(Found)
  end.

synopsis(#xmlElement{} = Element) ->
  content(description, none, fun brief_description/1, Element).

brief_description(#xmlElement{name = description} = D) ->
  content(briefDescription, none, fun brief_description/1, D);
brief_description(#xmlElement{name = briefDescription, content = Content}) ->
  format_text(Content).

description(#xmlElement{} = Element) ->
  content(description, none, fun full_description/1, Element).

full_description(#xmlElement{name = description} = D) ->
  content(fullDescription, none, fun full_description/1, D);
full_description(#xmlElement{name = fullDescription, content = Content}) ->
  format_text(Content).

%% XmlElements :: [#xmlElement()|#xmlText()|#xmlPI()|#xmlComment()|#xmlDecl()]
format_text(XmlElements) when is_list(XmlElements)->
  [format_text(X) || X <- XmlElements];
format_text(#xmlElement{name = Name, content = Content, attributes = Attrs}) ->
  NameStr = atom_to_list(Name),
  [ "<", NameStr, " ", format_attributes(Attrs), ">"
  , format_text(Content)
  , "</", NameStr, ">"
  ];
format_text(#xmlText{value = Value}) ->
  Value;
format_text(#xmlPI{value = Value}) ->
  Value;
format_text(#xmlComment{}) ->
  [];
format_text(#xmlDecl{}) ->
  [].

-spec format_attributes([#xmlAttribute{}]) -> unicode:chardata().
format_attributes(Attrs) ->
  format_attributes(Attrs, []).

-spec format_attributes([#xmlAttribute{}], unicode:chardata()) ->
  unicode:chardata().
format_attributes([], Result) ->
  Result;
format_attributes([#xmlAttribute{name = Name, value = Value} | Attrs], Result) ->
  NameBin = atom_to_binary(Name, utf8),
  ValueStr = to_chardata(Value),
  format_attributes(Attrs, [Result, NameBin, "=\"", ValueStr, "\""]).

-spec to_chardata(any()) -> unicode:chardata().
to_chardata(X) when is_atom(X) ->
  atom_to_binary(X, utf8);
to_chardata(X) when is_integer(X) ->
  integer_to_binary(X);
to_chardata(X) ->
  X.

type_def(ContinueFun, #xmlElement{name = typedecl} = Type) ->
  case content(typedef, {error, no_typedef}, ContinueFun, Type) of
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
