-module(rebar3_docs_xmerl).

-export([ '#root#'/4
        , '#element#'/5
        , '#text#'/1
        , '#xml-inheritance#'/0
        ]).

-include_lib("xmerl/include/xmerl.hrl").

-define(il2b(IOList), unicode:characters_to_binary(IOList)).
-define(l2i(L), list_to_integer(L)).
-define(l2ea(L), list_to_existing_atom(L)).


-type attributes() :: #{atom() => binary()}.
-type element()    :: #{ name => atom()
                       , attrs => attributes()
                       , content => [element()]
                       }.

%%------------------------------------------------------------------------------
%% xmerl:simple_export/2 API
%%------------------------------------------------------------------------------

-spec '#xml-inheritance#'() -> list().
'#xml-inheritance#'() -> [].

%% The '#root#' tag is called when the entire structure has been
%% exported. It does not appear in the structure itself.
-spec '#root#'(any(), any(), any(), any()) -> [any()].
'#root#'([#{name := module} = Module], _, _, _) ->
  [ {name, name(Module)},
    {synopsis, synopsis(Module)},
    {description, description(Module)},
    {functions, functions(Module)},
    {types, types(Module)},
    {private, is_private(Module)},
    {hidden, is_hidden(Module)}
  ].

-spec '#element#'(any(), any(), any(), any(), any()) -> any().
'#element#'(_, _, _, _, E) ->
  xmerl_to_map(E).

%% The '#text#' function is called for every text segment.
-spec '#text#'(any()) -> any().
'#text#'(Text) -> ?il2b(Text).

%%------------------------------------------------------------------------------
%% Helper functions
%%------------------------------------------------------------------------------

xmerl_to_map(#xmlText{} = Text) ->
  ?il2b(Text#xmlText.value);
xmerl_to_map(#xmlElement{} = Element) ->
  AttrsFun = fun(Attr, Acc) ->
                 Acc#{Attr#xmlAttribute.name => Attr#xmlAttribute.value}
             end,
  ContentFun = fun(X, Acc) ->
                   [xmerl_to_map(X) | Acc]
               end,
  #{ name    => Element#xmlElement.name
   , attrs   => lists:foldr(AttrsFun, #{}, Element#xmlElement.attributes)
   , content => lists:foldr(ContentFun, [], Element#xmlElement.content)
   };
xmerl_to_map(X) ->
  X.

name(#{attrs := Attrs}) ->
  ?l2ea(maps:get(name, Attrs)).

is_private(#{attrs := Attrs}) ->
  list_to_boolean(maps:get(private, Attrs, "no")).

is_hidden(#{attrs := Attrs}) ->
  list_to_boolean(maps:get(hidden, Attrs, "no")).

-spec functions(#{}) -> [any()].
functions(#{name := module} = M) ->
  content(functions, [], fun functions/1, M);
functions(#{name := functions, content := Content}) ->
  Functions = [ function(Function)
                || #{name := function} = Function <- Content
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

-spec function(element()) -> [tuple()].
function(#{attrs := Attrs} = Function) ->
  [ {kind        , 'function'}
  , {name        , name(Function)}
  , {arity       , ?l2i(maps:get(arity, Attrs))}
  , {exported    , list_to_boolean(maps:get(exported, Attrs))}
  , {synopsis    , synopsis(Function)}
  , {description , description(Function)}
  , {args        , arguments(Function)}
  , {spec        , typespec(Function)}
  , {equiv       , equiv(Function)}
  ].

-spec equiv(element()) -> binary() | none.
equiv(#{name := function, content := Elements}) ->
  case find_element(equiv, Elements) of
    #{content := EquivElements} ->
      #{content := Expr} = find_element(expr, EquivElements),
      format_text(Expr);
    undefined ->
      none
  end.

-spec arguments(element()) -> [any()].
arguments(#{name := function, content := Elements}) ->
  #{content := Args} = find_element(args, Elements),
  [argument(Arg) || Arg <- Args].

-spec argument(element()) -> [tuple()].
argument(#{name := arg, content := Arg}) ->
  #{content := Content} = find_element(argName, Arg),
  [{name, format_text(Content)}].

-spec typespec(element()) -> [tuple()].
typespec(#{name := function, content := Elements}) ->
  case find_element([typespec, type], Elements) of
    #{content := Types} ->
      [ {args, argument_types(Types)}
      , {return, return_type(Types)}
      ];
    undefined -> none
  end.

-spec argument_types(element()) -> [any()].
argument_types(Types) ->
  case find_elements([argtypes, type], Types) of
    []       -> [];
    ArgTypes -> [parse_type(T) || T <- ArgTypes]
  end.

-spec return_type(element()) -> [tuple()].
return_type(Types) ->
  case find_element(type, Types) of
    undefined -> [];
    Type -> parse_type(Type)
  end.

parse_type(_Type) ->
  [{name, "any()"}].

-spec types(element()) -> [any()].
types(#{name := module} = M) ->
  content(typedecls, [], fun types/1, M);
types(#{name := typedecls, content := Content}) ->
  Types = [type(Type) || #{name := typedecl} = Type <- Content],
  lists:sort(fun sort_by_name_arity/2, Types).

-spec type(element()) -> [any()].
type(#{name := typedecl} = Type) ->
  [ {kind        , 'type'}
  , {name        , type_name(Type)}
  , {arity       , type_arity(Type)}
  ].

type_name(#{name := typedecl} = Type) ->
  type_def(fun type_name/1, Type);
type_name(#{name := typedef} = TypeDef) ->
  case content(erlangName, {error, no_erlang_name}, fun type_name/1, TypeDef) of
    {error, no_erlang_name} -> erlang:error({not_found, erlangName});
    TypeName -> TypeName
  end;
type_name(#{name := erlangName, attrs := Attrs}) ->
  ?l2ea(maps:get(name, Attrs)).

type_arity(#{name := typedecl} = Type) ->
  type_def(fun type_arity/1, Type);
type_arity(#{name := typedef} = TypeDef) ->
  case content(argtypes, {error, no_argtypes}, fun type_arity/1, TypeDef) of
    {error, no_argtypes} -> erlang:error({not_found, argtypes});
    TypeArity -> TypeArity
  end;
type_arity(#{name := argtypes, content := Content}) ->
  count_args(Content).

count_args(Args) ->
  length([ Arg || #{name := type} = Arg <- Args ]).

content(Name, Default, ContinueFun, #{content := Content}) ->
  case find_element(Name, Content) of
    undefined -> Default;
    Found -> ContinueFun(Found)
  end.

synopsis(Element) ->
  content(description, none, fun brief_description/1, Element).

brief_description(#{name := description} = D) ->
  content(briefDescription, none, fun brief_description/1, D);
brief_description(#{name := briefDescription, content := Content}) ->
  format_text(Content).

description(Element) ->
  content(description, none, fun full_description/1, Element).

full_description(#{name := description} = D) ->
  content(fullDescription, none, fun full_description/1, D);
full_description(#{name := fullDescription, content := Content}) ->
  format_text(Content).

format_text(Elements) when is_list(Elements)->
  [format_text(X) || X <- Elements];
format_text(#{name := Name, content := Content, attrs := Attrs}) ->
  NameStr = atom_to_list(Name),
  [ "<", NameStr, " ", format_attributes(Attrs), ">"
  , format_text(Content)
  , "</", NameStr, ">"
  ];
format_text(Value) when is_binary(Value) ->
  Value;
format_text(#xmlPI{value = Value}) ->
  Value;
format_text(#xmlComment{}) ->
  [];
format_text(#xmlDecl{}) ->
  [].

-spec format_attributes(attributes()) -> unicode:chardata().
format_attributes(Attrs) ->
  [format_attribute(Name, Value) || {Name, Value} <- maps:to_list(Attrs)].

-spec format_attribute(atom(), string()) -> unicode:chardata().
format_attribute(Name, Value) ->
  NameBin = atom_to_binary(Name, utf8),
  ValueStr = to_chardata(Value),
  [NameBin, "=\"", ValueStr, "\""].

-spec to_chardata(any()) -> unicode:chardata().
to_chardata(X) when is_atom(X) ->
  atom_to_binary(X, utf8);
to_chardata(X) when is_integer(X) ->
  integer_to_binary(X);
to_chardata(X) ->
  X.

type_def(ContinueFun, #{name := typedecl} = Type) ->
  case content(typedef, {error, no_typedef}, ContinueFun, Type) of
    {error, no_typedef} -> erlang:error({not_found, typedef, Type});
    ContinuationResult -> ContinuationResult
  end.

list_to_boolean("yes") -> true;
list_to_boolean("no")  -> false.

-spec find_element([atom()], [element()]) -> element() | undefined.
find_element(Names, Elements) ->
  case find_elements(Names, Elements) of
    [Found] -> Found;
    [] -> undefined
  end.

-spec find_elements([atom()], [element()]) -> [element()].
find_elements([], Elements) ->
  Elements;
find_elements([Name | Rest], Content) ->
  case find_elements(Name, Content) of
    [#{content := Children}] -> find_elements(Rest, Children);
    Found when Rest =:= [] -> Found;
    _ -> []
  end;
find_elements(Name, Elements) ->
  [Element || #{name := N} = Element <- Elements, Name =:= N].
