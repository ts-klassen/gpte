-module(gpte_schema).

-export([
        schema/1
      , schema/2
    ]).

-export_type([
        schema/0
      , type/0
      , sample/0
    ]).

-type schema() :: map().

-type type() :: {module(), TypeName::atom()}
              | {module(), TypeName::atom(), Arity::non_neg_integer()}.

-gpte_type_description({sample/0, <<"This is a sample type.">>}).
-type sample() :: #{
        %% string() is not allowed. Use binary string.
        unicode_field => unicode:unicode_binary()
      , binstr_field => klsn:binstr()
      , klsn_binstr_field => klsn_binstr:binstr()
      , integer_field => integer()
      , number_field => number()
      , boolean_field => boolean()
      , enum_field => enum1 | enum2 % must have more than two enum
      , map_field => #{
            field => boolean()
          , required_field := boolean()
        }
      , array_field => [number()]
      , required_boolean_field := boolean()
    }.
-gpte_type_description([
        {sample/0, [], <<"sample object with 10 fields.">>}
      , {sample/0, [unicode_field], <<"field value must be `ユニコード`."/utf8>>}
      , {sample/0, [binstr_field], <<"field value must be `binstr value`.">>}
      , {sample/0, [klsn_binstr_field], <<"field value must be `klsn_binstr value`.">>}
      , {sample/0, [integer_field], <<"field value must be `512`.">>}
      , {sample/0, [number_field], <<"field value must be `1.024`.">>}
      , {sample/0, [boolean_field], <<"field value must be `false`.">>}
      , {sample/0, [enum_field], <<"field value must be `enum2`.">>}
      , {sample/0, [map_field], <<"sample object with 2 fields.">>}
      , {sample/0, [map_field, field], <<"field value must be `true`.">>}
      , {sample/0, [map_field, required_field], <<"field value must be `false`.">>}
      , {sample/0, [array_field], <<"array size must be exactly 6.">>}
      , {sample/0, [array_field, []], <<"must be a fibonacci sequence.">>}
      , {sample/0, [required_boolean_field], <<"field value must be `true`.">>}
    ]).

-spec schema(type()) -> schema().
schema(TypeSpec) ->
    Module = element(1, TypeSpec),
    TypeName = element(2, TypeSpec),
    Arity = case TypeSpec of
        {_, _, Arity0} ->
            Arity0;
        _ ->
            0
    end,
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(Module)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    Type = element(3, element(1, maps:get({type, TypeName, 0}, Types))),
    klsn_map:filter(#{
        name => {value, TypeName}
      , schema => {value, build(Type, {Module, TypeName, Arity, []})}
      , description => lookup_type_description({Module, TypeName, Arity})
    }).

-spec schema(type(), JSON::klsn:binstr()) -> term().
schema(TypeSpec, JSON) ->
    Module = element(1, TypeSpec),
    TypeName = element(2, TypeSpec),
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(Module)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    Type = element(3, element(1, maps:get({type, TypeName, 0}, Types))),
    parse(Type, jsone:decode(JSON)).


build({type, _, map, Properties}, DescSpec={Module, TypeName, Arity, Path}) ->
    DescMap = klsn_map:filter(#{
        description => lookup_field_description(DescSpec)
    }),
    DescMap#{
        type => object
      , properties => maps:from_list(lists:map(fun
            ({type, _, map_field_assoc, [{atom, _, KeyName}, ValueType]})->
                {KeyName, build(ValueType, {Module, TypeName, Arity, Path ++ [KeyName]})};
            ({type, _, map_field_exact, [{atom, _, KeyName}, ValueType]})->
                {KeyName, build(ValueType, {Module, TypeName, Arity, Path ++ [KeyName]})};
            (UnknownType)->
                error(unknown_properties, [UnknownType])
        end, Properties))
      , required => lists:filtermap(fun
            ({type, _, map_field_assoc, _Args})->
                false;
            ({type, _, map_field_exact, [{atom, _, KeyName}, _ValueType]})->
                {true, KeyName}
        end, Properties)
      , additionalProperties => false
    };
build({type, _, union, Args}, DescSpec) ->
    DescMap = klsn_map:filter(#{
        description => lookup_field_description(DescSpec)
    }),
    DescMap#{
        enum => lists:map(fun
            ({atom, _, Enum})->
                Enum;
            (UnknownType)->
                error(unknown_enum, [UnknownType])
        end, Args)
    };
build({type, _, list, [Arg]}, DescSpec={Module, TypeName, Arity, Path}) ->
    klsn_map:filter(#{
        type => {value, array}
      , items => {value, build(Arg, {Module, TypeName, Arity, Path ++ [[]]})}
      , description => lookup_field_description(DescSpec)
    });
build({remote_type, _, [{atom,_,unicode}, {atom,_,unicode_binary}, []]}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, string}
      , description => lookup_field_description(DescSpec)
    });
build({remote_type, _, [{atom, _, klsn}, {atom, _, binstr}, []]}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, string}
      , description => lookup_field_description(DescSpec)
    });
build({remote_type, _, [{atom, _, klsn_binstr}, {atom, _, binstr}, []]}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, string}
      , description => lookup_field_description(DescSpec)
    });
build({type, _, integer, _}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, integer}
      , description => lookup_field_description(DescSpec)
    });
build({type, _, number, _}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, number}
      , description => lookup_field_description(DescSpec)
    });
build({type, _, float, _}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, number}
      , description => lookup_field_description(DescSpec)
    });
build({type, _, boolean, _}, DescSpec) ->
    klsn_map:filter(#{
        type => {value, boolean}
      , description => lookup_field_description(DescSpec)
    });
build(UnknownType, DescSpec) ->
    error(unknown_type, [UnknownType, DescSpec]).


parse({type, _, map, Properties}, Data) ->
    maps:from_list(lists:filtermap(fun
        ({type, _, map_field_assoc, [{atom, _, KeyName}, ValueType]})->
            case klsn_map:lookup([atom_to_binary(KeyName)], Data) of
                {value, Value} ->
                    {true, {KeyName, parse(ValueType, Value)}};
                none ->
                    false
            end;
        ({type, _, map_field_exact, [{atom, _, KeyName}, ValueType]})->
            Name = atom_to_binary(KeyName),
            {true, {KeyName, parse(ValueType, maps:get(Name, Data))}};
        (UnknownType)->
            error(unknown_properties, [UnknownType])
    end, Properties));
parse({type, _, union, Args}, Data) ->
    EnumMap = maps:from_list(lists:map(fun
        ({atom, _, Enum})->
            {atom_to_binary(Enum), Enum};
        (UnknownType)->
            error(unknown_enum, [UnknownType])
    end, Args)),
    maps:get(Data, EnumMap);
parse({type, _, list, [Arg]}, Data) ->
    lists:map(fun(Elem)->
        parse(Arg, Elem)
    end, Data);
parse({remote_type, _, [{atom,_,unicode}, {atom,_,unicode_binary}, []]}, Data) when is_binary(Data) ->
    Data;
parse({remote_type, _, [{atom, _, klsn}, {atom, _, binstr}, []]}, Data) when is_binary(Data) ->
    Data;
parse({remote_type, _, [{atom, _, klsn_binstr}, {atom, _, binstr}, []]}, Data) when is_binary(Data) ->
    Data;
parse({type, _, integer, _}, Data) when is_integer(Data) ->
    Data;
parse({type, _, number, _}, Data) when is_number(Data) ->
    Data;
parse({type, _, float, _}, Data) when is_number(Data) ->
    Data + 0.0;
parse({type, _, boolean, _}, Data) when is_boolean(Data) ->
    Data;
parse(UnknownType, Data) ->
    error(unknown_type, [UnknownType, Data]).


-spec lookup_type_description({
        Module :: atom()
      , Type :: atom()
      , Arity :: non_neg_integer()
    }) -> klsn:maybe(klsn:binstr()).
lookup_type_description({Module, Type, Arity}) ->
    Attributes = Module:module_info(attributes),
    Res = lists:filtermap(fun
        ({gpte_type_description, List})->
            Descriptions = lists:filtermap(fun
                ({{Type0, Arity0}, Description}) when Type0 =:= Type, Arity0 =:= Arity ->
                    {true, Description};
                (_) ->
                    false
            end, List),
            case Descriptions of
                [] ->
                    false;
                _ ->
                    {true, Descriptions}
            end;
        (_) ->
            false
    end, Attributes),
    case Res of
        [[Value|_]|_] ->
            {value, Value};
        _ ->
            none
    end.


-spec lookup_field_description({
        Module :: atom()
      , Type :: atom()
      , Arity :: non_neg_integer()
      , Path :: [atom()]
    }) -> klsn:maybe(klsn:binstr()).
lookup_field_description({Module, Type, Arity, Path}) ->
    Attributes = Module:module_info(attributes),
    Res = lists:filtermap(fun
        ({gpte_type_description, List})->
            Descriptions = lists:filtermap(fun
                ({{Type0, Arity0}, Path0, Description}) when Type0 =:= Type, Arity0 =:= Arity, Path0 =:= Path ->
                    {true, Description};
                (_) ->
                    false
            end, List),
            case Descriptions of
                [] ->
                    false;
                _ ->
                    {true, Descriptions}
            end;
        (_) ->
            false
    end, Attributes),
    case Res of
        [[Value|_]|_] ->
            {value, Value};
        _ ->
            none
    end.

