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
      , enum_field => enum1 | enum2 % must have more than one enum
      , map_field => #{
            field => boolean()
          , required_field := boolean()
        }
      , array_field => [number()]
      , required_boolean_field := boolean()
    }.
-gpte_type_description([
        {sample/0, [], <<"sample object with 10 fields.">>}
        {sample/0, [unicode_field], <<"field value must be `ユニコード`."/utf8>>}
      , {sample/0, [binstr_field], <<"field value must be `binstr value`.">>}
      , {sample/0, [klsn_binstr_field], <<"field value must be `klsn_binstr value`.">>}
      , {sample/0, [integer_field], <<"field value must be `512`.">>}
      , {sample/0, [number_field], <<"field value must be `1.024`.">>}
      , {sample/0, [boolean_field], <<"field value must be `false`.">>}
      , {sample/0, [enum_field], <<"field value must be `enum2`.">>}
      , {sample/0, [map_field], <<"sample object with 2 fields.">>}
      , {sample/0, [map_field, field], <<"field value must be `true`.">>}
      , {sample/0, [map_field, required_field], <<"field value must be `false`.">>}
      , {sample/0, [array_field], <<"field value must be `[1,1,2,3,5]`.">>}
      , {sample/0, [required_boolean_field], <<"field value must be `true`.">>}
    ]).

%% openai does not support tuple...
%% tuple_field => {boolean(), integer(), klsn:binstr()}

-spec schema(type()) -> schema().
schema(TypeSpec) ->
    Module = element(1, TypeSpec),
    TypeName = element(2, TypeSpec),
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(Module)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    Type = element(3, element(1, maps:get({type, TypeName, 0}, Types))),
    #{
        name => TypeName
      , schema => parse(Type)
    }.

-spec schema(type(), JSON::klsn:binstr()) -> term().
schema(TypeSpec, JSON) ->
    Module = element(1, TypeSpec),
    TypeName = element(2, TypeSpec),
    {ok, Concrete} = dialyzer_utils:get_core_from_beam(code:which(Module)),
    {ok, Types} = dialyzer_utils:get_record_and_type_info(Concrete),
    Type = element(3, element(1, maps:get({type, TypeName, 0}, Types))),
    parse(Type, jsone:decode(JSON)).


parse({type, _, map, Properties}) ->

    #{
        type => object
      , properties => maps:from_list(lists:map(fun
            ({type, _, map_field_assoc, [{atom, _, KeyName}, ValueType]})->
                {KeyName, parse(ValueType)};
            ({type, _, map_field_exact, [{atom, _, KeyName}, ValueType]})->
                {KeyName, parse(ValueType)};
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
parse({type, _, union, Args}) ->
    #{
        enum => lists:map(fun
            ({atom, _, Enum})->
                Enum;
            (UnknownType)->
                error(unknown_enum, [UnknownType])
        end, Args)
    };
parse({type, _, tuple, Args}) ->
    #{
        type => array
      , prefixItems => lists:map(fun(Item)->
            parse(Item)
        end, Args)
    };
parse({type, _, list, [Arg]}) ->
    #{ type => array, items => parse(Arg) };
parse({remote_type, _, [{atom,_,unicode}, {atom,_,unicode_binary}, []]}) ->
    #{ type => string };
parse({remote_type, _, [{atom, _, klsn}, {atom, _, binstr}, []]}) ->
    #{ type => string };
parse({remote_type, _, [{atom, _, klsn_binstr}, {atom, _, binstr}, []]}) ->
    #{ type => string };
parse({type, _, integer, _}) ->
    #{ type => integer };
parse({type, _, number, _}) ->
    #{ type => number };
parse({type, _, float, _}) ->
    #{ type => number };
parse({type, _, boolean, _}) ->
    #{ type => boolean };
parse(UnknownType) ->
    error(unknown_type, [UnknownType]).


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
parse({type, _, tuple, Args}, Data) ->
    erlang:list_to_tuple(lists:map(fun({Item, Elem})->
        parse(Item, Elem)
    end, lists:zip(Args, Data)), Data);
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

