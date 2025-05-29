%% -*- erlang -*-
-module(gpte_schema_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for gpte_schema module

schema_structure_test() ->
    S = gpte_schema:schema({gpte_schema, sample}),
    ?assertEqual(sample, maps:get(name, S)),
    ?assertEqual(<<"This is a sample type.">>, maps:get(description, S)),
    SchemaMap = maps:get(schema, S),
    ?assertEqual(object, maps:get(type, SchemaMap)),
    Props = maps:get(properties, SchemaMap),
    Keys = maps:keys(Props),
    ExpectedKeys = [unicode_field, binstr_field, klsn_binstr_field,
                    integer_field, number_field, boolean_field,
                    enum_field, map_field, array_field,
                    required_boolean_field],
    ?assertEqual(lists:sort(ExpectedKeys), lists:sort(Keys)),
    ?assertEqual(false, maps:get(additionalProperties, SchemaMap)),
    ?assertEqual([required_boolean_field], maps:get(required, SchemaMap)).

schema_parse_test() ->
    JSONMap = #{
        <<"unicode_field">> => <<"ユニコード"/utf8>>,
        <<"binstr_field">> => <<"binstr value">>,
        <<"klsn_binstr_field">> => <<"klsn_binstr value">>,
        <<"integer_field">> => 512,
        <<"number_field">> => 1.024,
        <<"boolean_field">> => false,
        <<"enum_field">> => <<"enum2">>,
        <<"map_field">> => #{ <<"field">> => true, <<"required_field">> => false },
        <<"array_field">> => [0,1,1,2,3,5],
        <<"required_boolean_field">> => true
    },
    JSON = jsone:encode(JSONMap),
    Parsed = gpte_schema:schema({gpte_schema, sample}, JSON),
    Expected = #{
        unicode_field => <<"ユニコード"/utf8>>,
        binstr_field => <<"binstr value">>,
        klsn_binstr_field => <<"klsn_binstr value">>,
        integer_field => 512,
        number_field => 1.024,
        boolean_field => false,
        enum_field => enum2,
        map_field => #{ field => true, required_field => false },
        array_field => [0,1,1,2,3,5],
        required_boolean_field => true
    },
    ?assertEqual(Expected, Parsed).

schema_property_descriptions_test() ->
    S = gpte_schema:schema({gpte_schema, sample}),
    SchemaMap = maps:get(schema, S),
    Props = maps:get(properties, SchemaMap),
    GP = fun(Name) -> maps:get(Name, Props) end,
    %% unicode_field
    UF = GP(unicode_field),
    ?assertEqual(string, maps:get(type, UF)),
    ?assertEqual(<<"field value must be `ユニコード`."/utf8>>, maps:get(description, UF)),
    %% binstr_field
    BF = GP(binstr_field),
    ?assertEqual(string, maps:get(type, BF)),
    ?assertEqual(<<"field value must be `binstr value`.">>, maps:get(description, BF)),
    %% klsn_binstr_field
    KBF = GP(klsn_binstr_field),
    ?assertEqual(string, maps:get(type, KBF)),
    ?assertEqual(<<"field value must be `klsn_binstr value`.">>, maps:get(description, KBF)),
    %% integer_field
    IF = GP(integer_field),
    ?assertEqual(integer, maps:get(type, IF)),
    ?assertEqual(<<"field value must be `512`.">>, maps:get(description, IF)),
    %% number_field
    NF = GP(number_field),
    ?assertEqual(number, maps:get(type, NF)),
    ?assertEqual(<<"field value must be `1.024`.">>, maps:get(description, NF)),
    %% boolean_field
    BoolF = GP(boolean_field),
    ?assertEqual(boolean, maps:get(type, BoolF)),
    ?assertEqual(<<"field value must be `false`.">>, maps:get(description, BoolF)),
    %% enum_field
    EF = GP(enum_field),
    ?assertEqual([enum1, enum2], maps:get(enum, EF)),
    ?assertEqual(<<"field value must be `enum2`.">>, maps:get(description, EF)),
    ?assert(not maps:is_key(type, EF)),
    %% map_field
    MF = GP(map_field),
    ?assertEqual(object, maps:get(type, MF)),
    ?assertEqual(<<"sample object with 2 fields."/utf8>>, maps:get(description, MF)),
    NProps = maps:get(properties, MF),
    ?assertEqual(lists:sort([field, required_field]), lists:sort(maps:keys(NProps))),
    Fld = maps:get(field, NProps),
    RV = maps:get(required_field, NProps),
    ?assertEqual(boolean, maps:get(type, Fld)),
    ?assertEqual(<<"field value must be `true`.">>, maps:get(description, Fld)),
    ?assertEqual(boolean, maps:get(type, RV)),
    ?assertEqual(<<"field value must be `false`.">>, maps:get(description, RV)),
    %% array_field
    AF = GP(array_field),
    ?assertEqual(array, maps:get(type, AF)),
    ?assertEqual(<<"array size must be exactly 6."/utf8>>, maps:get(description, AF)),
    Items = maps:get(items, AF),
    ?assertEqual(number, maps:get(type, Items)),
    ?assertEqual(<<"must be a fibonacci sequence."/utf8>>, maps:get(description, Items)),
    %% required_boolean_field
    RBF = GP(required_boolean_field),
    ?assertEqual(boolean, maps:get(type, RBF)),
    ?assertEqual(<<"field value must be `true`.">>, maps:get(description, RBF)).

json_schema_roundtrip_test() ->
    S = gpte_schema:schema({gpte_schema, sample}),
    Sch = maps:get(schema, S),
    JSON = jsone:encode(Sch),
    ?assert(is_binary(JSON)),
    Dec = jsone:decode(JSON),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Dec)),
    Props = maps:get(<<"properties">>, Dec),
    ExpectedBinKeys = [<<"unicode_field">>,<<"binstr_field">>,<<"klsn_binstr_field">>,
                       <<"integer_field">>,<<"number_field">>,<<"boolean_field">>,
                       <<"enum_field">>,<<"map_field">>,<<"array_field">>,
                       <<"required_boolean_field">>],
    ?assertEqual(lists:sort(ExpectedBinKeys), lists:sort(maps:keys(Props))),
    ?assertEqual(false, maps:get(<<"additionalProperties">>, Dec)),
    ?assertEqual([<<"required_boolean_field">>], maps:get(<<"required">>, Dec)),
    EnumProp = maps:get(<<"enum_field">>, Props),
    ?assertEqual([<<"enum1">>,<<"enum2">>], maps:get(<<"enum">>, EnumProp)),
    ?assert(not maps:is_key(<<"type">>, EnumProp)).