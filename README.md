gpte
=====

An OTP application

Openai ChatGPT for Erlang

Build
-----

    $ rebar3 compile

minimal example
---------------
```
{Res, _} = chat_gpte:ask(<<"say this is a test">>, chat_gpte:new()),
Res.
```
```
1> {Res, _} = chat_gpte:ask(<<"say this is a test">>, chat_gpte:new()),
1> Res.
<<"This is a test.">>
```


How to Specify Response with Erlang Type Definition
---------------------

Here we have a type definition of `gpte_schema:sample/0`.

```erlang
-module(gpte_schema).

-export_type([sample/0]).

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
```

optionally, you can add description of the type and each field.

```erlang
-gpte_type_description({sample/0, <<"This is a sample type.">>}).
```

```erlang
-gpte_field_description([
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
```

We can specify the response to comply with `{gpte_schema, sample, 0}` (`gpte_schema:sample/0`) like this.

```
Chat0 = chat_gpte:new(),
Sample = {gpte_schema, sample, 0},
Chat1 = chat_gpte:schema(gpte_schema:schema(Sample), Chat0),
Chat2 = chat_gpte:system(<<"Answer in JSON format.">>, Chat1),
{Res, Chat3} = chat_gpte:ask(<<"Please fill out the sample.">>, Chat2),
gpte_schema:schema(Sample, Res).
```

```
1> Chat0 = chat_gpte:new(),
1> Sample = {gpte_schema, sample, 0},
1> Chat1 = chat_gpte:schema(gpte_schema:schema(Sample), Chat0),
1> Chat2 = chat_gpte:system(<<"Answer in JSON format.">>, Chat1),
1> {Res, Chat3} = chat_gpte:ask(<<"Please fill out the sample.">>, Chat2),
1> gpte_schema:schema(Sample, Res).
#{array_field => [0,1,1,2,3,5],
  binstr_field => <<"binstr value">>,boolean_field => false,
  enum_field => enum2,integer_field => 512,
  klsn_binstr_field => <<"klsn_binstr value">>,
  map_field => #{field => true,required_field => false},
  number_field => 1.024,required_boolean_field => true,
  unicode_field =>
      <<227,131,166,227,131,139,227,130,179,227,131,188,227,
        131,137>>}
```


JSON Schema example
---------------------

```
Schema = #{
    name => <<"user_info">>
  , schema => #{
        type => object
      , properties => #{
            username => #{ type => string }
          , first_name => #{ type => string }
          , last_name => #{ type => string }
          , age => #{ type => string }
        }
    }
},
Chat0 = chat_gpte:new(),
Chat1 = chat_gpte:schema(Schema, Chat0),
Chat2 = chat_gpte:system(<<"Answer in JSON format.">>, Chat1),
{Res, Chat3} = chat_gpte:ask(<<"Think of a random user_info.">>, Chat2),
io:format("~ts~n", [Res]).
```

```
1> Schema = #{
1>     name => <<"user_info">>
1>   , schema => #{
1>         type => object
1>       , properties => #{
1>             username => #{ type => string }
1>           , first_name => #{ type => string }
1>           , last_name => #{ type => string }
1>           , age => #{ type => string }
1>         }
1>     }
1> },
1> Chat0 = chat_gpte:new(),
1> Chat1 = chat_gpte:schema(Schema, Chat0),
1> Chat2 = chat_gpte:system(<<"Answer in JSON format.">>, Chat1),
1> {Res, Chat3} = chat_gpte:ask(<<"Think of a random user_info.">>, Chat2),
1> io:format("~ts~n", [Res]).
{"first_name":"John","last_name":"Doe","username":"johndoe123","age":"28"}
ok
```

Function Call Example (Deprecated)
----------------------------------

Use the tools function instead.

<details>
<summary>Deprecated Function Call Example</summary>

```
Chat0 = chat_gpte:new().
Function1 = gpte_functions:new(
    list_rooms
  , <<"list rooms in the house.">>
  , fun(#{house_id:=42}, _State) ->
        jsone:encode([
            #{id=>1, name=><<"main room">>}
          , #{id=>2, name=><<"klassen's room">>}
        ])
    end
  , [{
        integer
      , house_id
      , <<"id of the house">>
      , true
    }]
).
Function2 = gpte_functions:new(
    update_color
  , <<"change the lighting color of the selected room.">>
  , fun
        (#{house_id:=42, room_id:=2, color:=Color}, Color) ->
            throw(#{
                reason => same_color
              , detail => list_to_binary(io_lib:format(
                    "The room color is already ~s."
                  , [Color]
                ))
            });
        (#{house_id:=42, room_id:=2, color:=Color}, _State) ->
            Res = list_to_binary(io_lib:format(
                "The room color has been changed to ~s."
              , [Color]
            )),
            {Res, Color};
        (#{house_id:=42, room_id:=2}, white) ->
            throw(#{
                reason => same_color
              , detail => <<"The room color is already white.">>
            });
        (#{house_id:=42, room_id:=2}, _State) ->
            Res = <<"The room color has been changed to white.">>,
            {Res, white};
        (_, _) ->
            throw(#{
                reason => forbidden
              , detail => <<"You can't do anything to that room.">>
            })
    end
  , [
        {
            integer
          , house_id
          , <<"id of the house">>
          , true
        }
      , {
            integer
          , room_id
          , <<"id of the room">>
          , true
        }
      , {
            string
          , color
          , <<"name of the color to change. leave it empty for default.">>
          , false
        }
    ]
  , white
).
Chat1 = chat_gpte:function(Function1, Chat0).
Chat2 = chat_gpte:function(Function2, Chat1).
Chat3 = chat_gpte:system(<<"You are an ai assistant of klassen living at house id 42.">>, Chat2).
{Res1, Chat4} = chat_gpte:ask(<<"What is my room id?">>, Chat3).
{Res2, Chat5} = chat_gpte:ask(<<"Can you change my room color to blue?">>, Chat4).
{Res3, Chat6} = chat_gpte:ask(<<"Can you change my room color to blue again?">>, Chat5).
{Res4, Chat7} = chat_gpte:ask(<<"Can you change the main room color to blue?">>, Chat6).
{Res5, Chat8} = chat_gpte:ask(<<"Why can't you change the color?">>, Chat7).
io:format("~s~n~s~n~s~n~s~n~s~n", [Res1, Res2, Res3, Res4, Res5]).
```
```
12> io:format("~s~n~s~n~s~n~s~n~s~n", [Res1, Res2, Res3, Res4, Res5]).
Your room id is 2.
Sure! I have changed your room color to blue.
I'm sorry, but your room color is already blue. There is no need to change it again.
I'm sorry, but I am not able to change the color of the main room.
I apologize for the inconvenience. The main room color cannot be changed because it is not supported by the system or it may have certain restrictions set by the house owner.
ok
```

[Full output](./function_call_example.txt)

</details>

spec
----

### chat_gpte

#### new/0
```
-spec new() -> chat().
```

#### system/2
```
-spec system(unicode:unicode_binary(), chat()) -> chat().
```

#### function/2
```
-spec function(
        gpte_functions:choice(), chat()
    ) -> chat().
```

#### ask/2
```
-spec ask(
        unicode:unicode_binary(), chat()
    ) -> {unicode:unicode_binary(), chat()}.
```

#### messages/1
```
-spec messages(chat()) -> messages().
```

#### messages/2
```
-spec messages(messages(), chat()) -> chat().
```

### gpte_functions

#### new/5
```
-spec new(
        Name        :: atom()
      , Description :: unicode:unicode_binary()
      , Function    :: func()
      , Properties  :: [{
            Type        :: type()
          , Name        :: key()
          , Description :: unicode:unicode_binary()
          , Required    :: boolean()
        }]
      , State       :: state()
    ) -> choice().
```

type
----
### chat_gpte
```
-type role() :: system | assistant | user | function.

-type messages() :: [{role(), unicode:unicode_binary()} | reference()].
```

klsn
----
`klsn:maybe/1`
```
-type maybe(Value) :: {value, Value} | none.
```
