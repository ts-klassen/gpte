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


comprehensive example
---------------------
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

[Full output](./comprehensive_example.txt)

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

klsn
----
`klsn:maybe/1`
```
-type maybe(Value) :: {value, Value} | none.
```
