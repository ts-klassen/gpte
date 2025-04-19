# Using function tools with `chat_gpte`

`chat_gpte` supports OpenAI’s *function‑calling / tools* interface that allows the model to decide
when to run an arbitrary Erlang function and use its return value as part of the
conversation.  This document shows how to register Erlang functions as tools and
attach them to a conversation.

> The former `chat_gpte:function/2` helper (now marked *deprecated*) only covers
> the legacy "functions" field.  The examples below use the current
> `chat_gpte:tools/2` API that wraps the new unified *tools* field introduced by
> OpenAI.

---

## 1. Export a typed Erlang function

Every function you want to expose to the model **must**

1. be exported,
2. take **exactly one** argument, and
3. return `unicode:unicode_binary()/0` (a UTF‑8 encoded binary).

The single argument’s type **has to be declared** in the module’s `-spec` using
either a *user type* from the same module or a *remote type* (i.e. a type
exported from another module).  The type will be automatically converted into a
JSON Schema which is sent to OpenAI.

### Allowed Erlang types

`gpte_schema` does **not** yet expand user‑defined aliases.  Stick to the
following core types (and any combination / nesting of them):

* `unicode:unicode_binary()` – a UTF‑8 encoded binary string
* `klsn:binstr()` or `klsn_binstr:binstr()` – aliases of `unicode:unicode_binary()`
* `integer()` – an integer
* `number()` / `float()` – a floating‑point or integral number
* `boolean()` – `true` or `false`
* Enumerations written as a union of **two or more** atoms, e.g. `red | green | blue`
* Lists `[Type]`
* Maps `#{ key := Type, optional_key => Type2 }` (exact fields use `:=`,
  optional fields use `=>`)

You can freely nest these constructs.  If you need richer schemas consider
expanding them manually or waiting until alias support is added.

```erlang
%%% my_domotics.erl
-module(my_domotics).
-export([list_rooms/1, update_color/1]).

%% -- Types -------------------------------------------------------------------

-type list_rooms_args() :: #{ house_id := integer() }.
-gpte_type_description({list_rooms_args/0, <<"Arguments for list_rooms/1">>}).

-gpte_field_description([
    {list_rooms_args/0, [], <<"Required parameters for listing rooms.">>},
    {list_rooms_args/0, [house_id], <<"Identifier of the house.">>}
]).


-type update_color_args() :: #{
        house_id := integer()
      , room_id  := integer()
      , color    := unicode:unicode_binary()
    }.

-gpte_type_description({update_color_args/0, <<"Arguments for update_color/1">>} ).

-gpte_field_description([
    {update_color_args/0, [], <<"Parameters required to change the lighting colour.">>},
    {update_color_args/0, [house_id], <<"House identifier.">>},
    {update_color_args/0, [room_id],  <<"Room identifier inside the house.">>},
    {update_color_args/0, [color],    <<"Colour name – leave empty for default white.">>}
]).

%% -- Function descriptions ----------------------------------------------------

-gpte_function_description([
    {{list_rooms, 1},  <<"Return the list of rooms in the given house.">>},
    {{update_color,1},  <<"Change the lighting colour of a room.">>}
]).

%% -- Implementations ----------------------------------------------------------

-spec list_rooms(list_rooms_args()) -> unicode:unicode_binary().
list_rooms(#{house_id := 42}) ->
    jsone:encode([
        #{id => 1, name => <<"living room">>},
        #{id => 2, name => <<"bedroom">>}
    ]).

-spec update_color(update_color_args()) -> unicode:unicode_binary().
update_color(#{house_id := 42, room_id := 2, color := Color}) ->
    iolist_to_binary(io_lib:format(
        "Room colour changed to ~ts.",[Color])).
```

`gpte_tools` parses the type information **at runtime** by inspecting the BEAM
file, so you do **not** need to generate any schema yourself.

---

## 2. Build a `gpte_tools:tools()` value

`gpte_tools:new/0` starts with an empty tools map.  You can then add exported
functions in bulk via `gpte_tools:add_functions/2`:

```erlang
Tools0 = gpte_tools:new(),
Tools  = gpte_tools:add_functions([
            {my_domotics, list_rooms, 1},
            {my_domotics, update_color, 1}
        ], Tools0).
```

Each tuple is `{Module, FunctionName, Arity}`.  The helper will:

* look up the `-spec`,
* check that the function returns a UTF‑8 binary,
* convert the argument type to JSON Schema,
* copy the description provided by `-gpte_function_description`, and
* store a callback fun inside the tools map.

You can also construct the map manually if you need fine‑grained control –
see `src/gpte_tools.erl` for the expected structure.

---

## 3. Attach the tools to a chat

```erlang
Chat0 = chat_gpte:new(),
Chat1 = chat_gpte:system(
    <<"You are an AI assistant living in house id 42.">>, Chat0),
Chat2 = chat_gpte:tools(Tools, Chat1).
```

From now on every call to `chat_gpte:ask/2` will include the tool
specification.  When the model decides that one of the registered tools should
be invoked, `chat_gpte` will

1. decode the JSON arguments,
2. call the Erlang function, and
3. inject the function’s return value back into the conversation as a
   `tool`‑role message.

---

## 4. Full example session (shell)

```erlang
Tools  = gpte_tools:add_functions([
            {my_domotics, list_rooms, 1},
            {my_domotics, update_color, 1}
        ], gpte_tools:new()),
Chat0 = chat_gpte:new(),
Chat1 = chat_gpte:system(<<"You control the lights in house 42.">>, Chat0),
Chat2 = chat_gpte:tools(Tools, Chat1),
ok.

{Res1, Chat3} = chat_gpte:ask(<<"What rooms do I have?">>, Chat2),
io:format("~ts~n", [Res1]).

{Res2, Chat4} = chat_gpte:ask(<<"Make the bedroom blue.">>, Chat3),
io:format("~ts~n", [Res2]).
```

```
1> Tools  = gpte_tools:add_functions([
1>             {my_domotics, list_rooms, 1},
1>             {my_domotics, update_color, 1}
1>         ], gpte_tools:new()),
1> Chat0 = chat_gpte:new(),
1> Chat1 = chat_gpte:system(<<"You control the lights in house 42.">>, Chat0),
1> Chat2 = chat_gpte:tools(Tools, Chat1),
1> ok.
ok
2> 
2> {Res1, Chat3} = chat_gpte:ask(<<"What rooms do I have?">>, Chat2),
2> io:format("~ts~n", [Res1]).
You have the following rooms in house 42:

1. Living room
2. Bedroom
ok
3> 
3> {Res2, Chat4} = chat_gpte:ask(<<"Make the bedroom blue.">>, Chat3),
3> io:format("~ts~n", [Res2]).
The bedroom has been changed to blue.
ok
4>
```

---

## 5. Tips & gotchas

* `gpte_tools:add_functions/2` can only handle functions with **arity 1**.
* The argument type **must** be exported from its module; otherwise the helper
  cannot read it.
* When you specify `args_type` manually, `chat_gpte` will *validate* the JSON
  arguments against that type before calling your function.
* If you still need the old behaviour you can continue using
  `chat_gpte:function/2`, but new projects should migrate to `chat_gpte:tools/2`.

Enjoy building powerful GPT‑powered Erlang apps!
