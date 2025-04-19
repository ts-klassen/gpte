-module(gpte_functions).

% module deprecated. use gpte_tools.
-deprecated([
        new/3
      , new/4
      , new/5
      , name/1
      , name/2
      , description/1
      , description/2
      , state/1
      , state/2
      , upsert_property/5
      , delete_property/2
      , call/2
    ]).

-export([
        new/3
      , new/4
      , new/5
      , name/1
      , name/2
      , description/1
      , description/2
      , state/1
      , state/2
      , upsert_property/5
      , delete_property/2
      , call/2
    ]).

-export_type([
        choice/0
      , key/0
      , value/0
      , type/0
      , args/0
      , state/0
      , body/0
      , func/0
    ]).

-opaque choice() :: #{
        name := unicode:unicode_binary()
      , description := unicode:unicode_binary()
      , properties := #{
              % p_name() => #{type:=type(), description:=description()}
          }
      , required := sets:set()
      , function := func()
      , state := state()
    }.

-type key()   :: atom().
-type value() :: unicode:unicode_binary() | number().
-type type()  :: string | integer | number | boolean.
-type args()  :: maps:map(key(), value()).
-type state() :: term().
-type body()  :: unicode:unicode_binary().
-type func()  :: fun((args(), state()) -> {body(), state()} | body()).

-spec new(
        Name        :: atom()
      , Description :: unicode:unicode_binary()
      , Function    :: func()
    ) -> choice().
new(Name, Desc, Func) ->
    #{
        name => Name
      , description => Desc
      , properties => #{}
      , required => sets:new([{version, 2}])
      , function => Func
      , state => []
    }.

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
    ) -> choice().
new(Name, Desc, Func, Properties) ->
    Choice0 = new(Name, Desc, Func),
    lists:foldl(fun(Property, Choice)-> io:format("~p~n", [Property]),
        {
            Type
          , PName
          , Description
          , Required
        } = Property,
        upsert_property(
            Type
          , PName
          , Description
          , Required
          , Choice
        )
    end, Choice0, Properties).

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
new(Name, Desc, Func, Properties, State) ->
    Choice0 = new(Name, Desc, Func, Properties),
    state(State, Choice0).

-spec upsert_property(
        Type        :: type()
      , Name        :: key()
      , Description :: unicode:unicode_binary()
      , Required    :: boolean()
      , choice()
    ) -> choice().
upsert_property(Type, Name, Desc, IsReq, #{required:=ReqSet0}=Choice0) ->
    Choice = klsn_map:upsert([properties, Name], #{
            type=>Type, description=>Desc
        }, Choice0),
    ReqSet = case IsReq of
        true ->
            sets:add_element(Name, ReqSet0);
        false ->
            sets:del_element(Name, ReqSet0)
    end,
    klsn_map:upsert([required], ReqSet, Choice).

-spec delete_property(key(), choice()) -> choice().
delete_property(_Name, Choice) ->
    % FIXME
    Choice.

-spec call(
        unicode:unicode_binary(), choice()
    ) -> {unicode:unicode_binary(), choice()}.
call(JSONArgs, Choice0) ->
    try call_(JSONArgs, Choice0) of
        {Body, Choice} when is_binary(Body)->
            Res = #{
                sccess => true
              , payload => Body
            },
            {jsone:encode(Res), Choice};
        {Body, Choice} ->
            Res = #{
                sccess => false
              , reaseon => <<"Response was not an Erlang binary">>
              , detail => list_to_binary(io_lib:format("~p", [Body]))
            },
            {jsone:encode(Res), Choice}
    catch
        throw:#{reason:=Reason, detail:=Detail, choice:=Choice} ->
            Res = #{
                sccess => false
              , reaseon => list_to_binary(io_lib:format("~p", [Reason]))
              , detail => list_to_binary(io_lib:format("~p", [Detail]))
            },
            {jsone:encode(Res), Choice};
        throw:#{reason:=Reason, detail:=Detail} ->
            Res = #{
                sccess => false
              , reaseon => list_to_binary(io_lib:format("~p", [Reason]))
              , detail => list_to_binary(io_lib:format("~p", [Detail]))
            },
            {jsone:encode(Res), Choice0};
        Class:Detail ->
            Res = #{
                sccess => false
              , reaseon => list_to_binary(io_lib:format("Erlang ~p", [Class]))
              , detail => list_to_binary(io_lib:format("~p", [Detail]))
            },
            {jsone:encode(Res), Choice0}
    end.

-spec call_(
        unicode:unicode_binary(), choice()
    ) -> {unicode:unicode_binary(), choice()}.
call_(JSONArgs, #{function:=Func, state:=State0}=Choice0) ->
    Args = json_to_args(JSONArgs, Choice0),
    {Body, State} = case Func(Args, State0) of
        {B, S} when is_binary(B) ->
            {B, S};
        B ->
            {B, State0}
    end,
    Choice = Choice0#{state:=State},
    {Body, Choice}.

-spec json_to_args(unicode:unicode_binary(), choice()) -> args().
json_to_args(JSONArgs, #{properties:=Properties}) ->
    Args = jsone:decode(JSONArgs),
    maps:filtermap(fun(Name, #{type:=Type})->
        case klsn_map:lookup([atom_to_binary(Name)], Args) of
            none ->
                false;
            {value, Value} ->
                check_type(Type, Value),
                {true, Value}
        end
    end, Properties);
json_to_args(_, _) ->
    #{}.

-spec check_type(type(), term()) -> ok.
check_type(string, Value) when is_binary(Value) ->
    ok;
check_type(integer, Value) when is_integer(Value) ->
    ok;
check_type(number, Value) when is_number(Value) ->
    ok;
check_type(boolean, Value) when is_boolean(Value) ->
    ok;
check_type(Type, Value) ->
    throw(#{
        reason => badarg
      , detail => list_to_binary(io_lib:format(
            "~p does not match type ~p."
          , [Value, Type]
        ))
    }).

-spec name(choice()) -> klsn:maybe(unicode:unicode_binary()).
name(Choice) ->
    klsn_map:lookup([name], Choice).

-spec name(unicode:unicode_binary(), choice()) -> choice().
name(Name, Choice) ->
    klsn_map:upsert([name], Name, Choice).

-spec description(choice()) -> klsn:maybe(unicode:unicode_binary()).
description(Choice) ->
    klsn_map:lookup([description], Choice).

-spec description(unicode:unicode_binary(), choice()) -> choice().
description(Description, Choice) ->
    klsn_map:upsert([description], Description, Choice).

-spec state(choice()) -> klsn:maybe(unicode:unicode_binary()).
state(Choice) ->
    klsn_map:lookup([state], Choice).

-spec state(unicode:unicode_binary(), choice()) -> choice().
state(State, Choice) ->
    klsn_map:upsert([state], State, Choice).



