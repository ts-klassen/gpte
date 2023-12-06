-module(gpte_functions).

-export([
        new/3
      , name/1
      , name/2
      , description/1
      , description/2
      , upsert_properties/5
      , delete_properties/2
      , call/2
    ]).

-export_type([
        choice/0
      , key/0
      , value/0
      , type/0
      , args/0
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
    }.

-type key()   :: atom().
-type value() :: unicode:unicode_binary() | number().
-type type()  :: string | integer | number | boolean.
-type args()  :: maps:map(key(), value()).
-type func()  :: fun((args()) -> unicode:unicode_binary()).

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
    }.

-spec upsert_properties(
        Type        :: type()
      , Name        :: key()
      , Description :: unicode:unicode_binary()
      , Required    :: boolean()
      , choice()
    ) -> choice().
upsert_properties(Type, Name, Desc, IsReq, #{required:=ReqSet0}=Choice0) ->
    Choice = klsn_map:upsert([properties, Name], #{
            type=>Type, description=>Desc
        }, Choice0),
    ReqSet = case IsReq of
        true ->
            sets:add_element(Name, ReqSet0);
        false ->
            sets:delete_element(Name, ReqSet0)
    end,
    klsn_map:upsert([required], ReqSet, Choice).

-spec delete_properties(Name::atom(), choice()) -> choice().
delete_properties(_Name, Choice) ->
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
call_(JSONArgs, #{function:=Func}=Choice0) ->
    Args = json_to_args(JSONArgs, Choice0),
    Body = Func(Args),
    {Body, Choice0}.

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



