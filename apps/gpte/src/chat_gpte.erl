-module(chat_gpte).

-export([
        new/0
      , model/1
      , model/2
      , temperature/1
      , temperature/2
      , system_message/2
      , ask/2
    ]).

-export_type([
        chat/0
      , role/0
    ]).

-opaque chat() :: #{
        request := #{
            model := unicode:unicode_binary()
          , temperature := number()
          , messages := [message_()]
          , functions => [function_()]
        }
      , functions => term()
      , payloads => [gpte_api:payload()]
    }.

-type message_() :: #{
        role := role()
      , content := unicode:unicode_binary()
      , name => unicode:unicode_binary()
    }.

-type function_() :: #{
        name := unicode:unicode_binary()
      , description := unicode:unicode_binary()
      , parameters := parameters_()
    }.

-type parameters_() :: #{
        type := object
      , properties := properties_()
      , required := [atom() | unicode:unicode_binary()]
    }.

-type properties_() :: map().

-type role() :: system | assistant | user | function.


-spec new() -> chat().
new() ->
    #{
        request => #{
            model => <<"gpt-3.5-turbo">>
          , temperature => 0.7
          , messages => []
        }
      , payloads => []
    }.

-spec ask(
        unicode:unicode_binary(), chat()
    ) -> {unicode:unicode_binary(), chat()}.
ask(Question, Chat0) ->
    Message = #{
        role => user
      , content => Question
    },
    Chat1 = message_(Message, Chat0),
    Chat = request(Chat1),
    {maps:get(content, message_(Chat)), Chat}.


-spec request(chat()) -> chat().
request(#{request:=Request} = Chat0) ->
    Payload = gpte_api:chat(Request),
    Chat1 = payload_(Payload, Chat0),
    case Payload of
        #{<<"choices">>:=[#{<<"message">>:=#{
            <<"content">>:=Content,<<"role">>:=<<"assistant">>
        }}|_]} ->
            message_(#{role=>assistant, content=>Content}, Chat1);
        _ ->
            Chat1
    end.


% Getters and Setters

-spec model(chat()) -> klsn:maybe(unicode:unicode_binary()).
model(Chat) ->
    klsn_map:lookup([request, model], Chat).

-spec model(unicode:unicode_binary(), chat()) -> chat().
model(Model, Chat) ->
    klsn_map:upsert([request, model], Model, Chat).

-spec temperature(chat()) -> klsn:maybe(number()).
temperature(Chat) ->
    klsn_map:lookup([request, temperature], Chat).

-spec temperature(number(), chat()) -> chat().
temperature(Temperature, Chat) ->
    klsn_map:upsert([request, temperature], Temperature, Chat).

-spec system_message(unicode:unicode_binary(), chat()) -> chat().
system_message(Content, Chat) ->
    Message = #{
        role => system
      , content => Content
    },
    message_(Message, Chat).

-spec payload_(chat()) -> gpte_api:payload().
payload_(Chat) ->
    {value, [Payload|_]} = klsn_map:lookup([payloads], Chat),
    Payload.

-spec payload_(gpte_api:payload(), chat()) -> chat().
payload_(Payload, Chat) ->
    {value, Payloads} = klsn_map:lookup([payloads], Chat),
    klsn_map:upsert([payloads], [Payload|Payloads], Chat).

-spec message_(chat()) -> message_().
message_(Chat) ->
    {value, [Message|_]} = klsn_map:lookup([request, messages], Chat),
    Message.

-spec message_(message_(), chat()) -> chat().
message_(Message, Chat) ->
    {value, Messages} = klsn_map:lookup([request, messages], Chat),
    klsn_map:upsert([request, messages], [Message|Messages], Chat).

