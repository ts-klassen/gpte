-module(chat_gpte).

-export([
        new/0
      , model/1
      , model/2
      , temperature/1
      , temperature/2
      , messages/1
      , messages/2
      , system/2
      , ask/2
      , function/2
    ]).

-export_type([
        chat/0
      , role/0
      , messages/0
    ]).

-opaque chat() :: #{
        request := #{
            model := unicode:unicode_binary()
          , temperature := number()
          , messages := [message_()]
          , functions => [function_()]
        }
      , functions => maps:map(unicode:unicode_binary(), gpte_functions:choice())
      , payloads => [gpte_api:payload()]
    }.

-type message_() :: #{
        role := role()
      , content := unicode:unicode_binary()
      , function_call => unicode:unicode_binary()
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

-type messages() :: [{role(), unicode:unicode_binary()} | reference()].


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
    send_message_(Message, Chat0).
    
-spec send_message_(message_(), chat()
    ) -> {unicode:unicode_binary(), chat()}.
send_message_(Message0, Chat0) ->
    Chat1 = message_(Message0, Chat0),
    Chat = request(Chat1),
    Message = message_(Chat),
    case Message of
        #{function_call:=FC} ->
            function_call_(FC, Chat);
        #{content:=Content} ->
            {Content, Chat}
    end.



-spec function_call_(map(), chat()) -> {unicode:unicode_binary(), chat()}.
function_call_(#{<<"name">>:=Name, <<"arguments">>:=Args}, Chat0) ->
    {value, Choice0} = klsn_map:lookup([functions, Name], Chat0),
    {Res, Choice} = gpte_functions:call(Args, Choice0),
    Message = #{
        role => function
      , name => Name
      , content => Res
    },
    Chat = klsn_map:upsert([functions, Name], Choice, Chat0),
    send_message_(Message, Chat).
    
    

-spec request(chat()) -> chat().
request(#{request:=Request} = Chat0) ->
    Payload = gpte_api:chat(Request),
    Chat1 = payload_(Payload, Chat0),
    case Payload of
        #{<<"choices">>:=[#{<<"message">>:=#{
            <<"function_call">>:=FC
          , <<"role">>:=<<"assistant">>
          , <<"content">>:=Content
        }}|_]} ->
            message_(#{
                role=>assistant, content=>Content, function_call=>FC
            }, Chat1);
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

-spec backup_messages(message_()|[message_()], chat()) -> chat().
backup_messages(Messages, Chat0) when is_list(Messages) ->
    lists:foldl(fun(Message, Chat)->
        backup_messages(Message, Chat)
    end, Chat0, Messages);
backup_messages(Message, Chat0) when is_map(Message) ->
    case klsn_map:lookup([message_backup, msg_to_ref, Message], Chat0) of
        {value, _} ->
            Chat0;
        none ->
            Ref = make_ref(),
            Chat1 = klsn_map:upsert([message_backup, msg_to_ref, Message], Ref, Chat0),
            klsn_map:upsert([message_backup, ref_to_msg, Ref], Message, Chat1)
    end.

-spec message_to_ref(message_(), chat()) -> reference().
message_to_ref(Message, Chat) ->
    {value, Ref} = klsn_map:lookup([message_backup, msg_to_ref, Message], Chat),
    Ref.

-spec ref_to_message(reference(), chat()) -> klsn:maybe(message_()).
ref_to_message(Ref, Chat) ->
    klsn_map:lookup([message_backup, ref_to_msg, Ref], Chat).

-spec messages(chat()) -> messages().
messages(Chat) ->
    case klsn_map:lookup([request, messages], Chat) of
        none -> [];
        {value, Messages} -> lists:map(fun
            (#{content:=null}=Message) ->
                message_to_ref(Message, Chat);
            (#{role:=Role, content:=Content}=M) when map_size(M)=:=2 ->
                {Role, Content};
            (Message) ->
                message_to_ref(Message, Chat)
        end, Messages)
    end.


-spec messages(messages(), chat()) -> chat().
messages(Messages0, Chat) ->
    Messages =lists:filtermap(fun
        (Ref) when is_reference(Ref) ->
            case ref_to_message(Ref, Chat) of
                none -> false;
                {value, Message} -> {true, Message}
            end;
        ({Role, Content}) ->
            {true, #{role=>Role, content=>Content}}
    end, Messages0),
    klsn_map:upsert([request, messages], Messages, Chat).

-spec system(unicode:unicode_binary(), chat()) -> chat().
system(Content, Chat) ->
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
message_(Message, Chat0) ->
    {value, Messages} = klsn_map:lookup([request, messages], Chat0),
    Chat = backup_messages(Message, Chat0),
    klsn_map:upsert([request, messages], [Message|Messages], Chat).

-spec function(
        gpte_functions:choice(), chat()
    ) -> chat().
function(Choice, #{request:=#{functions:=Functions}}=Chat0) ->
    #{
        name := Name
      , description := Description
      , properties := Properties
      , required := Required
    } = Choice,
    Function = #{
        name => Name
      , description => Description
      , parameters => #{
            type => object
          , properties => Properties
          , required => sets:to_list(Required)
        }
    },
    Chat1 = klsn_map:upsert([request, functions], [Function|Functions], Chat0),
    klsn_map:upsert([functions, atom_to_binary(Name)], Choice, Chat1);

function(Choice, Chat) ->
    function(Choice, klsn_map:upsert([request, functions], [], Chat)).

