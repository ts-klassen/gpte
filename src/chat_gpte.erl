-module(chat_gpte).

-deprecated([{function, 2}]).

-export([
        new/0
      , model/1
      , model/2
      , temperature/1
      , temperature/2
      , response_format/1
      , response_format/2
      , delete_response_format/1
      , schema/1
      , schema/2
      , messages/1
      , messages/2
      , system/2
      , ask/1
      , ask/2
      , ask_n/2
      , ask_n/3
      , function/2
      , tools/2
      , total_tokens/1
      , on_moderation_flagged/2
      , lookup_last_usage/1
      , get_last_usage/1
    ]).

-export_type([
        chat/0
      , role/0
      , messages/0
      , on_moderation/0
      , moderation_result/0
      , usage/0
    ]).

-opaque chat() :: #{
        request := #{
            model := unicode:unicode_binary()
          , temperature => number()
          , messages := [message_()]
          , functions => [function_()]
          , response_format => map()
          , tools => map()
        }
      , functions => maps:map(unicode:unicode_binary(), gpte_functions:choice())
      , tools => gpte_tools:tools()
      , payloads => [gpte_api:payload()]
      , on_moderation_flagged := on_moderation()
      , moderation_cache := #{unicode:unicode_binary()=>moderation_result()}
      , message_backup => #{
            msg_to_ref := maps:map(message_(), reference())
          , ref_to_msg := maps:map(reference(), message_())
        }
    }.

-type message_() :: #{
        role := role()
      , content := unicode:unicode_binary()
      , function_call => unicode:unicode_binary()
      , tool_calls => map()
      , tool_call_id => unicode:unicode_binary()
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

-type on_moderation() :: fun((moderation_result(), chat())->any()).

-type moderation_result() :: #{
        input := unicode:unicode_binary()
      , payload := gpte_api:payload()
    }.

-type usage() :: #{
        completion_tokens := non_neg_integer()
      , prompt_tokens := non_neg_integer()
      , total_tokens := non_neg_integer()
    }.

-spec new() -> chat().
new() ->
    #{
        request => #{
            model => <<"gpt-4o-mini">>
          , messages => []
        }
      , payloads => []
      , on_moderation_flagged => fun(_, _) ->
            erlang:error(prompt_potentially_harmful)
        end
      , moderation_cache => #{}
    }.

-spec ask(
        unicode:unicode_binary()
    ) -> unicode:unicode_binary().
ask(Question) ->
    {Res, _} = ask(Question, new()),
    Res.

-spec ask(
        unicode:unicode_binary(), chat()
    ) -> {unicode:unicode_binary(), chat()}.
ask(Question, Chat0) ->
    Message = #{
        role => user
      , content => Question
    },
    run_moderation(Question, fun(_, Chat10) ->
        hd(send_message_(Message, Chat10))
    end, Chat0).

-spec ask_n(
        unicode:unicode_binary(), non_neg_integer()
    ) -> [unicode:unicode_binary()].
ask_n(Question, N) ->
    {Answers, _Chats} = lists:unzip(ask_n(Question, N, new())),
    Answers.

-spec ask_n(
        unicode:unicode_binary(), non_neg_integer(), chat()
    ) -> [{unicode:unicode_binary(), chat()}].
ask_n(Question, N, Chat0) ->
    Message = #{
        role => user
      , content => Question
    },
    Chat10 = klsn_map:upsert([request, n], N, Chat0),
    Chats = run_moderation(Question, fun(_, Chat20) ->
        send_message_(Message, Chat20)
    end, Chat10),
    lists:map(fun({Ans, Chat})->
        {Ans, klsn_map:remove([request, n], Chat)}
    end, Chats).



-spec send_message_(message_() | [message_(), ...], chat()
    ) -> [{unicode:unicode_binary(), chat()}].
send_message_([Message], Chat0) ->
    send_message_(Message, Chat0);
send_message_([Message|Messages], Chat0) ->
    Chat1 = message_(Message, Chat0),
    send_message_(Messages, Chat1);
send_message_(Message0, Chat0) ->
    Chat1 = message_(Message0, Chat0),
    Chats = request(Chat1),
    ChatWithMsg = lists:map(fun(Chat) ->
        {message_(Chat), Chat}
    end, Chats),
    lists:map(fun
        ({#{function_call:=FC}, Chat}) ->
            function_call_(FC, Chat);
        ({#{tool_calls:=TC}, Chat}) ->
            tool_calls_(TC, Chat);
        ({#{content:=Content}, Chat}) ->
            {Content, Chat}
    end, ChatWithMsg).

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
    hd(send_message_(Message, Chat)).


-spec tool_calls_(map(), chat()) -> {unicode:unicode_binary(), chat()}.
tool_calls_(TC, Chat0) ->
    Messages = lists:map(fun(Call=#{<<"type">> := <<"function">>})->
        Id = maps:get(<<"id">>, Call),
        NameBinStr = klsn_map:get([<<"function">>, <<"name">>], Call),
        % This atom must exist because gpte_tools:name() is an atom.
        Name = binary_to_existing_atom(NameBinStr),
        Arguments = klsn_map:get([<<"function">>, <<"arguments">>], Call),
        Spec = klsn_map:get([tools, functions, Name], Chat0),
        Args = case Spec of
            #{args_type := ArgsType} ->
                gpte_schema:schema(ArgsType, Arguments);
            _ ->
                Arguments
        end,
        Fun = maps:get(callback, Spec),
        #{
            role => tool
          , tool_call_id => Id
          , content => Fun(Args)
        }
    end, TC),
    hd(send_message_(Messages, Chat0)).
    
    

-spec request(chat()) -> [chat()].
request(#{request:=Request} = Chat0) ->
    Payload = gpte_api:chat(Request),
    Chat1 = payload_(Payload, Chat0),
    #{<<"choices">>:=Choices} = Payload,
    lists:map(fun
        (#{<<"message">>:=#{
            <<"tool_calls">>:=TC
          , <<"role">>:=<<"assistant">>
          , <<"content">>:=Content
        }}) ->
            message_(#{
                role=>assistant, content=>Content, tool_calls=>TC
            }, Chat1);
        (#{<<"message">>:=#{
            <<"function_call">>:=FC
          , <<"role">>:=<<"assistant">>
          , <<"content">>:=Content
        }}) ->
            message_(#{
                role=>assistant, content=>Content, function_call=>FC
            }, Chat1);
        (#{<<"message">>:=#{
            <<"content">>:=Content,<<"role">>:=<<"assistant">>
        }}) ->
            message_(#{role=>assistant, content=>Content}, Chat1);
        (_) ->
            Chat1
    end, Choices).


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

-spec response_format(chat()) -> klsn:maybe(map()).
response_format(Chat) ->
    klsn_map:lookup([request, response_format], Chat).

-spec response_format(map(), chat()) -> chat().
response_format(Format, Chat) ->
    klsn_map:upsert([request, response_format], Format, Chat).

-spec delete_response_format(chat()) -> chat().
delete_response_format(Chat) ->
    Request = maps:get(request, Chat),
    klsn_map:upsert([request], maps:remove(response_format, Request), Chat).

-spec schema(chat()) -> klsn:maybe(gpte_schema:schema()).
schema(Chat) ->
    klsn_map:lookup([request, response_format, json_schema], Chat).

-spec schema(gpte_schema:schema(), chat()) -> chat().
schema(Schema, Chat0) ->
    Chat = klsn_map:upsert([request, response_format, type], json_schema, Chat0),
    klsn_map:upsert([request, response_format, json_schema], Schema, Chat).

-spec on_moderation_flagged(on_moderation(), chat()) -> chat().
on_moderation_flagged(Func, Chat) ->
    klsn_map:upsert([on_moderation_flagged], Func, Chat).

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
    run_moderation(Content, fun(_, Chat10) ->
        message_(Message, Chat10)
    end, Chat).

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

-spec tools(gpte_tools:tools(), chat()) -> chat().
tools(Tools, Chat0) ->
    Chat10 = Chat0#{tools => Tools},
    Functions = lists:map(fun({Name, Data}) ->
        #{
            type => function
          , function => #{
                name => Name
              , description => maps:get(description, Data, <<>>)
              , parameters => maps:get(parameters, Data)
            }
        }
    end, maps:to_list(maps:get(functions, Tools, #{}))),
    Chat20 = klsn_map:upsert([request, tools], Functions, Chat10),
    Chat20.

-spec total_tokens(chat()) -> non_neg_integer().
total_tokens(#{payloads:=Payload}) ->
    lists:foldl(fun
        (#{<<"usage">>:=#{<<"total_tokens">>:=TotalTokens}}, 0) ->
            TotalTokens;
        (_, Acc) ->
            Acc
    end, 0, Payload).

-spec lookup_last_usage(chat()) -> klsn:maybe(usage()).
lookup_last_usage(#{payloads:=[#{<<"usage">>:=#{
    <<"prompt_tokens">> := P
  , <<"total_tokens">> := T
  , <<"completion_tokens">> := C}}|_]}) ->
    {value, #{prompt_tokens => P
            , total_tokens => T
            , completion_tokens => C}};
lookup_last_usage(_) ->
    none.

-spec get_last_usage(chat()) -> usage().
get_last_usage(Chat) ->
    klsn_maybe:get_value(lookup_last_usage(Chat)).

-spec run_moderation(
        unicode:unicode_binary()
      , on_moderation()
      , chat()
    ) -> any().
run_moderation(Input, Right, #{on_moderation_flagged:=Left}=Chat0) ->
    {Res, Chat10} = case klsn_map:lookup([moderation_cache, Input], Chat0) of
        {value, Res0} ->
            {Res0, Chat0};
        none ->
            Payload = gpte_api:moderations(#{
                model => <<"omni-moderation-latest">>
              , input => Input
            }),
            Res0 = #{input => Input, payload => Payload},
            {
                Res0
              , klsn_map:upsert([moderation_cache, Input], Res0, Chat0)
            }
    end,
    case Res of
        #{payload:=#{<<"results">>:=[#{<<"flagged">>:=false}]}} ->
            Right(Res, Chat10);
        _ ->
            Left(Res, Chat10)
    end.

