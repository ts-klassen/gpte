-module(gpte_cli).

-export([
        main/1
    ]).


main(["code" | Args]) ->
    code(Args);
main(["image" | Args]) ->
    image(Args);
main(Args) ->
    help(Args).


code(_) ->
    {ok, Cwd} = file:get_cwd(),
    Opt = #{dir => unicode:characters_to_binary(Cwd)
          , commit_all_messages => true},
    gpte_code:cli(Opt).


image(_) ->
    Chat0 = chat_gpte:new(),
    Chat10 = chat_gpte:model(<<"o3">>, Chat0),
    Tools0 = gpte_tools:new(),
    Tools10 = gpte_tools:add_functions([
        {gpte_image, v1_images_generations, 1}
    ], Tools0),
    Chat20 = chat_gpte:tools(Tools10, Chat10),
    chat(Chat20).

chat(Chat0) ->
    Chat10 = chat_gpte:on_moderation_flagged(fun(Reason, C)->
        klsn_io:format("prompt_potentially_harmful:~n~p~n", [Reason]),
        {<<"REJECTED: Your prompt is potentially harmful.">>, C}
    end, Chat0),
    resume_cli(Chat10).

help(_) ->
    klsn_io:format("WIP. Try running `gpte code` and type QUIT~n").



%% private sub functions

-spec resume_cli(chat_gpte:chat()) -> chat_gpte:chat().
resume_cli(Chat) ->
    case klsn_io:get_line(<<">> ">>) of
        <<"\n">> ->
            resume_cli(Chat);
        <<"QUIT\n">> ->
            Chat;
        <<"DATA\n">> ->
            resume_cli(ask(data(<<>>), Chat));
        User ->
            resume_cli(ask(User, Chat))
    end.

ask(Text, Chat10) ->
    {Res, Chat20} = chat_gpte:ask(Text, Chat10),
    klsn_io:format("ai: ~ts~n", [Res]),
    Chat20.

data(Data) ->
    case klsn_io:get_line(<<">> ">>) of
        <<".\n">> ->
            Data;
        <<"..", Part/binary>> ->
            data(<<".", Data/binary, Part/binary>>);
        Part ->
            data(<<Data/binary, Part/binary>>)
    end.

