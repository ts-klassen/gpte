-module(gpte_cli).

-export([
        main/1
    ]).


main(["code" | Args]) ->
    code(Args);
main(Args) ->
    help(Args).


code(_) ->
    application:ensure_all_started(gpte),
    {ok, Cwd} = file:get_cwd(),
    Opt = #{dir => unicode:characters_to_binary(Cwd)
          , commit_all_messages => true},
    gpte_code:cli(Opt).


help(_) ->
    klsn_io:format("WIP. Try running `gpte code` and type QUIT~n").

