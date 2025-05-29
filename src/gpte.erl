-module(gpte).

-export([
        main/1
    ]).

main(Args) ->
    application:ensure_all_started(gpte),
    gpte_cli:main(Args).

