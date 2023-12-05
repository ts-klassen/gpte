-module(gpte_api).

-export([
        request/2
      , chat/1
    ]).

-export_type([
        payload/0
    ]).

-type payload() :: map() | list().

-type chat() :: #{
        model := unicode:unicode_binary()
      , messages := [map(), ...] % will be reversed to an array
    }.

-spec chat(chat()) -> payload().
chat(#{messages:=Messages}=BodyMap) ->
    Url = <<"https://api.openai.com/v1/chat/completions">>,
    Res0 = request(Url, BodyMap#{messages:=lists:reverse(Messages)}),
    
    Res0.

-spec request(uri_string:uri_string(), payload()) -> payload().
request(Url, BodyMap) ->
    request_(Url, BodyMap, 3).
request_(_, _, 0) ->
    error(too_many_retry);
request_(Url, BodyMap, ReTry) ->
    Body = jsone:encode(BodyMap),
    Headers = [{"Authorization", "Bearer " ++ os:getenv("OPENAI_API_KEY")}],
    Request = {Url, Headers, "application/json", Body},
    Options = [{body_format, binary}],
    Res = httpc:request(post, Request, [], Options),
    case Res of
        {ok, {{_,200,_}, _, ResBody}} ->
            jsone:decode(ResBody);
        Error ->
            error_logger:info_msg("openai error:~n~p~n", [Error]),
            timer:sleep(10000),
            request_(Url, BodyMap, ReTry-1)
    end.
