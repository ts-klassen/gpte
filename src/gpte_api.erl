-module(gpte_api).

-export([
        request/2
      , chat/1
      , embeddings/1
      , image_generations/1
      , moderations/1
    ]).

-export_type([
        payload/0
      , chat/0
      , embeddings/0
      , image_generations/0
      , moderations/0
    ]).

-type payload() :: map() | list().

-type chat() :: #{
        model := unicode:unicode_binary()
      , messages := [map(), ...] % will be reversed to an array
    }.

-type embeddings() :: #{
        input := unicode:unicode_binary()
      , model := unicode:unicode_binary()
    }.

-type image_generations() :: #{
        prompt := unicode:unicode_binary()
      , model => unicode:unicode_binary()
      , n => non_neg_integer()
      , quality => hd
      , response_format => url | b64_json
      , size => unicode:unicode_binary()
      , style => null | vivid | natural
      , user => unicode:unicode_binary()
    }.

-type moderations() :: #{
        input := unicode:unicode_binary()
      , model := unicode:unicode_binary()
      }.

-spec chat(chat()) -> payload().
chat(#{messages:=Messages}=BodyMap) ->
    Url = <<"https://api.openai.com/v1/chat/completions">>,
    request(Url, BodyMap#{messages:=lists:reverse(Messages)}).

-spec embeddings(embeddings()) -> payload().
embeddings(RequestBody) ->
    Url = <<"https://api.openai.com/v1/embeddings">>,
    request(Url, RequestBody).

-spec image_generations(image_generations()) -> payload().
image_generations(RequestBody) ->
    Url = <<"https://api.openai.com/v1/images/generations">>,
    request(Url, RequestBody).

-spec moderations(moderations()) -> payload().
moderations(RequestBody) ->
    Url = <<"https://api.openai.com/v1/moderations">>,
    request(Url, RequestBody).

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
