-module(gpte_image).

-export([
        new_generations/0
      , model/1
      , model/2
      , generate/2
      , dir/1
      , dir/2
      , lookup_url/1
      , lookup_file/1
      , on_create/2
    ]).

-export_type([
        generations/0
      , model/0
      , on_create/0
    ]).

-opaque generations() :: #{
    request => #{
            input => unicode:unicode_binary()
          , model := model()
        }
  , response => #{}  
  , dir => unicode:unicode_binary()
  , file => unicode:unicode_binary()
  , on_create => on_create()
    }.

-type model() :: unicode:unicode_binary().

-type on_create() :: fun((Png::binary())->ok).

-spec new_generations() -> generations().
new_generations() -> #{
        request=>#{
            model => <<"dall-e-2">>
        }
    }.

-spec model(generations()) -> model().
model(#{request:=#{model:=Model}}) ->
    Model.

-spec model(model(), generations()) -> generations().
model(Model, Generations) ->
    klsn_map:upsert([request, model], Model, Generations).

-spec dir(generations()) -> unicode:unicode_binary().
dir(#{dir:=Dir}) ->
    Dir.

-spec dir(unicode:unicode_binary(), generations()) -> generations().
dir(Dir, Generations) ->
    klsn_map:upsert([dir], Dir, Generations).

-spec lookup_url(generations()) -> klsn:maybe(unicode:unicode_binary()).
lookup_url(#{response:=#{<<"data">>:=[#{<<"url">>:=Url}|_]}}) ->
    {value, Url};
lookup_url(_) ->
    none.

-spec lookup_file(generations()) -> klsn:maybe(unicode:unicode_binary()).
lookup_file(Generations) ->
    klsn_map:lookup([file], Generations).

-spec on_create(on_create(), generations()) -> generations().
on_create(OnCreate, Generations) ->
    Generations#{
        on_create => OnCreate
    }.

-spec generate(unicode:unicode_binary(), generations()) -> generations().
generate(Input, Generations0=#{request:=Request}) ->
    Generations10 = Generations0#{
        response => gpte_api:image_generations(Request#{
                prompt => Input
            })
    },
    #{response:=#{<<"data">>:=[#{<<"url">>:=Url}|_]}} = Generations10,
    Image = case Generations10 of
        #{dir:=_} ->
            data(Url);
        #{on_create:=_} ->
            data(Url);
        _ ->
            <<>>
    end,
    Generations20 = case Generations10 of
        #{dir:=Dir} ->
            Image = data(Url),
            try save(Dir, Url, Image) of
                Path ->
                    Generations10#{
                        file => Path
                    }
            catch
                _:_ ->
                    maps:remove(image, Generations10)
            end;
        _ ->
            maps:remove(image, Generations10)
    end,
    case Generations20 of
        #{on_create:=Func} ->
            ok = Func(Image);
        _ ->
            ok
    end,
    Generations20.

data(Url) ->
    {ok,{{_,200,_},_,Image}} = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
    Image.

save(Dir, Url, Image) ->
    {match, [{A,B}|_]} = re:run(Url, <<"/[^/]+\\?">>),
    Name = binary:part(Url, {A+1, B-2}),
    Path = filename:join(Dir, Name),
    ok = file:write_file(Path, Image),
    Path.

