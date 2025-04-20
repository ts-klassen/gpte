-module(gpte_image).

-export([
        new_generations/0
      , model/1
      , model/2
      , generate/2
      , dir/1
      , dir/2
      , lookup_url/1
      , lookup_revised_prompt/1
      , lookup_file/1
      , on_create/2
      , v1_images_generations/1
    ]).

-export_type([
        generations/0
      , model/0
      , on_create/0
      , v1_images_generations_args/0
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


-gpte_type_description({v1_images_generations_args/0, <<"This is a sample type.">>}).
-type v1_images_generations_args() :: #{
        payload := #{
            prompt := klsn:binstr()
          , model => 'dall-e-2' | 'dall-e-3'
          , n => integer()
          , quality => standard | hd
          , size => '256x256' | '512x512' | '1024x1024' | '1792x1024' | '1024x1792'
          , style => vivid | natural
        }
    }.
-gpte_field_description([
        {v1_images_generations_args/0, [], <<"argument for erlang function gpte_image:v1_images_generations/1.">>}
      , {v1_images_generations_args/0, [payload], <<"Request body of post
https://api.openai.com/v1/images/generations">>}
      , {v1_images_generations_args/0, [payload, prompt], <<"A text description of the desired image(s). The maximum length is 1000 characters for dall-e-2 and 4000 characters for dall-e-3.">>}
      , {v1_images_generations_args/0, [payload, model], <<"The model to use for image generation.">>}
      , {v1_images_generations_args/0, [payload, n], <<"The number of images to generate. Must be between 1 and 10. For dall-e-3, only n=1 is supported.">>}
      , {v1_images_generations_args/0, [payload, quality], <<"The quality of the image that will be generated. hd creates images with finer details and greater consistency across the image. This param is only supported for dall-e-3.">>}
      , {v1_images_generations_args/0, [payload, size], <<"The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 for dall-e-2. Must be one of 1024x1024, 1792x1024, or 1024x1792 for dall-e-3 models.">>}
      , {v1_images_generations_args/0, [payload, style], <<"The style of the generated images. Must be one of vivid or natural. Vivid causes the model to lean towards generating hyper-real and dramatic images. Natural causes the model to produce more natural, less hyper-real looking images. This param is only supported for dall-e-3.">>}
    ]).


-gpte_function_description({v1_images_generations/1, <<"Creates an image given a prompt.">>}).
-gpte_function_option({v1_images_generations/1, #{
        json_reply => true
      , catch_exception => true
      , report_exception_detail => false
      , report_exception_detail_to_ai => true
    }}).


-spec v1_images_generations(v1_images_generations_args()) -> jsone:json_value().
v1_images_generations(#{payload := Payload}) ->
    Res = gpte_api:image_generations(Payload),
    #{<<"data">>:=[#{<<"url">>:=Url}|_]} = Res,
    {ok, Cwd} = file:get_cwd(),
    Path = binary_to_list(iolist_to_binary(save(Cwd, Url, data(Url)))),
    EscPath = string:replace(Path, "'", "'\"'\"'", [global]),
    os:cmd("eog '" ++ EscPath ++ "'&"),
    #{
        payload => Res
      , saved_path => iolist_to_binary(Path)
      , which_eog => iolist_to_binary(os:cmd("which eog"))
    }.


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

-spec lookup_revised_prompt(generations()) -> klsn:maybe(unicode:unicode_binary()).
lookup_revised_prompt(#{response:=#{<<"data">>:=[#{<<"revised_prompt">>:=Revised_prompt}|_]}}) ->
    {value, Revised_prompt};
lookup_revised_prompt(_) ->
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

