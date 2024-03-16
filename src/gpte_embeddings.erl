-module(gpte_embeddings).

-export([
        new/0
      , model/1
      , model/2
      , embed/2
      , get_vector/1
      , lookup_vector/1
      , simple/1
      , simple/2
    ]).

-export_type([
        embeddings/0
      , vector/0
      , model/0
    ]).

-opaque embeddings() :: #{
    request => #{
            input => unicode:unicode_binary()
          , model := model()
        }
  , response => #{}  
    }.

-type vector() :: nonempty_list(float()).

-type model() :: unicode:unicode_binary().

-spec new() -> embeddings().
new() -> #{request=>#{
        model => <<"text-embedding-3-small">>
    }}.

-spec model(embeddings()) -> model().
model(#{request:=#{model:=Model}}) ->
    Model.

-spec model(model(), embeddings()) -> embeddings().
model(Model, Embeddings) ->
    klsn_map:upsert([request, model], Model, Embeddings).

-spec embed(unicode:unicode_binary(), embeddings()) -> embeddings().
embed(Input, Embeddings=#{request:=Request}) ->
    Embeddings#{
        response => gpte_api:embeddings(Request#{
                input => Input
            })
    }.

-spec lookup_vector(embeddings()) -> klsn:maybe(vector()).
lookup_vector(#{response:=#{<<"data">>:=[#{<<"embedding">>:=E}|_]}}) ->
    {value, E};
lookup_vector(_) ->
    none.

-spec get_vector(embeddings()) -> vector().
get_vector(Embeddings) ->
    klsn_maybe:get_value(lookup_vector(Embeddings)).
    
-spec simple(unicode:unicode_binary()) -> vector().
simple(Input) ->
    Embeddings10 = new(),
    Embeddings20 = embed(Input, Embeddings10),
    get_vector(Embeddings20).
    
-spec simple(unicode:unicode_binary(), model()) -> vector().
simple(Input, Model) ->
    Embeddings10 = model(Model, new()),
    Embeddings20 = embed(Input, Embeddings10),
    get_vector(Embeddings20).

