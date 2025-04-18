-module(gpte_tools).

-export_type([
        tools/0
    ]).

-export([
        
    ]).

-type tools() :: #{
        functions => #{
            name() => #{
                description := description()
              , parameters := parameters()
              , callback := fun((Args::any())->unicode:unicode_binary())
            }
        }
    }.

-type name() :: atom().

-type description() :: unicode:unicode_binary().

-type parameters() :: #{
        type := object
      , properties := maps:map(property_name(), property())
      , required := [property_name()]
      , additionalProperties := boolean()
    }.

-type property_name() :: atom().

-type property() :: #{
        type => object | array | string | integer | number | boolean
      , description => unicode:unicode_binary()
      , properties => maps:map(property_name(), property())
      , required => [property_name()]
      , additionalProperties => boolean()
      , items => property_name()
      , enum => atom()
    }.


-spec request_body(tools()) -> #{}.
request_body(Tools) ->
    #{}.

