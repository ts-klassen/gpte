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
              % if args_type specified, callback Args will be an erlang term.
              % if args_type is not specified, Args will be raw json binstr.
              , args_type => gpte_schema:type()
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
      , items => property()
      , enum => atom()
    }.


