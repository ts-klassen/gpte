-module(gpte_codex_proto).

-export([
        mk_configure_session/1
      , mk_user_input/2
      , mk_user_input/3
      , mk_exec_approval/3
      , mk_interrupt/0
      , encode_op/1
      , encode_frame/1
      , frame/1
      , new_buf/0
      , decode_chunk/2
      , decode_chunk/3
      , decode_line/1
    ]).

-export_type([
        sub_id/0
      , response_id/0
      , user_input/0
      , session_opts/0
      , sandbox_policy/0
      , user_input_opts/0
      , approval_decision/0
      , call_id/0
      , env/0
      , event_type/0
      , session_configured_event/0
      , task_started_event/0
      , agent_message_event/0
      , exec_approval_request_event/0
      , exec_start_event/0
      , exec_stop_event/0
      , turn_complete_event/0
      , task_complete_event/0
      , error_event/0
      , unknown_event/0
      , event/0
      , configure_session_op/0
      , user_input_op/0
      , exec_approval_op/0
      , interrupt_op/0
      , op/0
      , line/0
      , decode_buf/0
      , decode_error/0
      , provider_info/0
    ]).

%% Types
-type sub_id() :: unicode:unicode_binary().
-type response_id() :: unicode:unicode_binary().
-type user_input() :: unicode:unicode_binary().
-type env() :: #{unicode:unicode_binary() => unicode:unicode_binary()}.
-type call_id() :: unicode:unicode_binary().
%% Provider information passed through to Codex CLI
-type provider_info() :: #{
        name := unicode:unicode_binary()
      , wire_api => unicode:unicode_binary()
    }.

%% Sandbox policy sent in ConfigureSession (internally tagged enum on CLI side)
-type sandbox_policy() :: #{
        mode := unicode:unicode_binary() % "read-only" | "workspace-write" | "danger-full-access"
      , network_access => boolean()
      , exclude_tmpdir_env_var => boolean()
      , exclude_slash_tmp => boolean()
    }.

%% Session options accepted by mk_configure_session/1
%% Note: These are flattened by the transport encoder prior to submission.
%% Required: model, workspace_dir
%% Optional: cwd, env, protocol_version, provider (binary name or provider object),
%%           model_reasoning_effort, model_reasoning_summary, approval_policy,
%%           sandbox_policy, model_supports_reasoning_summaries,
%%           disable_response_storage, file_opener
-type session_opts() :: #{
        model := unicode:unicode_binary()
      , workspace_dir := unicode:unicode_binary()
      , cwd => unicode:unicode_binary()
      , env => env()
      , protocol_version => pos_integer()
      , provider => unicode:unicode_binary() | provider_info() % name or provider object (e.g., #{name => <<"OpenAI">>, wire_api => <<"responses">>})
      , model_reasoning_effort => unicode:unicode_binary() % "low" | "medium" | "high" | "none"
      , model_reasoning_summary => unicode:unicode_binary() % "auto" | "concise" | "detailed" | "none"
      , approval_policy => unicode:unicode_binary() % "untrusted" | "on-failure" | "on-request" | "never"
      , sandbox_policy => sandbox_policy()
      , model_supports_reasoning_summaries => boolean()
      , disable_response_storage => boolean()
      , file_opener => unicode:unicode_binary() % "vscode" | "cursor" | "windsurf" | "none"
    }.
-type user_input_opts() :: #{
        last_response_id => response_id()
    }.
-type approval_decision() :: approved | approved_for_session | denied | abort.

-type event_type() ::
        session_configured
      | task_started
      | agent_message
      | exec_approval_request
      | exec_start
      | exec_stop
      | turn_complete
      | task_complete
      | error
      | unknown.

-type session_configured_event() :: #{
        type := session_configured
      , sub_id => sub_id()
      , payload => map() | null
    }.

-type task_started_event() :: #{
        type := task_started
      , sub_id := sub_id()
      , payload => map() | null
    }.

-type agent_message_event() :: #{
        type := agent_message
      , sub_id := sub_id()
      , payload := #{message := unicode:unicode_binary()}
    }.

-type exec_approval_request_event() :: #{
        type := exec_approval_request
      , sub_id := sub_id()
      , payload := #{
            command := unicode:unicode_binary()
          , cwd := unicode:unicode_binary()
          , env => env()
          , call_id => call_id()
        }
    }.

-type exec_start_event() :: #{
        type := exec_start
      , sub_id := sub_id()
      , payload => map() | null
    }.

-type exec_stop_event() :: #{
        type := exec_stop
      , sub_id := sub_id()
      , payload => map() | null
    }.

-type turn_complete_event() :: #{
        type := turn_complete
      , sub_id := sub_id()
      , payload := #{response_id := response_id()}
    }.

-type task_complete_event() :: #{
        type := task_complete
      , sub_id := sub_id()
      , payload => map() | null
    }.

-type error_event() :: #{
        type := error
      , sub_id := sub_id()
      , payload := #{reason := unicode:unicode_binary()}
    }.

-type unknown_event() :: #{
        type := unknown
      , sub_id => sub_id()
      , payload := #{raw := json_value()}
    }.

-type json_value() :: map() | list() | unicode:unicode_binary() | number() | boolean() | null.

-type event() ::
        session_configured_event()
      | task_started_event()
      | agent_message_event()
      | exec_approval_request_event()
      | exec_start_event()
      | exec_stop_event()
      | turn_complete_event()
      | task_complete_event()
      | error_event()
      | unknown_event().

-type configure_session_op() :: #{op := configure_session, session := session_opts()}.
-type user_input_op() :: #{
        op := user_input
      , sub_id := sub_id()
      , input := user_input()
      , last_response_id => response_id()
    }.
-type exec_approval_op() :: #{op := exec_approval, sub_id := sub_id(), id := call_id(), decision := approval_decision()}.
-type interrupt_op() :: #{op := interrupt}.
-type op() :: configure_session_op() | user_input_op() | exec_approval_op() | interrupt_op().
-type line() :: binary().
-type decode_buf() :: binary().
-type decode_error() :: invalid_json.

%% API

-spec mk_configure_session(session_opts()) -> op().
mk_configure_session(Session) ->
    %% Build Op::ConfigureSession
    #{op => configure_session, session => Session}.

%% Convenience overload without opts
-spec mk_user_input(sub_id(), user_input()) -> op().
mk_user_input(SubId, Input) ->
    mk_user_input(SubId, Input, #{}).

-spec mk_user_input(sub_id(), user_input(), user_input_opts()) -> op().
mk_user_input(SubId, Input, Opts) ->
    %% Build Op::UserInput (optionally include last_response_id)
    Base = #{op => user_input, sub_id => SubId, input => Input},
    case Opts of
        #{last_response_id := RespId} -> Base#{last_response_id => RespId};
        _ -> Base
    end.

-spec mk_exec_approval(sub_id(), call_id(), approval_decision()) -> op().
mk_exec_approval(SubId, CallId, Decision) when is_binary(CallId) ->
    #{op => exec_approval, sub_id => SubId, id => CallId, decision => Decision}.

-spec mk_interrupt() -> op().
mk_interrupt() ->
    #{op => interrupt}.

-spec encode_op(op()) -> iodata().
encode_op(Op) ->
    %% Ensure encoding errors are tagged, not raw jsone crashes
    try jsone:encode(Op, [native_utf8]) of
        Enc -> Enc
    catch
        Class:Reason -> erlang:error({encode_op_failed, {Class, Reason}})
    end.

-spec encode_frame(op()) -> iodata().
encode_frame(Op) ->
    frame(encode_op(Op)).

-spec frame(iodata()) -> iodata().
frame(IoData) ->
    [IoData, <<"\n">>].

-spec new_buf() -> decode_buf().
new_buf() ->
    <<>>.

-spec decode_chunk(decode_buf(), binary()) -> {[event()], decode_buf()}.
decode_chunk(Buf, Data) ->
    decode_chunk(Buf, Data, #{on_invalid => drop}).

-spec decode_chunk(decode_buf(), binary(), #{on_invalid := drop | unknown}) -> {[event()], decode_buf()}.
decode_chunk(Buf, Data, Opts) ->
    Bin = <<Buf/binary, Data/binary>>,
    {Lines, Rest} = split_lines(Bin),
    Events = lists:reverse(lists:foldl(fun(Line0, Acc) ->
        Line = strip_cr(Line0),
        case Line of
            <<>> -> Acc; % ignore empty lines
            _ ->
                case decode_line(Line) of
                    {ok, Ev} -> [Ev | Acc];
                    {error, invalid_json} ->
                        case maps:get(on_invalid, Opts, drop) of
                            unknown -> [unknown_from(undefined, format_raw(Line)) | Acc];
                            drop -> Acc
                        end
                end
        end
    end, [], Lines)),
    {Events, Rest}.

-spec decode_line(line()) -> {ok, event()} | {error, decode_error()}.
decode_line(Line) ->
    try jsone:decode(Line) of
        Map when is_map(Map) ->
            %% Some Codex CLIs wrap events; unwrap when present.
            Map1 = case maps:get(<<"msg">>, Map, undefined) of
                Msg when is_map(Msg) -> Msg;
                _ -> Map
            end,
            Map2 = case maps:get(<<"event">>, Map1, undefined) of
                Ev when is_map(Ev) -> Ev;
                _ -> Map1
            end,
            Map3 = case maps:get(<<"op">>, Map2, undefined) of
                Op when is_map(Op) ->
                    case maps:is_key(<<"type">>, Op) of
                        true -> Op;
                        false -> Map2
                    end;
                _ -> Map2
            end,
            decode_event_map(Map3);
        Other ->
            {ok, unknown_from(undefined, Other)}
    catch
        _:_ -> {error, invalid_json}
    end.

%% Internal helpers -------------------------------------------------------

split_lines(Bin) when is_binary(Bin) ->
    Sz = byte_size(Bin),
    case Sz of
        0 -> {[], <<>>};
        _ ->
            [Rest|List] = lists:reverse(binary:split(Bin, <<"\n">>, [global])),
            {lists:reverse(List), Rest}
    end.

strip_cr(Line) when is_binary(Line), byte_size(Line) > 0 ->
    case binary:last(Line) of
        13 -> binary:part(Line, 0, byte_size(Line) - 1); % '\r'
        _ -> Line
    end;
strip_cr(Line) -> Line.

%% No further normalization beyond CR stripping.

decode_event_map(Map = #{<<"type">> := <<"session_configured">>}) ->
    Sub0 = maps:get(<<"sub_id">>, Map, undefined),
    Payload0 = maps:get(<<"payload">>, Map, #{}),
    case {Sub0, Payload0} of
        {undefined, P} when is_map(P); P =:= null ->
            {ok, #{type => session_configured, payload => P}};
        {SubId, P} when is_binary(SubId), is_map(P); is_binary(SubId), P =:= null ->
            {ok, #{type => session_configured, sub_id => SubId, payload => P}};
        _ ->
            {ok, unknown_from(Sub0, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"task_started">>, <<"sub_id">> := SubId}) when is_binary(SubId) ->
    Payload0 = maps:get(<<"payload">>, Map, #{}),
    case Payload0 of
        P when is_map(P) -> {ok, #{type => task_started, sub_id => SubId, payload => P}};
        null -> {ok, #{type => task_started, sub_id => SubId, payload => null}};
        _ -> {ok, unknown_from(SubId, Map)}
    end;
%% Some CLI variants omit sub_id/payload for task_started; normalize, but keep type safety
decode_event_map(Map = #{<<"type">> := <<"task_started">>}) ->
    Sub0 = maps:get(<<"sub_id">>, Map, undefined),
    SubOk = (Sub0 =:= undefined) orelse is_binary(Sub0),
    case {SubOk, maps:find(<<"payload">>, Map)} of
        {true, error} ->
            Ev0 = #{type => task_started, payload => #{}},
            {ok, add_sub(Ev0, Sub0)};
        {true, {ok, null}} ->
            Ev0 = #{type => task_started, payload => null},
            {ok, add_sub(Ev0, Sub0)};
        {true, {ok, P}} when is_map(P) ->
            Ev0 = #{type => task_started, payload => P},
            {ok, add_sub(Ev0, Sub0)};
        _ -> {ok, unknown_from(Sub0, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"agent_message">>, <<"sub_id">> := SubId, <<"payload">> := Payload}) when is_binary(SubId), is_map(Payload) ->
    case Payload of
        #{<<"message">> := Msg} when is_binary(Msg) ->
            {ok, #{type => agent_message, sub_id => SubId, payload => #{message => Msg}}};
        _ ->
            {ok, unknown_from(SubId, Map)}
    end;
%% Accept alternate shapes for agent_message without payload or sub_id
decode_event_map(Map = #{<<"type">> := <<"agent_message">>}) ->
    SubId = maps:get(<<"sub_id">>, Map, undefined),
    Msg0 = case maps:get(<<"payload">>, Map, undefined) of
        P when is_map(P) -> maps:get(<<"message">>, P, undefined);
        _ -> undefined
    end,
    Msg = case maps:get(<<"message">>, Map, undefined) of
        M1 when is_binary(M1) -> M1;
        _ -> Msg0
    end,
    case Msg of
        M2 when is_binary(M2) ->
            Ev0 = #{type => agent_message, payload => #{message => M2}},
            {ok, add_sub(Ev0, SubId)};
        _ -> {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"exec_approval_request">>, <<"sub_id">> := SubId, <<"payload">> := Payload}) when is_binary(SubId), is_map(Payload) ->
    Cmd = maps:get(<<"command">>, Payload, undefined),
    Cwd = maps:get(<<"cwd">>, Payload, undefined),
    case {Cmd, Cwd} of
        {C1, C2} when is_binary(C1), is_binary(C2) ->
            Event0 = #{type => exec_approval_request, sub_id => SubId, payload => #{command => Cmd, cwd => Cwd}},
            %% Optionally pass through call_id if present and binary
            Event1 = case maps:get(<<"call_id">>, Payload, undefined) of
                T when is_binary(T) ->
                    P0 = maps:get(payload, Event0),
                    Event0#{payload := P0#{call_id => T}};
                _ -> Event0
            end,
            case maps:is_key(<<"env">>, Payload) of
                true ->
                    Env = maps:get(<<"env">>, Payload),
                    case Env of
                        E when is_map(E) ->
                            case env_kv_binaries(E) of
                                true ->
                                    Payload0 = maps:get(payload, Event1),
                                    {ok, Event1#{payload := Payload0#{env => E}}};
                                false -> {ok, unknown_from(SubId, Map)}
                            end;
                        null -> {ok, Event1};
                        _ -> {ok, unknown_from(SubId, Map)}
                    end;
                false -> {ok, Event1}
            end;
        _ -> {ok, unknown_from(SubId, Map)}
    end;

%% Accept alternate shape where fields appear at top level (no payload wrapper)
%% Be permissive: construct payload from any recognized fields and return exec_approval_request.
decode_event_map(Map = #{<<"type">> := <<"exec_approval_request">>}) ->
    Sub0 = maps:get(<<"sub_id">>, Map, undefined),
    P0 = case maps:get(<<"payload">>, Map, undefined) of
        P when is_map(P) -> P;
        _ -> Map
    end,
    P1 = case maps:get(<<"command">>, P0, undefined) of
        C when is_binary(C) -> #{command => C};
        _ -> #{}
    end,
    P2 = case maps:get(<<"cwd">>, P0, undefined) of
        W when is_binary(W) -> P1#{cwd => W};
        _ -> P1
    end,
    P3 = case maps:get(<<"call_id">>, P0, undefined) of
        Id when is_binary(Id) -> P2#{call_id => Id};
        _ -> P2
    end,
    P4 = case maps:get(<<"env">>, P0, undefined) of
        E when is_map(E) -> case env_kv_binaries(E) of true -> P3#{env => E}; false -> P3 end;
        null -> P3;
        _ -> P3
    end,
    {ok, add_sub(#{type => exec_approval_request, payload => P4}, Sub0)};

decode_event_map(Map = #{<<"type">> := <<"exec_start">>, <<"sub_id">> := SubId}) when is_binary(SubId) ->
    Payload0 = maps:get(<<"payload">>, Map, #{}),
    case Payload0 of
        P when is_map(P) -> {ok, #{type => exec_start, sub_id => SubId, payload => P}};
        null -> {ok, #{type => exec_start, sub_id => SubId, payload => null}};
        _ -> {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"exec_stop">>, <<"sub_id">> := SubId}) when is_binary(SubId) ->
    Payload0 = maps:get(<<"payload">>, Map, #{}),
    case Payload0 of
        P when is_map(P) -> {ok, #{type => exec_stop, sub_id => SubId, payload => P}};
        null -> {ok, #{type => exec_stop, sub_id => SubId, payload => null}};
        _ -> {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"turn_complete">>, <<"sub_id">> := SubId, <<"payload">> := Payload}) when is_binary(SubId), is_map(Payload) ->
    case Payload of
        #{<<"response_id">> := RespId} when is_binary(RespId) ->
            {ok, #{type => turn_complete, sub_id => SubId, payload => #{response_id => RespId}}};
        _ ->
            {ok, unknown_from(SubId, Map)}
    end;

%% Accept alternate naming variants seen in diagrams
decode_event_map(Map = #{<<"type">> := <<"turn_completed">>, <<"sub_id">> := SubId, <<"payload">> := Payload}) when is_binary(SubId), is_map(Payload) ->
    case Payload of
        #{<<"response_id">> := RespId} when is_binary(RespId) ->
            {ok, #{type => turn_complete, sub_id => SubId, payload => #{response_id => RespId}}};
        _ ->
            {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"task_complete">>, <<"sub_id">> := SubId}) when is_binary(SubId) ->
    Payload0 = maps:get(<<"payload">>, Map, #{}),
    case Payload0 of
        P when is_map(P) -> {ok, #{type => task_complete, sub_id => SubId, payload => P}};
        null -> {ok, #{type => task_complete, sub_id => SubId, payload => null}};
        _ -> {ok, unknown_from(SubId, Map)}
    end;
%% Accept alternate shape for task_complete with fields at top level
decode_event_map(Map = #{<<"type">> := <<"task_complete">>}) ->
    SubId = maps:get(<<"sub_id">>, Map, undefined),
    Payload0 = case maps:find(<<"payload">>, Map) of
        {ok, P0} -> P0;
        error -> maps:without([<<"type">>, <<"sub_id">>], Map)
    end,
    case Payload0 of
        P1 when is_map(P1) ->
            Ev0 = #{type => task_complete, payload => P1},
            {ok, add_sub(Ev0, SubId)};
        null ->
            Ev0 = #{type => task_complete, payload => null},
            {ok, add_sub(Ev0, SubId)};
        _ -> {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"task_completed">>, <<"sub_id">> := SubId}) when is_binary(SubId) ->
    Payload0 = maps:get(<<"payload">>, Map, #{}),
    case Payload0 of
        P when is_map(P) -> {ok, #{type => task_complete, sub_id => SubId, payload => P}};
        null -> {ok, #{type => task_complete, sub_id => SubId, payload => null}};
        _ -> {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map = #{<<"type">> := <<"error">>, <<"sub_id">> := SubId, <<"payload">> := Payload}) when is_binary(SubId), is_map(Payload) ->
    case Payload of
        #{<<"reason">> := Reason} when is_binary(Reason) ->
            {ok, #{type => error, sub_id => SubId, payload => #{reason => Reason}}};
        _ ->
            {ok, unknown_from(SubId, Map)}
    end;

decode_event_map(Map) when is_map(Map) ->
    %% Unknown type or shape
    {ok, unknown_from(maps:get(<<"sub_id">>, Map, undefined), Map)}.

%% No non-map branch; invalid JSON is handled before calling this function.

unknown_from(undefined, Raw) -> #{type => unknown, payload => #{raw => Raw}};
unknown_from(SubId, Raw) when is_binary(SubId) -> #{type => unknown, sub_id => SubId, payload => #{raw => Raw}};
unknown_from(_, Raw) -> #{type => unknown, payload => #{raw => Raw}}.

add_sub(Ev, SubId) when is_binary(SubId) -> Ev#{sub_id => SubId};
add_sub(Ev, _) -> Ev.

env_kv_binaries(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Acc andalso is_binary(K) andalso is_binary(V)
    end, true, Map).

%% Format raw payload safely for unknown events when JSON is invalid.
%% Ensures the 'raw' value is a UTF-8 binary; if not, hex-encode with a prefix.
format_raw(Bin) when is_binary(Bin) ->
    case catch unicode:characters_to_binary(Bin, utf8, utf8) of
        Utf8 when is_binary(Utf8) -> Utf8;
        _ -> <<"HEX:", (bytes_to_hex(Bin))/binary>>
    end;
format_raw(Other) ->
    %% Fallback stringify for non-binaries
    iolist_to_binary(io_lib:format("~p", [Other])).

bytes_to_hex(Bin) ->
    bytes_to_hex(Bin, []).

bytes_to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
bytes_to_hex(<<B, Rest/binary>>, Acc) ->
    Hi = hex_digit(B bsr 4),
    Lo = hex_digit(B band 16#0F),
    bytes_to_hex(Rest, [Lo, Hi | Acc]).

hex_digit(N) when N >= 0, N =< 9 -> $0 + N;
hex_digit(N) when N >= 10, N =< 15 -> $a + (N - 10).
