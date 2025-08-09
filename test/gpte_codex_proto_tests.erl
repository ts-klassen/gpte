-module(gpte_codex_proto_tests).

-include_lib("eunit/include/eunit.hrl").

%% Basic constructors ----------------------------------------------------

configure_session_build_test() ->
    Session = #{model => <<"gpt-5-nano">>, workspace_dir => <<"/tmp">>, protocol_version => 1},
    Op = gpte_codex_proto:mk_configure_session(Session),
    ?assertMatch(#{op := configure_session, session := #{model := _, workspace_dir := _, protocol_version := _}}, Op).

user_input_build_test() ->
    Op = gpte_codex_proto:mk_user_input(<<"sub-1">>, <<"Hello">>, #{}),
    ?assertMatch(#{op := user_input, sub_id := <<"sub-1">>, input := <<"Hello">>}, Op),
    ?assertEqual(undefined, maps:get(last_response_id, Op, undefined)).

user_input_resume_build_test() ->
    Op = gpte_codex_proto:mk_user_input(<<"sub-2">>, <<"Continue">>, #{last_response_id => <<"resp-123">>}),
    ?assertMatch(#{op := user_input, sub_id := <<"sub-2">>, input := <<"Continue">>, last_response_id := <<"resp-123">>}, Op).

exec_approval_build_test() ->
    Allow = gpte_codex_proto:mk_exec_approval(<<"sub-3">>, allow),
    Deny = gpte_codex_proto:mk_exec_approval(<<"sub-3">>, deny),
    ?assertMatch(#{op := exec_approval, sub_id := <<"sub-3">>, decision := allow}, Allow),
    ?assertMatch(#{op := exec_approval, sub_id := <<"sub-3">>, decision := deny}, Deny).

interrupt_build_test() ->
    Op = gpte_codex_proto:mk_interrupt(),
    ?assertEqual(#{op => interrupt}, maps:from_list(maps:to_list(Op))).

%% Framing/encoding ------------------------------------------------------

frame_appends_newline_test() ->
    Data = <<"abc">>,
    Framed = gpte_codex_proto:frame(Data),
    ?assertEqual(<<"abc\n">>, iolist_to_binary(Framed)).

encode_returns_iodata_test() ->
    Op = #{op => configure_session, session => #{}},
    Io = gpte_codex_proto:encode_op(Op),
    ?assert(iolist_size(Io) > 0).

%% Decoding --------------------------------------------------------------

new_buf_is_binary_test() ->
    Buf = gpte_codex_proto:new_buf(),
    ?assert(is_binary(Buf)).

decode_line_valid_task_started_test() ->
    Line = <<"{\"type\":\"task_started\",\"sub_id\":\"s1\",\"payload\":{}}">>,
    ?assertMatch({ok, #{type := task_started, sub_id := <<"s1">>, payload := #{}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_invalid_json_test() ->
    Line = <<"{not json">>,
    ?assertEqual({error, invalid_json}, gpte_codex_proto:decode_line(Line)).

decode_line_agent_message_test() ->
    Line = <<"{\"type\":\"agent_message\",\"sub_id\":\"s2\",\"payload\":{\"message\":\"hi\"}}">>,
    ?assertMatch({ok, #{type := agent_message, sub_id := <<"s2">>, payload := #{message := <<"hi">>}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_exec_approval_request_test() ->
    Line = <<"{\"type\":\"exec_approval_request\",\"sub_id\":\"s3\",\"payload\":{\"command\":\"ls\",\"cwd\":\"/tmp\"}}">>,
    ?assertMatch({ok, #{type := exec_approval_request, sub_id := <<"s3">>, payload := #{command := <<"ls">>, cwd := <<"/tmp">>}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_exec_approval_request_with_env_test() ->
    Line = <<"{\"type\":\"exec_approval_request\",\"sub_id\":\"s3e\",\"payload\":{\"command\":\"echo\",\"cwd\":\"/tmp\",\"env\":{\"A\":\"1\"}}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(exec_approval_request, maps:get(type, Ev)),
    #{payload := P} = Ev,
    ?assertEqual(<<"1">>, maps:get(<<"A">>, maps:get(env, P), undefined)).

decode_line_exec_start_test() ->
    Line = <<"{\"type\":\"exec_start\",\"sub_id\":\"s4\",\"payload\":{}}">>,
    ?assertMatch({ok, #{type := exec_start, sub_id := <<"s4">>, payload := #{}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_task_started_payload_map_test() ->
    Line = <<"{\"type\":\"task_started\",\"sub_id\":\"ts2\",\"payload\":{\"phase\":\"p\"}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(task_started, maps:get(type, Ev)),
    ?assertEqual(<<"ts2">>, maps:get(sub_id, Ev)),
    #{payload := P} = Ev,
    ?assertEqual(<<"p">>, maps:get(<<"phase">>, P)).

decode_line_task_started_payload_null_test() ->
    Line = <<"{\"type\":\"task_started\",\"sub_id\":\"ts3\",\"payload\":null}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(task_started, maps:get(type, Ev)),
    ?assertEqual(<<"ts3">>, maps:get(sub_id, Ev)),
    ?assert(maps:is_key(payload, Ev)).

decode_line_task_started_payload_bad_type_test() ->
    Line = <<"{\"type\":\"task_started\",\"payload\":123}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_exec_start_payload_map_test() ->
    Line = <<"{\"type\":\"exec_start\",\"sub_id\":\"es1\",\"payload\":{\"v\":1}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(exec_start, maps:get(type, Ev)),
    ?assertEqual(<<"es1">>, maps:get(sub_id, Ev)),
    #{payload := P} = Ev,
    ?assertEqual(1, maps:get(<<"v">>, P)).

decode_line_exec_start_payload_null_test() ->
    Line = <<"{\"type\":\"exec_start\",\"sub_id\":\"es2\",\"payload\":null}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(exec_start, maps:get(type, Ev)),
    ?assertEqual(<<"es2">>, maps:get(sub_id, Ev)),
    ?assert(maps:is_key(payload, Ev)).

decode_line_exec_start_payload_bad_type_test() ->
    Line = <<"{\"type\":\"exec_start\",\"payload\":123}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_exec_stop_payload_map_test() ->
    Line = <<"{\"type\":\"exec_stop\",\"sub_id\":\"exs1\",\"payload\":{\"ok\":true}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(exec_stop, maps:get(type, Ev)),
    ?assertEqual(<<"exs1">>, maps:get(sub_id, Ev)),
    #{payload := P} = Ev,
    ?assertEqual(true, maps:get(<<"ok">>, P)).

decode_line_exec_stop_payload_null_test() ->
    Line = <<"{\"type\":\"exec_stop\",\"sub_id\":\"exs2\",\"payload\":null}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(exec_stop, maps:get(type, Ev)),
    ?assertEqual(<<"exs2">>, maps:get(sub_id, Ev)),
    ?assert(maps:is_key(payload, Ev)).

decode_line_exec_stop_payload_bad_type_test() ->
    Line = <<"{\"type\":\"exec_stop\",\"payload\":123}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_task_complete_payload_map_test() ->
    Line = <<"{\"type\":\"task_complete\",\"sub_id\":\"tc1\",\"payload\":{\"done\":true}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(task_complete, maps:get(type, Ev)),
    ?assertEqual(<<"tc1">>, maps:get(sub_id, Ev)),
    #{payload := P} = Ev,
    ?assertEqual(true, maps:get(<<"done">>, P)).

decode_line_task_complete_payload_null_test() ->
    Line = <<"{\"type\":\"task_complete\",\"sub_id\":\"tc2\",\"payload\":null}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(task_complete, maps:get(type, Ev)),
    ?assertEqual(<<"tc2">>, maps:get(sub_id, Ev)),
    ?assert(maps:is_key(payload, Ev)).

decode_line_task_complete_payload_bad_type_test() ->
    Line = <<"{\"type\":\"task_complete\",\"payload\":123}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_exec_stop_test() ->
    Line = <<"{\"type\":\"exec_stop\",\"sub_id\":\"s5\",\"payload\":{}}">>,
    ?assertMatch({ok, #{type := exec_stop, sub_id := <<"s5">>, payload := #{}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_turn_complete_test() ->
    Line = <<"{\"type\":\"turn_complete\",\"sub_id\":\"s6\",\"payload\":{\"response_id\":\"r-1\"}}">>,
    ?assertMatch({ok, #{type := turn_complete, sub_id := <<"s6">>, payload := #{response_id := <<"r-1">>}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_error_test() ->
    Line = <<"{\"type\":\"error\",\"sub_id\":\"s7\",\"payload\":{\"reason\":\"boom\"}}">>,
    ?assertMatch({ok, #{type := error, sub_id := <<"s7">>, payload := #{reason := <<"boom">>}}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_session_configured_test() ->
    Line = <<"{\"type\":\"session_configured\",\"sub_id\":\"sc1\"}">>,
    ?assertMatch({ok, #{type := session_configured, sub_id := <<"sc1">>}},
                 gpte_codex_proto:decode_line(Line)).

decode_line_session_configured_no_subid_test() ->
    Line = <<"{\"type\":\"session_configured\"}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(session_configured, maps:get(type, Ev)),
    ?assertEqual(undefined, maps:get(sub_id, Ev, undefined)).

decode_line_session_configured_payload_map_test() ->
    Line = <<"{\"type\":\"session_configured\",\"sub_id\":\"sc2\",\"payload\":{\"ok\":true}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(session_configured, maps:get(type, Ev)),
    #{payload := P} = Ev,
    ?assertEqual(true, maps:get(<<"ok">>, P)).

decode_line_session_configured_payload_null_test() ->
    Line = <<"{\"type\":\"session_configured\",\"payload\":null}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(session_configured, maps:get(type, Ev)),
    ?assert(maps:is_key(payload, Ev)).

decode_line_session_configured_payload_bad_type_test() ->
    Line = <<"{\"type\":\"session_configured\",\"payload\":123}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_unknown_type_test() ->
    Line = <<"{\"type\":\"new_thing\",\"sub_id\":\"sx\",\"payload\":{\"foo\":1}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)),
    ?assertEqual(<<"sx">>, maps:get(sub_id, Ev)).

decode_line_unknown_missing_sub_id_test() ->
    Line = <<"{\"type\":\"new_thing\",\"payload\":{}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)),
    ?assertEqual(undefined, maps:get(sub_id, Ev, undefined)).

decode_line_unknown_non_binary_sub_id_test() ->
    Line = <<"{\"type\":\"new_thing\",\"sub_id\":123,\"payload\":{}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)),
    %% sub_id should be omitted because it is non-binary
    ?assertEqual(undefined, maps:get(sub_id, Ev, undefined)).

%% Error/edge coverage ----------------------------------------------------

decode_line_agent_message_wrong_payload_test() ->
    %% message must be a binary; otherwise becomes unknown
    Line = <<"{\"type\":\"agent_message\",\"sub_id\":\"s8\",\"payload\":{\"message\":123}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_array_unknown_test() ->
    %% Non-map JSON should surface as unknown rather than invalid_json
    Line = <<"[1,2,3]">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_exec_approval_request_missing_field_test() ->
    %% missing cwd should downgrade to unknown
    Line = <<"{\"type\":\"exec_approval_request\",\"sub_id\":\"s9\",\"payload\":{\"command\":\"ls\"}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_exec_approval_request_bad_env_test() ->
    %% env values must be binaries; non-binary value downgrades to unknown
    Line = <<"{\"type\":\"exec_approval_request\",\"sub_id\":\"s10\",\"payload\":{\"command\":\"ls\",\"cwd\":\"/tmp\",\"env\":{\"A\":1}}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_chunk_ignores_invalid_line_test() ->
    %% one invalid JSON line between two valid lines, fed across two chunks and ignored
    L1 = <<"{\"type\":\"task_started\",\"sub_id\":\"x1\",\"payload\":{}}">>,
    L3 = <<"{\"type\":\"task_complete\",\"sub_id\":\"x1\",\"payload\":{}}">>,
    {E1, R1} = gpte_codex_proto:decode_chunk(gpte_codex_proto:new_buf(), <<L1/binary, "\n">>),
    ?assertEqual(1, length(E1)),
    {E2, R2} = gpte_codex_proto:decode_chunk(R1, <<"{bad json}\n", L3/binary, "\n">>),
    ?assertEqual(<<>>, R2),
    Types = [maps:get(type, X) || X <- E1 ++ E2],
    ?assertEqual([task_started, task_complete], Types).

decode_chunk_buffers_partial_line_test() ->
    %% split a single line across two chunks
    Part1 = <<"{\"type\":\"task_started\",\"sub_id\":\"p1\",\"payload\":{}">>,
    {Events0, Rest0} = gpte_codex_proto:decode_chunk(gpte_codex_proto:new_buf(), Part1),
    ?assertEqual([], Events0),
    ?assert(byte_size(Rest0) > 0),
    Part2 = <<"}\n">>,
    {Events1, Rest1} = gpte_codex_proto:decode_chunk(Rest0, Part2),
    ?assertEqual(1, length(Events1)),
    ?assertEqual(<<>>, Rest1).

decode_chunk_splits_lines_test() ->
    B1 = <<"{\"type\":\"task_started\",\"sub_id\":\"a\",\"payload\":{}}\n{\"type\":\"task_complete\",\"sub_id\":\"a\",\"payload\":{}}\n">>,
    {Events, Rest} = gpte_codex_proto:decode_chunk(gpte_codex_proto:new_buf(), B1),
    ?assertEqual(<<>>, Rest),
    ?assertEqual(2, length(Events)),
    ?assertMatch([#{type := task_started}, #{type := task_complete}], Events).

decode_chunk_crlf_lines_test() ->
    %% CRLF-delimited lines should decode and strip CR
    B = <<
        "{\"type\":\"task_started\",\"sub_id\":\"c1\",\"payload\":{}}\r\n"
        "{\"type\":\"task_complete\",\"sub_id\":\"c1\",\"payload\":{}}\r\n"
    >>,
    {Events, Rest} = gpte_codex_proto:decode_chunk(gpte_codex_proto:new_buf(), B),
    ?assertEqual(<<>>, Rest),
    ?assertEqual(2, length(Events)).

decode_chunk_empty_input_test() ->
    %% Empty input should yield no events and no rest
    {Events, Rest} = gpte_codex_proto:decode_chunk(gpte_codex_proto:new_buf(), <<>>),
    ?assertEqual([], Events),
    ?assertEqual(<<>>, Rest).

decode_chunk_empty_line_between_valid_test() ->
    %% An empty line between two valid lines should be ignored
    B = <<
        "{\"type\":\"task_started\",\"sub_id\":\"e1\",\"payload\":{}}\n"
        "\n"
        "{\"type\":\"task_complete\",\"sub_id\":\"e1\",\"payload\":{}}\n"
    >>,
    {Events, Rest} = gpte_codex_proto:decode_chunk(gpte_codex_proto:new_buf(), B),
    ?assertEqual(<<>>, Rest),
    Types = [maps:get(type, X) || X <- Events],
    ?assertEqual([task_started, task_complete], Types).

%% No quote-unescape behavior; valid JSON must be framed by real newlines.

decode_line_turn_complete_missing_response_id_test() ->
    %% Missing field should downgrade to unknown
    Line = <<"{\"type\":\"turn_complete\",\"sub_id\":\"u1\",\"payload\":{}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).

decode_line_error_missing_reason_test() ->
    %% Missing field should downgrade to unknown
    Line = <<"{\"type\":\"error\",\"sub_id\":\"u2\",\"payload\":{}}">>,
    {ok, Ev} = gpte_codex_proto:decode_line(Line),
    ?assertEqual(unknown, maps:get(type, Ev)).
