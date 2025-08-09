-module(gpte_codex_SUITE).

-include_lib("common_test/include/ct.hrl").

% TODO: test exec_approval and interrupt

-export([
        suite/0
      , all/0
      , init_per_suite/1
      , end_per_suite/1
      , init_per_testcase/2
      , end_per_testcase/2
      , smoke_open_close/1
      , session_user_input_flow/1
      , say_this_is_a_test/1
      , file_io_create_hello_md/1
    ]).

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [smoke_open_close, session_user_input_flow, say_this_is_a_test, file_io_create_hello_md].

init_per_suite(Cfg) ->
    case find_codex_program() of
        undefined -> {skip, "codex binary not found (set CODEX_BIN or ensure 'codex' on PATH)"};
        ProgBin ->
            [{codex_program, ProgBin} | Cfg]
    end.

end_per_suite(_Cfg) ->
    ok.

init_per_testcase(TC, Cfg) ->
    %% Prefer explicit workspace_dir from ct:get_config; otherwise create per-case dir under priv_dir
    Cfg1 = case ct:get_config(workspace_dir, undefined) of
        undefined ->
            Priv = proplists:get_value(priv_dir, Cfg),
            Ws0 = filename:join(Priv, atom_to_list(TC)),
            ok = ensure_dir(Ws0),
            Ws = unicode:characters_to_binary(Ws0),
            [{workspace_dir, Ws} | Cfg];
        Ws ->
            [{workspace_dir, unicode:characters_to_binary(Ws)} | Cfg]
    end,
    maybe_skip_network(TC, Cfg1).

end_per_testcase(_TC, _Cfg) ->
    ok.

%% Test cases -------------------------------------------------------------

smoke_open_close(Cfg) ->
    Prog = get_prog(Cfg),
    Ws = get_ws(Cfg),
    Args = get_args(),
    Env = get_env(),
    Opt = #{program => Prog, args => Args, env => Env, on_invalid => unknown, cwd => Ws, provider => #{ name => <<"OpenAI">>, wire_api => <<"responses">> }},
    C = gpte_codex:open(Opt),
    ok = gpte_codex:close(C).

session_user_input_flow(Cfg) ->
    Prog = get_prog(Cfg),
    Ws = get_ws(Cfg),
    Args = get_args(),
    Env = get_env(),
    Opt = #{program => Prog, args => Args, env => Env, on_invalid => unknown, cwd => Ws, provider => #{ name => <<"OpenAI">>, wire_api => <<"responses">> }},
    C0 = gpte_codex:open(Opt),
    Sess = #{
        model => <<"gpt-5-nano">>,
        workspace_dir => Ws,
        cwd => Ws,
        env => #{},
        protocol_version => 1,
        model_reasoning_effort => <<"medium">>,
        model_reasoning_summary => <<"none">>,
        approval_policy => <<"on-request">>,
        sandbox_policy => #{mode => <<"workspace-write">>, network_access => false}
    },
    ok = gpte_codex:configure_session(Sess, C0),
    ok = gpte_codex:user_input(<<"ct-1">>, <<"Hello from CT">>, C0),
    {Events, C1} = gpte_codex:recv_events(120000, C0),
    case is_list(Events) of
        true -> ok;
        false -> ct:fail({not_a_list, Events})
    end,
    case length(Events) >= 1 of
        true -> ok;
        false -> ct:fail({no_events, Events})
    end,
    Types = [maps:get(type, E) || E <- Events],
    ct:pal("received event types: ~p", [Types]),
    ok = gpte_codex:close(C1).

say_this_is_a_test(Cfg) ->
    Prog = get_prog(Cfg),
    Ws = get_ws(Cfg),
    Args = get_args(),
    Env = get_env(),
    Opt = #{program => Prog, args => Args, on_invalid => unknown, cwd => Ws, provider => #{ name => <<"OpenAI">>, wire_api => <<"responses">> }},
    C0 = gpte_codex:open(Opt),
    Sess = #{
        model => <<"gpt-5-nano">>,
        workspace_dir => Ws,
        cwd => Ws,
        env => #{},
        protocol_version => 1,
        model_reasoning_effort => <<"medium">>,
        model_reasoning_summary => <<"none">>,
        approval_policy => <<"on-request">>,
        sandbox_policy => #{mode => <<"workspace-write">>, network_access => false}
    },
    ok = gpte_codex:configure_session(Sess, C0),
    %% Standard API test prompt
    ok = gpte_codex:user_input(<<"ct-2">>, <<"Say this is a test">>, C0),
    Pattern = <<"this\\s+is\\s+a\\s+test!?">>,
    case gpte_codex:await_event(agent_message, 120000, C0) of
        {ok, Ev, C1} ->
            Msg = maps:get(message, maps:get(payload, Ev, #{}), <<>>),
            case re:run(Msg, Pattern, [caseless, dotall, multiline]) of
                {match, _} -> ok = gpte_codex:close(C1);
                nomatch -> ct:fail({no_phrase_found, Msg})
            end;
        {error, timeout, C1} ->
            %% Timeout; close and fail
            ok = gpte_codex:close(C1),
            ct:fail(no_phrase_found)
    end.

file_io_create_hello_md(Cfg) ->
    Prog = get_prog(Cfg),
    Ws = get_ws(Cfg),
    Args = get_args(),
    Env = get_env(),
    Opt = #{program => Prog, args => Args, env => Env, on_invalid => unknown, cwd => Ws, provider => #{ name => <<"OpenAI">>, wire_api => <<"responses">> }},
    C0 = gpte_codex:open(Opt),
    Sess = #{
        model => <<"gpt-5-nano">>,
        workspace_dir => Ws,
        cwd => Ws,
        env => #{},
        protocol_version => 1,
        model_reasoning_effort => <<"medium">>,
        model_reasoning_summary => <<"none">>,
        %% Avoid interactive approvals to keep the test deterministic
        approval_policy => <<"never">>,
        sandbox_policy => #{mode => <<"danger-full-access">>}
    },
    ok = gpte_codex:configure_session(Sess, C0),
    %% Prompt to create the file with content
    ok = gpte_codex:user_input(<<"ct-file-io">>, <<"Create hello.md with hello world">>, C0),
    %% Wait for the task_complete event
    case gpte_codex:await_event(task_complete, 120000, C0) of
        {ok, _Ev, C1} ->
            WsList = unicode:characters_to_list(Ws),
            Path = filename:join(WsList, "hello.md"),
            case file:read_file(Path) of
                {ok, Bin} ->
                    %% Accept: "hello world", "hello world!", and "hello, world!" (ignore case)
                    Patt = <<"hello\\s*,?\\s*world!?">>,
                    case re:run(Bin, Patt, [caseless]) of
                        {match, _} -> ok = gpte_codex:close(C1);
                        nomatch ->
                            ok = gpte_codex:close(C1),
                            ct:fail({hello_world_not_found, Bin})
                    end;
                {error, Reason} ->
                    ok = gpte_codex:close(C1),
                    ct:fail({file_not_found_or_unreadable, Path, Reason})
            end;
        {error, timeout, C1} ->
            ok = gpte_codex:close(C1),
            ct:fail(task_did_not_complete)
    end.

%% Helpers ----------------------------------------------------------------

get_prog(Cfg) ->
    case ct:get_config(codex_program, undefined) of
        undefined ->
            proplists:get_value(codex_program, Cfg);
        Prog -> unicode:characters_to_binary(Prog)
    end.

get_ws(Cfg) ->
    case ct:get_config(workspace_dir, undefined) of
        undefined ->
            case proplists:get_value(workspace_dir, Cfg) of
                undefined -> unicode:characters_to_binary(proplists:get_value(priv_dir, Cfg));
                Ws1 -> Ws1
            end;
        Ws -> unicode:characters_to_binary(Ws)
    end.

get_args() ->
    %% Only 'proto' is needed per CLI guidance.
    [<<"proto">>].

get_env() ->
    %% Collect select env vars from ct config or OS env.
    %% You can pass these in ct config as a map under 'codex_env'.
    case ct:get_config(codex_env, undefined) of
        M when is_map(M) -> normalize_env_map(M);
        _ ->
            Env0 = lists:filtermap(fun(K) ->
                case os:getenv(K) of
                    false -> false;
                    V -> {true, {K, V}}
                end
            end, ["OPENAI_API_KEY", "OPENAI_BASE_URL", "ANTHROPIC_API_KEY", "ANTHROPIC_BASE_URL"]),
            maps:from_list([{unicode:characters_to_binary(K), unicode:characters_to_binary(V)} || {K, V} <- Env0])
    end.

normalize_env_map(M) ->
    maps:from_list([
        {unicode:characters_to_binary(K), unicode:characters_to_binary(V)}
        || {K, V} <- maps:to_list(M)
    ]).

%% No internal await/matching helpers; rely on gpte_codex:await_agent_message/2

find_codex_program() ->
    %% Order of precedence:
    %% 1) ct:get_config(codex_program | codex_bin)
    %% 2) CODEX_BIN env var
    %% 3) find 'codex' on PATH
    case ct:get_config(codex_program, ct:get_config(codex_bin, undefined)) of
        Prog when is_list(Prog); is_binary(Prog) -> unicode:characters_to_binary(Prog);
        undefined ->
            case os:getenv("CODEX_BIN") of
                false ->
                    case os:find_executable("codex") of
                        false -> undefined;
                        Path -> unicode:characters_to_binary(Path)
                    end;
                Path0 ->
                    unicode:characters_to_binary(Path0)
            end
    end.

ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> filelib:ensure_dir(filename:join(Dir, ".keep")), file:make_dir(Dir), ok
    end.

maybe_skip_network(TC, Cfg) when TC =:= session_user_input_flow; TC =:= say_this_is_a_test; TC =:= file_io_create_hello_md ->
    case has_api_creds() of
        true -> Cfg;
        false -> {skip, "Missing API key (set OPENAI_API_KEY or ANTHROPIC_API_KEY)"}
    end;
maybe_skip_network(_TC, Cfg) ->
    Cfg.

has_api_creds() ->
    case ct:get_config(codex_env, undefined) of
        M when is_map(M) ->
            has_any_key(M, ["OPENAI_API_KEY", "ANTHROPIC_API_KEY"]);
        _ ->
            (os:getenv("OPENAI_API_KEY") =/= false) orelse (os:getenv("ANTHROPIC_API_KEY") =/= false)
    end.

has_any_key(Map, Keys) ->
    lists:any(fun(K) -> maps:is_key(unicode:characters_to_binary(K), normalize_env_map(Map)) end, Keys).
