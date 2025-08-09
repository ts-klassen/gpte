-module(gpte_codex_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([
        suite/0
      , all/0
      , init_per_suite/1
      , end_per_suite/1
      , init_per_testcase/2
      , end_per_testcase/2
      , smoke_open_close/1
      , session_user_input_flow/1
    ]).

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [smoke_open_close, session_user_input_flow].

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
    case ct:get_config(workspace_dir, undefined) of
        undefined ->
            Priv = proplists:get_value(priv_dir, Cfg),
            Ws0 = filename:join(Priv, atom_to_list(TC)),
            ok = ensure_dir(Ws0),
            Ws = unicode:characters_to_binary(Ws0),
            [{workspace_dir, Ws} | Cfg];
        Ws ->
            [{workspace_dir, unicode:characters_to_binary(Ws)} | Cfg]
    end.

end_per_testcase(_TC, _Cfg) ->
    ok.

%% Test cases -------------------------------------------------------------

smoke_open_close(Cfg) ->
    Prog = get_prog(Cfg),
    Args = get_args(),
    Env = get_env(),
    Opt = #{program => Prog, args => Args, env => Env, on_invalid => unknown},
    C = gpte_codex:open(Opt),
    ok = gpte_codex:close(C).

session_user_input_flow(Cfg) ->
    Prog = get_prog(Cfg),
    Ws = get_ws(Cfg),
    Args = get_args(),
    Env = get_env(),
    Opt = #{program => Prog, args => Args, env => Env, on_invalid => unknown},
    C0 = gpte_codex:open(Opt),
    Sess = #{model => <<"gpt-5-nano">>, workspace_dir => Ws, env => #{}, protocol_version => 1},
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
    %% Optional CT config key 'codex_args' may be a list of strings/binaries.
    %% Default to ["proto"].
    case ct:get_config(codex_args, ["proto"]) of
        L when is_list(L) -> [unicode:characters_to_binary(A) || A <- L];
        Other -> [unicode:characters_to_binary(Other)]
    end.

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
