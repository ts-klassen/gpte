-module(gpte_codex).

%% Public API -------------------------------------------------------------

-export([
        open/1
      , close/1
      , port/1
      , opts/1
      , buffer/1
      , send_op/2
      , configure_session/2
      , user_input/3
      , user_input/4
      , exec_approval/3
      , interrupt/1
      , recv_events/2
      , handle_port_data/2
      , await_event/3
    ]).

-export_type([
        opt/0
      , codex/0
    ]).

%% Types -----------------------------------------------------------------

%% Options to start the Codex daemon in protocol mode.
%%
%% Fields:
%% - program: Path or name of the codex binary (e.g. <<"codex">>)
%% - args: Additional args; the implementation will ensure proto mode
%% - cwd: Working directory for the port process
%% - env: Extra environment variables for the port process
%% - on_invalid: How to treat invalid JSON lines from Codex
-type opt() :: #{
        program := unicode:unicode_binary()
      , args => [unicode:unicode_binary()]
      , cwd => unicode:unicode_binary()
      , env => gpte_codex_proto:env()
      , provider => unicode:unicode_binary()
      , on_invalid => drop | unknown
    }.

%% Codex handle/state kept by the caller and passed to all operations.
%%
%% - port: The open port handle to the Codex process
%% - mon:  A monitor reference (if set by implementation)
%% - buf:  Decoder buffer for line-delimited protocol
%% - opts: Original options used to open the port
-type codex() :: #{
        port := port()
      , mon => reference()
      , buf := gpte_codex_proto:decode_buf()
      , opts := opt()
    }.

%% API -------------------------------------------------------------------

-spec open(opt()) -> codex().
open(Opt0) ->
    Opt = normalize_opt(Opt0),
    Prog = maps:get(program, Opt),
    Args0 = maps:get(args, Opt, []),
    Args = [unicode:characters_to_list(A) || A <- Args0],
    Exec = unicode:characters_to_list(Prog),
    PortOpts0 = [
        binary,
        exit_status,
        use_stdio,
        stderr_to_stdout,
        hide,
        {args, Args}
    ],
    PortOpts1 = case Opt of
        #{cwd := Cwd} -> [{cd, unicode:characters_to_list(Cwd)} | PortOpts0];
        _ -> PortOpts0
    end,
    PortOpts = case Opt of
        #{env := EnvMap} when is_map(EnvMap), map_size(EnvMap) > 0 ->
            EnvList = [
                {unicode:characters_to_list(K), unicode:characters_to_list(V)}
                || {K, V} <- maps:to_list(EnvMap)
            ],
            [{env, EnvList} | PortOpts1];
        _ -> PortOpts1
    end,
    Port = open_port({spawn_executable, Exec}, PortOpts),
    #{port => Port, buf => gpte_codex_proto:new_buf(), opts => Opt}.

-spec close(codex()) -> ok.
close(C) ->
    Port = port(C),
    _ = erlang:port_close(Port),
    ok.

-spec port(codex()) -> port().
port(C) ->
    case C of
        #{port := P} when is_port(P) -> P;
        _ -> erlang:error({badarg, port, C})
    end.

-spec opts(codex()) -> opt().
opts(C) ->
    case C of
        #{opts := O} -> O;
        _ -> erlang:error({badarg, opts, C})
    end.

-spec buffer(codex()) -> gpte_codex_proto:decode_buf().
buffer(C) ->
    case C of
        #{buf := B} when is_binary(B) -> B;
        _ -> erlang:error({badarg, buffer, C})
    end.

-spec send_op(gpte_codex_proto:op(), codex()) -> ok.
send_op(Op, C) ->
    Port = port(C),
    Frame = encode_outgoing(Op, C),
    case erlang:port_command(Port, Frame) of
        true -> ok;
        false -> erlang:error({port_command_failed, Frame})
    end.

-spec configure_session(gpte_codex_proto:session_opts(), codex()) -> ok.
configure_session(Session, C) ->
    Op = gpte_codex_proto:mk_configure_session(Session),
    send_op(Op, C).

-spec user_input(gpte_codex_proto:sub_id(), gpte_codex_proto:user_input(), codex()) -> ok.
user_input(SubId, Input, C) ->
    Op = gpte_codex_proto:mk_user_input(SubId, Input),
    send_op(Op, C).

-spec user_input(gpte_codex_proto:sub_id(), gpte_codex_proto:user_input(), gpte_codex_proto:user_input_opts(), codex()) -> ok.
user_input(SubId, Input, Opts0, C) ->
    Op = gpte_codex_proto:mk_user_input(SubId, Input, Opts0),
    send_op(Op, C).

-spec exec_approval(gpte_codex_proto:sub_id(), gpte_codex_proto:approval_decision(), codex()) -> ok.
exec_approval(SubId, Decision, C) ->
    Op = gpte_codex_proto:mk_exec_approval(SubId, Decision),
    send_op(Op, C).

-spec interrupt(codex()) -> ok.
interrupt(C) ->
    send_op(gpte_codex_proto:mk_interrupt(), C).

%% Receive and decode events from the port until timeout or no data.
%% Returns normalized events and updated decoder buffer.
-spec recv_events(infinity | non_neg_integer(), codex()) -> {[gpte_codex_proto:event()], codex()}.
recv_events(Timeout, C0) ->
    Port = port(C0),
    OnInvalid = maps:get(on_invalid, opts(C0), drop),
    receive
        {Port, {data, Bin}} when is_binary(Bin) ->
            {Events1, C1} = do_handle_data(Bin, OnInvalid, C0),
            recv_drain(Port, OnInvalid, C1, lists:reverse(Events1));
        {Port, closed} ->
            erlang:error(port_closed);
        {Port, {exit_status, Code}} ->
            erlang:error({port_exit_status, Code})
    after Timeout ->
        {[], C0}
    end.

%% Feed a chunk of bytes from the port into the decoder.
%% Useful for integrating with a caller-owned receive loop.
  -spec handle_port_data(binary(), codex()) -> {[gpte_codex_proto:event()], codex()}.
handle_port_data(Bin, C0) when is_binary(Bin) ->
    OnInvalid = maps:get(on_invalid, opts(C0), drop),
    do_handle_data(Bin, OnInvalid, C0).

%% Convenience: Block until an agent_message is received and return its text.
%% Also supports providers that stream deltas followed by a final message,
%% or that emit the final text inside task_complete payloads.
%% Returns the message text when found, or {error, timeout, C} on timeout.
-spec await_event(gpte_codex_proto:event_type(), infinity | non_neg_integer(), codex()) ->
          {ok, gpte_codex_proto:event(), codex()} | {error, timeout, codex()}.
await_event(Type, Timeout, C0) when is_atom(Type) ->
    Deadline = case Timeout of infinity -> infinity; T when is_integer(T), T >= 0 -> erlang:monotonic_time(millisecond) + T end,
    await_event_loop(Type, Deadline, C0).

await_event_loop(Type, Deadline, C0) ->
    Remain = case Deadline of infinity -> 30000; _ -> Deadline - erlang:monotonic_time(millisecond) end,
    case Remain =:= infinity orelse Remain > 0 of
        false -> {error, timeout, C0};
        true ->
            Chunk = case Deadline of infinity -> 30000; _ -> case Remain > 30000 of true -> 30000; false -> Remain end end,
            {Events, C1} = recv_events(Chunk, C0),
            case lists:filter(fun(E) -> maps:get(type, E) =:= Type end, Events) of
                [Ev | _] -> {ok, Ev, C1};
                [] -> await_event_loop(Type, Deadline, C1)
            end
    end.

%% Internal ---------------------------------------------------------------

normalize_opt(Opt0) when is_map(Opt0) ->
    case Opt0 of
        #{program := Prog} when is_binary(Prog) ->
            Opt0;
        #{program := Prog} ->
            Opt0#{program := unicode:characters_to_binary(Prog)};
        _ ->
            erlang:error({badopt, Opt0})
    end;
normalize_opt(Other) ->
    erlang:error({badopt, Other}).

do_handle_data(Bin, OnInvalid, C0) ->
    Buf0 = buffer(C0),
    {Events, Buf1} = gpte_codex_proto:decode_chunk(Buf0, Bin, #{on_invalid => OnInvalid}),
    {Events, C0#{buf := Buf1}}.

recv_drain(Port, OnInvalid, C0, EvAccRev) ->
    receive
        {Port, {data, Bin}} when is_binary(Bin) ->
            {Events1, C1} = do_handle_data(Bin, OnInvalid, C0),
            recv_drain(Port, OnInvalid, C1, lists:reverse(Events1) ++ EvAccRev);
        {Port, closed} ->
            erlang:error(port_closed);
        {Port, {exit_status, Code}} ->
            erlang:error({port_exit_status, Code})
    after 0 ->
        {lists:reverse(EvAccRev), C0}
    end.

encode_outgoing(Op, C) when is_map(Op) ->
    %% Codex CLI expects submissions as: { id, op: { type, ... } }
    %% Determine provider from Op/session, opts, or env heuristics.
    Provider0 = choose_provider(Op, C),
    OpInner = case maps:find(op, Op) of
        {ok, Type} ->
            Data0 = maps:remove(op, Op),
            Data1 = transform_op(Type, Data0),
            %% Build provider info object
            PInfo = provider_info(Provider0, C),
            case Type of
                configure_session -> ok = validate_configure_session(Data1, PInfo);
                _ -> ok
            end,
            case {Type, PInfo} of
                {configure_session, undefined} -> erlang:error({badarg, provider_missing, provider_required_description()});
                _ -> ok
            end,
            case PInfo of
                undefined -> Data1#{type => Type};
                _ -> Data1#{type => Type, provider => PInfo}
            end;
        error -> Op
    end,
    IdBin = integer_to_binary(erlang:unique_integer([monotonic, positive])),
    Top = #{id => IdBin, op => OpInner},
    gpte_codex_proto:frame(gpte_codex_proto:encode_op(Top)).

transform_op(configure_session, Data0 = #{session := _Sess0}) ->
    %% Flatten session fields into the op payload and remove provider from session.
    Sess0 = maps:get(session, Data0),
    Sess1 = case Sess0 of
        M when is_map(M) -> maps:remove(provider, M);
        Other -> Other
    end,
    maps:merge(maps:remove(session, Data0), Sess1);
transform_op(user_input, Data0) ->
    case maps:find(input, Data0) of
        {ok, Input} ->
            Items = [#{type => text, text => Input}],
            (maps:remove(input, Data0))#{items => Items};
        error -> Data0
    end;
transform_op(_, Data) ->
    Data.

choose_provider(Op, C) ->
    %% Priority (explicit only; no heuristics):
    %% 1) Op top-level 'provider'
    %% 2) Op.session 'provider'
    %% 3) C.opts 'provider'
    case maps:get(provider, Op, undefined) of
        P when is_binary(P); is_list(P); is_atom(P) -> normalize_provider(P);
        _ ->
            SessP = case maps:get(session, Op, undefined) of
                S when is_map(S) -> maps:get(provider, S, undefined);
                _ -> undefined
            end,
            case SessP of
                P2 when is_binary(P2); is_list(P2); is_atom(P2) -> normalize_provider(P2);
                _ ->
                    Opts = opts(C),
                    case maps:get(provider, Opts, undefined) of
                        P3 when is_binary(P3); is_list(P3); is_atom(P3) -> normalize_provider(P3);
                        _ -> undefined
                    end
            end
    end.

normalize_provider(P) when is_binary(P) -> P;
normalize_provider(P) when is_list(P) -> unicode:characters_to_binary(P);
normalize_provider(P) when is_atom(P) -> normalize_provider(atom_to_list(P));
normalize_provider(_) -> undefined.

%% Heuristic helpers removed to avoid implicit defaults

provider_info(undefined, _C) -> undefined;
provider_info(Provider, _C) ->
    P = normalize_provider(Provider),
    case P of
        undefined -> undefined;
        _ ->
            %% Minimal struct: refer to a known provider by name; let CLI read env from process/session config
            #{name => P}
    end.

%% Validate ConfigureSession payload has all required fields with valid values.
validate_configure_session(Map, ProviderInfo) when is_map(Map) ->
    Miss0 = [],
    Miss1 = case maps:is_key(model, Map) of true -> Miss0; false -> [missing_model() | Miss0] end,
    Miss2 = case maps:is_key(workspace_dir, Map) of true -> Miss1; false -> [missing_workspace_dir() | Miss1] end,
    Miss3 = case maps:find(model_reasoning_effort, Map) of
        {ok, V1} -> case enum_member(V1, [<<"low">>, <<"medium">>, <<"high">>, <<"none">>]) of true -> Miss2; false -> [invalid_model_reasoning_effort() | Miss2] end;
        error -> [missing_model_reasoning_effort() | Miss2]
    end,
    Miss4 = case maps:find(model_reasoning_summary, Map) of
        {ok, V2} -> case enum_member(V2, [<<"auto">>, <<"concise">>, <<"detailed">>, <<"none">>]) of true -> Miss3; false -> [invalid_model_reasoning_summary() | Miss3] end;
        error -> [missing_model_reasoning_summary() | Miss3]
    end,
    Miss5 = case maps:find(approval_policy, Map) of
        {ok, V3} -> case enum_member(V3, [<<"untrusted">>, <<"on-failure">>, <<"on-request">>, <<"never">>]) of true -> Miss4; false -> [invalid_approval_policy() | Miss4] end;
        error -> [missing_approval_policy() | Miss4]
    end,
    Miss6 = case maps:find(sandbox_policy, Map) of
        {ok, SP} when is_map(SP) ->
            case maps:find(mode, SP) of
                {ok, Mode} -> case enum_member(Mode, [<<"read-only">>, <<"workspace-write">>, <<"danger-full-access">>]) of true -> Miss5; false -> [invalid_sandbox_mode() | Miss5] end;
                error -> [missing_sandbox_mode() | Miss5]
            end;
        {ok, _Other} -> [invalid_sandbox_policy() | Miss5];
        error -> [missing_sandbox_policy() | Miss5]
    end,
    Miss = case ProviderInfo of undefined -> [missing_provider() | Miss6]; _ -> Miss6 end,
    case Miss of
        [] -> ok;
        _ -> erlang:error({bad_configure_session, lists:reverse(Miss)})
    end.

enum_member(V, Allowed) when is_binary(V) -> lists:member(V, Allowed);
enum_member(V, Allowed) when is_list(V) -> lists:member(unicode:characters_to_binary(V), Allowed);
enum_member(_, _) -> false.

missing_model() ->
    #{field => model, description => <<"Required: model id (e.g., o3, gpt-4o).">>}.

missing_workspace_dir() ->
    #{field => workspace_dir, description => <<"Required: workspace directory absolute path.">>}.

missing_model_reasoning_effort() ->
    #{field => model_reasoning_effort, description => <<"Required: reasoning effort level.">>, allowed => [<<"low">>, <<"medium">>, <<"high">>, <<"none">>]}.

invalid_model_reasoning_effort() ->
    #{field => model_reasoning_effort, description => <<"Invalid: must be one of: low | medium | high | none.">>, allowed => [<<"low">>, <<"medium">>, <<"high">>, <<"none">>]}.

missing_model_reasoning_summary() ->
    #{field => model_reasoning_summary, description => <<"Required: reasoning summary preference.">>, allowed => [<<"auto">>, <<"concise">>, <<"detailed">>, <<"none">>]}.

invalid_model_reasoning_summary() ->
    #{field => model_reasoning_summary, description => <<"Invalid: must be one of: auto | concise | detailed | none.">>, allowed => [<<"auto">>, <<"concise">>, <<"detailed">>, <<"none">>]}.

missing_approval_policy() ->
    #{field => approval_policy, description => <<"Required: approval policy.">>, allowed => [<<"untrusted">>, <<"on-failure">>, <<"on-request">>, <<"never">>]}.

invalid_approval_policy() ->
    #{field => approval_policy, description => <<"Invalid: must be one of: untrusted | on-failure | on-request | never.">>, allowed => [<<"untrusted">>, <<"on-failure">>, <<"on-request">>, <<"never">>]}.

missing_sandbox_policy() ->
    #{field => sandbox_policy, description => <<"Required: sandbox policy object with 'mode'.">>}.

invalid_sandbox_policy() ->
    #{field => sandbox_policy, description => <<"Invalid: must be an object with 'mode'.">>}.

missing_sandbox_mode() ->
    #{field => sandbox_policy_mode, description => <<"Required: sandbox mode.">>, allowed => [<<"read-only">>, <<"workspace-write">>, <<"danger-full-access">>]}.

invalid_sandbox_mode() ->
    #{field => sandbox_policy_mode, description => <<"Invalid: must be one of: read-only | workspace-write | danger-full-access.">>, allowed => [<<"read-only">>, <<"workspace-write">>, <<"danger-full-access">>]}.

missing_provider() ->
    #{field => provider, description => <<"Required: model provider name (e.g., openai, anthropic). Set via open/1 opts or include 'provider' in session options.">>, allowed => [<<"openai">>, <<"anthropic">>, <<"openai-chat-completions">>, <<"azure">>, <<"ollama">>, <<"mistral">>]}.

provider_required_description() ->
    #{description => <<"Provider is required. Set open/1 option 'provider' or include 'provider' in session options.">>, allowed => [<<"openai">>, <<"anthropic">>, <<"openai-chat-completions">>, <<"azure">>, <<"ollama">>, <<"mistral">>]}.
