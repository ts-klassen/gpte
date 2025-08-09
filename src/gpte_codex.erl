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
        #{env := EnvMap} when is_map(EnvMap) ->
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
    Frame = gpte_codex_proto:encode_frame(Op),
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
