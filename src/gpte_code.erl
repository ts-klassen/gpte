-module(gpte_code).

-export([
        cli/0
      , cli/1
      , resume_cli/1
    ]).

-export_type([
        opt/0
    ]).

-define(GIT_AUTHOR_DOMAIN, "git.usersmtp.com").

-type opt() :: #{
        dir => unicode:unicode_binary()
      , commit_all_messages => boolean()
    }.

-spec cli() -> chat_gpte:chat().
cli() ->
    cli(#{}).

-spec cli(opt()) -> chat_gpte:chat().
cli(Opt) ->
    Chat0 = chat_gpte:new(),
    Chat10 = chat_gpte:model(<<"o1-mini">>, Chat0),
    Chat20 = chat_gpte:temperature(1, Chat10),
    Chat30 = educate_annotation(Chat20, false),
    resume_cli(Chat30#{ ?MODULE => Opt }).


-spec resume_cli(chat_gpte:chat()) -> chat_gpte:chat().
resume_cli(Chat) ->
    case unicode:characters_to_binary(io:get_line(<<">> ">>)) of
        <<"QUIT\n">> ->
            Chat;
        <<"DATA\n">> ->
            resume_cli(ask(data(<<>>), Chat));
        User ->
            resume_cli(ask(User, Chat))
    end.

-spec data(unicode:unicode_binary()) -> unicode:unicode_binary().
data(Data) ->
    case unicode:characters_to_binary(io:get_line(<<">> ">>)) of
        <<".\n">> ->
            Data;
        <<"..", Part/binary>> ->
            data(<<".", Data/binary, Part/binary>>);
        Part ->
            data(<<Data/binary, Part/binary>>)
    end.

-spec ask(unicode:unicode_binary(), chat_gpte:chat()) -> chat_gpte:chat().
ask(Text10, Chat10) ->
    Opt = maps:get(?MODULE, Chat10),
    Text20 = unicode:characters_to_binary(Text10),
    git_commit(<<"User: ", Text20/binary>>, Opt),
    Text30 = run_user_cmd(decode_cmd(Text20), Text20, Opt),
    io:format("user: ~ts~n", [Text30]),
    {Res0, Chat20} = chat_gpte:ask(Text30, Chat10),
    AiCmd = decode_cmd(Res0),
    Res = run_ai_cmd(AiCmd, Res0, Opt),
    case lists:search(fun(#{cmd:=commit})->true;(_)->false end, AiCmd) of
        false ->
            git_commit(<<"AI Code Assistant: ", Res/binary>>,
                       Opt#{author=>ai});
        _ ->
            ok
    end,
    io:format("ai: ~ts~n", [Res]),
    Chat20.


-spec run_user_cmd(cmd_list(), unicode:unicode_binary(), opt()) -> unicode:unicode_binary().
run_user_cmd([], Text, _Opt) ->
    Text;
run_user_cmd([#{cmd:=file, file:=File}|CMDs], Text0, Opt) ->
    {ok, Cwd} = case Opt of
        #{dir:=CwdDir} ->
            {ok, CwdDir};
        _ ->
            file:get_cwd()
    end,
    case filelib:safe_relative_path(File, Cwd) of
        unsafe ->
            io:format("gpte: Filename ~ts unsafe. Skip.~n", [File]),
            run_user_cmd(CMDs, Text0, Opt);
        FileRelative ->
            Path = case Opt of
                #{dir:=Dir} ->
                    filename:append(Dir, FileRelative);
                _ ->
                    FileRelative
            end,
            case file:read_file(Path) of
                {ok, Data} ->
                    io:format("gpte: File read from ~ts~n", [Path]),
                    Annotation = <<"@file=", File/binary>>,
                    Code = <<Annotation/binary, "\n```\n", Data/binary, "\n```\n">>,
                    Text = binary:replace(Text0, Annotation, Code, [global]),
                    run_user_cmd(CMDs, Text, Opt);
                Error ->
                    io:format("gpte: Failed to read ~ts, ~p~n", [Path, Error]),
                    run_user_cmd(CMDs, Text0, Opt)
            end
    end;
run_user_cmd([_|CMDs], Text, Opt) ->
    run_user_cmd(CMDs, Text, Opt).

-spec run_ai_cmd(cmd_list(), unicode:unicode_binary(), opt()) -> unicode:unicode_binary().
run_ai_cmd([], Text, _Opt) ->
    Text;
run_ai_cmd([#{cmd:=file, file:=File, data:=Data}|CMDs], Text0, Opt) ->
    {ok, Cwd} = case Opt of
        #{dir:=CwdDir} ->
            {ok, CwdDir};
        _ ->
            file:get_cwd()
    end,
    case filelib:safe_relative_path(File, Cwd) of
        unsafe ->
            io:format("gpte: Filename ~ts unsafe. Skip.~n", [File]),
            run_ai_cmd(CMDs, Text0, Opt);
        FileRelative ->
            Path = case Opt of
                #{dir:=Dir} ->
                    filename:append(Dir, FileRelative);
                _ ->
                    FileRelative
            end,
            case file:write_file(Path, Data) of
                ok ->
                    io:format("gpte: File written to ~ts~n", [Path]),
                    git_add(Path, Opt),
                    Text = binary:replace(Text0, Data, <<"@omitted\n">>, [global]),
                    run_ai_cmd(CMDs, Text, Opt);
                Error ->
                    io:format("gpte: Failed to write ~ts, ~p~n", [Path, Error]),
                    run_ai_cmd(CMDs, Text0, Opt)
            end
    end;
run_ai_cmd([#{cmd:=commit, msg:=MSG}|CMDs], Text, Opt) ->
    git_commit(<<MSG/binary, "\n\n", Text/binary>>,
        Opt#{author=>ai, commit_all_messages=>true}),
    run_ai_cmd(CMDs, Text, Opt).


-spec git_add(unicode:unicode_binary(), opt()) -> ok.
git_add(Path0, #{dir:=Dir0}) ->
    Dir = esc(Dir0),
    Path = esc(Path0),
    io:format("git: ~ts~n", [os:cmd(unicode:characters_to_list(<<
        "git -C '"
      , Dir/binary
      , "' add '"
      , Path/binary
      , "' --verbose"
    >>))]);
git_add(_, _) ->
    ok.


-spec git_commit(unicode:unicode_binary(), opt()) -> ok.
git_commit(MSG0, #{dir:=Dir0, commit_all_messages:=true}=Opt) ->
    Dir = esc(Dir0),
    MSG = esc(MSG0),
    Author = case Opt of
        #{author:=ai} ->
            <<
                " --author='AI Code Assistant <ai.code.assistant@"
              , ?GIT_AUTHOR_DOMAIN
              , ">' "
            >>;
        _ ->
            <<>>
    end,
    io:format("git: ~ts~n", [os:cmd(unicode:characters_to_list(<<
        "git -C '"
      , Dir/binary
      , "' commit -m '"
      , MSG/binary
      , "' --allow-empty"
      , Author/binary
    >>))]);
git_commit(_, _) ->
    ok.

-spec esc(unicode:unicode_binary()) -> unicode:unicode_binary().
esc(Bin) ->
     binary:replace(Bin, <<"'">>, <<"'\"'\"'">>, [global]).

-type decode_cmd_state() :: md | cmd | code.
-type cmd_list() :: [cmd_map()].
-type cmd_map() :: #{
        cmd := file
      , file := unicode:unicode_binary()
      , data := unicode:unicode_binary()
    } | #{
        cmd := commit
      , msg := unicode:unicode_binary()
    }.


-spec decode_cmd(unicode:unicode_binary()) -> cmd_list().
decode_cmd(MD) ->
    NoCR = binary:replace(MD, <<"\r">>, <<>>, [global]),
    Lines = binary:split(NoCR, <<"\n">>, [global]),
    decode_cmd(Lines, md, []).

-spec decode_cmd(
        [unicode:unicode_binary()]
      , decode_cmd_state()
      , cmd_list()
    ) -> cmd_list().
decode_cmd([], _, CMD) ->
    CMD;
decode_cmd([<<"@file=", File/binary>>|MD], md, CMD) ->
    decode_cmd(MD, cmd, [#{cmd=>file, file=>File, data=> <<>>}|CMD]);
decode_cmd([<<"@commit=", MSG/binary>>|MD], md, CMD) ->
    decode_cmd(MD, md, [#{cmd=>commit, msg=>MSG}|CMD]);
decode_cmd([<<"```", _/binary>>|MD], cmd, CMD) ->
    decode_cmd(MD, code, CMD);
decode_cmd([<<"```", _/binary>>|MD], code, CMD) ->
    decode_cmd(MD, md, CMD);
decode_cmd([Line|MD], code, [#{data:=Data}=CMD|CMDs]) ->
    decode_cmd(MD, code, [CMD#{data:= <<Data/binary, Line/binary, "\n">>}|CMDs]);
decode_cmd([_|MD], cmd, CMD) ->
    decode_cmd(MD, md, CMD);
decode_cmd([_|MD], md, CMD) ->
    decode_cmd(MD, md, CMD).


-spec educate_annotation(chat_gpte:chat(), boolean()) -> chat_gpte:chat().
educate_annotation(Chat0, true) ->
    {Res, Chat10} = chat_gpte:ask(<<"Here are some markdown annotation rules you have to follow.

You will annotate a one-line commit message like this at the top.

@commit=my first commit

This commit message will be used if the user decides to accept your changes.

We annotate file names like this.

@file=testfile.txt
```
Hello world!
```

Code block with `@file` annotation must include the entire file content.
This will be used to overwrite files.

Other than that, it's just markdown. You can write whatever you want.

Try annotating under the following conditions:

- The commit message is `JavaScript Hello world`
- Let the user know that this is a hello world example for JavaScript
- The file name is `hello_world.js`
- The content is `console.log('hello world!');`
">>, Chat0),
    {_, Chat20} = case decode_cmd(Res) of
        [#{cmd:=file,file:=<<"hello_world.js">>, data:=<<"console.log('hello world!');\n">>}, #{cmd:=commit, msg:=<<"JavaScript Hello world">>}] ->
            io:format("gpte: educate_annotation succeed~n"),
            chat_gpte:ask(<<"Great! Make sure to include the `@commit` annotation and use the `@file` annotation when you print the entire code.">>, Chat10);
        CMD ->
            io:format("debug-ai: ~ts~n", [Res]),
            io:format("debug-cmd: ~p~n", [CMD]),
            io:format("gpte: educate_annotation failed~n"),
            chat_gpte:ask(<<"Not quite right. Make sure to include the `@commit` annotation and use the `@file` annotation when you print the entire code.">>, Chat10)
    end,
    io:format("gpte: Ready.~n"),
    Chat20;
educate_annotation(Chat0, false) ->
    Messages = [{assistant,<<"Understood! I'll make sure to include the `@commit` annotation and use the `@file` annotation when providing entire code files in future responses.">>},
 {user,<<"Great! Make sure to include the `@commit` annotation and use the `@file` annotation when you print the entire code.">>},
 {assistant,<<"@commit=JavaScript Hello world\n\nThis is a hello world example for JavaScript.\n\n@file=hello_world.js\n```\nconsole.log('hello world!');\n```">>},
 {user,<<"Here are some markdown annotation rules you have to follow.\n\nYou will annotate a one-line commit message like this at the top.\n\n@commit=my first commit\n\nThis commit message will be used if the user decides to accept your changes.\n\nWe annotate file names like this.\n\n@file=testfile.txt\n```\nHello world!\n```\n\nCode block with `@file` annotation must include the entire file content.\nThis will be used to overwrite files.\n\nOther than that, it's just markdown. You can write whatever you want.\n\nTry annotating under the following conditions:\n\n- The commit message is `JavaScript Hello world`\n- Let the user know that this is a hello world example for JavaScript\n- The file name is `hello_world.js`\n- The content is `console.log('hello world!');`\n">>}],
    chat_gpte:messages(Messages, Chat0).

