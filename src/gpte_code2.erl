-module(gpte_code2).

-define(GIT_AUTHOR_DOMAIN, "git.usersmtp.com").

-export([
        file/1
    ]).

-export_type([
        opt/0
      , file_resp/0
    ]).

-type opt() :: #{
        dir => unicode:unicode_binary()
      , commit_all_messages => boolean()
    }.

-type file_resp() :: #{
        read => [klsn:binstr()]
      , write => [#{
            file => klsn:binstr()
          , data => klsn:binstr()
        }]
      , is_done := boolean()
    }.


-spec file(chat_gpte:chat()) -> {klsn:binstr(), chat_gpte:chat()}.
file(Chat0) ->
    FileResp = {?MODULE, file_resp, 0},
    Chat10 = chat_gpte:model(<<"o3-mini">>, Chat0),
    Chat20 = chat_gpte:schema(gpte_schema:schema(FileResp), Chat10),
    {Res30, Chat30} = chat_gpte:ask(<<"Please respond in JSON format.
        read: Name or path of the files you want to read.
              It may NOT start with `/` nor contain `..`.
              Unsafe path will be rejected.
              If specified, write will be ignored.
        write: Files you want to write.
               Do NOT use this to communicate with the user.
               To reply to user, set is_done to true and exit the file editing mode.
               file: Name or path of the file to write.
                     It may NOT start with `/` nor contain `..`.
                     Unsafe path will be rejected.
               data: The data to write.
                     The entire file will be overwritten.
        is_done: Set true to finish.
                 When files are writted, I will show you the diff.
                 If the diff is what you expected, set it to true and finish.
                 When read or write is specified, is_done must be false.
                 If nothing to do in the first place, set it to true.
                 After you reply with is_done=true, you can talk with the user.
    ">>, Chat20),
    Loop = fun Self({Res, Chat}) ->
        case gpte_schema:schema(FileResp, Res) of
            #{ read := Read } when length(Read) > 0 ->
                Self(read_file(Read, Chat));
            #{ write := Write } when length(Write) > 0 ->
                Self(write_file(Write, Chat));
            #{ is_done := true } ->
                chat_gpte:ask(<<"Exit file editing. Entered interactive mode. Your response will be displayed to the user. Simply explain how the file editing or reading went. Try not to print the entier content. Showing the diff may be a better idea.">>, chat_gpte:delete_response_format(Chat))
        end
    end,
    Loop({Res30, Chat30}).

read_file(Read, Chat) ->
    {ok, Cwd} = file:get_cwd(),
    Text = try
        iolist_to_binary(lists:map(fun(File)->
            case filelib:safe_relative_path(File, Cwd) of
                unsafe ->
                    throw({?MODULE, unsafe, File});
                FileRelative ->
                    case file:read_file(FileRelative) of
                        {ok, Data} ->
                            quote_code(File, Data);
                        {error, enoent} ->
                            <<"file `",
                              File/binary, "` not found.\n">>;
                        _ ->
                            <<"error while reading `",
                              File/binary, "`.\n">>
                    end
            end
        end, Read))
    catch
        throw:{?MODULE, unsafe, File} ->
            <<"`", File/binary, "` is unsafe. Aborted.">>
    end,
    chat_gpte:ask(Text, Chat).

write_file(Write, Chat) ->
    {ok, Cwd} = file:get_cwd(),
    Text = try
        Errors = lists:map(fun(WriteElem)->
            File = maps:get(file, WriteElem),
            Data = maps:get(data, WriteElem),
            case filelib:safe_relative_path(File, Cwd) of
                unsafe ->
                    throw({?MODULE, unsafe, File});
                FileRelative ->
                    case file:write_file(FileRelative, Data) of
                        ok ->
                            <<>>;
                        _ ->
                            <<"error while writing `",
                              File/binary, "`.\n">>
                    end
            end
        end, Write),
        Diff = os:cmd("git diff"),
        iolist_to_binary([Errors, <<"\n\n">>, Diff])
    catch
        throw:{?MODULE, unsafe, File} ->
            <<"`", File/binary, "` is unsafe. Aborted.">>
    end,
    chat_gpte:ask(Text, Chat).


quote_code(File, Data) ->
    quote_code(File, Data, <<"```">>).
quote_code(File, Data, Quote) ->
    case re:run(Data, <<"^", Quote/binary>>) of
        nomatch ->
            <<
                File/binary
              , "\n"
              , Quote/binary
              , "\n"
              , Data/binary
              , "\n"
              , Quote/binary
              , "\n\n">>;
        _ ->
            quote_code(File, Data, <<"`", Quote/binary>>)
    end.


