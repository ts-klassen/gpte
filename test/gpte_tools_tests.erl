%% -*- erlang -*-
%% -*- erlang -*-
-module(gpte_tools_tests).
-include_lib("eunit/include/eunit.hrl").

%% Configure f/1 to catch and report exception detail to AI
-gpte_function_option({f/1, #{catch_exception => true, report_exception_detail_to_ai => true}}).
-export([f/1, error_fun/1, echo/1]).

%% @spec f/1 takes a klsn binstr, raises an error to test AI reporting
-spec f(klsn:binstr()) -> unicode:unicode_binary().
f(_Arg) ->
    erlang:error(test_error).

%% @spec error_fun/1 raises to test callback exception propagation
-spec error_fun(klsn:binstr()) -> unicode:unicode_binary().
error_fun(_Arg) ->
    erlang:error(bad_things).

%% @spec echo/1 returns a constant to test description fallback
-spec echo(gpte_schema:sample()) -> unicode:unicode_binary().
echo(_Arg) ->
    <<"ok">>.

%% Tests for gpte_tools module

new_test() ->
    ?assertEqual(#{}, gpte_tools:new()).

add_functions_empty_test() ->
    ?assertEqual(#{}, gpte_tools:add_functions([], gpte_tools:new())).

add_functions_sample_test() ->
    Tools = gpte_tools:add_functions([{gpte_tools, sample, 1}], gpte_tools:new()),
    Functions = maps:get(functions, Tools),
    Spec = maps:get(sample, Functions),
    ?assertEqual(<<"Sample erlang function.">>, maps:get(description, Spec)),
    Params = maps:get(parameters, Spec),
    ?assertEqual(object, maps:get(type, Params)),
    ?assertEqual({gpte_schema, sample, 0}, maps:get(args_type, Spec)),
    Callback = maps:get(callback, Spec),
    ?assert(is_function(Callback, 1)),
    ?assertEqual(<<"42\n">>, Callback(42)).

sample_test() ->
    ?assertEqual(<<"hello\n">>, gpte_tools:sample(hello)).

%% Error branches in add_functions/build_function
missing_module_test() ->
    Caught = catch gpte_tools:add_functions([{no_such_mod, foo, 1}], gpte_tools:new()),
    ?assertMatch({'EXIT', {{gpte_tools_build_error, _}, _}}, Caught).

not_exported_test() ->
    Caught = catch gpte_tools:add_functions([{lists, no_such_fun, 1}], gpte_tools:new()),
    ?assertMatch({'EXIT', {{gpte_tools_build_error, _}, _}}, Caught).

arity_mismatch_test() ->
    Caught = catch gpte_tools:add_functions([{gpte_tools, sample, 2}], gpte_tools:new()),
    ?assertMatch({'EXIT', {{gpte_tools_build_error, _}, _}}, Caught).

return_type_mismatch_test() ->
    Caught = catch gpte_tools:add_functions([{lists, length, 1}], gpte_tools:new()),
    ?assertMatch({'EXIT', {{gpte_tools_build_error, _}, _}}, Caught).

duplicate_add_functions_test() ->
    Tools1 = gpte_tools:add_functions([{gpte_tools, sample, 1}], gpte_tools:new()),
    Tools2 = gpte_tools:add_functions([{gpte_tools, sample, 1}], Tools1),
    Functions = maps:get(functions, Tools2),
    Names = maps:keys(Functions),
    ?assertEqual(2, length(Names)),
    ?assert(lists:member(sample, Names)),
    Other = [N || N <- Names, N =/= sample],
    [Dup] = Other,
    NameStr = atom_to_list(Dup),
    ?assert(lists:prefix("gpte_tools___", NameStr)).

%% Test callback exception propagation
error_callback_test() ->
    Tools = gpte_tools:add_functions([{gpte_tools_tests, error_fun, 1}], gpte_tools:new()),
    Functions = maps:get(functions, Tools),
    Spec = maps:get(error_fun, Functions),
    Callback = maps:get(callback, Spec),
    Caught = catch Callback(test),
    ?assertMatch({'EXIT', {bad_things, _}}, Caught).

%% Test description fallback to argument schema description for user_type args
desc_fallback_test() ->
    Tools = gpte_tools:add_functions([{gpte_tools_tests, echo, 1}], gpte_tools:new()),
    Functions = maps:get(functions, Tools),
    Spec = maps:get(echo, Functions),
    ?assertEqual(<<"This is a sample type.">>, maps:get(description, Spec)).

%% Test adding multiple helper functions in one call
multiple_helpers_test() ->
    Tools = gpte_tools:add_functions(
        [{gpte_tools_tests, echo, 1}, {gpte_tools_tests, error_fun, 1}],
        gpte_tools:new()),
    Functions = maps:get(functions, Tools),
    Keys = maps:keys(Functions),
    ?assertEqual(2, length(Keys)),
    ?assert(lists:member(echo, Keys)),
    ?assert(lists:member(error_fun, Keys)).

%% Test report_exception_detail_to_ai branch
report_exception_detail_to_ai_test() ->
    Tools = gpte_tools:add_functions(
        [{gpte_tools_tests, f, 1}],
        gpte_tools:new()),
    Spec = maps:get(f, maps:get(functions, Tools)),
    Callback = maps:get(callback, Spec),
    Result = Callback(<<"dummy">>),
    ?assert(is_binary(Result)),
    ?assertMatch({_, _}, binary:match(Result, <<"ERROR REPORT"/utf8>>)).