-module(register_h_tests).

-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").

register_h_test_() ->
    [
        {"Register a valid user should return status 201",
        ?setup(fun register_user/1)},
        {"Submitting a register request with an empty body should return 400.",
        ?setup(fun register_empty_body/1)},
        {"Trying to register two users with the same ID should return 422.",
        ?setup(fun register_duplicate_user/1)},
        {"Submitting an invalid json body should return 400",
        ?setup(fun register_invalid_body/1)}
    ].

%% Setup functions 

start() ->
    {ok, Pid} = application:ensure_all_started(companyk),
    mnesia:clear_table(user),
    Pid.

stop(_Pid) ->
    application:stop(companyk).

%% Tests

register_user(_Pid) ->
    Body = "{\"user\": \"Jakob\", \"password\": \"123\"}",
    {ok, {{_, Status, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], Body}),
    [
        ?_assertEqual(201, Status)
    ].

register_empty_body(_Pid) -> 
    {ok, {{_, Status, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], <<>>}),
    [
        ?_assertEqual(400, Status)
    ].

register_duplicate_user(_Pid) -> 
    Body = "{\"user\": \"Jakob\", \"password\": \"123\"}",
    {ok, {{_, Status1, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], Body}),
    {ok, {{_, Status2, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], Body}),
    [
        ?_assertEqual(201, Status1),
        ?_assertEqual(422, Status2)
    ].

register_invalid_body(_Pid) ->
    InvalidBody = "{\"user\": \"Jakob\" \"password\": \"123\"}",
    {ok, {{_, Status, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], InvalidBody}),
    [
        ?_assertEqual(400, Status)
    ].

