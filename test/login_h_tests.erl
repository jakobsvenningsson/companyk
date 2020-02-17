-module(login_h_tests).

-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").
-include("companyk.hrl").

login_h_test_() ->
    [
        {"A login request with valid credentials should return status 200 and an Auth token.", 
         ?setup(fun login/1)},
        {"A login request with bad credentials should return 400.", 
         ?setup(fun login_with_bad_credentials/1)}
    ].

%% Setup functions 

start() ->
    {ok, Pid} = application:ensure_all_started(companyk),
    mnesia:clear_table(user),
    Pid.

stop(_Pid) ->
    application:stop(companyk).

%% Tests

login(_Pid) ->
    User = "{\"user\": \"Jakob\", \"password\": \"123\"}", 
    {ok, {{_, Status1, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], User}),
    {ok, {{_, Status2, _}, _, JSON}} = test_utils:http_request(post, {?ENDPOINT ++ "/login", [], User}),
    [{<<"token">>, Token}] = jsx:decode(JSON),
    {ok, Key} = file:read_file(?JWT_KEY_PATH),
    [
     ?_assertEqual(201, Status1),
     ?_assertEqual(200, Status2),
     ?_assertMatch({ok, #{ id := <<"Jakob">>}}, jwerl:verify(Token, hs256, Key))
    ].

login_with_bad_credentials(_Pid) ->
    User = "{\"user\": \"Jakob\", \"password\": \"123\"}", 
    {ok, {{_, Status1, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/register", [], User}),
    BadUser = "{\"user\": \"Jakob\", \"password\": \"321\"}", 
    {ok, {{_, Status2, _}, _, JSON}} = test_utils:http_request(post, {?ENDPOINT ++ "/login", [], BadUser}),
    [
     ?_assertEqual(201, Status1),
     ?_assertEqual(401, Status2),
     ?_assertEqual(<<"Bad Credentials">>, JSON)
    ].



