-module(auth_tests).

-include_lib("eunit/include/eunit.hrl").

-include("test_helpers.hrl").

auth_test_() ->
    [
        {"Accessing protected route without providing authroization header should return 401.",
         ?setup(fun access_protected_content_without_auth_header/1)},
        {"Aaccessing protected route with invalid access token should return 401",
         ?setup(fun access_protected_content_with_invalid_auth_token/1)},
        {"Accessing /content with valid access token should be allowed",
         ?setup(fun access_content_path_with_valid_token/1)},
        {"Accessing /pay with valid access token should be allowed",
         ?setup(fun access_pay_path_with_valid_token/1)}
    ].

%% Setup functions 

start() ->
    {ok, Pid} = application:ensure_all_started(companyk),
    mnesia:clear_table(user),
    Token1 = test_utils:register_and_login_user(?USER1),
    Token2 = test_utils:register_and_login_user(?USER2),
    {Pid, {Token1, Token2}}.

stop({_, _}) ->
    application:stop(companyk).

%% Tests

access_protected_content_without_auth_header({_Pid, {_Token1, _Token2}}) -> 
    {ok, {{_, Status, _}, _, _Body}} = test_utils:http_request(get, {?ENDPOINT ++ "/content/user/123", []}),
    [
        ?_assertEqual(401, Status)
    ].

access_protected_content_with_invalid_auth_token({_Pid, {_Token1, _Token2}}) -> 
    {ok, {{_, Status, _}, _, _Body}} = test_utils:http_request(get, {?ENDPOINT ++ "/content/user/123", [?AUTH_H("invalid_token")]}),
    [
        ?_assertEqual(401, Status)
    ].

access_content_path_with_valid_token({_Pid, {Token1, _Token2}}) ->
    {ok, {{_, Status, _}, _, _}} = test_utils:http_request(get, {?ENDPOINT ++ "/content/user/123", [?AUTH_H(Token1)]}),
    [
        ?_assertEqual(200, Status)
    ].

access_pay_path_with_valid_token({_Pid, {Token1, Token2}}) ->
    {ok, {{_, Status1, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/content", [?AUTH_H(Token1)], ?CONTENT2}),
    {ok, {{_, Status2, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/pay/1", [?AUTH_H(Token2)], []}),
    [
        ?_assertEqual(201, Status1),
        ?_assertEqual(200, Status2)
    ].
