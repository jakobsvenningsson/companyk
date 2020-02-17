-module(test_utils).

-include("test_helpers.hrl").

-export([http_request/2, register_and_login_user/1]).

http_request(post, {URL, H, B}) ->
    httpc:request(post, {URL, H,"application/json", B}, [], [{body_format, binary}]);
http_request(get, {URL, H}) ->
    httpc:request(get, {URL, H}, [], [{body_format, binary}]).

register_and_login_user(User) ->
    {ok, {_, _, <<>>}} = http_request(post, {?ENDPOINT ++ "/register", [], User}),
    {ok, {_, _, Body}} = http_request(post, {?ENDPOINT ++ "/login", [], User}),
    [{<<"token">>, Token}] = jsx:decode(Body),
    binary_to_list(Token).
