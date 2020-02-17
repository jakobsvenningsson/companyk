-module(login_h).

-include("companyk.hrl").

-export([init/2]).

init(Req0, Env0) ->
    Reply = reply(Req0, Env0),
	{ok, Reply, Env0}.

reply(#{method := <<"POST">>, json := UserProps} = Req, _Env) ->
    %{ok, Data, Req} = cowboy_req:read_body(Req0),
    %UserProps = jsx:decode(Data),
    User = proplists:get_value(<<"user">>, UserProps),
    Password = proplists:get_value(<<"password">>, UserProps),
    case user_repository:login(User, Password) of 
        ok -> 
            {ok, Key} = file:read_file(?JWT_KEY_PATH),
            Jwt = jwerl:sign([{id, User}], hs256, Key),
            JSON = jsx:encode([{<<"token">>, Jwt}]),
            cowboy_req:reply(200, #{
                <<"Content-Type">> => <<"application/json">>
            }, JSON, Req);
        bad_credentials -> 
            cowboy_req:reply(401, #{
                <<"Content-Type">> => <<"text/plain">>
            }, <<"Bad Credentials">>, Req)
    end;

reply(#{method := <<"POST">>, has_body := false} = Req0, _Env) ->
	cowboy_req:reply(400, #{
		<<"Content-Type">> => <<"text/plain">>
	}, <<"Request has no body.">>, Req0).
