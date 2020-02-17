-module(register_h).

-export([init/2]).

init(Req0, Env0) ->
    Reply = reply(Req0, Env0),
	{ok, Reply, Env0}.

reply(#{method := <<"POST">>, json := UserProps} = Req, _Env) ->
    %%{ok, Data, Req} = cowboy_req:read_body(Req0),
    %%UserProps = jsx:decode(Data),
    User = proplists:get_value(<<"user">>, UserProps),
    Password = proplists:get_value(<<"password">>, UserProps),
    case user_repository:reg(User, hash_pw(Password)) of 
        ok -> 
            cowboy_req:reply(201, #{
                <<"Content-Type">> => <<"application/json">>
            }, <<>>, Req);
        error -> 
            cowboy_req:reply(422, #{
                <<"Content-Type">> => <<"text/plain">>
            }, <<"Registration Failed">>, Req)
    end;

reply(#{method := <<"POST">>, has_body := false} = Req0, _Env) ->
	cowboy_req:reply(400, #{
	<<"Content-Type">> => <<"text/plain">>
    }, <<"Missing Request Body">>, Req0).

%% Internal functions

hash_pw(Password) ->
    {ok, Salt} = bcrypt:gen_salt(),
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    Hash.
