-module(auth_middleware).

-include("companyk.hrl").

-export([execute/2]).

execute(#{path := <<"/content", _Rest/binary>>} = Req, Env) ->
    AuthToken = cowboy_req:header(<<"authorization">>, Req),
    authenticate(AuthToken, Req, Env);

execute(#{path := <<"/pay", _Rest/binary>>} = Req, Env) ->
    AuthToken = cowboy_req:header(<<"authorization">>, Req),
    authenticate(AuthToken, Req, Env);

execute(Req0, Env0) ->
    {ok, Req0, Env0}.

%% Internal functions

authenticate(undefined, Req0, _Env0) ->
    Req = cowboy_req:reply(401,
                           #{<<"Content-Type">> => <<"text/plain">>}, 
                           <<"Missing Auth Header">>, Req0),
    {stop, Req};

authenticate(JWT, Req0, Env0 = #{handler_opts := Opts}) ->
    <<"Bearer ", Token/binary>> = JWT,
    {ok, Key} = file:read_file(?JWT_KEY_PATH),
    Res = try 
              jwerl:verify(Token, hs256, Key) 
          catch 
              Type:Error -> {Type, Error}
          end,
    case Res of 
        {ok, #{ id := ID }} ->
            Env = Env0#{handler_opts := Opts#{user => ID}},
            io:format("SUCCESSSFULL ~p~n", [ID]),
            {ok, Req0, Env};
        {error, _} -> 
            Req = cowboy_req:reply(401,
                                   #{<<"Content-Type">> => <<"text/plain">>}, 
                                   <<"Bad Credentials">>, Req0),
            {stop, Req}
    end.
