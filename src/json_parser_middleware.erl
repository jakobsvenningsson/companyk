-module(json_parser_middleware).

-export([execute/2]).

execute(Req0 = #{has_body := true}, Env0)->
    {ok, Data, Req1} = cowboy_req:read_body(Req0),
    try jsx:decode(Data) of
        JSON -> 
            {ok, Req1#{json => JSON}, Env0}
    catch 
        error:_ ->
            Req2 = cowboy_req:reply(400, 
                                   #{<<"content-type">> => <<"application/json">>},
                                   <<"Invalid json">>, Req1),
            {stop, Req2}
    end;

execute(Req0, Env0) ->
    {ok, Req0, Env0}.
