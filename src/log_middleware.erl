-module(log_middleware).

-export([execute/2]).

execute(#{path := Path, host := Host, method := Method} = Req, Env) ->
    io:format("New Request: ~p ~p ~p~n", [binary_to_atom(Host, latin1), 
                                          binary_to_atom(Method, latin1),
                                          binary_to_atom(Path, latin1)]),
    {ok, Req, Env}.
