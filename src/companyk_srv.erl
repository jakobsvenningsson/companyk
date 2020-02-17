-module(companyk_srv).
-behaviour(gen_server).

-export([init/1, terminate/2, handle_call/3, start_link/0, handle_info/2, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init(_Args) ->
    {ok, {}, 0}.

handle_info(timeout, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
