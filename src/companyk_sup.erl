%%%-------------------------------------------------------------------
%% @doc companyk top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(companyk_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?SERVER, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 10},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

child_spec() -> #{id => companyk_srv,
                 start => {companyk_srv, start_link, []},
                 restart => permanent, 
                 shutdown => 2000,
                 type => worker,     
                 modules => [companyk_srv]}.
