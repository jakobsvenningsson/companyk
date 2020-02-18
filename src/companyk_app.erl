%%%-------------------------------------------------------------------
%% @doc companyk public API
%% @end
%%%-------------------------------------------------------------------

-module(companyk_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, Env} = application:get_env(companyk, env),
    user_repository:init_db(Env),
    content_repository:init_db(Env),
    Dispatch = cowboy_router:compile([
		{'_', [
               {"/content/[:sender_id]", content_h, #{}},
               {"/content/user/[:sender_id]", content_h, #{}},
               {"/register", register_h, #{}},
               {"/pay/:id", payment_h, #{}},
               {"/login", login_h, #{}}
        ]}
	]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
    		env => #{dispatch => Dispatch},
            middlewares => [cowboy_router, log_middleware, auth_middleware, json_parser_middleware, cowboy_handler]
	}),
    companyk_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).

%% internal functions
