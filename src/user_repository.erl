-module(user_repository).

-export([init_db/1, reg/2, login/2, get/1]).

-record(user, {
    id, 
    password
}).

init_db(Env) ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    try
        mnesia:table_info(user, all)
    catch 
        exit: _ ->
            Attr = {attributes, record_info(fields, user)},
            Opts = case Env of 
                test -> [Attr];
                prod -> [Attr, {disc_copies, [node()]}]
            end,
            {atomic, ok} = mnesia:create_table(user, Opts)
    end.


login(User, Password) ->
    T = fun() ->
        mnesia:read(user, User)            
    end, 
    case mnesia:transaction(T) of 
        {atomic, [#user{password = Hash}]} ->
            case bcrypt:hashpw(Password, Hash) of 
                {ok, Hash} -> ok;
                _ -> bad_credentials
            end;
        _ ->  bad_credentials 
    end.

reg(User, Password) ->
    T = fun() ->
        case mnesia:read(user, User) of 
            [] -> 
                mnesia:write(#user{id = User, password = Password});
            [_User] -> error
        end
    end, 
    case mnesia:transaction(T) of
        {atomic, ok} -> ok;
        {atomic, error} -> error 
    end.

get(User) ->
    T = fun() ->
        mnesia:read(user, User)
    end,
    case mnesia:transaction(T) of 
        {atomic, []} -> error;
        {atomic, [U]} -> {ok, U}
    end.
            
%% Internal functions
