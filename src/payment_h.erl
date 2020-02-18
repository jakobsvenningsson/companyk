-module(payment_h).

-include("content.hrl").

-export([init/2]).

init(Req0, State) ->
    Req = reply(Req0, State),
	{ok, Req, State}.

reply(#{method := <<"POST">>} = Req, #{user := ID}) ->
    ContentID = cowboy_req:binding(id, Req),
    case content_repository:mark_as_paid(ContentID, ID) of 
        {ok, Content} -> 
            Proplist = lists:zip(record_info(fields, content_meta), tl(tuple_to_list(Content))),
            JSON = jsx:encode(Proplist),
            cowboy_req:reply(200, #{
                <<"Content-Type">> => <<"application/json">>
            }, JSON, Req);
        not_authroized -> 
            cowboy_req:reply(401, #{
                <<"Content-Type">> => <<"text/plain">>
            }, <<"Only the receiver of the content can pay it">>, Req);
        content_does_not_exist -> 
            cowboy_req:reply(400, #{
                <<"Content-Type">> => <<"text/plain">>
            }, <<"Content does not exist">>, Req);
        not_payable -> 
            cowboy_req:reply(400, #{
                <<"Content-Type">> => <<"text/plain">>
            }, <<"Content not payable">>, Req)
    end;

reply(#{method := <<"POST">>, has_body := false} = Req, _State) ->
    reply:json(400, <<"Request has no body.">>, Req).
