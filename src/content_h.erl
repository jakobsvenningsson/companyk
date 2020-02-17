-module(content_h).

-export([init/2]).

init(Req0, State) ->
    Req = reply(Req0, State),
	{ok, Req, State}.

reply(#{method := <<"POST">>, json := Content} = Req, #{user := ID}) ->
    NewContent = [{<<"sender_id">>, <<ID/binary>>} | Content],
    {ok, InsertedContent} = content_repository:insert_content(NewContent),
    Body = jsx:encode(InsertedContent),
    cowboy_req:reply(201, #{
        <<"Content-Type">> => <<"application/json">>
    }, Body, Req);

reply(#{method := <<"POST">>, has_body := false} = Req, _State) ->
    reply:json(400, <<"Request has no body.">>, Req);

reply(#{method := <<"GET">>} = Req, #{user := ID}) ->
    SId = cowboy_req:binding(sender_id, Req),
    Content = content_repository:get_content(SId, ID),
    Body = jsx:encode(Content),
    cowboy_req:reply(200, #{
        <<"Content-Type">> => <<"application/json">>
    }, Body, Req).

