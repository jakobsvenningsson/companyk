-module(content_h).

-export([init/2]).

init(Req0, State) ->
    Req = reply(Req0, State),
	{ok, Req, State}.

reply(#{method := <<"POST">>, json := Content} = Req, #{user := ID}) ->
    NewContent = [{<<"sender_id">>, <<ID/binary>>} | Content],
    {ok, InsertedContent} = content_repository:insert_metadata(NewContent),
    Body = jsx:encode(InsertedContent),
    cowboy_req:reply(201, #{
        <<"Content-Type">> => <<"application/json">>
    }, Body, Req);

reply(#{method := <<"POST">>, has_body := true} = Req0, #{user := _ID}) ->
    {ok, Bin, Req} = cowboy_req:read_body(Req0),
    CId = cowboy_req:binding(sender_id, Req),
    case content_repository:insert_content(Bin, CId) of 
        ok -> 
            cowboy_req:reply(201, #{
                <<"content-type">> => <<"text/plain">>
             }, <<>>, Req);
        not_exist -> 
            cowboy_req:reply(400, #{
                <<"content-type">> => <<"text/plain">>
             }, <<"Invalid Content ID">>, Req)
    end;

reply(#{method := <<"POST">>, has_body := false} = Req, _State) ->
    reply:json(400, <<"Request has no body.">>, Req);

reply(#{method := <<"GET">>, path := <<"/content/user", _/binary>>} = Req, #{user := ID}) ->
    SId = cowboy_req:binding(sender_id, Req),
    Content = content_repository:get_content_metadata(SId, ID),
    Body = jsx:encode(Content),
    cowboy_req:reply(200, #{
        <<"Content-Type">> => <<"application/json">>
    }, Body, Req);

reply(#{method := <<"GET">>} = Req, #{user := ID}) ->
    CId = cowboy_req:binding(sender_id, Req),
    case content_repository:get_content(CId, ID) of
        {ok, Data} -> 
            cowboy_req:reply(200, #{
                <<"content-type">> => <<"application/octet-stream">>
             }, Data, Req);
        not_exist -> 
            cowboy_req:reply(400, #{
                <<"content-type">> => <<"text/plain">>
             }, <<"Invalid Content ID">>, Req)
    end.
