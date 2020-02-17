-module(content_repository).

-export([init_db/1, get_content/2, insert_content/1, mark_as_paid/2]).

-include("content.hrl").

-record(content_ids, {table_name, last_id}).

init_db(Env) ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    % Init content table
    try
        mnesia:table_info(content, all)
    catch 
        exit: _ ->
            AttrContentDB = [{attributes, record_info(fields, content)}, {index, [#content.sender_id]}],
            OptsContentDB = case Env of 
                test -> AttrContentDB;
                prod -> [{disc_copies, [node()]} | AttrContentDB]
            end,
            {atomic, ok} = mnesia:create_table(content, OptsContentDB)
    end,
    % Init id counter table
    try 
        mnesia:table_info(content_ids, all)
    catch 
        exit: _ ->
            AttrIdDB = [{attributes, record_info(fields, content_ids)}],
            OptsIdDB = case Env of 
                test -> AttrIdDB;
                prod -> [{disc_copies, [node()]} | AttrIdDB]
            end,
            {atomic, ok} = mnesia:create_table(content_ids, OptsIdDB),
            T = fun() ->
                mnesia:write(content_ids, #content_ids{table_name = content_id,
                                                              last_id = 0})
            end,
            mnesia:transaction(T)
    end.


insert_content(ContentProps) ->
    ContentRecord = proplist_to_record(ContentProps),
    T = fun() ->
        mnesia:write(ContentRecord)            
    end, 
    {atomic, ok} = mnesia:transaction(T),
    NewContentProps = [{<<"id">>, ContentRecord#content.id},
                       {<<"paid">>, ContentRecord#content.paid} | ContentProps],
    {ok, NewContentProps}.

get_content(SId, RId) ->
    T = fun() ->      
        mnesia:index_match_object({content, '_', SId, RId, '_', '_', '_', '_'}, 
                                  #content.sender_id)
    end, 
    {atomic, Contents} = mnesia:transaction(T), 
    F = fun(X, Y) -> {X#content.id} < {Y#content.id} end,
    SortedContents = lists:sort(F, Contents),
    lists:map(fun(Content) ->
        lists:zip(record_info(fields, content), tl(tuple_to_list(Content))) end, SortedContents).

mark_as_paid(ContentID, UserID) ->
    T = fun() ->
        case mnesia:read(content, binary_to_integer(ContentID)) of 
            [Content = #content{ is_payable = true, receiver_id = UserID}] ->
                UpdatedContent = Content#content{ paid = true },
                {mnesia:write(UpdatedContent), UpdatedContent};
            [#content{ is_payable = false, receiver_id = UserID}] ->
                not_payable;
            [] -> 
                content_does_not_exist;
            _ -> not_authroized
        end
    end, 
    {atomic, Res} = mnesia:transaction(T),
    Res.

%% Internal functions

proplist_to_record(Props) ->
    Index = mnesia:dirty_update_counter(content_ids, last_id, 1),
    #content{id = Index,
             sender_id = proplists:get_value(<<"sender_id">>, Props),
             receiver_id = proplists:get_value(<<"receiver_id">>, Props), 
             data = proplists:get_value(<<"data">>, Props),
             type = proplists:get_value(<<"type">>, Props),
             is_payable = proplists:get_value(<<"is_payable">>, Props), paid = false}.
