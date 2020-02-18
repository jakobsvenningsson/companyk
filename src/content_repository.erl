-module(content_repository).

-export([init_db/1, get_content_metadata/2, get_content/2, insert_metadata/1, insert_content/2, mark_as_paid/2]).

-include("content.hrl").

-record(content_ids, {table_name, last_id}).

init_db(Env) ->
    mnesia:create_schema([node()]),
    application:start(mnesia),
    
    AttrContentMetaDB = [{attributes, record_info(fields, content_meta)}, {index, [#content_meta.sender_id]}],
    create_table(content_meta, AttrContentMetaDB, Env), 
    create_table(content, [{attributes, record_info(fields, content)}], Env), 
    create_table(content_ids, [{attributes, record_info(fields, content_ids)}], Env).

insert_metadata(ContentProps) ->
    ContentRecord = proplist_to_record(ContentProps),
    T = fun() ->
        mnesia:write(ContentRecord)            
    end, 
    {atomic, ok} = mnesia:transaction(T),
    NewContentProps = [{<<"id">>, ContentRecord#content_meta.id},
                       {<<"uploaded">>, ContentRecord#content_meta.uploaded},
                       {<<"paid">>, ContentRecord#content_meta.paid} | ContentProps],
    {ok, NewContentProps}.

insert_content(Bin, CId) ->
    Content = #content{id = binary_to_integer(CId), data = Bin},
    T = fun() ->
        case mnesia:read(content_meta, Content#content.id) of 
            [Meta] -> 
                mnesia:write(Meta#content_meta{uploaded = true}),
                mnesia:write(Content);
            [] -> not_exist
        end
    end, 
    {atomic, Res} = mnesia:transaction(T),
    Res.

get_content_metadata(SId, RId) ->
    T = fun() ->      
        mnesia:index_match_object({content_meta, '_', SId, RId, '_', '_', '_', '_'}, 
                                  #content_meta.sender_id)
    end, 
    {atomic, Contents} = mnesia:transaction(T), 
    F = fun(X, Y) -> {X#content_meta.id} < {Y#content_meta.id} end,
    SortedContents = lists:sort(F, Contents),
    lists:map(fun(Content) ->
        lists:zip(record_info(fields, content_meta), tl(tuple_to_list(Content))) end, SortedContents).

get_content(CId, _User) ->
    T = fun() ->
        case mnesia:read(content, binary_to_integer(CId)) of 
            [#content{data = Data}] -> 
                {ok, Data};
            [] -> not_exist
        end
    end, 
    {atomic, Res} = mnesia:transaction(T),
    Res.

mark_as_paid(ContentID, UserID) ->
    T = fun() ->
        case mnesia:read(content_meta, binary_to_integer(ContentID)) of 
            [Content = #content_meta{ is_payable = true, receiver_id = UserID}] ->
                UpdatedContent = Content#content_meta{ paid = true },
                {mnesia:write(UpdatedContent), UpdatedContent};
            [#content_meta{ is_payable = false, receiver_id = UserID}] ->
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
    #content_meta{id = Index,
             sender_id = proplists:get_value(<<"sender_id">>, Props),
             receiver_id = proplists:get_value(<<"receiver_id">>, Props), 
             type = proplists:get_value(<<"type">>, Props),
             is_payable = proplists:get_value(<<"is_payable">>, Props), paid = false, uploaded = false}.

create_table(Name, Attrs, Env) ->
    try
        mnesia:table_info(Name, all)
    catch 
        exit: _ ->
            Opts = case Env of 
                test -> Attrs;
                prod -> [{disc_copies, [node()]} | Attrs]
            end,
            {atomic, ok} = mnesia:create_table(Name, Opts)
    end.
