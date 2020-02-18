-module(content_h_tests).

-include_lib("eunit/include/eunit.hrl").

-include("content.hrl").
-include("test_helpers.hrl").

content_h_test_() ->
    [
        {"Querying content from a user which has no content should return an empty list",
         ?setup(fun get_empty_content/1)},
        {"Querying content from a user which has a single content should return a list containing the single content", 
         ?setup(fun get_single_content/1)},
        {"Querying content from a user which has multiple content should return a list containing all of the user's content",
        ?setup(fun get_multiple_content/1)},
        {"Querying content from a user should only return content where the querying user is the receiver",
        ?setup(fun get_only_content_from_querying_user/1)},
        {"Created content metadata should be stored by the server and inserted meta should be returned.",
        ?setup(fun upload_content_metadata/1)},
        {"Created meta should be stored by the server and uploaded file should be linked to the ID returned when creating the file meta.",
        ?setup(fun upload_content_metadata_and_file/1)},
        {"Paying a 'payable' content should mark the content as paid.",
        ?setup(fun pay_payable_content/1)},
        {"Paying a 'non-payable' content should fail.",
        ?setup(fun pay_not_payable_content/1)},
        {"Only the receiver of a content can pay it.",
        ?setup(fun only_receiver_can_pay_content/1)}
    ].

%% Setup functions 

start() ->
    {ok, Pid} = application:ensure_all_started(companyk),
    test_utils:clear_tables(),
    Token1 = test_utils:register_and_login_user(?USER1),
    Token2 = test_utils:register_and_login_user(?USER2),
    {Pid, Token1, Token2}.

stop({_Pid, _Token1, _Token2}) ->
    application:stop(companyk).

%% Tests

upload_content_metadata({_Pid, Token, _}) ->
    {Status, ReturnedMeta} = upload_content_meta(?CONTENT1, Token),
    Tmp = decode_record(<<?CONTENT1>>),
    % Append 'id', 'sender_id' and 'paid' fields
    OriginalMeta = Tmp#content_meta{id = 1, paid = false, sender_id = <<"1">>, uploaded=false},
    [
        ?_assertMatch(201, Status),
        ?_assertEqual(OriginalMeta, ReturnedMeta)
    ].

upload_content_metadata_and_file({_Pid, Token1, Token2}) ->
    {Status1, ReturnedMeta} = upload_content_meta(?CONTENT1, Token1),
    Status2 = upload_content(ReturnedMeta#content_meta.id, ?DATA1, Token1),
    {Status3, [MetaRecord]} = get_content_metadata_from_user("1", Token2),
    {Status4, Data} = get_content(MetaRecord#content_meta.id, Token2),
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(201, Status2),
        ?_assertMatch(200, Status3),
        ?_assertMatch(200, Status4),
        ?_assertEqual(true, MetaRecord#content_meta.uploaded),
        ?_assertEqual(Data, ?DATA1)
    ].

get_empty_content({_Pid, Token, _}) -> 
    {Status, Content} = get_content_metadata_from_user("2", Token),
    [
        ?_assertMatch(200, Status),
        ?_assertMatch([], Content)
    ].

get_single_content({_Pid, Token1, Token2}) -> 
    {Status1, PostContent} = upload_content_meta(?CONTENT1, Token1),
    {Status2, [GetContent]} = get_content_metadata_from_user("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(200, Status2),
        ?_assertEqual(PostContent, GetContent)
    ].

get_multiple_content({_Pid, Token1, Token2}) -> 
    {Status1, PostContent1} = upload_content_meta(?CONTENT1, Token1),
    {Status2, PostContent2} = upload_content_meta(?CONTENT2, Token1),
    {Status3, [GetContent1, GetContent2]} = get_content_metadata_from_user("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(201, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(PostContent1, GetContent1),
        ?_assertEqual(PostContent2, GetContent2)
    ].

get_only_content_from_querying_user({_Pid, Token1, Token2}) -> 
    {Status1, PostContent1} = upload_content_meta(?CONTENT1, Token1),
    {Status2, PostContent2} = upload_content_meta(?CONTENT2, Token1),
    {Status3, PostContent3} = upload_content_meta(?CONTENT3, Token2),
    {Status4, [GetContent1, GetContent2]} = get_content_metadata_from_user("1", Token2), 
    {Status5, [GetContent3]} = get_content_metadata_from_user("2", Token1), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(201, Status2),
        ?_assertMatch(201, Status3),
        ?_assertMatch(200, Status4),
        ?_assertMatch(200, Status5),
        ?_assertEqual(PostContent1, GetContent1),
        ?_assertEqual(PostContent2, GetContent2),
        ?_assertEqual(PostContent3, GetContent3)
    ].

pay_payable_content({_Pid, Token1, Token2}) ->
    {Status1, PostContent} = upload_content_meta(?CONTENT2, Token1),
    Status2 = make_payment(PostContent#content_meta.id, Token2),
    {Status3, [GetContent]} = get_content_metadata_from_user("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(200, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(true, GetContent#content_meta.paid),
        ?_assertEqual(false, PostContent#content_meta.paid),
        ?_assertEqual(PostContent#content_meta.sender_id, GetContent#content_meta.sender_id),
        ?_assertEqual(PostContent#content_meta.receiver_id, GetContent#content_meta.receiver_id),
        ?_assertEqual(PostContent#content_meta.is_payable, GetContent#content_meta.is_payable),
        ?_assertEqual(PostContent#content_meta.type, GetContent#content_meta.type),
        ?_assertEqual(PostContent#content_meta.id, GetContent#content_meta.id)
    ].

pay_not_payable_content({_Pid, Token1, Token2}) -> 
    {Status1, PostContent} = upload_content_meta(?CONTENT1, Token1),
    Status2 = make_payment(PostContent#content_meta.id, Token2),
    {Status3, [GetContent]} = get_content_metadata_from_user("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(400, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(PostContent, GetContent)
    ].

only_receiver_can_pay_content({_Pid, Token1, Token2}) ->
    {Status1, PostContent} = upload_content_meta(?CONTENT2, Token1),
    Status2 = make_payment(PostContent#content_meta.id, Token1),
    {Status3, [GetContent]} = get_content_metadata_from_user("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(401, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(PostContent, GetContent)
    ].

%% Helpers

upload_content_meta(Meta, Token) ->
    {ok, {{_, Status, _}, _, Body}} = test_utils:http_request(post, {?ENDPOINT ++ "/content", [?AUTH_H(Token)], Meta}),
    {Status, decode_record(Body)}.

upload_content(CId, Bin, Token) ->
    {ok, {{_, Status, _}, _, _}} = test_utils:http_request(post, {?ENDPOINT ++ "/content/" ++ integer_to_list(CId), [?AUTH_H(Token)], Bin, "application/octet-stream"}),
    Status.

get_content_metadata_from_user(From, Token) ->
    {ok, {{_, Status, _}, _, Body}} = test_utils:http_request(get, {?ENDPOINT ++ "/content/user/" ++ From, [?AUTH_H(Token)]}),
    {Status, decode_records(Body)}.

get_content(CId, Token) ->
    {ok, {{_, Status, _}, _, Body}} = test_utils:http_request(get, {?ENDPOINT ++ "/content/" ++ integer_to_list(CId), [?AUTH_H(Token)]}),
    {Status, Body}.

make_payment(CId, Token) ->
    {ok, {{_, Status, _}, _, _Body}} = test_utils:http_request(post, {?ENDPOINT ++ "/pay/" ++ integer_to_list(CId), [?AUTH_H(Token)], <<>>}),
    Status.

decode_record(JSON) -> 
    PropList = jsx:decode(JSON),
    proplist_to_record(PropList).

decode_records(JSON) -> 
    PropLists = jsx:decode(JSON),
    lists:map(fun(Record) -> proplist_to_record(Record) end, PropLists).

proplist_to_record(PropList) ->
    ID = proplists:get_value(<<"id">>, PropList),
    SId = proplists:get_value(<<"sender_id">>, PropList),
    RId = proplists:get_value(<<"receiver_id">>, PropList),
    Paid = proplists:get_value(<<"paid">>, PropList),
    IsPayable = proplists:get_value(<<"is_payable">>, PropList),
    Type = proplists:get_value(<<"type">>, PropList),
    Uploaded = proplists:get_value(<<"uploaded">>, PropList),
    #content_meta{id = ID, sender_id = SId, receiver_id = RId, is_payable = IsPayable, type = Type, paid = Paid, uploaded = Uploaded}.
