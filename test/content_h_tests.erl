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
        {"Created content should be stored by the server and the created content should be returned.",
        ?setup(fun create_content/1)},
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
    mnesia:clear_table(user),
    mnesia:clear_table(content),
    mnesia:clear_table(content_ids),
    Token1 = test_utils:register_and_login_user(?USER1),
    Token2 = test_utils:register_and_login_user(?USER2),
    {Pid, Token1, Token2}.

stop({_Pid, _Token1, _Token2}) ->
    application:stop(companyk).

%% Tests

create_content({_Pid, Token, _}) ->
    {Status, ReturnedRecord} = create_content(?CONTENT1, Token),
    Tmp = decode_record(<<?CONTENT1>>),
    % Append 'id', 'sender_id' and 'paid' fields
    OriginalRecord = Tmp#content{id = 1, paid = false, sender_id = <<"1">>},
    [
        ?_assertMatch(201, Status),
        ?_assertEqual(ReturnedRecord, OriginalRecord)
    ].

get_empty_content({_Pid, Token, _}) -> 
    {Status, Content} = get_content("2", Token),
    [
        ?_assertMatch(200, Status),
        ?_assertMatch([], Content)
    ].

get_single_content({_Pid, Token1, Token2}) -> 
    {Status1, PostContent} = create_content(?CONTENT1, Token1),
    {Status2, [GetContent]} = get_content("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(200, Status2),
        ?_assertEqual(PostContent, GetContent)
    ].

get_multiple_content({_Pid, Token1, Token2}) -> 
    {Status1, PostContent1} = create_content(?CONTENT1, Token1),
    {Status2, PostContent2} = create_content(?CONTENT2, Token1),
    {Status3, [GetContent1, GetContent2]} = get_content("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(201, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(PostContent1, GetContent1),
        ?_assertEqual(PostContent2, GetContent2)
    ].

get_only_content_from_querying_user({_Pid, Token1, Token2}) -> 
    {Status1, PostContent1} = create_content(?CONTENT1, Token1),
    {Status2, PostContent2} = create_content(?CONTENT2, Token1),
    {Status3, PostContent3} = create_content(?CONTENT3, Token2),
    {Status4, [GetContent1, GetContent2]} = get_content("1", Token2), 
    {Status5, [GetContent3]} = get_content("2", Token1), 
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
    {Status1, PostContent} = create_content(?CONTENT2, Token1),
    Status2 = make_payment(integer_to_list(PostContent#content.id), Token2),
    {Status3, [GetContent]} = get_content("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(200, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(true, GetContent#content.paid),
        ?_assertEqual(false, PostContent#content.paid),
        ?_assertEqual(PostContent#content.sender_id, GetContent#content.sender_id),
        ?_assertEqual(PostContent#content.receiver_id, GetContent#content.receiver_id),
        ?_assertEqual(PostContent#content.is_payable, GetContent#content.is_payable),
        ?_assertEqual(PostContent#content.data, GetContent#content.data),
        ?_assertEqual(PostContent#content.type, GetContent#content.type),
        ?_assertEqual(PostContent#content.id, GetContent#content.id)
    ].

pay_not_payable_content({_Pid, Token1, Token2}) -> 
    {Status1, PostContent} = create_content(?CONTENT1, Token1),
    Status2 = make_payment(integer_to_list(PostContent#content.id), Token2),
    {Status3, [GetContent]} = get_content("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(400, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(PostContent, GetContent)
    ].

only_receiver_can_pay_content({_Pid, Token1, Token2}) ->
    {Status1, PostContent} = create_content(?CONTENT2, Token1),
    Status2 = make_payment(integer_to_binary(PostContent#content.id), Token1),
    {Status3, [GetContent]} = get_content("1", Token2), 
    [
        ?_assertMatch(201, Status1),
        ?_assertMatch(401, Status2),
        ?_assertMatch(200, Status3),
        ?_assertEqual(PostContent, GetContent)
    ].

%% Helpers


create_content(Content, Token) ->
    {ok, {{_, Status, _}, _, Body}} = test_utils:http_request(post, {?ENDPOINT ++ "/content", [?AUTH_H(Token)], Content}),
    {Status, decode_record(Body)}.

get_content(From, Token) ->
    {ok, {{_, Status, _}, _, Body}} = test_utils:http_request(get, {?ENDPOINT ++ "/content/" ++ From, [?AUTH_H(Token)]}),
    {Status, decode_records(Body)}.

make_payment(ContentID, Token) ->
    {ok, {{_, Status, _}, _, _Body}} = test_utils:http_request(post, {?ENDPOINT ++ "/pay/" ++ ContentID, [?AUTH_H(Token)], <<>>}),
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
    Data = proplists:get_value(<<"data">>, PropList),
    Paid = proplists:get_value(<<"paid">>, PropList),
    IsPayable = proplists:get_value(<<"is_payable">>, PropList),
    Type = proplists:get_value(<<"type">>, PropList),
    #content{id = ID, sender_id = SId, receiver_id = RId, data = Data, is_payable = IsPayable, type = Type, paid = Paid}.
