-record(content_meta, {
    id, 
    sender_id,
    receiver_id,
    type,
    is_payable,
    paid,
    uploaded
}).

-record(content, {
    id, 
    data
}).

