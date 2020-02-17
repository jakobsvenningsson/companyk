companyk
=====

companyk is a simple webservice which enabled content producers and consumers to send and receive content. Both the consumer and producer APIs are implemented as HTTP-based RESTful APIs. It is also possible for consumers to "pay" for payable content.

Build
-----

    $ rebar3 compile
    $ rebar3 shell

Run Tests
-----
    $ ERL_AFLAGS="-config config/test.config" rebar3 eunit

Usage Instructions
-----

1. <b>Retrieve API token</b>. Both the consumer and producer API requires its users to identify themselves with using a API token. API tokens can be retrieved by first registering a new user using the /register endpoint and afterward the /login endpoint.
        
        1> curl -X POST -d '{"name:": "johndoe", "password": "pw123"}' localhost:8080/register
        2> curl -X POST -d '{"name:": "johndoe", "password": "pw123"}' localhost:8080/login

2. <b>Create new content</b>. The sender_id field of the created content will be automatically set by the server to the user id associated with the used API token. 

        $ curl -X POST -H "Authorization: Bearer <<YOUR_API_TOKEN>>
        -d '{"receiver_id": "2", "data": "123456789", "type": "invoice", "is_payable": true}'
        localhost:8080/content

3. <b>Retrieve content.</b> The query will return all content sent from the user "johndoe" to the user associated with the used API token, i.e. a user can only retrieve content destined to itself.
   
        $ curl -X GET -H "Authorization: Bearer <<YOUR_API_TOKEN>>
        localhost:8080/content/:user

4. <b>Pay content</b>. The query will mark the content associated with :content_id as paid, given that the receiver of the content is user associated with the used API token, i.e. users can only pay for content destined for themselves. 

        $ curl -X POST -H "Authorization: Bearer <<YOUR_API_TOKEN>>
        localhost:8080/pay/:content_id