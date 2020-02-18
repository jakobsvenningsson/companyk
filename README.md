companyk
=====

companyk is a simple webservice which enabled content producers and consumers to send and receive content. Both the consumer and producer APIs are implemented as HTTP-based RESTful APIs. It is also possible for consumers to "pay" for payable content.

Build
-----

    $ rebar3 compile

Run
-----

    $ rebar3 shell

Run Tests
-----

    $ ERL_AFLAGS="-config config/test.config" rebar3 eunit

Usage Instructions
-----

1. <b>Retrieve API Token</b>. Both the consumer and producer API requires its users to identify themselves with a API token. API tokens can be retrieved by first registering a new user using the <b>/register</b> endpoint and afterward the <b>/login endpoint</b>.
        
        1> curl -X POST \
        -H 'content-type: application/json' \
        -d '{"user:": "1", "password": "pw123"}' \
        localhost:8080/register
        2> curl -X POST \
        -H 'content-type: application/json' \
        -d '{"user:": "1", "password": "pw123"}' \
        localhost:8080/login

2. <b>Create new Content</b>. Uploading content is a two-step process. The first step involves uploading metadata associated with the content to be uploaded. This is achieved by submitting a POST request to the </b>/content</b> endpoint with the metadata appended as json forma in the request body. The first request will return a content id which is used in the second step to link the uploaded content to its metadata. The second step uploads the actual content data which is done by submitting a POST request to <b>/content/:content_id</b>.
   
        1> curl -X POST \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        -H 'content-type: application/json' \
        -d '{"receiver_id": "2", "type": "invoice", "is_payable": true}' \
        localhost:8080/content
        2> curl -X POST \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        -H 'content-type: application/octet-stream' \
        -data-binary @- \
        localhost:8080/content/:content_id < <(echo "some_file_data")

3. <b>Retrieve Content.</b> The query will return all content metadata sent from the user "1" to the user associated with the used API token, i.e. a user can only retrieve content destined to itself. The metadata contains a the id of the content which can be used to download the content by submitting a GET request to the <b>/content/:content_id</b> endpoint.
   
        1> curl -X GET \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        localhost:8080/content/user/:user
        2> curl -X GET \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        localhost:8080/content/content/:content_id

4. <b>Pay Content</b>. The query will mark the content associated with :content_id as paid, given that the receiver of the content is user associated with the used API token, i.e. users can only pay for content destined for themselves. 

        $ curl -X POST \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        localhost:8080/pay/:content_id

Hosting Proposal
-----

CLoud...

User Requirements
-----

1. Can’t lose data

2. Content should be available for the users to read within 1 hour after it was sent

3. Sender wants to send data in batches so peaks of 50 requests per second should be
expected.
