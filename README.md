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

I would host the application using a cloud provider such as AWS. Deploying applications in the cloud is prefered since it is flexible (it's easy to increase or decrease availible resources on demand) and cost-efficient (you only pay for the resources you use). I would run my application inside of a docker container since AWS have features which makes it possible to easily deploy containerized applications.

User Requirements
-----

1. Can’t lose data.
   I have choosen to use erlangs built in database <b>mnesia</b> for persistant storage. 

2. Content should be available for the users to read within 1 hour after it was sent.
   
   Content is availible for the consumer immediately after the producer have finished uploading the content to the server.  

3. Sender wants to send data in batches so peaks of 50 requests per second should be
expected.
My implementation can handle multiple request concurrently. My implementation uses 10 acceptor processes, i.e. 10 processes with the sole job of accepting incoming connections and spawning processes which will handle the incoming requests. This should be enough to handle 50 requests per second. However, the number of requests that can be handled concurrently is limited by the server's availible computing resources. For instance, if each incoming request takes a very long time; then 50 requests per second might not be possible.   
