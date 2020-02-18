companyk
=====

companyk is a simple webservice which enables content producers and consumers to send and receive content. Both the consumer and producer APIs are implemented as HTTP-based RESTful APIs. It is also possible for consumers to "pay" for payable content.

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

1. <b>Retrieve API Token</b>. Both the consumer and producer API require its users to identify themselves with an API token. API tokens can be retrieved by first registering a new user using the <b>/register</b> endpoint and afterward the <b>/login endpoint</b>.
        
        1> curl -X POST \
        -H 'content-type: application/json' \
        -d '{"user:": "1", "password": "pw123"}' \
        localhost:8080/register
        2> curl -X POST \
        -H 'content-type: application/json' \
        -d '{"user:": "1", "password": "pw123"}' \
        localhost:8080/login

2. <b>Create new Content</b>. Uploading content is a two-step process. The first step involves uploading metadata associated with the content to be uploaded. This is achieved by submitting a POST request to the </b>/content</b> endpoint with the metadata appended as json format in the request body. The first request will return a content id which is used in the second step to link the uploaded content to its metadata. The second step uploads the actual content data which is done by submitting a POST request to <b>/content/:content_id</b>.
   
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

3. <b>Retrieve Content.</b> The <b>/content/user/:user_id</b> endpoint returns the metadata of all content sent from the user :user_id to the user associated with the used API token, i.e. a user can only retrieve content destined to itself. The metadata contains the id of the content that can be used to download the content by submitting a GET request to the <b>/content/:content_id</b> endpoint.
   
        1> curl -X GET \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        localhost:8080/content/user/:user_id
        2> curl -X GET \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        localhost:8080/content/:content_id

4. <b>Pay Content</b>. The query will mark the content associated with :content_id as paid, given that the receiver of the content is the user associated with the used API token, i.e. users can only pay for content destined for themselves. 

        $ curl -X POST \
        -H 'Authorization: Bearer <<YOUR_API_TOKEN>>' \
        localhost:8080/pay/:content_id

Hosting Proposal
-----

I would host the application using a cloud provider such as AWS. Deploying applications in the cloud is prefered since it is flexible (it's easy to increase or decrease availible resources on demand) and cost-efficient (you only pay for the resources you use). I would also run my application inside of a docker container since AWS has features which makes it possible to easily deploy containerized applications. Using containers also have other advantages, such as it simplifying handling and controling of the environnment which the application will run and you can have one environment for both production and development.

User Requirements
-----

1. <b>Can’t lose data</b>. 
   <br>I have choosen to use erlang's built in database <b>mnesia</b> for persistant storage. My main reason being that it fulfils the needs for the given application, it has built in support for distribution and convenient to use. However, it is possible that there are other options that would have been more suitible for the given task. Currently, my application only has support to run on a single node, i.e. if the harddrive of that node would fail then the data would be lost. However, it would be possible to add support for multiple nodes without too much effort due to erlang's built in support for distribution and mnesia. Using multiple nodes and multiple replicas of the database would make it possible to create better resilience towards harddisk failures and improve availibility. Additionally, using multiple nodes would require some changes to the hosting architecture since multiple instances of the server would be running with different addresses. Clients should not talk to any server directly when using multiple instances of the server, instead they should talk to a load balancer which distributes the incoming requests to different servers. This is possible using AWS.
<br>
1. <b>Content should be available for the users to read within 1 hour after it was sent.</b><br>
Content is availible for the consumer immediately after the producer have finished uploading the content to the server.
<br> 

3. <b>Sender wants to send data in batches so peaks of 50 requests per second should be
expected.</b>
<br>My implementation can handle multiple request concurrently. My implementation uses 10 acceptor processes, i.e. 10 processes with the sole job of accepting incoming connections and spawning processes which will handle the incoming requests. This should be enough to handle 50 requests per second (given that the requests are not very heavy). The number of requests that can be handled concurrently is also limited by the server's availible computing resources and the resources required for each request. For instance, if each incoming request takes a very long time; then 50 requests per second might not be a problem. This could possibly occur in my implementation if there are 50 concurrent file upload requests which all contain 100MB of data. However, this problem could be mitigated by using a seperate server for file uploads. This is possible since uploading content is a 2-step process and it is possible to store the content and the metadata on different servers. To reduce the load on the servers which are running my application, an option would be to outsource the hosting of the content to a third party provider and only store the metadata. However, this could be a problem if the content is sensitive and the third part is not trusted. Another possible solution to improve performance would be to use a content delivery network (CDN) to handle file downloads. It is also possible to increase throughput by running multiple instances of the server on different nodes, as discussed in (1) above.
