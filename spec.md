# Protocol Spec


## Summary
Weave is a bidirectional session layer multiplexing protocol similar to ipfs's multistream, uber's TChannel, and twitter's mux protocol. 


## Use cases
There are a lot of complicated things going on in distributed systems, that are pushed into the application layer that shouldn't be such as service discovery, routing, liveness checking, distributed authentication, tracing etc, the session layer is a better place to put things.

Connection Establishment is expensive, thus sharing connections is useful in many cases.

Routing RPC requests.
Being able to choose whether to perform requests based off of certain metrics.

Implementing distributed algorithms in layer 5, so you users at layer 7 don't have to worry about it, such as gossip protocols, job control, replication, routing, load balancing etc.  

Running different protocols on a single socket.

Trivial to implement http on top of it, in order to gain things like connection streaming and multiplexing since ocaml doesn't have http2 yet.

Eliminating HOL




## Notation 



Let field[c] be a field who's contents are a repeat sequence of size c.

Let field:s be a field that takes s bytes.

Let field~s be a field who's contents are of size s represented as an unsigned integer of size bytes. ie size:4 body:size 

Note all integers are unsigned. 




## Frame

All messages are embedded into this format.
size:4 type:1 version:2 tag:4 body~4


Here example of how it's used.
TINIT = size:4 0x03:1 tag:4  (count:2 (key~2 value~2)[count] )~4




## Headers

count:2 (key~2, value~2)[count]

Headers are a utf8 key value pairs

They are used for a variety of things in the protocol such as,
negotiating content type, tls, embedding routing information, etc .

It's prepended by a 16 bit int representing the number of pairs. 

Each key value string is prepended by a 16 bit integer, to denote length.








## Message Types
Messages are distinguished by T and R which denote transmitter and receiver.
RMSGs are matched by their corresponding TMSGS by their corresponding tag.

Both client and server can send messages of any kind. 


### TPING
size:4 0x01:1 tag:4 
Used to check liveness of peer.

### RPING
size:4 0x02:1 tag:4
Reply to tping.


### TINIT
size:4 0x03:1 version:2 tag:4 

Clients usually send this to begin an exchange.
In the future it will also be used to negotiate versions.
The headers are used to negotiate things like tls parameters, or request deadlines.

It can be used for auth before an exchange begins. 
You can't send any messages before you recieve the corresponding RINIT. 



### RINIT

size:4 0x03:1 version:2 tag:4 (key~2 value~2)[count]
Response for TINIT.


### TREQ
size:4 0x04:1 version:2 tag:4 count:2 (key~2 value~2)[count] path~2 body~4

Carries headers,followed by a logical destination represented as a utf8 encoded string, and the request body.

The destination can be used to route requests.
IE dev/dns/myapp/mydomain/com or hosta/ipc/workers/$, ip/10.1.120.6, dc/nyc/cluster/1



### RREQ
size:4 0x05 version:2 tag:4 (key~2 value~2)[count] status:1 body~4

Replies to a TREQ

Carries headers, a status code represented as a 1 bit integer, and a string containing the reason for the error.



Status codes are as follows
0 = OK request succesful.
1 = ERROR generic
2 = NACK didn't even attempt request.






### TCLOSE
size:4 0x06:1 version:2 tag:4 
Tells the other side to close session. 


### RCLOSE
size:4 0x06 version:2 tag:4 
Acknowledges TCLOSE and does it.





## Workflows


Request response initiated by client
```

  TINIT -> Server
  RINIT -> Client
  TREQ -> Server
  RREQ -> Client 


```



Request response initiated by server
```
  let Client = Server.conns.get(peer)

  TINIT -> Client
  RINIT -> Server

  TREQ -> Client
  RREQ -> Server 


```

