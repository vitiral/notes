# Basics
## Context
this is a container for all zmq sockets in a single process, and acts as the 
transport for all `inproc` sockets, which are the fastest way to connect
threads in a single process.

```
// Create
void *context = zmq_ctx_new();
// Destroy
zmq_ctx_destroy(context)
```

## Sockets
Sockets are the primary zmq communication handler

Creating/ destroying 
```
void *sock = zmq_socket(context, ZMQ_REQ);
zmq_close(sock);
```

There are multiple kinds of sockets, as defined by the second argument:

**Request/Reply Pattern Sockets**
- `ZMQ_REQ`: request socket. This allows only alternating calls to `zmq_send(request)` 
    and subsequent `zmq_recv(reply)`. Each request is round-robined to all *services*,
    and each reply received is matched with the last issued request.
- `ZMQ_REP`: reply socket. This allows only alternating calls to `zmq_recv(request)`
    and subsequent `zmq_send(reply)`. Each request received is fair-queued to all
    *clients*, and each reply sent is routed to the *client* that issued the last request

# Request Reply

```
void *requester = zmq_socket (context, ZMQ_REQ);
void *responder = zmq_socket (context, ZMQ_REP);
```

