
# Designing a Protocol
These questions are from the first page of the zmq guide. I want to answer each one
in my own words, and then see how zmq handles them.

> How do we handle I/O? Does our application block, or do we handle I/O in the 
> background? This is a key design decision. Blocking I/O creates architectures 
> that do not scale well. But background I/O can be very hard to do right.
We have to do background I/O. Blocking I/O is just NOT acceptable!

We can do this in an asyncio loop for simple (threadless) C

zmq does this asynchrounously in background threads. These communicate with 
application threads using lock-free data structures, so concurrent zmq applications
need no locks, semaphores, or other wait states

> How do we handle dynamic components, i.e., pieces that go away temporarily? Do 
> we formally split components into "clients" and "servers" and mandate that 
> servers cannot disappear? What then if we want to connect servers to servers?
> Do we try to reconnect every few seconds?

We have to handle dynamic components. Clients are servers, although their can be
special "servers" to handle clients, but they have to be able to go down / 
self-healing mesh network.

In zmq, components can come and go and start in any order, and zmq will 
automatically reconnect.

When data is being queued, it handles by either blocking or throwing away data,
depending on the "pattern"

> How do we represent a message on the wire? How do we frame data so it's easy 
> to write and read, safe from buffer overflows, efficient for small messages, 
> yet adequate for the very largest videos of dancing cats wearing party hats?
Wow, yes these are all important. Include data in the message for the total
length? But then how long do you buffer?

> How do we handle messages that we can't deliver immediately? Particularly, 
> if we're waiting for a component to come back online? Do we discard messages,  
> put them into a database, or into a memory queue?
Probably discard, but this could be a tough choice.

> Where do we store message queues? What happens if the component reading from 
> a queue is very slow and causes our queues to build up? What's our strategy 
> then? How do we handle lost messages? Do we wait for fresh data, request a 
> resend, or do we build some kind of reliability layer that ensures messages 
> cannot be lost? What if that layer itself crashes?

The reliability layer sounds like the best option -- but what does this mean
for performance? How long do we wait? I suppose we must drop data eventually,
right?

> What if we need to use a different network transport. Say, multicast instead 
> of TCP unicast? Or IPv6? Do we need to rewrite the applications, or is the 
> transport abstracted in some layer?

This NEEDS to be abstracted. Anything that can be a "socket" should be supported,
everything from TCP to a CAN bus

> How do we route messages? Can we send the same message to multiple peers? Can 
> we send replies back to an original requester?
yes!

> How do we write an API for another language? Do we re-implement a wire-level
> protocol or do we repackage a library? If the former, how can we guarantee
> efficient and stable stacks? If the latter, how can we guarantee
> interoperability?

This is the question I am trying to answer!

> How do we represent data so that it can be read between different architectures?
> Do we enforce a particular encoding for data types? How far is this the
> job of the messaging system rather than a higher layer?

No, raw data ONLY!

> How do we handle network errors? Do we wait and retry, ignore them silently, or abort?

Throw your hands in the air, wave them like you just don't care.
