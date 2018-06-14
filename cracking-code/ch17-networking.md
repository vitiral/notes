# Networking

There are 7 layers to the networking stack
- Layer 7: Application Layer: the high level protocols where data has been
  fully assembeld. i.e. HTTP, DNS, FTP, SNMP, DHCP
- Layer 6: Presentation/Syntax Layer: less a packet and more an actual data
  structure. Some parts of HTTP are in this layer, such as the encoding.
  Encryption happens here.
- Layer 5: Session Layer: opening, closing and managing a connection session
- Layer 4: Transport Layer: the Control Protocol, i.e. TCP or UDP
- Layer 3: Network/Internet Layer: i.e. router, modem, etc.
- Layer 2: Data Link Layer: i.e. switches
- Layer 1: Physical Layer (i.e. network card, wire, fiber, etc)


## Internet Addresses
Each computer must have a unique address, known as the Internet Protocol (IP)
address.

## Subnet Mask
What is a network/subnet mask? Explain how host A sends a message/packet
to host B when:
- (a) both are on same network
- (b) both are on different networks
- Explain which layer makes the routing decision and how.

A subnet defines the IP addresses that can be used within a network. It is used
to designate subnets, which are typically LANs (local area networks) which are
connected to the internet.

Systems within the same subnet can communicate directly with eachother, whearas
different subnets must communicate through a router.

Subnetworks can be used to partition different networks and limit traffic
between them.

So in human terms: when reserving computers on your local area network (via
DHCP or other means) to the addresses defined as local (`192.*` and `10.*` and
`172.16.0.0 - 172.31.255.255`) the subnets can tell routers which ones are
local to them and which require going through another router.

Switch: piece of hardware that has 8 or more ethernet ports. Knows the IP
addresses of the systems connected to it and routes packets correctly to them.

Router: "smarter" version of a switch which can be software defined.

## Protocol Stack: the protocol that computers communicate is TCP/IP

Protocol Layer                      | Comments
--------------                      | --------
Application Protocols Layer         | i.e. www, e-mail, FTP
Transmission Control Protocol       | TCP directs local packets using port numbers.
Internet Protocol Layer             | IP directs packets using an IP address
Hardware Layer                      | Converts binary packet data to network signals.

Sending a packet involves going from top to bottom on your computer, through the internet
and then from bottom to top on the other computer.

## Networking Infrastructure:
The network infrastructure is composed of:
- Client (you): you are on the network and have an ethernet/wifi card to
  perform the protocols.
- Modem: this creates a local area network and connects directly with your ISP
  over a cable. Your ISP may use intermediary routers but that is a minor detail.
- Internet Service Provider (ISP): your internet cable is plugged directly into
  some switch/computer owned by your ISP. This computer tracks your internet
  usage and directs your packet onto the internet backbone to a DSP at a
  specific IP Address.
- Routers: getting to the ISP requires going through one or more routers, which are
  devices that know the shortest/best path send something to a specific ip
  address.
- Domain Name Server (DNS): this converts the text address you are requesting
  (i.e. "www.google.com") into an IP Address. It then sends your packet over
  another router.
- Server: this is the computer/system you are interacting with. It has
  complexity of it's own.


## Server Environment
Firewalls: security tools to limit network access.
- looks at network address and ports of packets and allows, drops or rejects them according
  to pre-determined filters.
- stateful firewall: prescreens sessions. If the session passes, further communication is much faster.
- Application Layer: actually knows types of protocols and filters accordingly.

Load Balancers:
- Aims to optimize resource use, maximize throughput, minimize response time
  and avoid overhead in any specific resource.
- Distribute network trafic across multiple server nodes.
- A network request is received and it is routed to one of several nodes through some algorithm.
- Popular algorithms:
  - Round robin: do one node at a time and repeat, very simple.
  - DNS delegation: a location zone is served by a specific server cluster.
    Happens at the DNS itself (requires backbone support).
  - Weighted round robin: do one node a time, but do some nodes more.
  - Least connections: server has the least active connections (I think?)
