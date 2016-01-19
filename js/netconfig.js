netconfig = [{"Bond10G": {

    "#default": false,
    "address": "10.10.5.100",
    "auto": true,
    "bond-downdelay": "200",
    "bond-fail_over_mac": "None",
    "bond-lacp_rate": "Fast",
    "bond-miimon": "100",
    "bond-mode": "LACP",
    "bond-primary_reselect": "Failure",
    "bond-slaves": "eth0 eth1",
    "bond-updelay": "200",
    "bond-xmit_hash_policy": "Layer3_4",
    "dns-nameservers": "172.24.254.1",
    "dns-search": "eng.solidfire.net",
    "family": "inet",
    "gateway": "10.10.63.254",
    "macAddress": "d4:ae:52:74:b5:80",
    "macAddressPermanent": "00:00:00:00:00:00",
    "method": "static",
    "mtu": "9000",
    "netmask": "255.255.192.0",
    "network": "10.10.0.0",
    "physical":

{

    "address": "10.10.5.100",
    "macAddress": "d4:ae:52:74:b5:80",
    "macAddressPermanent": "00:00:00:00:00:00",
    "mtu": "9000",
    "netmask": "255.255.192.0",
    "network": "10.10.0.0",
    "upAndRunning": true

},
"routes": [ ],
"status": "UpAndRunning",
"symmetricRouteRules":

    [
        "ip route add 10.10.0.0/18 dev Bond10G src 10.10.5.100 table Bond10G",
        "ip route add default via 10.10.63.254 dev Bond10G src 10.10.5.100 table Bond10G",
        "ip rule add from 10.10.5.100 table Bond10G"
    ],
    "upAndRunning": true

},
"Bond1G":
{

    "#default": true,
    "address": "192.168.133.100",
    "auto": true,
    "bond-downdelay": "200",
    "bond-fail_over_mac": "Active",
    "bond-miimon": "100",
    "bond-mode": "ActivePassive",
    "bond-primary_reselect": "Failure",
    "bond-slaves": "eth2 eth3",
    "bond-updelay": "200",
    "dns-nameservers": "172.24.254.1",
    "dns-search": "eng.solidfire.net",
    "family": "inet",
    "gateway": "192.168.159.254",
    "macAddress": "d4:ae:52:74:b5:84",
    "macAddressPermanent": "00:00:00:00:00:00",
    "method": "static",
    "mtu": "1500",
    "netmask": "255.255.224.0",
    "network": "192.168.128.0",
    "physical":

{

    "address": "192.168.133.100",
    "macAddress": "d4:ae:52:74:b5:84",
    "macAddressPermanent": "00:00:00:00:00:00",
    "mtu": "1500",
    "netmask": "255.255.224.0",
    "network": "192.168.128.0",
    "upAndRunning": true

},
"routes": [ ],
"status": "UpAndRunning",
"symmetricRouteRules":

    [
        "ip route add 192.168.128.0/19 dev Bond1G src 192.168.133.100 table Bond1G",
        "ip route add default via 192.168.159.254 dev Bond1G src 192.168.133.100 table Bond1G",
        "ip rule add from 192.168.133.100 table Bond1G",
        "ip route add default via 192.168.159.254"
    ],
    "upAndRunning": true

},
"eth0":
{

    "auto": true,
    "bond-master": "Bond10G",
    "family": "inet",
    "macAddress": "d4:ae:52:74:b5:80",
    "macAddressPermanent": "d4:ae:52:74:b5:80",
    "method": "bond",
    "physical":

    {
        "address": "0.0.0.0",
        "macAddress": "d4:ae:52:74:b5:80",
        "macAddressPermanent": "d4:ae:52:74:b5:80",
        "netmask": "N/A",
        "network": "N/A",
        "upAndRunning": true
    },
    "status": "UpAndRunning",
    "upAndRunning": true

},
"eth1":
{

    "auto": true,
    "bond-master": "Bond10G",
    "family": "inet",
    "macAddress": "d4:ae:52:74:b5:80",
    "macAddressPermanent": "d4:ae:52:74:b5:82",
    "method": "bond",
    "physical":

    {
        "address": "0.0.0.0",
        "macAddress": "d4:ae:52:74:b5:80",
        "macAddressPermanent": "d4:ae:52:74:b5:82",
        "netmask": "N/A",
        "network": "N/A",
        "upAndRunning": true
    },
    "status": "UpAndRunning",
    "upAndRunning": true

},
"eth2":
{

    "auto": true,
    "bond-master": "Bond1G",
    "family": "inet",
    "macAddress": "d4:ae:52:74:b5:84",
    "macAddressPermanent": "d4:ae:52:74:b5:84",
    "method": "bond",
    "physical":

    {
        "address": "0.0.0.0",
        "macAddress": "d4:ae:52:74:b5:84",
        "macAddressPermanent": "d4:ae:52:74:b5:84",
        "netmask": "N/A",
        "network": "N/A",
        "upAndRunning": true
    },
    "status": "UpAndRunning",
    "upAndRunning": true

},
"eth3":
{

    "auto": true,
    "bond-master": "Bond1G",
    "family": "inet",
    "macAddress": "d4:ae:52:74:b5:86",
    "macAddressPermanent": "d4:ae:52:74:b5:86",
    "method": "bond",
    "physical":

    {
        "address": "0.0.0.0",
        "macAddress": "d4:ae:52:74:b5:86",
        "macAddressPermanent": "d4:ae:52:74:b5:86",
        "netmask": "N/A",
        "network": "N/A",
        "upAndRunning": true
    },
    "status": "UpAndRunning",
    "upAndRunning": true

},
"lo":
{

    "auto": true,
    "family": "inet",
    "macAddress": "00:00:00:00:00:00",
    "macAddressPermanent": "00:00:00:00:00:00",
    "method": "loopback",
    "physical":

        {
            "address": "0.0.0.0",
            "macAddress": "00:00:00:00:00:00",
            "macAddressPermanent": "00:00:00:00:00:00",
            "netmask": "N/A",
            "network": "N/A",
            "upAndRunning": true
        },
        "status": "UpAndRunning",
        "upAndRunning": true
    }

}]

macs = function (){
    index = 0
    mac_addresses = []
    while(index < netconfig.length){
        mac = netconfig[index]['Bond10G']['macAddress']
        mac_addresses.push(mac)
        index += 1
    }
    return mac_addresses
}()

print(macs)
