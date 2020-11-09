
- restart netctl (on laptop)
    systemctl restart netctl-auto@wlps0.service

# changing to eth0
get mac address:
 - cat /sys/class/net/enp11s0/address
set udev rules manually:
 - vim /etc/udev/rules.d/10-network.rules
 - SUBSYSTEM=="net", ACTION=="add", ATTR{address}=="40:16:7e:79:57:72", NAME="eth0"
