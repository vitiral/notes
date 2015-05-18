
# Wireless Settings

Install wireless tools

```
pacman -S wirless_tools
```

This gives you access to `iwconfig` and `iwlist`

Equivalent calls:
- ifconfig == ip addr
- ifdown == iplink set eth0 down
- ifup == iplink set eth0 up
- iw dev wlan0 link == give information about connection (signal strength, etc)

find networks and connect to them by installing `dialog` and typing `wifi-menu <interface name>`

- make sure ethernet is disabled before this is done


Enabled simple dhcp
```
systemctl enable dhcpcd@interface_name.service
```


# Filesystem
- Install `ntfs-3g` to write to ntfs filesystems


# Desktop
- install startx stuff
- install plasma
