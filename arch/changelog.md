

> this is a log of maunual changes that sometimes crop up in order to
> get libraries working

- to get elm working need to link lib
https://github.com/elm-lang/elm-platform/issues/113
```
$ cd /usr/lib
$ sudo ln -s libncurses++w.so.6.0 libtinfo.so.5
```

- 20160-11-22 created and edited /etc/udev/rules.d/10-network.rules to change MTU
    https://wiki.archlinux.org/index.php/Network_configuration

