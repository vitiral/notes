> this is a log of maunual changes that sometimes crop up in order to
> get libraries working

- to get elm working need to link lib
https://github.com/elm-lang/elm-platform/issues/113
```
$ cd /usr/lib
$ sudo ln -s libncurses++w.so.6.0 libtinfo.so.5
```
