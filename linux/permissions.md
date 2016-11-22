
```
- 	Ordinary file

4 r 	Owner can read the file
2 w 	Owner can write to the file
1 x 	Owner can execute the file

4 r 	Group can read the file
2 w 	Group can write to the file
1 x 	Group can execute the file

4 r 	Others can read the file
2 w 	Others can write to the file
1 x 	Others can execute the file
```

Prevent editing entirely:

chattr +i /etc/resolv.conf
