# ssh

ssh user@ip

`-A` transfers credentials

# scp
used for transfering files

scp from to
- one should be a file path
- the other should be a ssh login (user@ip)

## logging in automatically
scp ~/.ssh/id_rsa.pub user@ip:~/.ssh/authorized_keys
