# services
service management is through `systemctl`
- enable
- start
- restart

# common commands
- `chown`: change ownership. `chown $GROUP: folder/path -R` to give GROUP/USER ownership
  recursively

# groups
All user account related information are stored in the following files:

- /etc/passwd – Contains one line for each user account.
- /etc/shadow – Contains the password information in encrypted formatfor the system’s accounts and optional account aging information.
- /etc/group – Defines the groups on the system.
- /etc/default/useradd – This file contains a value for the default group, if none is specified by the useradd command.
- /etc/login.defs – This file defines the site-specific configuration for the shadow password suite stored in /etc/shadow file.

## list groups for user
```
groups [USER]
```

## Creating a user
TODO

## Add existing user to group
usermod -a -G libvirt garrett
