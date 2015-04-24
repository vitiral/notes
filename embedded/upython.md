# git integration
To develop a library ON micropython but still have git integration:

This is how I develop my [microtb](https://github.com/cloudformdesign/microtb)
library

- create a folder on your pyboard where you want to develop
```bash
cd /media/user/PYBFLASH     # cd into pyboard flash
mkdir microtb               # make the python library
touch microtb/__init__.py   # make it a python module
```

- create a folder where you want to store the git data
```bash
cd ~/projects
mkdir microtb       # make the git folder
cd microtb
git init
mkdir microtb       # make the actual python library that will be linked
```

- mount (not link) the folder in PYBFLASH into your
    git folder
```
sudo mount --bind /media/user/PYBFLASH/microtb microtb  # mount it
ls microtb                              # should show "__init__.py"
touch microtb/led.py                                    # create a new file
ls /media/user/PYBFLASH/microtb         # should show "__init.py led.py"
```

- add the files to git as you would normally do
```
git add --all microtb                   # add files to git
git status                              # should show all files added
git commit -am "initial commit"
```

- You can make the changes permanant using fstab, or you can create a script
    that mounts it when you need it mounted. Since I don't always have my
    pyboard connected, I went with the second one
```
nano mount.sh
```
Copy/Paste the following

```
folder="microtb"
mount='true'

while getopts 'u' flag; do
    case "${flag}" in
        u) mount='false' ;;
        *) error "expected option -u or none" ;;
    esac
done

if $mount -eq "true"; then
    echo "mounting: ${folder}"
    sudo mount --bind /media/$USER/PYBFLASH/${folder} ${folder}
else
    echo "unmounting: ${folder}"
    sudo umount ${folder}
fi
```
Exit with Cntrl+X then y + Enter and type the following
```
chmod a+x mount.sh
```

You can now mount with `./mount.sh` and unmount with `./mount.sh -u`

Change "folder" to whatever your library is called and it should be plug and play!
