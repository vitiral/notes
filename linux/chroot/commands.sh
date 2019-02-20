source ./lib.sh
J=$PWD/jail
mkdir -p $J/{bin,dev,proc,lib64,lib/x86_64-linux-gnu}

mount --bind /dev/ /home/chroot/dev/
mount --bind /proc/ /home/chroot/proc/

cp /bin/{bash,ls} $J/bin

cp_ldd /bin/bash $J
cp_ldd /bin/ls $J
sudo chroot --userspec=rett $PWD/jail bin/bash
