
# Domain Creation
Get the `--os-variant`
```
osinfo-query --fields=name,short-id,version os
```

## Initial setup 
- download windows iso from:
    https://www.microsoft.com/en-us/evalcenter/evaluate-windows-10-enterprise
- enable the default pool: 
    - `virsh pool-autostart default`
    - `virsh pool-start default`
- create the volume: 
    - `virsh vol-create-as default windows 30GiB --format vmdk`
- enable the default network: 
    - `virsh net-autostart default`
    - `virsh net-start default`

## create arch-linux domain
```
virt-install  \
  --name arch-linux_testing \
  --memory 1024             \
  --vcpus=2,maxvcpus=4      \
  --cpu host                \
  --cdrom $HOME/Downloads/archlinux-2017.08.01-x86_64.iso \
  --disk size=2,format=qcow2  \
  --network user            \
  --virt-type kvm           \                       
  --connect qemu:///session
```

## create gentoo domain

```
virt-install  \
  --name gentoo-linux 		\
  --memory 2048             \ 
  --vcpus=1,maxvcpus=2      \
  --cpu host                \
  --cdrom install-amd64-minimal-20170817.iso \
  --disk size=5,format=qcow2  \
  --network user            \
  --virt-type kvm           \
  --connect qemu:///system
```

## create windows domain
Works!
```
virt-install \
--name windows10 \
--cdrom /tmp/Win10_1703_N_English_x64.iso \
--memory 2048 \
--disk vol=default/windows \
--os-type windows \
--vcpus 2 \
--cpu host \
--virt-type kvm \
--network network=default \
--connect qemu:///system
```

```
virt-install \                 
    --name=windows10
    --memory=2098 \
    --cpu=host
    --vcpus=2 \
    --os-type=windows \
    --os-variant=win8.1 \
    --cdrom $HOME/Downloads/Windows/Win10_1703_N_English_x64.iso \
    --disk $HOME/.local/libvirt/disk/vms-win10,bus=virtio \
    --cdrom $HOME/Downloads/Windows/virtio-win-0.1.126.iso \
    --network bridge=virbr0 \
    --graphics vnc,listen=0.0.0.
```

# manage existing
Start a domain:
```
$ virsh start $DOMAIN
$ virt-viewer $DOMAIN --connect=qemu:///system
```

Create a snapshot
```
virsh snapshot-create-as --domain $DOMAIN \
--name "$SNAP_NAME" \
--description "some description" \
--live 
```

To revert a domain to a snapshot, enter:
- `virsh shutdown --domain freebsd`
- `virsh snapshot-revert --domain $DOMAIN --snapshotname $SNAP_NAME --running`

Delete a snapshot
- `virsh snapshot-delete --domain $DOMAIN --snapshotname $SNAP_NAME`

Stop a domain
```
virsh shutdown $DOMAIN
```

Completely stop a domain
```
virsh destroy $DOMAIN
```

Delete/undefine a domain
```
virsh undefine $DOMAIN
```

