- 2015-9(?):
    - sleep stopped working in linux 4.2.2 or 4.2.3. Had to downgrade to
        linux-4.1.6-1-x86_64.pkg.tar.xz
- 2015-12-24:
    - wireless driver wasn't working. Had to downgrade wpa_supplicant:
        `sudo pacman -U wpa_supplicant-1:2.3-1-x86_64.pkg.tar.xz`
    - confirmed that linux-4.2.5 still doesn't solve the sleep issue
    - confirmed that wpa_supplicant doesn't work with new linux firmware
        or kernel either
- 2016-02-05: upgrading
    Package (11)             Old Version         New Version         Net Change  Download Size
	core/linux               4.1.6-1             4.4.1-2               1.08 MiB      57.38 MiB
	core/linux-firmware      20150904.6ebf5d5-1  20160113.40e9ae8-1   16.15 MiB      34.08 MiB
	core/wpa_supplicant      1:2.3-1             1:2.5-2               0.18 MiB       0.68 MiB

