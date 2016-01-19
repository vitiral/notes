- 2015-9(?):
    - sleep stopped working in linux 4.2.2 or 4.2.3. Had to downgrade to
        linux-4.1.6-1-x86_64.pkg.tar.xz
- 2015-12-24:
    - wireless driver wasn't working. Had to downgrade wpa_supplicant:
        `sudo pacman -U wpa_supplicant-1:2.3-1-x86_64.pkg.tar.xz`
    - confirmed that linux-4.2.5 still doesn't solve the sleep issue
    - confirmed that wpa_supplicant doesn't work with new linux firmware
        or kernel either

