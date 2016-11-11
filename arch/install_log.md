- 2016-01-26
    - couldn't update via pacman because of key issues. Had to install ONLY archlinux-keyring
    - when I did update, failed because 
    ```
        error: failed to commit transaction (conflicting files)
        dcadec: /usr/lib/libdcadec.so.0 exists in filesystem
    ```
    - manually moved these to ~/software/backups/libdcadec
    - it looks like one package got moved from libdcadec.so.0.0.0 to 0.0.1, no changes otherwise
    
- 2015-9(?):
    - sleep stopped working in linux 4.2.2 or 4.2.3. Had to downgrade to
        linux-4.1.6-1-x86_64.pkg.tar.xz
- 2015-12-24:
    - wireless driver wasn't working. Had to downgrade wpa_supplicant:
        `sudo pacman -U wpa_supplicant-1:2.3-1-x86_64.pkg.tar.xz`
    - confirmed that linux-4.2.5 still doesn't solve the sleep issue
    - confirmed that wpa_supplicant doesn't work with new linux firmware
        or kernel either

