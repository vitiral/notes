# cleanup home folder
cd ~
rm .bash*
mkdir projects software

# update and upgrade
sudo apt-get update
sudo apt-get upgrade

# dev tools
sudo apt-get install git
sudo apt-get install zsh tmux vim
sudo apt-get install build-essential

# install base python packages
sudo apt-get install python-dev python3-dev
sudo apt-get install python-pip
sudo apt-get install python3-pip

# Install microcode. Check microcode with `dmesg | grep microcode`
sudo apt-get install microcode.ctl intel-microcode

# miscilanious
sudo apt-get install flashplugin-installer ubuntu-restricted-extras
sudo apt-get install libdvdcss
sudo apt-get install libreoffice vlc gimp pithos bleachbit
sudo apt-get install apvlv  # vim like pdf viewer
sudo apt-get install gdrive samba
sudo apt-get install unace unrar zip unzip p7zip-full p7zip-rar sharutils rar \
    uudeview mpack arj cabextract file-roller
sudo apt-get install openjdk-7-jdk

## note: still need to manually disable desktop integration prompts
sudo apt-get install unity-tweak-tool

# disable search feature (amazon can't spy on me)
gsettings set com.canonical.Unity.Lenses remote-content-search none 

# check this out
# http://www.unixmen.com/autmatically-disable-touchpad-typing-ubuntu/
# http://www.unixmen.com/how-to-improve-laptop-battery-life-and-usage-in-linux-using-tlp/
