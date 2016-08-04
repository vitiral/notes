

import subprocess

ip_fmt = '172.17.{}.{}'

for n in range(0, 256):
    ip_low = ip_fmt.format(n, 1)
    ip_high = ip_fmt.format(n, 255)
    cmd = 'sudo nbtscan {}-{}'.format(ip_low, ip_high)
    subprocess.check_call(cmd, shell=True)
