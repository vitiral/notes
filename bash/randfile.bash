# get the size of the file we want to create
export oversize=$[ ( $(df -h /var/log | awk '{print $2}' | sed 1d | sed 's/G//') + 2) * 1024];

# export oversize=$[10 * 1024];

# pass="$(dd if=/dev/urandom bs=128 count=1 2>/dev/null | base64)"
# echo "pass: $pass"
# pass=${pass//$'\n'/}
# echo "pass: $pass"
# echo "dd if=<(openssl enc -aes-256-ctr -pass pass:$pass -nosalt < /dev/zero) bs=1M count=$oversize iflag=fullblock | tee bogusly_big_file > /dev/null"
# dd if=<(openssl enc -aes-256-ctr -pass pass:$pass -nosalt < /dev/zero) bs=1M count=$oversize iflag=fullblock | tee bogusly_big_file > /dev/null



export oversize=$[10 * 1024]; export pass="$(dd if=/dev/urandom bs=128 count=1 2>/dev/null | base64)"; export pass=${pass//$'\n'/}; dd if=<(openssl enc -aes-256-ctr -pass pass:$pass -nosalt < /dev/zero) bs=1M count=$oversize iflag=fullblock | tee fill_log_file > /dev/null

# export oversize=$[ ( $(df -h /var/log | awk '{print $2}' | sed 1d | sed 's/G//') + 2) * 1024]; export pass="$(dd if=/dev/urandom bs=128 count=1 2>/dev/null | base64)"; export pass=${pass//$'\n'/}; dd if=<(openssl enc -aes-256-ctr -pass pass:$pass -nosalt < /dev/zero) bs=1M count=$oversize iflag=fullblock | tee /bogusly_big_file > /dev/null
