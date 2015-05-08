BASE="git clone git@github.com:cloudformdesign/"

repos=(notes cloudtb tinymem micropython tmq pymakec pastebin)

for i in "${repos[@]}"
do
    echo ${BASE}${i}
    `${BASE}${i}`
done
