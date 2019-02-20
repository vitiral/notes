lddfiles() {
  ldd $1 | egrep -o '/lib.*\.[0-9]'
}

# cp the dependent libraries of $1 to $2
cp_ldd() {
    mkdir -pv $2
    files=$(lddfiles $1)
    for f in $files; do
        cp -v $f ${2}${f}
    done
}
