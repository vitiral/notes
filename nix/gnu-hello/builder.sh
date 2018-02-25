set -e
unset PATH
for p in $baseInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done
for p in $buildInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done
echo "PATH=$PATH"

tar -xf $src

for d in *; do
  if [ -d "$d" ]; then
    cd "$d"
    break
  fi
done

./configure --prefix=$out
make
make install
