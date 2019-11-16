watrun() {
    OUT="out"
    mkdir -p $OUT
    wat2wasm $1.wat -o $OUT/$1.wasm || return 1
    wasm-interp $OUT/$1.wasm \
      --run-all-exports -vvv --trace \
      --call-stack-size 1024 \
      --value-stack-size 100000 \
}
