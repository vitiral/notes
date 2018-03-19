pkgs: attrs:
  with pkgs;
  let 
    bintools = binutils.bintools;
    defaultAttrs = {
      builder = "${bash}/bin/bash";
      args = [ ./builder.sh ];
      setup = ./setup.sh;
      baseInputs = [ gnutar gzip gnumake gcc bintools coreutils gawk gnused gnugrep ];
      buildInputs = [];
      system = builtins.currentSystem;
    };
  in
  derivation (defaultAttrs // attrs)

