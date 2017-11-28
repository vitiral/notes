with import <nixpkgs> {};
rec {
  mkRustCrate = callPackage ./rust-nix { };

  hello = (mkRustCrate {
    crateName = "hello";
    version = "0.1.0";
    src = ./.;
  }) pkgs.rustc;
}
