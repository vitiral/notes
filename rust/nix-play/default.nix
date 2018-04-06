with import <nixpkgs> {};
((import ./nix-play.nix).nix_play {}).override {
  crateOverrides = defaultCrateOverrides // {
    hello = attrs: {
        buildInputs = [
        # Some example external dependencies
        openssl xorg.libX11 xorg.libXtst
        # For `nix-shell`
        rustc cargo openssl pkgconfig
      ];
    };
  };
}

