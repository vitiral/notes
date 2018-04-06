# Latest Nightly
with import <nixpkgs> {};

let src = fetchFromGitHub {
      owner = "mozilla";
      repo = "nixpkgs-mozilla";
      # commit from: 2018-03-27
      rev = "2945b0b6b2fd19e7d23bac695afd65e320efcebe";
      sha256 = "034m1dryrzh2lmjvk3c0krgip652dql46w5yfwpvh7gavd3iypyw";
   };
in
with import "${src.out}/rust-overlay.nix" pkgs pkgs;

stdenv.mkDerivation {
  name = "rust-env";
  buildInputs = [ 
    latest.rustChannels.stable.rust

    # Add some dependencies
    pkgconfig openssl
  ];

  RUST_BACKTRACE = 1;
}
