{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = { self, fenix, flake-utils, naersk, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
        };
        target = "aarch64-unknown-linux-gnu";

        toolchain = with fenix.packages.${system}; toolchainOf {
          channel = "nightly";
          date = "2022-01-08";
          sha256 = "sha256-8brI/cS6bOV6QswJIwjqbIc4/MM79jOfQY5kAda8Mo8=";
        };
          dev-toolchain = toolchain.withComponents [
            "cargo" "rustc" "rust-src" "rustfmt" "clippy"
          ];

      in rec {
        # For `nix build` & `nix run`:
        defaultPackage = (naersk.lib.${system}.override {
          cargo = toolchain;
          rustc = toolchain;
        }).buildPackage {
          src = ./.;
          CARGO_BUILD_TARGET = target;
          CARGO_TARGET_AARCH64_UNKNOWN_LINUX_GNU_LINKER =
            "${pkgs.pkgsCross.aarch64-multiplatform.stdenv.cc}/bin/${target}-gcc";
        };

        # For `nix develop` (optional, can be skipped):
        devShell = pkgs.mkShell {
          nativeBuildInputs = [
            dev-toolchain
            toolchain.rust-analyzer
          ];
          #RUST_SRC_PATH = "${toolchain.rust-src}/lib/rustlib/src/rust/library";
        };
      }
    );
}
