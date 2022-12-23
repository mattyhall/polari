{
  description = "A functional programming language built in Zig";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    zig = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    zls = {
      url = "github:erikarvstedt/zls/fix-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {self, nixpkgs, flake-utils, flake-compat, zig, zls, gitignore}:
    flake-utils.lib.eachSystem (builtins.attrNames zig.packages) (system:
      let
        pkgs = import nixpkgs { inherit system; };
        zigLatest = zig.packages.${system}.master-2022-12-21;
        inherit (gitignore.lib) gitignoreSource;
      in
        rec {
          packages = rec {
            polaric = pkgs.stdenvNoCC.mkDerivation {
              name = "polaric";
              version = "master";
              src = gitignoreSource ./.;
              nativeBuildInputs = [ zigLatest ];
              dontConfigure = true;
              dontInstall = true;
              buildPhase = ''
                mkdir -p $out
                zig build install -Drelease-safe=true --prefix $out
              '';
              XDG_CACHE_HOME = ".cache";
            };
            default = polaric;
          };

          checks = {
            format = pkgs.runCommand "check-format" { nativeBuildInputs = [ zigLatest ]; } ''
              zig fmt --check ${./.}
              touch $out
            '';
            test = pkgs.runCommand "test" { nativeBuildInputs = [ zigLatest ]; } ''
              cd ${./.}
              XDG_CACHE_HOME=$out/.cache
              CACHE_DIR=$out/zig-cache
              OUT_DIR=$out/zig-out
              mkdir -p $XDG_CACHE_HOME $CACHE_DIR $OUT_DIR
              XDG_CACHE_HOME=$XDG_CACHE_HOME zig build test --prefix $OUT_DIR --cache-dir $CACHE_DIR
            '';
          };

          devShells.default = pkgs.mkShell {
            buildInputs = (with pkgs; [
              zigLatest
              zls.packages.${system}.default
              bashInteractive
              gdb
            ]);
          };
        }
    );
}
