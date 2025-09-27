{
  description = "A development environment for the Maelstorm OCaml project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [];
        };
        ocamlPkgs = pkgs.ocaml-ng.ocamlPackages;
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with ocamlPkgs; [
            ocaml
            dune_3
            ocamlformat
            ocaml-lsp
            alcotest
          ] ++ (with pkgs; [
            git
          ]);
        };
      });
}
