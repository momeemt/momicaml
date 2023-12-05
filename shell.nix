{ pkgs ? import ./nix/pkgs.nix }:
let
  frameworks = pkgs.darwin.apple_sdk.frameworks;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    opam
    ocamlPackages.ocaml
    ocamlPackages.dune_2
    ocamlPackages.ocaml-lsp
    ocamlPackages.batteries
    ocamlPackages.alcotest
    frameworks.Security
    frameworks.CoreFoundation
    frameworks.CoreServices
  ];
}
