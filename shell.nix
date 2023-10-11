{ pkgs ? import ./nix/pkgs.nix }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    dune_2
    ocaml
  ];
}
