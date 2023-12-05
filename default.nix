{ pkgs ? import ./nix/pkgs.nix }:
pkgs.ocamlPackages.buildDunePackage {
  pname = "momicaml";
  version = "0.1.0";
  useDune2 = true; 
  src = ./.;
  nativeBuildInputs = with pkgs; [
    git
  ];
  checkInputs = with pkgs; [
    ocamlPackages.alcotest
    ocamlPackages.ppx_let
  ];
  buildInputs = with pkgs; [
    ocamlPackages.ocaml-syntax-shims
  ];
  doCheck = true;
  checkPhase = ''
    dune runtest
  '';
}
