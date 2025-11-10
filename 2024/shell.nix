let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    pkg-config
    rustc
    cargo
    rust-analyzer
  ];

  buildInputs = with pkgs;[
  ];
}
