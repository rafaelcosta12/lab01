{
  description = "Lab01 - Automatos e Expressoes Regulares";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskell.compiler.ghc967
          cabal-install
          libyaml
        ];
        shellHook = ''
          echo "Ambiente Haskell (GHC 9.6.7) pronto!"
        '';
      };
    };
}