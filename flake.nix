{
  inputs.opam-nix.url = "github:tweag/opam-nix";
  inputs.nixpkgs.follows = "opam-nix/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, opam-nix, flake-utils }:
    {
      overlay = final: prev: {
        ocamlclose = prev.ocamlclose.overrideAttrs (oa: {
          src = builtins.filterSource (path: type:
            baseNameOf path != "package-defs.json" && baseNameOf path
            != "flake.nix" && baseNameOf path != "flake.lock") ./.;
          nativeBuildInputs = oa.nativeBuildInputs
            ++ [ final.nixpkgs.makeWrapper ];
          postFixup = ''
            wrapProgram $out/bin/ocamlclose --prefix PATH : $(dirname $(command -v ocaml)):${
              nixpkgs.lib.makeBinPath [ final.dune final.nixpkgs.procps ]
            }
          '';
        });
      };
    } // flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        # To regenerate package-defs.json:
        # nix develop -c opam-nix-regen package-defs.json
        scope = opam-nix.lib.${system}.materializedDefsToScope { }
          ./package-defs.json;
        ocamlPackages = scope.overrideScope' self.overlay;
      in {
        legacyPackages = ocamlPackages;

        defaultPackage = self.legacyPackages.${system}.ocamlclose;
        devShell = pkgs.mkShell {
          buildInputs = with opam-nix.packages.${system}; [
            opam-nix-gen
            opam-nix-regen
          ];
          inputsFrom = [ self.defaultPackage.${system} ];
        };
      });
}
