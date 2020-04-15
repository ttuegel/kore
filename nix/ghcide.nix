{ sources }:

_: pkgs:

let
  mkPackages = { ghc, stackYaml }:
    pkgs.haskell-nix.stackProject {
        src = sources.ghcide;
        inherit stackYaml;
        modules = [({config, ...}: {
          ghc.package = ghc;
          compiler.version = pkgs.lib.mkForce ghc.version;
          reinstallableLibGhc = true;
          packages.ghc.flags.ghci = pkgs.lib.mkForce true;
          packages.ghci.flags.ghci = pkgs.lib.mkForce true;
          # This fixes a performance issue, probably https://gitlab.haskell.org/ghc/ghc/issues/15524
          packages.ghcide.configureFlags = [ "--enable-executable-dynamic" ];
          packages.haskell-lsp.doHaddock = false;
        })];
      };
  mkHieCore = args@{...}:
    let packages = mkPackages args;
    in packages.ghcide.components.exes.ghcide // { inherit packages; };
  ghc = pkgs.haskell-nix.compiler.ghc865;
in

{
  ghcide = mkHieCore {
    # Compiler should be the same as LTS Haskell.
    inherit ghc;
    stackYaml = "stack.yaml";
  };
  hie-bios = (mkPackages { inherit ghc; stackYaml = "stack.yaml"; }).hie-bios.components.exes.hie-bios;
}
