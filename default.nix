{ profiling ? false
, release ? false
, threaded ? !profiling
}:

let
  sources = import ./nix/sources.nix;
  haskell-nix = import sources."haskell.nix" {};
  nixpkgs =
    let
      inherit (haskell-nix) nixpkgsArgs;
      args = nixpkgsArgs // {
        overlays =
          (nixpkgsArgs.overlays or [])
          ++ [ (import ./nix/ghcide.nix { inherit sources; }) ]
          ++ [ (import ./nix/stylish-haskell.nix { inherit sources; }) ]
          ;
        config =
          (nixpkgsArgs.config or {})
          ;
      };
    in import haskell-nix.sources.nixpkgs-1909 args;
  pkgs = nixpkgs;
  yq = "${pkgs.lib.getBin pkgs.yq}/bin/yq";
  localPackages = [
    "kore-prelude"
    "kore-base"
    "kore-test-base"
    "kore-syntax"
    "kore"
  ];
  localModule = package: {
    packages.${package} = {
      enableLibraryProfiling = profiling;
      enableExecutableProfiling = profiling;
      profilingDetail = "toplevel-functions";

      postUnpack = ''
        substituteInPlace "$sourceRoot/package.yaml" \
          --replace "../package-common.yaml" "./package-common.yaml" \
          --replace "../package-haskell.yaml" "./package-haskell.yaml"
        cp ${./package-common.yaml} "$sourceRoot/package-common.yaml"
        cp ${./package-haskell.yaml} "$sourceRoot/package-haskell.yaml"
      '';
    };
  };
  defaultModules = [
    {
      # package *
      enableLibraryProfiling = true;
      profilingDetail = "none";

      packages.kore.flags = { inherit release threaded; };
    }
  ];
  project =
    pkgs.haskell-nix.stackProject {
      src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "kore"; };
      modules = defaultModules ++ map localModule localPackages;
    };
  shell = import ./shell.nix { inherit default; };
  default =
    {
      inherit localPackages pkgs project;
      cache = [
        pkgs.haskell-nix.haskellNixRoots
        (pkgs.haskell-nix.withInputs shell)
      ];
    };

in default
