let
  default = import ./default.nix { release = true; };
  profiling = import ./default.nix { profiling = true; release = true; };
  inherit (default.pkgs) recurseIntoAttrs;
in
[
  default.cache
  (recurseIntoAttrs default.project.kore.components)
  (recurseIntoAttrs profiling.project.kore.components)
]
