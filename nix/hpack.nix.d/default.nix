{
  pkgs = hackage:
    {
      packages = {
        "http-client".revision = (((hackage."http-client")."0.7.5").revisions).default;
        "http-client".flags.network-uri = true;
        "cookie".revision = (((hackage."cookie")."0.4.5").revisions).default;
        "tf-random".revision = (((hackage."tf-random")."0.5").revisions).default;
        "cereal".revision = (((hackage."cereal")."0.5.8.1").revisions).default;
        "cereal".flags.bytestring-builder = false;
        "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.8.0").revisions).default;
        "interpolate".revision = (((hackage."interpolate")."0.2.1").revisions).default;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.6.1").revisions).default;
        "text-metrics".revision = (((hackage."text-metrics")."0.3.0").revisions).default;
        "text-metrics".flags.dev = false;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.10").revisions).default;
        "bifunctors".flags.semigroups = true;
        "bifunctors".flags.tagged = true;
        "x509-validation".revision = (((hackage."x509-validation")."1.6.11").revisions).default;
        "split".revision = (((hackage."split")."0.2.3.4").revisions).default;
        "data-fix".revision = (((hackage."data-fix")."0.3.1").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "logging-facade".revision = (((hackage."logging-facade")."0.3.0").revisions).default;
        "base-compat-batteries".revision = (((hackage."base-compat-batteries")."0.11.2").revisions).default;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.4.1").revisions).default;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.5").revisions).default;
        "zlib".revision = (((hackage."zlib")."0.6.2.2").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.bundled-c-zlib = false;
        "zlib".flags.pkg-config = false;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "th-expand-syns".revision = (((hackage."th-expand-syns")."0.4.6.0").revisions).default;
        "cryptonite".revision = (((hackage."cryptonite")."0.28").revisions).default;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.use_target_attributes = true;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.support_aesni = true;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.check_alignment = false;
        "cryptonite".flags.old_toolchain_inliner = false;
        "clock".revision = (((hackage."clock")."0.8.2").revisions).default;
        "clock".flags.llvm = false;
        "th-orphans".revision = (((hackage."th-orphans")."0.13.11").revisions).default;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "syb".revision = (((hackage."syb")."0.7.2.1").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.2.1").revisions).default;
        "distributive".flags.semigroups = true;
        "distributive".flags.tagged = true;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.6").revisions).default;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.14.2").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "QuickCheck".flags.old-random = false;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "hspec-discover".revision = (((hackage."hspec-discover")."2.7.8").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "haskell-src-meta".revision = (((hackage."haskell-src-meta")."0.8.5").revisions).default;
        "random".revision = (((hackage."random")."1.2.0").revisions).default;
        "temporary".revision = (((hackage."temporary")."1.3").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.3").revisions).default;
        "network".revision = (((hackage."network")."3.1.2.1").revisions).default;
        "network".flags.devel = false;
        "connection".revision = (((hackage."connection")."0.3.1").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.1.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "dlist".revision = (((hackage."dlist")."1.0").revisions).default;
        "dlist".flags.werror = false;
        "conduit".revision = (((hackage."conduit")."1.3.4").revisions).default;
        "x509-store".revision = (((hackage."x509-store")."1.6.7").revisions).default;
        "HUnit".revision = (((hackage."HUnit")."1.6.2.0").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.14.0").revisions).default;
        "th-reify-many".revision = (((hackage."th-reify-many")."0.1.9").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.7").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.6.0").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.5.0").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.6").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "template-haskell".revision = (((hackage."template-haskell")."2.16.0.0").revisions).default;
        "hspec-expectations".revision = (((hackage."hspec-expectations")."0.8.2").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.15.1").revisions).default;
        "vector".revision = (((hackage."vector")."0.12.2.0").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "call-stack".revision = (((hackage."call-stack")."0.3.0").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.1.0").revisions).default;
        "safe".revision = (((hackage."safe")."0.3.19").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.2.1").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.11.2").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.5").revisions).default;
        "time-compat".flags.old-locale = false;
        "mockery".revision = (((hackage."mockery")."0.3.5").revisions).default;
        "x509-system".revision = (((hackage."x509-system")."1.6.6").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.11").revisions).default;
        "ansi-terminal".flags.example = false;
        "tagged".revision = (((hackage."tagged")."0.8.6.1").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "x509".revision = (((hackage."x509")."1.7.5").revisions).default;
        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.23.1").revisions).default;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.2.0.1").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.2.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3.1").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "these".revision = (((hackage."these")."1.1.1.1").revisions).default;
        "these".flags.assoc = true;
        "socks".revision = (((hackage."socks")."0.6.1").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.2.1").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "bytestring".revision = (((hackage."bytestring")."0.10.12.0").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.11").revisions).default;
        "setenv".revision = (((hackage."setenv")."0.1.1.3").revisions).default;
        "mime-types".revision = (((hackage."mime-types")."0.1.0.9").revisions).default;
        "http-client-tls".revision = (((hackage."http-client-tls")."0.3.5.3").revisions).default;
        "indexed-traversable".revision = (((hackage."indexed-traversable")."0.1.1").revisions).default;
        "text".revision = (((hackage."text")."1.2.4.1").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."3.2.1.0").revisions).default;
        "assoc".revision = (((hackage."assoc")."1.0.2").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.13.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.2.0.1").revisions).default;
        "base".revision = (((hackage."base")."4.14.1.0").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.8").revisions).default;
        "comonad".flags.indexed-traversable = true;
        "comonad".flags.distributive = true;
        "comonad".flags.containers = true;
        "hspec".revision = (((hackage."hspec")."2.7.8").revisions).default;
        "time".revision = (((hackage."time")."1.9.3").revisions).default;
        "th-compat".revision = (((hackage."th-compat")."0.1.1").revisions).default;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.4").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.properties = true;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "quickcheck-io".revision = (((hackage."quickcheck-io")."0.2.0").revisions).default;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.4").revisions).default;
        "attoparsec".flags.developer = false;
        "infer-license".revision = (((hackage."infer-license")."0.2.0").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "happy".revision = (((hackage."happy")."1.20.0").revisions).default;
        "strict".revision = (((hackage."strict")."0.4.0.1").revisions).default;
        "strict".flags.assoc = true;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.4").revisions).default;
        "hspec-core".revision = (((hackage."hspec-core")."2.7.8").revisions).default;
        "process".revision = (((hackage."process")."1.6.9.0").revisions).default;
        "tls".revision = (((hackage."tls")."1.5.5").revisions).default;
        "tls".flags.compat = true;
        "tls".flags.network = true;
        "tls".flags.hans = false;
        "th-lift".revision = (((hackage."th-lift")."0.8.2").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.2").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "resourcet".revision = (((hackage."resourcet")."1.2.4.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "Glob".revision = (((hackage."Glob")."0.10.1").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.5.5.1").revisions).default;
        "aeson".flags.cffi = false;
        "aeson".flags.fast = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.developer = false;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.10.4").revisions).default;
        "th-lift-instances".revision = (((hackage."th-lift-instances")."0.1.18").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.4").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.4.2.0").revisions).default;
        "memory".revision = (((hackage."memory")."0.15.0").revisions).default;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_basement = true;
        "memory".flags.support_foundation = true;
        "memory".flags.support_deepseq = true;
        "array".revision = (((hackage."array")."0.5.4.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.3.0").revisions).default;
        };
      compiler = {
        version = "8.10.4";
        nix-name = "ghc8104";
        packages = {
          "exceptions" = "0.10.4";
          "binary" = "0.8.8.0";
          "ghc-prim" = "0.6.1";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.14.0";
          "directory" = "1.3.6.0";
          "template-haskell" = "2.16.0.0";
          "containers" = "0.6.2.1";
          "bytestring" = "0.10.12.0";
          "text" = "1.2.4.1";
          "Cabal" = "3.2.1.0";
          "base" = "4.14.1.0";
          "time" = "1.9.3";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.9.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.10.4";
          "array" = "0.5.4.0";
          "integer-gmp" = "1.0.3.0";
          };
        };
      };
  extras = hackage:
    { packages = { hpack = ./.plan.nix/hpack.nix; }; };
  modules = [ ({ lib, ... }: { packages = { "hpack" = { flags = {}; }; }; }) ];
  }