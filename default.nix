let
  sources = import nix/sources.nix;
  haskellNix = import sources.haskellNix {};

  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  pkgs = import nixpkgsSrc nixpkgsArgs;

  sourceByRegex =
    src: regexes: builtins.filterSource (path: type:
      let relPath = pkgs.lib.removePrefix (toString src + "/") (toString path); in
      let match = builtins.match (pkgs.lib.strings.concatStringsSep "|" regexes); in
      ( type == "directory"  && match (relPath + "/") != null
      || match relPath != null)) src;
in pkgs.haskell-nix.cabalProject {
  src = sourceByRegex ./. [
      "cabal.project"
      "app/"
      "app/.*.hs"
      ".*.cabal"
      "LICENSE"
    ];
  compiler-nix-name = "ghc901";
}
