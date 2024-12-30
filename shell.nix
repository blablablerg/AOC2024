with import <nixpkgs> {};

stdenv.mkDerivation {
	name = "AOC2024";
	buildInputs = [
	  (pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs;
      [ 
        haskell-language-server
        cabal-install
        hoogle
        pretty-simple
        pqueue
        memoize
        regex-tdfa
        multiset
      ]))
	];

}
