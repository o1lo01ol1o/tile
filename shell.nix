(import ./default.nix).shellFor {
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}