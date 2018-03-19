rec {
  makeOverridable = f: origArgs:
    let
      origRes = f origArgs;
    in
    origRes // {
      result = origRes;
      override = newArgs: makeOverridable f (origArgs // newArgs);
    };
}
