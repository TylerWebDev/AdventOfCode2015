// setup
Elm.Native = Elm.Native || {};
Elm.Native.Native = Elm.Native.Native || {};

// definition
Elm.Native.Native.make = function(localRuntime) {
	'use strict';

  // attempt to short-circuit
  if ('values' in Elm.Native.Native)
  {
    return Elm.Native.Native.values;
  }

  function md5(string) {
    var hash = window.md5(string);
    return hash;
  }

  return Elm.Native.Native.values = {
    md5: md5
  };
}
