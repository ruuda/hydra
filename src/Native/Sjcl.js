// Elm mangles names based on username and module name.  So 'Native.Sjcl' in the
// repository 'ruuda/hydra' gets the ugly name below.
var _ruuda$hydra$Native_Sjcl = function() {
  return {
    // The 'sjcl' object is provided by SJCL itself in sjcl.js.
    'encrypt': sjcl.encrypt,
    'decrypt': sjcl.decrypt
  };
};
