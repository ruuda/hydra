// Elm mangles names based on username and module name. So 'Native.Sjcl' in the
// repository 'ruuda/hydra' gets the ugly name below.
var _ruuda$hydra$Native_Sjcl = {
  'encrypt': {
    'arity': 2,
    // The 'sjcl' object is provided by SJCL itself in sjcl.js.
    'func': sjcl.encrypt
  },
  // For reasons unknown to me, 'encrypt' must be a wrapper object as above,
  // but 'decrypt' is called directly, and it must be a function.
  'decrypt': sjcl.decrypt
};
