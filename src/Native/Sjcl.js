// Elm mangles names based on username and module name. So 'Native.Sjcl' in the
// repository 'ruuda/hydra' gets the ugly name below.
var _ruuda$hydra$Native_Sjcl = {
  // Functions in Elm are curried, so curry the SJCL functions here.
  'encrypt': function (key) {
    return function (plaintext) {
      // The 'sjcl' object is provided by SJCL itself in sjcl.js.
      return sjcl.encrypt(key, plaintext);
    };
  },
  'decrypt': function (key) {
    return function (ciphertext) {
      return sjcl.decrypt(key, ciphertext);
    };
  }
};
