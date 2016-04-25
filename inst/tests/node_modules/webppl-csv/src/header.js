module.exports = function(env) {
  // Header code goes here

  var exampleHeaderFn = function(s, k, a, x) {
    return k(s, x + 1);
  }

  return {
    // Adjust exports here
    exampleHeaderFn: exampleHeaderFn
  }
}
