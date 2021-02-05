
const base64url = require('base64url')

exports.encode_ = function(input) {
  return function() {
    return base64url.encode(input)
  }
}

exports.decode_ = function(input) {
  return function() {
    return base64url.decode(input)
  }
}
