
const Redis = require("ioredis")

exports.create_ = function(port) {
  return function(host) {
    return function(opts) {
      return function() {
        return new Redis(port,host, opts)
      }
    }
  }
}

exports.createFromConnectionString_ = function(cstr) {
  return function() {
    return new Redis(cstr)
  }
}

exports.disconnect_ = function(client) {
  return function() {
    client.disconnect()
  }
}

exports.set_ = function(client) {
  return function(key) {
    return function(val) {
      return function(onError, onSuccess) {
        client.set(key, val, function(err, res) {
          if (err) {
            onError(err)
          } else {
            onSuccess({})
          }
        })

        return function(cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess()
        }
      }
    }
  }
}

exports.get_ = function(client) {
  return function(key) {
    return function(onError, onSuccess) {
      client.get(key, function(err, res) {
        if (err) {
          onError(err)
        } else {
          onSuccess(res)
        }
      })

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess()
      }
    }
  }
}

exports.expire_ = function(client) {
  return function(key) {
    return function(ttl) {
      return function(onError, onSuccess) {
        client.expire(key, ttl, function(err, res) {
          if (err) {
            onError(err)
          } else {
            onSuccess({})
          }
        })

        return function(cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess()
        }
      }
    }
  }
}
