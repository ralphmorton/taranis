
const path = require('path')

exports.fastify_ = function(port) {
  return function(apiPrefix) {
    return function(staticDir) {
      return function(handler) {
        return function(onError, onSuccess) {
          const fastify = require('fastify')({
            logger: true
          })

          if (staticDir) {
            fastify.register(require('fastify-static'), {
              root: path.join(__dirname, staticDir)
            })
          }

          fastify.route({
            method: ['GET', 'POST', 'DELETE', 'PUT', 'PATCH', 'HEAD', 'OPTIONS'],
            url: '/' + apiPrefix + '/*',
            handler: function (request, reply) {
              handler(request)(reply)()
            }
          })

          fastify.listen(port, '0.0.0.0', (err, address) => {
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
}

exports.requestMethod_ = function(req) {
  return function() {
    return req.method
  }
}

exports.requestPath_ = function(req) {
  return function() {
    return req.url.split('?')[0].split('/').filter(function(s) { return s.length > 0 }).map(function(p) { return decodeURI(p) })
  }
}

exports.requestQueryParams_ = function(req) {
  return function() {
    var res = []

    const keys = Object.keys(req.query)
    for (i = 0; i < keys.length; i++) {
      res.push({ key: keys[i], value: req.query[keys[i]] })
    }

    return res
  }
}

exports.requestHeaders_ = function(req) {
  return function() {
    var res = []
    const keys = Object.keys(req.headers)

    for (i = 0; i < keys.length; i++) {
      res.push({ key: keys[i], value: req.headers[keys[i]] })
    }

    return res
  }
}

exports.requestBody_ = function(req) {
  return function() {
    return req.body
  }
}

exports.writeResponse_ = function(rsp) {
  return function(info) {
    return function() {
      rsp.status(info.statusCode)
      rsp.send(info.body)
    }
  }
}
