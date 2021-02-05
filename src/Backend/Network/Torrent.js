
var WebTorrent = require('webtorrent')

exports.create_ = function() {
  return new WebTorrent()
}

exports.download_ = function(client) {
  return function(opts) {
    return function(uri) {
      return function(onError, onSuccess) {
        var torrent = client.get(uri) || client.add(uri, opts)
        torrent.originalMagnetUri = uri

        torrent.on('error', function(err) {
          onError(err)
        })

        torrent.on('done', function() {
          onSuccess(torrent)
        })

        return function(cancelError, onCancelerError, onCancelerSuccess) {
          client.remove(uri, { destroyStore: true })
          onCancelerSuccess()
        }
      }
    }
  }
}

exports.torrents_ = function(client) {
  return function() {
    return client.torrents
  }
}

exports.get_ = function(client) {
  return function(uri) {
    return function() {
      return client.get(uri)
    }
  }
}

exports.remove_ = function(client) {
  return function(opts) {
    return function(uri) {
      return function() {
        let torrent = client.torrents.filter(function(torrent) {
          let torrentUri = torrent.originalMagnetUri || torrent.magnetURI
          return torrentUri == uri
         })

        client.remove(torrent.originalMagnetUri || uri, opts)
      }
    }
  }
}

exports.progress_ = function(torrent) {
  return function() {
    return torrent.progress
  }
}

exports.magnetUri_ = function(torrent) {
  return function() {
    return torrent.originalMagnetUri || torrent.magnetURI
  }
}
