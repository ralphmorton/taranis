
const { BlobSASPermissions, BlobServiceClient } = require('@azure/storage-blob')

exports.createServiceClient_ = function(connStr) {
  return function() {
    return BlobServiceClient.fromConnectionString(connStr)
  }
}

exports.createContainerClient_ = function(serviceClient) {
  return function(name) {
    return function() {
      return serviceClient.getContainerClient(name);
    }
  }
}

exports.createBlobClient_ = function(containerClient) {
  return function(name) {
    return function() {
      return containerClient.getBlockBlobClient(name)
    }
  }
}

exports.upload_ = function(blobClient) {
  return function(data) {
    return function(onError, onSuccess) {
      blobClient.upload(data, data.length).then(
        function(success) {
          onSuccess({})
        },
        function(err) {
          onError(err)
        }
      )

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess()
      }
    }
  }
}

exports.uploadFile_ = function(blobClient) {
  return function(path) {
    return function(onError, onSuccess) {
      blobClient.uploadFile(path).then(
        function(success) {
          onSuccess({})
        },
        function(err) {
          onError(err)
        }
      )

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess()
      }
    }
  }
}

exports.listBlobs_ = function(containerClient) {
  return function(opts) {
    return function(onError, onSuccess) {
      let iter = containerClient.listBlobsFlat(opts)
      let results = []

      function read() {
        iter.next().then(
          function(item) {
            if (!item.done) {
              results.push({
                name: item.value.name
              })

              read()
            } else {
              onSuccess(results)
            }
          },
          function(err) {
            onError(err)
          }
        )
      }

      read()

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess()
      }
    }
  }
}

exports.generateBlobSasUrl_ = function(blobClient) {
  return function(opts) {
    return function(onError, onSuccess) {
      let expiresOn = new Date(new Date().getTime() + (opts.ttlSeconds * 1000))

      blobClient.generateSasUrl({
        expiresOn: expiresOn,
        permissions: BlobSASPermissions.from({ read: true })
      }).then(
        function(url) {
          onSuccess(url)
        },
        function(err) {
          onError(err)
        }
      )

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess()
      }
    }
  }
}

exports.setTags_ = function(blobClient) {
  return function(tags) {
    return function(onError, onSuccess) {
      let tagsObj = {}
      tags.forEach(function(tag) {
        tagsObj[tag.name] = tag.value
      })

      blobClient.setTags(tagsObj, {}).then(
        function(res) {
          onSuccess({})
        },
        function(err) {
          onError(err)
        }
      )

      return function(cancelError, onCancelerError, onCancelerSuccess) {
        onCancelerSuccess()
      }
    }
  }
}

exports.getTags_ = function(blobClient) {
  return function(onError, onSuccess) {
    blobClient.getTags({}).then(
      function(res) {
        let tags = []

        Object.keys(res.tags).forEach(function(key) {
          tags.push({
            name: key,
            value: res.tags[key]
          })
        })

        onSuccess(tags)
      },
      function(err) {
        onError(err)
      }
    )

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess()
    }
  }
}

exports.deleteIfExists_ = function(blobClient) {
  return function(onError, onSuccess) {
    blobClient.deleteIfExists({}).then(
      function(res) {
        onSuccess({})
      },
      function(err) {
        onError(err)
      }
    )

    return function(cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess()
    }
  }
}
