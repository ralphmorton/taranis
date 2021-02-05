
const fs = require("fs")
const path = require('path')

exports.resolve_ = function(p) {
  return function() {
    return path.normalize(path.resolve(p))
  }
}

exports.exists_ = function(p) {
  return function() {
    return fs.existsSync(p)
  }
}

exports.listFilesRecursive_ = function(p) {
  return function() {
    const getAllFiles = function(dirPath, prefix, arrayOfFiles) {
      files = fs.readdirSync(dirPath)
    
      arrayOfFiles = arrayOfFiles || []
    
      files.forEach(function(file) {
        if (fs.statSync(dirPath + "/" + file).isDirectory()) {
          pref = prefix ? (prefix + "/" + file) : file
          arrayOfFiles = getAllFiles(dirPath + "/" + file, pref, arrayOfFiles)
        } else {
          arrayOfFiles.push(prefix ? path.join(prefix, "/", file) : file)
        }
      })
    
      return arrayOfFiles
    }

    return getAllFiles(p, null, [])
  }
}
