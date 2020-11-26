'use strict'
const path = require("path");
const fs = require("fs");
const util = require("util");
const yauzl = require("yauzl");
const Transform = require("stream").Transform;

function mkdirp(dir, cb) {
  if (dir === ".") return cb();
  fs.stat(dir, function(err) {
    if (err == null) return cb(); // already exists

    var parent = path.dirname(dir);
    mkdirp(parent, function() {
      process.stdout.write(dir.replace(/\/$/, "") + "/\n");
      fs.mkdir(dir, cb);
    });
  });
}

function unzip(zipname, destdir, callback) {
  function handleZipFile(err, zipfile) {
    if (err) throw err;

    // track when we've closed all our file handles
    var handleCount = 0;
    function incrementHandleCount() {
      handleCount++;
    }
    function decrementHandleCount() {
      handleCount--;
      if (handleCount === 0) {
        console.log("all input and output handles closed");
      }
    }

    incrementHandleCount();
    zipfile.on("close", function() {
      console.log("closed input file");
      decrementHandleCount();
    });

    zipfile.readEntry();
    zipfile.on("entry", function(entry) {
      if (/\/$/.test(entry.fileName)) {
        // directory file names end with '/'
        mkdirp(path.join(destdir, entry.fileName), function() {
          if (err) throw err;
          zipfile.readEntry();
        });
      } else {
        // ensure parent directory exists
        mkdirp(path.join(destdir, path.dirname(entry.fileName)), function() {
          zipfile.openReadStream(entry, function(err, readStream) {
            if (err) throw err;
            // report progress through large files
            var byteCount = 0;
            var totalBytes = entry.uncompressedSize;
            var lastReportedString = byteCount + "/" + totalBytes + "  0%";
            process.stdout.write(entry.fileName + "..." + lastReportedString);
            function reportString(msg) {
              var clearString = "";
              for (var i = 0; i < lastReportedString.length; i++) {
                clearString += "\b";
                if (i >= msg.length) {
                  clearString += " \b";
                }
              }
              process.stdout.write(clearString + msg);
              lastReportedString = msg;
            }
            // report progress at 60Hz
            var progressInterval = setInterval(function() {
              reportString(byteCount + "/" + totalBytes + "  " + ((byteCount / totalBytes * 100) | 0) + "%");
            }, 1000 / 60);
            var filter = new Transform();
            filter._transform = function(chunk, encoding, cb) {
              byteCount += chunk.length;
              cb(null, chunk);
            };
            filter._flush = function(cb) {
              clearInterval(progressInterval);
              reportString("");
              // delete the "..."
              process.stdout.write("\b \b\b \b\b \b\n");
              cb();
              zipfile.readEntry();
            };

            // pump file contents
            const writeStream = fs.createWriteStream(path.join(destdir, entry.fileName));
            incrementHandleCount();
            writeStream.on("close", decrementHandleCount);
            readStream.pipe(filter).pipe(writeStream);
          });
        });
      }
    });
    zipfile.once("end", callback);
  }
  yauzl.open(zipname, {lazyEntries: true}, handleZipFile);
}
module.exports = unzip