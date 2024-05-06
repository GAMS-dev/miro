const path = require('node:path');
const fs = require('node:fs');
const yauzl = require('yauzl');
const { Transform } = require('node:stream');

function mkdirp(dir, cb) {
  if (dir === '.') return cb();
  fs.stat(dir, (err) => {
    if (err == null) return cb(); // already exists

    const parent = path.dirname(dir);
    mkdirp(parent, () => {
      process.stdout.write(`${dir.replace(/\/$/, '')}/\n`);
      fs.mkdir(dir, cb);
    });
    return true;
  });
  return true;
}

function unzip(zipname, destdir, callback) {
  function handleZipFile(err, zipfile) {
    if (err) throw err;

    // track when we've closed all our file handles
    let handleCount = 0;
    function incrementHandleCount() {
      handleCount += 1;
    }
    function decrementHandleCount() {
      handleCount -= 1;
      if (handleCount === 0) {
        process.stdout.write('all input and output handles closed');
      }
    }

    incrementHandleCount();
    zipfile.on('close', () => {
      process.stdout.write('closed input file');
      decrementHandleCount();
    });

    zipfile.readEntry();
    zipfile.on('entry', (entry) => {
      if (/\/$/.test(entry.fileName)) {
        // directory file names end with '/'
        mkdirp(path.join(destdir, entry.fileName), (e) => {
          if (e) throw e;
          zipfile.readEntry();
        });
      } else {
        // ensure parent directory exists
        mkdirp(path.join(destdir, path.dirname(entry.fileName)), () => {
          zipfile.openReadStream(entry, (e, readStream) => {
            if (e) throw e;
            // report progress through large files
            let byteCount = 0;
            const totalBytes = entry.uncompressedSize;
            let lastReportedString = `${byteCount}/${totalBytes}  0%`;
            process.stdout.write(`${entry.fileName}...${lastReportedString}`);
            function reportString(msg) {
              let clearString = '';
              for (let i = 0; i < lastReportedString.length; i += 1) {
                clearString += '\b';
                if (i >= msg.length) {
                  clearString += ' \b';
                }
              }
              process.stdout.write(clearString + msg);
              lastReportedString = msg;
            }
            // report progress at 60Hz
            const progressInterval = setInterval(() => {
              reportString(`${byteCount}/${totalBytes}  ${((byteCount / totalBytes) * 100) || 0}%`);
            }, 1000 / 60);
            const filter = new Transform();
            // eslint-disable-next-line no-underscore-dangle
            filter._transform = (chunk, encoding, cb) => {
              byteCount += chunk.length;
              cb(null, chunk);
            };
            // eslint-disable-next-line no-underscore-dangle
            filter._flush = (cb) => {
              clearInterval(progressInterval);
              reportString('');
              // delete the "..."
              process.stdout.write('\b \b\b \b\b \b\n');
              cb();
              zipfile.readEntry();
            };

            // pump file contents
            const writeStream = fs.createWriteStream(path.join(destdir, entry.fileName));
            incrementHandleCount();
            writeStream.on('close', decrementHandleCount);
            readStream.pipe(filter).pipe(writeStream);
          });
        });
      }
    });
    zipfile.once('end', callback);
  }
  yauzl.open(zipname, { lazyEntries: true }, handleZipFile);
}
module.exports = unzip;
