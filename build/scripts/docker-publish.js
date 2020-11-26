const path  = require('path');
const execa = require('execa');
const fs = require('fs');

const clArgs = process.argv.slice(2);

let unstableImages = false;
if ( clArgs[0] === '--unstable' ) {
    unstableImages = true;
}

(async () => {
    if ( process.platform != 'linux' ) {
        console.error('Please publish images on Linux!');
        process.exit(1);
    }
    let miroVersion = 'unstable';

    if ( !unstableImages ) {
        miroVersion = fs.readFileSync('version', 'utf8').trim();
        try {
            const subproc =  execa('docker', [ 'tag', 'gamsmiro-ui', 'hub.gams.com/gamsmiro-ui' ]);
            subproc.stderr.pipe(process.stderr);
            subproc.stdout.pipe(process.stderr);
            await subproc;
            const subprocAdmin =  execa('docker', [ 'tag', 'gamsmiro-admin', 'hub.gams.com/gamsmiro-admin' ]);
            subprocAdmin.stderr.pipe(process.stderr);
            subprocAdmin.stdout.pipe(process.stderr);
            await subprocAdmin;
        } catch (e) {
            console.log(`Problems tagging docker image. Error message: ${e.message}`);
            process.exit(1);
        }
    }
    
    try {
        const subproc =  execa('docker', [ 'tag', 'gamsmiro-ui', `hub.gams.com/gamsmiro-ui:${miroVersion}` ]);
        subproc.stderr.pipe(process.stderr);
        subproc.stdout.pipe(process.stderr);
        await subproc;
        const subprocAdmin =  execa('docker', [ 'tag', 'gamsmiro-admin', `hub.gams.com/gamsmiro-admin:${miroVersion}` ]);
        subprocAdmin.stderr.pipe(process.stderr);
        subprocAdmin.stdout.pipe(process.stderr);
        await subprocAdmin;
    } catch (e) {
        console.log(`Problems tagging docker image. Error message: ${e.message}`);
        process.exit(1);
    }

    if ( !unstableImages ) {
        try {
            const subproc =  execa('docker', [ 'push', 'hub.gams.com/gamsmiro-ui' ]);
            subproc.stderr.pipe(process.stderr);
            subproc.stdout.pipe(process.stderr);
            await subproc;
            const subprocAdmin =  execa('docker', [ 'push', 'hub.gams.com/gamsmiro-admin' ]);
            subprocAdmin.stderr.pipe(process.stderr);
            subprocAdmin.stdout.pipe(process.stderr);
            await subprocAdmin;
        } catch (e) {
            console.log(`Problems pushing docker image. Error message: ${e.message}`);
            process.exit(1);
        }
    }
    
    try {
        const subproc =  execa('docker', [ 'push', `hub.gams.com/gamsmiro-ui:${miroVersion}` ]);
        subproc.stderr.pipe(process.stderr);
        subproc.stdout.pipe(process.stderr);
        await subproc;
        const subprocAdmin =  execa('docker', [ 'push', `hub.gams.com/gamsmiro-admin:${miroVersion}` ]);
        subprocAdmin.stderr.pipe(process.stderr);
        subprocAdmin.stdout.pipe(process.stderr);
        await subprocAdmin;
    } catch (e) {
        console.log(`Problems pushing docker image. Error message: ${e.message}`);
        process.exit(1);
    }
})();