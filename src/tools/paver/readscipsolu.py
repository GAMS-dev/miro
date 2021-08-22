'''PAVER reader for SCIP solution files.'''

import numpy as np;

KEYS = ["best", "opt", "feas", "inf", "unkn", "bestdual", "unbnd"];

def addCommandLineOptions(parser) :
    '''Make command line parser aware of command line options of this reader.'''
    # pylint: disable=W0613
    pass;


def read(f, paver, **attribs) :
    '''Reads input in form of a SCIP .solu file with best known solution status and primal and dual bounds for a set of instances.

    Skips data for instances that have no instance record yet, so the solu file should be read after the solver outcomes have been read.
    '''
    # pylint: disable=W0613

    for line in f :

        line = line.strip();
        if (len(line) == 0) or (line[0] == '#') :
            continue;

        r = line.split();
        assert(len(r) > 0);  # otherwise we should have continued before

        key = r[0].strip('=');

        if key not in KEYS :
            print('ERROR: Key ' + key + ' at position ' + f.name + ':' + f.tell() + ' unknown. Ignoring line.');
            continue;

        if len(r) == 1 :
            print('ERROR: No instance name given at position ' + f.name + ':' + f.tell() + '. Ignoring line.');
            continue;

        i = r[1];

        if key in ["best", "opt", "bestdual"] :
            if len(r) == 2 :
                print('ERROR: No value given at position ' + f.name + ':' + f.tell() + '. Ignoring line.');
                continue;
            # TODO how to check whether r[2] is indeed a float ?
            val = float(r[2]);
        else :
            val = None;

        # skip solution data for instances without solve data
        if not paver.hasInstance(i) :
            continue;

        d = paver.getInstanceAttribute(i, 'Direction');

        # TODO we could also update previously set bounds, if better
        if key == "best" :
            assert val is not None;
            paver.addInstanceAttribute(i, 'KnownPrimalBound', val);
        elif key == "opt" :
            assert val is not None;
            paver.addInstanceAttribute(i, 'KnownPrimalBound', val);
            paver.addInstanceAttribute(i, 'KnownDualBound', val);
        elif key == "feas" :
            # TODO currently not supported
            pass;
        elif key == "inf" :
            paver.addInstanceAttribute(i, 'KnownPrimalBound', d * np.inf);
            paver.addInstanceAttribute(i, 'KnownDualBound',   d * np.inf);
        elif key == "unkn" :
            pass;
        elif key == "bestdual" :
            assert val is not None;
            paver.addInstanceAttribute(i, 'KnownDualBound', val);
        elif key == "unbnd" :
            paver.addInstanceAttribute(i, 'KnownPrimalBound', -d * np.inf);
            paver.addInstanceAttribute(i, 'KnownDualBound',   -d * np.inf);

    f.close();
