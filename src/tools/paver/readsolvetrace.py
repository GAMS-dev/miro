'''PAVER reader for solvetrace files.'''

import os;
import pandas as pd;

DEFAULTCOLS = ['lineNum', 'seriesID', 'node', 'seconds', 'bestFound', 'bestBound' ];

COLRENAME = { 'node' : 'Node',
               'seconds' : 'Time',
               'bestFound' : 'PrimalBound',
               'bestBound' : 'DualBound'
             }

INTCOLS = ['lineNum', 'Node'];
FLOATCOLS = ['Time', 'PrimalBound', 'DualBound'];

if os.path.isfile('solverrename.py') :
    print('executing solver rename file solverrename.py')
    exec(compile(open('solverrename.py').read(), 'solverrename.py', 'exec'));
else :
    solverrename = {}; # pylint: disable=C0103

def addCommandLineOptions(parser) :
    '''Make command line parser aware of command line options of this reader.'''
    # pylint: disable=W0613
    pass;

def read(f, paver, **attribs) :
    '''Reads input in form of a GAMS solvetrace (aka miptrace) file with progress of primal and dual bounds w.r.t. time and number of nodes.'''
    # pylint: disable=W0613

    solver = None;
    runname = None;
    instance = None;
    cols = DEFAULTCOLS;
    tracedata = [];
    seenstart = False;
    seenend = False;

    for line in f :

        if line.startswith("* solvetrace file") or line.startswith("* miptrace file") :
            pos = line.find("ID = ");
            if pos >= 0 :
                solverid = line[pos+5:].split()[0].upper();

                if solverid in solverrename :
                    solverid = solverrename[solverid];

                # see if we find a solver name which matches our solverid (case insensitive) or at least starts with the same name
                for sr in paver.solvedata.items :
                    s = sr.split('@')[0];

                    if s.upper() == solverid :
                        solver = s;
                        runname = sr.split('@')[1];
                        break;
                    elif s.upper().startswith(solverid) or solverid.startswith(s.upper()) :
                        solver = s;
                        runname = sr.split('@')[1];

                if solver is None :
                    raise BaseException('Do not have solver with name ' + solverid);

            pos = line.find("Instance = ");
            if pos >= 0 :
                instance = line[pos+11:].split()[0];

            continue;

        if line.startswith("* fields are") :
            # get rid of '* fields are' and spaces at begin and end
            line = line[12:].strip();
            # remove ',' at end, if there
            if line[-1] == ',' :
                line = line[:-1];

            # collect solvetrace record definition
            cols = [];
            for c in line.split(',') :
                assert(len(c.strip()) > 0);
                cols.append(c.strip());

            continue;

        # skip comment and empty lines
        if line[0] == '*' or len(line.strip()) == 0 :
            continue;

        record = {};
        colit = iter(cols);
        for r in line.split(',') :
            r = r.strip();
            c = next(colit);

            if r.upper() == "NA" :
                continue;

            if c in COLRENAME :
                c = COLRENAME[c];

            if c in INTCOLS :
                record[c] = int(r);
            elif c in FLOATCOLS :
                record[c] = float(r);
            else :
                record[c] = r.strip();

        if 'seriesID' in cols:
            seenstart |= (record['seriesID'] == 'S');
            seenend |= (record['seriesID'] == 'E');

        tracedata.append(record);

    if instance is None :
        raise BaseException("No information on instancedata in solvetrace file");

    # TODO we could autocomplete with primal / dual bound and solvetime from data
    if 'seriesID' in cols and (not seenstart or not seenend) :
        print('Incomplete solvetrace, ignoring.')
        return

    if 'lineNum' in cols :
        df = pd.DataFrame.from_records(tracedata, index = 'lineNum');
    else :
        df = pd.DataFrame(tracedata);

    paver.addSolveTrace(instance, solver, runname, df);
