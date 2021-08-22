'''PAVER reader for GAMS trace files.'''
import os;
import numpy as np;

import utils;
from utils import TerminationStatus;

DEFAULTCOLS = ['InputFileName', 'ModelType', 'SolverName', 'NLP', 'MIP',
               'JulianDate', 'Direction', 'NumberOfEquations', 'NumberOfVariables', 'NumberOfDiscreteVariables',
               'NumberOfNonZeros', 'NumberOfNonlinearNonZeros', 'OptionFile', 'ModelStatus', 'SolverStatus',
               'ObjectiveValue', 'ObjectiveValueEstimate', 'SolverTime', 'NumberOfIterations', 'NumberOfDomainViolations',
               'NumberOfNodes', 'User1'];

# 'SolverStatus' is required if 'TerminationStatus' is not given
REQUIREDCOLS = ['InputFileName', 'SolverName', 'ObjectiveValue', 'SolverTime'];

INTCOLS = ['NumberOfEquations', 'NumberOfVariables', 'NumberOfDiscreteVariables', 'NumberOfNonZeros', 'NumberOfNonlinearNonZeros',
           'ModelStatus', 'SolverStatus', 'NumberOfIterations', 'NumberOfDomainViolations', 'NumberOfNodes','TerminationStatus'
           ];

FLOATCOLS = ['ObjectiveValue', 'ObjectiveValueEstimate', 'SolverTime', 'JulianDate', 'ETSolver',
             'PrimalVarInfeas', 'DualVarInfeas', 'PrimalConInfeas', 'DualConInfeas', 'PrimalCompSlack', 'DualCompSlack'];

INSTANCEATTRS = ['Direction', 'NumberOfEquations', 'NumberOfVariables', 'NumberOfDiscreteVariables',
                 'NumberOfNonZeros', 'NumberOfNonlinearNonZeros'];

SOLVEATTRS = ['JulianDate', 'TerminationStatus', 'PrimalBound', 'DualBound',
              'SolverTime', 'ETSolver', 'NumberOfIterations', 'NumberOfDomainViolations', 'NumberOfNodes',
              'PrimalVarInfeas', 'DualVarInfeas', 'PrimalConInfeas', 'DualConInfeas', 'PrimalCompSlack', 'DualCompSlack'];

SolutionStatus = utils.Enumeration("Unbounded GlobalOptimal LocalOptimal Feasible LocalInfeasible GlobalInfeasible NoSolution Other"); # pylint: disable = C0103

# MODEL STATUS CODE
# 1     Optimal
# 2     Locally Optimal
# 3     Unbounded
# 4     Infeasible
# 5     Locally Infeasible
# 6     Intermediate Infeasible
# 7     Intermediate Nonoptimal
# 8     Integer Solution
# 9     Intermediate Non-Integer
# 10     Integer Infeasible
# 11     Licensing Problems - No Solution
# 12     Error Unknown
# 13     Error No Solution
# 14     No Solution Returned
# 15     Solved Unique
# 16     Solved
# 17     Solved Singular
# 18     Unbounded - No Solution
# 19     Infeasible - No Solution
# pylint: disable=E1101
MODELSTAT2SOLUSTAT = {
                       1 : SolutionStatus.GlobalOptimal,
                       2 : SolutionStatus.LocalOptimal,
                       3 : SolutionStatus.Unbounded,
                       4 : SolutionStatus.GlobalInfeasible,
                       5 : SolutionStatus.LocalInfeasible,
                       6 : SolutionStatus.NoSolution,
                       7 : SolutionStatus.Feasible,
                       8 : SolutionStatus.LocalOptimal,
                       9 : SolutionStatus.NoSolution,
                       10 : SolutionStatus.GlobalInfeasible,
                       11 : SolutionStatus.NoSolution,
                       12 : SolutionStatus.NoSolution,
                       13 : SolutionStatus.NoSolution,
                       14 : SolutionStatus.NoSolution,
                       15 : SolutionStatus.GlobalOptimal,
                       16 : SolutionStatus.GlobalOptimal,
                       17 : SolutionStatus.GlobalOptimal,
                       18 : SolutionStatus.Unbounded,
                       19 : SolutionStatus.GlobalInfeasible
                       };

# SOLVER STATUS CODE
# 1     Normal Completion
# 2     Iteration Interrupt
# 3     Resource Interrupt
# 4     Terminated by Solver
# 5     Evaluation Error Limit
# 6     Capability Problems
# 7     Licensing Problems
# 8     User Interrupt
# 9     Error Setup Failure
# 10     Error Solver Failure
# 11     Error Internal Solver Error
# 12     Solve Processing Skipped
# 13     Error System Failure
SOLVERSTAT2TERMSTAT = {
                       1 : TerminationStatus.Normal,
                       2 : TerminationStatus.IterationLimit,
                       3 : TerminationStatus.TimeLimit,
                       4 : TerminationStatus.Other,
                       5 : TerminationStatus.OtherLimit,
                       6 : TerminationStatus.CapabilityProblem,
                       7 : TerminationStatus.Other,
                       8 : TerminationStatus.UserInterrupt,
                       9 : TerminationStatus.Error,
                       10 : TerminationStatus.Error,
                       11 : TerminationStatus.Error,
                       12 : TerminationStatus.Other,
                       13 : TerminationStatus.Error
                       }
# pylint: enable=E1101

if os.path.isfile('solverrename.py') :
    print('executing solver rename file solverrename.py')
    exec(compile(open('solverrename.py').read(), 'solverrename.py', 'exec'));
else :
    solverrename = {}; # pylint: disable=C0103

# infinity value in trace file seem to depend on solver (Baron 1e+50, SCIP 1e+20, ...)
GAMSSOLVERINFINITY = 1e20;

def addCommandLineOptions(parser) :
    '''Make command line parser aware of command line options of this reader.'''
    parser.add_argument('--optfileisrunname', help = 'whether to treat GAMS option file names as solver run name', action = 'store_true', default = False);


def read(f, paver, **attribs) :
    '''Stores data in form of GAMS trace file in a SolveData object.

    Currently uses runid 0 for all data.

    If a Trace Record Definition is given, assumes that it uses comma separated values.

    @param tracefile Name of GAMS trace file.
    @param data Object to store solving data.
    @param optfileisrunname Whether the OptionFile field should be takes as run name instead as part of the solver identifier.
    @param solvername Name to use for solver. Default is content of 'SolverName' field.
    @param runname Name to use for run, if not specified by option file. Default is '0'.
    '''
    cols = DEFAULTCOLS;
    intracerecorddef = False;

    optfileisrunname = False;
    runname = None;
    solvername = None;
    if 'optfileisrunname' in attribs :
        optfileisrunname = attribs['optfileisrunname'];
    if 'runname' in attribs :
        runname = attribs['runname'];
    if 'solvername' in attribs :
        solvername = attribs['solvername'];
    settingsname = None;

    linenr = 0;
    for line in f :
        linenr += 1;

        if( line[0] == '*' ) :
            # skip GamsSolve line
            if( line.find('GamsSolve') >= 0 ) :
                continue;
            if( line.find('GAMS/Examiner') >= 0 ) :
                continue;

            # starting trace record definition (discards any previously read definition)
            if( line.find('Trace Record Definition') >= 0 ) :
                cols = [];
                intracerecorddef = True;
                continue;

            if( intracerecorddef ) :
                # get rid of '*' and spaces at begin and end
                line = line[1:].strip();
                # take empty comment line as end of trace record definition
                if len(line) == 0 :
                    intracerecorddef = False;
                    continue;
                # remove ',' at begin and end, if there
                if line[0] == ',' :
                    line = line[1:];
                if line[-1] == ',' :
                    line = line[:-1];

                # append to trace record definition
                for c in line.split(',') :
                    assert(len(c.strip()) > 0);
                    cols.append(c.strip());

            elif( line.find('SETTINGS') >= 0 ) :
                # get rid of '*' and spaces at begin and end
                line = line[1:].strip();
                settingsname = line.split(',')[1];

            # skip comment lines
            continue;

        if( intracerecorddef ) :
            intracerecorddef = False;
            # check if required columns are present
            for rc in REQUIREDCOLS :
                assert rc in cols, 'Required column "' + str(rc) + '" not in trace records definition for file "' + str(f) +'".';

            #print cols;

        record = {};
        colit = iter(cols);
        for r in line.split(',') :
            r = r.strip();
            c = next(colit);
            if c in INTCOLS :
                if r == "NA" or r == "UNDF" or r == "" or r.find('acr?') >= 0 :
                    pass;
                else :
                    record[c] = int(r);
            elif c in FLOATCOLS :
                if r == "NA" or r == "UNDF" or r == "" or r.find('acr?') >= 0 :
                    pass;
                elif float(r) >= GAMSSOLVERINFINITY :
                    record[c] = np.inf;
                elif float(r) <= -GAMSSOLVERINFINITY :
                    record[c] = -np.inf;
                else :
                    record[c] = float(r);
            else :
                record[c] = r.strip();

        # if we seem to get examiner written trace files, take only the lines for the solu point
        # TODO optionally, do SOLUP_SCALED
        if 'WhatPoint' in record and record['WhatPoint'] != 'SOLUP' :
            continue;

        instanceid = record['InputFileName'];
        if solvername is None :
            solverid = record['SolverName'];
        else :
            solverid = solvername;
        if runname is not None :
            thisrunname = runname;
        else :
            thisrunname = '0';

        if 'OptionFile' in record and record['OptionFile'] != 'NA' and record['OptionFile'] != '' :
            if settingsname is not None :
                record['OptionFile'] = settingsname;
            if optfileisrunname :
                thisrunname = record['OptionFile'];
            else :
                solverid += '.' + record['OptionFile'];

        if solverid in solverrename :
            solverid = solverrename[solverid];

        if paver.hasSolveAttributes(instanceid, solverid, thisrunname) :
            raise BaseException(str(f) + ':' + str(linenr) + ': Already have data for run ' + thisrunname + ' of solver ' + solverid + ' on instance ' + instanceid + '.');

        if ('Direction' in record) and (record['Direction'] != "NA") :
            # map trace record direction (0 = min, 1 = max) to SolveData direction (1 = min, -1 = max)
            record['Direction'] = 1 - 2 * int(record['Direction']);
        else :
            # assume minimization, if not specified
            record['Direction'] = 1;

        if 'ModelStatus' in record :
            record['SolutionStatus'] = MODELSTAT2SOLUSTAT[int(record['ModelStatus'])];

        if 'SolverStatus' in record :
            record['TerminationStatus'] = SOLVERSTAT2TERMSTAT[int(record['SolverStatus'])];
        elif not 'TerminationStatus' in record :
            raise BaseException(str(f) + ':' + str(linenr) + ': Missing termination status for run ' + thisrunname + ' of solver ' + solverid + ' on instance ' + instanceid + '.\n\t' + line);

        if 'ObjectiveValue' in record :
            record['PrimalBound'] = record['ObjectiveValue'];
        else :
            record['PrimalBound'] = record['Direction'] * np.inf;
        if 'ObjectiveValueEstimate' in record :
            record['DualBound'] = record['ObjectiveValueEstimate'];
        else :
            record['DualBound'] = -record['Direction'] * np.inf;

        #overwrite primal / dual bounds with info from solution status
        if 'SolutionStatus' in record :
            # pylint: disable=E1101
            if record['SolutionStatus'] == SolutionStatus.GlobalInfeasible :
                record['PrimalBound'] =  record['Direction'] * np.inf;
                record['DualBound'] =  record['Direction'] * np.inf;
            elif record['SolutionStatus'] == SolutionStatus.Unbounded :
                record['PrimalBound'] = -record['Direction'] * np.inf;
                record['DualBound'] = -record['Direction'] * np.inf;
            elif record['SolutionStatus'] == SolutionStatus.GlobalOptimal :
                record['DualBound'] = record['PrimalBound'];
            elif record['SolutionStatus'] == SolutionStatus.NoSolution :
                record['PrimalBound'] = record['Direction'] * np.inf;
            elif record['SolutionStatus'] == SolutionStatus.LocalInfeasible :
                if 'PrimalBound' in record :
                    del record['PrimalBound'];
            # pylint: enable=E1101

        for c in INSTANCEATTRS :
            if c in record :
                paver.addInstanceAttribute(instanceid, c, record[c]);

        for c in SOLVEATTRS :
            if c in record :
                paver.addSolveAttribute(instanceid, solverid, thisrunname, c, record[c]);


    f.close();
