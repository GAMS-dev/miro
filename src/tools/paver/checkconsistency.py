'''PAVER Consistency Checks.
Checks that mark failed solver runs and instances.
'''
import utils;
import numpy as np;

class CheckTerminationStatus :
    '''Checks whether the solver terminated nicely, e.g., normally or because some limit was exceeded, but not on an error.'''

    def __init__(self, badstatus = list(range(utils.TerminationStatus.CapabilityProblem, utils.TerminationStatus.Other+1)) + [np.nan]) : # pylint: disable=E1101
        self._badstatus = badstatus;

    def __call__(self, paver):
        count = 0;
        for i in paver.solvedata.items :
            df = paver.solvedata[i];
            #failed = df['TerminationStatus'].isin(self._badstatus);
            for s in self._badstatus :
                if np.isnan(s) :
                    failed = df['TerminationStatus'].map(np.isnan);
                else :
                    failed = df['TerminationStatus'] == s;
                df['Fail'][failed] = True;
                if np.isnan(s) :
                    df['FailReason'][failed] += "No data.";
                else :
                    df['FailReason'][failed] += "Termstatus is " + utils.TerminationStatusNames[s];
                count += failed.sum();

        return count;


class CheckBounds :
    '''Checks primal and dual bounds for consistency.'''
    _dirword = {1.0 : 'exceeds', -1.0 : 'falls below'};

    def __init__(self, reltol, abstol, ignoredualbounds = False) :
        assert reltol >= 0.0;
        assert abstol >= 0.0;

        self.reltol = reltol;
        self.abstol = abstol;
        self.ignoredb = ignoredualbounds;

    def __call__(self, paver) :
        count = 0;

        # compare primal bounds with known dual bounds
        if paver.hasSolveAttribute('PrimalBound') and paver.hasInstanceAttribute('KnownDualBound') :
            # fill missing bound data by -inf
            #paver.instancedata['KnownDualBound'] = paver.instancedata['KnownDualBound'].fillna(-paver.instancedata['Direction'] * np.inf);
            for i in paver.solvedata.items :
                df = paver.solvedata[i];
                # get instances with primal bounds
                dfpb = df; #.dropna(subset = ['PrimalBound']);

                # get instances with absolute error above tolerance
                abserr = paver.instancedata['Direction'] * paver.instancedata['KnownDualBound'] - paver.instancedata['Direction'] * dfpb['PrimalBound'] > self.abstol;
                # get instances with relative error above tolerance
                relerr = utils.isRelGT(paver.instancedata['Direction'] * paver.instancedata['KnownDualBound'], paver.instancedata['Direction'] * dfpb['PrimalBound'], self.reltol);

                # failed are those with absolute and relative error above tolerance
                failed = abserr & relerr;

                df['Fail'][failed] = True;
                df['FailReason'][failed] += 'Primal bound contradicts best known dual bound';

                count += failed.sum();


        # compare dual bounds with known primal bounds
        if not self.ignoredb and paver.hasSolveAttribute('DualBound') and paver.hasInstanceAttribute('KnownPrimalBound') :
            # fill missing bound data by +inf
            #paver.instancedata['KnownPrimalBound'] = paver.instancedata['KnownPrimalBound'].fillna(paver.instancedata['Direction'] * np.inf);
            for i in paver.solvedata.items :
                df = paver.solvedata[i];
                # get instances with dual bound
                dfdb = df; #.dropna(subset = ['DualBound']);

                # get instances with absolute error above tolerance
                abserr = paver.instancedata['Direction'] * dfdb['DualBound'] - paver.instancedata['Direction'] * paver.instancedata['KnownPrimalBound'] > self.abstol;
                # get instances with relative error above tolerance
                relerr = utils.isRelGT(paver.instancedata['Direction'] * dfdb['DualBound'], paver.instancedata['Direction'] * paver.instancedata['KnownPrimalBound'], self.reltol);

                # failed are those with absolute and relative error above tolerance
                failed = abserr & relerr;

                df['Fail'][failed] = True;
                df['FailReason'][failed] += 'Dual bound contradicts best known primal bound';

                count += failed.sum();

        # compare primal and dual bounds of solvers among each other
        if not self.ignoredb and paver.hasSolveAttribute('PrimalBound') and paver.hasSolveAttribute('DualBound') :
            for sr1 in paver.solvedata.items :
                df1 = paver.solvedata[sr1];
                # get instances with dual bound in solver 1
                #df1 = df1.dropna(subset = ['DualBound']).convert_objects(convert_numeric=True);
                for sr2 in paver.solvedata.items :
                    df2 = paver.solvedata[sr2];
                    # get instances with primal bound in solver 2
                    #df2 = df2.dropna(subset = ['PrimalBound']).convert_objects(convert_numeric=True);

                    abserr = paver.instancedata['Direction'] * df1['DualBound'] - paver.instancedata['Direction'] * df2['PrimalBound'] > self.abstol;
                    relerr = utils.isRelGT(paver.instancedata['Direction'] * df1['DualBound'], paver.instancedata['Direction'] * df2['PrimalBound'], self.reltol);

                    # failed are those with absolute and relative error above tolerance and where the instance is not declared as failed already
                    #print abserr, relerr, df1['Fail'], df2['Fail'];
                    failed = abserr & relerr; # & ~df1['Fail'] & ~df2['Fail'];
                    failed[df1['Fail']] = False;
                    failed[df2['Fail']] = False;

                    paver.instancedata.loc[failed,'Fail'] = True;
                    paver.instancedata.loc[failed,'FailReason'] += 'Dual bound of ' + str(sr1) + ' contradicts primal bound of ' + str(sr2);

                    count += failed.sum();

        return count;



class CheckExaminer :
    '''Checks that examiner computed infeasibilities are within tolerances.'''

    def __init__(self, primaltol, dualtol = np.inf, slacktol = np.inf) :
        assert primaltol >= 0.0;
        assert dualtol >= 0.0;
        assert slacktol >= 0.0;

        self._primaltol = primaltol;
        self._dualtol = dualtol;
        self._slacktol = slacktol;

    def __call__(self, paver) :
        count = 0;

        tol = { 'PrimalVarInfeas' : self._primaltol,
                'DualVarInfeas'   : self._dualtol,
                'PrimalConInfeas' : self._primaltol,
                'DualConInfeas'   : self._dualtol,
                'PrimalCompSlack' : self._slacktol,
                'DualCompSlack'   : self._slacktol
              }

        for i in paver.solvedata.items :
            df = paver.solvedata[i];
            for attrib in list(tol.keys()) :
                if not paver.hasSolveAttribute(attrib) :
                    continue;

                failed = (df[attrib] > tol[attrib]);

                # sort out instances without solution or which were declared unbounded
                if paver.hasSolveAttribute('PrimalBound') :
                    failed &= (abs(df['PrimalBound']) < np.inf);

                df['Fail'][failed] = True;
                df['FailReason'][failed] += attrib + ' > ' + str(tol[attrib]);

                count += failed.sum();

        return count;


class ConsistencyCheck :
    '''Methods to check consistency of solver outcomes.
    Stores a list of checks that can be executed to check a particular solver run
    or that crosscheck all solver outcomes for a particular instance.
    '''

    def __init__(self, checks = list()):
        self.checks = checks;

    def __call__(self, paver) :
        '''Marks inconsistencies in the instancedata and solvedata of a PAVER object
           @param paver PAVER object
           @return Number of inconsistencies found.
        '''
        count = 0;

        # doesn't work
        #paver.addSolveAttribute(None, None, None, 'Fail', None);

        for sr in paver.solvedata :
            for i in paver.instancedata.index :
                paver.addSolveAttribute(i, sr.split('@')[0], sr.split('@')[1], 'Fail', False);
                paver.addSolveAttribute(i, sr.split('@')[0], sr.split('@')[1], 'FailReason', "");

        paver.instancedata['Fail'] = False;
        paver.instancedata['FailReason'] = "";

        for c in self.checks :
            count += c(paver);

        return count;
