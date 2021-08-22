#!/usr/bin/env python

'''Main PAVER class and run driver.'''

import os;
import shutil;
import argparse;
import sys;
from datetime import datetime;
import matplotlib;
import numpy as np;
import pandas as pd;

matplotlib.use('svg');
matplotlib.use('agg');

import HTML;
import utils;
import readgamstrace;
import readscipsolu;
import readsolvetrace;
import checkconsistency;
import solvestat;
import writesolvedata;

DEFAULTZEROGAPTOL = 1e-9;
DEFAULTWRITETEXT = None;
DEFAULTWRITEHTML = 'results';
DEFAULTFIGFORMAT = 'png';
DEFAULTWRITEIMG = None;
DEFAULTGMSWEBITER = 1;

def _defaultaggrfunc(attribute, rundata) :
    '''Returns aggregated value for of several solver runs on the same instance for a particular solve attribute.'''

    # try to convert to numerics (bool, int, float, ...)
    if attribute != 'FailReason' :
        rundata = rundata.convert_objects(convert_numeric = True);

    if attribute == 'TerminationStatus' :
        # give preference to the worst one (error over other over capability over limits over normal)
        status = rundata.max();

        # if there are different limits hit, report "otherlimit" as status
        # pylint: disable=E1101
        if status in range(utils.TerminationStatus.TimeLimit+1, utils.TerminationStatus.IterationLimit+1) \
        and rundata[rundata.between(utils.TerminationStatus.TimeLimit, utils.TerminationStatus.IterationLimit)].nunique() > 1 :
            status = utils.TerminationStatus.OtherLimit;

        return status;

    if rundata.dtype == np.bool :
        # for a series of Booleans, we currently return the maximum ('or'), which is good if attribute is Fail
        return rundata.max();

    if np.issubdtype(rundata.dtype, float) or np.issubdtype(rundata.dtype, int) :
        # for numeric values, return the mean value
        return rundata.mean();

    if isinstance(rundata[0], str) :
        result = '';
        for (_, r), d in rundata.items() :
            if d != '' :
                if len(result) > 0 :
                    result += ' ';
                result += 'Run ' + str(r) + ': ' + d;
        return result;

    print('Warning: Unknown data type for attribute ' + attribute + '. Using NaN in aggregation of solver runs.');
    return np.nan;

class Paver :
    '''Class to drive paver scripts and hold data.

    The instancedata is a Pandas DataFrame with
    - index: instance names
    - column: instance attributes (#variables, best known bounds, ...)

    The solvedata is a Pandas Panel with
    - items: the solver@run pair as string
    - major_axis: instance names
    - minor_axis: solve attributes (solve time, #nodes, best bounds, ...)
    '''

    htmlheader = '''
        <STYLE>
        table.dataframe { text-align: right; font-size: 12px; border: 1px; border-collapse: collapse}
        @media (prefers-color-scheme: dark) {table.dataframe {border-color:#444a54;}
        </STYLE>
        ''';

    def __init__(self, paversetup, options) :
        self.instancedata = pd.DataFrame();
        self.solvedata = pd.Panel();
        self.solvetraces = {};
        self.aggrsolvedata = None;
        self.aggregated = None;
        self.options = options;

        self._setup = paversetup;
        self._consistencycheck = None;
        self._solvestat = None;
        self._solvedatawriter = None;

    def hasInstance(self, instance) :
        '''Indicates whether a given instance name is known to PAVER.'''
        assert (instance in self.instancedata.index) == (instance in self.solvedata.major_axis);
        return instance in self.instancedata.index;

    def hasInstanceAttribute(self, attrname) :
        '''Indicates whether a given instance attribute is known to PAVER.'''
        return attrname in self.instancedata.columns;

    def getInstanceAttribute(self, instance, attrname) :
        '''Returns the value of an instance attribute.'''
        assert instance in self.instancedata.index;
        assert attrname in self.instancedata.columns;
        return self.instancedata[attrname][instance];

    def addInstance(self, instance) :
        '''Make an instance known to PAVER.'''

        if instance not in self.instancedata.index :
            # add instance to index of instance data
            self.instancedata = self.instancedata.append(pd.DataFrame({}, index = [instance]));

        if instance not in self.solvedata.major_axis :
            # transpose to get instances on items axis
            x = self.solvedata.transpose(1, 0, 2);
            # add a new data frame for this instance
            x[instance] = pd.DataFrame(index = self.solvedata.items, columns = self.solvedata.minor_axis);
            # transpose back
            self.solvedata = x.transpose(1, 0, 2);

    def addSolverRun(self, solver, run) :
        '''Make a solver run known to PAVER.'''

        if solver+'@'+run not in self.solvedata :
            # add a new data frame for this solver
            #midx = pd.MultiIndex.from_tuples([(solver, run)], names = ['Solver', 'Run']);
            #print midx;
            #self.solvedata[midx[0]] = pd.DataFrame(index = self.solvedata.major_axis, columns = self.solvedata.minor_axis);
            self.solvedata[solver+'@'+run] = pd.DataFrame(index = self.solvedata.major_axis, columns = self.solvedata.minor_axis);

    def addInstanceAttribute(self, instance, attrname, attrvalue = None) :
        '''Adds an attribute for an instance.
        If instance not known to PAVER yet, it will be made known.
        If attrvalue is None, only the attribute will be made known to PAVER.
        '''

        # make sure instance is known to PAVER
        self.addInstance(instance);

        # add attribute to instance attributes
        if attrname not in self.instancedata :
            # add attribute as column
            self.instancedata[attrname] = np.nan;

        if attrvalue is not None :
            if np.isnan(self.instancedata[attrname][instance]) :
                # replace NaN by attrvalue
                self.instancedata[attrname][instance] = attrvalue;
            elif not self.options['nocheckinstanceattr'] :
                # already have an entry, assert that we try to set to the same value
                assert self.instancedata[attrname][instance] == attrvalue;

    def hasSolveAttribute(self, attrname) :
        '''Indicates whether a given solve attribute is known to PAVER.'''
        return attrname in self.solvedata.minor_axis;

    def addSolveAttribute(self, instance, solver, run, attrname, attrvalue):
        '''Adds an attribute for a solver run.
        If (solver,run) not known to PAVER yet, it will be made known.
        If instance not known to PAVER yet, it will be made known.
        If attribute not known to PAVER yet, it willl be made known.
        If attrvalue is None, only the attribute will be made known to PAVER. In this case, also solver, run, and instance can be None.
        '''

        #print 'adding', solver, run, instance, attrname, attrvalue;

        if solver is not None and run is not None :
            self.addSolverRun(solver, run);
        if instance is not None :
            self.addInstance(instance);

        if attrname not in self.solvedata.minor_axis :
            # transpose to get attribute names on items axis
            x = self.solvedata.transpose(2, 1, 0);
            # add a new data frame for this attribute
            x[attrname] = pd.DataFrame(index = self.solvedata.items, columns = self.solvedata.major_axis);
            # transpose back
            self.solvedata = x.transpose(2, 1, 0);

        # assign value
        if attrvalue is not None :
            assert solver is not None;
            assert run is not None;
            assert instance is not None;
            if np.isnan(self.solvedata[solver+'@'+run][attrname][instance]) :
                # replace NaN by attrvalue
                self.solvedata[solver+'@'+run][attrname][instance] = attrvalue;
            else :
                # already have an entry, assert that we try to set to the same value
                assert self.solvedata[solver+'@'+run][attrname][instance] == attrvalue;

    def hasSolveAttributes(self, instance, solver, run) :
        '''Indicates whether some solve attribute is already set for a particular instance and solver run.'''

        if solver+'@'+run not in self.solvedata :
            return False;
        if not self.hasInstance(instance) :
            return False;

        return self.solvedata[solver+'@'+run].loc[instance].count() > 0;

    def addSolveTrace(self, instance, solver, run, solvetrace):
        '''Stores a solve trace for a solver run on an instance.
        @param instance The name of the instance.
        @param solver The name of the solver.
        @param run The name of the solver run.
        @param solvetrace The solvetrace as Pandas DataFrame.
        '''

        if (instance, solver, run) in self.solvetraces :
            raise BaseException("Already have solve trace for run (" + solver + "," + run + ") on instance " + instance + ".");

        self.solvetraces[(instance, solver, run)] = solvetrace;

    def getSolvers(self) :
        '''Returns a set with the solver names (without runnames) from the solvedata.'''
        solvers = set();
        for sr in self.solvedata.items :
            solvers.add(sr.split('@')[0]);
        return solvers;

    def _updateKnownPrimalBound(self) :
        '''Update known primal bound value when a solver found a better value (and did not fail).'''

        if not self.hasSolveAttribute('PrimalBound') :
            return;

        if not self.hasInstanceAttribute('KnownPrimalBound') :
            return;

        instancefails = self.instancedata['Fail'];
        direction = self.instancedata['Direction'];
        knownpbs = self.instancedata['KnownPrimalBound'];

        for sr in self.solvedata.items :
            df = self.solvedata[sr];
            solverfails = df['Fail'];
            solverpbs = df['PrimalBound'];
            for i, solverpb in solverpbs.items() :
                if solverfails[i] or instancefails[i] :
                    continue;
                knownpb = knownpbs[i];
                if (np.isnan(knownpb) or utils.isRelGT(direction[i] * knownpb, direction[i] * solverpb, 1e-6)) and not np.isinf(solverpb) :
                    print('Solver', sr, 'improved known primal bound for instance', i, 'from', knownpb, 'to', solverpb);
                    knownpbs[i] = solverpb;

    def _computeGaps(self) :
        '''Computes gaps between solver reported primal and dual bounds and known optimal values.'''
        zerogaptol = self.options['zerogaptol'];

        if self.hasSolveAttribute('PrimalBound') and self.hasSolveAttribute('DualBound') :
            self.addSolveAttribute(None, None, None, 'Gap', None);
            #self.solvedata[self.solvedata.items[0]]['Gap'][self.solvedata.major_axis[0]] = 42;

            for sr in self.solvedata.items :
                df = self.solvedata[sr];
                df['Gap'] = utils.computeGap(self.instancedata['Direction'] * df['PrimalBound'], self.instancedata['Direction'] * df['DualBound'], tol = zerogaptol);
                self.solvedata[sr] = df;

        if self.hasInstanceAttribute('KnownPrimalBound') and self.hasInstanceAttribute('KnownDualBound') :
            if self.hasSolveAttribute('PrimalBound') :
                self.addSolveAttribute(None, None, None, 'PrimalGap', None);

                for sr in self.solvedata.items :
                    df = self.solvedata[sr];
                    df['PrimalGap'] = utils.computeGap((self.instancedata['Direction'] * df['PrimalBound']).fillna(np.inf), self.instancedata['Direction'] * self.instancedata['KnownPrimalBound'], tol = zerogaptol);
                    # only have primal gap if best primal bound is optimal
                    df.loc[self.instancedata['KnownPrimalBound'] != self.instancedata['KnownDualBound'], 'PrimalGap'] = np.nan;
                    self.solvedata[sr] = df;

            if self.hasSolveAttribute('DualBound') :
                self.addSolveAttribute(None, None, None, 'DualGap', None);

                for sr in self.solvedata.items :
                    df = self.solvedata[sr];
                    df['DualGap'] = utils.computeGap(self.instancedata['Direction'] * self.instancedata['KnownDualBound'], (self.instancedata['Direction'] * df['DualBound']).fillna(-np.inf), tol = zerogaptol);
                    # only have dual gap if best dual bound is optimal
                    df.loc[self.instancedata['KnownPrimalBound'] != self.instancedata['KnownDualBound'], 'DualGap'] = np.nan;
                    self.solvedata[sr] = df;

    def _evaluateSolvetraces(self):
        '''Computes primal and dual integrals for each given solve trace.'''

        def _mygapFloatFloat(a, b) :
            '''Computes the gap between two numbers (always positive).'''
            if a is None or np.isnan(a) or b is None or np.isnan(b) :
                return 1.0;
            if a == b :
                return 0.0;
            if np.isinf(a) or np.isinf(b) :
                return 1.0;
            if a*b < 0.0 :
                return 1.0;
            return abs(a-b) / max(abs(a), abs(b));

        def _mygapSeriesSeries(a, b) :
            '''Computes the gap between two series (always positive; elementwise).'''
            result = pd.Series(index = a.index);
            for i in a.index :
                result[i] = _mygapFloatFloat(a[i], b[i]);
            return result;

        def _mygapSeriesFloat(a, b) :
            '''Computes the gap between a series and a number (always positive; elementwise).'''
            result = pd.Series(index = a.index);
            for i in a.index :
                result[i] = _mygapFloatFloat(a[i], b);
            return result;

        for (instance, solver, run), solvetrace in self.solvetraces.items() :

            # we need a time column
            if 'Time' not in solvetrace :
                continue;

            solvetrace = solvetrace.sort_values(by='Time');

            # check if we have an optimal value for the instance
            knownopt = None;
            if self.hasInstanceAttribute('KnownPrimalBound') and self.hasInstanceAttribute('KnownDualBound') :
                pb = self.instancedata['KnownPrimalBound'][instance];
                db = self.instancedata['KnownDualBound'][instance];
                if pb == db :
                    knownopt = pb;

            direction = self.instancedata['Direction'][instance];

            gapint = None;
            pgapint = None;
            dgapint = None;

            if 'PrimalBound' in solvetrace and 'DualBound' in solvetrace :
                #solvetrace['Gap'] = _mygapSeriesSeries(direction * solvetrace['PrimalBound'], direction * solvetrace['DualBound']);
                gapint = 0.0;
            if 'PrimalBound' in solvetrace and knownopt is not None :
                #solvetrace['PrimalGap'] = _mygapSeriesFloat(direction * solvetrace['PrimalBound'], direction * knownopt);
                pgapint = 0.0;
            if 'DualBound' in solvetrace and knownopt is not None :
                #solvetrace['DualGap'] = _mygapSeriesFloat(direction * solvetrace['DualBound'], direction * knownopt);
                dgapint = 0.0;

            lasttime = 0.0;
            lastidx = -1;
            lastpb = np.nan;
            lastdb = np.nan;
            for idx, row in solvetrace.iterrows() :

                time = row['Time'];
                if not time >= lasttime :
                    print('Warning: Time running backwards in solvetrace for solver ' + solver + ' run ' + run + ' instance ' + instance + ': from ' + str(lasttime) + ' to ' + str(time) + ': Ignoring.');
                    continue;
                assert time >= lasttime;

                if 'PrimalBound' in row :
                    pb = row['PrimalBound'];
                    if not np.isnan(lastpb) and direction * pb > direction * lastpb and time != lasttime :
                        print('Warning: Primal bound worsen for solver', solver, 'run', run, 'instance', instance, 'from', lastpb, '(index', lastidx, 'time', lasttime, ')', 'to', pb, '(index', idx, 'time', time, ')');

                if 'DualBound' in row :
                    db = row['DualBound'];
                    if not np.isnan(lastdb) and direction * db < direction * lastdb and time != lasttime :
                        print('Warning: Dual bound worsen for solver', solver, 'run', run, 'instance', instance, 'from', lastdb, '(index', lastidx, 'time', lasttime, ')', 'to', db, '(index', idx, 'time', time, ')');

                if gapint is not None :
                    g = _mygapFloatFloat(lastpb, lastdb);
                    gapint += (time - lasttime) * g;

                if pgapint is not None :
                    g = _mygapFloatFloat(lastpb, knownopt);
                    pgapint += (time - lasttime) * g;

                if dgapint is not None :
                    g = _mygapFloatFloat(lastdb, knownopt);
                    dgapint += (time - lasttime) * g;

                lastpb = pb;
                lastdb = db;
                lasttime = time;
                lastidx = idx;

            if gapint is not None :
                self.addSolveAttribute(instance, solver, run, 'PrimalDualIntegral', gapint)

            if pgapint is not None :
                self.addSolveAttribute(instance, solver, run, 'PrimalIntegral', pgapint)

            if dgapint is not None :
                self.addSolveAttribute(instance, solver, run, 'DualIntegral', dgapint)


    def _aggregateRunRecords(self, solverruns) :
        '''Aggregates a given list of solver runs.

        If one record has been marked as failed, so will be the aggregated result.
        Computes arithmetic averages of numeric attributes (solving time, primal and dual bounds, ...).
        For termination status, returns the worst status among all runs.

        @param solverruns Solver runs (as tuple (solver, run)) for which we aggregate run records.
        '''
        if len(solverruns) == 0 :
            return;

        solver = solverruns[0][0];

        # if we have just one run, just copy that one
        if len(solverruns) == 1 :
            self.aggrsolvedata[solver] = self.solvedata[solverruns[0][0] + '@' + solverruns[0][1]];
            return;

        self.aggrsolvedata[solver] = pd.DataFrame(index = self.solvedata.major_axis, columns = self.solvedata.minor_axis);

        # for each solve attribute (column)
        for attr in self.solvedata.minor_axis :
            # for each instance (items)
            for inst in self.solvedata.major_axis :
                aggrval = _defaultaggrfunc(attr, self.solvedata.ix[solverruns, inst, attr]);
                #print attr, inst, '=', aggrval;
                self.aggrsolvedata[solver][attr][inst] = aggrval;
                #self.aggrsolvedata.set_value(solver, attr, inst, aggrval);

    def _aggregateSolvedata(self) :
        '''Aggregates runs belongs to the same solver in solvedata and stores result in aggrsolvedata.
        If all solvers have only one run, the resulting aggrsolvedata will be identical to solvedata, except that run names are dropped from the items.
        '''
        # couldn't get multiindex work properly, especially when transposing the panel
        solvers = set();
        for solverrun in self.solvedata.items :
            solvers.add(solverrun.split('@')[0]);

        self.aggrsolvedata = pd.Panel(items = solvers, major_axis = self.solvedata.major_axis, minor_axis = self.solvedata.minor_axis);

        for solver in solvers :
            solverruns = [];
            for sr in self.solvedata.items :
                [s,r] = sr.split('@');
                if s != solver :
                    continue;
                solverruns.append((s, r));

            self._aggregateRunRecords(solverruns);

            if len(solverruns) > 1 :
                # remember that a real aggregation took place
                self.aggregated = True;

        selector = self._setup.getInstanceSelector(self);
        if selector is not None :
            self.aggrsolvedata = self.aggrsolvedata.select(selector, axis=1) ;
            self.instancedata = self.instancedata.reindex(self.aggrsolvedata.major_axis);
            self.aggregated = True;


    def read(self, filename, **attribs) :
        '''Reads a PAVER input file.
        @param filename Name of file to read.
        @param extension Specify file extension to determine reader, otherwise tries to determine from filename.
        '''
        if 'extension' in attribs :
            extension = attribs['extension'];
        else :
            extension = None;

        if isinstance(filename, str) :
            atpos = filename.rfind('@');
            if atpos >= 0 :
                solvername = filename[atpos+1:];
                filename = filename[:atpos];
                if len(solvername) == 0 :
                    solvername = filename;
                attribs['solvername'] = solvername ;
            if extension is None :
                dotpos = filename.rfind('.');
                if dotpos >= 0 :
                    extension = filename[dotpos+1:];
            f = open(filename, 'r');  # raises IOError if failing  pylint: disable=W0621
        else :
            f = input;

        if extension is None :
            raise BaseException('Cannot determine file format for file ' + filename);

        if extension == 'trc' or extension == 'pav' :
            readgamstrace.read(f, self, **attribs);
        elif extension == 'solu' :
            readscipsolu.read(f, self, **attribs);
        elif extension == 'solvetrace' :
            readsolvetrace.read(f, self, **attribs);
        else :
            raise BaseException('Cannot determine file format for file ' + filename);

    def run(self) :
        '''Runs main PAVER machinery, that is finalizes and analyzes data.'''
        print('finalizing data');

        # run consistency checks
        self._consistencycheck = checkconsistency.ConsistencyCheck(self._setup.getConsistencyChecks(self));
        self._consistencycheck(self);

        # update known primal bounds w.r.t. solver values
        #self._updateKnownPrimalBound();

        # compute gaps and integrals
        if 'zerogaptol' not in self.options :
            self.options['zerogaptol'] = DEFAULTZEROGAPTOL;
        self._computeGaps();
        self._evaluateSolvetraces();

        # aggregate runs for each solver
        self._aggregateSolvedata();

        if len(self.aggrsolvedata.items) == 0 :
            print('No data, skipping calculations.')
            return;

        # compute solving statistics
        print('computing solve statistics');
        self._solvestat = solvestat.StatisticsGenerator(self._setup.getMetrics(self));
        self._solvestat.calculate(self);

        # setup solvedata writer
        print('setting up solving data writer');
        self._solvedatawriter = writesolvedata.SolveDataWriter(self,
                                                               self._setup.getWriteSolveDataInstanceColumns(self),
                                                               self._setup.getWriteSolveDataRunColumns(self));

    def writeText(self, dirname) :
        '''Write all results in text form to given directory.'''
        if os.access(dirname, os.F_OK) :
            if not os.access(dirname, os.W_OK) :
                raise BaseException('Cannot write to directory ' + dirname);
        else :
            os.makedirs(dirname);

        # print solvedata
        if self._solvedatawriter is not None :
            solvedata = open(os.path.join(dirname, 'solvedata.txt'), 'w');
            attrranges = open(os.path.join(dirname, 'attrranges.txt'), 'w');
            self._solvedatawriter.writeText(self.solvedata, solvedata, attrranges);
            solvedata.close();
            attrranges.close();

        # print aggrsolvedata
        if self._solvedatawriter is not None and self.aggregated :
            solvedata = open(os.path.join(dirname, 'aggrsolvedata.txt'), 'w');
            attrranges = open(os.path.join(dirname, 'aggrattrranges.txt'), 'w');
            self._solvedatawriter.writeText(self.aggrsolvedata, solvedata, attrranges);
            solvedata.close();
            attrranges.close();

        # print statistics
        if self._solvestat is not None :
            categories = self._solvestat.getCategories();
            for c in categories :
                ct = c.replace(' ', '');
                self._solvestat.writeText(c, dirname, "stat_" + ct);

        # print raw instance and solve data
        raw = open(os.path.join(dirname, 'raw.txt'), 'w');
        print(self.instancedata.to_string(), file=raw);
        print("\n", file=raw);
        print(self.solvedata.to_frame(filter_observations=False).to_string(), file=raw);
        if self.aggregated :
            print("\n", self.aggrsolvedata.to_frame(filter_observations=False).to_string(), file=raw);
        raw.close();

        # print options
        opts = open(os.path.join(dirname, 'options.txt'), 'w');
        for name, value in self.options.items() :
            print('{0:30s} {1:s}'.format(name, str(value)), file=opts);
        opts.close();

    def writeHTML(self, dirname) :
        '''Write all results in HTML form to given directory.'''
        if os.access(dirname, os.F_OK) :
            if not os.access(dirname, os.W_OK) :
                raise BaseException('Cannot write to directory ' + dirname);
        else :
            os.makedirs(dirname);

        shutil.copy(os.path.join(os.path.dirname(__file__), 'documentation.html'), os.path.join(dirname, 'documentation.html'));

        index = open(os.path.join(dirname, 'index.html'), 'w');

        print('<HTML><HEAD>', self.htmlheader, '</HEAD><BODY>', file=index);

        #print >> index, '<h2>Performance Results</h2>';
        print('<p>', file=index);
        print('Results written on', datetime.utcnow().ctime(), 'UTC', \
        'using the <A href="http://www.gamsworld.org/performance/paver2/" target="_blank">PAVER 2 tools</A>.', file=index);
        print('</p>', file=index);
        print('<p>', file=index);
        print("<A href='#' onclick = 'Miro.changeTab($(this), 1, 6)'>Documentation</A>", file=index)
        print('</p>', file=index);

        print('<HR>', file=index);

        if self.aggregated :
            # print solver and run names
            print('<P>', file=index);
            table = [HTML.TableRow(['Solver', 'Run'], header = True)];
            prevsolver = None;
            for solverrun in self.solvedata.items :
                [solver, run] = solverrun.split('@');
                if solver != prevsolver :
                    table.append([solver, run]);
                else :
                    table.append(['', run]);
            print(HTML.Table(table), file=index);
            print('</P>', file=index);
        else :
            # print solver names
            print('<P>', file=index);
            table = [HTML.TableRow(['Solver'], header = True)];
            for solverrun in self.solvedata.items :
                table.append([solverrun.split('@')[0]]);
            print(HTML.Table(table), file=index);
            print('</P>', file=index);

        print('<P>', file=index);
        print('<b>Number of Instances:</b>', len(self.instancedata.index), file=index);
        # TODO could give some statistics on observed model types?
        print('</P>', file=index)

        # print solve and instance data
        if self._solvedatawriter is not None :
            solvedata = open(os.path.join(dirname, 'solvedata.html'), 'w');
            print("<HTML>", file=solvedata);
            print("<HEAD>", self.htmlheader, "</HEAD>", file=solvedata);
            print("<BODY>", file=solvedata);
            self._solvedatawriter.writeHTML(self.solvedata, solvedata, dirname);
            print("</BODY>", "</HTML>", file=solvedata);
            solvedata.close();
            print("<P><b><a href='#' onclick = 'Miro.changeTab($(this), 1, 5)'>Detailed Instance and Solving Data</a></b></P>", file=index);

        # print aggregated solve and instance data
        if self._solvedatawriter is not None and self.aggregated :
            solvedata = open(os.path.join(dirname, 'solvedataaggr.html'), 'w');
            print("<HTML>", file=solvedata);
            print("<HEAD>", self.htmlheader, "</HEAD>", file=solvedata);
            print("<BODY>", file=solvedata);
            self._solvedatawriter.writeHTML(self.aggrsolvedata, solvedata, dirname, 'aggr');
            print("</BODY>", "</HTML>", file=solvedata);
            solvedata.close();
            print('<P><b><a href="solvedataaggr.html">Detailed Instance and Aggregated Solving Data</a></b></P>', file=index);

        # print raw instance and (aggregated) solve data
        raw = open(os.path.join(dirname, 'raw.html'), 'w');
        raw_instance_csv = open(os.path.join(dirname, 'raw_instance.csv'), 'w');
        raw_solver_csv = open(os.path.join(dirname, 'raw_solverruns.csv'), 'w');
        print("<HTML><HEAD>", self.htmlheader, "</HEAD><BODY>", file=raw);
        print("<P>", "Instance Data:<BR>", self.instancedata.to_html(), "</P>", file=raw);
        print("<P>", "Solver Runs:<BR>", self.solvedata.to_frame(filter_observations=False).to_html(), "</P>", file=raw);
        print("Instance Data:", self.instancedata.to_csv(), file=raw_instance_csv);
        print("Solver Runs:", self.solvedata.to_frame(filter_observations=False).to_csv(), file=raw_solver_csv);
        if self.aggregated :
            print("<P>", "Aggregated Solver Runs:<BR>", self.aggrsolvedata.to_frame(filter_observations=False).to_html(), "</P>", file=raw);
            print("Aggregated Solver Runs:", self.aggrsolvedata.to_frame(filter_observations=False).to_csv(), file=raw_solver_csv);
        print("</BODY>", "</HTML>", file=raw);
        #for (instance, solver, run), solvetrace in self.solvetraces.iteritems() :
        #    print >> raw, "Solvetrace (" + solver + "," + run + ") on instance " + instance;
        #    print >> raw, solvetrace.to_html();
        raw.close();
        raw_instance_csv.close();
        raw_solver_csv.close();

        print('<P>(<a href=paver/raw.html target=_blank>Raw Data</a>)</P>', file=index);
        print('<P><a href=paver/raw_instance.csv download>Download Raw Instance Data</a></P>', file=index);
        print('<P><a href=paver/raw_solverruns.csv download>Download Raw Solver Runs Data</a></P>', file=index);
        print('<HR>', file=index);

        # print solve statistics
        if self._solvestat is not None :

            print('<P><b>Statistics</b>:', file=index)
            print('<UL>', file=index);
            categories = self._solvestat.getCategories();
            n = 2;
            for c in categories :
                ct = c.replace(' ', '');
                self._solvestat.writeHTML(self, c, dirname, "stat_" + ct, args);
                #print('<LI><a href="stat_' + ct + '.html">', c, '</a>', file=index);
                print(n);
                print("<LI><a href='#' onclick = 'Miro.changeTab($(this), 1, ", n, ")'>", c, "</a>", file=index);
                n+=1;
            print('</UL></P>', file=index);

        # print options
        print('<HR>', file=index)
        print('<P>PAVER options:', file=index);
        t = [];
        for name, value in self.options.items() :
            t.append([name, value]);
        print(HTML.Table(t, attribs = {'class' : 'dataframe'}, col_align=['left', 'right']), file=index);
        print('</P>', file=index);

        print('</BODY></HTML>', file=index);
        index.close();

def setupArgumentParser(paversetup) :
    '''Sets up command line argument parsers.'''
    parser = argparse.ArgumentParser(description = 'Python Performance Analysis and Visualization for Efficient Reproducibility', fromfile_prefix_chars = '@');
    parser.add_argument('file', type = str, help = 'input file (see also --tracefile and --solufile)', nargs = '*', default = []);
    parser.add_argument('--setup', type = str, help = 'paver setup file');
    parser.add_argument('--tracefile', type = str, help = 'GAMS trace file (specify <filename>@<solvername> to overwrite solvername, or <filename>@ to use filename as solvername)', action = 'append', default = []);
    parser.add_argument('--solufile', type = str, help = 'SCIP solution file', action = 'append', default = []);
    parser.add_argument('--writetext', type = str, default = DEFAULTWRITETEXT,
                        help = 'directory where to write results in text format (default: ' + str(DEFAULTWRITETEXT) + ')');
    parser.add_argument('--writehtml', type = str, default = DEFAULTWRITEHTML,
                        help = 'directory where to write results in HTML format (default: ' + str(DEFAULTWRITEHTML) + ')');
    parser.add_argument('--figformat', type = str, default = DEFAULTFIGFORMAT,
                        help = 'format for plotted figures (default: ' + str(DEFAULTFIGFORMAT) + ')');
    parser.add_argument('--writeimg', type = str, default = DEFAULTWRITEIMG,
                        help = 'directory where to write results in IMG format (default: ' + str(DEFAULTWRITEIMG) + ')');
    parser.add_argument('--gmswebiter', type = str, default = DEFAULTGMSWEBITER,
                        help = 'Iteration of the Paver runs in the current GAMS WebUI session.  (default: ' + str(DEFAULTGMSWEBITER) + ')');

    # Paver object options
    parser.add_argument('--zerogaptol', type = float, default = DEFAULTZEROGAPTOL,
                        help = 'tolerance on bounds for zero gap (default: ' + str(DEFAULTZEROGAPTOL) + ')');
    parser.add_argument('--nocheckinstanceattr', action = 'store_true',
                        help = 'disables check that instance attributes are the same among different runs');

    solvestat.addCommandLineOptions(parser);
    paversetup.addCommandLineOptions(parser);
    readgamstrace.addCommandLineOptions(parser);
    readscipsolu.addCommandLineOptions(parser);
    readsolvetrace.addCommandLineOptions(parser);

    return parser;

if __name__ == '__main__' :
    # pylint: disable=C0103
    if sys.argv.count('--setup') :
        setuppos = sys.argv.index('--setup');
        if len(sys.argv) <= setuppos+1 :
            print('Missing argument for --setup option.')
            sys.exit();
        setupfile = sys.argv[setuppos+1];
    else :
        setupfile = os.path.join(os.path.dirname(__file__), 'setupdefault.py');

    print('reading setup file', setupfile);
    exec(compile(open(setupfile).read(), setupfile, 'exec'));
    setup = PaverSetup();  # pylint: disable=E0602

    paverparser = setupArgumentParser(setup);
    args = paverparser.parse_args();

    paver = Paver(setup, dict(**args.__dict__));

    # TODO ensure that solu files are read after all solver outcomes
    for f in args.file :
        print('reading', f);
        paver.read(f, **args.__dict__);

    for f in args.tracefile :
        print('reading GAMS trace file ', f);
        paver.read(f, extension = 'trc', **args.__dict__);

    for f in args.solufile :
        print('reading SCIP solution file ', f);
        paver.read(f, extension = 'solu', **args.__dict__);

    paver.run();

    if len(args.writehtml.strip()) > 0 :
        print('writing HTML output to', args.writehtml, '(this can take a while...)');
        paver.writeHTML(args.writehtml);
    if args.writetext is not None :
        print('writing text output to', args.writetext);
        paver.writeText(args.writetext);
