'''PAVER default setup'''
import numpy as np;

import checkconsistency;
import writesolvedata;
import metric;
import utils;

def _splitusereval(usereval) :
    '''Splits value of --eval option.'''
    splitted = {};

    fields = usereval.split('@');
    splitted['attrib'] = fields[0];

    for field in fields[1:] :
        [key, val] = field.split('=');
        splitted[key] = val;

    return splitted;

class PaverSetup :
    '''Defines a PAVER setup.
    Tries to infer useful checks, metric, and data table columns from instance and solve data.
    '''

    _dataplots = False;

    _defaultboundsreltol = 1e-6;
    _defaultboundsabstol = 1e-4;
    _defaultfeastol = 2e-6;
    _defaultopttol = np.inf;

    _defaulttimeshift = 10.0;
    _defaultnodeshift = 100;
    _defaultmintime = 1.0;
    _defaultfailtime = None;
    _defaultfailnodes = None;
    _defaultgaptol = 1e-6;
    _defaultevalgap = [0.01, 0.1];
    _defaultfiltertime = 10.0;
    _defaultfilternodes = None;
    _defaulttimerelimpr = 0.1;
    _defaultboundrelimpr = 0.1;

    def __init__(self) :
        pass;


    def addCommandLineOptions(self, parser) :
        '''Make command line parser aware of command line options of this setup.'''

        parser.add_argument('--ccreltol', action = 'store', type = float, default = self._defaultboundsreltol,
                            help = 'relative tolerance in consistency check of bounds (default: ' + str(self._defaultboundsreltol) + ')');
        parser.add_argument('--ccabstol', action = 'store', type = float, default = self._defaultboundsabstol,
                            help = 'absolute tolerance in consistency check of bounds (default: ' + str(self._defaultboundsabstol) + ')');
        parser.add_argument('--ccfeastol', action = 'store', type = float, default = self._defaultfeastol,
                            help = 'tolerance for examiner computed primal infeasibilities in consistency check (default: ' + str(self._defaultfeastol) + ')');
        parser.add_argument('--ccopttol', action = 'store', type = float, default = self._defaultopttol,
                            help = 'tolerance for examiner computed dual infeasibilities in consistency check (default: ' + str(self._defaultopttol) + ')');

        parser.add_argument('--timeshift', action = 'store', type = float, default = self._defaulttimeshift,
                            help = 'shift value for shifted geom. means of timings (default: ' + str(self._defaulttimeshift) + ')');
        parser.add_argument('--nodeshift', action = 'store', type = float, default = self._defaultnodeshift,
                            help = 'shift value for shifted geom. means of node numbers (default: ' + str(self._defaultnodeshift) + ')');
        parser.add_argument('--mintime', action = 'store', type = float, default = self._defaultmintime,
                            help = 'time values are increased to mintime for geom. means (default: ' + str(self._defaultmintime) + ')');
        parser.add_argument('--failtime', action = 'store', type = float, default = self._defaultfailtime,
                            help = 'time value to use for failed runs in means (default: omit these instances from means)');
        parser.add_argument('--failnodes', action = 'store', type = float, default = self._defaultfailnodes,
                            help = 'number of nodes to use for failed runs in means (default: omit these instances from means)');
        parser.add_argument('--gaptol', action = 'store', type = float, default = self._defaultgaptol,
                            help = 'maximal allowed relative gap for solved instances (default: ' + str(self._defaultgaptol) + ')');
        parser.add_argument('--evalgap', action = 'append', type = float, default = None,
                            help = 'gap for which to evaluate performance additionally (default: ' + str(self._defaultevalgap) + ')');
        parser.add_argument('--filtertime', action = 'store', type = float, default = self._defaultfiltertime,
                            help = 'add evaluation on only those instances where at least one solver took at least FILTERTIME seconds (default: ' + str(self._defaultfiltertime) + ')');
        parser.add_argument('--filternodes', action = 'store', type = int, default = self._defaultfilternodes,
                            help = 'add evaluation on only those instances where at least one solver enumerates at least FILTERNODES nodes (default: None)');
        parser.add_argument('--timerelimpr', action = 'store', type = float, default = self._defaulttimerelimpr,
                            help = 'minimal relative improvement of time for being better (default: ' + str(self._defaulttimerelimpr) + ')');
        parser.add_argument('--boundrelimpr', action = 'store', type = float, default = self._defaultboundrelimpr,
                            help = 'minimal relative improvement of bound for being better (default: ' + str(self._defaultboundrelimpr) + ')');
        parser.add_argument('--ignoredualbounds', action = 'store_true', default = None,
                            help = 'disables dual bounds in solving data output (default: auto)');
        parser.add_argument('--eval', action = 'append', default = [],
                            help = 'attribute to evaluate other than time and nodes/iterations (default: none)');
        parser.add_argument('--extendedprofiles', action = 'store_true',
                            help = 'enables extended performance profiles');
        parser.add_argument('--instances', action = 'store', type = str, default = None,
                            help = 'name of file with list of instances to select for comparison (default: compare all)');

    def getConsistencyChecks(self, paver) :
        '''Gives a list of consistency checks to execute.'''

        if 'ccreltol' not in paver.options :
            paver.options['ccreltol'] = self._defaultboundsreltol;
        if 'ccabstol' not in paver.options :
            paver.options['ccabstol'] = self._defaultboundsabstol;
        if 'ccfeastol' not in paver.options :
            paver.options['ccfeastol'] = self._defaultfeastol;
        if 'ccopttol' not in paver.options :
            paver.options['ccopttol'] = self._defaultopttol;

        checks = [];

        checks.append(checkconsistency.CheckTerminationStatus());

        checks.append(checkconsistency.CheckBounds(reltol = paver.options['ccreltol'], abstol = paver.options['ccabstol'], ignoredualbounds = 'ignoredualbounds' in paver.options and paver.options['ignoredualbounds']));

        checks.append(checkconsistency.CheckExaminer(paver.options['ccfeastol'], paver.options['ccopttol'], paver.options['ccopttol']));

        return checks;

    def getMetrics(self, paver) :
        '''Gives a list of performance metrics to evaluate.'''

        if 'timeshift' not in paver.options :
            paver.options['timeshift'] = self._defaulttimeshift;
        if 'nodeshift' not in paver.options :
            paver.options['nodeshift'] = self._defaultnodeshift;
        if 'mintime' not in paver.options :
            paver.options['mintime'] = self._defaultmintime;
        if 'failtime' not in paver.options :
            paver.options['failtime'] = self._defaultfailtime;
        if 'failnodes' not in paver.options :
            paver.options['failnodes'] = self._defaultfailnodes;
        if 'gaptol' not in paver.options :
            paver.options['gaptol'] = self._defaultgaptol;
        if 'evalgap' not in paver.options or paver.options['evalgap'] is None:
            paver.options['evalgap'] = self._defaultevalgap;
        if 'timerelimpr' not in paver.options :
            paver.options['timerelimpr'] = self._defaulttimerelimpr;
        if 'boundrelimpr' not in paver.options :
            paver.options['boundrelimpr'] = self._defaultboundrelimpr;
        if 'filtertime' not in paver.options :
            paver.options['filtertime'] = self._defaultfiltertime;

        metrics = [];

        # check whether we have dual bounds/gaps
        ignoredualbounds = 'ignoredualbounds' in paver.options and paver.options['ignoredualbounds'];

        valsbyattr = {};
        for attr in paver.aggrsolvedata.minor_axis :
            valsbyattr[attr] = paver.aggrsolvedata.loc[:, :, attr].stack();
        havegap = not ignoredualbounds and paver.hasSolveAttribute('Gap') and valsbyattr['Gap'].nunique() > 1;
        havedualbound = not ignoredualbounds and paver.hasSolveAttribute('DualBound') and len(set(valsbyattr['Gap']) - set([-np.inf, np.inf])) > 0;
        havedualgap = not ignoredualbounds and paver.hasSolveAttribute('DualGap') and not np.isinf(valsbyattr['DualGap'].min());

        fails = paver.aggrsolvedata.loc[:, :, 'Fail'].astype(np.bool);
        failsany = fails.any(axis=1) | paver.instancedata['Fail'];

        # mask for no fails (for each solver) and no fail on instance in general
        filternofail = ~fails;
        filternofail[paver.instancedata['Fail']] = False;
        filternofail.name = "no fail";

        # get instances without fail for all solver
        filterallnofail = ~failsany;
        filterallnofail.name = "no fail by all solver"

        filterallnofailknownopt = None;
        if paver.hasInstanceAttribute('KnownPrimalBound') and paver.hasInstanceAttribute('KnownDualBound') :
            filterallnofailknownopt = filterallnofail & (paver.instancedata['KnownPrimalBound'] == paver.instancedata['KnownDualBound']);
            filterallnofailknownopt.name = "no fail by all solver and known optimal value"

        # get instances solved by all solvers up to a certain gap
        filterallmaxgap = [];
        if havegap :
            for g in [paver.options['gaptol']] + paver.options['evalgap'] :
                f = (paver.aggrsolvedata.loc[:, :, 'Gap'] <= g).min(axis=1);
                f[failsany] = False;
                f.name = "gap <= %.6g%% and no fail for all solvers" % (100*g);
                filterallmaxgap.append(f);

        # get instances solved up to a certain gap
        filtermaxgap = [];
        if havegap :
            for g in [paver.options['gaptol']] + paver.options['evalgap'] :
                f = (paver.aggrsolvedata.loc[:, :, 'Gap'] <= g)[filternofail].fillna(False).astype(np.bool);
                f.name = "gap <= %.6g%% and not failed" % (100*g);
                filtermaxgap.append(f);

        # get instances where all solvers found a solution up to a certain quality
        filterallmaxprimgap = [];
        if paver.hasSolveAttribute('PrimalGap') :
            for g in [paver.options['gaptol']] + paver.options['evalgap'] :
                f = (paver.aggrsolvedata.loc[:, :, 'PrimalGap'] <= g).min(axis=1);
                f[failsany] = False;
                f.name = "within %.6g%% of known optimal value and no fail for all solvers" % (100*g);
                filterallmaxprimgap.append(f);

        # get instances where a solution up to a certain quality was found
        filtermaxprimgap = [];
        if paver.hasSolveAttribute('PrimalGap') :
            for g in [paver.options['gaptol']] + paver.options['evalgap'] :
                f = (paver.aggrsolvedata.loc[:, :, 'PrimalGap'] <= g)[filternofail].fillna(False).astype(np.bool);
                f.name = "within %.6g%% of known optimal value and not failed" % (100*g);
                filtermaxprimgap.append(f);

        # get instances with a certain dual gap
        filtermaxdualgap = [];
        if havedualgap :
            for g in [paver.options['gaptol']] + paver.options['evalgap'] :
                f = (paver.aggrsolvedata.loc[:, :, 'DualGap'] <= g)[filternofail].fillna(False).astype(np.bool);
                f.name = "dual gap <= %.6g%% and not failed" % (100*g);
                filtermaxdualgap.append(f);

        # get instances with a certain minimal (max) solving time and no fail
        filterminmaxtime = (paver.aggrsolvedata.loc[:, :, 'SolverTime'].max(axis = 1) >= paver.options['filtertime'])[filterallnofail].reindex_like(filterallnofail).fillna(False);
        filterminmaxtime.name = 'time >= ' + str(paver.options['filtertime']) + ' by at least one solver and no fail for all solvers';
        if filterminmaxtime.sum() == 0 :
            filterminmaxtime = None;

        # get instances with a certain minimal (max) solving time and no fail and known optimal value
        if filterallnofailknownopt is not None and filterminmaxtime is not None :
            filterminmaxtimeknownopt = filterminmaxtime & filterallnofailknownopt;
            filterminmaxtimeknownopt.name = 'time >= ' + str(paver.options['filtertime']) + ' by at least one solver and no fail for all solvers and known optimal value';

        if paver.hasSolveAttribute('NumberOfNodes') and 'filternodes' in paver.options and paver.options['filternodes'] is not None :
            # get instances with a certain minimal (max) number of nodes and no fail
            filterminmaxnodes = (paver.aggrsolvedata.loc[:, :, 'NumberOfNodes'].max(axis = 1) >= paver.options['filternodes'])[filterallnofail].reindex_like(filterallnofail).fillna(False);
            filterminmaxnodes.name = 'nodes >= ' + str(paver.options['filternodes']) + ' by at least one solver and no fail for all solvers';

            # get instances with a certain minimal (max) solving time and no fail and known optimal value
            if filterallnofailknownopt is not None :
                filterminmaxnodesknownopt = filterminmaxnodes & filterallnofailknownopt;
                filterminmaxnodesknownopt.name = 'nodes >= ' + str(paver.options['filternodes']) + ' by at least one solver and no fail for all solvers and known optimal value';
        else :
            filterminmaxnodes = None;
            filterminmaxnodesknownopt = None;

        m = metric.Metric('Status', 'Fail');
        filterfails = fails.copy();
        filterfails[paver.instancedata['Fail']] = True;
        filterfails.name = 'instance or solve run marked as failed'
        m.filter = [filterfails];
        m.betterisup = True;
        m.means = False;
        m.quantiles = [];
        m.boxplot = False;
        metrics.append(m);

        # pylint: disable=E1101
        m = metric.Metric('Status', 'TerminationStatus');
        termstat = paver.aggrsolvedata.loc[:, :, 'TerminationStatus'];
        filtertermnormal = (termstat == utils.TerminationStatus.Normal);
        filtertermnormal.name = "normal termination";
        filtertermlimit = (termstat > utils.TerminationStatus.Normal) & (termstat <= utils.TerminationStatus.OtherLimit);
        filtertermlimit.name = "exceeded some limit";
        filtertermuser = (termstat == utils.TerminationStatus.UserInterrupt);
        filtertermuser.name = "interrupted by user";
        filtertermcapa = (termstat == utils.TerminationStatus.CapabilityProblem);
        filtertermcapa.name = "capability problem";
        filtertermerror = (termstat > utils.TerminationStatus.CapabilityProblem);
        filtertermerror.name = "error or other problem";
        m.filter = [filtertermnormal, filtertermlimit, filtertermuser, filtertermcapa, filtertermerror];
        m.means = False;
        m.quantiles = [];
        m.boxplot = False;
        metrics.append(m);
        # pylint: enable=E1101

        m = metric.Metric('Efficiency', 'SolverTime');
        m.shift = paver.options['timeshift'];
        m.clip_lower = paver.options['mintime'];
        m.failvalue = paver.options['failtime'];
        if m.failvalue is not None :
            m.clip_upper = m.failvalue;
        m.reltol = paver.options['timerelimpr'];
        m.abstol = paver.options['mintime'];
        m.filter = [None, filterallnofail];
        m.ppfilter = [filternofail];
        if filterminmaxtime is not None :
            m.filter.append(filterminmaxtime);
            m.ppfilter.append(filterminmaxtime);
        if filterminmaxnodes is not None :
            m.filter.append(filterminmaxnodes);
            m.ppfilter.append(filterminmaxnodes);
        m.filter += filterallmaxgap + filterallmaxprimgap;
        m.ppfilter += filtermaxgap + filtermaxprimgap;
        m.ppabsolute = True;
        m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];
        metrics.append(m);

        if paver.hasSolveAttribute('NumberOfNodes') and valsbyattr['NumberOfNodes'].max() > 1 and valsbyattr['NumberOfNodes'].nunique() > 1 :
            m = metric.Metric('Efficiency', 'NumberOfNodes');
            m.shift = paver.options['nodeshift'];
            m.clip_lower = 1;
            m.failvalue = paver.options['failnodes'];
            if m.failvalue is not None :
                m.clip_upper = m.failvalue;
            m.reltol = 0.1;
            m.filter = [filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            if len(filterallmaxgap) > 0 :
                # w.r.t. all solved instances
                m.filter.append(filterallmaxgap[0]);
                m.ppfilter = [filtermaxgap[0]];
                m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];
            metrics.append(m);

        if paver.hasSolveAttribute('NumberOfIterations') and valsbyattr['NumberOfIterations'].max() > 1 and valsbyattr['NumberOfIterations'].nunique() > 1 :
            m = metric.Metric('Efficiency', 'NumberOfIterations');
            #m.shift = paver.options['nodeshift'];
            m.clip_lower = 1;
            #m.failvalue = paver.options['failnodes'];
            #if m.failvalue is not None :
            #    m.clip_upper = m.failvalue;
            m.reltol = 0.1;
            m.filter = [filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            if len(filterallmaxgap) > 0 :
                # w.r.t. all solved instances
                m.filter.append(filterallmaxgap[0]);
                m.ppfilter = [filtermaxgap[0]];
                m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];
            metrics.append(m);

        for usereval in paver.options['eval'] :
            ueval = _splitusereval(usereval);
            attrib = ueval['attrib'];

            # skip if no interesting data
            if not paver.hasSolveAttribute(attrib) or valsbyattr[attrib].nunique() <= 1 :
                continue;

            m = metric.Metric('--eval', attrib);

            #omit = evaluators.OmitFailedInstance;
            if 'fail' in ueval :
                m.failvalue = float(ueval['fail']);
                #omit = evaluators.OmitInconsistentInstance;
            if 'min' in ueval :
                m.clip_lower = float(ueval['min']);
            if 'max' in ueval :
                m.clip_upper = float(ueval['max']);
            if 'shift' in ueval :
                m.shift = float(ueval['shift']);
            if 'absimpr' in ueval :
                m.abstol = float(ueval['absimpr']);
            else :
                m.abstol = 0.0;
            if 'relimpr' in ueval :
                m.reltol = float(ueval['relimpr']);
            else :
                m.reltol = 0.1;
            m.filter = [None, filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            m.filter += filterallmaxgap + filterallmaxprimgap;
            m.ppfilter = [filternofail] + filtermaxgap + filtermaxprimgap;
            m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];

            metrics.append(m);

        if paver.hasSolveAttribute('PrimalDualIntegral') and not ignoredualbounds :
            m = metric.Metric('Efficiency', 'PrimalDualIntegral');
            m.clip_lower = paver.options['mintime'];
            m.shift = paver.options['timeshift'];
            m.failvalue = paver.options['failtime'];
            if m.failvalue is not None :
                m.clip_upper = m.failvalue;
            m.reltol = paver.options['timerelimpr'];
            m.abstol = paver.options['mintime'];
            m.filter = [filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            m.ppfilter = [filternofail];
            m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];
            metrics.append(m);

        if havegap :
            # averages and quantiles on gap (more or less useful)
            m = metric.Metric('Solution Quality', 'Gap');
            m.clip_lower = 0;
            m.clip_upper = 2.0;
            m.filter = [filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            m.ppfilter = [filternofail];
            m.ppabsolute = True;
            m.pprelative = False;
            metrics.append(m);

            # counts on instance within a certain gap
            m = metric.Metric('Solution Quality', 'Gap');
            m.filter = filtermaxgap;
            m.boxplot = False;
            m.means = False;
            m.quantiles = [];
            metrics.append(m);

        if paver.hasSolveAttribute('PrimalIntegral') and filterallnofailknownopt is not None:
            m = metric.Metric('Efficiency', 'PrimalIntegral');
            m.clip_lower = paver.options['mintime'];
            m.shift = paver.options['timeshift'];
            m.failvalue = paver.options['failtime'];
            if m.failvalue is not None :
                m.clip_upper = m.failvalue;
            m.reltol = paver.options['timerelimpr'];
            m.abstol = paver.options['mintime'];
            m.filter = [filterallnofailknownopt];
            if filterminmaxtimeknownopt is not None :
                m.filter.append(filterminmaxtimeknownopt);
            if filterminmaxnodesknownopt is not None :
                m.filter.append(filterminmaxnodesknownopt);
            m.ppfilter = [filternofail];
            m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];
            metrics.append(m);

        if paver.hasSolveAttribute('PrimalGap') and filterallnofailknownopt is not None :
            # averages and quantiles on primal gap (more or less useful)
            m = metric.Metric('Solution Quality', 'PrimalGap');
            m.clip_lower = 0;
            m.clip_upper = 2.0;
            m.filter = [filterallnofailknownopt];
            if filterminmaxtimeknownopt is not None :
                m.filter.append(filterminmaxtimeknownopt);
            if filterminmaxnodesknownopt is not None :
                m.filter.append(filterminmaxnodesknownopt);
            m.ppfilter = [filternofail];
            m.ppabsolute = True;
            m.pprelative = False;
            metrics.append(m);

            # counts on instance within a certain primal gap
            m = metric.Metric('Solution Quality', 'PrimalGap');
            m.filter = filtermaxprimgap;
            m.boxplot = False;
            m.means = False;
            m.quantiles = [];
            metrics.append(m);

        if paver.hasSolveAttribute('DualIntegral') and not ignoredualbounds and filterallnofailknownopt is not None :
            m = metric.Metric('Efficiency', 'DualIntegral');
            m.clip_lower = paver.options['mintime'];
            m.shift = paver.options['timeshift'];
            m.failvalue = paver.options['failtime'];
            if m.failvalue is not None :
                m.clip_upper = m.failvalue;
            m.reltol = paver.options['timerelimpr'];
            m.abstol = paver.options['mintime'];
            m.filter = [filterallnofailknownopt];
            if filterminmaxtimeknownopt is not None :
                m.filter.append(filterminmaxtimeknownopt);
            if filterminmaxnodesknownopt is not None :
                m.filter.append(filterminmaxnodesknownopt);
            m.ppfilter = [filternofail];
            m.ppextended = 'extendedprofiles' in paver.options and paver.options['extendedprofiles'];
            metrics.append(m);

        if havedualgap and filterallnofailknownopt is not None :
            # averages and quantiles on dual gap (more or less useful)
            m = metric.Metric('Solution Quality', 'DualGap');
            m.clip_lower = 0;
            m.clip_upper = 2.0;
            m.filter = [filterallnofailknownopt];
            if filterminmaxtimeknownopt is not None :
                m.filter.append(filterminmaxtimeknownopt);
            if filterminmaxnodesknownopt is not None :
                m.filter.append(filterminmaxnodesknownopt);
            metrics.append(m);

            # counts on instance within a certain dual gap
            m = metric.Metric('Solution Quality', 'DualGap');
            m.filter = filtermaxdualgap;
            m.boxplot = False;
            m.means = False;
            m.quantiles = [];
            metrics.append(m);

        if paver.hasSolveAttribute('PrimalBound') :
            m = metric.Metric('Solution Quality', 'PrimalBound');
            m.filter = [filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            m.multbydirection = True;
            m.means = False;
            m.quantiles = [];
            m.boxplot = False;
            m.reltol = paver.options['boundrelimpr'];
            metrics.append(m);

        if havedualbound :
            m = metric.Metric('Solution Quality', 'DualBound');
            m.filter = [filterallnofail];
            if filterminmaxtime is not None :
                m.filter.append(filterminmaxtime);
            if filterminmaxnodes is not None :
                m.filter.append(filterminmaxnodes);
            m.multbydirection = True;
            m.betterisup = True;
            m.means = False;
            m.quantiles = [];
            m.boxplot = False;
            m.reltol = paver.options['boundrelimpr'];
            metrics.append(m);

        return metrics;

    def getWriteSolveDataRunColumns(self, paver) :
        '''Gives a list of columns for the detailed solver run tables.'''

        defaultruncolumns = [];
        defaultruncolumns.append(writesolvedata.TermStatusColumn());
        #_defaultcolumns.append(SolveStatusColumn());

        # by default, show primal and dual bounds, primal and dual gaps, gap, and solver time
        if 'ignoredualbounds' in paver.options :
            ignoredualbounds = paver.options['ignoredualbounds'];
        else :
            ignoredualbounds = False;

        # transform into data frame, because group_by didn't work on panel (for me)
        valsbyattr = {};
        for attr in paver.aggrsolvedata.minor_axis :
            valsbyattr[attr] = paver.aggrsolvedata.loc[:, :, attr].stack();

        if not ignoredualbounds and paver.hasSolveAttribute('DualBound') and \
            len(set(valsbyattr['DualBound']) - set([-np.inf, np.inf])) > 0 :
            incldualbounds = True;
            defaultruncolumns.append(writesolvedata.FloatColumn('DualBound',
                                                                header = 'Dual bound'));
        else :
            incldualbounds = False;

        if incldualbounds and paver.hasSolveAttribute('DualIntegral') and not np.isinf(valsbyattr['DualIntegral'].min()) :
            incldualint = True;
            defaultruncolumns.append(writesolvedata.FloatColumn('DualIntegral',
                                                                format = 'f',
                                                                precision = 2,
                                                                header = 'Dual Int.',
                                                                plot = self._dataplots));
        else :
            incldualint = False;

        if not incldualint and incldualbounds and paver.hasSolveAttribute('DualGap') and not np.isinf(valsbyattr['DualGap'].min()) :
            defaultruncolumns.append(writesolvedata.GapColumn('DualGap',
                                                              plot = self._dataplots));

        if paver.hasSolveAttribute('PrimalBound') and \
            len(set(valsbyattr['PrimalBound']) - set([-np.inf, np.inf])) > 0 :
            defaultruncolumns.append(writesolvedata.FloatColumn('PrimalBound',
                                                                header = 'Primal bound'));

        if paver.hasSolveAttribute('PrimalIntegral') and not np.isinf(valsbyattr['PrimalIntegral'].min()) :
            inclprimalint = True;
            defaultruncolumns.append(writesolvedata.FloatColumn('PrimalIntegral',
                                                                format = 'f',
                                                                precision = 2,
                                                                header = 'Prim. Int.',
                                                                plot = self._dataplots));
        else :
            inclprimalint = False;

        if not inclprimalint and paver.hasSolveAttribute('PrimalGap') and not np.isinf(valsbyattr['PrimalGap'].min()) :
            defaultruncolumns.append(writesolvedata.GapColumn('PrimalGap',
                                                              header = 'PrimGap',
                                                              plot = self._dataplots));

        if not ignoredualbounds and paver.hasSolveAttribute('Gap') and valsbyattr['Gap'].nunique() > 1 :
            defaultruncolumns.append(writesolvedata.GapColumn('Gap',
                                                              header = 'Gap',
                                                              plot = self._dataplots));

        if incldualbounds and paver.hasSolveAttribute('PrimalDualIntegral' ) and not np.isinf(valsbyattr['PrimalDualIntegral'].min()) :
            defaultruncolumns.append(writesolvedata.FloatColumn('PrimalDualIntegral',
                                                                format = 'f',
                                                                precision = 2,
                                                                header = 'Gap Int.',
                                                                plot = self._dataplots));

        defaultruncolumns.append(writesolvedata.FloatColumn('SolverTime',
                                                            header = 'Time',
                                                            unit = 's',
                                                            format = 'f',
                                                            precision = 2,
                                                            plot = self._dataplots));


        if paver.hasSolveAttribute('NumberOfNodes') and valsbyattr['NumberOfNodes'].max() > 1 and valsbyattr['NumberOfNodes'].nunique() > 1 :
            inclnodes = True;
            defaultruncolumns.append(writesolvedata.IntColumn('NumberOfNodes',
                                                              header = 'Nodes',
                                                              align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                              plot = self._dataplots));
        else :
            inclnodes = False;

        if not inclnodes and paver.hasSolveAttribute('NumberOfIterations') and valsbyattr['NumberOfIterations'].max() > 1 and valsbyattr['NumberOfIterations'].nunique() > 1 :
            defaultruncolumns.append(writesolvedata.IntColumn('NumberOfIterations',
                                                              header = 'Iters',
                                                              align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                              plot = self._dataplots));

        for usereval in paver.options['eval'] :
            ueval = _splitusereval(usereval);
            attrib = ueval['attrib'];

            # skip if no interesting data
            if not paver.hasSolveAttribute(attrib) or valsbyattr[attrib].nunique() <= 1 :
                continue;

            defaultruncolumns.append(writesolvedata.StringColumn(attrib,
                                                                 align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                                 plot = self._dataplots));

        for attrib in ['PrimalVarInfeas', 'DualVarInfeas', 'PrimalConInfeas', 'DualConInfeas', 'PrimalCompSlack', 'DualCompSlack'] :
            if paver.hasSolveAttribute(attrib) and valsbyattr[attrib].max() > 0.0 :
                defaultruncolumns.append(writesolvedata.ExaminerColumn(plot = self._dataplots, plotcap = 10.0));
                break;

        return defaultruncolumns;

    def getWriteSolveDataInstanceColumns(self, paver) :
        '''Gives a list of columns for the detailed instance tables.'''

        defaultinstancecolumns = [];

        if 'ignoredualbounds' in paver.options :
            ignoredualbounds = paver.options['ignoredualbounds'];
        else :
            ignoredualbounds = False;

        # get minima and maxima for each attribute over all solver-run's and instances
        #minattr = paver.solvedata.min('items').min('index');
        #maxattr = paver.solvedata.max('items').min('index');

        # get minima and maxima for each attribute
        #minattr = paver.instancedata.min();
        #maxattr = paver.instancedata.max();

        if paver.hasInstanceAttribute('NumberOfVariables') and \
            len(paver.instancedata.groupby('NumberOfVariables', sort = False).groups) > 1 :
            defaultinstancecolumns.append(writesolvedata.IntColumn('NumberOfVariables',
                                                                   align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                                   header = '#Vars',
                                                                   plot = self._dataplots));

        if paver.hasInstanceAttribute('NumberOfDiscreteVariables') and \
            len(paver.instancedata.groupby('NumberOfDiscreteVariables', sort = False).groups) > 1 :
            defaultinstancecolumns.append(writesolvedata.IntColumn('NumberOfDiscreteVariables',
                                                                   align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                                   header = '#Disc',
                                                                   plot = self._dataplots));

        if paver.hasInstanceAttribute('NumberOfEquations') and \
            len(paver.instancedata.groupby('NumberOfEquations', sort = False).groups) > 1 :
            defaultinstancecolumns.append(writesolvedata.IntColumn('NumberOfEquations',
                                                                   align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                                   header = '#Equs',
                                                                   plot = self._dataplots));

        if paver.hasInstanceAttribute('Direction') and \
            len(paver.instancedata.groupby('Direction').groups) > 1 :
            defaultinstancecolumns.append(writesolvedata.StringColumn(lambda paver, data, instance, solverrun : utils.DirectionName[data['Direction'][instance]],
                                                                      align = writesolvedata.ColumnAlign.right, # pylint: disable=E1101
                                                                      header = 'Dir'));

        if not ignoredualbounds and paver.hasInstanceAttribute('KnownDualBound') and \
            len(set(paver.instancedata.groupby('KnownDualBound', sort = False).groups.keys()) - set([-np.inf, np.inf])) > 0 :
            defaultinstancecolumns.append(writesolvedata.FloatColumn('KnownDualBound',
                                                                     header = 'Dual bound'));

        if paver.hasInstanceAttribute('KnownPrimalBound') and \
            len(set(paver.instancedata.groupby('KnownPrimalBound', sort = False).groups.keys()) - set([-np.inf, np.inf])) > 0 :
            defaultinstancecolumns.append(writesolvedata.FloatColumn('KnownPrimalBound',
                                                                     header = 'Primal bound'));

        return defaultinstancecolumns;

    def getInstanceSelector(self, paver) :
        if 'instances' not in paver.options or paver.options['instances'] is None :
            return None;
        instances = set();
        for line in open(paver.options['instances']) :
            instances.add(line.strip());
        return lambda x : x in instances;
