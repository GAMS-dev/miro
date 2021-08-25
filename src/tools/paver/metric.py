'''PAVER Benchmark Metric'''

import numpy as np;
#import utils;


class Metric :
    '''Defines a set of benchmark metrics w.r.t. a solve attribute.'''

    def __init__(self, category, attribute) :
        '''Constructor.
        @param category Category of the metric.
        @param attribute Solve attribute that is analyzed by the metric.
        '''
        self.category = category;
        self.attribute = attribute;
        self.filter = [None];
        self.clip_lower = -np.inf;
        self.clip_upper =  np.inf;
        self.shift = 0.0;
        self.failvalue = None;
        self.navalue = None;
        self.reltol = None;
        self.abstol = None;
        self.multbydirection = False;
        self.betterisup = False;
        self.ppfilter = [];
        self.pprelative = True;
        self.pprelsemilog = True;
        self.ppabsolute = False;
        self.ppabssemilog = False;
        self.ppextended = False;

        self.means = True;
        self.quantiles = [0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0];
        self.boxplot = True;
