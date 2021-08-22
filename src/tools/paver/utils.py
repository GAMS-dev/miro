'''PAVER utility methods.'''

import time;

import numpy as np;
import pandas as pd;

class Enumeration(object) :
    '''Helper class to define an enumeration.'''
    def __init__(self, names) :
        for number, name in enumerate(names.split()) :
            setattr(self, name, number);

    def getName(self, number) :
        '''Gives the original name for a particular enum int.'''
        for a in self.__dict__ :
            if self.__dict__[a] == number :
                return a;
        raise BaseException('Have no attribute with number ' + str(number) + '.');

def relDiff(a, b):
    '''Computes the relative difference between two floats.

    If a or b is infinity, return +/-inf in case of different infinities and 0.0 otherwise.
    If both are finite, returns the absolute difference if |a| <= 1 and |b| <= 1, otherwise returns (a-b)/max(|a|,|b|).
    '''
    if isinstance(a, pd.Series) :
        result = pd.Series(index = a.index);
        for i in a.index :
            result[i] = relDiff(a[i], b[i]);
        return result;

    if abs(a) >= np.inf or abs(b) >= np.inf :
        if a == b :
            return 0.0;
        else :
            return a - b;
    else :
        return float(a-b) / float(max(abs(a), abs(b), 1.0));


def isRelGT(a, b, tol) :
    '''Indicates whether the relative difference of a and b is larger than tol.'''
    return relDiff(a, b) > tol;

def computeGap(upper, lower, tol = 1e-9) :
    '''Computes the gap between two values, or elementwise for two Series '''

    if isinstance(upper, pd.Series) :
        result = pd.Series(index = upper.index);
        for i in upper.index :
            result[i] = computeGap(upper[i], lower[i], tol);
        return result;

    if lower == upper or abs(upper - lower) <= tol :  #the latter does not give true if both bounds are at the same infinity
        return 0.0;
    if abs(upper) <= tol or abs(lower) <= tol or abs(upper) >= np.inf or abs(lower) >= np.inf or upper * lower < 0.0 :
        return np.inf;
    return float(upper - lower) / float(min(abs(upper), abs(lower)));


# pylint: disable=E1101
# pylint: disable=C0103
TerminationStatus = Enumeration("Normal TimeLimit NodeLimit IterationLimit OtherLimit UserInterrupt CapabilityProblem Error Other");
TerminationStatusNames = {
                          TerminationStatus.Normal            : "normal",
                          TerminationStatus.TimeLimit         : "timelim",
                          TerminationStatus.NodeLimit         : "nodelim",
                          TerminationStatus.IterationLimit    : "iterlim",
                          TerminationStatus.OtherLimit        : "othrlim",
                          TerminationStatus.UserInterrupt     : "userint",
                          TerminationStatus.CapabilityProblem : "capabil",
                          TerminationStatus.Error             : "error",
                          TerminationStatus.Other             : "other"
                          };
# pylint: enable=E1101

DirectionName = { -1 : "max", 1 : "min" }
# pylint: enable=C0103


class Timer():
    def __enter__(self) :
        self.start = time.time();

    def __exit__(self, *args) :
        pass;#print '{0:.4f}s'.format(time.time() - self.start), traceback.extract_stack()[-2];
