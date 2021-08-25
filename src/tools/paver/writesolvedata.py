'''PAVER writing of detailed instance and solving data tables'''
import os;
import sys;
import math;
import matplotlib.pyplot as plt;
import numpy as np;
import pandas as pd;

from utils import Enumeration;
from utils import TerminationStatus;
import utils;
import HTML;

ColumnAlign = Enumeration("left center right");  # pylint: disable=C0103

def _shortenstr(string, width) :
    '''Shortens a string to a given width, adding * if something had to be cut off.'''
    if len(string) <= width :
        return string;
    if width == 0 :
        return '';
    return string[0:width-1] + '*';

class SolveDataColumn():
    '''Abstract base class for formatting of columns in solvedata table.
    A derived class can or has to implement the following functions:

    getHeader()
    getUnit()
    getAlign()
    getWidth()   [mandatory]
    getStr()     [mandatory]
    getNumber()  [mandatory for plotted columns]
    getColor()

    '''

    def __init__(self, **attribs):
        if 'header' in attribs :
            self._header = attribs['header'];
        else :
            self._header = None;

        if 'align' in attribs :
            self._align = attribs['align'];
        else :
            self._align = None;

        if 'unit' in attribs :
            self._unit = attribs['unit'];
        else :
            self._unit = None;

        if 'plot' in attribs :
            self.plot = attribs['plot'];
        else :
            self.plot = False;

        if 'plotcap' in attribs :
            self.plotcap = attribs['plotcap'];
        else :
            self.plotcap = None;

    def getHeader(self):
        '''Returns the column header string.'''
        return self._header;

    def getUnit(self) :
        '''Returns the column unit string, or None.'''
        return self._unit;

    def getAlign(self) :
        '''Returns the column alignment as ColumnAlign enum, or None.'''
        return self._align;

    def getColor(self, paver, data, instance, solverrun = None) :
        '''Returns the text color for a table cell.'''
        # pylint: disable=W0613,R0201
        return None;

def _getAttributeValueFunction(paver, data, attrname, instance, solverrun = None):
    '''Helper function to get attribute value either from data (either instancedata or solvedata).'''
    # pylint: disable=W0613
    if solverrun is None :
        # return instance attribute
        return data[attrname][instance];

    # return solver run attribute
    return data[solverrun][attrname][instance];

class StringColumn(SolveDataColumn) :
    '''A versatile column class to print strings.'''

    def __init__(self, source, **attribs) :
        SolveDataColumn.__init__(self, **attribs);

        if isinstance(source, str) :
            self._getvaluefunc = lambda paver, data, instance, solverrun : _getAttributeValueFunction(paver, data, source, instance, solverrun);

            if self._header is None :
                self._header = source;
        else :
            self._getvaluefunc = source;

        if 'width' in attribs :
            self._width = attribs['width'];
        else :
            self._width = None;

    def getWidth(self) :
        '''Returns column width.'''
        return self._width;

    def getStr(self, paver, data, instance, solverrun = None) :
        '''Returns table cell content to show as string.'''
        val = None;
        try :
            val = self._getvaluefunc(paver, data, instance, solverrun);
        except :
            pass;  # pylint: disable=W0702
        if isinstance(val, str) :
            return val;
        if val is None or np.isnan(val) :
            return '--';
        return str(val);

    def getNumber(self, paver, data, instance, solverrun = None) :
        '''Returns table cell content as number (to generate plots).'''
        try :
            val = self._getvaluefunc(paver, data, instance, solverrun);
            if np.isnan(val) :
                return None;
            return val;
        except :  # pylint: disable=W0702
            return None;

class IntColumn(SolveDataColumn) :
    '''A versatile column class to print integers.'''

    def __init__(self, source, **attribs):
        SolveDataColumn.__init__(self, **attribs);

        if isinstance(source, str) :
            self._getvaluefunc = lambda paver, data, instance, solverrun : _getAttributeValueFunction(paver, data, source, instance, solverrun);

            if self._header is None :
                self._header = source;
        else :
            self._getvaluefunc = source;

    def getWidth(self) :
        '''Returns column width. We use automatic detection here.'''
        # pylint: disable=R0201
        return None;

    def getStr(self, paver, data, instance, solverrun = None) :
        '''Returns table cell content to show as string.'''
        val = None;
        try :
            val = self._getvaluefunc(paver, data, instance, solverrun);
        except :  # pylint: disable=W0702
            pass;

        if val is None or np.isnan(val) :
            return '--';
        if np.isposinf(val) :
            return 'inf';
        if np.isneginf(val) :
            return '-inf';
        return str(int(val));

    def getNumber(self, paver, data, instance, solverrun = None) :
        '''Returns table cell content as number (to generate plots).'''
        try :
            val = self._getvaluefunc(paver, data, instance, solverrun);
            if np.isnan(val) :
                return None;
            return int(val);
        except :  # pylint: disable=W0702
            return None;

class FloatColumn(SolveDataColumn) :
    '''A versatile column class to print floating point numbers.'''

    def __init__(self, source, **attribs):
        SolveDataColumn.__init__(self, **attribs);

        if isinstance(source, str) :
            self._getvaluefunc = lambda paver, data, instance, solverrun : _getAttributeValueFunction(paver, data, source, instance, solverrun);

            if self._header is None :
                self._header = source;
        else :
            self._getvaluefunc = source;

        if 'precision' in attribs :
            self._precision = attribs['precision'];
        else :
            self._precision = 6;

        if 'format' in attribs :
            self._format = attribs['format'];
        else :
            self._format = 'e';

        if self._align is None :
            self._align = ColumnAlign.right;  # pylint: disable=E1101

    def getWidth(self) :
        '''Returns column width.'''
        if self._format == 'e' :
            # sign, digit before decimal, decmial, _precision digits, exponent (4 chars)
            return self._precision + 7;
        else :
            # automatic determination of width
            return None;

    def getStr(self, paver, data, instance, solverrun = None) :
        '''Returns table cell content to show as string.'''
        val = self._getvaluefunc(paver, data, instance, solverrun);
        if val is None or np.isnan(val) :
            return '--';
        if np.isposinf(val) :
            return 'inf';
        if np.isneginf(val) :
            return '-inf';
        return ('{0:.' + str(self._precision) + self._format + '}').format(val);

    def getNumber(self, paver, data, instance, solverrun = None) :
        '''Returns table cell content as number (to generate plots).'''
        val = self._getvaluefunc(paver, data, instance, solverrun);
        if np.isnan(val) :
            return None;
        return val;


class GapColumn(SolveDataColumn) :
    '''A versatile column class to print gaps.'''

    def __init__(self, attrname, **attribs):
        SolveDataColumn.__init__(self, **attribs);

        self._attrname = attrname;

        if 'precision' in attribs :
            self._precision = attribs['precision'];
        else :
            self._precision = 2;

        if self._header is None :
            self._header = attrname;

        if self._unit is None :
            self._unit = '%';

        if 'cap' in attribs :
            self._cap = attribs['cap'];
        else :
            self._cap = 10.0;

        # overwrite default plotcap from base class
        if 'plotcap' not in attribs :
            self.plotcap = 20.0 * self._cap;

        if self._align is None :
            self._align = ColumnAlign.right;  # pylint: disable=E1101

    def getWidth(self) :
        '''Returns column width.'''
        width = 0;
        if not np.isinf(self._cap) :
            width += math.ceil(math.log10(self._cap));
        else :
            # automatic determination of width
            return None;
        width += 1;  # digit
        width += self._precision;

        return max(width, len(self._header), 6);

    def getStr(self, paver, data, instance, solverrun) :
        '''Returns table cell content to show as string.'''
        # pylint: disable=W0613
        val = data[solverrun][self._attrname][instance];
        if np.isposinf(val) :
            return 'inf';
        if val >= self._cap :
            return 'Large';
        if val <= -self._cap :
            return '-Large';
        if np.isneginf(val) :
            return '-inf';
        if np.isnan(val) :
            return '--';
        return ('{0:.' + str(self._precision) + 'f}').format(100.0*val);

    def getNumber(self, paver, data, instance, solverrun) :
        '''Returns table cell content as number (to generate plots).'''
        # pylint: disable=W0613
        val = data[solverrun][self._attrname][instance];
        if np.isnan(val) :
            return None;
        return val;


class TermStatusColumn(SolveDataColumn) :
    '''Column to print termination status.'''

    # pylint: disable=E1101
    _termcolor = {
             TerminationStatus.Normal            : "Blue",
             TerminationStatus.TimeLimit         : "Black",
             TerminationStatus.NodeLimit         : "Black",
             TerminationStatus.IterationLimit    : "Black",
             TerminationStatus.OtherLimit        : "Black",
             TerminationStatus.UserInterrupt     : "Red",
             TerminationStatus.CapabilityProblem : "Red",
             TerminationStatus.Error             : "Red",
             TerminationStatus.Other             : "Red"
             };
    # pylint: enable=E1101

    def __init__(self, **attribs):
        SolveDataColumn.__init__(self, **attribs);

        if self._header is None:
            self._header = 'TermStatus';

        if self._align is None :
            self._align = ColumnAlign.left;  # pylint: disable=E1101

    def getWidth(self) :
        '''Returns column width.'''
        # pylint: disable=R0201
        return 10;

    def getStr(self, paver, data, instance, solverrun) :
        '''Returns table cell content to show as string.'''
        # pylint: disable=R0201,W0613
        status = data[solverrun]['TerminationStatus'][instance];
        if np.isnan(status) :
            return '--';
        return utils.TerminationStatusNames[status];

    def getColor(self, paver, data, instance, solverrun = None) :
        status = data[solverrun]['TerminationStatus'][instance];
        if np.isnan(status) :
            return None;
        return self._termcolor[status];

    def getNumber(self, paver, data, instance, solverrun) :
        '''Returns table cell content as number (to generate plots).'''
        # pylint: disable=R0201,W0613
        val = data[solverrun]['TerminationStatus'][instance];
        if np.isnan(val) :
            return None;
        return val;


class ExaminerColumn(SolveDataColumn) :
    '''Column to report maximal infeasibility of all examiner computed values'''

    _shortstring = {
        'PrimalVarInfeas' : 'pv',
        'DualVarInfeas'   : 'dv',
        'PrimalConInfeas' : 'pc',
        'DualConInfeas'   : 'dc',
        'PrimalCompSlack' : 'ps',
        'DualCompSlack'   : 'ds' }

    def __init__(self, **attribs):
        SolveDataColumn.__init__(self, **attribs);

        if 'precision' in attribs :
            self._precision = attribs['precision'];
        else :
            self._precision = 2;

        if 'format' in attribs :
            self._format = attribs['format'];
        else :
            self._format = 'e';

    def getWidth(self) :
        '''Returns column width.'''
        if self._format == 'e' :
            # sign, digit before decimal, decmial, _precision digits, exponent (4 chars), short string
            return self._precision + 9;
        else :
            # automatic determination of width
            return None;

    def getHeader(self):
        return "Viol.";

    def getUnit(self) :
        return None;

    def getAlign(self) :
        return ColumnAlign.right;  # pylint: disable=E1101

    def getStr(self, paver, data, instance, solverrun) :
        '''Returns table cell content to show as string.'''
        # pylint: disable=W0613
        maxinfeas = -np.inf;
        maxinfeasattrib = None;
        for attrib in ['PrimalVarInfeas', 'DualVarInfeas', 'PrimalConInfeas', 'DualConInfeas', 'PrimalCompSlack', 'DualCompSlack'] :
            if attrib not in data[solverrun] :
                continue;
            infeas = data[solverrun][attrib][instance];
            if not np.isnan(infeas) and infeas > maxinfeas :
                maxinfeas = infeas;
                maxinfeasattrib = attrib;

        if maxinfeasattrib is None :
            return '--';

        return ('{0:.' + str(self._precision) + self._format + '}' + self._shortstring[maxinfeasattrib]).format(maxinfeas);

    def getNumber(self, paver, data, instance, solverrun) :
        '''Returns table cell content as number (to generate plots).'''
        # pylint: disable=W0613,R0201
        maxinfeas = -np.inf;
        for attrib in ['PrimalVarInfeas', 'DualVarInfeas', 'PrimalConInfeas', 'DualConInfeas', 'PrimalCompSlack', 'DualCompSlack'] :
            if attrib not in data[solverrun] :
                continue;
            infeas = data[solverrun][attrib][instance];
            if not np.isnan(infeas) and infeas > maxinfeas :
                maxinfeas = infeas;

        if np.isneginf(maxinfeas) :
            return None;
        return maxinfeas;

class SolveDataWriter() :
    '''Class to write instance and solve data in a table with user-specified column objects.'''

    def __init__(self, paver, instancecolumns = None, runcolumns = None, singleruns = True, aggrrun = True) :
        self._paver = paver;
        self._instancecolumns = instancecolumns;
        self._runcolumns = runcolumns;
        self._singleruns = singleruns;
        self._aggrrun = aggrrun;

    def _makeChart(self, directory, basename, column, data, solverrun = None) :
        '''Makes a barchart from the values of a specific column.'''
        # assemble data
        plotdata = [];
        for i in self._paver.instancedata.index :
            plotdata.append( (i, column.getNumber(self._paver, data, i, solverrun), column.getStr(self._paver, data, i, solverrun)) );

        # sort data such that missing data is front
        plotdata.sort(key=lambda x : x[1] if x[1] is not None else -np.inf);

        # extract labels and bar heights
        # get last index with missing plotdata
        inames = [];
        barstart = np.empty(len(plotdata));
        ticks = np.empty(len(plotdata));
        heights = np.empty(len(plotdata));
        lastna = None;
        colors = [];
        for s in range(len(plotdata)) :
            inames.append(plotdata[s][0]);
            if plotdata[s][1] is not None :
                if column.plotcap is not None :
                    heights[s] = max(min(plotdata[s][1], column.plotcap), -column.plotcap);
                    if plotdata[s][1] != heights[s] :
                        colors.append('m');
                    else :
                        colors.append('b');
                else :
                    heights[s] = plotdata[s][1];
                    colors.append('b');
            else :
                heights[s] = 0;
                lastna = s;
                colors.append('r');
                #print solvername, runname, plotdata[s][0];
            barstart[s] = s + 0.1;
            ticks[s] = s + 0.5;

        # generate bar chart
        fig = plt.figure(); # start new figure
        rects = plt.bar(barstart, heights, 0.8, color = colors, edgecolor = colors);
        plt.gca().set_xticks(ticks);
        plt.gca().set_xticklabels(inames, rotation = 'vertical', size = 'xx-small');
        plt.gca().tick_params('x', length = 0);

        # arrange size
        plt.xlim(0, len(plotdata));
        minheight = min(heights);
        maxheight = max(heights);
        if column.plotcap is not None :
            minheight = max(-column.plotcap, minheight);
            maxheight = min(+column.plotcap, maxheight);
        #print column.getHeader(), solvername, runname, minheight, maxheight, len(plotdata), column.plotcap;
        assert maxheight < float("inf");
        assert minheight > float("-inf");
        if minheight != maxheight :
            plt.ylim(-minheight, maxheight * 1.2);
            plt.gca().set_aspect(0.2 * len(plotdata) / (maxheight - minheight));
        fig.set_figwidth(20);

        # put actual numbers over chart
        for s in range(len(plotdata)) :
            plt.text(rects[s].get_x()+rects[s].get_width()/2., heights[s]+0.01*maxheight, plotdata[s][2], size = 'xx-small', ha = 'center', va = 'bottom', rotation = 'vertical');

        # mark instances without plotdata
        if lastna is not None :
            plt.plot([lastna+1, lastna+1], [minheight, maxheight], color = 'r');
            if lastna >= 2 :
                rotation = 'horizontal';
            else :
                rotation = 'vertical'
            plt.text((lastna+1) / 2.0, maxheight, 'no data', size = 'xx-small', rotation = rotation, horizontalalignment = 'center', verticalalignment = 'top', color = 'r');

        # add title
        #plt.ylabel('Percentage of instances');
        title = column.getHeader();
        if column.getUnit() is not None :
            title += ' [' + column.getUnit() + ']';
        if solverrun is not None:
            title += ' for ' + str(solverrun);
        plt.title(title);
        plt.tight_layout(pad=0.4, w_pad=0.5, h_pad=1.0);

        # generate output files
        if self._paver.options['figformat'] != 'png' :
            with utils.Timer() :
                plt.savefig(os.path.join(directory, basename + '.' + self._paver.options['figformat']), bbox_inches = 'tight');
        with utils.Timer() :
            plt.savefig(os.path.join(directory, basename + '.png'), bbox_inches = 'tight', dpi = 10);
        plt.close();

    def writeHTML(self, solvedata, out, chartsdir = None, plotfileprefix = '') :
        '''Generates HTML tables for PAVER instance data and given solve data.'''

        print("<P>For a short description on how gaps and integrals are calculated, refer to <A href='#' onclick = 'Miro.changeTab($(this), 5, 6)'>the documentation</A>.</P>", file=out);

        # print instances and solver runs
        col_align = ['left'];
        col_style = [None];
        header1row = [HTML.TableCell('Instance', header = True,  align = 'center',
                                     attribs = {'colspan' : len(self._instancecolumns)+1,
                                                'style' : 'border-right: thick solid ;'})];
        header2row = ['name'];

        haveplot = False;

        # add column alignments and headers for instance data
        for c in self._instancecolumns :
            if c.getAlign() is not None :
                col_align.append(ColumnAlign.getName(c.getAlign()));
            else :
                col_align.append('');
            col_style.append('font-family: monospace');

            cell = HTML.TableCell(c.getHeader(), header = True);
            if c.getUnit() is not None :
                cell.attribs['title'] = 'in ' + str(c.getUnit());
            header2row.append(cell);

            haveplot |= c.plot;
        col_style[-1] = 'border-right: thick solid; font-family: monospace'

        # get column alignments and headers for one run
        caligns = [];
        cnames = [];
        for c in self._runcolumns :
            if c.getAlign() is not None :
                caligns.append(ColumnAlign.getName(c.getAlign()));
            else :
                caligns.append('');

            cell = HTML.TableCell(c.getHeader(), header = True);
            if c.getUnit() is not None :
                cell.attribs['title'] = 'in ' + str(c.getUnit());
            cnames.append(cell);

            haveplot |= c.plot;

        # setup header rows for runs
        for sr in sorted(solvedata.items) :
            header1row.append(HTML.TableCell(str(sr), header = True, align = 'center',
                                             attribs = {'colspan' : len(self._runcolumns)},
                                             style = 'border-right: thick solid ;'));

            col_align += caligns;
            col_style += (['font-family: monospace'] * (len(self._runcolumns)-1)) + ['border-right: thick solid; font-family: monospace'];
            header2row += cnames;

        # start HTML table
        t = HTML.Table(col_align = col_align, col_styles = col_style);
        t.style += ' font-size: 12px;';
        t.rows.append(HTML.TableRow(header1row, header = True));
        t.rows.append(HTML.TableRow(header2row, header = True));

        if haveplot and chartsdir is not None:
            row = [''];

            chartnr = 1;
            for c in self._instancecolumns :
                if c.plot :
                    plotname = plotfileprefix + 'datacolumn_' + str(chartnr);
                    self._makeChart(chartsdir, plotname, c, self._paver.instancedata);
                    row.append(HTML.TableCell('<a href="' + plotname + '.' + self._paver.options['figformat'] + '"><img width=50 src="' + plotname + '.png"></a>', align = 'center'));
                    chartnr += 1;
                else :
                    row.append(HTML.TableCell(''));

            for sr in sorted(solvedata.items) :
                for c in self._runcolumns :
                    if c.plot :
                        plotname = plotfileprefix + 'datacolumn_' + str(chartnr);
                        self._makeChart(chartsdir, plotname, c, solvedata, sr);
                        row.append(HTML.TableCell('<a href="' + plotname + '.' + self._paver.options['figformat'] + '"><img width=50 src="' + plotname + '.png"></a>', align = 'center'));
                        chartnr += 1;
                    else :
                        row.append(HTML.TableCell(''));

            t.rows.append(row);

        # add actual data
        bgcolor = 'LightGrey';
        count = 0;
        for i in sorted(self._paver.instancedata.index) :

            count += 1;
            if count % 30 == 0 :
                t.rows.append(HTML.TableRow(header1row, header = True));
                t.rows.append(HTML.TableRow(header2row, header = True));

            if bgcolor == '' :
                bgcolor = 'LightGrey';
            else :
                bgcolor = '';

            rowattribs = {'bgcolor' : bgcolor};

            row = [HTML.TableCell(i)];

            if self._paver.instancedata['Fail'][i] :
                reasontxt = self._paver.instancedata['FailReason'][i];
                rowattribs['bgcolor'] = 'Pink';
                rowattribs['title'] = reasontxt;

            for c in self._instancecolumns :
                celltext = c.getStr(self._paver, self._paver.instancedata, i);
                cellcolor = c.getColor(self._paver, self._paver.instancedata, i);
                if cellcolor is not None :
                    celltext = '<font color=' + str(cellcolor) + '>' + celltext + '</font>';
                row.append(HTML.TableCell(celltext));
            row[-1].style = 'border-right: thick solid ;'

            for sr in sorted(solvedata.items) :
                cellattribs = {};

                if solvedata[sr]['Fail'][i] :
                    reasontxt = solvedata[sr]['FailReason'][i];
                    cellattribs['bgcolor'] = 'Pink';
                    cellattribs['title'] = reasontxt;

                for c in self._runcolumns :
                    celltext = c.getStr(self._paver, solvedata, i, sr);
                    cellcolor = c.getColor(self._paver, solvedata, i, sr);
                    if cellcolor is not None :
                        celltext = '<font color=' + str(cellcolor) + '>' + celltext + '</font>';
                    thiscellattribs = cellattribs.copy();
                    if c.getUnit() is not None :
                        thiscellattribs['title'] = 'in ' + str(c.getUnit());
                    row.append(HTML.TableCell(celltext, attribs = thiscellattribs));

            t.rows.append(HTML.TableRow(row, attribs = rowattribs));

        print("<P>", t, "</P>", file=out);

        # print small statistics of instance attributes
        df1 = self._paver.instancedata.describe();
        print("<P>Instance Attributes:<BR>", file=out);
        df1.transpose().to_html(out, float_format = '{0:.2f}'.format);
        print("</P>", file=out);

        # print small statistics of solve attributes
        # why is this not working???
        # df2 = solvedata.transpose(2,1,0).to_frame().convert_objects(convert_numeric=True).describe();
        df2 = pd.DataFrame(index = df1.index, columns = solvedata.minor_axis);
        for a in solvedata.minor_axis :
            df2[a] = solvedata.loc[:, :, a].convert_objects(convert_numeric=True).stack().describe();
        print("<P>Solve Attributes (all solver runs):<BR>", file=out);
        df2.transpose().to_html(out, float_format = '{0:.2f}'.format);
        print("</P>", file=out)

    def writeText(self, solvedata, out = sys.stdout, outattrranges = None) :
        '''Generates text tables for PAVER instance data and given solve data.'''
        # check widths for columns that do not provide their own width (at least 4 for 'name' header)
        colwidth = {};
        for coltype in [self._runcolumns, self._instancecolumns] :
            for c in coltype :
                if c.getWidth() is None :
                    colwidth[c] = len(c.getHeader());
                else :
                    colwidth[c] = c.getWidth();

        instancenamewidth = 4;
        for i in self._paver.instancedata.index :
            instancenamewidth = max(instancenamewidth, len(i));

            for c in self._instancecolumns :
                if c.getWidth() is None :
                    colwidth[c] = max(colwidth[c], len(c.getStr(self._paver, self._paver.instancedata, i)));

            for sr in solvedata.items :
                for c in self._runcolumns :
                    if c.getWidth() is None :
                        colwidth[c] = max(colwidth[c], len(c.getStr(self._paver, solvedata, i, sr)));

        # get width of all instance columns together
        instancewidth = instancenamewidth;
        for c in self._instancecolumns :
            instancewidth += colwidth[c] + 1;
        instancewidth += 1;

        # get width of all run columns together
        runwidth = 0;
        for c in self._runcolumns :
            runwidth += colwidth[c] + 1;
        runwidth += 1;

        alignchar = { None : '', ColumnAlign.left : '<', ColumnAlign.center : '^', ColumnAlign.right : '>' };  # pylint: disable=E1101


        # print first header row
        line = ('{0:^' + str(instancewidth) + 's}').format('Instance') + '|';
        for sr in sorted(solvedata.items) :
            line += ('{0:^'+str(runwidth)+'s}').format(_shortenstr(str(sr), runwidth)) + '|';
        print(line, file=out);

        # print second header row
        line = ('{0:' + str(instancenamewidth) + 's}').format('name');
        for c in self._instancecolumns :
            s = _shortenstr(c.getHeader(), colwidth[c]);
            if c.getUnit() is not None and len(s) + 2 + len(c.getUnit()) <= colwidth[c] :
                s += '[' + c.getUnit() + ']';
            line += ('{0:' + alignchar[c.getAlign()] + str(colwidth[c]) + 's}').format(s) + ' ';
        line += 'I|';

        runcolstr = '';
        for c in self._runcolumns :
            s = _shortenstr(c.getHeader(), colwidth[c]);
            if c.getUnit() is not None and len(s) + 2 + len(c.getUnit()) <= colwidth[c] :
                s += '[' + c.getUnit() + ']';
            runcolstr += ('{0:' + alignchar[c.getAlign()] + str(colwidth[c]) + 's}').format(s) + ' ';
        runcolstr += 'I|';
        line += runcolstr * len(solvedata.items);
        print(line, file=out);

        # print ---- line
        line = '-' * (instancewidth+1);
        line += '-' * len(solvedata.items) * (runwidth+1);
        print(line, file=out);

        # print actual data
        for i in sorted(self._paver.instancedata.index) :

            line = ('{0:' + str(instancenamewidth) + 's}').format(i);

            for c in self._instancecolumns :
                line += ('{0:' + alignchar[c.getAlign()] + str(colwidth[c]) + 's}').format(_shortenstr(c.getStr(self._paver, self._paver.instancedata, i), colwidth[c])) + ' ';

            if self._paver.instancedata['Fail'][i] :
                line += '!';
            else :
                line += ' ';
            line += '|';

            for sr in sorted(solvedata.items) :
                for c in self._runcolumns :
                    line += ('{0:' + alignchar[c.getAlign()] + str(colwidth[c]) + 's}').format(_shortenstr(c.getStr(self._paver, solvedata, i, sr), colwidth[c])) + ' ';

                if solvedata[sr]['Fail'][i] :
                    line += '!';
                else :
                    line += ' ';
                line += '|'

            print(line, file=out);

        # print small statistics of instance and solve run attributes
        if outattrranges is not None :
            df1 = self._paver.instancedata.describe();
            # why is this not working???
            # df2 = solvedata.transpose(2,1,0).to_frame().convert_objects(convert_numeric=True).describe();
            df2 = pd.DataFrame(index = df1.index, columns = solvedata.minor_axis);
            for a in solvedata.minor_axis :
                df2[a] = solvedata.loc[:, :, a].convert_objects(convert_numeric=True).stack().describe();
            print(pd.concat([df1, df2], axis=1).transpose(), file=outattrranges);
