/*
    Use this command to compile the example:
LINUX:
    export GAMSCPPAPI=$GAMSDIR/apifiles/C++/api
    export GAMSCAPI=$GAMSDIR/apifiles/C/api
    g++ read_psse28.cpp $GAMSCPPAPI/gdxco.cpp $GAMSCAPI/gdxcc.c -I$GAMSCPPAPI -I$GAMSCAPI -std=c++0x -ldl -o read_psse28
Windows (cmd):
    cl /EHsc read_psse28.cpp %GAMSDIR%\apifiles\C++\api\gdxco.cpp %GAMSDIR%\apifiles\C\api\gdxcc.c -I %GAMSDIR%\apifiles\C++\api -I %GAMSDIR%\apifilatof(busnum[i]), 2 );
Windows (PowerShell):
    cl /EHsc read_psse28.cpp $env:GAMSDIR\apifiles\C++\api\gdxco.cpp $env:GAMSDIR\apifiles\C\api\gdxcc.c -I $env:GAMSDIR\apifiles\C++\api -I $env:GAMSDIR\apifiles\C\api
*/

#include <map>
#include <cmath>
#include <vector>
#include <string>
#include <cstring>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include "gdxco.hpp"

using namespace std;
using namespace GAMS;

static std::string Indx[GMS_MAX_INDEX_DIM];
static gdxValues_t Values;

void ReportGDXError(GDX &PGX) {
    std::string S;

    cout << "**** Fatal GDX Error" << endl;
    PGX.ErrorStr(PGX.GetLastError(), S);
    cout << "**** " << S << endl;
    exit(1);
}

void ReportIOError(int N, const std::string &msg) {
    cout << "**** Fatal I/O Error = " << N << " when calling " << msg << endl;
    exit(1);
}

void WriteData(GDX &PGX, const std::string &s, const double V) {
    Indx[0] = s;
    Values[GMS_VAL_LEVEL] = V;
    PGX.DataWriteStr(Indx,Values);
    Indx[0] = "";
}

void WriteData(GDX &PGX, const std::string *s, const double V, const int dim) {
    int i;
    for( i=0; i<dim; i++ )
    {
        Indx[i] = s[i];
    }
    Values[GMS_VAL_LEVEL] = V;
    PGX.DataWriteStr(Indx,Values);
    for( i=0; i<dim; i++ )
    {
        Indx[i] = "";
    }
}

int main (int argc, char *argv[]) {

    std::string Msg, inFileName, outFileName, Sysdir;
    int ErrNr;


    int i=0, j=0, k=0, c=0, status=0;
    int numbuses=0, numgens=0, numlines=0, maxc=0;
    double mult;
    string *temp, tmp;

    int  baseMVA=0;

    // Bus data
    int lastbus = 0;
    vector<int>    busmap(20000);
    vector<int>    busnum(20000);
    vector<string> name(20000);
    vector<double> baseKV(20000);
    vector<int>    type(20000);
    vector<double> gshunt(20000);
    vector<double> bshunt(20000);
    vector<int>    area(20000);
    vector<int>    zone(20000);
    vector<double> vm(20000);
    vector<double> va(20000);
    vector<int>    owner(20000);
    vector<double> minVM(20000);
    vector<double> maxVM(20000);

    // Load data
    double *Pd, *Qd;
    int bus;

    // Generator data
    vector<string>  atBus(2000);
    vector<string>  genIndex(2000);
    vector<double>  Pg(2000);
    vector<double>  Qg(2000);
    vector<double>  Qmax(2000);
    vector<double>  Qmin(2000);
    vector<double>  Vs(2000);
    vector<string>  IREG(2000);
    vector<double>  Mbase(2000);
    vector<double>  Zr(2000);
    vector<double>  Zx(2000);
    vector<double>  Rt(2000);
    vector<double>  Xt(2000);
    vector<double>  Gtap(2000);
    vector<int>     gen_status(2000);
    vector<double>  RMPCT(2000);
    vector<double>  Pmax(2000);
    vector<double>  Pmin(2000);


    // Branch (line) data
    vector<int>     from(15000);
    vector<int>     to(15000);
    vector<int>     circuit(15000);
    vector<double>  r(15000);
    vector<double>  x(15000);
    vector<double>  bc(15000);
    vector<double>  rateA(15000);
    vector<double>  rateB(15000);
    vector<double>  rateC(15000);
    vector<double>  ratio(15000);
    vector<double>  angle(15000);
    vector<double>  gi(15000);
    vector<double>  bi(15000);
    vector<double>  gj(15000);
    vector<double>  bj(15000);
    vector<int>     line_status(15000);
    vector<int>     transformer(15000);
    vector<double>  minAngleDiff(15000);
    vector<double>  maxAngleDiff(15000);
    vector<double>  length(15000);

    // Transformer temp data
    int bus_t[3];
    double r_t[3], x_t[3], nomv_t[3];
    double minVM_t[3];
    double maxVM_t[3];
    double rateA_t[3];
    double rateB_t[3];
    double rateC_t[3];
    double sbase_t[3];
    double ratio_t[3];
    double angle_t[3];
    double vmstar=0, vastar=0;

    // Switched shunt data
    vector<double*> switchedBs(20000);
    vector<double> switched_bshunt_given(20000);
    vector<int*> switched_elements(20000);
    vector<int> num_switched_shunts(20000);


    if (argc < 2 || argc > 3) {
        cout << "**** read_psse32: incorrect number of parameters" << endl;
        exit(1);
    }

    if (2 == argc) {
        outFileName = "output.gdx";
    } else {
        outFileName = argv[2];
    }


    inFileName = argv[1];
    Sysdir = getenv("GAMSDIR");
    cout << "read_psse32 using GAMS system directory: " << Sysdir << endl;
    GDX PGX(Sysdir, Msg); 

    if (Msg != "") {
        cout << "**** Could not load GDX library" << endl << "**** " << Msg << endl;
        exit(1);
    }

    PGX.GetDLLVersion(Msg);
    cout << "Using GDX DLL version: " << Msg << endl;


    cout << "Opening " << inFileName << endl;
    ifstream infile(inFileName);
    if(!infile)
    {
        cout << "File not found, exiting..." << endl;
        exit(1);
    }
    string LINE;
    char *cell, *line, *line2;

    // Get S_base and strip off header
    getline(infile, LINE);
    // Remove CEII warning
    if( LINE.find("CEII") != string::npos )
    {
        cout << LINE << endl;
        getline(infile, LINE);
    }
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ");
    cell = strtok(NULL," ");
    baseMVA = atof(cell);
    delete[] line;
    
    getline(infile, LINE);
    getline(infile, LINE);


    cout << "Getting bus data" << endl;
    // Get bus data
    getline(infile, LINE);
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ,'");
    while( LINE.find("END OF BUS DATA") == string::npos )
    {
        if( numbuses >= busnum.size() )
        {
            cout << "Resizing vectors..." << endl;
            busnum.resize(  2*busnum.size() );
            name.resize(    2*busnum.size() );
            baseKV.resize(  2*busnum.size() );
            type.resize(    2*busnum.size() );
            gshunt.resize(  2*busnum.size() );
            bshunt.resize(  2*busnum.size() );
            area.resize(    2*busnum.size() );
            zone.resize(    2*busnum.size() );
            vm.resize(      2*busnum.size() );
            va.resize(      2*busnum.size() );
            owner.resize(   2*busnum.size() );
            minVM.resize(   2*busnum.size() );
            maxVM.resize(   2*busnum.size() );
            switchedBs.resize(            2*busnum.size() );
            switched_elements.resize(     2*busnum.size() );
            switched_bshunt_given.resize( 2*busnum.size() );
            num_switched_shunts.resize(   2*busnum.size() );
        }
        i = atoi(cell);
        if( i > lastbus )
        {
            lastbus = i;
        }
        busnum[numbuses] = i;
        while( i >= busmap.size() )
        {
            busmap.resize(  2*busmap.size() );
        }
        busmap[i] = numbuses;

        // Name is given as a quoted string, may have spaces
        cell = strtok(NULL, ",");
        name[numbuses] = cell;
        
        cell = strtok(NULL, " ,");
        baseKV[numbuses] = atof(cell);
        
        cell = strtok(NULL, " ,");
        type[numbuses] = atoi(cell);
        
        cell = strtok(NULL, " ,");
        gshunt[numbuses] = atof(cell);
        
        cell = strtok(NULL, " ,");
        bshunt[numbuses] = atof(cell);
        
        cell = strtok(NULL, " ,");
        area[numbuses] = atoi(cell);
        
        cell = strtok(NULL, " ,");
        zone[numbuses] = atoi(cell);

        cell = strtok(NULL, " ,");
        vm[numbuses] = atof(cell);

        cell = strtok(NULL, " ,");
        va[numbuses] = atof(cell);

        // Vm min and max not given -- picking 0.90 and 1.10 based on defaults in spec
        minVM[numbuses] = 0.80;
        maxVM[numbuses] = 1.10;

//        cell = strtok(NULL, " ,");
//        owner[numbuses] = atoi(cell);

        delete[] line;
        numbuses++;
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());
        cell = strtok(line, " ,");
    }
    delete[] line;

    Pd = new double[numbuses];
    Qd = new double[numbuses];
    for( j=0; j<numbuses; j++ )
    {
        Pd[j] = 0;
        Qd[j] = 0;
    }

    cout << "Getting load data" << endl;
    // Get load data
    while( LINE.find("BEGIN LOAD DATA") == string::npos )
    {
        getline(infile, LINE);
    }
    //cout << LINE << endl;
    getline(infile, LINE);
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ,");
    while( LINE.find("END OF LOAD DATA") == string::npos )
    {
        //cout << LINE << endl;
        // The first entry can be either bus number or bus name, but in
        // all of our data it's bus number, so we're going with that.
        // Bus number also allows us to index into loads much easier.
        i = atoi(cell);
        i = busmap[i];

        // Load number may have been given as quoted string (no spaces),
        // but we can just aggregate loads at the same bus so the identifier
        // can be tossed out.
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");

        // Discard out-of-service loads
        if( atoi(cell) == 1 )
        {
            // We also can ignore areas and zones, defaulting to bus info
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");

            // Which of these are actually important to us?
            // Demand at constant MVA load?
            cell = strtok(NULL, " ,");
            Pd[i] += atof(cell);
            cell = strtok(NULL, " ,");
            Qd[i] += atof(cell);

            // Demand at constant current load?
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");

            // Demand at constant admittance load?
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");

            // Ignore owner of demand
            cell = strtok(NULL, " ,");
        }

        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());
        cell = strtok(line, " ,");
    }
    delete[] line;

    cout << "Getting generator data" << endl;
    // Get generator data
    // PSSE doesn't seem to have cost data included
    while( LINE.find("BEGIN GENERATOR DATA") == string::npos )
    {
        getline(infile, LINE);
    }
    getline(infile, LINE);
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ,");
    while( LINE.find("END OF GENERATOR DATA") == string::npos )
    {
        if( numgens >= atBus.size() )
        {
            cout << "Resizing vectors..." << endl;
            atBus.resize(      2*atBus.size() );
            genIndex.resize(   2*atBus.size() );
            Pg.resize(         2*atBus.size() );
            Qg.resize(         2*atBus.size() );
            Qmax.resize(       2*atBus.size() );
            Qmin.resize(       2*atBus.size() );
            Vs.resize(         2*atBus.size() );
            IREG.resize(       2*atBus.size() );
            Mbase.resize(      2*atBus.size() );
            Zr.resize(         2*atBus.size() );
            Zx.resize(         2*atBus.size() );
            Rt.resize(         2*atBus.size() );
            Xt.resize(         2*atBus.size() );
            Gtap.resize(       2*atBus.size() );
            gen_status.resize( 2*atBus.size() );
            RMPCT.resize(      2*atBus.size() );
            Pmax.resize(       2*atBus.size() );
            Pmin.resize(       2*atBus.size() );
        }
        //cout << LINE << endl;
        atBus[numgens] = cell;

        cell = strtok(NULL, " ,");
        genIndex[numgens] = cell;

        // Gen Index may have been given as quoted string (ignore it either way)
        if( ((string)cell).length() == 1 )
        {
            cell = strtok(NULL, ",'");
            cell = strtok(NULL, " ,");
        }
        else if( ((string)cell).find("'") != string::npos )
        {
            cell = strtok(NULL, ",'");
        }
        else
        {
            cell = strtok(NULL, " ,");
        }
        Pg[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Qg[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Qmax[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Qmin[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Vs[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        IREG[numgens] = cell;

        cell = strtok(NULL, " ,");
        Mbase[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Zr[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Zx[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Rt[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Xt[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Gtap[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        gen_status[numgens] = atoi(cell);

        cell = strtok(NULL, " ,");
        RMPCT[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Pmax[numgens] = atof(cell);

        cell = strtok(NULL, " ,");
        Pmin[numgens] = atof(cell);

        // Ignoring owner and fraction of ownership (financial information)
        //cell = strtok(NULL, " ,");
        //cell = strtok(NULL, " ,");

        delete[] line;
        numgens++;
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());
        cell = strtok(line, " ,");
    }
    delete[] line;
    

    cout << "Getting line data" << endl;
    // Get branch (line) data
    while( LINE.find("BEGIN BRANCH DATA") == string::npos )
    {
        getline(infile, LINE);
    }
    getline(infile, LINE);
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ,");
    while( LINE.find("END OF BRANCH DATA") == string::npos )
    {
        if( numlines >= from.size() )
        {
            cout << "Resizing vectors..." << endl;
            from.resize(        2*from.size() );
            to.resize(          2*from.size() );
            circuit.resize(     2*from.size() );
            r.resize(           2*from.size() );
            x.resize(           2*from.size() );
            bc.resize(          2*from.size() );
            rateA.resize(       2*from.size() );
            rateB.resize(       2*from.size() );
            rateC.resize(       2*from.size() );
            ratio.resize(       2*from.size() );
            angle.resize(       2*from.size() );
            gi.resize(          2*from.size() );
            bi.resize(          2*from.size() );
            gj.resize(          2*from.size() );
            bj.resize(          2*from.size() );
            line_status.resize( 2*from.size() );
            transformer.resize( 2*from.size() );
            minAngleDiff.resize(2*from.size() );
            maxAngleDiff.resize(2*from.size() );
            length.resize(      2*from.size() );
        }
        from[numlines] = atoi(cell);

        // "To" bus is negative if it's the metered end(?)
        cell = strtok(NULL, " ,");
        to[numlines] = abs(atoi(cell));

        // Apparently circuit identifier may also be quoted string? (No spaces)
        // Working here under the assumption that either way, it's a number.
        cell = strtok(NULL, " ,'");
        c = atoi(cell);
        if( c > maxc )
        {
            maxc = c;
        }
        circuit[numlines] = c;

        cell = strtok(NULL, " ,'");
        r[numlines] = atof(cell);

        // Some of the x values are negative...?
        cell = strtok(NULL, " ,");
        x[numlines] = abs(atof(cell));

        cell = strtok(NULL, " ,");
        bc[numlines] = atof(cell);

        cell = strtok(NULL, " ,");
        rateA[numlines] = atof(cell);
        if(rateA[numlines] == 0) {
            rateA[numlines] = 99999;
        }

        cell = strtok(NULL, " ,");
        rateB[numlines] = atof(cell);
        if(rateB[numlines] == 0) {
            rateB[numlines] = 99999;
        }

        cell = strtok(NULL, " ,");
        rateC[numlines] = atof(cell);
        if(rateC[numlines] == 0) {
            rateC[numlines] = 99999;
        }

        // Shunt element on i side of branch
        cell = strtok(NULL, " ,");
        gi[numlines] = atof(cell);

        cell = strtok(NULL, " ,");
        bi[numlines] = atof(cell);

        // Shunt element on j side of branch
        cell = strtok(NULL, " ,");
        gj[numlines] = atof(cell);

        cell = strtok(NULL, " ,");
        bj[numlines] = atof(cell);

        cell = strtok(NULL, " ,");
        line_status[numlines] = atoi(cell);

        cell = strtok(NULL, " ,");
        length[numlines] = atof(cell);

        // This section only includes lines without transformers
        ratio[numlines] = 0;
        angle[numlines] = 0;
        transformer[numlines] = 0;

        // Ignoring owner and fraction of ownership (financial information)
        //cell = strtok(NULL, " ,");
        //cell = strtok(NULL, " ,");

        // We don't have line-specific limits on angle difference
        minAngleDiff[numlines] = -360;
        maxAngleDiff[numlines] = 360;

        delete[] line;
        numlines++;
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());
        cell = strtok(line, " ,");
    }
    delete[] line;


    // Transformer data -- this could take some time to figure out
    // Careful! At least one of the columns has quotes and spaces!
    // Transformers include the line between the noted busses
    cout << "Getting transformer data" << endl;
    // Get transformer data
    while( LINE.find("BEGIN TRANSFORMER DATA") == string::npos )
    {
        getline(infile, LINE);
    }
    getline(infile, LINE);
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ,");
    while( LINE.find("END OF TRANSFORMER DATA") == string::npos )
    {
        if( numlines >= from.size() )
        {
            cout << "Resizing vectors..." << endl;
            from.resize(        2*from.size() );
            to.resize(          2*from.size() );
            circuit.resize(     2*from.size() );
            r.resize(           2*from.size() );
            x.resize(           2*from.size() );
            bc.resize(          2*from.size() );
            rateA.resize(       2*from.size() );
            rateB.resize(       2*from.size() );
            rateC.resize(       2*from.size() );
            ratio.resize(       2*from.size() );
            angle.resize(       2*from.size() );
            minVM.resize(       2*from.size() );
            maxVM.resize(       2*from.size() );
            gi.resize(          2*from.size() );
            bi.resize(          2*from.size() );
            gj.resize(          2*from.size() );
            bj.resize(          2*from.size() );
            line_status.resize( 2*from.size() );
            transformer.resize( 2*from.size() );
            minAngleDiff.resize(2*from.size() );
            maxAngleDiff.resize(2*from.size() );
            length.resize(      2*from.size() );
        }

        bus_t[0] = atoi(cell);
        cell = strtok(NULL, " ,");
        bus_t[1] = atoi(cell);
        cell = strtok(NULL, " ,'");
        bus_t[2] = atoi(cell);


        // Circuit number may be given as quoted string
        cell = strtok(NULL, " ,'");
        c = atoi(cell);


        // Status (col 12) is the only other entry in the first line we use right now
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");

        // Name is given as a quoted string, so we just need to match the end quote
        cell = strtok(NULL, "'");
        cell = strtok(NULL, " ,'");
        status = atoi(cell);

        // Read the second line of the transformer entry
        delete[] line;
        line = new char[2];
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());

        cell = strtok(line, " ,");
        r_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        x_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        sbase_t[0] = atof(cell);

        // If third bus is not zero, we need more of the columns on this line.
        if( bus_t[2] != 0 )
        {
            cell = strtok(NULL, " ,");
            r_t[1] = atof(cell);
            cell = strtok(NULL, " ,");
            x_t[1] = atof(cell);
            cell = strtok(NULL, " ,");
            sbase_t[1] = atof(cell);

            cell = strtok(NULL, " ,");
            r_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            x_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            sbase_t[2] = atof(cell);

            cell = strtok(NULL, " ,");
            vmstar = atof(cell);
            cell = strtok(NULL, " ,");
            vastar = atof(cell);
        }


        // Read the third line of the transformer entry
        delete[] line;
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());

        cell = strtok(line, " ,");
        ratio_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        nomv_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        angle_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        rateA_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        rateB_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        rateC_t[0] = atof(cell);

        // There's a bunch of parameters relating to controls here,
        // but there's no place for them in the models yet
        cell = strtok(NULL, " ,");
        cell = strtok(NULL, " ,");
        cell = strtok(NULL, " ,");

        cell = strtok(NULL, " ,");
        minVM_t[0] = atof(cell);
        cell = strtok(NULL, " ,");
        maxVM_t[0] = atof(cell);

        // Read the fourth line of the transformer entry
        delete[] line;
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());

        cell = strtok(line, " ,");
        ratio_t[1] = atof(cell);
        cell = strtok(NULL, " ,");
        nomv_t[1] = atof(cell);

        // If third bus is not zero, this is a 3-winding transformer, so finish reading line 4
        // Read the extra line and adding 2 more branches and add a dummy "star bus"
        if( bus_t[2] != 0 )
        {
            cell = strtok(NULL, " ,");
            angle_t[1] = atof(cell);
            cell = strtok(NULL, " ,");
            rateA_t[1] = atof(cell);
            cell = strtok(NULL, " ,");
            rateB_t[1] = atof(cell);
            cell = strtok(NULL, " ,");
            rateC_t[1] = atof(cell);

            // There's a bunch of parameters relating to controls here,
            // but there's no place for them in the models yet
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");

            cell = strtok(NULL, " ,");
            minVM_t[1] = atof(cell);
            cell = strtok(NULL, " ,");
            maxVM_t[1] = atof(cell);

            // Read the fifth line (only in k!=0 transformers)
            delete[] line;
            getline(infile, LINE);
            line = new char[LINE.length() + 1];
            strcpy(line, LINE.c_str());
            cell = strtok(line, " ,");
            ratio_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            nomv_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            angle_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            rateA_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            rateB_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            rateC_t[2] = atof(cell);

            // There's a bunch of parameters relating to controls here,
            // but there's no place for them in the models yet
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");
            cell = strtok(NULL, " ,");

            cell = strtok(NULL, " ,");
            minVM_t[2] = atof(cell);
            cell = strtok(NULL, " ,");
            maxVM_t[2] = atof(cell);

            // Add star bus to buses
            lastbus++;
            busnum[numbuses] = lastbus;
            type[numbuses] = 1;
            Pd[numbuses] = 0;
            Qd[numbuses] = 0;
            gshunt[numbuses] = 0;
            bshunt[numbuses] = 0;
            area[numbuses] = 0;
            vm[numbuses] = vmstar;
            va[numbuses] = vastar;
            baseKV[numbuses] = 0;
            // What should the min/max voltage be at the star bus?
            maxVM[numbuses] = 10;
            minVM[numbuses] = 0;
            numbuses++;
        }

        j = (bus_t[2]==0)?bus_t[1]:(lastbus);
        // If two-winding transformer, we only have one line to add, otherwise we have 3
        // For two-winding, j is the second bus
        // For three-winding, j is the star bus
        //if( bus_t[2] != 0 )
        //{
        //    cout << bus_t[0] << " " << bus_t[1] << " " << bus_t[2] << endl;
        //    cout << "\t" << j << endl;
        //}
        for(i=0; i<=((bus_t[2]==0)?0:2); i++)
        {
            if (maxVM_t[i] > 0)
            {
                maxVM[busmap[bus_t[i]]] = maxVM_t[i];
            }
            if (minVM_t[i] > 0)
            {
                minVM[busmap[bus_t[i]]] = minVM_t[i];
            }

            if( c > maxc )
            {
                maxc = c;
            }
            from[numlines]        = bus_t[i];
            to[numlines]          = j;
            circuit[numlines]     = c;
            line_status[numlines] = status;
            transformer[numlines] = 1;

            r[numlines] = r_t[i];
            x[numlines] = (x_t[i]==0) ? x_t[0] : x_t[i];
            bc[numlines] = 0;
            //if(x[numlines] == 0)
            //{
            //    cout << "BAD x VALUE" << endl;
            //}
            //if( bus_t[2] != 0 )
            //{
            //    cout << "\t" << numlines << ": " << from[numlines] << "." << to[numlines] << "." << circuit[numlines] << "\t" << r[numlines] << "\t" << x[numlines] << endl;
            //}

            if( rateA_t[i] == 0 )
            {
                rateA_t[i] = 99999;
            }
            rateA[numlines] = rateA_t[i];
            if( rateB_t[i] == 0 )
            {
                rateB_t[i] = 99999;
            }
            rateB[numlines] = rateB_t[i];
            if( rateC_t[i] == 0 )
            {
                rateC_t[i] = 99999;
            }
            rateC[numlines] = rateC_t[i];

            ratio[numlines] = ratio_t[i]/((bus_t[2]==0)?ratio_t[1]:1.0);
            angle[numlines] = angle_t[i];

            // We don't have line-specific limits on angle difference
            minAngleDiff[numlines] = -360;
            maxAngleDiff[numlines] = 360;

            numlines++;
        }

        // Clean up and get the next entry
        delete[] line;
        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());
        cell = strtok(line, " ,");
    }
    delete[] line;

    // Area Interchange Data -- will we use this?

    // Switched shunt data
    cout << "Getting switched shunt data" << endl;
    // Get load data
    while( LINE.find("BEGIN SWITCHED SHUNT DATA") == string::npos )
    {
        getline(infile, LINE);
    }
    //cout << LINE << endl;
    getline(infile, LINE);
    line = new char[LINE.length() + 1];
    strcpy(line, LINE.c_str());
    cell = strtok(line," ,");
    while( LINE.find("END OF SWITCHED SHUNT DATA") == string::npos )
    {
        //cout << LINE << endl;
        // The first entry can be either bus number or bus name, but in
        // all of our data it's bus number, so we're going with that.
        // Bus number also allows us to index into loads much easier.
        i = atoi(cell);
        i = busmap[i];

        // Shunt number may have been given as quoted string (no spaces),
        // but we can just aggregate shunts at the same bus so the identifier
        // can be tossed out.

        // Ignore the shunt type and voltage parameters
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");
        cell = strtok(NULL, " ,'");

        cell = strtok(NULL, " ,");
        switched_bshunt_given[i] += atof(cell);
        
        switched_elements[i] = new int[10];
        switchedBs[i] = new double[10];
        j = 0;
        while (cell = strtok(NULL, " ,")) {
            switched_elements[i][j] = atof(cell);
            cell = strtok(NULL, " ,");
            switchedBs[i][j] += atof(cell);
            j++;
        }
        num_switched_shunts[i] = j;

        getline(infile, LINE);
        line = new char[LINE.length() + 1];
        strcpy(line, LINE.c_str());
        cell = strtok(line, " ,");
    }
    delete[] line;


    infile.close();
    cout << "Done getting data, begin writing " << outFileName << endl;



    // Write data to GDX
    PGX.OpenWrite(outFileName, "opf_data", ErrNr);
    if (ErrNr) ReportIOError(ErrNr,"gdxOpenWrite");


    // Write set of integers to preserve order
    j = numbuses;
    if( numgens > j ) { j = numbuses; }
    if( numlines > j ) { j = numlines; }
    if( maxc > j ) { j = maxc; }
    if (!PGX.DataWriteStrStart("ints", "\"List of Integers used\"", 1, GMS_DT_SET, 0))
        { ReportGDXError(PGX); }
    for( i=1; i<=j; i++ )
    {
        line = new char[30];
        sprintf(line, "%d", i);
        WriteData( PGX, line, NULL );
        delete[] line;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);


    // Write bus set
    if (!PGX.DataWriteStrStart("bus", "\"List of buses\"", 1, GMS_DT_SET, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numbuses; i++ )
    {
        line = new char[30];
        sprintf(line, "%d", busnum[i]);
        tmp = line;
        WriteData(PGX, tmp, NULL);
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Write gen set
    if (!PGX.DataWriteStrStart("gen", "\"List of generators\"", 1, GMS_DT_SET, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numgens; i++ )
    {
        line = new char[30];
        sprintf(line, "%d", i+1);
        WriteData( PGX, line, NULL );
        delete[] line;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Write branchrows set
    if (!PGX.DataWriteStrStart("branchrows", "\"List of lines\"", 1, GMS_DT_SET, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numlines; i++ )
    {
        line = new char[30];
        sprintf(line, "%d", i+1);
        WriteData( PGX, line, NULL );
        delete[] line;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Check to make sure we don't have any bad circuit identifiers (given as noninteger)
    for( i=0; i<numlines; i++ )
    {
        if( circuit[i] == 0 )
        {
            maxc++;
            circuit[i] = maxc;
        }
    }
    // Write circuit set
    if (!PGX.DataWriteStrStart("circuit", "\"Most lines between buses\"", 1, GMS_DT_SET, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<maxc; i++ )
    {
        line = new char[30];
        sprintf(line, "%d", i+1);
        WriteData( PGX, line, NULL );
        delete[] line;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    cout << "Done writing sets, beginning parameters" << endl;

    // Write baseMVA
    if (!PGX.DataWriteStrStart("baseMVA", "\"System MVA base\"", 0, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    WriteData( PGX, "", baseMVA ); 
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Write busdata parameter (matrix)
    if (!PGX.DataWriteStrStart("busdata", "\"Bus data\"", 2, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numbuses; i++ )
    {
        line = new char[30];
        temp = new string[2];
        sprintf(line, "%d", i+1);
        temp[0] = line;
        temp[1] = "1";
        WriteData( PGX, temp, (double)busnum[i], 2 );
        temp[1] = "2";
        WriteData( PGX, temp, (double)type[i], 2 );
        temp[1] = "3";
        WriteData( PGX, temp, (double)Pd[i], 2 );
        temp[1] = "4";
        WriteData( PGX, temp, (double)Qd[i], 2 );
        temp[1] = "5";
        WriteData( PGX, temp, (double)gshunt[i], 2 );
        temp[1] = "6";
        WriteData( PGX, temp, (double)bshunt[i], 2 );
        temp[1] = "7";
        WriteData( PGX, temp, (double)area[i], 2 );
        temp[1] = "8";
        WriteData( PGX, temp, (double)vm[i], 2 );
        temp[1] = "9";
        WriteData( PGX, temp, (double)va[i], 2 );
        temp[1] = "10";
        WriteData( PGX, temp, (double)baseKV[i], 2 );
        temp[1] = "11";
        WriteData( PGX, temp, (double)zone[i], 2 );
        temp[1] = "12";
        WriteData( PGX, temp, (double)maxVM[i], 2 );
        temp[1] = "13";
        WriteData( PGX, temp, (double)minVM[i], 2 );

        delete[] line;
        delete[] temp;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Write gendata parameter (matrix)
    if (!PGX.DataWriteStrStart("gendata", "\"Generator data\"", 2, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numgens; i++ )
    {
        line = new char[30];
        temp = new string[2];
        sprintf(line, "%d", i+1);
        temp[0] = line;

        // Assuming buses are named with numbers, so we can use the atBus parameter
        temp[1] = atBus[i];
        strcpy(line, temp[1].c_str());
        temp[1] = "1";
        WriteData( PGX, temp, atof(line), 2 );
        delete[] line;

        temp[1] = "2";
        WriteData( PGX, temp, (double)Pg[i], 2 );
        temp[1] = "3";
        WriteData( PGX, temp, (double)Qg[i], 2 );

        temp[1] = "4";
        WriteData( PGX, temp, (double)Qmax[i], 2 );
        temp[1] = "5";
        WriteData( PGX, temp, (double)Qmin[i], 2 );

        temp[1] = "6";
        WriteData( PGX, temp, (double)Vs[i], 2 );
        temp[1] = "7";
        WriteData( PGX, temp, (double)Mbase[i], 2 );
        temp[1] = "8";
        WriteData( PGX, temp, (double)gen_status[i], 2 );

        temp[1] = "9";
        WriteData( PGX, temp, (double)Pmax[i], 2 );
        temp[1] = "10";
        WriteData( PGX, temp, (double)Pmin[i], 2 );

        // Pc1, Pc2, Qc1min, Qc1max, Qc2min, Qc2max not given (what are they?)
        temp[1] = "11";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "12";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "13";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "14";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "15";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "16";
        WriteData( PGX, temp, 0.0, 2 );

        // Ramp rates not given either?
        temp[1] = "17";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "18";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "19";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "20";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "21";
        WriteData( PGX, temp, 0.0, 2 );


        delete[] temp;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Write gencostdata parameter (matrix)
    if (!PGX.DataWriteStrStart("gencostdata", "\"Generator cost data\"", 2, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numgens; i++ )
    {
        line = new char[30];
        temp = new string[2];
        sprintf(line, "%d", i+1);
        temp[0] = line;

        // Cost curves not given -- set a simple linear function for now
        temp[1] = "1";
        WriteData( PGX, temp, 2.0, 2 );
        temp[1] = "2";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "3";
        WriteData( PGX, temp, 0.0, 2 );
        temp[1] = "4";
        WriteData( PGX, temp, 3.0, 2 );
        temp[1] = "5";
        WriteData( PGX, temp, 2.0, 2 );
        temp[1] = "6";
        WriteData( PGX, temp, 1.0, 2 );
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);
    
    // Write branchdata parameter
    if (!PGX.DataWriteStrStart("branchdata", "\"Branch (line) data\"", 2, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numlines; i++ )
    {
        line = new char[30];
        temp = new string[2];
        sprintf(line, "%d", i+1);
        temp[0] = line;

        // Assuming buses are named with numbers, so we can use the to/from parameters
        temp[1] = "1";
        WriteData( PGX, temp, (double)from[i], 2 );
        temp[1] = "2";
        WriteData( PGX, temp, (double)to[i], 2 );
        temp[1] = "14";
        WriteData( PGX, temp, (double)circuit[i], 2 );

        temp[1] = "3";
        WriteData( PGX, temp, (double)r[i], 2 );
        temp[1] = "4";
        WriteData( PGX, temp, (double)x[i], 2 );
        temp[1] = "5";
        WriteData( PGX, temp, (double)bc[i], 2 );

        temp[1] = "6";
        WriteData( PGX, temp, (double)rateA[i], 2 );
        temp[1] = "7";
        WriteData( PGX, temp, (double)rateB[i], 2 );
        temp[1] = "8";
        WriteData( PGX, temp, (double)rateC[i], 2 );

        temp[1] = "9";
        WriteData( PGX, temp, (double)ratio[i], 2 );
        temp[1] = "10";
        WriteData( PGX, temp, (double)angle[i], 2 );

        temp[1] = "11";
        WriteData( PGX, temp, (double)line_status[i], 2 );

        temp[1] = "12";
        WriteData( PGX, temp, (double)minAngleDiff[i], 2 );
        temp[1] = "13";
        WriteData( PGX, temp, (double)maxAngleDiff[i], 2 );

        temp[1] = "15";
        WriteData( PGX, temp, (double)transformer[i], 2 );

        delete[] line;
        delete[] temp;
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Set pwcostcoef(gen,'1') to 0 so raw2gdx.gms takes the cost functions rather than piecewise points for now
    if (!PGX.DataWriteStrStart("pwcostcoef", "\"Piecewise linear cost coefficients -- not given in PSSE format\"", 2, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numgens; i++ )
    {
        line = new char[30];
        temp = new string[2];
        sprintf(line, "%d", i+1);
        temp[0] = line;

        temp[1] = "1";
        WriteData( PGX, temp, 0, 2 );
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);

    // Write switched shunt data
    if (!PGX.DataWriteStrStart("switchedshuntdata", "\"Switched shunt temp data\"", 3, GMS_DT_PAR, 0))
        { ReportGDXError(PGX); }
    for( i=0; i<numbuses; i++ )
    {
        line = new char[30];
        temp = new string[3];
        sprintf(line, "%d", i+1);
        temp[0] = line;

        temp[1] = "numswitchedshunts";
        temp[2] = "given";
        WriteData( PGX, temp, num_switched_shunts[i], 3 );

        temp[1] = "switchedBs";
        temp[2] = "given";
        WriteData( PGX, temp, switched_bshunt_given[i], 3 );

        for( j=0; j<num_switched_shunts[i]; j++ ) {
            line2 = new char[30];
            sprintf(line, "%d", j+1);
            temp[2] = line;

            temp[1] = "switchedelements";
            WriteData( PGX, temp, switched_elements[i][j], 3 );

            temp[1] = "switchedBs";
            WriteData( PGX, temp, switchedBs[i][j], 3 );
        }
    }
    if (!PGX.DataWriteDone()) ReportGDXError(PGX);
    
    cout << "Done!" << endl;
    if (ErrNr = PGX.Close()) ReportIOError(ErrNr,"gdxClose");

    exit(0);
} /* main */

