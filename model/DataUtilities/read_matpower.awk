BEGIN { FS="[ \t]*;?" ; MVA=1 ; nbus=0 ; ngen=0 ; nbranch=0 ; ndc=0 ; maxc=0 ; costcols=0 }

/];/             { BUS=0 ; GEN=0 ; BRANCH=0 ; GENCOST=0 ; DCLINE=0 }
/mpc\.baseMVA/   { if($3 != 0) {baseMVA=$3+0} }
BUS==1           { nbus++ ; busset[nbus] = $2+0 ; line = "" ; for(i=2; i<=14; i++){line = sprintf("%s %-14s", line,$i)} ; busdata[nbus] = line }
GEN==1           { ngen++ ; line = "" ; for(i=2; i<=22; i++){line = sprintf("%s %-14s", line,$i)} ; gendata[ngen] = line }
BRANCH==1        { nbranch++ ; c = branches[$2,$3] + 1 ; branches[$2,$3] = c ; if(c > maxc) {maxc=c} ; lineset[nbranch] = $2+0"."$3"."c ; trans = 0 ; if ((($10+0 != 0) && ($10+0 != 1)) || ($11+0 != 0)) { trans = 1 } line = "" ; for(i=2; i<=14; i++){line = sprintf("%s %-14s", line,$i)} ; branchdata[nbranch] = sprintf("%s %-14s %-14s",line,c,trans) }
GENCOST==1       { j++ ; cols = $5 ; if($2+0 == 1){cols = 2*$5} ; if(cols+4 > costcols){costcols = cols+4} ; line = "" ; for(i=2; i<=(cols+5); i++){line = sprintf("%s %-14s", line,$i)} ; gencostdata[j] = line }
#DCLINE==1        { ndc++ ; dcset[ndc] = $2+0"."$3+0 ; line = "" ; for(i=2; i<=18; i++) {line = sprintf("%s %-14s", line, $i)} ; dcbranchdata[ndc] = line }
/mpc\.bus/       { BUS=1 }
/mpc\.gen[^cd]/  { GEN=1 }
/mpc\.branch/    { BRANCH=1 }
/mpc\.gencost/   { GENCOST=1 ; j=0 }
#/mpc\.dcline/    { DCLINE=1 }

END { nmax=nbus; if(ngen>nbus){nmax=nbus} ; if(ngen>nmax){nmax=ngen} ; 
      if(nbranch>nmax){nmax=nbranch}; if(costcols>nmax){nmax=costcols} ;
      if(21>nmax){nmax=21};
      printf("parameter baseMVA /" baseMVA "/;\n");
      printf("set ints /0");
      for (i=1; i<=nmax; i++) { printf(", " i) };
      printf("/;\n");
      printf("set bus /" busset[1]);
      for (i=2; i<=nbus; i++) { printf(", " busset[i]) };
      printf("/;\n");
      printf("set gen /1");
      for (i=2; i<=ngen; i++) { printf(", " i) };
      printf("/;\n");
      printf("set circuit /1");
      for (i=2; i<=maxc; i++) { printf(", " i) };
      printf("/;\n");
      printf("set branchrows /1");
      for (i=2; i<=nbranch; i++) { printf(", " i) };
      printf("/;\n");
      printf("set line /" lineset[1]);
      for (i=2; i<=nbranch; i++) { printf(", " lineset[i]) };
      printf("/;\n\n");
      #printf("set dcline /" lineset[1]);
      #for (i=2; i<=ndc; i++) { printf(", " dcset[i]) };
      #printf("/;\n\n");
      printf("table busdata(ints,ints) \n                ");
      for (i=1; i<=13; i++) { printf("%-14s ",i) };
      j=0;
      for (i in busdata) { j++ ; printf("\n%-14s %s",j,busdata[i]) };
      printf("\n;\n\n");
      printf("table gendata(gen,ints) \n                ");
      for (i=1; i<=21; i++) { printf("%-14s ",i) };
      for (i in gendata) { printf("\n%-14s %s",i,gendata[i]) };
      printf("\n;\n\n");
      printf("table gencostdata(gen,ints) \n                ");
      for (i=1; i<=costcols; i++) { printf("%-14s ",i) };
      for (i in gencostdata) { printf("\n%-14s %s",i,gencostdata[i]) };
      printf("\n;\n\n");
      printf("table branchdata(ints,ints) \n                ");
      for (i=1; i<=15; i++) { printf("%-14s ",i) };
      for (i in branchdata) { printf("\n%-14s %s",i,branchdata[i]) };
      printf("\n;\n\n");
      #printf("table dcbranchdata(ints,ints) \n                ");
      #for (i=1; i<=14; i++) { printf("%-14s ",i) };
      #for (i in branchdata) { printf("\n%-14s %s",i,dcbranchdata[i]) };
      #printf("\n;\n\n");
      printf("parameter pwcostcoef(gen,*);\n");
      printf("\n;\n\n");
      printf("parameter switchedshuntdata(bus,*,*);\n");
      printf("\n;\n\n");
    }

