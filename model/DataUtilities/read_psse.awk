# Note: Currently does not support 3-winding transformer or many other exotic devices
function abs(x){return ((x < 0.0) ? -x : x)}
BEGIN { FS="[ \t]*,[ \t]*" ; MVA=1 ; nbus=0 ; ngen=0 ; nbranch=0 ; maxc=0 }

/END OF BUS/         { BUS=0 }
/END OF LOAD/        { LOAD=0 }
/END OF GEN/         { GEN=0 }
/END OF BRANCH/      { BRANCH=0 }
/END OF TRANSFORMER/ { TRANS=0 }
MVA==1          { if($2 != 0) {baseMVA=$2+0} else baseMVA=$3+0 ; MVA=0 }
# minVm and maxVm not given -- arbitrarily picking 0.90 and 1.10
BUS==1          { nbus++ ; busset[nbus] = $1+0 ; busdata[$1+0] = sprintf("%-14s %-14s %-14s %-14s %-14s %-14.6f %-14.6f %-14.2f %-14s",$1+0,$4,$5,$6,$7,$9,$10,$3,$8) ; minVM[$1+0] = 0.90 ; maxVM[$1+0] = 1.10 }
LOAD==1         { D[$1+0] = sprintf("%-14.6f %-14.6f",$6,$7) }
GEN==1          { ngen++ ; gendata[ngen] = sprintf("%-14s %-14.4f %-14.4f %-14.4f %-14.4f %-14.6f %-14.4f %-14i %-14.6f %-14.6f %-14s %-14s %-14s %-14s %-14s %-14s %-14s %-14s %-14s %-14s %-14s",$1+0,$3,$4,$5,$6,$7,$9,$15,$17,$18,0,0,0,0,0,0,0,0,0,0,0) }
BRANCH==1       { nbranch++ ; c = $3 ; if(substr(c,1,1)=="'"){c = substr(c,2,length(c)-2)} ; c = c+0 ; if(c > maxc) {maxc=c} ; if($7 == 0){$7=9999} ; lineset[nbranch] = $1+0"."abs($2)"."c ; branchdata[nbranch] = sprintf("%-14i %-14i %-14.6f %-14.6f %-14.6f %-14.4f %-14.4f %-14.4f %-14s %-14s %-14i %-14i %-14i %-14i",$1+0,abs($2),$4,$5,$6,$7,$8,$9,0,0,$14,-360,360,c) }
TRANS==1 && tr==5  { tr=0 } # Haven't seen a 3-winding transformer yet
TRANS==1 && tr==4  { tr=5 ; if(k==0){tr=0} ; ratio2=$1+0 ; nomv2=$2+0 }
# Set voltage limits as given by transformer info (may be different depending on earlier column -- check specification)
TRANS==1 && tr==3  { tr=4 ; ratio1=$1+0 ; nomv1=$2 ; angle1=$3 ; rateA1=$4 ; rateB1=$5 ; rateC1=$6 ; if(rateA1==0){rateA1=9999} ; if(rateB1==0){rateB1=9999} ; if(rateC1==0){rateC1=9999} ; if($10+0 > 0) {minVM[i] = $10+0} ; if($11+0 > 0) {maxVM[i] = $11+0} }
TRANS==1 && tr==2  { tr=3 ; r1=$1+0.0 ; x1=$2 ; sbase1=$3 } # Only considering 2-winding transformers for now
TRANS==1 && tr==1  { tr=2 ; i=$1+0 ; j=$2+0 ; k=$3+0 ; c=$4 ; if(substr(c,1,1)=="'"){c = substr(c,2,length(c)-2)} ; c = c+0 ; if(c > maxc) {maxc=c} ; status=$14 }
TRANS==1 && tr==0 && k==0 { tr=1 ; nbranch++ ; lineset[nbranch] = i"."j"."c ; branchdata[nbranch] = sprintf("%-14i %-14i %-14.6f %-14.6f %-14.6f %-14.4f %-14.4f %-14.4f %-14.6f %-14.6f %-14i %-14i %-14i %-14i",i,j,r1,x1,0,rateA1,rateB1,rateC1,ratio1,angle1,status,-360,360,c) } # Reached the end of a transformer -- write the line data
TRANS==1 && tr==0 && k>0  { } # Haven't seen a 3-winding transformer yet
/BEGIN LOAD/         { LOAD=1 }
/BEGIN GEN/          { GEN=1 }
/BEGIN BRANCH/       { BRANCH=1 }
/BEGIN TRANSFORMER/  { TRANS=1 ; tr=1 ; k=0 }
NR==3           { BUS=1 }

END { nmax=nbus; if(ngen>nmax){nmax=ngen} ; if(nbranch>nmax){nmax=nbranch};
      if(21>nmax){nmax=21}
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
      printf("table busdata(ints,ints) \n               %-14s %-14s ",1,2);
      for (i=5; i<=11; i++) { printf("%-14s ",i) };
      printf("%-14s %-14s %-14s %-14s",14,13,3,4);
      j=0;
      for (i in busdata) { j++ ; printf("\n%-14s %s %-14.6f %-14.6f %s",j,busdata[i],maxVM[i],minVM[i],D[i]) };
      printf("\n;\n\n");
      printf("table gendata(gen,ints) \n               ");
      for (i=1; i<=21; i++) { printf("%-14s ",i) };
      for (i in gendata) { printf("\n%-14s %s",i,gendata[i]) };
      printf("\n;\n\n");
      printf("table gencostdata(gen,ints) \n               ");
      for (i=1; i<=6; i++) { printf("%-14s ",i) };
      for (i in gendata) { printf("\n%-14s %-14s %-14s %-14s %-14s %-14s %-14s",i,2,0,0,3,1,2) };
      printf("\n;\n\n");
      printf("table branchdata(ints,ints) \n               ");
      for (i=1; i<=14; i++) { printf("%-14s ",i) };
      for (i in branchdata) { printf("\n%-14s %s",i,branchdata[i]) };
      printf("\n;\n\n");
      printf("parameter pwcostcoef(gen,*);\n");
      printf("\n;\n\n");
    }

