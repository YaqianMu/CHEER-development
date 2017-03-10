

*===========================policy shock on static model========================

parameter Header Flag determining whether to output header records in output files ;
Header = 1 ;

*----- Declare the output file names

file reportfile / "Output/output.csv" / ;

* ----- Model output options
display "Begin output listing" ;

options limcol=000, limrow=000 ;
options solprint=off ;
*option solvelink=2;

put reportfile ;
if (Header eq 1, put 'Scenario,rate,year,variable,sector,subsector,labor,qualifier,value,solvestat' / ; ) ;

*        print control (.pc)     5 Formatted output; Non-numeric output is quoted, and each item is delimited with commas.
reportfile.pc   = 5 ;
*        page width (.pw)
reportfile.pw = 255 ;
*        .nj numeric justification (default 1)
reportfile.nj =   1 ;
*        .nw numeric field width (default 12)
reportfile.nw =  15 ;
*        number of decimals displayed (.nd)
reportfile.nd =   9 ;
*        numeric zero tolerance (.nz)
reportfile.nz =   0 ;
*        numeric zero tolerance (.nz)
reportfile.nr =   0 ;

file screen / 'con' / ;


parameter unem,pwage;
parameter
report1    reporting variable
report2    reporting variable
report3    reporting variable
report4    reporting variable
report5
report6
report7
report8
report9
report10
report11
report12


set z reduction rate /1*5/  ;
parameter rate(z) /
                   1=0,
                   2=10,
                   3=50,
                   4=100,
                   5=200/;

set sce reduction rate /S0*S8/  ;
set mapsce(sce,sub_elec)  /
S1        .        nuclear
S2        .        wind
S3        .        solar
S4        .        Hydro
S5        .        biomass
/
 ;

parameter sce_s(sce,sub_elec) ;
sce_s(sce,sub_elec) =0;
sce_s(sce,sub_elec)$mapsce(sce,sub_elec) =1;
$offorder

display sce_s;

*=========BAU
loop(sce$(ord(sce) eq 1),

   loop(z$(ord(z) eq 1),

$include CHEER.gen

solve CHEER using mcp;

display CHEER.modelstat, CHEER.solvestat,subelec0,rate;


UNEM(lm,z)=UR.l(lm);
pwage(lm,i,z)=pl.l(lm,i);

report1(z,'so2',i)=scoef_e('process',i)*qdout.l(i) ;
report1(z,'so2','fd')=scoef_e('process','fd')*sum(i,qc.l(i)) ;
report1(z,'so2','total')=sum(i,report1(z,'so2',i))+report1(z,'so2','fd') ;

report1(z,'NOX',i)=ncoef_e('process',i)*qdout.l(i) ;
report1(z,'NOX','fd')=ncoef_e('process','fd')*sum(i,qc.l(i)) ;
report1(z,'NOX','total')=sum(i,report1(z,'NOX',i))+report1(z,'NOX','fd') ;

report2(z,i)=sum(fe,ccoef_p(fe)*qin.l(fe,i)*(1-r_feed(fe,i)));
report2(z,"elec")=sum(sub_elec,sum(fe,ccoef_p(fe)*qin_ele.l(fe,sub_elec)));
report2(z,"fd")=sum(fe,ccoef_h(fe)*qc.l(fe));
report2(z,"Total")=sum(i,report2(z,i))+report2(z,"fd");

report3(z,sub_elec)=sum(fe,ccoef_p(fe)*qin_ele.l(fe,sub_elec)*(1-r_feed(fe,"elec")));

report6(z,lm,"total")= sum(i,qlin.l(lm,i))+sum(sub_elec,qlin_ele.l(lm,sub_elec));
report6(z,lm,i)= qlin.l(lm,i);
report6(z,lm,"elec")= sum(sub_elec,qlin_ele.l(lm,sub_elec));
report6(z,lm,sub_elec)= qlin_ele.l(lm,sub_elec);

report4(z,lm,i) = qlin.l(lm,i)/qdout.l(i);
report4(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec);
report4(z,"total",i) = sum(lm,qlin.l(lm,i))/qdout.l(i);
report4(z,"total",sub_elec) = sum(lm,qlin_ele.l(lm,sub_elec))/qelec.l(sub_elec);

$include %RepPath%\report_static


));


loop(sce$(ord(sce) gt 1 and ord(sce) lt 7),

         loop(z$(ord(z) gt 1),
         loop(sub_elec,
if(sce_s(sce,sub_elec) ne 0,

yelec.fx(sub_elec) = 1+sce_s(sce,sub_elec)*(outputelec0(sub_elec)+1*rate(z))/outputelec0(sub_elec);
);
);
$include CHEER.gen

solve CHEER using mcp;

display CHEER.modelstat, CHEER.solvestat,subelec0,rate;


UNEM(lm,z)=UR.l(lm);
pwage(lm,i,z)=pl.l(lm,i);

report1(z,'so2',i)=scoef_e('process',i)*qdout.l(i) ;
report1(z,'so2','fd')=scoef_e('process','fd')*sum(i,qc.l(i)) ;
report1(z,'so2','total')=sum(i,report1(z,'so2',i))+report1(z,'so2','fd') ;

report1(z,'NOX',i)=ncoef_e('process',i)*qdout.l(i) ;
report1(z,'NOX','fd')=ncoef_e('process','fd')*sum(i,qc.l(i)) ;
report1(z,'NOX','total')=sum(i,report1(z,'NOX',i))+report1(z,'NOX','fd') ;

report2(z,i)=sum(fe,ccoef_p(fe)*qin.l(fe,i)*(1-r_feed(fe,i)));
report2(z,"elec")=sum(sub_elec,sum(fe,ccoef_p(fe)*qin_ele.l(fe,sub_elec)));
report2(z,"fd")=sum(fe,ccoef_h(fe)*qc.l(fe));
report2(z,"Total")=sum(i,report2(z,i))+report2(z,"fd");

report3(z,sub_elec)=sum(fe,ccoef_p(fe)*qin_ele.l(fe,sub_elec)*(1-r_feed(fe,"elec")));

report6(z,lm,"total")= sum(i,qlin.l(lm,i))+sum(sub_elec,qlin_ele.l(lm,sub_elec));
report6(z,lm,i)= qlin.l(lm,i);
report6(z,lm,"elec")= sum(sub_elec,qlin_ele.l(lm,sub_elec));
report6(z,lm,sub_elec)= qlin_ele.l(lm,sub_elec);

$include %RepPath%\report_static

));
