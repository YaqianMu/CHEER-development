

*===========================policy shock on static model========================

parameter Header Flag determining whether to output header records in output files ;
Header = 1 ;

*----- Declare the output file names

file reportfile / "Output/output.csv" / ;

* ----- Model output options
display "Begin output listing" ;

options limcol=000, limrow=000 ;
options solprint=on ;
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

DEE
IEE
TEE
TEE_DE
TEE_WE
;


set z reduction rate /1*5/  ;
parameter rate(z) /
1=0,
2=1
3=5,
4=15,
5=25
/;


*set sce reduction rate /BAU, ECF_n, ECF_w, ECF_s, ECF_h, ECF_b, LST_n, LST_w, LST_s, LST_h, LST_b, MRQ_n, MRQ_w, MRQ_s, MRQ_h, MRQ_b/  ;
set sce reduction rate /BAU, wind_ECF, solabor_ECF, wind_LST, solabor_LST,wind_MRQ, solabor_MRQ/  ;
set mapsce(sce,sub_elec)  /
wind_ECF        .        wind
solabor_ECF        .        solar
wind_LST        .        wind
solabor_LST        .        solar
wind_MRQ        .        wind
solabor_MRQ        .        solar
/ ;

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

display CHEER.modelstat, CHEER.solvestat,subelec0,rate,ur.l,t_re.l;

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
report4(z,lm,"elec") =sum(sub_elec, qlin_ele.l(lm,sub_elec))/sum(sub_elec,qelec.l(sub_elec));
report4(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec);
report4(z,"total",i) = sum(lm,qlin.l(lm,i))/qdout.l(i);
report4(z,"total","elec") =sum((lm,sub_elec), qlin_ele.l(lm,sub_elec))/sum(sub_elec,qelec.l(sub_elec));
report4(z,"total",sub_elec) = sum(lm,qlin_ele.l(lm,sub_elec))/qelec.l(sub_elec);

report3(z,sub_elec) = taxelec0(sub_elec)-t_re.l(sub_elec);

*==Direct employment impact
DEE(z,lm,gen)  = (qelec.l(gen)-outputelec0(gen))*laborelec0(lm,gen)/outputelec0(gen);


IEE(z,lm,gen)  =  (qelec.l(gen)-outputelec0(gen))*laborelec0(lm,gen)/outputelec0(gen)
                               - (qelec.l(gen)-outputelec0(gen))*sum(genn,laborelec0(lm,genn))/sum(genn,outputelec0(genn));

TEE(z,lm,i) = qlin.l(lm,i)-labor_q0(lm,i);
TEE(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec);
TEE(z,lm,"elec") = sum(sub_elec,qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec));
TEE(z,lm,"total") = sum(i,qlin.l(lm,i)-labor_q0(lm,i))- (qlin.l(lm,"elec")-labor_q0(lm,"elec"))+sum(sub_elec,qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec));


TEE_DE(z,lm,i) =  (qdout.l(i)-output0(i))*labor_q0(lm,i)/output0(i);
TEE_DE(z,lm,sub_elec) =  (qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec);
TEE_DE(z,lm,"elec") =  sum(sub_elec,(qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec));
TEE_DE(z,lm,"total") = sum(i, (qdout.l(i)-output0(i))*labor_q0(lm,i)/output0(i))
                    +sum(sub_elec,(qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec));

TEE_WE(z,lm,i) =  output0(i)*(qlin.l(lm,i)/qdout.l(i)-labor_q0(lm,i)/output0(i));
TEE_WE(z,lm,sub_elec) =  outputelec0(sub_elec)*(qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec)-laborelec0(lm,sub_elec)/outputelec0(sub_elec));
TEE_WE(z,lm,"elec") =  sum(sub_elec,TEE_WE(z,lm,sub_elec));
TEE_WE(z,lm,"total") = sum(i,TEE_WE(z,lm,i))
                    +sum(sub_elec,TEE_WE(z,lm,sub_elec));

$include %RepPath%\report_static


));



loop(sce$(ord(sce) ge 2 and ord(sce) le 3),

         loop(z$(ord(z) gt 1),

Switch_fee=1;

tax_s(sub_elec)=1-sce_s(sce,sub_elec);

ret0(sub_elec) =sce_s(sce,sub_elec)*(outputelec0(sub_elec)+1*rate(z))/outputelec0(sub_elec);;

$include CHEER.gen

solve CHEER using mcp;




UNEM(lm,z)=UR.l(lm);
pwage(lm,i,z)=pl.l(lm,i);

display CHEER.modelstat, CHEER.solvestat,subelec0,rate,tax_s,ret0,ur.l,t_re.l;

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
report4(z,lm,"elec") =sum(sub_elec, qlin_ele.l(lm,sub_elec))/sum(sub_elec,qelec.l(sub_elec));
report4(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec);
report4(z,"total",i) = sum(lm,qlin.l(lm,i))/qdout.l(i);
report4(z,"total","elec") =sum((lm,sub_elec), qlin_ele.l(lm,sub_elec))/sum(sub_elec,qelec.l(sub_elec));
report4(z,"total",sub_elec) = sum(lm,qlin_ele.l(lm,sub_elec))/qelec.l(sub_elec);

report3(z,sub_elec) = taxelec0(sub_elec)-t_re.l(sub_elec);

DEE(z,lm,gen)  = (qelec.l(gen)-outputelec0(gen))*laborelec0(lm,gen)/outputelec0(gen);


IEE(z,lm,gen)  =  (qelec.l(gen)-outputelec0(gen))*laborelec0(lm,gen)/outputelec0(gen)
                               - (qelec.l(gen)-outputelec0(gen))*sum(genn,laborelec0(lm,genn))/sum(genn,outputelec0(genn));

TEE(z,lm,i) = qlin.l(lm,i)-labor_q0(lm,i);
TEE(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec);
TEE(z,lm,"elec") = sum(sub_elec,qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec));
TEE(z,lm,"total") = sum(i,qlin.l(lm,i)-labor_q0(lm,i))- (qlin.l(lm,"elec")-labor_q0(lm,"elec"))+sum(sub_elec,qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec));

TEE_DE(z,lm,i) =  (qdout.l(i)-output0(i))*labor_q0(lm,i)/output0(i);
TEE_DE(z,lm,sub_elec) =  (qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec);
TEE_DE(z,lm,"elec") =  sum(sub_elec,(qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec));
TEE_DE(z,lm,"total") = sum(i, (qdout.l(i)-output0(i))*labor_q0(lm,i)/output0(i))
                                 +sum(sub_elec,(qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec));

TEE_WE(z,lm,i) =  output0(i)*(qlin.l(lm,i)/qdout.l(i)-labor_q0(lm,i)/output0(i));
TEE_WE(z,lm,sub_elec) =  outputelec0(sub_elec)*(qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec)-laborelec0(lm,sub_elec)/outputelec0(sub_elec));
TEE_WE(z,lm,"elec") =  sum(sub_elec,TEE_WE(z,lm,sub_elec));
TEE_WE(z,lm,"total") = sum(i,TEE_WE(z,lm,i))
                                 +sum(sub_elec,TEE_WE(z,lm,sub_elec));

$include %RepPath%\report_static

));


loop(sce$(ord(sce) ge 4 and ord(sce) le 5),

         loop(z$(ord(z) gt 1),

Switch_fee=0;
tax_s(sub_elec)=1-sce_s(sce,sub_elec);

ret0(sub_elec) =sce_s(sce,sub_elec)*(outputelec0(sub_elec)+1*rate(z))/outputelec0(sub_elec);

$include CHEER.gen

solve CHEER using mcp;

display CHEER.modelstat, CHEER.solvestat,subelec0,rate,tax_s,ur.l,t_re.l;


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
report4(z,lm,"elec") =sum(sub_elec, qlin_ele.l(lm,sub_elec))/sum(sub_elec,qelec.l(sub_elec));
report4(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec);
report4(z,"total",i) = sum(lm,qlin.l(lm,i))/qdout.l(i);
report4(z,"total","elec") =sum((lm,sub_elec), qlin_ele.l(lm,sub_elec))/sum(sub_elec,qelec.l(sub_elec));
report4(z,"total",sub_elec) = sum(lm,qlin_ele.l(lm,sub_elec))/qelec.l(sub_elec);

report3(z,sub_elec) = taxelec0(sub_elec)-t_re.l(sub_elec);

DEE(z,lm,gen)  = (qelec.l(gen)-outputelec0(gen))*laborelec0(lm,gen)/outputelec0(gen);


IEE(z,lm,gen)  =  (qelec.l(gen)-outputelec0(gen))*laborelec0(lm,gen)/outputelec0(gen)
                               - (qelec.l(gen)-outputelec0(gen))*sum(genn,laborelec0(lm,genn))/sum(genn,outputelec0(genn));

TEE(z,lm,i) = qlin.l(lm,i)-labor_q0(lm,i);
TEE(z,lm,sub_elec) = qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec);
TEE(z,lm,"elec") = sum(sub_elec,qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec));
TEE(z,lm,"total") = sum(i,qlin.l(lm,i)-labor_q0(lm,i))- (qlin.l(lm,"elec")-labor_q0(lm,"elec"))+sum(sub_elec,qlin_ele.l(lm,sub_elec)-laborelec0(lm,sub_elec));

TEE_DE(z,lm,i) =  (qdout.l(i)-output0(i))*labor_q0(lm,i)/output0(i);
TEE_DE(z,lm,sub_elec) =  (qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec);
TEE_DE(z,lm,"elec") =  sum(sub_elec,(qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec));
TEE_DE(z,lm,"total") = sum(i, (qdout.l(i)-output0(i))*labor_q0(lm,i)/output0(i))
                                  +sum(sub_elec,(qelec.l(sub_elec)-outputelec0(sub_elec))*laborelec0(lm,sub_elec)/outputelec0(sub_elec));

TEE_WE(z,lm,i) =  output0(i)*(qlin.l(lm,i)/qdout.l(i)-labor_q0(lm,i)/output0(i));
TEE_WE(z,lm,sub_elec) =  outputelec0(sub_elec)*(qlin_ele.l(lm,sub_elec)/qelec.l(sub_elec)-laborelec0(lm,sub_elec)/outputelec0(sub_elec));
TEE_WE(z,lm,"elec") =  sum(sub_elec,TEE_WE(z,lm,sub_elec));
TEE_WE(z,lm,"total") = sum(i,TEE_WE(z,lm,i))
                                  +sum(sub_elec,TEE_WE(z,lm,sub_elec));


$include %RepPath%\report_static

));

$ontext
loop(sce$(ord(sce) ge 6 and ord(sce) le 7),
*loop(sce$(ord(sce) eq 7 ),

         loop(z$(ord(z) gt 1),
 Switch_fee=1;
 tax_s(sub_elec)=1;


         loop(sub_elec,
        if(sce_s(sce,sub_elec) ne 0,

        yelec.fx(sub_elec) = 1+sce_s(sce,sub_elec)*(outputelec0(sub_elec)+1*rate(z))/outputelec0(sub_elec);
        else
        yelec.lo(sub_elec) = 0;
        yelec.up(sub_elec) = +INF;
        );
        );

$include CHEER.gen

solve CHEER using mcp;

display CHEER.modelstat, CHEER.solvestat,subelec0,rate,tax_s,pco2.l,ur.l,t_re.l;


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
$offtext
