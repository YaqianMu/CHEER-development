

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
IEE_id
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

$include %RepPath%\Post_par
$include %RepPath%\report_static
display report4,report9;

));



loop(sce$(ord(sce) ge 2 and ord(sce) le 3),

         loop(z$(ord(z) gt 1),

Switch_fee=1;

tax_s(sub_elec)=1-sce_s(sce,sub_elec);

ret0(sub_elec) =sce_s(sce,sub_elec)*(outputelec0(sub_elec)+1*rate(z))/outputelec0(sub_elec);;

$include CHEER.gen

solve CHEER using mcp;

display CHEER.modelstat, CHEER.solvestat,subelec0,rate,tax_s,ret0,ur.l,t_re.l;

$include %RepPath%\Post_par
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

$include %RepPath%\Post_par
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

$include %RepPath%\Post_par
$include %RepPath%\report_static

));
$offtext
