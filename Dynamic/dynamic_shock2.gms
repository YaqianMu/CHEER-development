*----------------------------------------------*
*Dynamic process
*2015年12月7日 5年一计算，劳动力分类,劳动力供给比例动态化
*12月11日，修改劳动力增长的迭代方式，新增tqlabor参数表示劳动力供给数量，由于考虑了
*工资差异，fact（“labor”）表示的是劳动力总报酬，而非数量
*12月16日，TFP，AEEI都先假设为0
*12/26   tfp=0.06  aeei=-0.01
*03/14   tfp和aeei通过core加入模型    电力行业0.3%，其他行业1% from EPPA 6
*03/17   calibration和baseline分开
*03/28   整合ff供给/发电量的趋势
*0419    report一次能源消费
*1207 add learning curve
*----------------------------------------------*
simu_s =1;
tax_s =1;
re_s=1;

*== switch for learning curve *=on
bata(sub_elec)$(1-switch_learn) = 0;

options solprint=on ;

loop(scn$(pscn2(scn)),

  loop(t$(ord(t) le card(t)),

*== drivers of dynamic simulation=====
fact("capital") = fact_supp("capital",t);

tqlabor_s0 = fact_supp("labor",t);
*same population share as base year
tlabor_s0(lm)                           =fact_supp("labor",t)*tlprop("2012",lm);
*change population share
*tlabor_s0(lm)                           =tqlabor_s0*tlprop(t+1,lm);
*ur.lo(lm)=0.85*ur.l(lm);

aeei(s)=AEEI_t(s,t);

xscale = xscale_t(t);

gprod0$(simu_s ne 0)=gprod_b(t);

rgdp0$(simu_s eq 0)=rgdp_b(t);

ret0(sub_elec)=renewable_scn(t,sub_elec,scn);

subelec0(sub_elec)= -subsidy_h(t,sub_elec)+taxelec0(sub_elec);

*==============parameter for policy shock===============
clim=clim_scn(scn);
clim_ms=clim_ms_scn(scn);
clim_m(s) = clim_m_scn(s,scn);
clim0= clim_trend_scn(t,scn);

price_co2$(clim_ms eq 1)=price_co2_t(t);

tax_s=tax_scn(scn);

*==== higher subsidy for renewable

*taxelec0(sub_elec)$(wsb(sub_elec) and ord(t) eq 2)=100*taxelec0(sub_elec);
*taxelec0("wind")$(ord(t) eq 2)=100*taxelec0("wind");
*taxelec0("solar")$(ord(t) eq 2)=100*taxelec0("solar");
*taxelec0("biomass")$(ord(t) eq 2)=100*taxelec0("biomass");

*==code for learning curve

mkup_t(t,sub_elec)$(ord(t) eq 1) = ((elecout_t(t,sub_elec))/elecout_t("2012",sub_elec))**bata(sub_elec)*mkup_t("2012",sub_elec) ;

mkup_t(t,sub_elec)$(ord(t) gt 1) = ((elecout_t(t-1,sub_elec))/elecout_t("2012",sub_elec))**bata(sub_elec)*mkup_t("2012",sub_elec) ;

emkup(sub_elec)=mkup_t(t,sub_elec);


$include CHEER.gen



solve CHEER using mcp;

display CHEER.modelstat, CHEER.solvestat,clim,cquota,t,fact;



*-------------
*sore results
*-------------

*stocks

invest_k(t)                  =   grossinvk.l;
kstock(t)$(ord(t)=1)         =   kstock0;

*$ontext
report1('so2',i)=scoef_e('process',i)*qdout.l(i) ;
report1('so2','fd')=scoef_e('process','fd')*sum(i,qc.l(i)) ;
report1('so2','total')=sum(i,report1('so2',i))+report1('so2','fd') ;

report1('NOX',i)=ncoef_e('process',i)*qdout.l(i) ;
report1('NOX','fd')=ncoef_e('process','fd')*sum(i,qc.l(i)) ;
report1('NOX','total')=sum(i,report1('NOX',i))+report1('NOX','fd') ;

report2(i)=sum(fe,ccoef_p(fe)*qin.l(fe,i)*(1-r_feed(fe,i)))+ccoef_pro(i)*qdout.l(i);
report2("elec")=sum(sub_elec,sum(fe,ccoef_p(fe)*qin_ele.l(fe,sub_elec)))-sum(fe,ccoef_p(fe)*qin_ele.l(fe,"nuclear"));
report2("fd")=sum(fe,ccoef_h(fe)*qc.l(fe));
report2("Total")=sum(i,report2(i))+report2("fd");

report3(sub_elec)=sum(fe,ccoef_p(fe)*qin_ele.l(fe,sub_elec)*(1-r_feed(fe,"elec")));

report4("subsidy",sub_elec)= subelec0(sub_elec);
report4("markup",sub_elec)= emkup(sub_elec);

report5(t)=   pcons.l*grosscons.l+pinv.l*grossinvk.l+sum(i,py.l(i)*((nx0(i)+xinv0(i)+xcons0(i))*xscale));

report6(lm,"total")= sum(i,qlin.l(i,lm))+sum(sub_elec,qlin_ele.l(sub_elec,lm));
report6(lm,i)= qlin.l(i,lm);
report6(lm,"elec")= sum(sub_elec,qlin_ele.l(sub_elec,lm));
report6(lm,sub_elec)= qlin_ele.l(sub_elec,lm);

report8("output",t,sub_elec)=qelec.l(sub_elec);
report8("output",t,"Total")=sum(sub_elec,report8("output",t,sub_elec));
report8("share",t,sub_elec)$(not wse(sub_elec))=qelec.l(sub_elec)/report8("output",t,"Total");
report8("share",t,sub_elec)$(wse(sub_elec))=qelec.l(sub_elec)/report8("output",t,"Total");

report12("Billion Yuan",t,e)=qc.l(e)+sum(j,qin.l(e,j))+sum(sub_elec,qin_ele.l(e,sub_elec));
report12("Billion Yuan",t,sub_elec)$(cfe(sub_elec))=report12("Billion Yuan",t,"elec")*report8("share",t,sub_elec);
report12("coal equivalent calculation(Mt)",t,fe)=eet1(fe)/100*report12("Billion Yuan",t,fe)/report12("Billion Yuan","2012",fe);
report12("coal equivalent calculation(Mt)",t,sub_elec)$(cfe(sub_elec))=eet1(sub_elec)/100*report12("Billion Yuan",t,sub_elec)/report12("Billion Yuan","2012",sub_elec);
report12("coal equivalent calculation(Mt)",t,"Total")=sum(fe,report12("coal equivalent calculation(Mt)",t,fe))+sum(sub_elec$(cfe(sub_elec)),report12("coal equivalent calculation(Mt)",t,sub_elec));
report12("coal equivalent calculation(Mt)",t,"nfshare")=sum(sub_elec$(cfe(sub_elec)),report12("coal equivalent calculation(Mt)",t,sub_elec))/report12("coal equivalent calculation(Mt)",t,"Total");
report12("calorific value calculation(Mt)",t,fe)=eet2(fe)/100*report12("Billion Yuan",t,fe)/report12("Billion Yuan","2012",fe);
report12("calorific value calculation(Mt)",t,sub_elec)$(cfe(sub_elec))=eet2(sub_elec)/100*report12("Billion Yuan",t,sub_elec)/report12("Billion Yuan","2012",sub_elec);
report12("calorific value calculation(Mt)",t,"Total")=sum(fe,report12("calorific value calculation(Mt)",t,fe))+sum(sub_elec$(cfe(sub_elec)),report12("calorific value calculation(Mt)",t,sub_elec));
report12("calorific value calculation(Mt)",t,"nfshare")=sum(sub_elec$(cfe(sub_elec)),report12("calorific value calculation(Mt)",t,sub_elec))/report12("calorific value calculation(Mt)",t,"Total");



qlin.l("elec",lm)=sum(sub_elec,qlin_ele.l(sub_elec,lm));

*------------------
*update endowments
*------------------

jk(t)$(invest_k(t)/kstock(t) ge zetak)  = kstock(t)/betak*
                                           (betak*zetak-1+sqrt(1+2*betak*(invest_k(t)/kstock(t)-zetak)));
jk(t)$(invest_k(t)/kstock(t) le zetak)  = invest_k(t);


kstock(t+1)$(ord(t) eq 1)                  =3*jk(t)+(1-deltak)**3*kstock(t);
kstock(t+1)$(ord(t) gt  1)                  =5*jk(t)+(1-deltak)**5*kstock(t);

fact_supp("capital",t+1)                         =rork0*kstock(t+1);


*=======labor  update =========
*need population analysis                   2005-2014年均人口增长率为0.005019 劳动参与率平均为0.58   劳动参与率恒定时，人口增长率与劳动供给增长率相等
*tqlabor_s0$(ord(t) eq 1)                    =tqlabor_s0*(1+lgrowth_b(t+1))**3;
*tqlabor_s0$(ord(t) gt  1)                    =tqlabor_s0*(1+lgrowth_b(t+1))**5;

fact_supp("labor",t+1)  $(ord(t) eq 1)                    =fact_supp("labor",t)  *(1+lgrowth_b(t+1))**3;
fact_supp("labor",t+1)  $(ord(t) gt 1)                     =fact_supp("labor",t)  *(1+lgrowth_b(t+1))**5;


*=======energy effiency  update =========

*aeei(i)$(not elec(i))          =  aeei(i)/(1+0.01)**5;
*aeei("fd")       =  aeei("fd")/(1+0.01)**5;
*aeei("elec") =  aeei("elec")/(1+0.01)**5;

AEEI_t(i,t+1)$(ord(t) eq 1)  = AEEI_t(i,t)/(1+0.01)**3;
AEEI_t("fd",t+1)$(ord(t) eq 1)  =  AEEI_t("fd",t)/(1+0.01)**3;
AEEI_t("elec",t+1)$(ord(t) eq 1)  =  AEEI_t("elec",t)/(1+0.01)**3;

AEEI_t(i,t+1)$(ord(t) gt 1)  =  AEEI_t(i,t)/(1+0.01)**5;
AEEI_t("fd",t+1)$(ord(t) gt 1)  =  AEEI_t("fd",t)/(1+0.01)**5;
AEEI_t("elec",t+1)$(ord(t) gt 1)  =  AEEI_t("elec",t)/(1+0.01)**5;

*=======exogenous trade balance  =========
*trade imbalance and stock change phased out at 1% per year
xscale_t(t+1)$(ord(t) eq 1)                                  =0.99**(3*(ord(t)));
xscale_t(t+1)$(ord(t) gt 1)                                   =0.99**(5*(ord(t)));

*=======electricity Output===============
elecout_t(t,sub_elec)=qelec.l(sub_elec);

*=======exogenous carbon price==========
*price_co2_t(t)= 0;
price_co2_t(t)$(pscna(scn)) = pco2.l;

$include %RepPath%/report_dynamic

display tqlabor_s0,tlabor_s0,cquota,rgdp.l,gprod.l,emkup,fact,price_co2_t;
  );
  if(pscna(scn),
  PAT(t,"PCO2")$(pscna(scn))=price_co2_t(t);
  execute_unload "trend.gdx" PAT
  execute 'gdxxrw.exe trend.gdx par=PAT rng=A1:E18';)
  );

display fact_supp,AEEI_t,xscale_t,mkup_t,elecout_t,price_co2_t;
