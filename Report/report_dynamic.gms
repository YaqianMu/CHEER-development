* --
* -- PANDA - PRC Aggregate National Development Assessment Model
* --
* --           All rights reserved
* --
* --           David Roland-Holst, Samuel G. Evans
* --           Cecilia Han Springer, and MU Yaqian
* --
* --           Berkeley Energy and Resources, BEAR LLC
* --           1442A Walnut Street, Suite 108
* --           Berkeley, CA 94705 USA
* --
* --           Email: dwrh@berkeley.edu
* --           Tel: 510-220-4567
* --           Fax: 510-524-4591
* --
* --           October, 2016

* --

* -- postscn.gms
* --
* -- This file produces scnulation results in Excel compatible CSV files
* -- Two files are produced for each interval, a reportfile containing desired scnulation variables,
* --      and a samfile containing complete Social Accounting Matrices
* --


*=====================================generation of accounting scalar====================

*=====================================transfer to csv file================================

* ----- Output the results


put reportfile ;


* ----- Sectoral results

loop(i,
  put scn.tl,t.tl, 'output', i.tl, '','','','output', (qdout.l(i)),CHEER.modelstat / ;
    put scn.tl,t.tl, 'Sectoral price', i.tl, '','','','Sectoral price', (py.l(i)),CHEER.modelstat / ;
  loop(lm,
  put scn.tl,t.tl, 'employment', i.tl,'', '',lm.tl,'employment', (qlin.l(lm,i)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'Sectoral wage', i.tl,'', '',lm.tl,'Sectoral wage', (pl.l(lm,i)),CHEER.modelstat / ;
      );
) ;

* ----- electricity results
loop(sub_elec,
  put scn.tl,t.tl, 'elec_output', '','', sub_elec.tl,'','elec_output' , (qelec.l(sub_elec)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'elec_price', '','', sub_elec.tl,'','elec_price' , (pelec.l(sub_elec)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'elec_share', '','', sub_elec.tl,'','elec_share' , (report8("share",t,sub_elec)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'elec_tax', '','', sub_elec.tl,'','elec_tax' , (t_re.l(sub_elec)),CHEER.modelstat / ;
  loop(lm,
  put scn.tl,t.tl, 'employment','','', sub_elec.tl ,lm.tl,'employment', (qlin_ele.l(lm,sub_elec)),CHEER.modelstat / ;
      );
) ;

* ----- emission results

loop(i,
  put scn.tl,t.tl, 'ECO2', i.tl,'', '','','ECO2', (report2(i)),CHEER.modelstat / ;
) ;

put scn.tl,t.tl, 'ECO2', 'fd','', '','','ECO2', (report2("fd")),CHEER.modelstat / ;
*put scn.tl,t.tl, 'ECO2', '','fossil' ,'','','ECO2', (report2("fossil")),CHEER.modelstat / ;
*put scn.tl,t.tl, 'ECO2', '','cement' ,'','','ECO2', (report2("cement")),CHEER.modelstat / ;
put scn.tl,t.tl, 'ECO2', '','total' ,'','','ECO2', (report2("total")),CHEER.modelstat / ;

loop(i,
  put scn.tl,t.tl, 'ESO2', i.tl,'', '','','ESO2', (report1('so2',i)),CHEER.modelstat / ;
) ;

put scn.tl,t.tl, 'ESO2', 'fd','', '','','ESO2', (report1('so2',"fd")),CHEER.modelstat / ;
put scn.tl,t.tl, 'ESO2','' ,'total' ,'','','ESO2', (report1('so2',"total")),CHEER.modelstat / ;

loop(i,
  put scn.tl,t.tl, 'ENOX', i.tl,'', '','','ENOX', (report1('NOX',i)),CHEER.modelstat / ;
) ;

put scn.tl,t.tl, 'ENOX', 'fd','','' ,'','ENOX', (report1('NOX',"fd")),CHEER.modelstat / ;
put scn.tl,t.tl, 'ENOX', '','total' ,'','','ENOX', (report1('NOX',"total")),CHEER.modelstat / ;

loop(sub_elec,
  put scn.tl,t.tl, 'elec_CO2','','', sub_elec.tl ,'','elec_CO2' , (report3(sub_elec)),CHEER.modelstat / ;
) ;


* ----- employment results
loop(lm,
  put scn.tl,t.tl, 'unemployment','','' , '',lm.tl,'ur', (UR.l(lm)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'total employment','', '', '',lm.tl,'total employment', (tlabor_s0(lm)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'aggregated wage','','', '',lm.tl,'aggregated wage', (pls.l(lm)),CHEER.modelstat / ;

);

$ontext
* ----- energy results
loop(fe,
  put scn.tl,t.tl, 'Energy Consumption','','', fe.tl ,'','Billion Yuan' , (report12("Billion Yuan",t,fe)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'Energy Consumption','','', fe.tl ,'','coal equivalent calculation(Mt)' , (report12("coal equivalent calculation(Mt)",t,fe)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'Energy Consumption','','', fe.tl ,'','calorific value calculation(Mt)' , (report12("calorific value calculation(Mt)",t,fe)),CHEER.modelstat / ;
) ;

loop(sub_elec,
  put scn.tl,t.tl, 'Energy Consumption','','', sub_elec.tl ,'','Billion Yuan' , (report12("Billion Yuan",t,sub_elec)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'Energy Consumption','','', sub_elec.tl ,'','coal equivalent calculation(Mt)' , (report12("coal equivalent calculation(Mt)",t,sub_elec)),CHEER.modelstat / ;
  put scn.tl,t.tl, 'Energy Consumption','','', sub_elec.tl ,'','calorific value calculation(Mt)' , (report12("calorific value calculation(Mt)",t,sub_elec)),CHEER.modelstat / ;
) ;

put scn.tl,t.tl, 'Energy Total','','', '' ,'','coal equivalent calculation(Mt)' , (report12("coal equivalent calculation(Mt)",t,"Total")),CHEER.modelstat / ;
put scn.tl,t.tl, 'Energy non-fossil share','','', '' ,'','coal equivalent calculation(Mt)' , (report12("coal equivalent calculation(Mt)",t,"nfshare")),CHEER.modelstat / ;

put scn.tl,t.tl, 'Energy Total','','', '' ,'','calorific value calculation(Mt)' , (report12("calorific value calculation(Mt)",t,"Total")),CHEER.modelstat / ;
put scn.tl,t.tl, 'Energy non-fossil share','','', '' ,'','calorific value calculation(Mt)' , (report12("calorific value calculation(Mt)",t,"nfshare")),CHEER.modelstat / ;

$offtext

* ----- macro results
  put scn.tl,t.tl, 'GDP','', '', '','','GDP', rgdp.l,CHEER.modelstat / ;
  put scn.tl,t.tl, 'TFP','', '', '','','TFP', gprod.l,CHEER.modelstat / ;
  put scn.tl,t.tl, 'Welfare','', '', '','','Welfare', util.l,CHEER.modelstat / ;

*------- policy shock
loop(sub_elec,
  put scn.tl,t.tl, 'subelec0','', '', sub_elec.tl,'','subsidy', report4("subsidy",sub_elec),CHEER.modelstat / ;
  put scn.tl,t.tl, 'markup','', '', sub_elec.tl,'','markup', report4("markup",sub_elec),CHEER.modelstat / ;
  );
  put scn.tl,t.tl, 'clim0','', '', '','','clim0', clim0,CHEER.modelstat / ;
  put scn.tl,t.tl, 'tclim','', '', '','','tclim', tclim.l,CHEER.modelstat / ;
  put scn.tl,t.tl, 'tclim_ms','', '', '','','tclim_ms', tclim_ms.l,CHEER.modelstat / ;
  put scn.tl,t.tl, 'pco2_ms','', '', '','','carbon price', pco2_ms.l,CHEER.modelstat / ;
  put scn.tl,t.tl, 'pco2','', '', '','','carbon price', pco2.l,CHEER.modelstat / ;

loop(cm,
  put scn.tl,t.tl, 'clim_s','', '', cm.tl,'','clim_s', clim_s(cm),CHEER.modelstat / ;
);
