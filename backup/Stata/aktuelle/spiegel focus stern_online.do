
******************************
** Market 7: News Magazines **
******************************

/** Included Magazines: 			Focus
									DerSpiegel
									Stern 
 
 ** Time: 							
 */

 
**** Assign Globals ****
*************************

**** Sub1 
global y11list visitsFOCUS

global y21list FOCUS


**** Sub2
global y12list visitsSpiegel

global y22list Spiegel


****  Sub3
global y13list visitsStern

global y23list Stern

* Graphical Analysis


tsline $y11list $y12list $y13list, xtitle("") ///
lw(medthick medium) ytitle("Visits") ///    
legend(ring(0) position(1) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))


tsline $y21list $y22list $y23list, xtitle("") ///
lw(medthick medium) ytitle("Total ad pages") ///    
legend(ring(0) position(4) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))    

graph2tex, epsfile(ads_fss2)

****** DIAGNOSTICS ********
***************************

******************
**** Plotting ****
******************

pac $y11list
ac $y11list

pac $y12list
ac $y12list

pac $y13list
ac $y13list

pac $y21list
ac $y21list

pac $y22list
ac $y22list

pac $y23list
ac $y23list

*******************
**** Unit Root ****
*******************

* Sub1: Circulation
varsoc $y11list, maxlag(15)
pperron $y11list, trend lag(0)
dfuller $y11list, trend lag(0)

* Sub2: Circulation
varsoc $y12list, maxlag(15)
pperron $y12list, trend lag(2)
dfuller $y12list, trend lag(2)

* Sub3: Circulation
varsoc $y13list, maxlag(15) 
pperron $y13list, trend lag(6)
dfuller $y13list, trend lag(6)
dfuller d.$y13list, trend lag(6)

* Sub1: Adsite
varsoc $y21list, maxlag(15)
pperron $y21list , trend lag(1) 
pperron d.$y21list , trend lag(1) 
dfuller $y21list , trend lag(1) 
dfuller d.$y21list , trend lag(1) 


* Sub2: Adsite
varsoc $y22list, maxlag(15)
pperron $y22list , trend lag(2)
dfuller $y22list , trend lag(2)
dfuller d.$y22list , trend lag(2)

* Sub3: Adsite
varsoc $y23list, maxlag(15) 
pperron $y23list, trend lag(2)
dfuller $y23list, trend lag(2)
dfuller d.$y23list, trend lag(2)


		**** 1) ARIMA ****
*------------------------------------------------------------------------*
set more off
	*** Circulation ***
	*******************

arima $y11list $y12list $y13list, arima(1,0,0) r
estimates store fo_arima

predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y12list $y13list $y11list, arima(1,0,0) r
estimates store sp_arima

predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y13list $y12list $y11list, arima(1,0,0) r
estimates store st_arima

predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_1)
xcorr e_sub1 e_sub3, lags(6) tab // gen(sub1_3_1)
xcorr e_sub3 e_sub2, lags(6) tab // gen(sub3_2_1)

****** Granger Causality ******
*******************************

quietly var e_sub1 e_sub2 e_sub3, lags(1)
vargranger


tsline e_sub1 e_sub2 e_sub3, xtitle("") ///
lw(medthick medium) ytitle("Visits") ///    
legend(ring(0) position(5) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(circ_arma_fss2)


drop e_sub1 e_sub2 e_sub3 


	*** Adsites ***
	***************
arima $y21list $y22list $y23list, arima(1,1,4) r
estimates store fo_arima2

predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y22list $y23list $y21list, arima(1,1,4) r
estimates store sp_arima2

predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y23list $y22list $y21list, arima(1,1,4) r 
estimates store st_arima2

predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_2)
xcorr e_sub1 e_sub3, lags(6) tab // gen(sub1_3_2)
xcorr e_sub3 e_sub2, lags(6) tab // gen(sub3_2_2)

****** Granger Causality ******
*******************************

quietly var e_sub1 e_sub2 e_sub3, lags(1)
vargranger

tsline e_sub1 e_sub2 e_sub3, xtitle("Ad Market") ///
lw(medthick medium) ytitle("Ad Market") ///    
legend(ring(0) position(1) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(ads_arma_fss2)

drop e_sub1 e_sub2 e_sub3  

			**** 2) OLS regression ****
*----------------------------------------------------------------------------*

	*** Circulation A ***

reg $y11list $y12list $y13list, r
estimates store fo_ols

predict e_sub1, resid
corrgram e_sub1, lags(6)

reg $y12list $y11list $y13list, r
estimates store sp_ols

predict e_sub2, resid
corrgram e_sub2, lags(6)

reg $y13list $y12list $y11list, r
estimates store st_ols

predict e_sub3, resid
corrgram e_sub3, lags(6)

estimates stats fo_ols fo_arima sp_ols   ///
 sp_arima st_ols st_arima 


*** Crosscorrelation: Circulation A ***
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_3)
xcorr e_sub1 e_sub3, lags(6) tab // gen(sub1_3_3)
xcorr e_sub3 e_sub2, lags(6) tab // gen(sub3_2_3)

****** Granger Causality ******
*******************************

quietly var e_sub1 e_sub2 e_sub3, lags(1)
vargranger

tsline e_sub1 e_sub2 e_sub3, xtitle("Reader Market") ///
lw(medthick medium) ytitle("Reader Market") ///    
legend(ring(0) position(7) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(circ_ols_fss2)

drop e_sub1 e_sub2 e_sub3  

	*** Adsites A ***
	
reg $y21list $y22list $y23list, r
estimates store fo_ols2

predict e_sub1, resid
corrgram e_sub1, lags(12)

reg $y22list $y21list $y23list, r
estimates store sp_ols2
 
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg $y23list $y22list $y21list, r 
estimates store st_ols2

predict e_sub3, resid
corrgram e_sub3, lags(12)

estimates stats fo_ols2 fo_arima2 sp_ols2 sp_arima2 st_ols2  ///
  st_arima2

*** Crosscorrelation: Circulation A ***
xcorr e_sub1 e_sub2, lags(6) tab //gen(sub1_2_4)
xcorr e_sub1 e_sub3, lags(6) tab //gen(sub1_3_4)
xcorr e_sub3 e_sub2, lags(6) tab //gen(sub3_2_4)

****** Granger Causality ******
*******************************

quietly var e_sub1 e_sub2 e_sub3, lags(1)
vargranger

tsline e_sub1 e_sub2 e_sub3, xtitle("Ad Market") ///
lw(medthick medium) ytitle("Ad Market") ///    
legend(ring(0) position(4) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(ads_ols_fss2)

drop e_sub1 e_sub2 e_sub3 
