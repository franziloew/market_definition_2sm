
******************************
** Market 7: News Magazines **
******************************

/** Included Magazines: 			Focus
									DerSpiegel
									Stern 
 
 ** Time: 							2004w33 - 2006w33 
 */

 
**** Assign Globals ****
*************************

**** Sub1 
global y11list retailFOCUS
global x11list aboshareFOCUS

global y21list totaladsiteFOCUS


**** Sub2
global y12list retailDerSpiegel
global x12list aboshareDerSpiegel

global y22list totaladsiteDerSpiegel


****  Sub3
global y13list retailStern
global x13list aboshareStern

global y23list totaladsiteStern

* Graphical Analysis
tsline $x11list $x12list $x13list, xtitle("") ///
lw(medthick medium) ytitle("Share of subscription on total sales") ///    
legend(ring(0) position(1) row(2) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

tsline $y11list $y12list $y13list, xtitle("") ///
lw(medthick medium) ytitle("Weekly Retail Sales") ///    
legend(ring(0) position(1) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(circ_fss2)

tsline $y21list $y22list $y23list, xtitle("") ///
lw(medthick medium) ytitle("Total ad pages") ///    
legend(ring(0) position(4) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))    

graph2tex, epsfile(ads_fss2)


		**** 1) ARMA ****
*------------------------------------------------------------------------*
set more off
	*** Circulation ***
	*******************

arima $y11list $y12list $y13list, arima(1,0,1) r
estimates store fo_arima

predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y12list $y13list $y11list, arima(1,0,1) r
estimates store sp_arima

predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y13list $y12list $y11list, arima(1,0,1) r
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
lw(medthick medium) ytitle("Reader Market") ///    
legend(ring(0) position(1) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(circ_arma_fss)


drop e_sub1 e_sub2 e_sub3 


	*** Adsites ***
	***************
arima $y21list $y22list $y23list, arima(1,0,1) r
estimates store fo_arima2

predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y22list $y23list $y21list, arima(1,0,1) r
estimates store sp_arima2

predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y23list $y22list $y21list, arima(1,0,1) r 
estimates store st_arima2

predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_2)
xcorr e_sub1 e_sub3, lags(6) tab // gen(sub1_3_2)
xcorr e_sub3 e_sub2, lags(6) tab // gen(sub3_2_2)

****** Granger Causality ******
*******************************

quietly var e_sub1 e_sub2 e_sub3, lags(4)
vargranger

tsline e_sub1 e_sub2 e_sub3, xtitle("Ad Market") ///
lw(medthick medium) ytitle("Ad Market") ///    
legend(ring(0) position(1) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(ads_arma_fss)

drop e_sub1 e_sub2 e_sub3  

			**** 2) OLS regression ****
*----------------------------------------------------------------------------*

	*** Circulation A ***

reg $y11list $y12list $y13list, r
estimates store fo_ols

predict e_sub1, resid
corrgram e_sub1, lags(12)

reg $y12list $y11list $y13list, r
estimates store sp_ols

predict e_sub2, resid
corrgram e_sub2, lags(12)

reg $y13list $y12list $y11list, r
estimates store st_ols

predict e_sub3, resid
corrgram e_sub3, lags(12)

estimates stats fo_ols fo_arima sp_ols   ///
 sp_arima st_ols st_arima 


*** Crosscorrelation: Circulation A ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_3)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_3)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_3)

****** Granger Causality ******
*******************************

quietly var e_sub1 e_sub2 e_sub3, lags(1)
vargranger

tsline e_sub1 e_sub2 e_sub3, xtitle("Reader Market") ///
lw(medthick medium) ytitle("Reader Market") ///    
legend(ring(0) position(7) row(1) label(1 "Focus") label(2 "DerSpiegel") ///
label(3 "Stern"))

graph2tex, epsfile(circ_ols_fss)

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
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_4)
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
varsoc $y11list, maxlag(17)
pperron $y11list, trend lag(0)
pperron d.$y11list, trend lag(0)
dfuller $y11list, trend lag(0)
dfuller d.$y11list, trend lag(0)


* Sub2: Circulation
varsoc $y12list, maxlag(15)
pperron $y12list, trend lag(4)
pperron d.$y12list, trend lag(4)
dfuller $y12list, trend lag(4)
dfuller d.$y12list, trend lag(4)



* Sub3: Circulation
varsoc $y13list, maxlag(15) 
pperron $y13list, trend lag(2)
dfuller $y13list, trend lag(2)
dfuller d.$y13list, trend lag(2)


* Sub1: Adsite
varsoc $y21list, maxlag(15)
pperron $y21list , trend lag(2) 
pperron d.$y21list , trend lag(2) 
dfuller $y21list , trend lag(2) 
dfuller d.$y21list , trend lag(2) 

* Sub2: Adsite
varsoc $y22list, maxlag(15)
pperron $y22list , trend lag(2)
pperron d.$y22list , trend lag(2)
dfuller $y22list , trend lag(2)
dfuller d.$y22list , trend lag(2)

* Sub3: Adsite
varsoc $y23list, maxlag(15) // lag 1
pperron $y23list, trend lag(2)
pperron d.$y23list , trend lag(2)
dfuller $y23list, trend lag(2)
dfuller d.$y23list , trend lag(2)


***********************************
**** Testing for Cointegration ****
***********************************

*** Circulation ***
*******************

varsoc $y11list $y12list, maxlag(12) // lag 5

reg $y11list $y12list
predict e, resid
dfuller e, lags(5) // Variabels are not cointegrated
drop e

*** Adsites ***
***************

varsoc $y21list $y22list, maxlag(12) // lag 4

reg $y21list $y22list
predict e, resid
dfuller e, lags(4) // Variabels are not cointegrated
drop e


***************************************
**** Testing for Cross Correlation ****
***************************************

*** Wihtout prewhiting
** Circulation

xcorr $y11list $y12list, lags(12) xtitle("") ///
	ytitle("Cross Correlations of TV Movie / TV Spielfilm")
xcorr $y11list $y13list, lags(12) xtitle("") ///
	ytitle("Cross Correlations of TV Movie / TV Today")
xcorr $y13list $y12list, lags(12) xtitle("") ///
	ytitle("Cross Correlations of TV Today / TV Spielfilm")
	
** Ad-Sites

xcorr $y21list $y22list, lags(12) xtitle("") ///
	ytitle("Cross Correlations of TV Movie / TV Spielfilm")
xcorr $y21list $y23list, lags(12) xtitle("") ///
	ytitle("Cross Correlations of TV Movie / TV Today")
xcorr $y23list $y22list, lags(12) xtitle("") ///
	ytitle("Cross Correlations of TV Today / TV Spielfilm")
