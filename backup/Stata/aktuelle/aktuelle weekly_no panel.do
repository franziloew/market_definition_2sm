log using "C:\Users\Jorgito\Google Drive/HSU/Paper/2sm/Stata/Frauenmagazine/bunte gala.txt", text replace

****************************
** Market 1: Entertaining **
****************************

**** Assign Globals ****
*************************

**** Sub1 
global y11list retailBUNTE
global y11listlog log_retailBUNTE
global x11list aboBUNTE contentBUNTE

global y21list totaladsiteBUNTE
global y21listlog log_totaladsiteBUNTE


**** Sub2
global y12list retailGala
global y12listlog log_retailGala
global x12list aboGala contentGala

global y22list totaladsiteGala
global y22lisllog log_totaladsiteGala

****  Sub3
global y13list retailFrauimSpiegel
global y13listlog log_retailFrauimSpiegel
global x13list contentFrauimSpiegel

global y23list totaladsiteFrauimSpiegel
global y23listlog log_totaladsiteFrauimSpiegel

****  Sub4
global y14list retailNEUEPOST
global y14listlog log_retailNEUEPOST
global x14list contentNEUEPOST

global y24list totaladsiteNEUEPOST
global y24listlog log_totaladsiteNEUEPOST

****  Sub5
global y15list retailLaura
global y15listlog log_retailLaura
global x15list contentLaura

global y25list totaladsiteLaura
global y25listlog log_totaladsiteLaura

****  Sub6
global y16list retailBildderFrau
global y16listlog log_retailBildderFrau
global x16list contentBildderFrau

global y26list totaladsiteBildderFrau
global y26listlog log_totaladsiteBildderFrau

*** Time Var
		
global timelist1 quad week logtime
global timelist2 quad logtime
global timelist3 week logtime


* Graphical Analysis
tsline $y11list $y12list $y13list $y14list, ///
xtitle("") ///
lw(medthick medium) ytitle("Weekly Sales") ///    
legend(ring(0) position(3) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "FrauimSpiegel") label(4 "NEUEPOST"))

graph2tex, epsfile(circ_frauen)

tsline $y21list $y22list $y23list $y24list, xtitle("") ///
lw(medthick medium) ytitle("Total sites per Copy") ///    
legend(ring(0) position(11) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "FrauimSpiegel") label(4 "NEUEPOST")) 

graph2tex, epsfile(ads_frauen)


		**** 1) ARMA ****
*------------------------------------------------------------------------*

	*** Circulation ***
	*******************

arima $y11list $y12list $y13list $y14list, arima(2,1,0)  
predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y12list $y11list $y13list $y14list, arima(2,1,0) 
predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y13list $y12list $y11list $y14list, arima(2,1,0) 
predict e_sub3, resid
corrgram e_sub3, lags(12)

arima $y14list $y12list $y13list $y11list, arima(2,1,0) 
predict e_sub4, resid
corrgram e_sub4, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_1)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_1)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_1)
xcorr e_sub3 e_sub4, lags(6) tab gen(sub3_4_1)
xcorr e_sub2 e_sub4, lags(6) tab gen(sub2_4_1)
xcorr e_sub1 e_sub4, lags(6) tab gen(sub1_4_1)

tsline e_sub1 e_sub2 e_sub3 e_sub4, xtitle("") ///
lw(medthick medium) ytitle("") ///    
legend(ring(0) position(1) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "FrauimSpiegel") label(4 "NEUEPOST"))

graph2tex, epsfile(circ_arma_frauen)

drop e_sub1 e_sub2 e_sub3 e_sub4


	*** Adsites ***
	***************
arima $y21list $y22list $y23list $y24list, arima(1,1,1)
predict e_sub1, resid

arima $y22list $y21list $y23list $y24list, arima(1,1,1) 
predict e_sub2, resid

arima $y23list $y22list $y21list $y24list, arima(1,1,1) 
predict e_sub3, resid

arima $y24list $y23list $y22list $y21list, arima(1,1,1) 
predict e_sub4, resid

*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_2)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_2)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_2)
xcorr e_sub3 e_sub4, lags(6) tab gen(sub3_4_2)
xcorr e_sub2 e_sub4, lags(6) tab gen(sub2_4_2)
xcorr e_sub1 e_sub4, lags(6) tab gen(sub1_4_2)

tsline e_sub1 e_sub2 e_sub3 e_sub4, xtitle("") ///
lw(medthick medium) ytitle("") ///    
legend(ring(0) position(1) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "FrauimSpiegel") label(4 "NEUEPOST"))

graph2tex, epsfile(ads_arma_frauen)

drop e_sub1 e_sub2 e_sub3 e_sub4

			**** 2) OLS regression ****
*----------------------------------------------------------------------------*

	*** Circulation A ***
	
reg $y11list $y12list $y13list $y14list
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg $y12list $y11list $y13list $y14list
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg  $y13list $y12list $y11list $y14list
predict e_sub3, resid
corrgram e_sub3, lags(12)

reg  $y14list $y13list $y12list $y11list
predict e_sub4, resid
corrgram e_sub4, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_3)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_3)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_3)
xcorr e_sub3 e_sub4, lags(6) tab gen(sub3_4_3)
xcorr e_sub2 e_sub4, lags(6) tab gen(sub2_4_3)
xcorr e_sub1 e_sub4, lags(6) tab gen(sub1_4_3)

tsline e_sub1 e_sub2 e_sub3 e_sub4, xtitle("") ///
lw(medthick medium) ytitle("") ///    
legend(ring(0) position(1) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "FrauimSpiegel") label(4 "NEUEPOST"))

graph2tex, epsfile(circ_ols_frauen)

drop e_sub1 e_sub2 e_sub3 e_sub4 

	*** Adsites A ***
	
reg $y21list $y22list $y23list $y24list
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg $y22list $y21list $y23list $y24list
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg $y23list $y21list $y22list $y24list 
predict e_sub3, resid
corrgram e_sub3, lags(12)

reg $y24list $y21list $y22list $y23list 
predict e_sub4, resid
corrgram e_sub4, lags(12)


xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_4)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_4)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_4)
xcorr e_sub3 e_sub4, lags(6) tab gen(sub3_4_4)
xcorr e_sub2 e_sub4, lags(6) tab gen(sub2_4_4)
xcorr e_sub1 e_sub4, lags(6) tab gen(sub1_4_4)

tsline e_sub1 e_sub2 e_sub3 e_sub4, xtitle("") ///
lw(medthick medium) ytitle("") ///    
legend(ring(0) position(1) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "FrauimSpiegel") label(4 "NEUEPOST"))

graph2tex, epsfile(ads_ols_frauen)

drop e_sub1 e_sub2 e_sub3 e_sub4 


****** DIAGNOSTICS ********
***************************


*******************
**** Unit Root ****
*******************

* Sub1: Circulation
varsoc $y11list, maxlag(17)
pperron $y11list, trend lag(1)
pperron d.$y11list, trend lag(1)
dfuller $y11list, trend lag(1)
dfuller d.$y11list, trend lag(1)


* Sub2: Circulation
varsoc $y12list, maxlag(15)
pperron $y12list, trend lag(3)
pperron d.$y12list, trend lag(3)
dfuller $y12list, trend lag(3)
dfuller d.$y12list, trend lag(3)



* Sub3: Circulation
varsoc $y13list, maxlag(15) 
pperron $y13list, trend lag(2)
dfuller $y13list, trend lag(2)
dfuller d.$y13list, trend lag(2)


* Sub1: Adsite
varsoc $y21list, maxlag(15)
pperron $y21list , trend lag(6) 
pperron d.$y21list , trend lag(6) 
dfuller $y21list , trend lag(6) 
dfuller d.$y21list , trend lag(6) 

* Sub2: Adsite
varsoc $y22list, maxlag(15)
pperron $y22list , trend lag(1)
pperron d.$y22list , trend lag(1)
dfuller $y22list , trend lag(1)
dfuller d.$y22list , trend lag(1)

* Sub3: Adsite
varsoc $y23list, maxlag(15) // lag 1
pperron $y23list, trend lag(1)
pperron d.$y23list , trend lag(1)
dfuller $y23list, trend lag(1)
dfuller d.$y23list , trend lag(1)


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
