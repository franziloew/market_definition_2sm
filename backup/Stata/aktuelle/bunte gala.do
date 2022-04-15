log using "C:\Users\Jorgito\Google Drive/HSU/Paper/2sm/Stata/Frauenmagazine/bunte gala.txt", text replace

****************************
** Market 1: Entertaining **
****************************

**** Assign Globals ****
*************************

**** Sub1 
global y11list retrailBUNTE
global y11listlog log_retailBUNTE
global x11list aboBUNTE contentBUNTE

global y21list totaladsiteBUNTE
global y21listlog log_totaladsiteBUNTE


**** Sub2
global y12list retrailGala
global y12listlog log_retailGala
global x12list aboGala contentGala

global y22list totaladsiteGala
global y22lisllog log_totaladsiteGala


****  Sub3
global y13list retrailBildderFrau
global y13listlog log_retrailBildderFrau
global x13list contentBildderFrau

global y23list totaladsiteBildderFrau
global y23listlog log_totaladsiteBildderFrau

****  Sub4
global y14list retrailFrauimSpiegel
global y14listlog log_retrailFrauimSpiegel
global x14list contentFrauimSpiegel

global y24list totaladsiteFrauimSpiegel
global y24listlog log_totaladsiteFrauimSpiegel

****  Sub5
global y15list retrailLaura
global y15listlog log_retrailLaura
global x15list contentLaura

global y25list totaladsiteLaura
global y25listlog log_totaladsiteLaura

****  Sub6
global y16list retrailNEUEPOST
global y16listlog log_retrailNEUEPOST
global x16list contentNEUEPOST

global y26list totaladsiteNEUEPOST
global y26listlog log_totaladsiteNEUEPOST

*** Time Var
		
global timelist1 quad week logtime
global timelist2 quad logtime
global timelist3 week logtime


* Graphical Analysis
tsline $y11list $y12list $y13list $y14list $y15list $y16list, ///
xtitle("") ///
lw(medthick medium) ytitle("Weekly Sales") ///    
legend(ring(0) position(1) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "Bild der Frau") label(4 "FrauimSpiegel") label(5 "Laura") ///
label(6 "NEUEPOST")) 

tsline $y21list $y22list $y23list $y24list $y25list $y26list, xtitle("") ///
lw(medthick medium) ytitle("Total sites per Copy") ///    
legend(ring(0) position(11) row(2) label(1 "Bunte") label(2 "Gala") ///
label(3 "Bild der Frau") label(4 "FrauimSpiegel") label(5 "Laura") ///
label(6 "NEUEPOST")) 


		**** 1) ARMA ****
*------------------------------------------------------------------------*

	*** Circulation ***
	*******************

arima $y11list $y12list $y13list $y14list $y15list $y16list, arima(2,1,0) 
predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y12list $y11list $y13list $y14list $y15list $y16list, arima(2,1,0)
predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y13list $y12list $y11list $y14list $y15list $y16list, arima(2,1,0) 
predict e_sub3, resid
corrgram e_sub3, lags(12)

arima $y14list $y12list $y11list $y13list $y15list $y16list, arima(2,1,0) 
predict e_sub4, resid
corrgram e_sub4, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab //gen(sub1_2_1)
xcorr e_sub1 e_sub3, lags(6) tab //gen(sub1_3_1)
xcorr e_sub3 e_sub2, lags(6) tab //gen(sub3_2_1)
xcorr e_sub3 e_sub4, lags(6) tab //gen(sub3_4_1)


drop e_sub1 e_sub2 e_sub3 e_sub4


	*** Adsites ***
	***************
arima $y21list $y22list $y23list $y24list $y25list $y26list, arima(1,1,1)
predict e_sub1, resid

arima $y22list $y21list $y23list $y24list $y25list $y26list, arima(1,1,1)
predict e_sub2, resid

arima $y23list $y22list $y21list $y24list $y25list $y26list, arima(1,1,1)
predict e_sub3, resid

arima $y24list $y23list $y22list $y21list $y25list $y26list, arima(1,1,1)
predict e_sub3, resid

*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_2)
xcorr e_sub1 e_sub3, lags(6) tab //gen(sub1_3_2)
xcorr e_sub3 e_sub2, lags(6) tab // gen(sub3_2_2)
xcorr e_sub3 e_sub4, lags(6) tab // gen(sub3_4_2)

drop e_sub1 e_sub2 e_sub3 e_sub4

			**** 2) OLS regression ****
*----------------------------------------------------------------------------*

	*** Circulation A ***
	
reg $y11list $y12list $y13list $y14list $y15list $y16list
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg $y12list $y11list $y13list $y14list $y15list $y16list
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg  $y13list $y12list $y11list $y14list $y15list $y16list
predict e_sub3, resid
corrgram e_sub3, lags(12)

reg  $y14list $y13list $y12list $y11list $y15list $y16list
predict e_sub4, resid
corrgram e_sub4, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab //gen(sub1_2_3)
xcorr e_sub1 e_sub3, lags(6) tab //gen(sub1_3_3)
xcorr e_sub3 e_sub2, lags(6) tab //gen(sub3_2_3)
xcorr e_sub3 e_sub4, lags(6) tab //gen(sub3_4_3)


drop e_sub1 e_sub2 e_sub3 e_sub4 

	*** Adsites A ***
	
reg $y21list $y22list $y23list $y24list $y25list $y26list
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg $y22list $y21list $y23list $y24list $y25list $y26list
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg $y23list $y21list $y22list $y24list $y25list $y26list
predict e_sub3, resid
corrgram e_sub3, lags(12)

reg $y24list $y21list $y22list $y23list $y25list $y26list
predict e_sub4, resid
corrgram e_sub4, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab //gen(sub1_2_4)
xcorr e_sub1 e_sub3, lags(6) tab //gen(sub1_3_4)
xcorr e_sub3 e_sub2, lags(6) tab //gen(sub3_2_4)
xcorr e_sub3 e_sub4, lags(6) tab //gen(sub3_4_4)


drop e_sub1 e_sub2 e_sub3 e_sub4 


/*	*** Circulation B ***

reg d.$y11listlog d.$y12listlog d.$y13listlog week
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg d.$y12listlog d.$y11listlog d.$y13listlog week
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg d.$y13listlog d.$y11listlog d.$y12listlog week
predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation B ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_4)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_4)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_4)

drop e_sub1 e_sub2 e_sub3 

	*** Adsites B ***
	
reg d.log_totaladsiteBUNTE d.log_totaladsiteGala d.log_totaladsiteSUPERillu week
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg d.log_totaladsiteGala d.log_totaladsiteBUNTE d.log_totaladsiteSUPERillu week
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg d.log_totaladsiteSUPERillu d.log_totaladsiteGala d.log_totaladsiteBUNTE week
predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Adsite B ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_6)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_6)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_6)

drop e_sub1 e_sub2 e_sub3 

drop sub1_2_1 sub1_3_1 sub3_2_1 sub1_2_2 sub1_3_2 sub3_2_2 sub1_2_3 sub1_3_3 sub3_2_3 sub1_2_4 sub1_3_4 sub3_2_4 sub1_2_5 sub1_3_5 sub3_2_5 sub1_2_6 sub1_3_6 sub3_2_6


****** DIAGNOSTICS ********
***************************


*******************
**** Unit Root ****
*******************

* Sub1: Circulation
varsoc $y11list, maxlag(17)
pperron $y11list, trend lag(6)
pperron d.$y11list, trend lag(6)
dfuller $y11list, trend lag(6)
dfuller d.$y11list, trend lag(6)


* Sub2: Circulation
varsoc $y12list, maxlag(15)
pperron $y12list, trend lag(3)
pperron d.$y12list, trend lag(3)
dfuller $y12list, trend lag(3)
dfuller d.$y12list, trend lag(3)



* Sub3: Circulation
varsoc $y13list, maxlag(15) 
pperron $y13list, trend lag(13)
dfuller $y13list, trend lag(13)
dfuller d.$y13list, trend lag(13)


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
