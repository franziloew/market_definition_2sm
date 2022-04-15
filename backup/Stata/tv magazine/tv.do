log using "C:\Users\Jorgito\Google Drive\HSU\Paper\2sm\Stata\tv magazine\tv magazines.txt", text replace

**************************
** Market: TV Magazines **
**************************

tsset time


**** Assign Globals ****
*************************

**** Sub1 
global y11list retailTVMovie
global x11list aboTVMovie contentTVMovie
global y11listlog log_retailTVMovie

global y21list totaladsiteTVMovie
global y21listlog log_totaladsiteTVMovie


**** Sub2
global y12list retailTVSpielfilm
global x12list aboTVSpielfilm contentTVSpielfilm
global y12listlog log_retailTVSpielfilm


global y22list totaladsiteTVSpielfilm
global y22listlog log_totaladsiteTVSpielfilm


****  Sub3
global y13list retailTVToday
global x13list aboTVToday contentTVToday
global y13listlog log_retailTVToday

global y23list totaladsiteTVToday
global y23listlog log_totaladsiteTVToday



*** Time Var

global timelist1 quad month logtime
global timelist2 quad logtime
global timelist3 month logtime

* Graphical Analysis
tsline $y11list $y12list $y13list, xtitle("") xlabel(1 "1/2005" 15 "15/2005" 27 "01/2006" ///
52 "26/2006") ///
lw(medthick medium) ytitle("Retail Sales") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

graph2tex, epsfile(circ_tv)

tsline $y11list $y12list $y13list, xtitle("") xlabel(1 "1/2005" 15 "15/2005" 27 "01/2006" ///
52 "26/2006") ///
lw(medthick medium) ytitle("Total ad-sites per Copy") ///    
legend(ring(0) position(11) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))   

graph2tex, epsfile(ads_tv)
 

		**** 1) ARMA ****
*------------------------------------------------------------------------*

	*** Circulation ***
	*******************

arima $y11list $y12list $y13list, arima(1,1,0) r
*Calculate the error term (residuals)
predict e_sub1, resid
*Estimate autocorrelation
corrgram e_sub1, lags(12)

arima $y12list $y11list $y13list, arima(1,1,0) r
predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y13list $y11list $y12list, arima(1,1,0) r
predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_1)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_1)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_1)

* Graphical Analysis
tsline $y11list $y12list $y13list, xtitle("") xlabel(1 "1/2005" 15 "15/2005" 27 "01/2006" ///
52 "26/2006") ///
lw(medthick medium) ytitle("Resid Reader Market ARIMA") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday")) // 107 "1/2007" 159 "1/2009" 211 "1/2011" 263 "1/2013" 

graph2tex, epsfile(circ_arima_tv)

drop e_sub1 e_sub2 e_sub3 



	*** Adsites ***
	***************
arima $y21list $y22list $y23list, arima(1,0,1) r
predict e_sub1, resid
corrgram e_sub1, lags(12)

arima $y22list $y21list $y23list, arima(1,0,1) r
predict e_sub2, resid
corrgram e_sub2, lags(12)

arima $y23list $y22list $y21list, arima(1,0,1) r
predict e_sub3, resid
corrgram e_sub3, lags(12)

*** Crosscorrelation: Circulation ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_2)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_2)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_2)

* Graphical Analysis
tsline e_sub1 e_sub2 e_sub3, xtitle("") xlabel(1 "1/2005" 15 "15/2005" 27 "01/2006" ///
52 "26/2006") ///
lw(medthick medium) ytitle("Resid Ad Market ARIMA ") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

graph2tex, epsfile(ads_arima_tv)

drop e_sub1 e_sub2 e_sub3  

			**** 2) OLS regression ****
*----------------------------------------------------------------------------*

	*** Circulation A ***
	
reg d.$y11list d.$y12list d.$y13list, robust

predict e_sub1, resid
corrgram e_sub1, lags(12)

reg d.$y12list d.$y11list d.$y13list, robust

predict e_sub2, resid
corrgram e_sub2, lags(12)

reg d.$y13list d.$y12list d.$y11list, robust

predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation A ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_3)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_3)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_3)

* Graphical Analysis
tsline e_sub1 e_sub2 e_sub3, xtitle("") xlabel(1 "1/2005" 15 "15/2005" 27 "01/2006" ///
52 "26/2006") ///
lw(medthick medium) ytitle("Resid Reader Market OLS") ///    
legend(ring(0) position(7) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

graph2tex, epsfile(circ_ols_tv)

drop e_sub1 e_sub2 e_sub3  



	*** Adsites A ***
	
reg d.$y21list d.$y22list d.$y23list, r

predict e_sub1, resid
corrgram e_sub1, lags(12)

reg d.$y22list d.$y23list d.$y21list, r

predict e_sub2, resid
corrgram e_sub2, lags(12)

reg d.$y23list d.$y21list d.$y22list, r 
predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Adsite A ***
xcorr e_sub1 e_sub2, lags(6) tab gen(sub1_2_4)
xcorr e_sub1 e_sub3, lags(6) tab gen(sub1_3_4)
xcorr e_sub3 e_sub2, lags(6) tab gen(sub3_2_4)

* Graphical Analysis
tsline e_sub1 e_sub2 e_sub3, xtitle("") xlabel(1 "1/2005" 15 "15/2005" 27 "01/2006" ///
52 "26/2006") ///
lw(medthick medium) ytitle("Resid Ad Market OLS") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

graph2tex, epsfile(ads_ols_tv)

drop e_sub1 e_sub2 e_sub3 

	/*** Circulation B ***

reg d.$y11listlog d.$y12listlog d.$y13listlog month
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg d.$y12listlog d.$y11listlog d.$y13listlog month
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg d.$y13listlog d.$y11listlog d.$y12listlog month
predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Circulation B ***
xcorr e_sub1 e_sub2, lags(6) tab //gen(sub1_2_5)
xcorr e_sub1 e_sub3, lags(6) tab //gen(sub1_3_5)
xcorr e_sub3 e_sub2, lags(6) tab //gen(sub3_2_5)

* Graphical Analysis
tsline e_sub1 e_sub2 e_sub3, xtitle("") ///
lw(medthick medium) ytitle("Resid Reader Market OLS Log") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

drop e_sub1 e_sub2 e_sub3 

	*** Adsites B ***
	
reg d.log_adsiteTVMovie d.log_adsiteTVSpielfilm d.log_adsiteTVToday month
predict e_sub1, resid
corrgram e_sub1, lags(12)

reg d.log_adsiteTVSpielfilm d.log_adsiteTVMovie d.log_adsiteTVToday month
predict e_sub2, resid
corrgram e_sub2, lags(12)

reg d.log_adsiteTVToday d.log_adsiteTVSpielfilm d.log_adsiteTVMovie month 
predict e_sub3, resid
corrgram e_sub3, lags(12)


*** Crosscorrelation: Adsite B ***
xcorr e_sub1 e_sub2, lags(6) tab // gen(sub1_2_6)
xcorr e_sub1 e_sub3, lags(6) tab // gen(sub1_3_6)
xcorr e_sub3 e_sub2, lags(6) tab // gen(sub3_2_6)

* Graphical Analysis
tsline e_sub1 e_sub2 e_sub3, xtitle("") ///
lw(medthick medium) ytitle("Residuals Total ad-sites per Copy") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

drop e_sub1 e_sub2 e_sub3 */



****** DIAGNOSTICS ********
***************************


*******************
**** Unit Root ****
*******************

* Sub1: Circulation
varsoc $y11list, maxlag(12)
pperron $y11list, trend lag(1)
pperron d.$y11list, trend lag(1)
dfuller $y11list, trend lag(1)
dfuller d.$y11list, trend lag(1)


* Sub2: Circulation
varsoc $y12list, maxlag(12)
pperron $y12list, trend lag(1)
pperron d.$y12list, trend lag(1)
dfuller $y12list, trend lag(1)
dfuller d.$y12list, trend lag(1)


* Sub3: Circulation
varsoc $y13list, maxlag(12) 
pperron $y13list, trend lag(1)
dfuller $y13list, trend lag(1)
dfuller d.$y13list, trend lag(1)


* Sub1: Adsite
varsoc $y21list, maxlag(15)
pperron $y21list , trend lag(1) 
pperron d.$y21list , trend lag(1) 
dfuller $y21list , trend lag(1) 
dfuller d.$y21list , trend lag(1) 

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

* Graphical Analysis
tsline $y11list $y12list $y13list, xtitle("") ///
lw(medthick medium) ytitle("Monthly Retail Sales") ///    
legend(ring(0) position(1) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))

tsline $y21list $y22list $y23list, xtitle("") ///
lw(medthick medium) ytitle("Monthly Ad-sites / Total sites per Copy") ///    
legend(ring(0) position(7) row(2) label(1 "TVMovie") label(2 "TVSpielfilm") ///
label(3 "TVToday"))    

* Plotting

*** Circulation ***
*******************
twoway (tsline $y11list)
twoway (tsline d.$y11list)

twoway (tsline $y12list)
twoway (tsline d.$y12list)


*** Adsites ***
***************

twoway (tsline $y21list)
twoway (tsline d.$y21list)

twoway (tsline $y22list)
twoway (tsline d.$y22list)


**** Explore autocorrelation ****
 
corrgram $y11list, lags(12)
corrgram $y12list, lags(12)

corrgram $y21list, lags(12)
corrgram $y22list, lags(12)

corrgram $y13list, lags(12)
corrgram $y23list, lags(12)

ac $y11list
pac  $y11list

ac $y12list
pac $y12list

ac $y13list

ac $y21list

ac $y22list

ac $y23list

*******************
**** Unit Root ****
*******************

* TVMovie: Circulation
varsoc $y11list, maxlag(17)
pperron $y11list, trend lag(13)
pperron d.$y11list, trend lag(13)
dfuller $y11list, trend lag(13)
dfuller d.$y11list, trend lag(13)


* TVSpielfilm: Circulation
varsoc $y12list, maxlag(15)
pperron $y12list, trend lag(14)
pperron d.$y12list, trend lag(14)
dfuller $y12list, trend lag(14)
dfuller d.$y12list, trend lag(14)



* TVToday: Circulation
varsoc $y13list, maxlag(15) 
pperron $y13list, trend lag(13)
dfuller $y13list, trend lag(13)
dfuller d.$y13list, trend lag(13)


* TVMovie: Adsite
varsoc $y21list, maxlag(15) // lag 12
pperron $y21list , trend lag(12) 
pperron d.$y21list , trend lag(12) 


* TVSpielfilm: Adsite
varsoc $y22list, maxlag(15) // lag 1
pperron $y22list , trend lag(14)
pperron d.$y22list , trend lag(14)

* TVToday: Adsite
varsoc $y23list, maxlag(15) // lag 1
pperron $y23list, trend lag(14)
pperron d.$y23list , trend lag(1)


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




		**** 3) Vector Error Correction Model ****
*----------------------------------------------------------------------------*


		*** Circulation ***
		*******************
varsoc $y11list $y12list $y13list , maxlag(12)

vec $y11list $y12list $y13list, lags(4)
predict e_movie, resid
corrgram e_movie, lags(12)

vec $y12list $y11list $y13list, lags(4)
predict e_spfilm, resid
corrgram e_spfilm, lags(12)

vec $y13list $y12list $y11list, lags(4)
predict e_sub, resid
corrgram e_sub, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_movie e_spfilm, lags(6) tab
xcorr e_spfilm e_sub, lags(6) tab
xcorr e_sub e_spfilm, lags(6) tab


drop e_movie e_spfilm e_sub 


			*** Adsites ***
			***************
varsoc $y21list $y22list $y23list , maxlag(12)

vec $y21list $y22list $y23list, lags(4)
predict e_movie, resid
corrgram e_movie, lags(12)

vec $y22list $y21list $y23list, lags(4)
predict e_spfilm, resid
corrgram e_spfilm, lags(12)

vec $y23list $y22list $y21list, lags(4)
predict e_sub, resid
corrgram e_sub, lags(12)


*** Crosscorrelation: Circulation ***
xcorr e_movie e_spfilm, lags(12) tab
xcorr e_spfilm e_sub, lags(12) tab
xcorr e_sub e_spfilm, lags(12) tab


drop e_movie e_spfilm e_sub


log close
