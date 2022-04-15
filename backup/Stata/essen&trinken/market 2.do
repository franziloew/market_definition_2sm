********************
** Market 2: Food **
********************

*** Essen&Trinken (guj) / Meine Familie&ich (hbm) 
****************************


**** Assign Globals ****
************************

**** Meine Familie&Ich 
global y11list circMeineFamilieuIch
global y21list adsiteMeineFamilieuIch

global x11list cpriceMeineFamilieuIch contentMeineFamilieuIch adsiteMeineFamilieuIch
global x21list circMeineFamilieuIch adpriceMeineFamilieuIch

global i11list cpriceMeineFamilieuIch_ins contentMeineFamilieuIch_ins ///
					adsiteMeineFamilieuIch_ins log_contenthbm log_numhbm ///
					L.cpriceMeineFamilieuIch_ins L.contentMeineFamilieuIch_ins  ///
				    L.adsiteMeineFamilieuIch_ins L.log_contenthbm L.log_numhbm
					
global i21list circMeineFamilieuIch_ins adpriceMeineFamilieuIch_ins ///
					L.circMeineFamilieuIch_ins L.adpriceMeineFamilieuIch_ins

**** EssenuTrinken
global y12list circEssenuTrinken
global y22list adsiteEssenuTrinken

global x12list cpriceEssenuTrinken contentEssenuTrinken adsiteEssenuTrinken
global x22list circEssenuTrinken adpriceEssenuTrinken

global i12list cpriceEssenuTrinken_ins contentEssenuTrinken_ins adsiteEssenuTrinken_ins ///
				log_contentguj log_numguj L.cpriceEssenuTrinken_ins L.contentEssenuTrinken_ins  ///
				L.adsiteEssenuTrinken_ins L.log_contentguj L.log_numguj
global i22list circEssenuTrinken_ins adpriceEssenuTrinken_ins L.circEssenuTrinken_ins L.adpriceEssenuTrinken_ins

*** Further Instruments


global x3list circKochenuGenießen circRezeptemitPfiff 
				
global x4list adsiteRezeptemitPfiff adsiteRezeptemitPfiff

*** Time Var
		
global timelist1 quad time logtime
global timelist2 qseas2 qseas3 qseas4
global timelist3 quad logtime
global timelist4 time logtime

**** Unit Root ****
*******************

dfuller circMeineFamilieuIch, lag(4) 
dfuller d.circMeineFamilieuIch, lag(4)

dfuller circEssenuTrinken, lag(4) 
dfuller d.circEssenuTrinken, lag(4)

dfuller adsiteMeineFamilieuIch, lag(4) 
dfuller d.adsiteMeineFamilieuIch, lag(4)

dfuller adsiteEssenuTrinken , lag(4) 
dfuller d.adsiteEssenuTrinken, lag(4) 



**** Lag Selection + Granger Causality ****
*******************************************

*** 1. Readers Market 

varsoc circMeineFamilieuIch circEssenuTrinken, maxlag(10) // lag 5 (SBIC)

*** 2. Advertiser Market 

varsoc adsiteMeineFamilieuIch adsiteEssenuTrinken, maxlag(10) // lag 4 (SBIC)

**** Cointegration ****
***********************

qui reg circMeineFamilieuIch circEssenuTrinken
predict e, resid
dfuller e, lag(5)

drop e
qui reg circEssenuTrinken circMeineFamilieuIch
predict e, resid
dfuller e, lag(5)

drop e
qui reg adsiteMeineFamilieuIch adsiteEssenuTrinken
predict e, resid
dfuller e, lag(4)

drop e
qui reg adsiteEssenuTrinken adsiteMeineFamilieuIch
predict e, resid
dfuller e, lag(4)


******************************
**** Testing Cross Correlation
*******************************

*** 1)  Simple OLS regressions
********************************

			*** Circulation ***
			*******************

reg $y11list $y12list $x3list $timelist1
predict e_fam, resid

reg $y12list $y11list $x3list $timelist1
predict e_essen, resid

xcorr e_fam e_essen, lags(12) tab
drop e_fam e_essen

				*** Adsites ***
				***************
reg $y21list $y22list $x3list $timelist2
predict e_fam1, resid

reg $y22list $y21list $x3list $timelist2 
predict e_essen1, resid

xcorr e_fam1 e_essen1, lags(12) tab
drop e_fam1 e_essen1


/**** MODEL Estimation ****
 **************************

Instrumented 2sls regression
			*** Meine Familie&Ich  ***
ivreg $y11list2 ($x11list2 = $i11list2) $timelist
predict e_fam2, resid

ivreg $y21list2 ($x21list2 = $i21list2) $timelist 
predict e_fam3, resid

			*** EssenuTrinken ***
ivreg $y12list2 ($x12list2 = $i12list2) $timelist 
predict e_essen2, resid

ivreg $y22list2 ($x22list2 = $i22list2) $timelist 
predict e_essen3, resid

xcorr e_fam2 e_essen2, lags(12) tab

xcorr e_fam3 e_essen3, lags(12) tab


* Non-instrumented Seemingly Unrelated Regression (SUR) -> Table 4(S.12)
sureg ($y11list2 $x11list2 $timelist) ///
($y21list2 $x21list2 $timelist), corr

** Instrumented Regression with FD

reg3 ($y11list2 $x11list2 $timelist $y12list2, dfk) ///
($y21list2 $x21list2 $timelist $y22list2, dfk) ///
($y12list2 $x12list2 $timelist $y11list2, dfk) ///
($y22list2 $x22list2 $timelist $y21list2, dfk), ///
3sls ins($i11list2 $i21list2 $i12list2 $i22list2 $timelist) */

