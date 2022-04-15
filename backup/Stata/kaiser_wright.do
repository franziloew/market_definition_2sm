* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* Price Structure in 2sm: Evidence from the magazine industry               *
*                                                      						*
* This document is mainly based on Kaiser&Wright (2006)  					*
* (Deatils are available upon request)                                      *
*                                                                           *
* Franziska Löw, 2016                                                       *
*                                                                           *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

use "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\sample_kaiser_wright.dta", clear

encode titel, gen(title)
xtset title year

**************************************
* I) Looking at and summarizing data *
**************************************

* describe *
************

* browse *
**********

* list * 
********

* summarize *
*************
sum circ adsite content cprice adprice 

**************************************
* II) Exploring Data by Graphs       *
**************************************

bysort market: egen sales_mean=mean(sales)
twoway scatter sales market, msymbol(circle_hollow) || connected sales_mean ///
market, msymbol(diamond)

bysort year: egen sales_mean1=mean(sales)
twoway scatter sales year, msymbol(circle_hollow) || connected sales_mean1 ///
year, msymbol(diamond)

twoway scatter share_g adshare_g 
twoway scatter share adshare 


** Comparing Genre Markets with Sample-Duopoly
xtline share_g share if market == 1 


/* Testing instruments
**********************
* Publisher Av. */
pwcorr sales sales_p, sig // corr. .73
pwcorr adprice adprice_p, sig // corr. .11
pwcorr cprice cprice_p, sig // corr. .93
pwcorr adsite adsite_p, sig // corr. .17
pwcorr content content_p, sig // corr. -.06 (p-value .36)

xtreg sales sales_p adprice_p adsite_p cprice_p content_p 
xtreg adprice sales_p adprice_p adsite_p cprice_p content_p 
xtreg cprice sales_p adprice_p adsite_p cprice_p content_p 
xtreg adsite sales_p adprice_p adsite_p cprice_p content_p
xtreg content sales_p adprice_p adsite_p cprice_p content_p 

* Generate additional instruments
*********************************
* Time Trend
xtset title year

gen t = _n
gen t2 = t^2

label variable t "linear timetrend"
label variable t2 "quadratic timetrend"

* Laged Instruments
xtset title year

gen na_inst1l = L.na_inst1 
gen nc_inst1l = L.nc_inst1 
gen p_inst1l = L.p_inst1
gen nr_inst1l = L.nr_inst1 
gen a_inst1l = L.a_inst1

label variable na_inst1l "lagged na_inst1"
label variable nc_inst1l "lagged nc_inst1"
label variable p_inst1l "lagged p_inst1"
label variable nr_inst1l "lagged nr_inst1"
label variable a_inst1l "lagged a_inst1"

* Magazines of Market 5 have the same publisher, so may be excluded

gen insample = 1
replace insample = 0 if market == 5

* Assign Global Varlist
***********************
* Dependent Var 
global y1list share   // Market Share Reader-market 
global y2list adshare // Market Share Advertiser-market 

* Endogenous Var
* global x1list na nc p
global x1list na p 
global x2list nr a 

* Instruments 
* publisher average
* global i1list na_inst1 nc_inst1 p_inst1 
global i1list na_inst1 p_inst1 
global i2list nr_inst1 a_inst1 

* timetrend
global timelist t t2 

* lag variables
* global lag1list na_inst1l nc_inst1l p_inst1l 
global lag1list na_inst1l p_inst1l 
global lag2list nr_inst1l a_inst1l 

*****************
* III) Analysis *
*****************

* Simple Panel - OLS regressions
xtreg $y1list $x1list $timelist if insample == 1, fe // Readers-Market Demand Function
xtreg $y2list $x2list $timelist if insample == 1, fe // Advertiser-Market Demand Function

* Instrumented 2sls regression
xtivreg2 $y1list ($x1list = $i1list $timelist $lag1list) if insample == 1, fd gmm first
xtivreg2 $y2list ($x2list = $i2list $timelist $lag2list) if insample == 1, fd gmm first


* Non-instrumented Seemingly Unrelated Regression (SUR) -> Table 4(S.12)
* not sure which one is the right syntax...
sureg ($y1list $x1list, fd gmm) ///
($y2list $x2list, fd gmm) if insample == 1, corr

reg3 ($y1list $x1list, su) ///
($y2list $x2list, su) if insample == 1, ///
2sls 

** Instrumented GMM Regression with FD

reg3 ($y1list $x1list, fd gmm dfk) ///
($y2list $x2list, fd gmm dfk) if insample == 1, ///
2sls ins($i1list $i2list $lag1list $lag2list $timelist) 

