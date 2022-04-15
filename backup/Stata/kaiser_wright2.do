** Work with Panel All 
clear all
set more off
import excel "C:\Users\Jorgito\Google Drive\HSU\Paper\2sm\Stata\panel_alle.xls", sheet("Tabelle1") firstrow


*** Declare Time Var

* Quaterly Data
tostring jahr, replace
tostring quartal, replace

gen year = jahr + " " + quartal
gen time= quarterly(year,"YQ")
format time %tq

drop jahr quartal year

rename verkauf sales
rename verbreitung circ
label var circ "Verbreitung"
rename anzeigen adsite
label var adsite "Anzeigen"
rename p4c adprice
label var adprice "p4c"
rename p cprice
label var cprice "Copyprice"
rename umfang content

** Delete Blanks 
replace titel = subinstr(titel," ","",.)

* Count publishers Magazines in a given year 
sort time verlag
egen num = count(titel), by (verlag time)

* Collapse Av. Publisher Data
collapse num (sum) druck circ sales abos EV content adsite adprice ///
		cprice beihefte, by( verlag time )


* Reshape Data from Long to Wide
reshape wide druck sales circ abos EV content adsite ///
   adprice cprice beihefte num, i(time) j(verlag) string
 
** Save data
save "/Users/Franzi/Google Drive/HSU/Paper/2sm/Stata/verlag.dta", replace

* Use nopanel
use "/Users/Franzi/Google Drive/HSU/Paper/2sm/Stata/nopanel.dta"

merge m:m time using "C:\Users\Jorgito\Google Drive\HSU\Paper\2sm\Stata\verlag.dta"


*************************** Building Variables *************************

*** Bunte (hbm) / Gala (guj) 
****************************

**** MARKET SHARES

gen sharebunte = circBunte/(circBunte+circGala)
gen sharegala = circGala/(circBunte+circGala)

gen adsharebunte = adsiteBunte/(adsiteBunte+adsiteGala)
gen adsharegala = adsiteGala/(adsiteBunte+adsiteGala)

**** INSTRUMENTS 

***** 1) cprice 
				*** Bunte ***
gen cpricebunte = cpriceBunte-cpriceGala
gen cpricebunte_ins = ((cpricehbm-cpriceBunte)/(numhbm-1)) - ///
    ((cpriceguj-cpriceGala)/(numguj-1))
	
				*** Gala ***
gen cpricegala = cpriceGala-cpriceBunte
gen cpricegala_ins = ((cpriceguj-cpriceGala)/(numguj-1)) - ///
    ((cpricehbm-cpriceBunte)/(numhbm-1))

***** 2) adprice 
				*** Bunte ***
gen adpricebunte = adpriceBunte-adpriceGala
gen adpricebunte_ins = ((adpricehbm-adpriceBunte)/(numhbm-1)) - ///
    ((adpriceguj-adpriceGala)/(numguj-1))
	
				*** Gala ***
gen adpricegala = adpriceGala-adpriceBunte
gen adpricegala_ins = ((adpriceguj-adpriceGala)/(numguj-1)) - ///
    ((adpricehbm-adpriceBunte)/(numhbm-1))

***** 3) content 
				*** Bunte ***
gen contentbunte = contentBunte-contentGala
gen contentbunte_ins = ((contenthbm-contentBunte)/(numhbm-1)) - ///
    ((contentguj-contentGala)/(numguj-1))
	
				*** Gala ***
gen contentgala = contentGala-contentBunte
gen contentgala_ins = ((contentguj-contentGala)/(numguj-1)) - ///
    ((contenthbm-contentBunte)/(numhbm-1))

***** 4) adsites 
				*** Bunte ***
gen adsitebunte = adsiteBunte-adsiteGala
gen adsitebunte_ins = ((adsitehbm-adsiteBunte)/(numhbm-1)) - ///
    ((adsiteguj-adsiteGala)/(numguj-1))
	
				*** Gala ***
gen adsitegala = adsiteGala-adsiteBunte
gen adsitegala_ins = ((adsiteguj-adsiteGala)/(numguj-1)) - ///
    ((adsitehbm-adsiteBunte)/(numhbm-1))
	
***** 5) circ 
				*** Bunte ***
gen circbunte = circBunte-circGala
gen circbunte_ins = ((circhbm-circBunte)/(numhbm-1)) - ///
    ((circguj-circGala)/(numguj-1))
	
				*** Gala ***
gen circgala = circGala-circBunte
gen circgala_ins = ((circguj-circGala)/(numguj-1)) - ///
    ((circhbm-circBunte)/(numhbm-1))

**** 6) Natural log of Number of magazines titles 
gen log_numhbm = log(numhbm)
gen log_numguj = log(numguj)

**** 7) Natural log of the total number of pages 
gen log_contenthbm = log(contenthbm)
gen log_contentguj = log(contentguj)

**** 8) linear and quadratic timetrend

gen quad = time^2

****************************** Assign Globals **************************
global y11list sharebunte
global y21list adsharebunte

global x11list d.cpricebunte d.contentbunte d.adsitebunte
global x21list d.circbunte d.adpricebunte

global i11list cpricebunte_ins contentbunte_ins adsitebunte_ins ///
				log_contenthbm L.cpricebunte_ins L.contentbunte_ins  ///
				L.adsitebunte_ins L.log_contenthbm
global i21list circbunte_ins adpricebunte_ins L.circbunte_ins L.adpricebunte_ins

global timelist quad time				


* Simple Panel - OLS regressions
sort time
			*** Stern ***
reg $y11list $x11list $timelist //   Readers-Market Demand Function
reg $y21list $x21list $timelist   //   Advertiser-Market Demand Function
			*** Der Spiegel ***
reg $y12list $x12list $timelist //    Readers-Market Demand Function
reg $y22list $x22list $timelist   //    Advertiser-Market Demand Function

* Instrumented 2sls regression
			*** Stern ***
ivreg $y11list ($x11list = $i11list $timelist) if (time>=tq(1982q4)), first
ivreg $y21list ($x21list = $i21list $timelist) if (time>=tq(1982q4)), first
			*** Der Spiegel ***
ivreg $y12list ($x12list = $i12list $timelist) if (time>=tq(1982q4)), first
ivreg $y22list ($x22list = $i22list $timelist) if (time>=tq(1982q4)), first


* Non-instrumented Seemingly Unrelated Regression (SUR) -> Table 4(S.12)
* not sure which one is the right syntax...
sureg ($y11list $x11list $timelist) ///
($y21list $x21list $timelist), corr

** Instrumented Regression with FD

reg3 ($y11list $x11list $timelist, dfk) ///
($y21list $x21list $timelist, dfk), ///
3sls ins($i11list $i21list $timelist)

reg3 ($y12list $x12list $timelist, dfk) ///
($y22list $x22list $timelist, dfk) if (time>=tq(1982q4)), ///
2sls ins($i12list $i22list $timelist)

** Instrumented GMM regresion

gmm (d.sharebunte - d.cpricebunte*{b1} - d.contentbunte*{b2} ///
	- d.adsitebunte*{b3} - quad*{b4} - time*{b5} - {b6}), ///
	instruments($i11list)

ivgmm0 d.sharebunte $timelist ($x11list=$i11list)
	
gmm (d.adsharebunte - d.circbunte*{b1} - d.adpricebunte*{b2} ///
	- quad*{b3} - time*{b4} - {b5}), ///
	instruments($i21list)
	
/* NEU MACHEN *******************************

*** Meine Familie&Ich(hbm) / Essen&Trinken (guj) 
***********************************************
***** 1) Circulation
gen circess_p = (circhbm-circ)/(numhbm-1) if titel == "Essen&Trinken" 
gen circfam_p = (circguj-circ)/(numguj-1) if titel == "MeineFamilie&Ich" 

***** 2) Adsite
gen adsiteess_p = (adsitehbm-adsite)/(numhbm-1) if titel == "Essen&Trinken" 
gen adsitefam_p = (adsiteguj-adsite)/(numguj-1)  if titel == "MeineFamilie&Ich" 

***** 3) Content
gen contentess_p = (contenthbm-content)/(numhbm-1) if titel == "Essen&Trinken" 
gen contentfam_p = (contentguj-content)/(numguj-1)  if titel == "MeineFamilie&Ich"

***** 4) Adprice
gen adpriceess_p = (adpricehbm-adprice)/(numhbm-1) if titel == "Essen&Trinken" 
gen adpricefam_p = (adpriceguj-adprice)/(numguj-1) if titel == "MeineFamilie&Ich" 

***** 5) Cprice
gen cpriceess_p = (cpricehbm-cprice)/(numhbm-1) if titel == "Essen&Trinken" 
gen cpricefam_p = (cpriceguj-cprice)/(numguj-1) if titel == "MeineFamilie&Ich" 

** Mein schöner Garten (hbm)  / Flora (guj)
***********************************************
***** 1) Circulation
gen circflora_p = (circhbm-circ)/(numhbm-1) if titel == "MeinschönerGarten" 
gen circgarten_p = (circguj-circ)/(numguj-1) if titel == "FloraGarten" 

***** 2) Adsite
gen adsiteflora_p = (adsitehbm-adsite)/(numhbm-1) if titel == "MeinschönerGarten" 
gen adsitegarten_p = (adsiteguj-adsite)/(numguj-1)  if titel == "FloraGarten" 

***** 3) Content
gen contentflora_p = (contenthbm-content)/(numhbm-1) if titel == "MeinschönerGarten" 
gen contentgarten_p = (contentguj-content)/(numguj-1)  if titel == "FloraGarten"

***** 4) Adprice
gen adpriceflora_p = (adpricehbm-adprice)/(numhbm-1) if titel == "MeinschönerGarten" 
gen adpricegarten_p = (adpriceguj-adprice)/(numguj-1) if titel == "FloraGarten" 

***** 5) Cprice
gen cpriceflora_p = (cpricehbm-cprice)/(numhbm-1) if titel == "MeinschönerGarten" 
gen cpricegarten_p = (cpriceguj-cprice)/(numguj-1) if titel == "FloraGarten" 

** Madame (mpv) / Vogue (cnv)
***********************************************
***** 1) Circulation
gen circmadame_p = (circmpv-circ)/(nummpv-1) if titel == "Madame" 
gen circvogue_p = (circcnv-circ)/(numcnv-1) if titel == "Vogue" 

***** 2) Adsite
gen adsitemadame_p = (adsitempv-adsite)/(nummpv-1) if titel == "Madame" 
gen adsitevogue_p = (adsitecnv-adsite)/(numcnv-1)  if titel == "Vogue" 

***** 3) Content
gen contentmadame_p = (contentmpv-content)/(nummpv-1) if titel == "Madame" 
gen contentvogue_p = (contentcnv-content)/(numcnv-1)  if titel == "Vogue"

***** 4) Adprice
gen adpricemadame_p = (adpricempv-adprice)/(nummpv-1) if titel == "Madame" 
gen adpricevogue_p = (adpricecnv-adprice)/(numcnv-1) if titel == "Vogue" 

***** 5) Cprice
gen cpricemadame_p = (cpricempv-cprice)/(nummpv-1) if titel == "Madame" 
gen cpricevogue_p = (cpricecnv-cprice)/(numcnv-1) if titel == "Vogue" 

** Bella (bmk) / Tina (bmk)
***********************************************
***** 1) Circulation
gen circbella_p = (circbmk-circ)/(numbmk-1) if titel == "Bella" 
gen circtina_p = (circbmk-circ)/(numbmk-1) if titel == "Tina" 

***** 2) Adsite
gen adsitebella_p = (adsitempv-adsite)/(numbmk-1) if titel == "Bella" 
gen adsitetina_p = (adsitecnv-adsite)/(numbmk-1)  if titel == "Tina" 

***** 3) Content
gen contentbella_p = (contentbmk-content)/(numbmk-1) if titel == "Bella" 
gen contenttina_p = (contentbmk-content)/(numbmk-1)  if titel == "Tina"

***** 4) Adprice
gen adpricebella_p = (adpricebmk-adprice)/(numbmk-1) if titel == "Bella" 
gen adpricetina_p = (adpricebmk-adprice)/(numbmk-1) if titel == "Tina" 

***** 5) Cprice
gen cpricebella_p = (cpricebmk-cprice)/(numbmk-1) if titel == "Bella" 
gen cpricetina_p = (cpricebmk-cprice)/(numbmk-1) if titel == "Tina" 

** Chip (vbc) / PC Welt (idg)
***********************************************
***** 1) Circulation
gen circchip_p = circvbc if titel == "Chip" 
gen circpc_p = circidg if titel == "PCWelt" 

***** 2) Adsite
gen adsitechip_p = adsitevbc if titel == "Chip" 
gen adsitepc_p = adsiteidg  if titel == "PCWelt" 

***** 3) Content
gen contentchip_p = contentvbc if titel == "Chip" 
gen contentpv_p = contentidg  if titel == "PCWelt"

***** 4) Adprice
gen adpricechip_p = adpricevbc if titel == "Chip" 
gen adpricepc_p = adpriceidg if titel == "PCWelt" 

***** 5) Cprice
gen cpricechip_p = cpricevbc if titel == "Chip" 
gen cpricepc_p = cpriceidg if titel == "PCWelt" */






