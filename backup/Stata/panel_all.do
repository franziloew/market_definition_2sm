** Work with Panel All 
clear all

import excel "C:\Users\Jorgito\Google Drive\HSU\Paper\2sm\Stata\panel_alle.xls", sheet("Tabelle1") firstrow


*** Declare Time Var

* Quaterly Data
tostring jahr, replace
tostring quartal, replace

gen time = jahr + " " + quartal
gen year= quarterly(time,"YQ")
format year %tq

drop jahr quartal time
order titel year

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

* Count publishers Magazines in a given year 
encode titel, gen(title)
encode verlag, gen(pub)

sort year pub
egen N_pubyear = count(titel), by (pub year)

* Count Number of printed Pages of pubX in a given year
sort year pub
egen N_pubcontent = sum(content), by (pub year)

* Save Data
save "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\panel_all.dta", replace

* Generate Average Variables 
* 1) Genre
* Collapse Data by Year and Genre (remove /* if data should be collapsed by genre)
collapse druck sales circ abos EV ///
 content adsite adprice cprice beihefte, by(year genre)

sort genre year 

* Reshape Data from Long to Wide
reshape wide druck sales circ abos EV content adsite ///
   adprice cprice beihefte, i(year) j(genre) string

*Save Data 
save "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\genre.dta", replace

* 2) Publisher

use "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\panel_all.dta", clear

* Collapse Data by Year and Verlag
collapse druck sales circ abos ///
 EV content adsite adprice cprice beihefte, by(year verlag)

sort verlag year 

* Reshape Data from Long to Wide
reshape wide druck sales circ abos ///
EV content adsite adprice cprice beihefte, i(year) j(verlag) string

*Save Data 
save "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\verlag.dta", replace

* Combinde Data-Sets 

use "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\panel_all.dta", clear

merge m:m year using "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\genre.dta"
drop _merge
merge m:m year using "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\verlag.dta"
drop _merge

save "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\panel_all.dta", replace

* Declare Data to be Panel Data

encode titel, gen(title)
xtset title year

* Generate Instrumental Variables 
* 1) Publ Average
gen sales_p = .
replace sales_p = salesasv if verlag == "asv"
replace sales_p = salesbmk if verlag == "bmk"
replace sales_p = salesguj if verlag == "guj"
replace sales_p = saleshbm if verlag == "hbm"
replace sales_p = salesmpv if verlag == "mpv"
replace sales_p = salescnv if verlag == "cnv"
replace sales_p = salesvbc if verlag == "vbc"
replace sales_p = salesidg if verlag == "idg"
label var sales_p "Publishers average sales" 

gen adsite_p = .
replace adsite_p = adsiteasv if verlag == "asv"
replace adsite_p = adsitebmk if verlag == "bmk"
replace adsite_p = adsiteguj if verlag == "guj"
replace adsite_p = adsitehbm if verlag == "hbm"
replace adsite_p = adsitempv if verlag == "mpv"
replace adsite_p = adsitecnv if verlag == "cnv"
replace adsite_p = adsitevbc if verlag == "vbc"
replace adsite_p = adsiteidg if verlag == "idg"
label var adsite_p "Publishers average adsites" 

gen content_p = .
replace content_p = contentasv if verlag == "asv"
replace content_p = contentbmk if verlag == "bmk"
replace content_p = contentguj if verlag == "guj"
replace content_p = contenthbm if verlag == "hbm"
replace content_p = contentmpv if verlag == "mpv"
replace content_p = contentcnv if verlag == "cnv"
replace content_p = contentvbc if verlag == "vbc"
replace content_p = contentidg if verlag == "idg"
label var content_p "Publishers average content" 

gen adprice_p = .
replace adprice_p = adpriceasv if verlag == "asv"
replace adprice_p = adpricebmk if verlag == "bmk"
replace adprice_p = adpriceguj if verlag == "guj"
replace adprice_p = adpricehbm if verlag == "hbm"
replace adprice_p = adpricempv if verlag == "mpv"
replace adprice_p = adpricecnv if verlag == "cnv"
replace adprice_p = adpricevbc if verlag == "vbc"
replace adprice_p = adpriceidg if verlag == "idg"
label var adprice_p "Publishers average adprice" 

gen cprice_p = .
replace cprice_p = cpriceasv if verlag == "asv"
replace cprice_p = cpricebmk if verlag == "bmk"
replace cprice_p = cpriceguj if verlag == "guj"
replace cprice_p = cpricehbm if verlag == "hbm"
replace cprice_p = cpricempv if verlag == "mpv"
replace cprice_p = cpricecnv if verlag == "cnv"
replace cprice_p = cpricevbc if verlag == "vbc"
replace cprice_p = cpriceidg if verlag == "idg"
label var cprice_p "Publishers average adprice" 


* Testing instruments (remove */ )
/*pwcorr sales sales_p if insample == 1, sig // corr. .59
xtreg sales sales_p adprice_p adsite_p cprice_p content_p if insample == 1

pwcorr adprice adprice_p if insample == 1, sig // corr. .34
xtreg adprice sales_p adprice_p adsite_p cprice_p content_p if insample == 1

pwcorr cprice cprice_p if insample == 1, sig // corr. .66
xtreg cprice sales_p adprice_p adsite_p cprice_p content_p if insample == 1

pwcorr adsite adsite_p if insample == 1, sig // corr. .47
xtreg adsite sales_p adprice_p adsite_p cprice_p content_p if insample == 1

pwcorr content content_p if insample == 1, sig // corr. .32
xtreg content sales_p adprice_p adsite_p cprice_p content_p if insample == 1*/

* 2) Genre Average

gen sales_g = .
replace sales_g = sales14frauen if genre == "14frauen"
replace sales_g = salesaktuelle if genre == "aktuelle"
replace sales_g = salescar if genre == "car"
replace sales_g = salescomputer if genre == "computer"
replace sales_g = salesecon if genre == "econ"
replace sales_g = saleseltern if genre == "eltern"
replace sales_g = salesess if genre == "ess"
replace sales_g = salesliving if genre == "living"
replace sales_g = salesmonfrauen if genre == "monfrauen"
replace sales_g = salesmusic if genre == "music"
replace sales_g = salessport if genre == "sport"
replace sales_g = salestv if genre == "tv"
replace sales_g = saleswofrauen if genre == "wofrauen"
label var sales_g "Genre average sales" 

gen adsite_g = .
replace adsite_g = adsite14frauen if genre == "14frauen"
replace adsite_g = adsiteaktuelle if genre == "aktuelle"
replace adsite_g = adsitecar if genre == "car"
replace adsite_g = adsitecomputer if genre == "computer"
replace adsite_g = adsiteecon if genre == "econ"
replace adsite_g = adsiteeltern if genre == "eltern"
replace adsite_g = adsiteess if genre == "ess"
replace adsite_g = adsiteliving if genre == "living"
replace adsite_g = adsitemonfrauen if genre == "monfrauen"
replace adsite_g = adsitemusic if genre == "music"
replace adsite_g = adsitesport if genre == "sport"
replace adsite_g = adsitetv if genre == "tv"
replace adsite_g = adsitewofrauen if genre == "wofrauen"
label var adsite_g "Genre average adsites" 

gen content_g = .
replace content_g = content14frauen if genre == "14frauen"
replace content_g = contentaktuelle if genre == "aktuelle"
replace content_g = contentcar if genre == "car"
replace content_g = contentcomputer if genre == "computer"
replace content_g = contentecon if genre == "econ"
replace content_g = contenteltern if genre == "eltern"
replace content_g = contentess if genre == "ess"
replace content_g = contentliving if genre == "living"
replace content_g = contentmonfrauen if genre == "monfrauen"
replace content_g = contentmusic if genre == "music"
replace content_g = contentsport if genre == "sport"
replace content_g = contenttv if genre == "tv"
replace content_g = contentwofrauen if genre == "wofrauen"
label var content_g "Genre average content" 

gen adprice_g = .
replace adprice_g = adprice14frauen if genre == "14frauen"
replace adprice_g = adpriceaktuelle if genre == "aktuelle"
replace adprice_g = adpricecar if genre == "car"
replace adprice_g = adpricecomputer if genre == "computer"
replace adprice_g = adpriceecon if genre == "econ"
replace adprice_g = adpriceeltern if genre == "eltern"
replace adprice_g = adpriceess if genre == "ess"
replace adprice_g = adpriceliving if genre == "living"
replace adprice_g = adpricemonfrauen if genre == "monfrauen"
replace adprice_g = adpricemusic if genre == "music"
replace adprice_g = adpricesport if genre == "sport"
replace adprice_g = adpricetv if genre == "tv"
replace adprice_g = adpricewofrauen if genre == "wofrauen"
label var adprice_g "Genre average adprice" 

gen cprice_g = .
replace cprice_g = cprice14frauen if genre == "14frauen"
replace cprice_g = cpriceaktuelle if genre == "aktuelle"
replace cprice_g = cpricecar if genre == "car"
replace cprice_g = cpricecomputer if genre == "computer"
replace cprice_g = cpriceecon if genre == "econ"
replace cprice_g = cpriceeltern if genre == "eltern"
replace cprice_g = cpriceess if genre == "ess"
replace cprice_g = cpriceliving if genre == "living"
replace cprice_g = cpricemonfrauen if genre == "monfrauen"
replace cprice_g = cpricemusic if genre == "music"
replace cprice_g = cpricesport if genre == "sport"
replace cprice_g = cpricetv if genre == "tv"
replace cprice_g = cpricewofrauen if genre == "wofrauen"
label var cprice_g "Genre average adprice" 

* Testing instruments (remove */)
/*pwcorr sales sales_g, sig // corr. .72
pwcorr adprice adprice_g, sig // corr. .63
pwcorr cprice cprice_g, sig // corr. .77
pwcorr adsite adsite_g, sig // corr. .698
pwcorr content content_g, sig // corr. .73

xtreg sales sales_g adprice_g adsite_g cprice_g content_g 
xtreg adprice sales_g adprice_g adsite_g cprice_g content_g 
xtreg cprice sales_g adprice_g adsite_g cprice_g content_g
xtreg adsite sales_g adprice_g adsite_g cprice_g content_g
xtreg content sales_g adprice_g adsite_g cprice_g content_g */


* Calculating market share
* 1) 14frauen
gen share_g =.
gen adshare_g =.

bysort year : egen dmnd14frauen = total(sales) ///
 if genre == "14frauen" //sum up sales 
label variable dmnd14frauen "total Demand in genre 14frauen"

replace share_g = sales/dmnd14frauen if genre == "14frauen" 

bysort year : egen addmnd14frauen = total(adsite) ///
 if genre == "14frauen" //sum up sales 
label variable addmnd14frauen "total Ad Demand in genre 14frauen"

replace adshare_g = adsite/addmnd14frauen if genre == "14frauen" 

* 2) aktuelle
bysort year : egen dmndaktuelle = total(sales) ///
 if genre == "aktuelle" //sum up sales 
label variable dmndaktuelle "total Demand in genre aktuelle"

replace share_g = sales/dmndaktuelle if genre == "aktuelle" 

bysort year : egen addmndaktuelle = total(adsite) ///
 if genre == "aktuelle" //sum up sales 
label variable addmndaktuelle "total Ad Demand in genre aktuelle"

replace adshare_g = adsite/addmndaktuelle if genre == "aktuelle" 

* 3) car
bysort year : egen dmndcar = total(sales) ///
 if genre == "car" //sum up sales 
label variable dmndcar "total Demand in genre car"

replace share_g = sales/dmndcar if genre == "car" 

bysort year : egen addmndcar = total(adsite) ///
 if genre == "car" //sum up sales 
label variable addmndcar "total Ad Demand in genre car"

replace adshare_g = adsite/addmndcar if genre == "car" 

* 4) computer
bysort year : egen dmndcomputer = total(sales) ///
 if genre == "computer" //sum up sales 
label variable dmndcar "total Demand in genre computer"

replace share_g = sales/dmndcomputer if genre == "computer" 

bysort year : egen addmndcomputer = total(adsite) ///
 if genre == "computer" //sum up sales 
label variable addmndcomputer "total Ad Demand in genre computer"

replace adshare_g = adsite/addmndcomputer if genre == "computer" 

* 5) econ
bysort year : egen dmndecon = total(sales) ///
 if genre == "econ" //sum up sales 
label variable dmndcar "total Demand in genre econ"

replace share_g = sales/dmndecon if genre == "econ" 

bysort year : egen addmndecon = total(adsite) ///
 if genre == "econ" //sum up sales 
label variable addmndecon "total Ad Demand in genre econ"

replace adshare_g = adsite/addmndecon if genre == "econ" 

* 6) eltern
bysort year : egen dmndeltern = total(sales) ///
 if genre == "eltern" //sum up sales 
label variable dmndcar "total Demand in genre eltern"

replace share_g = sales/dmndeltern if genre == "eltern" 

bysort year : egen addmndeltern = total(adsite) ///
 if genre == "eltern" //sum up sales 
label variable addmndeltern "total Ad Demand in genre eltern"

replace adshare_g = adsite/addmndeltern if genre == "eltern" 

* 7) ess
bysort year : egen dmndess = total(sales) ///
 if genre == "ess" //sum up sales 
label variable dmndess "total Demand in genre ess"

replace share_g = sales/dmndess if genre == "ess" 

bysort year : egen addmndess = total(adsite) ///
 if genre == "ess" //sum up sales 
label variable addmndess "total Ad Demand in genre ess"

replace adshare_g = adsite/addmndess if genre == "ess" 

* 8) living
bysort year : egen dmndliving = total(sales) ///
 if genre == "living" //sum up sales 
label variable dmndliving "total Demand in genre living"

replace share_g = sales/dmndliving if genre == "living" 

bysort year : egen addmndliving = total(adsite) ///
 if genre == "living" //sum up sales 
label variable addmndliving "total Ad Demand in genre living"

replace adshare_g = adsite/addmndliving if genre == "living" 

* 9) monfrauen
bysort year : egen dmndmonfrauen = total(sales) ///
 if genre == "monfrauen" //sum up sales 
label variable dmndmonfrauen "total Demand in genre monfrauen"

replace share_g = sales/dmndmonfrauen if genre == "monfrauen" 

bysort year : egen addmndmonfrauen = total(adsite) ///
 if genre == "monfrauen" //sum up sales 
label variable addmndmonfrauen "total Ad Demand in genre monfrauen"

replace adshare_g = adsite/addmndmonfrauen if genre == "monfrauen"

* 10) music
bysort year : egen dmndmusic = total(sales) ///
 if genre == "music" //sum up sales 
label variable dmndmusic "total Demand in genre music"

replace share_g = sales/dmndmusic if genre == "music" 

bysort year : egen addmndmusic = total(adsite) ///
 if genre == "music" //sum up sales 
label variable addmndmusic "total Ad Demand in genre music"

replace adshare_g = adsite/addmndmusic if genre == "music"

* 11) sport
bysort year : egen dmndsport = total(sales) ///
 if genre == "sport" //sum up sales 
label variable dmndsport "total Demand in genre sport"

replace share_g = sales/dmndsport if genre == "sport" 

bysort year : egen addmndsport = total(adsite) ///
 if genre == "sport" //sum up sales 
label variable addmndsport "total Ad Demand in genre sport"

replace adshare_g = adsite/addmndsport if genre == "sport"

* 12) tv
bysort year : egen dmndtv = total(sales) ///
 if genre == "tv" //sum up sales 
label variable dmndtv "total Demand in genre tv"

replace share_g = sales/dmndtv if genre == "tv" 

bysort year : egen addmndtv = total(adsite) ///
 if genre == "tv" //sum up sales 
label variable addmndtv "total Ad Demand in genre tv"

replace adshare_g = adsite/addmndtv if genre == "tv"

* 13) wofrauen
bysort year : egen dmndwofrauen = total(sales) ///
 if genre == "wofrauen" //sum up sales 
label variable dmndwofrauen "total Demand in genre wofrauen"

replace share_g = sales/dmndwofrauen if genre == "wofrauen" 

bysort year : egen addmndwofrauen = total(adsite) ///
 if genre == "wofrauen" //sum up sales 
label variable addmndwofrauen "total Ad Demand in genre wofrauen"

replace adshare_g = adsite/addmndwofrauen if genre == "wofrauen"

save "C:\Users\Jorgito\Dropbox\Paper\Paper\2sm\Stata\panel_all.dta", replace
* Discover Data (remove */) 

/*xtline share if genre == "14frauen", overlay
xtline share if genre == "aktuelle", overlay
xtline share if genre == "car", overlay
xtline share if genre == "computer", overlay
xtline share if genre == "econ", overlay
xtline share if genre == "eltern", overlay
xtline share if genre == "ess", overlay
xtline share if genre == "living", overlay
xtline share if genre == "monfrauen", overlay
xtline share if genre == "music", overlay
xtline share if genre == "sport", overlay
xtline share if genre == "tv", overlay
xtline share if genre == "wofrauen", overlay

xtline adshare if genre == "14frauen", overlay
xtline adshare if genre == "aktuelle", overlay
xtline adshare if genre == "car", overlay
xtline adshare if genre == "computer", overlay
xtline adshare if genre == "econ", overlay
xtline adshare if genre == "eltern", overlay
xtline adshare if genre == "ess", overlay
xtline adshare if genre == "living", overlay
xtline adshare if genre == "monfrauen", overlay
xtline adshare if genre == "music", overlay
xtline adshare if genre == "sport", overlay
xtline adshare if genre == "tv", overlay
xtline adshare if genre == "wofrauen", overlay*/
