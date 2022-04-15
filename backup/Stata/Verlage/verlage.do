** adprice 
import excel "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlag_adprice.xls", sheet("Einzelhefte") firstrow

format EVT %td
gen quad = qofd(EVT)
format quad %tq

rename Verlag verlag
collapse Preis4C11, by(quad verlag)

replace verlag = "bmg" if verlag == "Bauer Advertising KG"
replace verlag = "cnv" if verlag == "Condé Nast Verlag GmbH"
replace verlag = "guj" if verlag == "Gruner + Jahr GmbH & Co KG"
replace verlag = "hbm" if verlag == "Hubert Burda Media"
replace verlag = "mps" if verlag == "Motor Presse Stuttgart GmbH & Co. KG"
replace verlag = "vmg" if verlag == "Vision Media GmbH"

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_adprice.dta"

** adsites
clear
import excel "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlag_adtotal.xls", sheet("Einzelhefte") firstrow

format EVT %td
gen quad = qofd(EVT)
format quad %tq

rename Verlag verlag
collapse Copypreis AnzeigenGesamt AnzeigenSeiten, by(quad verlag)

rename AnzeigenGesamt adtotal
rename AnzeigenSeiten adsites


replace verlag = "bmg" if verlag == "Bauer Advertising KG"
replace verlag = "cnv" if verlag == "Condé Nast Verlag GmbH"
replace verlag = "guj" if verlag == "Gruner + Jahr GmbH & Co KG"
replace verlag = "hbm" if verlag == "Hubert Burda Media"
replace verlag = "mps" if verlag == "Motor Presse Stuttgart GmbH & Co. KG"
replace verlag = "vmg" if verlag == "Vision Media GmbH"

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_adtotal.dta"

** sales 
clear
import excel "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlag_circ.xls", sheet("Einzelhefte") firstrow

format EVT %td
gen quad = qofd(EVT)
format quad %tq

rename Verlag verlag
rename VerkaufGesamt sales

collapse sales, by(quad verlag)

replace verlag = "bmg" if verlag == "Bauer Advertising KG"
replace verlag = "cnv" if verlag == "Condé Nast Verlag GmbH"
replace verlag = "guj" if verlag == "Gruner + Jahr GmbH & Co KG"
replace verlag = "hbm" if verlag == "Hubert Burda Media"
replace verlag = "mps" if verlag == "Motor Presse Stuttgart GmbH & Co. KG"
replace verlag = "vmg" if verlag == "Vision Media GmbH"

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_sales.dta"

** content
clear
import excel "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlag_content.xls", sheet("Einzelhefte") firstrow

format EVT %td
gen quad = qofd(EVT)
format quad %tq

rename Verlag verlag
rename Heftumfang content
rename UmsatzBruttoTEUR rev

collapse content rev, by(quad verlag)

replace verlag = "bmg" if verlag == "Bauer Advertising KG"
replace verlag = "cnv" if verlag == "Condé Nast Verlag GmbH"
replace verlag = "guj" if verlag == "Gruner + Jahr GmbH & Co KG"
replace verlag = "hbm" if verlag == "Hubert Burda Media"
replace verlag = "mps" if verlag == "Motor Presse Stuttgart GmbH & Co. KG"
replace verlag = "vmg" if verlag == "Vision Media GmbH"

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_content.dta"

** Merge Datasets

merge m:m quad verlag using "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_adprice.dta", nogenerate
merge m:m quad verlag using "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_adtotal.dta", nogenerate
merge m:m quad verlag using "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_sales.dta", nogenerate

rename Preis4C11 adprice
rename Copypreis cprice

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_long_ab2003.dta", replace

** Collapse by Year
gen date = dofq(quad)
format date %td

gen jahr = yofd(date)
format jahr %ty

collapse content rev cprice adtotal adsites adprice sales, by(jahr verlag) // yearly av. data

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_long_ab2003_yearly.dta", replace

** Import earlier dataset 
import excel "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\panel_alle.xls", sheet("Tabelle1") firstrow

format jahr %ty

rename verkauf sales
rename umfang content
rename anzeigen adsites
rename p4c adprice
rename p cprice


collapse druck verbreitung sales abos EV content adsites adprice cprice beihefte, by ( jahr titel genre verlag frequenz)

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\panel_all.dta", replace

* Publisher Average Variables
collapse druck verbreitung sales abos EV content adsites adprice cprice beihefte, by (jahr verlag)
rename adprice adprice2
save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_long_bis2006.dta"

** Merge Datasets (ab 2003 und bis 2003)
clear
use "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_long_ab2003_yearly.dta", clear
merge m:m jahr verlag using "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_long_bis2006.dta"

replace adprice = adprice2 if adprice ==.

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_long.dta"

** Keep Obs if Jahr > 2002 
keep if jahr > 2002

** Reshape Dataset from long to wide
drop adprice2 _merge

reshape wide content rev cprice adtotal adsites adprice sales druck verbreitung abos EV beihefte , i(jahr) j(verlag) string

save "C:\Users\Jorgito\Dropbox\Paper\2sm\Stata\Verlage\verlage_all_wide.dta"
