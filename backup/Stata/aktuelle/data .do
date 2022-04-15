clear all
set more off

*** weekly

gen week = weekly(HeftNr,"wy")

format week %tw

drop if week == .

*** monthly

gen month1 = substr(HeftNr,1,2)

gen year1 = substr(HeftNr,5,8)

gen time = month1 + "/" + year1

gen month = monthly(time,"my")
format month %tm


****** every 2 weeks
*** A

gen week1 = substr(HeftNr,1,2)
destring week1, gen(week2)
gen week = week2+week2
tostring week, replace

gen year1 = substr(HeftNr,5,8)

gen time1 = week + "/" + year1

drop week week1 week2 year 

gen week = weekly(time1,"wy")
format week %tw

drop if week == .

*** B
gen week1 = substr(HeftNr,1,2)
gen year1 = substr(HeftNr,3,.)

destring week1,gen(week) ignore("/")
destring year1,gen(year) ignore("/")

sort year week

gen time = _n+1

** To monthly 
gen date = dofw(week)
format date %td

gen month = mofd(date)
format month %tm

drop date week

ds month Titel, not
collapse `r(varlist)', by(month Titel)
tsset month


*******

destring Copypreis, gen(cprice1) ignore(",")
gen cprice = cprice1/100

keep if Titel == "TV Spielfilm" | Titel == "TV Movie" | Titel == "TV Today"

rename VerbreitungGesamt circ
rename VerkaufGesamt sales
rename AboExemplareGesamt abo
rename EinzelVKGesamt retail

rename AnzeigenGesamt totalads
rename AnzeigenSeiten totaladsite
rename Heftumfang content
gen adsite = content/totaladsite

rename Preis4C11 adprice

gen aboshare = sales/abo

gen log_circ=log(circ)
gen log_sales=log(sales)
gen log_retail=log(retail)
gen log_totalads=log(totalads)
gen log_adprice=log(adprice)
gen log_adsite=log(adsite)
gen log_totaladsite = log(totaladsite)
gen log_content = log(content)
gen log_cprice=log(cprice)


*** Reshape Data

replace Titel = subinstr(Titel," ","",.)
replace HeftNr = subinstr(HeftNr," ","",.)
ds Titel week, not

reshape wide `r(varlist)', i(week) j(Titel) string

tsset week
