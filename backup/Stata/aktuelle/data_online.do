clear all
set more off

*** Time var

tostring Monat, replace
tostring Jahr, replace

gen month1 = Monat + "/" + Jahr

gen month = monthly(month1,"my")
format month %tm

drop month1 Jahr Monat


*******

rename UniqueUserMonatMio user
rename VisitsGesamtGesamt visits
rename Dachmarke title

*** Reshape Data

replace title = subinstr(title," ","",.)

replace title = "FOCUS" if title == "FOCUSONLINE"
replace title = "Stern" if title == "stern.de"
replace title = "Spiegel" if title == "SPIEGELONLINE"

ds title month, not

reshape wide `r(varlist)', i(month) j(title) string

tsset month
