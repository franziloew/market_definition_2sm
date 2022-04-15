program define myreg, rclass

drop _all

set obs 1000

** Marginal Cost consist of a common-market shock and a asymmetric/firm-specific market shock
** Market 1
gen shock_c1 = runiform(0,0.01) //common shock in market1
gen shock_c11 = runiform(0,0.01)   //asymmetric shock in market1/firm1
gen shock_c12 = runiform(0,0.01)   //asymmetric shock in market1/firm2

gen c11 = shock_c1+shock_c11      //overall shock firm1 in market1
gen c12 = shock_c1+shock_c12     //overall shock firm2 in market1

** Market 2
gen shock_c2 = runiform(0,0.01) //common shock in market2
gen shock_c21 = runiform(0,0.01)   //asymmetric shock in market2/firm1
gen shock_c22 = runiform(0,.001)   //asymmetric shock in market2/firm2

gen c21 = shock_c1+shock_c11      //overall shock firm1 in market2
gen c22 = shock_c1+shock_c12      //overall shock firm2 in market2

** Indirect Network-Effects (INE)
gen d = runiform(0,0.5)     // INE from Market2 on Market1
gen g = runiform(0,0.5)     // INE from Market1 on Market2

** Degree of Homogeneity with 1=Total Substitutes 
gen w = uniform()         // Homogeneity on Market1
gen p = uniform()         // Homogeneity on Market2

** Profit Function

gen q1 = ""
gen q2 = ""
gen s1 = ""
gen s2 = ""
destring q1, replace
destring q2, replace
destring s1, replace
destring s2, replace


gen pi = 1+d*s1+g*s1-q2*w-c11-2*q1 // Profit Function Firm1
