clear all

cd "C:\\Users\\Tomas\\Desktop\\RepTwfe"

import delimited "C:\Users\Tomas\Desktop\RepTwfe\dataTWFE.csv", encoding(ISO-8859-2) 

bys cohort_year year: egen meanc = mean(dep_var)




* Generamos las dummies.

gen rel_year = year - cohort_year

forval i= -5(1)5{
	if `i' < 0{
		local n = abs(`i')
		gen rel_year_m`n' = (rel_year == `i')
	} 
	else{
		gen rel_year_`i' = (rel_year == `i')
	}
}

* Una variable que sea Pre y Post

gen pre = (rel_year <-5)
gen post = (rel_year > 5)

* Corremos el primer modelo

reghdfe dep_var pre rel_year_m5 rel_year_m4 rel_year_m3 rel_year_m2  rel_year_0 rel_year_1 rel_year_2 rel_year_3 rel_year_4 rel_year_5 post, a(unit year) cl(state) 

*https://www.matthieugomez.com/statar/regressions.html

mat B = e(b)'

mat results = J(1,1,.)

mat C = B[2..11,1]

mat results = results \ C
mat results = results \ C
mat results = results \ C
mat results = results \ C

clear

svmat results
drop in 1

gen t = .

local time = -5
forval i = 1(1)1000{
	replace t = `time' in `i'
	local time = `time' + 1
	if `time' == -1{
		local time = 0 	
	}
	else{
	if `time' == 6{
		local time = -5
		}
	}
}

* Creamos los intervalos de confianza
	
bys t : egen mt = mean(results1)	
bys t : egen sdt = sd(results1)	

gen lower_ic = mt - 1.96*0.32
gen upper_ic = mt + 1.96*0.20


	
collapse (mean) results1 lower_ic upper_ic, by(t) 

insobs 1
replace t = -1 in 11 	
gen a = 0
gen real_treatment = t + 1 if t >=-1 
replace results1 = 0 if t == -1

sort t
#delimit;
twoway (rarea upper_ic lower_ic t, color(gs14))
(connected results1 t, lcolor(blue) mcolor(blue))
(line a t, lpattern(dash) lcolor(black) lwidth(thin))

(line a t if t <= -1, lcolor(red) lpattern(dash))
(line real_treatment t if t >=-1, lcolor(red) lpattern(dash))

, scheme(s1color) xlabel(-5(1)5)
legend(order(2 "Estimated effect" 4 "True effect"))
title("TWFE event-study regression with binned end-points")
;





#delimit;
twoway 
(line dep_var year, lwidth(vvvthin) lcolor(gray))
(line meanc year if cohort_year == 1986, lwidth(thick) lcolor(red)) 
(line meanc year if cohort_year == 1992, lwidth(thick) lcolor(navy)) 
(line meanc year if cohort_year == 1998, lwidth(thick) lcolor(green)) 
(line meanc year if cohort_year == 2004, lwidth(thick) lcolor(purple)),


 ytitle("Value") xtitle("Time") scheme(s1color) xline(1986, lcolor(red) lwidth(thick)) xline(1992, lcolor(navy) lwidth(thick)) xline(1998, lcolor(green) lwidth(thick)) xline(2004, lcolor(purple) lwidth(thick))
 
 legend(order(1 "" 2 "1986" 3 "1992" 4 "1998" 5 "2004") col(5))
 
 title("One draw of the DGP with homogeneous effects across" "cohorts and with all groups being eventually treated")
;





clear all

cd "C:\\Users\\Tomas\\Desktop\\RepTwfe"

import delimited "C:\Users\Tomas\Desktop\RepTwfe\dataTWFE.csv", encoding(ISO-8859-2) 

bys cohort_year year: egen meanc = mean(dep_var)





gen rel_year = year - cohort_year

* Hay que hacer como un recode. Hacemos una variable con el año minimo

egen minimo_r = min(rel_year)

replace rel_year = rel_year - minimo_r

summ rel_year

drop minimo_r






















