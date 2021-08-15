clear all

cd "C:\\Users\\Tomas\\Desktop\\RepTwfe"

import delimited "C:\Users\Tomas\Desktop\RepTwfe\dataTWFE.csv", encoding(ISO-8859-2) 

bys cohort_year year: egen meanc = mean(dep_var) 


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


* 

gen rel_year = year - cohort_year
tab rel_year, gen(rel_yea)