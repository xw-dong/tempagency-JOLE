/*
	- Sales record `price.dta' (firm-sourced)
	- Worker's info `info.dta' (firm-sourced)
	- Worker's date of entering the firm `entry.dta' (firm-sourced)
	- Aptitude test scores dataset `aptitude.dta' (firm-sourced)
	- Regional CPI `regioncpi.dta'
	- Regional unemployment rate `regionunemp.dta'
in the current working directory required
*/

********************************************************
************ First, construct the full sample **********
********************************************************

//// 1. Merge price, info
use price, clear
merge m:1 ハッシュID using info.dta, nogenerate
merge m:1 ハッシュID using entry.dta, nogenerate
merge m:1 ハッシュID using aptitude.dta, nogenerate
encode ハッシュID, generate(numid) // numid = worker's id
egen worker_cli_id = group(numid senttocode)
g nmonth = monthly(month, "YM")
format nmonth %tm
label variable nmonth "計上月" // nmonth = formatted year-month


//// 2. Drop observations with missing information
drop if missing(nmonth) // representative of price info, 71743 obs
drop if missing(position_cate) // representative of basic info, 64339 obs
drop if missing(entrydate)
drop if missing(overall_assessment) // representative of aptitude, 56628 obs
drop if branch == 4 // drop observations from one branch with limited sample size, 56441 obs
g female = 1-gender
g univ = cond(education_cate>2,1,0)



//// 3. Calculate tenure
g tenuretot = nmonth - entrydate + 1 



//// 4. Generate age variables
g nyear = floor(nmonth/12) + 1960
order nyear, after(nmonth)
g nquarter = floor(nmonth/3)
order nquarter, after(nmonth)
format nquarter %tq
g age = nyear - birthyear
g age_entry= entryyr - birthyear
g experience = age - education_year - 6
g experience_entry = age_entry - education_year - 6
replace experience_entry = 0 if experience_entry<0
replace experience = 0 if experience<0



//// 5. Rescale
g age_entry_scaled = age_entry/10
su age_entry_scaled
g age_entry_dev = age_entry_scaled - r(mean)
g experience_scaled = experience/10
su experience_scaled
g experience_dev = experience_scaled - r(mean)
g experience_entry_scaled = experience_entry/10
su experience_entry_scaled
g experience_entry_dev = experience_entry_scaled - r(mean)
g tenure_year = tenure/12
g tenure_scaled = tenure_year/10
su tenure_scaled



//// 6. Merge regional CPI and regional unemployment rate
g str region = ""
replace region = "Tokai" if branch==1
replace region = "Kinki" if branch==2
replace region = "SouthernKanto" if branch==3 | branch==6
replace region = "KyushuOkinawa" if branch==7
merge m:1 region nmonth using regioncpi
drop if _merge==2
drop _merge
merge m:1 region nquarter using regionunemp
drop if _merge==2
drop _merge



//// 7. Drop early and late entries
drop if entrydate < (2015 - 1960) * 12 + 4 - 1 
drop if entrydate > (2019 - 1960) * 12 + 11 - 1 // 35924 obs



//// 8. Drop outliers with long tenures
sort numid nmonth
bysort numid: g n = _n
bysort numid: g N = _N
g temp = .
replace temp = tenuretot if n==N
bysort numid: egen tenuretotori = mean(temp) // record worker's longest tenure before dropping top 1%
drop temp n N
centile tenuretot, centile(99)  // = 48 months                  
keep if tenuretot<r(c_1)
save sampleS, replace // 35564 obs



//// 9. Order of clients for each worker
use sampleS, clear
sort ハッシュID nmonth senttocode
keep ハッシュID senttocode
duplicates drop
bysort ハッシュID: g ordclient=_n
save ordclient, replace

use sampleS, replace
merge m:1 ハッシュID senttocode using ordclient
drop if _merge==2
drop _merge
sort ハッシュID nmonth senttocode



//// 10. Fee and wage related variables

// original monthly amounts
g lbillingamount_noised = log(billingamount_noised)
g lcost_noised = log(cost_noised)
g markup_abs = billingamount_noised - cost_noised
g lmarkup_abs = log(markup_abs)


// hourly fee = fee of each record/hours of each record
g lhoursworked = log(hoursworked)
g billingamount_hr = billingamount_noised/hoursworked
g lbillingamount_hr = log(billingamount_hr)
g markup_abs_hr = markup_abs/hoursworked
g lmarkup_abs_hr = ln(markup_abs_hr)


// hourly wage = wage of each record (equ. monthly wage)/monthly hours
sort ハッシュID nmonth senttocode
bysort numid nmonth: egen billingamount_m = total(billingamount_noised)
bysort numid nmonth: egen hoursworked_m = total(hoursworked)
label variable hoursworked_m "monthly hours worked"

g cost_hr = cost_noised/hoursworked_m
g lcost_hr = log(cost_hr)

g markup_rel = billingamount_hr/cost_hr
g lmarkup_rel = log(markup_rel)



//// 11. Initial training period
sort numid nmonth
bysort numid: g n = _n
g initialblank = .
replace initialblank = tenuretot - 1 if n==1
bysort numid: egen temp=mean(initialblank)
replace initialblank = temp 
drop temp n


save sampleS, replace


********************************************************
*********** End constructing the full sample ***********
********************************************************
******** Start constructing the analysis sample ********
********************************************************


use sampleS, clear


//// 1. Drop worker-client pairs lasting shorter than 3 months
sort numid nmonth senttocode
bysort numid senttocode: g N=_N
sort numid senttocode nmonth
bysort numid senttocode: g n=_n
sort numid nmonth senttocode
drop if N<3

//// 2. Wage adjustments for first and last worker-client months
g temp1 = .
replace temp1 = cost_hr if n==2
bysort numid senttocode: egen temp2 = mean(temp1)
replace cost_hr = temp2 if n==1
drop temp*

g temp1 = .
replace temp1 = cost_hr if n==N-1
bysort numid senttocode: egen temp2 = mean(temp1)
replace cost_hr = temp2 if n==N
drop temp* n N

replace lcost_hr = log(cost_hr)
replace markup_rel = billingamount_hr/cost_hr
replace lmarkup_rel = log(markup_rel)

//// 3. Generate `tenureini' and `tenure2' to indicate first and second month of tenure
sort numid nmonth
bysort numid: g n = _n
g tenureini=.
replace tenureini = tenuretot if n==1
g tenure2=.
replace tenure2 = tenuretot if n==2
bysort numid: egen temp1=mean(tenureini)
replace tenureini = temp1
bysort numid: egen temp2=mean(tenure2)
replace tenure2 = temp2
drop temp* n

g lbillingamount_m = log(billingamount_m)
g lmarkup_rel_m = lbillingamount_m - lcost_noised
g lhoursworked_m = log(hoursworked_m)


//// 4. Generate fee and wage related variables at first and second month of tenure
foreach x in billingamount_hr cost_hr markup_rel lbillingamount_hr lcost_hr lmarkup_rel cost_noised lbillingamount_m lcost_noised lmarkup_rel_m lhoursworked_m{
	g `x'_ini = .
	replace `x'_ini = `x' if tenuretot == tenureini
	bysort numid: egen temp = mean(`x'_ini)
	replace `x'_ini = temp
	drop temp
	g `x'_2 = .
	replace `x'_2 = `x' if tenuretot == tenure2
	bysort numid: egen temp = mean(`x'_2)
	replace `x'_2 = temp
	drop temp
}

save sampleS_main, replace


********************************************************
******* End of constructing the analysis sample ********
********************************************************
