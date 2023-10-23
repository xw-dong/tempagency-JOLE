/*
	- Full sample `sampleS.dta'
	- Analysis sample `sampleS_main.dta'
in the current working directory required
*/



////////////////////////////////////////////////////////////////
//////////// table 1: summary stat at worker-level /////////////
////////////////////////////////////////////////////////////////

use sampleS_main, clear
sort numid nmonth
bysort numid: g n=_n
bysort numid: g N=_N
keep if n==N
drop n N
save sampleS_main_worker, replace

use sampleS, clear
sort numid nmonth
bysort numid: g n=_n
bysort numid: g N=_N
keep if n==N
drop n N
save sampleS_worker, replace

use sampleS_worker, clear
append using sampleS_main_worker
bysort numid: g N=_N
drop if N==2
drop N
save sampleS_worker_excluded, replace

use infoentry, clear
append using sampleS_worker
bysort ハッシュID: g N=_N
drop if N==2
drop N
replace age_entry = entryyr - birthyear
replace experience_entry = age_entry - education_year - 6
replace female = 1 - gender
save sampleS_worker_never, replace


// never
use sampleS_worker_never, clear
g tenure_max = resignation_month - entrydate + 1
replace tenure_max = 721 - entrydate + 1 if !retired
qui su tenure_max
replace tenure_max = `r(mean)' if tenure_max==.
g tenure_comp = tenure_max if retired==1
g tenure_incomp = tenure_max if retired==0
g recenthire = cond(entrydate>715,1,0)
g retired_recentno = retired if recenthire==0
g retired_recentyes = retired if recenthire==1

label variable female "Female"
label variable univ "Education: 4-year University+"
label variable age_entry "Age at entry"
label variable experience_entry "Potential work experience at entry"
label variable retired "Exited firm"
label variable tenure_max "Max tenure"
label variable tenure_comp "Completed tenure"
label variable tenure_incomp "Max incompleted tenure"
label variable recenthire "Recent hires (Sep-Nov 2019)"
label variable retired_recentno "Non-recent hire exits"
label variable retired_recentyes "Recent hire exits"

keep female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes
order female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes
outreg2 using tabsums_worker.tex, sum(log) eqkeep(mean sd N) cttop(Never placed) keep(female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes) title("Summary statistics") label tex replace 

// excluded
use sampleS_worker_excluded, clear
g tenure_max = resignation_month - entrydate + 1
replace tenure_max = 721 - entrydate + 1 if !retired
qui su tenure_max
replace tenure_max = `r(mean)' if tenure_max==.
g tenure_comp = tenure_max if retired==1
g tenure_incomp = tenure_max if retired==0
g recenthire = cond(entrydate>715,1,0)
g retired_recentno = retired if recenthire==0
g retired_recentyes = retired if recenthire==1

label variable female "Female"
label variable univ "Education: 4-year University+"
label variable age_entry "Age at entry"
label variable experience_entry "Potential work experience at entry"
label variable retired "Exited firm"
label variable tenure_max "Max tenure"
label variable tenure_comp "Completed tenure"
label variable tenure_incomp "Max incompleted tenure"
label variable initialblank "Training period"
label variable recenthire "Recent hires (Sep-Nov 2019)"
label variable retired_recentno "Non-recent hire exits"
label variable retired_recentyes "Recent hire exits"

merge 1:m numid using sampleS
keep if _merge == 3
sort numid nmonth
bysort numid nmonth: g n=_n
bysort numid nmonth: g N=_N
keep if n==N

keep female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes
order female age_entry univ experience_entry initialblank retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes
outreg2 using tabsums_worker.tex, sum(log) eqkeep(mean sd N) cttop(Excluded) keep(female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes) title("Summary statistics") label tex

// main
use sampleS_main_worker, clear
g tenure_max = resignation_month - entrydate + 1
replace tenure_max = 721 - entrydate + 1 if !retired
qui su tenure_max
replace tenure_max = `r(mean)' if tenure_max==.
g tenure_comp = tenure_max if retired==1
g tenure_incomp = tenure_max if retired==0
g recenthire = cond(entrydate>715,1,0)
g retired_recentno = retired if recenthire==0
g retired_recentyes = retired if recenthire==1

label variable female "Female"
label variable univ "Education: 4-year University+"
label variable age_entry "Age at entry"
label variable experience_entry "Potential work experience at entry"
label variable retired "Exited firm"
label variable tenure_max "Max tenure"
label variable tenure_comp "Completed tenure"
label variable tenure_incomp "Max incompleted tenure"
label variable initialblank "Training period"
label variable recenthire "Recent hires (Sep-Nov 2019)"
label variable retired_recentno "Non-recent hire exits"
label variable retired_recentyes "Recent hire exits"

keep female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes
order female age_entry univ experience_entry initialblank retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes
outreg2 using tabsums_worker.tex, sum(log) eqkeep(mean sd N) cttop(Main) keep(female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes) title("Summary statistics") label tex

// main - male
use sampleS_main_worker, clear
g tenure_max = resignation_month - entrydate + 1
replace tenure_max = 721 - entrydate + 1 if !retired
qui su tenure_max
replace tenure_max = `r(mean)' if tenure_max==.
g tenure_comp = tenure_max if retired==1
g tenure_incomp = tenure_max if retired==0
g recenthire = cond(entrydate>715,1,0)
g retired_recentno = retired if recenthire==0
g retired_recentyes = retired if recenthire==1


label variable female "Female"
label variable univ "Education: University+"
label variable age_entry "Age at entry"
label variable experience_entry "Potential work experience at entry"
label variable retired "Exited firm"
label variable tenure_max "Max tenure"
label variable tenure_comp "Completed tenure"
label variable tenure_incomp "Max incompleted tenure"
label variable initialblank "Training period"
label variable recenthire "Recent hires (Sep-Nov 2019)"
label variable retired_recentno "Non-recent hire exits"
label variable retired_recentyes "Recent hire exits"

keep if female==0

keep female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes
order female age_entry univ experience_entry initialblank retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes
outreg2 using tabsums_worker.tex, sum(log) eqkeep(mean sd N) cttop(Main: Male) keep(female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes) title("Summary statistics") label tex


// main - female
use sampleS_main_worker, clear
g tenure_max = resignation_month - entrydate + 1
replace tenure_max = 721 - entrydate + 1 if !retired
qui su tenure_max
replace tenure_max = `r(mean)' if tenure_max==.
g tenure_comp = tenure_max if retired==1
g tenure_incomp = tenure_max if retired==0
g recenthire = cond(entrydate>715,1,0)
g retired_recentno = retired if recenthire==0
g retired_recentyes = retired if recenthire==1


label variable female "Female"
label variable univ "Education: University+"
label variable age_entry "Age at entry"
label variable experience_entry "Potential work experience at entry"
label variable retired "Exited firm"
label variable tenure_max "Max tenure"
label variable tenure_comp "Completed tenure"
label variable tenure_incomp "Max incompleted tenure"
label variable initialblank "Training period"
label variable recenthire "Recent hires (Sep-Nov 2019)"
label variable retired_recentno "Non-recent hire exits"
label variable retired_recentyes "Recent hire exits"

keep if female==1


keep female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes
order female age_entry univ experience_entry initialblank retired tenure_max tenure_comp tenure_incomp recenthire retired_recentno retired_recentyes
outreg2 using tabsums_worker.tex, sum(log) eqkeep(mean sd N) cttop(Main: Female) keep(female age_entry univ experience_entry retired tenure_max tenure_comp tenure_incomp initialblank recenthire retired_recentno retired_recentyes) title("Summary statistics") label tex


////////////////////////////////////////////////////////////////
///// table 2: summary stat at worker-month-level //////
////////////////////////////////////////////////////////////////


use sampleS_main, clear
qui tab numid
local n_workers = r(r)
qui tab senttocode
local n_clients = r(r)
keep numid senttocode female billingamount_hr cost_hr markup_rel hoursworked_m
order hoursworked_m billingamount_hr cost_hr markup_rel
label variable hoursworked_m "Hours worked"
label variable billingamount_hr "Hourly fee"
label variable cost_hr "Hourly wage"
label variable markup_rel "Relative markup"
outreg2 using tabsums_workermonth.tex, sum(log) eqkeep(mean sd N) cttop(Main) keep(billingamount_hr cost_hr markup_rel hoursworked_m) title("Summary statistics") addstat(No. Workers, `n_workers', No. Clients, `n_clients') label tex replace
qui tab numid if female==0
local n_workers = r(r)
qui tab senttocode if female==0
local n_clients = r(r)
outreg2 using tabsums_workermonth.tex if female==0, sum(log) eqkeep(mean sd N) cttop(Male) keep(billingamount_hr cost_hr markup_rel hoursworked_m) title("Summary statistics") label tex 
qui tab numid if female==1
local n_workers = r(r)
qui tab senttocode
local n_clients = r(r) if female==1
outreg2 using tabsums_workermonth.tex if female==1, sum(log) eqkeep(mean sd N) cttop(Female) keep(billingamount_hr cost_hr markup_rel hoursworked_m) title("Summary statistics") label tex




////////////////////////////////////////////////////////
////////// table 3 trainging period reg /////////
////////////////////////////////////////////////////////
use sampleS_main, clear
bysort numid: g n=_n
keep if n==1
drop n
g entrycohort = entryyr
replace entrycohort = entryyr - 1 if entrymo < 4
reg initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled i.female i.univ ib(3).branch, vce(cluster numid)
eststo iniblank1
reg initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled i.female i.univ ib(3).branch i.entrydate, vce(cluster numid)
eststo iniblank3
qui su initialblank
local smean = round(r(mean),0.001)
esttab iniblank1 iniblank3 using tabtrainingreg_s.tex, replace b(3) se r2 star(* 0.1 ** 0.05 *** 0.01) title("Determinants of initial training period measured in month") nomtitles noomitted /*
*/	order(experience_entry c.experience_entry_scaled#c.experience_entry_scaled 1.female) /*
*/	drop(_cons 0.female 0.univ *.branch *.entrydate) /*
*/	coeflabels(experience_entry "Potential experience at entry" c.experience_entry_scaled#c.experience_entry_scaled "Potential experience$^2$/100" 1.female "Female" 1.univ "University+") /*
*/	addnotes("Standard errors are reported in parentheses. Entry cohort is defined by the fiscal year-month of entry. Each model includes branch office fixed effects.") /*
*/	prefoot("[1em]" "Entry cohort FE & No & Yes\\[1em]" "\hline") /*
*/	postfoot("Sample mean & `smean' & `smean'\\[1em]" "\hline")





/////////////////////////////////////////////////////////////////
////////// table 4 & figure 2 survival estimate /////////
/////////////////////////////////////////////////////////////////



use sampleS_main, clear
sort numid nmonth
bysort numid: g n = _n
bysort numid: g N = _N
drop if n != N
replace retired=0 if tenuretotori>48

tab education_cate, g(educcate)
tab branch, g(brc)
label variable lbillingamount_hr_2 "Initial hourly fee"
label variable lcost_hr_2 "Initial hourly wage"
label variable lmarkup_rel_2 "Initial markup"
label variable lbillingamount_m_2 "Initial monthly fee"
label variable lcost_noised_2 "Initial monthly wage"
label variable lmarkup_rel_m_2 "Initial monthly markup"
label variable lhoursworked_m_2 "Initial hours worked"
label variable initialblank "Training period"
label variable female "Female"
label variable univ "University+"

stset tenuretot, failure(retired)

g experience_entry_scaled_sq = experience_entry_scaled*experience_entry_scaled

streg lbillingamount_m_2 lhoursworked_m_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
eststo lt1
streg lcost_noised_2 lhoursworked_m_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
eststo lt2
streg lmarkup_rel_m_2 lhoursworked_m_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
eststo lt3
streg lbillingamount_hr_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
eststo lt4
streg lcost_hr_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
eststo lt5
streg lmarkup_rel_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
eststo lt6
esttab lt1 lt2 lt3 lt4 lt5 lt6 using tabsurv_s_lognormal.tex, replace b(3) se pr2 star(* 0.1 ** 0.05 *** 0.01) label drop(brc*) order(initialblank lbillingamount_m_2 lcost_noised_2 lmarkup_rel_m_2 lhoursworked_m_2 lbillingamount_hr_2 lcost_hr_2 lmarkup_rel_2) coeflabels(experience_entry "Potential experience" experience_entry_scaled_sq "Potential experience$^2$/100" 1.female "Female" 1.univ "University+") title("Survival estimate based on Log-normal model: Tenure") scalars("ll Log lik." "chi2 Chi-squared")


qui streg lmarkup_rel_m_2 lhoursworked_m_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
predict S, surv
sts generate survkm = s
sts generate survkmlb = lb(s)
sts generate survkmub = ub(s)
stcurve, surv addplot(line survkm _t, sort lwidth(medium) lcolor(gs1)|| line survkmlb _t, sort lpattern(dash) lwidth(medium) lcolor(gs1) legend(order(1 "Lognormal" 2 "Kaplan-Meier" 3 "KM 95% CI")) || line survkmub _t, sort lpattern(dash)  lwidth(medium) lcolor(gs1)) title("") ylabel(0(0.2)1, grid) xtitle("Tenure in months") scheme(s1mono) lcolor(gs8) xlabel(0(12)120) ylabel(,angle(0)) ytitle("Survival probabiliy") range(0 120)
graph save figsurv_s_ln, replace
graph export "figsurv_s_ln.pdf", as(pdf) replace
predict indmt, mean time // mean = 58.6
_pctile indmt, p(33)
return list
predict indmedt, median time
predict xb, xb


/////////////////////////////////////////
////////// table 5 initial /////////
/////////////////////////////////////////


use sampleS_main, clear
sort numid nmonth
keep if tenuretot == tenureini
reg lbillingamount_hr initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled i.female i.univ ib(3).branch i.entrydate, vce(cluster numid)
eststo feeini
reg lcost_hr initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled i.female i.univ ib(3).branch i.entrydate, vce(cluster numid)
eststo wageini
reg lmarkup_rel initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled i.female i.univ ib(3).branch i.entrydate, vce(cluster numid)
eststo markupini
esttab feeini wageini markupini using tabinitial_s.tex, replace b(3) se r2 star(* 0.1 ** 0.05 *** 0.01) title("Determinants of hourly fee, wage and markup at the first pay") mtitles("log(fee)" "log(wage)" "log(markup)") noomitted drop(_cons 0.female 0.univ *.branch *.entrydate) coeflabels(initialblank "Training period" experience_entry "Potential experience at entry" c.experience_entry_scaled#c.experience_entry_scaled "Potential experience$^2$/100" 1.female "Female" 1.univ "University+") addnotes("Standard errors are reported in parentheses. Each model includes entry month-year and branch fixed effects.") 



//////////////////////////////////////////////
////////// table 6 homo linear /////////
//////////////////////////////////////////////


eststo clear
use sampleS_main, clear
g tenuretot_scaled = tenuretot/12

mat feeb = J(1,5,0)
mat feebse = J(1,5,0)
mat feebp = J(1,5,0)
mat feer2 = J(1,5,0)

mat wageb = J(1,5,0)
mat wagebse = J(1,5,0)
mat wagebp = J(1,5,0)
mat wager2 = J(1,5,0)

mat markupb = J(1,5,0)
mat markupbse = J(1,5,0)
mat markupbp = J(1,5,0)
mat markupr2 = J(1,5,0)

// model 1

reg lbillingamount_hr tenuretot_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
mat feer2[1,1] = e(r2)
qui lincom tenuretot_scaled
mat feeb[1,1] = `r(estimate)'
mat feebse[1,1] = `r(se)'
mat feebp[1,1] = `r(p)'

reg lcost_hr tenuretot_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
mat wager2[1,1] = e(r2)
qui lincom tenuretot_scaled
mat wageb[1,1] = `r(estimate)'
mat wagebse[1,1] = `r(se)'
mat wagebp[1,1] = `r(p)'

reg lmarkup_rel tenuretot_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
mat markupr2[1,1] = e(r2)
qui lincom tenuretot_scaled
mat markupb[1,1] = `r(estimate)'
mat markupbse[1,1] = `r(se)'
mat markupbp[1,1] = `r(p)'

// model 2
reghdfe lbillingamount_hr tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
mat feer2[1,2] = e(r2)
qui lincom tenuretot_scaled
mat feeb[1,2] = `r(estimate)'
mat feebse[1,2] = `r(se)'
mat feebp[1,2] = `r(p)'

reghdfe lcost_hr tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
mat wager2[1,2] = e(r2)
qui lincom tenuretot_scaled
mat wageb[1,2] = `r(estimate)'
mat wagebse[1,2] = `r(se)'
mat wagebp[1,2] = `r(p)'

reghdfe lmarkup_rel tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
mat markupr2[1,2] = e(r2)
qui lincom tenuretot_scaled
mat markupb[1,2] = `r(estimate)'
mat markupbse[1,2] = `r(se)'
mat markupbp[1,2] = `r(p)'

// model 3
reghdfe lbillingamount_hr tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat feer2[1,3] = e(r2)
qui lincom tenuretot_scaled
mat feeb[1,3] = `r(estimate)'
mat feebse[1,3] = `r(se)'
mat feebp[1,3] = `r(p)'

reghdfe lcost_hr tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat wager2[1,3] = e(r2)
qui lincom tenuretot_scaled
mat wageb[1,3] = `r(estimate)'
mat wagebse[1,3] = `r(se)'
mat wagebp[1,3] = `r(p)'

reghdfe lmarkup_rel tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid) 
mat markupr2[1,3] = e(r2)
qui lincom tenuretot_scaled
mat markupb[1,3] = `r(estimate)'
mat markupbse[1,3] = `r(se)'
mat markupbp[1,3] = `r(p)'


// model 4
reghdfe lbillingamount_hr tenuretot_scaled cpi unemp i.ordclient [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat feer2[1,4] = e(r2)
qui lincom tenuretot_scaled
mat feeb[1,4] = `r(estimate)'
mat feebse[1,4] = `r(se)'
mat feebp[1,4] = `r(p)'

reghdfe lcost_hr tenuretot_scaled cpi unemp i.ordclient [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat wager2[1,4] = e(r2)
qui lincom tenuretot_scaled
mat wageb[1,4] = `r(estimate)'
mat wagebse[1,4] = `r(se)'
mat wagebp[1,4] = `r(p)'

reghdfe lmarkup_rel tenuretot_scaled cpi unemp i.ordclient [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat markupr2[1,4] = e(r2)
qui lincom tenuretot_scaled
mat markupb[1,4] = `r(estimate)'
mat markupbse[1,4] = `r(se)'
mat markupbp[1,4] = `r(p)'

// model 5
reghdfe lbillingamount_hr tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
mat feer2[1,5] = e(r2)
qui lincom tenuretot_scaled
mat feeb[1,5] = `r(estimate)'
mat feebse[1,5] = `r(se)'
mat feebp[1,5] = `r(p)'

reghdfe lcost_hr tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
mat wager2[1,5] = e(r2)
qui lincom tenuretot_scaled
mat wageb[1,5] = `r(estimate)'
mat wagebse[1,5] = `r(se)'
mat wagebp[1,5] = `r(p)'

reghdfe lmarkup_rel tenuretot_scaled cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
mat markupr2[1,5] = e(r2)
qui lincom tenuretot_scaled
mat markupb[1,5] = `r(estimate)'
mat markupbse[1,5] = `r(se)'
mat markupbp[1,5] = `r(p)'


reg lbillingamount_hr tenuretot_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo fee
estadd matrix b0 = feeb
estadd matrix se0 = feebse
estadd matrix p0 = feebp
estadd matrix r20 = feer2

reg lcost_hr tenuretot_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo wage
estadd matrix b0 = wageb
estadd matrix se0 = wagebse
estadd matrix p0 = wagebp
estadd matrix r20 = wager2

reg lmarkup_rel tenuretot_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo markup
estadd matrix b0 = markupb
estadd matrix se0 = markupbse
estadd matrix p0 = markupbp
estadd matrix r20 = markupr2

qui tab numid if e(sample)
local n_workers = r(r)
qui tab senttocode if e(sample)
local n_clients = r(r)

estout fee wage markup using "tabhomo_a_s.tex", title("Annual growth rates – homogeneous linear models") cells(b0(fmt(3)) se0(par fmt(3)) p0(par({ }) fmt(3)) r20(par([ ]) fmt(3))) varlabels(c1 "Model 1" c2 "Model 2" c3 "Model 3" c4 "Model 4" c5 "Model 5") replace style(tex) /*
*/	prehead("\begin{table}[htbp]\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Annual growth rates -- homogeneous linear models}" "\label{tab:profile-homo-weighted}" "\begin{tabular}{L{5cm}C{3cm}C{3cm}C{3cm}})" "\hline\hline") /*
*/	prefoot("[1em]" "No. observations & `e(N)' & `e(N)'\\" "Workers & `n_workers' & `n_workers' & `n_workers'\\" "Clients & `n_clients' & `n_clients' & `n_clients'\\\hline") /*
*/	postfoot("\end{tabular}" "\end{table}")


//////////////////////////////////////////////////////
////////// table 7 homo linear-spline /////////
//////////////////////////////////////////////////////



eststo clear
use sampleS_main, clear
g m15later = cond(tenuretot>=15,1,0)
g tend_scaled = (tenuretot - 15)*m15later/12
g tenuretot_scaled = tenuretot/12

//fee
mat feeb1 = J(1,5,0)
mat feeb1se = J(1,5,0)
mat feeb1p = J(1,5,0)
mat feeb2 = J(1,5,0)
mat feeb2se = J(1,5,0)
mat feeb2p = J(1,5,0)
mat feer2 = J(1,5,0)

reg lbillingamount_hr tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
mat feer2[1,1] = e(r2)
qui lincom tenuretot_scaled
mat feeb1[1,1] = `r(estimate)'
mat feeb1se[1,1] = `r(se)'
mat feeb1p[1,1] = `r(p)'
qui lincom tend_scaled
mat feeb2[1,1] = `r(estimate)'
mat feeb2se[1,1] = `r(se)'
mat feeb2p[1,1] = `r(p)'

reghdfe lbillingamount_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
mat feer2[1,2] = e(r2)
qui lincom tenuretot_scaled
mat feeb1[1,2] = `r(estimate)'
mat feeb1se[1,2] = `r(se)'
mat feeb1p[1,2] = `r(p)'
qui lincom tend_scaled
mat feeb2[1,2] = `r(estimate)'
mat feeb2se[1,2] = `r(se)'
mat feeb2p[1,2] = `r(p)'

reghdfe lbillingamount_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat feer2[1,3] = e(r2)
qui lincom tenuretot_scaled
mat feeb1[1,3] = `r(estimate)'
mat feeb1se[1,3] = `r(se)'
mat feeb1p[1,3] = `r(p)'
qui lincom tend_scaled
mat feeb2[1,3] = `r(estimate)'
mat feeb2se[1,3] = `r(se)'
mat feeb2p[1,3] = `r(p)'


reghdfe lbillingamount_hr tenuretot_scaled tend_scaled cpi unemp i.ordclient [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat feer2[1,4] = e(r2)
qui lincom tenuretot_scaled
mat feeb1[1,4] = `r(estimate)'
mat feeb1se[1,4] = `r(se)'
mat feeb1p[1,4] = `r(p)'
qui lincom tend_scaled
mat feeb2[1,4] = `r(estimate)'
mat feeb2se[1,4] = `r(se)'
mat feeb2p[1,4] = `r(p)'


reghdfe lbillingamount_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
mat feer2[1,5] = e(r2)
qui lincom tenuretot_scaled
mat feeb1[1,5] = `r(estimate)'
mat feeb1se[1,5] = `r(se)'
mat feeb1p[1,5] = `r(p)'
qui lincom tend_scaled
mat feeb2[1,5] = `r(estimate)'
mat feeb2se[1,5] = `r(se)'
mat feeb2p[1,5] = `r(p)'

reg lbillingamount_hr tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo fee1
estadd matrix b0 = feeb1
estadd matrix se0 = feeb1se
estadd matrix p0 = feeb1p
estadd matrix r20 = feer2

reg lbillingamount_hr tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo fee2
estadd matrix b0 = feeb2
estadd matrix se0 = feeb2se
estadd matrix p0 = feeb2p


//wage
mat wageb1 = J(1,5,0)
mat wageb1se = J(1,5,0)
mat wageb1p = J(1,5,0)
mat wageb2 = J(1,5,0)
mat wageb2se = J(1,5,0)
mat wageb2p = J(1,5,0)
mat wager2 = J(1,5,0)

reg lcost_hr tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
mat wager2[1,1] = e(r2)
qui lincom tenuretot_scaled
mat wageb1[1,1] = `r(estimate)'
mat wageb1se[1,1] = `r(se)'
mat wageb1p[1,1] = `r(p)'
qui lincom tend_scaled
mat wageb2[1,1] = `r(estimate)'
mat wageb2se[1,1] = `r(se)'
mat wageb2p[1,1] = `r(p)'

reghdfe lcost_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
mat wager2[1,2] = e(r2)
qui lincom tenuretot_scaled
mat wageb1[1,2] = `r(estimate)'
mat wageb1se[1,2] = `r(se)'
mat wageb1p[1,2] = `r(p)'
qui lincom tend_scaled
mat wageb2[1,2] = `r(estimate)'
mat wageb2se[1,2] = `r(se)'
mat wageb2p[1,2] = `r(p)'

reghdfe lcost_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat wager2[1,3] = e(r2)
qui lincom tenuretot_scaled
mat wageb1[1,3] = `r(estimate)'
mat wageb1se[1,3] = `r(se)'
mat wageb1p[1,3] = `r(p)'
qui lincom tend_scaled
mat wageb2[1,3] = `r(estimate)'
mat wageb2se[1,3] = `r(se)'
mat wageb2p[1,3] = `r(p)'


reghdfe lcost_hr tenuretot_scaled tend_scaled cpi unemp i.ordclient [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat wager2[1,4] = e(r2)
qui lincom tenuretot_scaled
mat wageb1[1,4] = `r(estimate)'
mat wageb1se[1,4] = `r(se)'
mat wageb1p[1,4] = `r(p)'
qui lincom tend_scaled
mat wageb2[1,4] = `r(estimate)'
mat wageb2se[1,4] = `r(se)'
mat wageb2p[1,4] = `r(p)'


reghdfe lcost_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
mat wager2[1,5] = e(r2)
qui lincom tenuretot_scaled
mat wageb1[1,5] = `r(estimate)'
mat wageb1se[1,5] = `r(se)'
mat wageb1p[1,5] = `r(p)'
qui lincom tend_scaled
mat wageb2[1,5] = `r(estimate)'
mat wageb2se[1,5] = `r(se)'
mat wageb2p[1,5] = `r(p)'

reg lcost_hr tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo wage1
estadd matrix b0 = wageb1
estadd matrix se0 = wageb1se
estadd matrix p0 = wageb1p
estadd matrix r20 = wager2

reg lcost_hr tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo wage2
estadd matrix b0 = wageb2
estadd matrix se0 = wageb2se
estadd matrix p0 = wageb2p



//markup
mat markupb1 = J(1,5,0)
mat markupb1se = J(1,5,0)
mat markupb1p = J(1,5,0)
mat markupb2 = J(1,5,0)
mat markupb2se = J(1,5,0)
mat markupb2p = J(1,5,0)
mat markupr2 = J(1,5,0)

reg lmarkup_rel tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
mat markupr2[1,1] = e(r2)
qui lincom tenuretot_scaled
mat markupb1[1,1] = `r(estimate)'
mat markupb1se[1,1] = `r(se)'
mat markupb1p[1,1] = `r(p)'
qui lincom tend_scaled
mat markupb2[1,1] = `r(estimate)'
mat markupb2se[1,1] = `r(se)'
mat markupb2p[1,1] = `r(p)'

reghdfe lmarkup_rel tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
mat markupr2[1,2] = e(r2)
qui lincom tenuretot_scaled
mat markupb1[1,2] = `r(estimate)'
mat markupb1se[1,2] = `r(se)'
mat markupb1p[1,2] = `r(p)'
qui lincom tend_scaled
mat markupb2[1,2] = `r(estimate)'
mat markupb2se[1,2] = `r(se)'
mat markupb2p[1,2] = `r(p)'

reghdfe lmarkup_rel tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat markupr2[1,3] = e(r2)
qui lincom tenuretot_scaled
mat markupb1[1,3] = `r(estimate)'
mat markupb1se[1,3] = `r(se)'
mat markupb1p[1,3] = `r(p)'
qui lincom tend_scaled
mat markupb2[1,3] = `r(estimate)'
mat markupb2se[1,3] = `r(se)'
mat markupb2p[1,3] = `r(p)'


reghdfe lmarkup_rel tenuretot_scaled tend_scaled cpi unemp i.ordclient [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
mat markupr2[1,4] = e(r2)
qui lincom tenuretot_scaled
mat markupb1[1,4] = `r(estimate)'
mat markupb1se[1,4] = `r(se)'
mat markupb1p[1,4] = `r(p)'
qui lincom tend_scaled
mat markupb2[1,4] = `r(estimate)'
mat markupb2se[1,4] = `r(se)'
mat markupb2p[1,4] = `r(p)'


reghdfe lmarkup_rel tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
mat markupr2[1,5] = e(r2)
qui lincom tenuretot_scaled
mat markupb1[1,5] = `r(estimate)'
mat markupb1se[1,5] = `r(se)'
mat markupb1p[1,5] = `r(p)'
qui lincom tend_scaled
mat markupb2[1,5] = `r(estimate)'
mat markupb2se[1,5] = `r(se)'
mat markupb2p[1,5] = `r(p)'

reg lmarkup_rel tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo markup1
estadd matrix b0 = markupb1
estadd matrix se0 = markupb1se
estadd matrix p0 = markupb1p
estadd matrix r20 = markupr2

reg lmarkup_rel tenuretot_scaled tend_scaled initialblank experience_entry c.experience_entry_scaled#c.experience_entry_scaled cpi unemp i.gender i.univ ib(3).branch [aweight = hoursworked], vce(cluster numid)
eststo markup2
estadd matrix b0 = markupb2
estadd matrix se0 = markupb2se
estadd matrix p0 = markupb2p


estout fee1 fee2 wage1 wage2 markup1 markup2 using "tabhomospline0_s.tex", title("Annual growth rates – homogeneous linear spline models") cells(b0(fmt(3)) se0(par fmt(3)) p0(par({ }) fmt(3)) r20(par([ ]) fmt(3))) varlabels(c1 "Model 1" c2 "Model 2" c3 "Model 3" c4 "Model 4" c5 "Model 5") replace style(tex)/*
*/	prehead("\begin{table}[htbp]\centering" "\def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}" "\caption{Annual growth rates -- homogeneous linear spline models}" "\label{tab:profile-homo-spline-weighted}" "\begin{tabular}{L{4cm}C{1.8cm}C{1.8cm}C{1.8cm}C{1.8cm}C{1.8cm}C{1.8cm}}" "\hline\hline") /*
*/	postfoot("\hline" "\end{tabular}" "\end{table}")





//model 2
eststo clear
use sampleS_main, clear
g m15later = cond(tenuretot>=15,1,0)
g tend_scaled = (tenuretot - 15)*m15later/12
g tenuretot_scaled = tenuretot/12

reghdfe lbillingamount_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
scalar homospline_m2_fee_b0 = _b[tenuretot_scaled]
scalar homospline_m2_fee_b1 = _b[tend_scaled]

reghdfe lcost_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
scalar homospline_m2_wage_b0 = _b[tenuretot_scaled]
scalar homospline_m2_wage_b1 = _b[tend_scaled]

reghdfe lmarkup_rel tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
scalar homospline_m2_markup_b0 = _b[tenuretot_scaled]
scalar homospline_m2_markup_b1 = _b[tend_scaled]



/* choice of spline point
mat spl = J(48,2,0)
forvalues i=1/48{
	mat spl[`i',1] = `i'
}
forvalues i=1/48{
	use sampleS_main, clear
	g m`i'later = cond(tenuretot>=`i',1,0)
	g tend_scaled = (tenuretot - `i')*m`i'later/12
	g tenuretot_scaled = tenuretot/12
	qui reghdfe lcost_hr tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
	mat spl[`i',2]  = e(r2)
}
clear
svmat spl
rename spl1 cutpoint
rename spl2 r2
scatter r2 cutpoint, saving(cutpoint_r2, replace)
graph export "figsppointr2_s.pdf", as(pdf) replace
*/





////////////////////////////////////////////////
////////// table 8 hetero linear /////////
////////////////////////////////////////////////



eststo clear
use sampleS_main, clear
g tenuretot_scaled = tenuretot/12
g initialblanklong = cond(initialblank>=3, 1, 0)

reghdfe lbillingamount_hr tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
eststo feew2_s 
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lcost_hr tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
eststo wagew2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lmarkup_rel tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
eststo markupw2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lbillingamount_hr tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
eststo feewcli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lcost_hr tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
eststo wagewcli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lmarkup_rel tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
eststo markupwcli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lbillingamount_hr tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
eststo feewbycli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lcost_hr tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
eststo wagewbycli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lmarkup_rel tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#i.female c.tenuretot_scaled#i.univ c.tenuretot_scaled#ib(3).branch cpi unemp [aweight = hoursworked], absorb(worker_cli_id) vce(cluster numid)
eststo markupwbycli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

qui tab numid if e(sample)
local n_workers = r(r)
qui tab senttocode if e(sample)
local n_clients = r(r)



esttab feew2_s wagew2_s markupw2_s feewcli2_s wagewcli2_s markupwcli2_s feewbycli2_s wagewbycli2_s markupwbycli2_s using tabhetero_a_s.tex, replace b(3) se stats(N r2 F_statistics p_value) star(* 0.1 ** 0.05 *** 0.01) /*
*/	title("Annual growth rates – heterogeneous linear models" /*
*/	mtitles("Fee" "Wage" "Markup" "Fee" "Wage" "Markup" "Fee" "Wage" "Markup") noomitted /*
*/	keep(tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank 1.female#c.tenuretot_scaled 1.univ#c.tenuretot_scaled) /*
*/	order(tenuretot_scaled c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank) /*
*/	coeflabels(tenuretot_scaled "Tenure(years)" c.tenuretot_scaled#c.experience_entry "$\times\$Potential experience at entry"  c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled "$\times\$Potential exp$^2/100$" c.tenuretot_scaled#c.initialblank "$\times\$Training period" 1.female#c.tenuretot_scaled "$\times\$Female" 1.univ#c.tenuretot_scaled "$\times\$University+") /*
*/	prefoot("[1em]" "Workers & `n_workers' & `n_workers' & `n_workers' & `n_workers' & `n_workers' & `n_workers' & `n_workers' & `n_workers' & `n_workers'\\" "Clients & `n_clients' & `n_clients' & `n_clients' & `n_clients' & `n_clients' & `n_clients' & `n_clients' & `n_clients' & `n_clients'\\") /*
*/	addnotes("Standard errors in parentheses (clustered at individual level). Estimation is weighted by hours worked. F-statistics are for the joint hypothesis: 5 tenure interaction terms (potential experience, potential experience$^2\$, training period, female, university+) are equal to 0. See notes to Table 6 for details of the model specifications.") 






////////////////////////////////////////////////////////
////////// table 9 hetero linear-spline /////////
////////////////////////////////////////////////////////


eststo clear
use sampleS_main, clear
g tenuretot_scaled = tenuretot/12
g m15later = cond(tenuretot>=15,1,0)
g tend_scaled = (tenuretot - 15)*m15later/12

reghdfe lbillingamount_hr tenuretot_scaled tend_scaled c.tenuretot_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) c.tend_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
eststo feew2_s 
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ c.tend_scaled#c.experience_entry c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.initialblank c.tend_scaled#1.female c.tend_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lcost_hr tenuretot_scaled tend_scaled c.tenuretot_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) c.tend_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
eststo wagew2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ c.tend_scaled#c.experience_entry c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.initialblank c.tend_scaled#1.female c.tend_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lmarkup_rel tenuretot_scaled tend_scaled c.tenuretot_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) c.tend_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
eststo markupw2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ c.tend_scaled#c.experience_entry c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.initialblank c.tend_scaled#1.female c.tend_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lbillingamount_hr tenuretot_scaled tend_scaled c.tenuretot_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) c.tend_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
eststo feewcli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ c.tend_scaled#c.experience_entry c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.initialblank c.tend_scaled#1.female c.tend_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lcost_hr tenuretot_scaled tend_scaled c.tenuretot_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) c.tend_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
eststo wagewcli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ c.tend_scaled#c.experience_entry c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.initialblank c.tend_scaled#1.female c.tend_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

reghdfe lmarkup_rel tenuretot_scaled tend_scaled c.tenuretot_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) c.tend_scaled#(c.experience_entry c.experience_entry_scaled#c.experience_entry_scaled c.initialblank i.female i.univ ib(3).branch) cpi unemp [aweight = hoursworked], absorb(numid senttocode) vce(cluster numid)
eststo markupwcli2_s
test c.tenuretot_scaled#c.experience_entry c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tenuretot_scaled#1.female c.tenuretot_scaled#1.univ c.tend_scaled#c.experience_entry c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.initialblank c.tend_scaled#1.female c.tend_scaled#1.univ
estadd scalar F_statistics = r(F)
estadd scalar p_value = r(p)

esttab feew2_s wagew2_s markupw2_s feewcli2_s wagewcli2_s markupwcli2_s using tabheterospline_a_s15.tex, replace b(3) se stats(r2 F_statistics p_value) star(* 0.1 ** 0.05 *** 0.01) /*
*/	title("Annual growth rates - heterogeneous linear-spline models") /*
*/	mtitles("Fee" "Wage" "Markup" "Fee" "Wage" "Markup") noomitted /*
*/	order(tenuretot_scaled tend_scaled c.tenuretot_scaled#c.experience_entry c.tend_scaled#c.experience_entry  c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled c.tenuretot_scaled#c.initialblank c.tend_scaled#c.initialblank 1.female#c.tenuretot_scaled  1.female#c.tend_scaled 1.univ#c.tenuretot_scaled 1.univ#c.tend_scaled) drop(cpi unemp _cons *branch*) /*
*/	coeflabels(tenuretot_scaled "Tenure" tend_scaled "$\times\$After 15m." c.tenuretot_scaled#c.experience_entry "$\times\$Pot-Exp at entry" c.tend_scaled#c.experience_entry "$\times\$Pot-Exp$\times\$After 15m." c.tenuretot_scaled#c.experience_entry_scaled#c.experience_entry_scaled "$\times\$Pot-Exp$^2/100$" c.tend_scaled#c.experience_entry_scaled#c.experience_entry_scaled  "$\times\$Pot-Exp$^2/100\times\$After 15m." c.tenuretot_scaled#c.initialblank "$\times\$Training period" c.tend_scaled#c.initialblank "$\times\$Training$\times\$After 15m." 1.female#c.tenuretot_scaled "$\times\$Female" 1.female#c.tend_scaled "$\times\$Female$\times\$After 15m." 1.univ#c.tenuretot_scaled "$\times\$University+" 1.univ#c.tend_scaled "$\times\$University+$\times\$After 15m.") /*
*/	prefoot("[1em]" "Tenure$\times\$Branch & Yes& Yes& Yes& Yes& Yes& Yes\\[1em]" "Regional CPI/Unemployment & Yes& Yes& Yes& Yes& Yes& Yes\\[1em]") 





////////////////////////////////////////////////
////////// table 10 IRR: baseline /////////
////////////////////////////////////////////////


use sampleS_main, clear
sort numid nmonth
bysort numid: g n = _n
bysort numid: g N = _N
drop if n != N
replace retired=0 if tenuretotori>120

tab education_cate, g(educcate)
tab branch, g(brc)
stset tenuretot, failure(retired)

g experience_entry_scaled_sq = experience_entry_scaled*experience_entry_scaled

streg lmarkup_rel_m_2 lhoursworked_m_2 initialblank experience_entry experience_entry_scaled_sq female univ brc1 brc2 brc4 brc5, dist(ln)
g t_old = _t
forvalues i=1/120{
	qui replace _t = `i'
	predict s`i', surv
}
replace _t = t_old
keep numid s1-s120
save survivor_ln, replace

use sampleS_main, clear
g m15later = cond(tenuretot>=15,1,0)
g tend_scaled = (tenuretot - 15)*m15later/12
g tenuretot_scaled = tenuretot/12
reghdfe lbillingamount_m tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid) resid
predict lf_fitted0, xbd
scalar b1f = _b[tenuretot_scaled]/12
scalar b2f = b1f + _b[tend_scaled]/12
reghdfe lcost_noised tenuretot_scaled tend_scaled cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid) resid
predict lw_fitted0, xbd
scalar b1w = _b[tenuretot_scaled]/12
scalar b2w = b1w + _b[tend_scaled]/12
sort numid nmonth
bysort numid: g n=_n
keep if n==1
keep numid tenuretot lf_fitted0 lw_fitted0 cpi unemp cost_noised_ini initialblank female education_cate experience_entry entryyr entrymo branch experience_entry experience_entry_scaled univ markup_abs

merge 1:1 numid using survivor_ln, nogenerate
replace s1 = 1
replace s2 = 1
replace s3 = 1
forvalues i = 1/14{
	g lf_fitted`i' = .
	replace lf_fitted`i' = lf_fitted0 + b1f*`i' - b1f*tenuretot if tenuretot<15
	replace lf_fitted`i' = lf_fitted0 + b1f*`i' - b2f*tenuretot if tenuretot>=15
	g lw_fitted`i' = .
	replace lw_fitted`i' = lw_fitted0 + b1w*`i' - b1w*tenuretot if tenuretot<15
	replace lw_fitted`i' = lw_fitted0 + b1w*`i' - b2w*tenuretot if tenuretot>=15
}
forvalues i = 15/72{
	g lf_fitted`i' = .
	replace lf_fitted`i' = lf_fitted0 + b2f*`i' - b1f*tenuretot if tenuretot<15
	replace lf_fitted`i' = lf_fitted0 + b2f*`i' - b2f*tenuretot if tenuretot>=15
	g lw_fitted`i' = .
	replace lw_fitted`i' = lw_fitted0 + b2w*`i' - b1w*tenuretot if tenuretot<15
	replace lw_fitted`i' = lw_fitted0 + b2w*`i' - b2w*tenuretot if tenuretot>=15
}
g expentry_long = .
replace expentry_long = 0 if experience_entry < 6
replace expentry_long = 1 if experience_entry >5
keep numid initialblank female univ expentry_long cost_noised_ini s1-s120 lf* lw*
save fittedfw, replace



////////////////////////////////////////////////////////////////
////////// figure 1 histogram: training period /////////
////////////////////////////////////////////////////////////////

use sampleS_main, clear
sort numid nmonth
bysort numid: g n = _n
keep if n==1
replace initialblank = 10 if initialblank>10
hist initialblank, discrete frac width(1) scheme(s1mono) ylabel(,angle(0)) xlabel(0 "0" 2 "2" 4 "4" 6 "6" 8 "8" 10 "10+") xtitle("Training period in months") ytitle("Fraction of workers")
graph export "figinitialblank_s.pdf", as(pdf) replace




///////////////////////////////////////////////////////////////////
////////// figure 3 binscatter: initial by tenure /////////
///////////////////////////////////////////////////////////////////

use sampleS_main, clear
binscatter billingamount_hr_ini cost_hr_ini tenuretot, nquantiles(48) linetype(none) yscale(log)  ylabel(,angle(0)) legend(lab(1 "Hourly initial fee") lab(2 "Hourly initial wage")) ytitle("Average initial fee/wage")  xtitle("Tenure(month)")  xlabel(0(12)48)  saving(binscini_tenuretot_s, replace) scheme(s1mono) mcolor(gs10)

clear
insheet using dataforinibyten.csv
twoway (scatter billingamount_hr_ini tenuretot, mcolor(gs10) connect(direct) msymbol(none) lwidth(medthick)) (scatter cost_hr_ini tenuretot, mcolor()  connect(direct) msymbol(none) lwidth(medthick)) , graphregion(fcolor(white))  xtitle(tenuretot) ytitle(billingamount_hr_ini and cost_hr_ini) legend(lab(1 billingamount_hr_ini) lab(2 cost_hr_ini) order(1 2)) yscale(log) ylabel(,angle(0)) legend(lab(1 "Hourly initial fee") lab(2 "Hourly initial wage")) ytitle("Average initial fee or wage (JPY)") xtitle("Tenure in months") xlabel(0(12)48) saving(binscini_tenuretot_s, replace) scheme(s1mono)


graph export "figbinsc_ini_s.pdf", as(pdf) replace


//////////////////////////////////////////////////////////////////
////////// figure 4 initial fee/wage by training /////////
//////////////////////////////////////////////////////////////////


use sampleS_main, clear
sort numid nmonth
keep if tenuretot == tenureini
replace initialblank = 6 if initialblank >= 6
statsby mean=r(mean) ub=r(ub) lb=r(lb) N=r(N), by(initialblank) clear: ci mean billingamount_hr
twoway bar mean initialblank, barw(0.5)  scheme(s1mono) ylabel(,angle(0)) xlabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6+") xtitle("Training period in months") ytitle("Initial fee (JPY)") || rcap ub lb initialblank, legend(off) saving(trainingfeeini, replace)
graph export "figtrainfeeini_f1.pdf", as(pdf) replace
use sampleS_main, clear
sort numid nmonth
keep if tenuretot == tenureini
replace initialblank = 6 if initialblank >= 6
statsby mean=r(mean) ub=r(ub) lb=r(lb) N=r(N), by(initialblank) clear: ci mean cost_hr
twoway bar mean initialblank, barw(0.5)  scheme(s1mono) ylabel(,angle(0)) xlabel(0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6+") xtitle("Training period in months") ytitle("Initial wage (JPY)") || rcap ub lb initialblank, legend(off) saving(trainingwageini, replace)
graph export "figtrainfeeini_f2.pdf", as(pdf) replace



////////////////////////////////////////////////////////////////
////////// figure 5 fee/wage profile (model 2) /////////
////////////////////////////////////////////////////////////////

use sampleS_main, clear
mat few_rgnytot = J(48, 11, 0)
forvalues i = 1/48{
	mat few_rgnytot[`i',1] = `i'
}
reghdfe lbillingamount_hr ib(4).tenuretot cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid) resid
di e(r2_a)
foreach i of numlist 5/48{
	mat few_rgnytot[`i',2] = _b[`i'.tenuretot] 
	scalar temp`i' = sqrt(e(V)["`i'.tenuretot","`i'.tenuretot"])
	mat few_rgnytot[`i',6] = _b[`i'.tenuretot] - temp`i'*1.960525
	mat few_rgnytot[`i',7] = _b[`i'.tenuretot] + temp`i'*1.960525
}
reghdfe lcost_hr ib(4).tenuretot cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid) resid
di e(r2_a)
foreach i of numlist 5/48{
	mat few_rgnytot[`i',3] = _b[`i'.tenuretot] 
	scalar temp`i' = sqrt(e(V)["`i'.tenuretot","`i'.tenuretot"])
	mat few_rgnytot[`i',8] = _b[`i'.tenuretot] - temp`i'*1.960525
	mat few_rgnytot[`i',9] = _b[`i'.tenuretot] + temp`i'*1.960525
}
reghdfe lmarkup_rel ib(4).tenuretot cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid) resid
di e(r2_a)
foreach i of numlist 5/48{
	mat few_rgnytot[`i',4] = _b[`i'.tenuretot] 
	scalar temp`i' = sqrt(e(V)["`i'.tenuretot","`i'.tenuretot"])
	mat few_rgnytot[`i',10] = _b[`i'.tenuretot] - temp`i'*1.960525
	mat few_rgnytot[`i',11] = _b[`i'.tenuretot] + temp`i'*1.960525
}
su r_fee r_wage r_markup [aweight = hoursworked]
su r_fee r_wage r_markup [aweight = hoursworked] if tenuretot>4
qui reghdfe lmarkup_abs_hr ib(4).tenuretot cpi unemp [aweight = hoursworked], absorb(numid) vce(cluster numid)
foreach i of numlist 5/48{
	mat few_rgnytot[`i',5] = _b[`i'.tenuretot] 
}
clear 
svmat few_rgnytot
rename few_rgnytot1 tenuretot
rename few_rgnytot2 lbillingamount_hr
rename few_rgnytot3 lcost_hr
rename few_rgnytot4 lmarkup_rel
rename few_rgnytot5 lmarkup_abs_hr
rename few_rgnytot6 cilower_b
rename few_rgnytot7 ciupper_b
rename few_rgnytot8 cilower_c
rename few_rgnytot9 ciupper_c
rename few_rgnytot10 cilower_m
rename few_rgnytot11 ciupper_m
label variable lbillingamount_hr "log(Fee)"
label variable lcost_hr "log(Wage)"
label variable lmarkup_rel "log(Markup)"
g fee_spline = 0
replace fee_spline = homospline_m2_fee_b0*(tenuretot - 4)/12 if tenuretot>4 & tenuretot<15
replace fee_spline = homospline_m2_fee_b0*(tenuretot - 4)/12 + homospline_m2_fee_b1*(tenuretot - 15)/12 if tenuretot>=15
g wage_spline = 0
replace wage_spline = homospline_m2_wage_b0*(tenuretot - 4)/12 if tenuretot>4 & tenuretot<15
replace wage_spline = homospline_m2_wage_b0*(tenuretot - 4)/12 + homospline_m2_wage_b1*(tenuretot - 15)/12 if tenuretot>=15
g markup_spline = 0
replace markup_spline = homospline_m2_markup_b0*(tenuretot - 4)/12 if tenuretot>4 & tenuretot<15
replace markup_spline = homospline_m2_markup_b0*(tenuretot - 4)/12 + homospline_m2_markup_b1*(tenuretot - 15)/12 if tenuretot>=15
twoway (line lbillingamount_hr tenuretot if tenuretot>3, lwidth(thick)) (line lcost_hr tenuretot if tenuretot>3, lwidth(medium)) (line fee_spline tenuretot if tenuretot>3, lwidth(medthick) lpattern(dash) legend(lab(3 "Spline model fit"))) (line wage_spline tenuretot if tenuretot>3, lwidth(medthin) lpattern(dash) legend(lab(4 "Spline model fit"))) (rarea ciupper_b cilower_b tenuretot if tenuretot>3, bcolor(gs14%70) legend(lab(5 "95%CI") col(2))) (rarea ciupper_c cilower_c tenuretot if tenuretot>3, bcolor(gs14%50) legend(lab(6 "95%CI") order(1 2 5 6 3 4))), ylabel(-0.05(0.05)0.3, angle(0)) xlabel(0(12)48) xline(15) yline(0, lcolor(gs10)) saving(few_rgny1_s, replace) xtitle("Tenure in months") ytitle("Log points growth") scheme(s1mono)
graph export "figprofilew_f1.pdf", as(pdf) replace
twoway (line lmarkup_rel tenuretot if tenuretot>3, lwidth(medium)) (line markup_spline tenuretot if tenuretot>3, lwidth(medthin) lpattern(dash) legend(lab(2 "Spline model fit"))) (rarea ciupper_m cilower_m tenuretot if tenuretot>3, bcolor(gs14%50) legend(lab(3 "95%CI") order(1 3 2) col(3))), ylabel(-0.05(0.05)0.15, angle(0)) xlabel(0(12)48) xline(15)  yline(0, lcolor(gs10)) saving(few_rgny2_s, replace) xtitle("Tenure in months") ytitle("Log points growth") scheme(s1mono)
graph export "figprofilew_f2.pdf", as(pdf) replace



