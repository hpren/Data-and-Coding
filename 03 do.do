
*** I. Control variables
*** II. Descriptive stats and Regression analysis

* Set working directory

if fileexists("C:\Users\huipingr\Dropbox") {
    cd "C:\Users\huipingr\Dropbox\4_SASB\Data and Coding"
}
else if fileexists("D:\Dropbox\Dropbox\Dropbox") {
    cd "D:\Dropbox\Dropbox\Dropbox\4_SASB\Data and Coding"
}

/* / Checked with Compustat data (okay)

import excel "SASB Metrics and Data (working).xlsx", sheet("Compustat Links") firstrow clear
destring, replace
rename SASBSampleName SASBconm
rename _all, lower 
keep sasbconm cusip latest gvkey conm tic cik
duplicates list gvkey cik //Suntrust has become Truist in 2019
save compustat_lin.dta, replace

// import delimited "raw data\Compustat_controls.csv", clear 
// destring, replace
// merge n:1 gvkey cik using compustat_lin.dta //should be all merged


* ---------------------------------
*** Construct control variables ***

import delimited "raw data\Compustat_controls.csv", clear 
destring, replace

gen datadate_num = date(datadate, "YMD")
format datadate_num %td
gen year = year(datadate_num)
drop datadate_num

*** Replace missing values 

* Tidewater Inc. - changed fiscal year end to December
drop if gvkey == 10565 & datadate == "2017-03-31" 
* Arconic Corp - starts to trade in 2020 April -> The shares outstanding is replaced as 109,021,376 as if on 2020 March 19 (in 10-K), the stock price is replaced as the closing price of the first day of trading (April 1, 2020)
list if gvkey == 35978
replace csho = 109.02 if gvkey == 35978 & year == 2019
replace prcc_f = 6.92 if gvkey == 35978 & year == 2019

* Carrier Global Corp - starts to trade on April 3 2020 -> The shares outstainding is replaced as 866,158,910 shares as if on the spin-off day (in 10-K), the stock price is replaced as the closing price of the first date of trading (April 3, 2020)

list if gvkey == 36191
replace csho = 866.16 if gvkey == 36191 & year == 2019
replace prcc_f = 16.92 if gvkey == 36191 & year == 2019

* Plains GP Holdings LP - No book value of equity, replaced as SEQ (stockholders equity-parent)
list if gvkey == 18468
replace ceq = seq if gvkey == 18468 & year == 2020

* Generate control variables
* LnAT
gen lnat = ln(at)
* ROA
gen roa = ni/at
* Leverage
gen lev = lt/at
* Gross Profit Margin
gen gpm = gp/sale
* Cash
gen cash = che/at
* Loss
gen loss = (ni<0)
* Market-to-Book ratio
gen btm = ceq/(prcc_f*csho)

tabstat at lnat roa lev gpm cash loss btm, stats(N mean sd min p25 p50 p75 max) c(s) f(%9.2f)

keep gvkey datadate year conm tic cusip cik at ceq che lt gp ni sale csho prcc_f lnat roa lev gpm cash loss btm


drop if missing(lnat, roa, lev, gpm, cash, loss, btm)
duplicates report gvkey year
duplicates list gvkey year

save control.dta, replace
*/

/* boycott and proposal data prep */ 

***boycott data

*import excel and save as stata dataset
import excel "li_2026_boycott_data.xlsx", sheet("Sheet1") firstrow clear
rename _all, lower 
gen year = year(date)
keep boycott boycott_category issuername cusip year
bysort cusip year: gen boycott_count = _N
bysort issuername year boycott_category: keep if _n == 1
bysort issuername year (boycott_category): gen topic_num = _n
reshape wide boycott_category, i(issuername year) j(topic_num)
gen boycott = 1

gen original_obs = 1
egen cusip_id = group(cusip)
tsset cusip_id year
tsfill, full
replace original_obs = 0 if missing(original_obs)

foreach var of varlist issuername cusip {
    
    * Carry values forward
    bysort cusip_id (year): replace `var' = `var'[_n-1] if missing(`var')
    
    * Carry values backward for years before the first observed year
    gsort cusip_id -year
    by cusip_id: replace `var' = `var'[_n-1] if missing(`var')
}

sort cusip_id year

*boycott data is 2016 - 2022, so l3 var will be valid for 2018 onward
gen boycott_current = (boycott == 1)
gen boycott_lastyear = (L1.boycott == 1)
gen boycott_twoyearsago = (L2.boycott == 1)

gen boycott_l3 = boycott_current | boycott_lastyear | boycott_twoyearsago

gen boycott_f1 = (F1.boycott == 1)


drop boycott_count original_obs cusip_id boycott_current boycott_lastyear boycott_twoyearsago

replace boycott = 0 if boycott == .

save boycott.dta, replace

***proposal data

*import esg proposal data and save as stata dataset
import excel "master_proxy.xlsx", sheet("proposal_data") firstrow clear
rename _all, lower 
drop if year != filing_datayear
rename filing_datacusip cusip 
rename filing_datafdate proposal_date
rename category proposal_category
keep proposal_category year cusip proposal_date

bysort cusip year: gen proposal_count = _N

bysort cusip year (proposal_category): gen topic_num = _n
reshape wide proposal_category, i(cusip year) j(topic_num)
gen proposal = 1

gen e_proposal = 0
foreach var of varlist proposal_category1-proposal_category8 {
    replace e_proposal = 1 if inlist(`var', ///
        "Carbon Asset Risk ", "Deforestation", "Environmental Management" , "GHG Emissions Management" , "Greenhouse Gas Emissions" ,  "Renewable Energy", "Sustainability Disclosure and Management", "Sustainable Governance")
}

gen s_proposal = 0
foreach var of varlist proposal_category1-proposal_category8 {
    replace s_proposal = 1 if inlist(`var', ///
        "Board Diversity", "Conservative groups", "Decent Work", "Diversity at Work"  , "Health", "Health ", "Human Rights")
}

gen g_proposal = 0
foreach var of varlist proposal_category1-proposal_category8 {
    replace g_proposal = 1 if inlist(`var', ///
        "Board Diversity", "Board Oversight", "ESG Pay Links ", "Sustainable Governance")
}

gen p_proposal = 0
foreach var of varlist proposal_category1-proposal_category8 {
    replace p_proposal = 1 if inlist(`var', ///
        "Election Spending", "Lobbying", "Other political activity", "Political Spending")
}


gen original_obs = 1
egen cusip_id = group(cusip)
tsset cusip_id year
tsfill, full
replace original_obs = 0 if missing(original_obs)

foreach var of varlist cusip {
    
    * Carry values forward
    bysort cusip_id (year): replace `var' = `var'[_n-1] if missing(`var')
    
    * Carry values backward for years before the first observed year
    gsort cusip_id -year
    by cusip_id: replace `var' = `var'[_n-1] if missing(`var')
}

sort cusip_id year

gen proposal_count_current = (proposal_count)
gen proposalcount_lastyear = (L1.proposal_count)
gen proposalcount_twoyearsago = (L2.proposal_count)
gen proposalcount_f1 = (F1.proposal_count)

egen proposalcount_3 = rowtotal(proposal_count_current proposalcount_lastyear proposalcount_twoyearsago)

drop proposal_count_current proposalcount_lastyear proposalcount_twoyearsago original_obs 

foreach var of varlist proposal e_proposal s_proposal g_proposal p_proposal {
   
	gen `var'_current = (`var' == 1)
	gen `var'_lastyear = (L1.`var' == 1)
	gen `var'_twoyearsago = (L2.`var' == 1)
	gen `var'_f1 = (F1.`var' == 1)
    gen `var'_3 = `var'_current | `var'_lastyear | `var'_twoyearsago
	drop `var'_current `var'_lastyear `var'_twoyearsago
}

drop cusip_id

save master_proxy.dta, replace
/*
NOTE TO SELF: RESUME, RESHAPING DATA SO THERE IS A VAR FOR PROPOSALS n-1 AND N+1 ETRC

*/




*** Union data

*import union membership details and save as stata dataset 
import excel "union_master.xlsx", sheet("analysis_data") firstrow clear
rename _all, lower 
rename sasbmainindustry main_industry


bysort year: egen q25_var = pctile(unionparticipationrate), p(25)
bysort year: egen q75_var = pctile(unionparticipationrate), p(75)

gen low_union = (unionparticipationrate < q25_var) if !missing(unionparticipationrate)
gen high_union = (unionparticipationrate > q75_var) if !missing(unionparticipationrate)

drop q25_var q75_var 

save union.dta, replace




/*
* 1) Link SASB companies in sample with identifiers and controls
import excel "SASB Metrics and Data (working).xlsx", sheet("Sample") firstrow clear
rename _all, lower
browse
keep lineinmaster companyname reportingyear
rename companyname sasbconm
rename reportingyear year
keep lineinmaster sasbconm year

merge m:1 sasbconm using compustat_lin.dta
drop _merge
save sample.dta, replace

* 2) Link sample with control variables 

use sample.dta, clear
distinct sasbconm
merge 1:1 cik year using control.dta  // 6 observations dropped due to missing value

list gvkey cik sasbconm year if _merge == 1

keep if _merge == 3
drop _merge 
sort lineinmaster year
sum 
save sample_control.dta, replace
export excel using "sample_control.xlsx", firstrow(variables) replace
*/

**# Bookmark #1 Regressions from here

/*
* 3) Merge with SASB metrics

use regression2.dta, clear
sum
rename _all, lower
rename line_in_master lineinmaster

// note: main analysis, drop those non-applicable metrics
drop if reported_adj == .

/* robustness test - add non-applicable metrics as non-disclosed metrics
// replace reported = 0 if reported == 2
*/

distinct line //Total number of SASB reports in the sample
merge m:1 lineinmaster using sample_control.dta
list sasbconm if _merge == 2  //Intel and VMware are dopped due to no SASB metrics
keep if _merge == 3
distinct line
drop _merge
save regression3.dta, replace
*/

*4) Merge with additional data for analysis

use regression3.dta, clear

*** boycott data
merge m:1 cusip year using boycott.dta
list sasbconm if _merge == 3  
drop if _merge == 2
drop _merge
drop boycott_category2 boycott_category3 // none of the matched firms had multiple boycotts in the relevant years
rename boycott_category1 boycott_category
replace boycott = 0 if missing(boycott)

save regression4.dta, replace

*** proposal data 
use regression4.dta, clear
merge m:1 cusip year using master_proxy.dta

drop if _merge == 2
drop _merge

replace proposal = 0 if missing(proposal)

save regression5.dta, replace

*** union data

use regression5.dta, clear
merge m:1 main_industry year using union.dta

drop if _merge == 2
drop _merge

* fill missing data

foreach var of varlist boycott_l3 boycott_f1 proposal_count proposalcount_f1 proposalcount_3 proposal_f1 proposal_3 e_proposal e_proposal_3 e_proposal_f1 s_proposal s_proposal_3 s_proposal_f1 g_proposal g_proposal_3 g_proposal_f1 p_proposal p_proposal_3 p_proposal_f1{
   
   replace `var' = 0 if `var' == .
	
}

save regression6.dta, replace



*5) construct additional variables needed for analysis

tab industry if metrics80 == "Total amount of monetary losses as a result of legal proceedings associated with"


tab main_industry
gen highb2c = 1 if main_industry == "Consumer Goods"|main_industry == "Healthcare"|main_industry=="Food & Beverage"
replace highb2c = 0 if highb2c == .

tab industry if main_industry == "Transportation"

*sin industry
gen sinind = 1 if industry == "TOBACCO"|industry =="CASINOS & GAMING"|industry == "AEROSPACE & DEFENSE"|industry=="OIL & GAS – EXPLORATION & PRODUCTION"
replace sinind = 0 if sinind == .
tab sinind

* Data breach 
gen databreach = 1 if regexm(metrics80, "data breach")
replace databreach = 0 if databreach == .

* Quant metrics other than monetary losses and data breach
gen other_quant = 1 if quant==1 & monetaryloss==0 & databreach == 0
replace other_quant = 0 if other_quant == .
tab other_quant

tab ind_code if databreach == 1

* Number of industries that are covered in the reported
bys lineinmaster industry: gen tag = _n==1
bys lineinmaster: egen n_ind = total(tag)
gen multiind = (n_ind > 1)

**************************************************************
*** This is our master file***********************************
save regression7.dta, replace
**************************************************************
**************************************************************

/*
**** run regresssions on new vars

this is where I am testing out the new variables for the analysis. We will move this code when we determine what to include in the paper

global controls lnat roa lev gpm cash loss btm
winsor2 $controls, cuts(1 99) replace

tabstat reported quantitative monetaryloss databreach other_quant multiind b2c highb2c sinind boycott proposal high_union low_union $controls, stats(N mean sd p25 p50 p75) c(s) f(%9.3f)

**** 2. Correlation Metrics
pwcorr reported quantitative monetaryloss databreach other_quant multiind b2c highb2c sinind boycott proposal high_union low_union $controls, sig star(0.05) 


*****testing out new vars

*proposals
qui logit reported proposal  $controls
est store m1
qui logit reported proposal quantitative b2c sinind multiind $controls
est store m2
qui logit reported proposal_count  $controls
est store m3
qui logit reported proposal_count  quantitative b2c sinind multiind $controls
est store m4
qui logit reported e_proposal $controls
est store m5 
qui logit reported e_proposal quantitative b2c sinind multiind $controls
est store m6 
qui logit reported s_proposal $controls
est store m7 
qui logit reported s_proposal quantitative b2c sinind multiind $controls
est store m8
qui logit reported g_proposal $controls
est store m9 
qui logit reported g_proposal quantitative b2c sinind multiind  $controls
est store m10 
qui logit reported p_proposal $controls
est store m11
qui logit reported p_proposal quantitative b2c sinind multiind $controls
est store m12 
esttab m1 m2 m3 m4 m5 m6 m7 m8 m9 m10 m11 m12 using proposals.rtf, replace compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) keep(proposal proposal_count e_proposal s_proposal g_proposal p_proposal quantitative b2c sinind multiind) order(proposal proposal_count e_proposal s_proposal g_proposal p_proposal quantitative b2c sinind multiind) 

** notes: proposal, proposal count, e proposal and s proposal load and are negative



*boycotts

qui logit reported boycott  $controls
est store m1
qui logit reported boycott quantitative b2c sinind multiind $controls
est store m2
qui logit reported boycott_l3  $controls if year != 2017
est store m3
qui logit reported boycott_l3  quantitative b2c sinind multiind $controls if year != 2017
est store m4

esttab m1 m2 m3 m4   using boycotts.rtf, replace compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) keep(boycott boycott_l3 reported quantitative b2c sinind multiind) order(boycott boycott_l3 reported quantitative b2c sinind multiind) 


*unions

qui logit reported low_union high_union
est store m1
qui logit reported low_union high_union quantitative b2c sinind multiind  $controls
est store m2
esttab m1 m2 using unions.rtf, replace compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) keep(low_union high_union quantitative b2c sinind multiind) order(low_union high_union quantitative b2c sinind multiind) 

*/


******************************************
**** 1. Descriptive Statistics

*use master file for all analyses
use regression7.dta, clear

distinct gvkey
preserve
bys lineinmaster industry: gen tag = _n==1
bys lineinmaster: egen n_ind = total(tag)
bys lineinmaster: keep if _n==1
tab n_ind
restore

** Distribution of main industries 

preserve
duplicates drop lineinmaster sasb_industry, force
asdoc tab sasb_industry, sort
restore

** Distribution of most common metrics 

preserve
duplicates drop lineinmaster code, force
contract metrics80
gsort -_freq
egen total = total(_freq)
gen percent = 100 * _freq / total
gen cum_percent = sum(percent)
export excel using "metrics80.xlsx", firstrow(variables) replace
restore


/*

**********Huiping: I moved this above to structure the do-file in a way that allows us to generate a data set that doesn't change at all once we do analysis. Let me know if that makes sense and then we can delete this. We will need to do the same thing for the additional analyses *******************


tab industry if metrics80 == "Total amount of monetary losses as a result of legal proceedings associated with"


tab main_industry
gen highb2c = 1 if main_industry == "Consumer Goods"|main_industry == "Healthcare"|main_industry=="Food & Beverage"
replace highb2c = 0 if highb2c == .

tab industry if main_industry == "Transportation"

gen sinind = 1 if industry == "TOBACCO"|industry =="CASINOS & GAMING"|industry == "AEROSPACE & DEFENSE"|industry=="OIL & GAS – EXPLORATION & PRODUCTION"
replace sinind = 0 if sinind == .
tab sinind

* Data breach 
gen databreach = 1 if regexm(metrics80, "data breach")
replace databreach = 0 if databreach == .

* Quant metrics other than monetary losses and data breach
gen other_quant = 1 if quant==1 & monetaryloss==0 & databreach == 0
replace other_quant = 0 if other_quant == .
tab other_quant

tab ind_code if databreach == 1

* Number of industries that are covered in the reported
bys lineinmaster industry: gen tag = _n==1
bys lineinmaster: egen n_ind = total(tag)
gen multiind = (n_ind > 1)

** Descriptive Statistics

global controls lnat roa lev gpm cash loss btm
winsor2 $controls, cuts(1 99) replace


*/


** Descriptive Statistics
tabstat reported quantitative monetaryloss databreach other_quant multiind b2c highb2c sinind $controls, stats(N mean sd p25 p50 p75) c(s) f(%9.3f)

**** 2. Correlation Metrics
pwcorr reported quantitative monetaryloss databreach other_quant multiind b2c highb2c sinind $controls, sig star(0.05) 

**** 3. Univariate Tests

ttest reported, by(quantitative)
ttest reported if quantitative==1, by(monetaryloss)
ttest reported if quantitative==1, by(databreach)
ttest reported if quantitative==1, by(other_quant)
ttest reported, by(multiind)
ttest reported, by(highb2c)
ttest reported, by(sinind)

**** 4. Regression Analysis

qui logit reported quantitative b2c sinind
est store m1
qui logit reported quantitative b2c sinind multiind
est store m2
qui logit reported quantitative b2c sinind multiind $controls
est store m3
esttab m1 m2 m3 using mm.rtf, replace compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order(quantitative b2c) 


qui logit reported monetaryloss databreach other_quant
est store m1
qui logit reported monetaryloss databreach other_quant b2c sinind multiind 
est store m2
qui logit reported monetaryloss databreach other_quant b2c sinind multiind $controls 
est store m3
esttab m1 m2 m3, b(%9.3f) compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order(monetaryloss databreach other_quant b2c sinind multiind) 

* if conditional on quantitative metrics

qui logit reported monetaryloss databreach other_quant if quantitative == 1 
est store m1
qui logit reported monetaryloss databreach other_quant b2c sinind multiind if quantitative == 1 
est store m2
qui logit reported monetaryloss databreach other_quant b2c sinind multiind $controls if quantitative == 1 
est store m3
esttab m1 m2 m3, b(%9.3f) compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order(monetaryloss databreach other_quant b2c sinind multiind) 


qui logit reported quantitative b2c sinind i.quantitative##c.b2c multiind $controls
est store m1
qui logit reported quantitative b2c sinind i.quantitative##c.sinind multiind $controls
est store m2
qui logit reported quantitative b2c sinind i.quantitative##c.b2c i.quantitative##c.sinind multiind $controls
est store m3
esttab m1 m2 m3 using mm.rtf, replace compress nogap drop(0.*) star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order(quantitative b2c) 

*** Sensitivity Test - excluding repeated metrics

bysort lineinmaster metrics: gen overlap = (_N > 1)
tab overlap
bysort overlap: distinct lineinmaster

preserve
keep if overlap == 0
qui logit reported quantitative b2c sinind multiind $controls
est store m1
qui logit reported monetaryloss databreach other_quant b2c sinind multiind $controls 
est store m2 
qui logit reported quantitative b2c sinind i.quantitative##c.b2c i.quantitative##c.sinind multiind $controls
est store m3
esttab m1 m2 m3 using mm.rtf, replace compress nogap drop(0.*) star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order(quantitative b2c) 
restore


*** Cross-sectional tests

* Firm size - Big firm and small firm 

summ at, detail
gen high_at = at >= r(p50)

qui logit reported quantitative b2c sinind i.quantitative##c.b2c i.quantitative##c.sinind multiind $controls if high_at == 1
est store mlarge
qui logit reported quantitative b2c sinind i.quantitative##c.b2c i.quantitative##c.sinind multiind $controls if high_at == 0
est store msmall
qui logit reported quantitative b2c sinind i.quantitative##c.b2c i.quantitative##c.sinind multiind $controls if loss == 0
est store mprofit
qui logit reported quantitative b2c sinind i.quantitative##c.b2c i.quantitative##c.sinind multiind $controls if loss == 1
est store mloss
esttab mlarge msmall mprofit mloss using mm.rtf, replace compress nogap drop(0.*) star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order(quantitative b2c) 

**# Break down quant metrics

*** Additional: test different quant metrics ***
use regression2.dta, clear
sum
rename _all, lower
rename line_in_master lineinmaster
// note: main analysis, drop those non-applicable metrics
drop if reported_adj == .

distinct line //Total number of SASB reports in the sample
merge m:1 lineinmaster using sample_control.dta
list sasbconm if _merge == 2  //Intel and VMware are dopped due to no SASB metrics
keep if _merge == 3
distinct line
drop _merge
keep if quantitative == 1


* merge with controls 
gen sinind = 1 if industry == "TOBACCO"|industry =="CASINOS & GAMING"|industry == "AEROSPACE & DEFENSE"|industry=="OIL & GAS – EXPLORATION & PRODUCTION"
replace sinind = 0 if sinind == .
tab sinind

* Number of industries that are covered in the reported
bys lineinmaster industry: gen tag = _n==1
bys lineinmaster: egen n_ind = total(tag)
gen multiind = (n_ind > 1)

global controls lnat roa lev gpm cash loss btm
winsor2 $controls, cuts(1 99) replace

*** Merge with quant topics
merge n:1 code using quant_topics.dta
drop if _merge == 2

tab code if _merge == 1

tab quant_topic
encode quant_topic, gen(quant_topic_num)

tab quant_topic_num, nolabel  // 9 is other quant
asdoc tab quant_topic_num, sort 

** Descriptive Statistics
tabstat reported b2c sinind multiind $controls, stats(N mean sd p25 p50 p75) c(s) f(%9.3f)

** Regressions
logit reported ib12.quant_topic_num
est store m1  
qui logit reported ib12.quant_topic_num b2c sinind multiind 
est store m2
qui logit reported ib12.quant_topic_num b2c sinind multiind $controls 
est store m3
esttab m1 m2 m3 using mm.rtf, label replace compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order($quant_metrics) 

**** Broad quant metrics

tab broad_topic
encode broad_topic, gen(broad_topic_num)

tab broad_topic_num, nolabel  // 13 is other quant

** Regressions
logit reported ib13.broad_topic_num
est store m1  
qui logit reported ib13.broad_topic_num b2c sinind multiind 
est store m2
qui logit reported ib13.broad_topic_num b2c sinind multiind $controls 
est store m3
esttab m1 m2 m3 using mm.rtf, label replace compress nogap star(* 0.1 ** 0.05 *** 0.01) scalar(N r2_p) order($quant_metrics) 


*Topics less likely to be reported
* Data Security
* Fuel Economy & Emissions in Use-phase
* Labor Practice
* Product Labeling & Marketing 
* Product Lifecycle Management
* Reserves Valuation & Capital Expenditures
* Security, Human Rights & Rights of Indigenous Peoples

* Employee Recruitment, Inclusion & Performance
* Waste & Hazardous Materials Management










