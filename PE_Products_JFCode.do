********************************************************************************
********************************************************************************
********************************************************************************
* BARBARIAN AT THE STORE? PRIVATE EQUITY, PRODUCTS, AND CONSUMERS
* FORTHCOMING AT THE JOURNAL OF FINANCE
* CESARE FRACASSI, ALESSANDRO PREVITERO, AND ALBERT SHEEN

********************************************************************************
********************************************************************************
********************************************************************************



* DEFINE WORKING DIRECTORY

global mainfolder "/Users/aleprevi/Desktop/PE_Products"
cd "$mainfolder/Analysis"


*Note: STEP 1 and 2, scramble the data and generate the main data sets 

********************************************************************************
********************************************************************************
** STEP 3: TABLE II: SUMMARY STATS
********************************************************************************
********************************************************************************
* PANEL A: OVERALL NIELSEN DATA
********************************************************************************

* N. PRODUCTS AND STORES PER PRODUCT
use jf_files/nupc_all, clear
keep nupc nstores_date
egen nstores_upc=mean(nstores_date)
keep if _n==1
label var nupc "Products"
label var nstores_upc "Stores per Product"
eststo clear
estpost tabstat nupc nstores_upc, stat(mean) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product1_1.tex", /// 
	replace label type style(tex) nolines f cells("mean(label(_) fmt(%12.0fc))") ///
	collabels(none) nomtitle mlabels(,none) nonumber noobs 

* N. OF PRODUCTS PER STORE	
use jf_files/product_store, clear
label var nupc_tot "Products per Store"
eststo clear
estpost tabstat nupc_tot, stat(mean) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product1_15.tex", replace /// 
	label type style(tex) nolines f   cells("mean(label(_) fmt(%12.0fc))") ///
	collabels(none) nomtitle mlabels(,none)  nonumber noobs 

* N. OF FIRMS. 

use jf_files/firm_allstores_data0.dta, clear
egen nfirms=nvals(id_nielsen)
merge m:1 id_nielsen using jf_files/pe_events, keepusing(pub2priv) 
drop if _merge==2
drop _merge

unique(id_nielsen) if after0==1 & pub2priv==1
gen nfirms_public=r(sum)

unique(id_nielsen) if after0==1 & pub2priv==0
gen nfirms_private=r(sum)

label var nfirms "Firms"
label var nfirms_private "Private Target Deals"
label var nfirms_public "Public Target Deals"

keep if _n==1
eststo clear
estpost tabstat nfirms, stat(mean) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product1_2.tex", replace ///
	label type style(tex) nolines f   cells("mean(label(_) fmt(%12.0fc))") ///
	collabels(none) nomtitle mlabels(,none)  nonumber noobs 

* N. OF STORES, CHAINS, DMA, COUNTIES, STATES, AND ZIP

use month_date store_id using "jf_files/stores_firms0", clear
bys month_date store_id: keep if _n==1
gen year=year(dofm(month_date))
rename store_id store_code_uc
sort store_code_uc year 
merge m:1 store_code_uc year using jf_files/stores_new, ///
	keepusing(parent_code store_zip3 fips_state_code fips_county_code dma_code)	
keep store_code_uc parent_code dma_code fips_county_code fips_state_code store_zip3
egen nstores=nvals(store_code_uc)
egen nchains=nvals(parent_code)
egen ndma=nvals(dma_code)
egen ncounty=nvals(fips_county_code)
egen nstate=nvals(fips_state_code)
egen nzip=nvals(store_zip3)
keep if _n==1

label var nstores "Stores"
label var nchains "Chains"
label var ndma "Designated Market Areas"
label var ncounty "Counties"
label var nstate "States"
label var nzip "3-Digit ZIP"

estpost tabstat nstores nchains nzip ncounty ndma nstate , stat(n mean) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product1_3.tex", replace ///
	label type style(tex) nolines f cells("mean(label(N.)fmt(%12.0fc))")  ///
	collabels(none) nodep mlabels(,none) unstack nonumber noobs

	
* N. OF DEALS

use jf_files/firm_allstores_matched0.dta, clear
keep if after0==1
keep id_nielsen sales_sum_all0
duplicates drop
*missing
merge m:1 id_nielsen using jf_files/pe_events, keepusing(pub2priv) 
drop if _merge==2
drop _merge 


gen ndeals=_N

egen temp_npriv_priv=count(pub2priv) if pub2priv==0
egen npriv_priv=max(temp_npriv_priv) 

egen temp_npub_priv=count(pub2priv) if pub2priv==1
egen npub_priv=max(temp_npub_priv)


egen temp_sales_priv=total(sales_sum_all0) if pub2priv==0
egen sales_priv=max(temp_sales_priv)

egen temp_sales_pub=total(sales_sum_all0) if pub2priv>0
egen sales_pub=max(temp_sales_pub)

egen sales_all=total(sales_sum_all0) 

gen perc_sales_priv=sales_priv/sales_all
gen perc_sales_pub=sales_pub/sales_all

sum perc* 

keep n*
duplicates drop

label var ndeals "PE Deals"
label var npriv_priv "Private Target Deals"
label var npub_priv "Public Target Deals"
eststo clear
estpost tabstat ndeals npriv_priv  npub_priv, stat(mean) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product2_0.tex", ///
	replace label type style(tex)  f   cells("mean(label(_) fmt(%12.0fc))") ///
	collabels(none) nolines nomtitle mlabels(,none)  nonumber noobs 

* PANEL B: PRODUCT CATEGORY CHARACTERISTICS
********************************************************************************

use jf_files/nupc_all, clear
keep industry nupc_date nstores_industry
bys industry: keep if _n==1
label var nupc_date "N. Products per Category"
label var nstores_industry "N. Stores per Category"
egen nindustry=nvals(industry)
label var nindustry "N. Categories"

eststo clear
estpost tabstat  nindustry nupc_date nstores_industry, ///	
	stat(n mean median sd) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product3_0.tex" ///
	, replace label type style(tex) booktabs f   ///
	cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines nomtitle mlabels(,none)  nonumber noobs 

* n firms & HHI
use nfirms hhi using jf_files/hhi_store.dta, clear

label var nfirms "N. Firms per Category-Store"
label var hhi "Herfindal Index"

eststo clear
estpost tabstat  nfirms hhi, stat(n mean median sd) col(stat)
esttab using "$mainfolder/Paper/Tables/summary_stats_product3_1.tex", ///
	replace label type style(tex) booktabs f   cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) 	fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines nomtitle mlabels(,none)  nonumber noobs 
 

* PANEL C: FIRM CHARACTERISTICS
********************************************************************************

use jf_files/firm_allstores_data0.dta, clear

label var upc_num_all0 "N. Products per Firm"
label var stores_num_all0 "N. Stores per Firm"
label var parent_num_all0 "N. Chains per Firm"
label var industry1000_num_all0 "N. Categories per Firm"

eststo clear

estpost tabstat upc_num_all0 stores_num_all0 parent_num_all0 industry1000_num_all0, ///
	stat(n mean median sd) col(stat)
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product4_0.tex", /// 
	replace label type style(tex) nolines booktabs f cells("count(label(Obs.) fmt(%9.0fc)) mean(label(Mean)fmt(%9.2fc)) p50(label(Median)fmt(%9.2fc)) 	sd(label(S.D.)fmt(%9.2fc)) ") ///
	collabels(none) nomtitle unstack nonumber noobs 


* TABLE A6: Summary Statistics of Matching Procedure 
********************************************************************************

estpost tabstat upc_num_all0 stores_num_all0 parent_num_all0 industry1000_num_all0, ///
	stat(n mean median sd) col(stat) by(treat) nototal
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product4_1.tex", replace  ///
	label  type style(tex) nolines booktabs f ///
	cells("count(label(Obs.) fmt(%9.0fc)) mean(label(Mean)fmt(%9.2fc)) p50(label(Median)fmt(%9.2fc)) sd(label(S.D.)fmt(%9.2fc)) ") ///
	collabels(none) nomtitle unstack nonumber noobs 

use jf_files/firm_allstores_matched0.dta, clear
sort id_cohort month_date
gen growth_sales_all0=((sales_sum_all0)-(l12.sales_sum_all0))/(l12.sales_sum_all0)
keep if time_to_event==0
bys cohort: gen n=_N
drop if n==1

* Add labels
label var upc_num_all0 "N. Products"
format upc_num_all0 %12.0gc
label var stores_num_all0 "N. Stores"
format stores_num_all0 %12.0gc
label var parent_num_all0 "N. Chains"
format parent_num_all0 %12.0gc
label var industry1000_num_all0 "N. Categories"
format industry1000_num_all0 %12.0gc
label var sales_sum_all0 "Monthly Sales"
format sales_sum_all0 %12.0gc
label var growth_sales_all0 "Monthly Sales Growth"
format growth_sales_all0 %12.2gc
label var units_sum_all0 "Monthly Units Sold"
format units_sum_all0 %12.0gc
label var price_av_all0 "Average Price"
format price_av_all0 %12.2gc
label var dma_num_all0 "N. DMAs"
format dma_num_all0 %12.0gc
label var county_num_all0 "N. Counties"
format county_num_all0 %12.0gc
label var state_num_all0 "N. States"
format state_num_all0 %12.0gc
label var zip_num_all0 "N. 3-digit ZIP Codes"
format zip_num_all0 %12.0gc


eststo clear
eststo treat: quietly estpost tabstat sales_sum_all0 growth_sales_all0 ///
	upc_num_all0 stores_num_all0 units_sum_all0 price_av_all0 ///
	industry1000_num_all0 parent_num_all0  zip_num_all0 county_num_all0 ///
	state_num_all0 dma_num_all0 if treat==1, stat(n mean median) columns(statistics)

eststo control: quietly estpost tabstat sales_sum_all0 growth_sales_all0 ///
	upc_num_all0 stores_num_all0 units_sum_all0 price_av_all0  ///
	industry1000_num_all0 parent_num_all0  zip_num_all0 county_num_all0 ///
	state_num_all0 dma_num_all0 if treat==0, stat(n mean median) columns(statistics)

eststo diff: quietly estpost ttest sales_sum_all0 growth_sales_all0 ///
	upc_num_all0 stores_num_all0 units_sum_all0 price_av_all0 ///
	industry1000_num_all0 parent_num_all0  zip_num_all0 county_num_all0 ///
	state_num_all0 dma_num_all0, by(treat) unequal
	
esttab treat control diff using "$mainfolder/Paper/Tables/summary_stats_product4_2.tex", replace /// 
	cells("mean(pattern(1 1 0) label(Mean) fmt(%12.2fc)) p50(label(Median) pattern(1 1 0) fmt(%12.2fc))  b(star pattern(0 0 1) label(Diff) 	fmt(%12.2fc)) t(pattern(0 0 1) label(t-stat) par fmt(2))") /// 
	label tex fragment
	

* PANEL D: PRODUCT CHARACTERISTICS IN OUR SAMPLE BY TREATMENT
********************************************************************************

use ln_unitprice ln_units ln_sales treat time_to_event month_date  store_id  /// 
	upc cohort industry using jf_files/upc_data_matched_best1_allstores_shortv2, clear
	
egen double store_upc=group(store_id upc cohort industry)
duplicates drop
sort store_upc month_date
xtset store_upc month_date
drop store_id upc industry cohort
gen unitprice=exp(ln_unitprice)
drop ln_unitprice
gen units=exp(ln_units)-1
drop ln_units
gen sales=exp(ln_sales)-1
drop ln_sales
label var unitprice "Price"
label var units "Monthly Units Sold per Store"
label var sales "Monthly Sales per Store"

foreach var of varlist unitprice units sales {
	gen `var'_ch=(`var'-l12.`var')/l12.`var'
	}
label var unitprice_ch "Price Growth"
label var units_ch "Units Growth"
label var sales_ch "Sales growth"

estpost tabstat unitprice units sales, stat(n mean median sd p1 p5 p25 p50 p75 ///
	p95 p99  min max) col(stat)
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product5_0.tex", replace ///
	label type style(tex) booktabs f  ///
	cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines  nomtitle mlabels(,none)  nonumber noobs 

estpost tabstat unitprice units sales, nototal stat(n mean median sd p1 p5 p25 ///
	p50 p75 p95 p99  min max) col(stat) by(treat)
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product5_1.tex", replace ///
	label type style(tex) booktabs f ///
	cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines  nomtitle mlabels(,none)  nonumber noobs  unstack

keep if time_to_event==0

gen befrecession= (year(dofm(month_date))<2009)
gen durrecession= (year(dofm(month_date))> 2008 &  year(dofm(month_date))< 2011)
gen aftrecession= (year(dofm(month_date))> 2010)

foreach var of varlist unitprice units sales *_ch {
	winsor `var', gen(`var'_win) p(0.01)
	}
	
estpost tabstat unitprice_win units_win sales_win *_ch_win if befrecession==1, ///
	nototal stat(n mean median sd p1 p5 p25 p50 p75 p95 p99  min max) ///
	col(stat) by(treat)
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product5_11.tex", replace ///
	label type style(tex) booktabs f ///
	cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines  nomtitle mlabels(,none)  nonumber noobs  unstack
	
estpost tabstat unitprice_win units_win sales_win *_ch_win if durrecession==1, ///
	nototal stat(n mean median sd p1 p5 p25 p50 p75 p95 p99  min max) col(stat) ///
	by(treat)
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product5_12.tex", replace ///
	label type style(tex) booktabs f ///
	cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines  nomtitle mlabels(,none)  nonumber noobs  unstack
	
estpost tabstat unitprice_win units_win sales_win *_ch_win if aftrecession==1, ///
	nototal stat(n mean median sd p1 p5 p25 p50 p75 p95 p99  min max) ///
	col(stat) by(treat)
	
esttab using "$mainfolder/Paper/Tables/summary_stats_product5_13.tex", replace ///
	label type style(tex) booktabs f  ///
	cells("count(label(Obs.) fmt(%12.0fc)) mean(label(Mean) fmt(%12.2fc)) p50(label(Median) fmt(%12.2fc)) sd(label(S.D.) fmt(%12.2fc))") ///
	collabels(none) nolines  nomtitle mlabels(,none)  nonumber noobs  unstack

********************************************************************************
********************************************************************************
** STEP 4- FIG I: PRIVATE EQUITY DEALS OVER TIME
********************************************************************************
********************************************************************************


use jf_files/firm_allstores_matched0, clear
keep if treat==1
gen deal_year=year(deal_date)
gen deal_month=mofd(deal_date)
keep id_nielsen deal_month deal_year 
duplicates drop

bys deal_year: egen npe_year=count(id_nielsen)
bys deal_month: egen npe_month=count(id_nielsen)
duplicates drop deal_month, force
drop id_nielsen
format deal_month %tm
tsset deal_month
tsfill
replace npe_month=0 if npe_month==. 
label var npe_month "Number of PE Deals"
label var deal_month "Date"
twoway bar npe_month deal_month,  graphregion(color(white)) ///
	xlabel(#8, labsize(small))  name("pe_event_month", replace) ///
	saving("pe_event_month", replace)
	
graph export "$mainfolder/Paper/Tables/pe_event_month.eps", replace
drop deal_month npe_month
duplicates drop
label var deal_year "Year"
label var npe_year "PE Events"
twoway bar  npe_year deal_year,  graphregion(color(white)) lwidth(large) ///
	xlabel(#10, labsize(small))  name("pe_event_year", replace) ///
	saving("pe_event_year", replace)
	
graph export "$mainfolder/Paper/Tables/pe_event_year.eps", replace
*/ 
 
 

********************************************************************************
********************************************************************************
** STEP 5- PRODUCT LEVEL ANALYSIS 
********************************************************************************
********************************************************************************


* TABLE 3 PANEL C
********************************************************************************

use jf_files/upc_data_matched_best1_allstores_shortv2, clear

drop store_id industry

sum ln_sales

version 15

capture log close
log using logfiles/upc_matching_results_allbest1_all.log, replace


char _dta[omit] prevalent
	eststo clear
	
	
foreach var of varlist ln_unitprice ln_units ln_sales {
	
	di "`var'"

	reghdfe `var' after if  time_to_event>-25 &  time_to_event<61 , ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date)
	eststo m1_`var'

	}
estout using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_best1_all.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.5f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none) mlabels(, none) varlabels(_cons Constant) 
	
log close



* TABLE 8 PANEL A: PUBLIC VS PRIVATE
********************************************************************************


use "jf_files/upc_data_matched_best1_allstores_shortv2", clear

drop store_id industry

version 15

capture log close
log using logfiles/upc_matching_results_allbest1_all_split.log, replace


char _dta[omit] prevalent

eststo clear
	
foreach var of varlist ln_sales ln_unitprice ln_units  {
	
	di "`var'"

	reghdfe `var' after if pus_pe==1 & time_to_event>-25 &  time_to_event<61 , ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date) 
	eststo m1_`var'
	
	reghdfe `var' after if priv_pe==1 & time_to_event>-25 &  time_to_event<61 , ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date) 
	eststo m2_`var'
	
	}

estout m1_* m2_* using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_best1_all_private_public.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none) mlabels(, none) varlabels(_cons Constant) 

log close




* TABLE 9 PANEL A:  FINANCIAL CONSTRAINTS - SA INDEX
********************************************************************************


use "jf_files/upc_data_matched_best1_allstores_shortv2", clear

drop store_id industry


version 15

capture log close
log using logfiles/upc_matching_results_allbest1_all_split_largesmall.log, replace


merge m:1 id_nielsen using jf_files/large_small, keep(1 3) nogen  keepusing(SA_*)

bys cohort : egen SA_high2=max(SA_high) 
drop SA_high
bys cohort : egen SA_medium2=max(SA_medium) 
drop SA_medium
bys cohort : egen SA_low2=max(SA_low) 
drop SA_low



char _dta[omit] prevalent

eststo clear
	
foreach var of varlist ln_sales ln_unitprice ln_units  {
	
	di "`var'"

	reghdfe `var' after if SA_high2==1 & time_to_event>-25 &  time_to_event<61 , ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date) 
	eststo m1_`var'
	
	reghdfe `var' after if SA_medium2==1 & time_to_event>-25 &  time_to_event<61 , ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date)
	eststo m2_`var'
	
	reghdfe `var' after if SA_low2==1 & time_to_event>-25 &  time_to_event<61 , ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date)
	eststo m3_`var'
	
	}

estout m1_* m2_* m3_* using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_best1_all_SA.tex",  ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none) mlabels(, none) varlabels(_cons Constant) 

		


log close



* TABLE 10 PANEL A: PRE- VS POST-RECESSION
********************************************************************************


capture log close
log using logfiles/upc_matching_results_allbest1_allstores_PREPOST.log, replace

*  POST RECESSION
		
use  cohort after month_date if after==1 using "jf_files/upc_data_matched_best1_allstores_shortv2", clear
 
bys cohort (month_date): keep if _n==1
gen deal_year=year(dofm(month_date))
keep cohort deal_year
duplicates drop

sort cohort
save jf_files/cohort_deal_date, replace

use "jf_files/upc_data_matched_best1_allstores_shortv2", clear

drop store_id industry

merge m:1 cohort using jf_files/cohort_deal_date, keep(1 3) nogen nol nonotes 

drop if deal_year<2011

version 15


char _dta[omit] prevalent
	eststo clear
	
	
foreach var of varlist ln_sales ln_unitprice ln_units  {
	
	di "`var'"

	reghdfe `var' after if  time_to_event>-25 &  time_to_event<61, ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date)
	eststo m_`var'_post

	}

*  PRE RECESSION
		

use "jf_files/upc_data_matched_best1_allstores_shortv2", clear

drop store_id industry

merge m:1 cohort using jf_files/cohort_deal_date, keep(1 3) nogen nol nonotes 

drop if deal_year>2010

version 13


char _dta[omit] prevalent
	
	
foreach var of varlist  ln_sales ln_unitprice ln_units {
	
	di "`var'"

	reghdfe `var' after if  time_to_event>-25 &  time_to_event<61, /// 
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date)
	eststo m_`var'_pre

	}


estout m_ln_sales_pre m_ln_sales_post  using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_sales_PREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label collabels(none) ///
	varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout m_ln_unitprice_pre m_ln_unitprice_post  using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_price_PREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout m_ln_units_pre m_ln_units_post  using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_units_PREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

log close



* TABLE A11 PANEL C: EXCLUDING TIME AFTER EXIT
********************************************************************************


use "jf_files/upc_data_matched_best1_allstores_shortv2", clear

drop store_id industry

capture drop _merge
merge m:1 id_nielsen using jf_files/exit, keep(1 3) 
drop _merge
gen exit= ((exit_type=="IPO" | exit_type=="secondary" | exit_type=="trade") & ///
	(month_date>=mofd(date_exit)))
	
drop exit_type
bys cohort month_date: egen exit2 = max(exit)
drop exit
rename exit2 exit

drop if exit==1

compress

capture log close
log using logfiles/upc_matching_results_allbest1_all_noexit.log, replace
eststo clear
foreach var of varlist ln_unitprice ln_units ln_sales {
	
	di "`var'"

	reghdfe `var' after if  time_to_event>-25 &  time_to_event<61, ///
		absorb(cohort#id_nielsen cohort#month_date) vce(cluster id_nielsen month_date)
	eststo m1_`var'

	}
	
estout using "$mainfolder/Paper/Tables/TABLE_upc_main_matched_all_best1_all_noexit.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.5f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none) mlabels(, none) varlabels(_cons Constant) 
	
log close



********************************************************************************
********************************************************************************
** STEP 6- FIRM-CATEGORY LEVEL ANALYSIS 
********************************************************************************
********************************************************************************
	

use jf_files/final_firm_category, replace

* RUN ALL REGRESSIONS

	reghdfe new_upc_sum1000 after if time_to_event>-25 & time_to_event<61 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 


eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_1
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & pus_pe==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_puspe_1
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & priv_pe==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_privpe_1
	
	reghdfe `var'  after   after_hhi if time_to_event>-25 & time_to_event<61 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_hhi_1
	}


* TABLE 3 PANEL B
********************************************************************************
	
estout  ln_sales_sum_1 ln_price_av_1 ln_units_sum_1    using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	

* TABLE 4 PANEL B
********************************************************************************
	
estout   ln_upc_num_1 new_upc_sum1000_1 old_upc_sum1000_1   using  ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
	
* TABLE 5 PANEL B
********************************************************************************

estout ln_stores_num_1  ln_parent_num_1 ln_zip_num_1  using /// 
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
* TABLE 8 PANEL A
********************************************************************************

estout  ln_sales_sum_puspe_1 ln_sales_sum_privpe_1 ln_price_av_puspe_1 ///
	ln_price_av_privpe_1 ln_units_sum_puspe_1 ln_units_sum_privpe_1  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_split.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


* TABLE 8 PANEL B
********************************************************************************

estout  ln_upc_num_puspe_1  ln_upc_num_privpe_1  new_upc_sum1000_puspe_1 ///
	new_upc_sum1000_privpe_1 old_upc_sum1000_puspe_1  old_upc_sum1000_privpe_1 ///
	using "$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_split.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
		
* TABLE 8 PANEL C
********************************************************************************

estout ln_stores_num_puspe_1 ln_stores_num_privpe_1 ln_parent_num_puspe_1  /// 
	ln_parent_num_privpe_1 ln_zip_num_puspe_1 ln_zip_num_privpe_1 using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_split.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


* TABLE 12 PANEL A
********************************************************************************
	
estout  ln_sales_sum_hhi_1 ln_price_av_hhi_1  ln_units_sum_hhi_1    using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_hhi.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt( %11.0gc)  ///
	labels( "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) t(par fmt(2))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
	
* TABLE 12 PANEL B
********************************************************************************

estout  ln_upc_num_hhi_1  new_upc_sum1000_hhi_1 old_upc_sum1000_hhi_1   using  ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_hhi.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(  %11.0gc) ///
	labels( "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) t(par fmt(2))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

* TABLE 12 PANEL C
********************************************************************************

estout ln_stores_num_hhi_1  ln_parent_num_hhi_1 ln_zip_num_hhi_1 using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_hhi.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(  %11.0gc) ///
	labels( "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) t(par fmt(2))) ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
* FIGURES 2, 3, 4 - WITHIN FIRM
********************************************************************************

cd "$mainfolder/Paper/Tables/"
foreach var of varlist ln_* new_upc* old_upc* *dummy {

		reghdfe `var' after_m*  after_p1-after_p60 if time_to_event>-25 & /// 
		time_to_event<61  , absorb(id_cohort month_date##cohort) vce(cluster ///
		id_nielsen month_date) 
		eststo AllDeals
		
		coefplot AllDeals , levels(90)  graphregion(color(white)) drop(_cons) ///
		vertical  yline(0) xline(25) recast(line)  lwidth(*2) ///
		ciopts(recast(rline) lpattern(dash))  name("`var'_firmmat1000", replace) ///
		saving(`var'_firmmat1000, replace)
		
		graph export "`var'_firmmat_1000.eps", replace
		}

cd "$mainfolder/Analysis"




* TABLE 9 PANEL A, B, AND C WITHIN FIRM-CATEGORY - MECHANISM: FINANCIAL CONSTRAINS
********************************************************************************
	

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & SA_high==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_large
	estadd scalar `var'_large_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & SA_medium==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_medium
	estadd scalar `var'_medium_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & SA_low==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_small
	estadd scalar `var'_small_nobs = e(N)
	
	}

estout  ln_sales_sum_large ln_sales_sum_medium ln_sales_sum_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_salesLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_large ln_price_av_medium ln_price_av_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_priceLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_large ln_units_sum_medium ln_units_sum_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_unitsLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_large ln_upc_num_medium ln_upc_num_small  using ///
	"$mainfolder\Paper\Tables\TABLE_firm1000_analysis_match_product_nupcLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum1000_large new_upc_sum1000_medium new_upc_sum1000_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_newLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum1000_large old_upc_sum1000_medium old_upc_sum1000_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_oldLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_large ln_stores_num_medium ln_stores_num_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_StoreLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  ///
	varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout ln_parent_num_large ln_parent_num_medium ln_parent_num_small using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ChainLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_large ln_zip_num_medium ln_zip_num_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ZipLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


	
	
* TABLE 10 PANEL A, B, AND C WITHIN FIRM-CATEGORY - MECHANISM: PRIVATE EQUITY FIRM TYPE
********************************************************************************
	


eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy {
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & growthpe==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_acq
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & growthpe==0, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_noacq
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_acq ln_sales_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_sales_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_acq ln_price_av_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_price_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_acq ln_units_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_units_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_acq ln_upc_num_noacq  using ///
	"$mainfolder\Paper\Tables\TABLE_firm1000_analysis_match_product_nupc_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum1000_acq new_upc_sum1000_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_new_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum1000_acq old_upc_sum1000_noacq  using ///
	"$mainfolder\Paper\Tables\TABLE_firm1000_analysis_match_product_old_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_acq ln_stores_num_noacq  using ///
	"$mainfolder\Paper\Tables\TABLE_firm1000_analysis_match_geog1_Store_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_acq ln_parent_num_noacq using ///
	"$mainfolder\Paper\Tables\TABLE_firm1000_analysis_match_geog1_Chain_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_acq ln_zip_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_Zip_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 



	
* TABLE 11 PANEL A, B, AND C WITHIN FIRM-CATEGORY - MECHANISM: DURING VS AFTER FINANCIAL CRISIS
********************************************************************************
	

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & deal_year<2011, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_pre
	estadd scalar `var'_nobs = e(N)

	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & deal_year>2010, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_post
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_pre ln_sales_sum_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_salesPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_pre ln_price_av_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_pricePREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_pre ln_units_sum_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_unitsPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_pre ln_upc_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_nupcPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum1000_pre new_upc_sum1000_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_newPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum1000_pre old_upc_sum1000_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_oldPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_pre ln_stores_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_StorePREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_pre ln_parent_num_post using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ChainPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_pre ln_zip_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ZipPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


* TABLE 12 PANEL A AND B: MECHANISM: INDUSTRY STRUCTURE - MKT SHARE AND HHI
********************************************************************************
	

eststo clear

foreach var of varlist ln_sales_sum ln_price_av ln_units_sum ln_upc_num ///
	new_upc_sum1000 old_upc_sum1000 ln_stores_num ln_parent_num ln_zip_num  {
	
	reghdfe `var'  after   if hhi_high==1 & time_to_event>-25 & time_to_event<61 ///
		, absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_hh
	
	reghdfe `var'  after   if hhi_high==0 & time_to_event>-25 & time_to_event<61 ///
		, absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_hl
		
	reghdfe `var'  after   if mktshare0_high==1 & time_to_event>-25 & ///
		time_to_event<61 , absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date) 
	eststo `var'_mh
	
	reghdfe `var'  after   if mktshare0_high==0 & time_to_event>-25 & ///
		time_to_event<61 , absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date)
	eststo `var'_ml
	
	estout `var'* using ///
		"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_`var'splithhms.tex", ///
		drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
		labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
		t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
		type label collabels(none)  varlabels(_cons Constant) ///
		mlabels(, none) substitute(_ \_) 	
		
	}



* TABLE 12 PANEL C : MECHANISM: INDUSTRY STRUCTURE - HIGH-INCOME CONSUMERS
********************************************************************************
	

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & rich==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_rich
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 &  rich==0, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_poor
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_rich ln_sales_sum_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_salesRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout  ln_price_av_rich ln_price_av_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_priceRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none) ///
	varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_rich ln_units_sum_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_unitsRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_rich ln_upc_num_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_nupcRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout  new_upc_sum1000_rich new_upc_sum1000_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_newRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum1000_rich old_upc_sum1000_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_oldRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_rich ln_stores_num_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_StoreRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_rich ln_parent_num_poor using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ChainRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_rich ln_zip_num_poor  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ZipRICH.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
	
	
* TABLE A7 - PANEL B - PRIVATE EQUITY AND CONSUMER GOODS- ANNUAL COEFFICIENTS 
********************************************************************************

eststo clear
foreach var2 of varlist ln_* *dummy {

	reghdfe `var2' after_ym2 after_ym1 after_yp1-after_yp4   if ///
		time_to_event>-25 & time_to_event<61  , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
		
	eststo `var2'_yc
	}

estout ln_sales_sum_yc  ln_price_av_yc ln_units_sum_yc ln_upc_num_yc ///
	ln_stores_num_yc ln_parent_num_yc  ln_zip_num_yc   using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_all_year_coeff.tex",  ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
eststo clear	
		
	
* TABLE A9 - EFFECTS OF PE ON ACQUISITIVE FIRMS WITH EXTERNAL GROWTH
********************************************************************************

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy {

	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & acquiring_13==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_acq
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & acquiring_13==0, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_noacq
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_acq ln_sales_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_salesACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_acq ln_price_av_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_priceACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_acq ln_units_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_unitsACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label collabels(none) ///
	varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_acq ln_upc_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_nupcACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum1000_acq new_upc_sum1000_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_newACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum1000_acq old_upc_sum1000_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_oldACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_acq ln_stores_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_StoreACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_acq ln_parent_num_noacq using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ChainACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_acq ln_zip_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ZipACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


	
* TABLE A10: EFFECTS OF PE ON NON-ACQUISITIVE FIRMS WITH ORGANIC GROWTH
********************************************************************************

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy {
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & ///
		acquiring_98==1, absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date)
	eststo `var'_acq
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & ///
		acquiring_98==0, absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date)
	eststo `var'_noacq
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_acq ln_sales_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_salesACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_acq ln_price_av_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_priceACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_acq ln_units_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_unitsACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_acq ln_upc_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_nupcACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum1000_acq new_upc_sum1000_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_newACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
 
estout  old_upc_sum1000_acq old_upc_sum1000_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_oldACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_acq ln_stores_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_StoreACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_acq ln_parent_num_noacq using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ChainACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_acq ln_zip_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_ZipACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
	
	

	
* TABLE A11: PRIVATE EQUITY, SALES, AND PRICES - EXCLUDING TIME AFTER EXIT
********************************************************************************
	
	
eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & exit==0, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_1
	}

estout  ln_sales_sum_1 ln_price_av_1 ln_units_sum_1    using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_noexit.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) ///
	substitute(_ \_) 


estout   ln_upc_num_1 new_upc_sum1000_1 old_upc_sum1000_1   using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_noexit.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) ///
	substitute(_ \_) 


estout ln_stores_num_1  ln_parent_num_1 ln_zip_num_1  using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_noexit.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 




	
* TABLE A14: MECHANISM - ALL VARIABLES INTERACTED
********************************************************************************
	
				
* PANEL A - Correlation table

cd "$mainfolder/Paper/Tables/"
corrtex pus_pe SA_low growthpe mktshare0_high hhi_high rich, ///
	file(correl) replace case sig dig(3) nb	

gen a_pus_pe=after*pus_pe
label var a_pus_pe "After * Public"
gen a_SA_low=after*SA_low
label var a_SA_low "After * Low SA Index"
gen a_growthpe=after*growthpe
label var a_growthpe "After * Growth PE Firms"
gen a_mktshare0_high=after*mktshare0_high
label var a_mktshare0_high "After * High Market Share"
gen a_hhi_high=after*hhi_high
label var a_hhi_high "After * High HHI"
gen a_rich=after*rich
label var a_rich "After * High-Income Consumers"


 * PANELS B, C, D: REGRESSIONS

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy {
	reghdfe `var' after a_*  if time_to_event>-25 & time_to_event<61 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_int
	}

estout  ln_sales_sum_int ln_price_av_int ln_units_sum_int using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_sales_int.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout  ln_upc_num_int new_upc_sum1000_int old_upc_sum1000_int using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_product_int.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_int  ln_parent_num_int  ln_zip_num_int using ///
	"$mainfolder/Paper/Tables/TABLE_firm1000_analysis_match_geog1_int.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
	
	
	
********************************************************************************
********************************************************************************
** STEP 7- FIRM LEVEL ANALYSIS
********************************************************************************
********************************************************************************
		
use  "$mainfolder/Analysis/jf_files/final_firm", clear		


* RUN ALL REGRESSIONS

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma  {
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_0
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & pus_pe==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_puspe_0
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & priv_pe==1 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_prvpe_0
	}

	

	
* TABLE 3 PANEL A
********************************************************************************

estout ln_sales_sum_0  ln_price_av_0 ln_units_sum_0 using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none) varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
	
* TABLE 4 PANEL A
********************************************************************************
		
estout ln_upc_num_0 new_upc_sum_0 old_upc_sum_0  ln_industry1000_num_0 using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
* TABLE 5 PANEL A
********************************************************************************
		
estout ln_stores_num_0 ln_parent_num_0  ln_zip_num_0  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog.tex",  ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


* TABLE 8 PANEL A - WITHIN FIRM
********************************************************************************

estout ln_sales_sum_puspe_0 ln_sales_sum_prvpe_0 ln_price_av_puspe_0 ///
	ln_price_av_prvpe_0 ln_units_sum_puspe_0  ln_units_sum_prvpe_0   using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_split.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none) varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


* TABLE 8 PANEL B - WITHIN FIRM
********************************************************************************

estout  ln_upc_num_puspe_0  ln_upc_num_prvpe_0  new_upc_sum_puspe_0 ///
	new_upc_sum_prvpe_0 old_upc_sum_puspe_0 old_upc_sum_prvpe_0 ///
	ln_industry1000_num_puspe_0   ln_industry1000_num_prvpe_0      using  ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_split.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	

* TABLE 8 PANEL C - WITHIN FIRM
********************************************************************************

estout ln_stores_num_puspe_0  ln_stores_num_prvpe_0  ln_parent_num_puspe_0 ///
	ln_parent_num_prvpe_0 ln_zip_num_puspe_0  ln_zip_num_prvpe_0    using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog_split.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


* TABLE 13 PANEL A - WITHIN FIRM COL 1-3
********************************************************************************
	
estout n_ma_0 n_ma_puspe_0 n_ma_prvpe_0  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_acquisition.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 



* FIGURES 2, 3, 4 - WITHIN FIRM
********************************************************************************

cd "$mainfolder/Paper/Tables/"

foreach var2 of varlist ln_* *dummy {
	eststo clear
	reghdfe `var2' after_m*  after_p1-after_p60 if time_to_event>-25 & ///
		time_to_event<61  , absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date)
	eststo AllDeals
	
	coefplot AllDeals , levels(90) graphregion(color(white)) drop(_cons) ///
		vertical  yline(0) xline(25) recast(line)  lwidth(*2) ///
		ciopts(recast(rline) lpattern(dash) )  name("`var2'_firmmat0", replace) ///
		saving("`var2'_firmmat0", replace)
		
	graph export "`var2'_firmmat0.eps", replace
	
	}
cd "$mainfolder/Analysis/"




* TABLE 9 - WITHIN FIRM - MECHANISM - FINANCIAL CONSTRAINS
********************************************************************************

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma*{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & SA_high==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_large
	estadd scalar `var'_high_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & SA_medium==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_medium
	estadd scalar `var'_medium_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & SA_low==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_small
	estadd scalar `var'_small_nobs = e(N)
	
	}

* PANEL A
estout  ln_sales_sum_large ln_sales_sum_medium ln_sales_sum_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_salesLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout  ln_price_av_large ln_price_av_medium ln_price_av_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_priceLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_large ln_units_sum_medium ln_units_sum_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_unitsLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

*PANEL B

estout  ln_upc_num_large ln_upc_num_medium ln_upc_num_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_nupcLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum_large new_upc_sum_medium new_upc_sum_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_newLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum_large old_upc_sum_medium old_upc_sum_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_oldLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_industry1000_num_large  ln_industry1000_num_medium ///
	ln_industry1000_num_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_ncatLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 


*PANEL C
estout ln_stores_num_large ln_stores_num_medium ln_stores_num_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_StoreLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_large ln_parent_num_medium ln_parent_num_small using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ChainLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout ln_zip_num_large ln_zip_num_medium ln_zip_num_small  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ZipLARGESMALL.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	


* TABLE 10 PANEL A, B, AND C WITHIN FIRM - MECHANISM: PRIVATE EQUITY FIRM TYPE
********************************************************************************
	

eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma*{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & growthpe==1 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_acq
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & growthpe==0, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_noacq
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_acq ln_sales_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_sales_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_acq ln_price_av_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_price_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_acq ln_units_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_units_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
estout  ln_upc_num_acq ln_upc_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_nupc_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout  new_upc_sum_acq new_upc_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_new_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum_acq old_upc_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_old_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_industry1000_num_acq ln_industry1000_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_ncat_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
estout ln_stores_num_acq ln_stores_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_Store_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_acq ln_parent_num_noacq using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_Chain_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_acq ln_zip_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_Zip_growthpe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 



	
	
* TABLE 11 - WITHIN FIRM - MECHANISM - DURING VS AFTER FINANCIAL CRISIS
********************************************************************************


eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma*{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & deal_year<2011, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_pre
	estadd scalar `var'_nobs = e(N)

	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & deal_year>2010, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_post
	estadd scalar `var'_nobs = e(N)
	
	}

* PANEL A
	
estout  ln_sales_sum_pre ln_sales_sum_post  ///
	using "$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_salesPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_pre ln_price_av_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_pricePREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_pre ln_units_sum_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_unitsPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

* PANEL B

estout  ln_upc_num_pre ln_upc_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_nupcPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum_pre new_upc_sum_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_newPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum_pre old_upc_sum_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_oldPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_)
	
estout  ln_industry1000_num_pre ln_industry1000_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_ncatPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) labels("N. Obs.")) ///
	style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

* PANEL C
	
estout ln_stores_num_pre ln_stores_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_StorePREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_pre ln_parent_num_post using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ChainPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_pre ln_zip_num_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ZipPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
* TABLE 13 PANEL A - WITHIN FIRM COL 4,5
********************************************************************************
	
estout n_ma_pre n_ma_post  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_acquisitionPREPOST.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))   style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 



* TABLE A7 - PANEL A - PRIVATE EQUITY AND CONSUMER GOODS - ANNUAL COEFFICIENTS
********************************************************************************

eststo clear
foreach var2 of varlist ln_* *dummy n_ma*{

	reghdfe `var2' after_ym2 after_ym1 after_yp1-after_yp4  if time_to_event>-25 ///
		& time_to_event<61  , absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date) 
	eststo `var2'_yc
	}

estout ln_sales_sum_yc  ln_price_av_yc ln_units_sum_yc ln_upc_num_yc  ///
	ln_stores_num_yc ln_parent_num_yc  ln_zip_num_yc ln_industry1000_num_yc  ///
	using "$mainfolder/Paper/Tables/TABLE_firm0_all_year_coeff.tex",  ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) ///
	substitute(_ \_) 
	
eststo clear	
		
		


* TABLE A9 - EFFECTS OF PE ON ACQUISITIVE FIRMS WITH EXTERNAL GROWTH
********************************************************************************

eststo clear

foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma*{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & acquiring_13==1, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_acq
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & acquiring_13==0, ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date)
	eststo `var'_noacq
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_acq ln_sales_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_salesACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_acq ln_price_av_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_priceACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_acq ln_units_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_unitsACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
	
estout  ln_upc_num_acq ln_upc_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_nupcACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
		
estout  new_upc_sum_acq new_upc_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_newACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	stardetach replace  type label collabels(none)  varlabels(_cons Constant) ///
	mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum_acq old_upc_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_oldACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_industry1000_num_acq ln_industry1000_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_ncatACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
	
estout ln_stores_num_acq ln_stores_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_StoreACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_acq ln_parent_num_noacq using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ChainACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_acq ln_zip_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ZipACQ13.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 





	
* TABLE A10: EFFECTS OF PE ON NON-ACQUISITIVE FIRMS WITH ORGANIC GROWTH
********************************************************************************


eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma*{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & ///
		acquiring_98==1, absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date)
	eststo `var'_acq
	estadd scalar `var'_nobs = e(N)
	
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & ///
		acquiring_98==0, absorb(id_cohort month_date##cohort) ///
		vce(cluster id_nielsen month_date)
	eststo `var'_noacq
	estadd scalar `var'_nobs = e(N)
	
	}

estout  ln_sales_sum_acq ln_sales_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_salesACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_price_av_acq ln_price_av_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_priceACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_units_sum_acq ln_units_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_unitsACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
estout  ln_upc_num_acq ln_upc_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_nupcACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  new_upc_sum_acq new_upc_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_newACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  old_upc_sum_acq old_upc_sum_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_oldACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout  ln_industry1000_num_acq ln_industry1000_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_ncatACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout ln_stores_num_acq ln_stores_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_StoreACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_parent_num_acq ln_parent_num_noacq using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ChainACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) style(tex) ///
	cell("b(star fmt(%9.3f)) t(par fmt(2)) N")  ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_zip_num_acq ln_zip_num_noacq  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog1_ZipACQ98.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats( N, fmt(%11.0gc) ///
	labels("N. Obs."))  style(tex)  cell("b(star fmt(%9.3f)) t(par fmt(2)) N") ///
	starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type label ///
	collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 



	

* TABLE A11: PRIVATE EQUITY, SALES, AND PRICES - EXCLUDING TIME AFTER EXIT
********************************************************************************
			
eststo clear
foreach var of varlist ln_* new_upc* old_upc* *dummy n_ma  {

	reghdfe `var' after if time_to_event>-25 & time_to_event<61 & exit==0 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_0

	}

	
estout ln_sales_sum_0  ln_price_av_0 ln_units_sum_0 using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_sales_noexit.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  ///
	type label collabels(none) varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

estout   ln_upc_num_0 new_upc_sum_0 old_upc_sum_0  ln_industry1000_num_0 ///
	using "$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_product_noexit.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout ln_stores_num_0 ln_parent_num_0  ln_zip_num_0  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_geog_noexit.tex",  ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 




* TABLE 13 - PANEL B - MECHANISM - COMPANY STRATEGY AND INVESTMENTS - ADVERTISING
********************************************************************************


use "$mainfolder/Analysis/jf_files/firm_allstores_matched0", clear

merge m:1 id_nielsen using  jf_files/pe_events, keepusing(pub2priv second) 
drop if _merge==2
drop _merge


gen temp_pus_pe= ((pub2priv==1) & treat==1)
bys cohort  month_date: egen pus_pe=max(temp_pus_pe) 

gen temp_priv_pe= (pub2priv==0 & treat==1)
bys cohort  month_date: egen priv_pe=max(temp_priv_pe) 


bys month_date cohort: egen temp=max(deal_date)
gen deal_year=year(temp)	
drop temp


gen year=year(dofm(month_date))
gen after_year=(year>=deal_year)
gen year_to_event=year-deal_year
keep id_nielsen cohort year deal_year after_year year_to_event id_cohort pus_pe priv_pe
duplicates drop

merge id_nielsen year using  jf_files/advertising_clean, sort uniqusing nokeep


drop if  _merge==1
tab _merge
drop _merge

label var after_year "After"

eststo clear
foreach var of varlist ln_ad  {
	reghdfe `var' after_year  if year_to_event>-3 & year_to_event<6 , ///
		absorb(id_cohort year) vce(cluster id_nielsen year)
	eststo `var'_0
	
	reghdfe `var' after_year  if year_to_event>-3 & year_to_event<6 & ///
		pus_pe==1, absorb(id_cohort year) vce(cluster id_nielsen year)
	eststo `var'_puspe_0
	
	reghdfe `var' after_year  if year_to_event>-3 & year_to_event<6 & ///
		priv_pe==1, absorb(id_cohort year) vce(cluster id_nielsen year)
	eststo `var'_prvpe_0
	
	reghdfe `var' after_year  if year_to_event>-3 & year_to_event<6 & ///
		deal_year<2011, absorb(id_cohort year) vce(cluster id_nielsen year)
	eststo `var'_pre_0
	
	reghdfe `var' after_year  if year_to_event>-3 & year_to_event<6 & ///
		deal_year>2010, absorb(id_cohort year) vce(cluster id_nielsen year)
	eststo `var'_post_0
	
	
	}

estout ln_ad_0 ln_ad_puspe_0 ln_ad_prvpe_0 ln_ad_pre_0 ln_ad_post_0 using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_analysis_match_MARKETING.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  type ///
	label collabels(none) varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	

* TABLE 13 - PANEL B - MECHANISM - COMPANY STRATEGY AND INVESTMENTS - PRICE ADJ
********************************************************************************
	
use jf_files/stdev_data, clear

eststo clear
reghdfe sd_price after, absorb(cohort#id_nielsen) vce(cluster id_nielsen)
eststo m1
reghdfe sd_price after##c.hhi, absorb(cohort#id_nielsen) vce(cluster id_nielsen)
eststo m2
reghdfe sd_price after##c.mktshare0, absorb(cohort#id_nielsen) vce(cluster id_nielsen)
eststo m3
reghdfe sd_price after##hhi_high, absorb(cohort#id_nielsen) vce(cluster id_nielsen)
eststo m4
reghdfe sd_price after##mktshare0_high, absorb(cohort#id_nielsen) vce(cluster id_nielsen)
eststo m5
reghdfe sd_price after##crisis, absorb(cohort#id_nielsen) vce(cluster id_nielsen)
eststo m6

estout using "$mainfolder/Paper/Tables/TABLE_price_stdev_uni.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  ///
	type label collabels(none) mlabels(,titles) varlabels(_cons Constant) 


		
********************************************************************************
********************************************************************************
* STEP 8: COMPETITION RESPONSE ANALYSIS
********************************************************************************
********************************************************************************



* PRODUCT LEVEL - PRICES
********************************************************************************


eststo clear 

use "$mainfolder/Analysis/jf_files/competition_all", clear
* define growth in price

sort id month_date

gen price_growth=(month_unitprice-l12.month_unitprice)/l12.month_unitprice

gen high_growth= ((price_growth>1 & price_growth~=.) | price_growth<-0.5)
bys id: egen high_growth_spam=max(high_growth)
drop if high_growth_spam==1

* Keep store with closest price level and growth pre-trend

gen price0=month_unitprice if time_to_event==0 & treat==1
gen price_growth0=price_growth if time_to_event==0 & treat==1

bys cohort: egen price0treat=max(price0)
bys cohort: egen price_growth0treat=max(price_growth0)

drop price0 price_growth0

gen deltaprice=abs(month_unitprice-price0treat)
gen deltaprice_growth=abs(price_growth-price_growth0treat)

bys cohort: egen sdprice=sd(month_unitprice-price0treat)
bys cohort: egen sdprice_growth=sd(price_growth-price_growth0treat)

gen distance=sqrt((deltaprice/sdprice)^2 + (deltaprice_growth/sdprice_growth)^2)

bys cohort time_to_event treat (distance store_id) : gen dist=_n if treat==0

gen best=1 if dist==1 & time_to_event==0

bys cohort store_id: egen maxbest=max(best)

bys treat: sum price_growth if time_to_event==0 & (dist==1 | dist==.), d

count
keep if treat==1 | maxbest==1
count


drop id maxbest best dist distance sdprice* deltaprice* price_growt* price0* 

egen double id=group(cohort store_id)

gen after=0
replace after=1 if treat==1 & time_to_event>-1

gen ln_prices=ln(month_unitprice)
*gen ln_sales=ln(1+month_sales)
*gen ln_units=ln(1+month_units)


keep id ln_* after cohort time_to_event month_date treat

label var after "After"


gen after0=0
replace after0=1 if time_to_event==0 & treat==1

forvalues t = 24(-1)1 {
	gen after_m`t'=0 
	replace after_m`t'=1 if time_to_event==-`t' & treat==1
	label var after_m`t' "-`t'"
	}
forvalues t = 1/60 {
	gen after_p`t'=0
	replace after_p`t'=1 if time_to_event==`t' & treat==1
	label var after_p`t' "+`t'"
	}

		
	forvalues v = 1/70 {
		capture label var after_m`v' " " 
		capture label var after_p`v' " " 
		}

	label var after_m24 "-24"
	label var after_m12 "-12"
	label var after_p1 "+1"
	label var after_p12 "+12"
	label var after_p24 "+24"
	label var after_p36 "+36"
	label var after_p48 "+48"
	label var after_p60 "+60"



* TABLE 6 - PANEL A - COL 1 COMPETITOR RESPONSE
********************************************************************************
				
reghdfe ln_prices after, absorb(id cohort##time_to_event) vce(cluster id month_date)  
eststo prices_all



* FIGURE 5 PANEL A- COMPETITOR RESPONSE
********************************************************************************
	

reghdfe ln_prices after_m* after_p* if time_to_event>-25 & time_to_event<61 , ///
	absorb(id cohort##time_to_event) vce(cluster id)  
	
coefplot , levels(90) graphregion(color(white)) drop(_cons) vertical yline(0) ///
	xline(25) recast(line) ciopts(recast(rline) lpattern(dash)) lwidth(*2) ///
	name("competitor_response_all", replace) ///
	saving("$mainfolder/Paper/Tables/competitor_response_all", replace)
graph export "$mainfolder/Paper/Tables/competitor_response_all.eps", replace
		


* TABLE 6 - PANEL A - COL 2,3 COMPETITOR RESPONSE
********************************************************************************

foreach var in sameparent samedma  {
	 
	use "$mainfolder/Analysis/jf_files/competition_`var'", clear
	* define growth in price

	sort id month_date

	gen price_growth=(month_unitprice-l12.month_unitprice)/l12.month_unitprice

	gen high_growth= ((price_growth>1 & price_growth~=.) | price_growth<-0.5)
	bys id: egen high_growth_spam=max(high_growth)
	drop if high_growth_spam==1

	* Keep store with closest price level and growth pre-trend

	gen price0=month_unitprice if time_to_event==0 & treat==1
	gen price_growth0=price_growth if time_to_event==0 & treat==1

	bys cohort: egen price0treat=max(price0)
	bys cohort: egen price_growth0treat=max(price_growth0)

	drop price0 price_growth0

	gen deltaprice=abs(month_unitprice-price0treat)
	gen deltaprice_growth=abs(price_growth-price_growth0treat)

	bys cohort: egen sdprice=sd(month_unitprice-price0treat)
	bys cohort: egen sdprice_growth=sd(price_growth-price_growth0treat)

	gen distance=sqrt((deltaprice/sdprice)^2 + (deltaprice_growth/sdprice_growth)^2)

	bys cohort time_to_event treat (distance store_id) : gen dist=_n if treat==0

	gen best=1 if dist==1 & time_to_event==0

	bys cohort store_id: egen maxbest=max(best)

	bys treat: sum price_growth if time_to_event==0 & (dist==1 | dist==.), d

	count
	keep if treat==1 | maxbest==1
	count


	drop id maxbest best dist distance sdprice* deltaprice* price_growt* price0* 

	egen double id=group(cohort store_id)

	gen after=0
	replace after=1 if treat==1 & time_to_event>-1

	gen ln_prices=ln(month_unitprice)
	*gen ln_sales=ln(1+month_sales)
	*gen ln_units=ln(1+month_units)


	keep id ln_* after cohort time_to_event month_date treat

	label var after "After"

	reghdfe ln_prices after, absorb(id cohort##time_to_event) vce(cluster id month_date) 
	eststo prices_`var'

	gen after0=0
	replace after0=1 if time_to_event==0 & treat==1

	forvalues t = 24(-1)1 {
		gen after_m`t'=0 
		replace after_m`t'=1 if time_to_event==-`t' & treat==1
		label var after_m`t' "-`t'"
		}
	forvalues t = 1/60 {
		gen after_p`t'=0
		replace after_p`t'=1 if time_to_event==`t' & treat==1
		label var after_p`t' "+`t'"
		}

		
	forvalues v = 1/70 {
		capture label var after_m`v' " " 
		capture label var after_p`v' " " 
		}

	label var after_m24 "-24"
	label var after_m12 "-12"
	label var after_p1 "+1"
	label var after_p12 "+12"
	label var after_p24 "+24"
	label var after_p36 "+36"
	label var after_p48 "+48"
	label var after_p60 "+60"
	
	


* FIGURE A3 - COMPETITOR RESPONSE BY TYPE
********************************************************************************
	
	reghdfe ln_prices after_m* after_p* if time_to_event>-25 & time_to_event<61 ///
		, absorb(id cohort##time_to_event) vce(cluster id) 
		
	coefplot , levels(90) graphregion(color(white)) drop(_cons) vertical ///
		yline(0)  xline(25) recast(line) ciopts(recast(rline) lpattern(dash)) ///
		lwidth(*2)  name("competitor_response_`var'", replace) ///
		saving("$mainfolder/Paper/Tables/competitor_response_`var'", replace)
		
	graph export "$mainfolder/Paper/Tables/competitor_response_`var'.eps", replace
			
	}
 
 
 * estout table
 estout prices_all prices_sameparent prices_samedma  ///
	using "$mainfolder/Paper/Tables/TABLE_competitor_prices.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  ///
	type label collabels(none) mlabels(,none) varlabels(_cons Constant)  

 
foreach var in  full dma parent   {
	use jf_files/competitor_deal_nodeal_`var'_short, clear
	keep in 1/500
	keep id_nielsen 
	duplicates drop
	sort id_nielsen
	gen id_nielsen2 = _n
	save temp_mapping/competitor_deal_nodeal_`var'_short, replace

	use jf_files/competitor_deal_nodeal_`var'_short, clear
	merge m:1 id_nielsen using temp_mapping/competitor_deal_nodeal_`var'_short, nogenerate keep(3)
	drop id_nielsen
	rename id_nielsen2 id_nielsen
	compress
	save jf_files/competitor_deal_nodeal_`var'_short, replace
	}

	

*  * FIRM LEVEL - N. UPC
********************************************************************************


eststo clear
foreach var in full dma parent   {
	use jf_files/competitor_deal_nodeal_`var'_short, clear
		
	
	forvalues v = 1/70 {
		capture label var after_m`v' " " 
		capture label var after_p`v' " " 
		}

	label var after_m24 "-24"
	label var after_m12 "-12"
	label var after_p1 "+1"
	label var after_p12 "+12"
	label var after_p24 "+24"
	label var after_p36 "+36"
	label var after_p48 "+48"
	label var after_p60 "+60"
	

* TABLE 6 - PANEL B - COL 1 COMPETITOR RESPONSE
********************************************************************************
						
	reghdfe ln_upc_num after if max_window==1, ///
		absorb(i.cohort#i.store_id#i.industry i.cohort#i.month_date#i.industry) ///
		cluster(id  month_date)
	eststo ln_upc_`var'



* FIGURE 5 PANEL B - COMPETITOR RESPONSE
********************************************************************************
								

	reghdfe ln_upc_num after_m*  after_p* if max_window==1, ///
		absorb(i.cohort#i.store_id#i.industry i.cohort#i.month_date#i.industry) ///
		cluster(id  month_date)
		
	coefplot , levels(90) graphregion(color(white)) drop(_cons) vertical ///
		yline(0)  xline(25) recast(line) ciopts(recast(rline) lpattern(dash)) ///
		lwidth(*2)  name("competitor_ln_nupc_`var'", replace) ///
		saving("$mainfolder/Paper/Tables/competitor_nupc_ln_`var'", replace)
		
	graph export "$mainfolder/Paper/Tables/competitor_nupc_ln_`var'.eps", replace	
	
	 
	
	}

*ESTOUT TABLE

estout ln_upc_full ln_upc_parent ln_upc_dma   using ///
	"$mainfolder/Paper/Tables/TABLE_competitor_nupc.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none) mlabels(,none) varlabels(_cons Constant)  


 

********************************************************************************
********************************************************************************
* STEP 9: OTHER ONLINE APPENDIX TABLES  
********************************************************************************
********************************************************************************

 

* TABLE A2: LIST OF LARGEST PRODUCT CATEGORIES
********************************************************************************

use "$mainfolder/Analysis/jf_files/firm_allstores_data1000", clear
label var industry1000 "Product Category"
bys industry1000 month_date: egen sales_tot=total(sales_sum_all1000)
bys industry1000 month_date: egen nupc_tot=total(upc_num_all1000)
keep industry1000 month_date sales_tot nupc_tot 
duplicates drop
bys industry1000: egen sales = mean(sales_tot)
bys industry1000: egen nupc = mean(nupc_tot)
drop month_date sales_tot nupc_tot
duplicates drop
rename industry1000 product_module_code1
merge m:m product_module_code1 using jf_files/product1, ///
	keepusing(product_module_code1 product_module_descr1)
drop _merge
duplicates drop
drop if sales==.
gsort -sales product_module_descr1
format sales %12.0fc 
format nupc %12.0fc
cd "$mainfolder/Paper/Tables"
listtex product_module_descr1 sales nupc if _n<12 using ///
	"list_topcat.tex", replace rs(tabular)



* TABLE A3: LIST OF MOST COMMON PRIVATE EQUITY BUYERS
********************************************************************************
cd "$mainfolder/Analysis"
use jf_files/firm_allstores_matched0.dta, clear
keep if after0==1
keep id_nielsen
duplicates drop
count
joinby id_nielsen using jf_files/upc_firm, unmatched(none)
drop upc
duplicates drop
count
replace name=strtrim(name)
drop if name=="lipari foods, llc"
rename name firm_gs1
joinby firm_gs1 using jf_files/pe_deals_details, unmatched(master)
tab _merge
drop if pe_name==""
preserve
bys id_nielsen: egen tot_deal_num_av=mean(total_deal_num)
bys id_nielsen: egen tot_deal_consumer_num_av=mean(total_deal_consumer_num)
bys id_nielsen: egen tot_consumer_frac_av=mean(total_consumer_frac)
keep id_nielsen tot_*
duplicates drop
sum tot_deal_num_av, d
gen high_deal_num=(tot_deal_num_av>r(p50)) if tot_deal_num_av~=.
sum tot_deal_consumer_num_av, d
gen high_deal_consumer_num=(tot_deal_consumer_num_av>r(p50)) if tot_deal_consumer_num_av~=.
sum tot_consumer_frac_av, d
gen high_consumer_frac=(tot_consumer_frac_av>r(p50)) if tot_consumer_frac_av~=.
sort id_nielsen
save datasets/id_deal_details, replace
restore
bys pe_name: gen ndeal=_N
keep pe_name ndeal
duplicates drop
drop if pe_name==""
gsort -ndeal
keep if _n<11
cd "$mainfolder/Paper/Tables"
listtex pe_name ndeal using "list_gp.tex", replace rs(tabular)



* TABLE A4: LARGEST PRIVATE EQUITY DEALS
********************************************************************************
cd "$mainfolder/Analysis"
use jf_files/firm_allstores_matched0.dta, clear
keep if after0==1
keep id_nielsen deal_date sales_sum_all0
duplicates drop
joinby id_nielsen using jf_files/upc_firm, unmatched(none)
drop upc
duplicates drop
replace name=proper(name)
gsort -sales_sum_all0 name
format sales_sum_all0 %12.0fc
cd "$mainfolder/Paper/Tables"
listtex name deal_date sales_sum_all0  if _n<11 using ///
	"list_dealvalue.tex", replace rs(tabular)

 

* TABLE A5: PRIVATE EQUITY DEAL SELECTION
********************************************************************************

		
****** within the same store across INDUSTRY. 

cd "$mainfolder/Analysis"


eststo clear

use jf_files/store_industry, clear

rename industry industry1000

merge m:1 industry1000 using jf_files/richpoor, keepusing(rich) keep(1 3)

rename industry1000 industry
label var rich "Rich Category"
reghdfe pe_industry  rich hi ln_price_av ln_sales *growth_w, absorb(month_date) cluster(industry)
eststo industry_target

* Firm Targets
use jf_files/firm_industry_target, clear

reghdfe event ln_price_av ln_sales nupcs_growth_w sales_growth_w price_growth_w ///
	if event_industry==1, absorb(month_date#industry) cluster(id_nielsen)
eststo firm_target


* UPC targets

use jf_files/upc_industry_target, clear

reghdfe event ln_price_av ln_sales sales_growth_w price_growth_w if ///
	event_industry==1, absorb(month_date#industry) cluster(upc)
eststo upc_target


estout using "$mainfolder/Paper/Tables/TABLE_predict_pe.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs.")) style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant) mlabels(, none) ///
	substitute(_ \_ hi "Herfindal Index" nupcs\_growth\_w "Growth N. Products") 



* TABLE A8: MERGER PRICES, SALES, UNITS, PRODUCT AVAILABILITY
********************************************************************************

use Datasets/firm_allstores_MA_matched0, clear

forvalues v = 1/70 {
	capture label var after_m`v' " " 
	capture label var after_p`v' " " 
	}

label var after_m24 "-24"
label var after_m12 "-12"
label var after_p1 "+1"
label var after_p12 "+12"
label var after_p24 "+24"
label var after_p36 "+36"
label var after_p48 "+48"
label var after_p60 "+60"


	
rename month_date date
merge m:1 id_nielsen date using Datasets/new_old, keepusing(new_upc_sum* old_upc_sum* *dummy) 
drop if _merge==2
drop _merge
rename date month_date


foreach var of varlist ln_*  new_upc* old_upc*{
	reghdfe `var' after if time_to_event>-25 & time_to_event<61 , ///
		absorb(id_cohort month_date##cohort) vce(cluster id_nielsen month_date) 
	eststo `var'_0
	}

estout ln_sales_sum_0  ln_price_av_0 ln_units_sum_0 using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_MA_analysis_match_sales.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none) varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
	
estout   ln_upc_num_0 new_upc_sum_0 old_upc_sum_0  ln_industry1000_num_0   using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_MA_analysis_match_product.tex", ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace  ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 

	
estout ln_stores_num_0 ln_parent_num_0  ln_zip_num_0  using ///
	"$mainfolder/Paper/Tables/TABLE_firm0_MA_analysis_match_geog.tex",  ///
	drop(_cons _Iff49ind* _Ifyear*, relax) stats(r2_a N, fmt(%9.3f  %11.0gc) ///
	labels("Adj. R-Square" "N. Obs."))  style(tex) cell(b(star fmt(%9.3f)) ///
	t(par fmt(2))) starlevels(* 0.10 ** 0.05 *** 0.01) stardetach replace ///
	type label collabels(none)  varlabels(_cons Constant)  mlabels(, none) substitute(_ \_) 
				


********************************************************************************
********************************************************************************
** STEP 10: OTHER STATISTICS MENTIONED IN THE TEXT OF THE PAPER
********************************************************************************
********************************************************************************

* UPCS MATCHED WITH GS1
********************************************************************************

use jf_files/temp_upc_names, clear
destring upc, replace force
sort upc
save jf_files/tempupc, replace


use jf_files/nupc_all, clear

merge m:1 upc using jf_files/tempupc, keepusing(id_nielsen name) keep(1 3)
*erase jf_files/tempupc.dta
tab _merge
count if id_nielsen<100000
count

drop _merge

merge m:1 upc using jf_files/ctl_br, keep(1 3)
tab ctl_br


* N of treated products, and average stores. 
********************************************************************************

use upc store_id month_date cohort treat if treat==1 using ///
	"jf_files/upc_data_matched_best1_allstores_shortv2", clear
unique upc
unique cohort
bys upc month_date: gen nstore_upc=_N
bys upc: egen nstore_upc_av=mean(nstore_upc)
keep upc nstore_upc_av
duplicates drop
sum nstore_upc_av


* n. of treated products
********************************************************************************

use jf_files/nupc_all, clear
merge m:1 upc using jf_files/tempupc
drop if _merge==2
drop _merge
merge m:1 id_nielsen using datasets/list_treat,
count if _merge==3



* HOW MANY TREATMENT AND CONTROL FIRMS DROP OUT OF THE SAMPLE AFTER THE DEAL? 
********************************************************************************


use jf_files/firm_allstores_matched0, clear
unique id_nielsen, by(treat time_to_event) gen(nf)
keep time_to_event treat nf
duplicates drop
drop if nf==.
reshape wide nf, i(time_to_event) j(treat)
label var nf0 "N. Control Firms"
label var  nf1 "N. Treatment Firms"
label var time_to_event "Time to Event"
sort time_to_event
twoway (line nf1 time_to_event if time_to_event>-25 &  time_to_event<61, ///
	lpattern(solid) lcolor(blue) lwidth(large))  (line nf0 time_to_event if ///
	time_to_event>-25 &  time_to_event<61, lpattern(dash) lcolor(red) ///
	lwidth(large) ), ysc(r(0 250)) ylabel(0(50)250, angle(0) grid) ///
	graphregion(color(white))  xlabel(#10, labsize(small))  ///
	name("nfirms_year_treat", replace) saving("nfirms_year_treat", replace)
	
graph export "$mainfolder/Paper/Tables/nfirms_year_treat.eps", replace

* do the same, but only for deals in 2008-2011, for which we have the full data
use jf_files/firm_allstores_matched0, clear
bys cohort: egen deal_date2=max(deal_date)
keep if year(deal_date2)>2007 & year(deal_date2)<2012
unique id_nielsen, by(treat time_to_event) gen(nf)
keep time_to_event treat nf
duplicates drop
drop if nf==.
reshape wide nf, i(time_to_event) j(treat)
label var nf0 "N. Control Firms"
label var  nf1 "N. Treatment Firms"
label var time_to_event "Time to Event"
sort time_to_event
twoway (line nf1 time_to_event if time_to_event>-25 &  time_to_event<61, ///
	lpattern(solid) lcolor(blue) lwidth(large))  (line nf0 time_to_event ///
	if time_to_event>-25 &  time_to_event<61, lpattern(dash) lcolor(red) ///
	lwidth(large) ), ysc(r(0 100)) ylabel(0(20)100, angle(0) grid) ///
	graphregion(color(white))  xlabel(#10, labsize(small))  ///
	name("nfirms_year_treat", replace) saving("nfirms_year_treat200811", replace)
	
graph export "$mainfolder/Paper/Tables/nfirms_year_treat200811.eps", replace













