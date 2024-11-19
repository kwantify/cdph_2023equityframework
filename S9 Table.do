* ACS age, sex, race data
* Source https://data.chhs.ca.gov/dataset/covid-19-time-series-metrics-by-county-and-state

* Created by: Ada Kwan
* Created on: Nov 11, 2024
* Edited on:

*****************************************************
** GLOBALS
*****************************************************

cd "/Users/adak/Library/CloudStorage/Box-Box/___PROJECTS/__UCSF_TBIIC LAB/0 PROJECTS/EQUIP-CA/601 ANALYSIS/2024-1111 framework/"

glo data "data/"
glo outputs "outputs"


*****************************************************
** IMPORTING DATASETS: acs, hpi website, index comparisons
*****************************************************


import delimited "${data}acs_age_sex.csv", varnames(1) encoding(UTF-8) clear 

	drop v459

	foreach var of varlist * {
	  label variable `var' "`=`var'[1]'"
	  replace `var'="" if _n==1
	  destring `var', replace
	}
	
	drop in 1
	drop *m
	
	gen geoid = substr(geo_id, -10, .)
		order geoid geo_id
		ren geo_id geoid_full
		ren name geoid_name
			
	keep geoid geoid_full geoid_name ///
	s0101_c01_001e s0101_c01_002e s0101_c01_003e s0101_c01_004e s0101_c01_005e s0101_c01_006e s0101_c01_007e s0101_c01_008e s0101_c01_009e s0101_c01_010e s0101_c01_011e s0101_c01_012e s0101_c01_013e s0101_c01_014e s0101_c01_015e s0101_c01_016e s0101_c01_017e s0101_c01_018e s0101_c01_019e ///
	s0101_c02_002e s0101_c02_003e s0101_c02_004e s0101_c02_005e s0101_c02_006e s0101_c02_007e s0101_c02_008e s0101_c02_009e s0101_c02_010e s0101_c02_011e s0101_c02_012e s0101_c02_013e s0101_c02_014e s0101_c02_015e s0101_c02_016e s0101_c02_017e s0101_c02_018e s0101_c02_019e 
	
	ren s0101_c02_002e s0101_c01_002e_perc
	ren s0101_c02_003e s0101_c01_003e_perc
	ren s0101_c02_004e s0101_c01_004e_perc 
	ren s0101_c02_005e s0101_c01_005e_perc 
	ren s0101_c02_006e s0101_c01_006e_perc 
	ren s0101_c02_007e s0101_c01_007e_perc 
	ren s0101_c02_008e s0101_c01_008e_perc 
	ren s0101_c02_009e s0101_c01_009e_perc 
	ren s0101_c02_010e s0101_c01_010e_perc 
	ren s0101_c02_011e s0101_c01_011e_perc 
	ren s0101_c02_012e s0101_c01_012e_perc
	ren s0101_c02_013e s0101_c01_013e_perc
	ren s0101_c02_014e s0101_c01_014e_perc
	ren s0101_c02_015e s0101_c01_015e_perc
	ren s0101_c02_016e s0101_c01_016e_perc
	ren s0101_c02_017e s0101_c01_017e_perc
	ren s0101_c02_018e s0101_c01_018e_perc
	ren s0101_c02_019e s0101_c01_019e_perc
		
			*la var s0101_c01_001e_perc "Perc of total population"
			la var s0101_c01_002e_perc "Perc of total population!!AGE!!Under 5 years"
			la var s0101_c01_003e_perc "Perc of total population!!AGE!!5 to 9 years"
			la var s0101_c01_004e_perc "Perc of total population!!AGE!!10 to 14 years"
			la var s0101_c01_005e_perc "Perc of total population!!AGE!!15 to 19 years"
			la var s0101_c01_006e_perc "Perc of total population!!AGE!!20 to 24 years"
			la var s0101_c01_007e_perc "Perc of total population!!AGE!!25 to 29 years"
			la var s0101_c01_008e_perc "Perc of total population!!AGE!!30 to 34 years"
			la var s0101_c01_009e_perc "Perc of total population!!AGE!!35 to 39 years"
			la var s0101_c01_010e_perc "Perc of total population!!AGE!!40 to 44 years"
			la var s0101_c01_011e_perc "Perc of total population!!AGE!!45 to 49 years"
			la var s0101_c01_012e_perc "Perc of total population!!AGE!!50 to 54 years"
			la var s0101_c01_013e_perc "Perc of total population!!AGE!!55 to 59 years"
			la var s0101_c01_014e_perc "Perc of total population!!AGE!!60 to 64 years"
			la var s0101_c01_015e_perc "Perc of total population!!AGE!!65 to 69 years"
			la var s0101_c01_016e_perc "Perc of total population!!AGE!!70 to 74 years"
			la var s0101_c01_017e_perc "Perc of total population!!AGE!!75 to 79 years"
			la var s0101_c01_018e_perc "Perc of total population!!AGE!!80 to 84 years"
			la var s0101_c01_019e_perc "Perc of total population!!AGE!!85 years and over"
			
			
		foreach var of varlist s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc {
			replace `var' = "" if `var' == "-"
		}
			
		destring s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc, replace

	save "${data}acs_age_sex.dta", replace
	use "${data}acs_age_sex.dta", clear
	
		drop s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc
	
	
		gen geoid_trunc = substr(geoid, -10, 9)
		gen geoid_last = substr(geoid, -1, .)
			
			duplicates report geoid_trunc
			
		replace geoid_trunc = geoid_trunc + "0" if strlen(geoid_trunc) == 9
		order geoid geoid_full geoid_name geoid_trunc
	
		local sum_vars s0101_c01_001e s0101_c01_002e s0101_c01_003e s0101_c01_004e s0101_c01_005e s0101_c01_006e s0101_c01_007e s0101_c01_008e s0101_c01_009e s0101_c01_010e s0101_c01_011e s0101_c01_012e s0101_c01_013e s0101_c01_014e s0101_c01_015e s0101_c01_016e s0101_c01_017e s0101_c01_018e s0101_c01_019e

	
		collapse (sum) `sum_vars', by(geoid_trunc)
			
			
			la var s0101_c01_001e "Total population"
			la var s0101_c01_002e "Total population!!AGE!!Under 5 years"
			la var s0101_c01_003e "Total population!!AGE!!5 to 9 years"
			la var s0101_c01_004e "Total population!!AGE!!10 to 14 years"
			la var s0101_c01_005e "Total population!!AGE!!15 to 19 years"
			la var s0101_c01_006e "Total population!!AGE!!20 to 24 years"
			la var s0101_c01_007e "Total population!!AGE!!25 to 29 years"
			la var s0101_c01_008e "Total population!!AGE!!30 to 34 years"
			la var s0101_c01_009e "Total population!!AGE!!35 to 39 years"
			la var s0101_c01_010e "Total population!!AGE!!40 to 44 years"
			la var s0101_c01_011e "Total population!!AGE!!45 to 49 years"
			la var s0101_c01_012e "Total population!!AGE!!50 to 54 years"
			la var s0101_c01_013e "Total population!!AGE!!55 to 59 years"
			la var s0101_c01_014e "Total population!!AGE!!60 to 64 years"
			la var s0101_c01_015e "Total population!!AGE!!65 to 69 years"
			la var s0101_c01_016e "Total population!!AGE!!70 to 74 years"
			la var s0101_c01_017e "Total population!!AGE!!75 to 79 years"
			la var s0101_c01_018e "Total population!!AGE!!80 to 84 years"
			la var s0101_c01_019e "Total population!!AGE!!85 years and over"
			
		foreach var of varlist s0101_c01_001e s0101_c01_002e s0101_c01_003e s0101_c01_004e s0101_c01_005e s0101_c01_006e s0101_c01_007e s0101_c01_008e s0101_c01_009e s0101_c01_010e s0101_c01_011e s0101_c01_012e s0101_c01_013e s0101_c01_014e s0101_c01_015e s0101_c01_016e s0101_c01_017e s0101_c01_018e s0101_c01_019e {
			gen `var'_perc = 100 * `var' / s0101_c01_001e
 		}
			
			
			la var s0101_c01_001e_perc "Perc of total population"
			la var s0101_c01_002e_perc "Perc of total population!!AGE!!Under 5 years"
			la var s0101_c01_003e_perc "Perc of total population!!AGE!!5 to 9 years"
			la var s0101_c01_004e_perc "Perc of total population!!AGE!!10 to 14 years"
			la var s0101_c01_005e_perc "Perc of total population!!AGE!!15 to 19 years"
			la var s0101_c01_006e_perc "Perc of total population!!AGE!!20 to 24 years"
			la var s0101_c01_007e_perc "Perc of total population!!AGE!!25 to 29 years"
			la var s0101_c01_008e_perc "Perc of total population!!AGE!!30 to 34 years"
			la var s0101_c01_009e_perc "Perc of total population!!AGE!!35 to 39 years"
			la var s0101_c01_010e_perc "Perc of total population!!AGE!!40 to 44 years"
			la var s0101_c01_011e_perc "Perc of total population!!AGE!!45 to 49 years"
			la var s0101_c01_012e_perc "Perc of total population!!AGE!!50 to 54 years"
			la var s0101_c01_013e_perc "Perc of total population!!AGE!!55 to 59 years"
			la var s0101_c01_014e_perc "Perc of total population!!AGE!!60 to 64 years"
			la var s0101_c01_015e_perc "Perc of total population!!AGE!!65 to 69 years"
			la var s0101_c01_016e_perc "Perc of total population!!AGE!!70 to 74 years"
			la var s0101_c01_017e_perc "Perc of total population!!AGE!!75 to 79 years"
			la var s0101_c01_018e_perc "Perc of total population!!AGE!!80 to 84 years"
			la var s0101_c01_019e_perc "Perc of total population!!AGE!!85 years and over"
		
			ren geoid_trunc geoid
		
		save "${data}aggregated_acs_age_sex.dta", replace

	
	
import delimited "${data}acs_race.csv", varnames(1) encoding(UTF-8) clear 

	drop v76

	foreach var of varlist * {
		  label variable `var' "`=`var'[1]'"
		  replace `var'="" if _n==1
		  destring `var', replace
		}
		
		drop in 1
		
		gen geoid = substr(geo_id, -10, .)
			order geoid geo_id
			ren geo_id geoid_full
			ren name geoid_name
			
		keep geoid geoid_full geoid_name ///
			p9_001n p9_002n p9_003n p9_004n p9_005n p9_006n p9_007n p9_008n p9_009n p9_010n p9_011n
			
	save "${data}acs_race.dta", replace
	
	
	
	
	
import delimited "${data}hpi_diversity_index.csv", varnames(1) encoding(UTF-8) clear 
	/*
	Technical definition
	Probability that two people in this geography, chosen at random, will be of different race/ethnicities (Gini-Simpson Diversity Index)
	The value of Simpson's D ranges from 0 to 1, with 0 representing infinite diversity and 1 representing no diversity, so the larger the value of D, the lower the diversity. For this reason, Simpson's 
		index is usually expressed as its inverse (1/D) or its compliment (1-D) which is also known as the Gini-Simpson index

	Data source
	American Community Survey, Table DP05

	Year(s)
	2015 - 2019
	*/

		tostring geoid, replace
		ren population diversity_index_population
		ren value  diversity_index_value
		ren percentile  diversity_index_percentile 
		
		keep geoid diversity_index_population diversity_index_value diversity_index_percentile

	save "${data}hpi_diversity_index.dta", replace
	
import delimited "${data}hpiscore_v2.csv", varnames(1) encoding(UTF-8) clear 

	tostring geoid, replace
	ren population hpi_v2_population
	ren value  hpi2_score
	ren percentile  hpi2_perc
	
	keep geoid hpi_v2_population hpi2_score hpi2_perc
	
	save "${data}hpi_v2.dta", replace

	
import delimited "${data}hpiscore_v3.csv", varnames(1) encoding(UTF-8) clear 

	tostring geoid, replace
	ren population hpi_v3_population
	ren value  hpi3_score
	ren percentile  hpi3_perc
	
	keep geoid hpi_v3_population hpi3_score hpi3_perc
	
	save "${data}hpi_v3.dta", replace
	
import excel "/Users/adak/Library/CloudStorage/Box-Box/___PROJECTS/__UCSF_TBIIC LAB/0 PROJECTS/EQUIP-CA/500 DATA MAPPING/hpi_3_complete_file.xlsx", sheet("HPI 3") firstrow case(lower) clear
	
	ren geo_id geoid
	tostring geoid, replace
	
		foreach var of varlist county pop pctgqtract urbantype leb leb_pctile hpi hpi_pctile hpi_quartile hpi_least_healthy_25pct economic economic_pctile education education_pctile insurance insurance_pctile clean_enviro clean_enviro_pctile housing housing_pctile neighborhood neighborhood_pctile social social_pctile transportation transportation_pctile abovepoverty abovepoverty_pctile automobile automobile_pctile bachelorsed bachelorsed_pctile censusresponse censusresponse_pctile commute commute_pctile dieselpm dieselpm_pctile employed employed_pctile h20contam h20contam_pctile homeownership homeownership_pctile houserepair houserepair_pctile inhighschool inhighschool_pctile inpreschool inpreschool_pctile insured insured_pctile ownsevere ownsevere_pctile ozone ozone_pctile parkaccess parkaccess_pctile percapitaincome percapitaincome_pctile pm25 pm25_pctile rentsevere rentsevere_pctile retail retail_pctile treecanopy treecanopy_pctile uncrowded uncrowded_pctile voting voting_pctile latino_pct white_pct black_pct asian_pct multiple_pct nativeam_pct pacificisl_pct other_pct version notes {
			ren `var' `var'_v3
		}


	save "${data}hpi_v3_complete.dta", replace

*****************************************************
** MERGING DATASETS: master tracts, master covid outcomes
*****************************************************
	
	use "${data}acs_age_sex.dta",clear
		merge 1:1 geoid using "${data}acs_race.dta"
		drop _merge
		
		save "${data}master_acs.dta", replace
		
	use "${data}hpi_v2.dta", clear
		merge 1:1 geoid using "${data}hpi_v3.dta"
		drop _merge
	
		merge 1:1 geoid using "${data}hpi_diversity_index.dta"
		drop _merge
		
		merge 1:1 geoid using "${data}index_comparisons/ABSM_8K_CT.dta"
		drop _merge
		
		merge 1:1 geoid using "${data}hpi_v3_complete.dta"
		drop _merge
		
		/*
		preserve
		
			merge 1:1 geoid using "${data}acs_age_sex.dta"
			drop if _merge == 2
			ren _merge _merge_acs
			
			merge 1:1 geoid using "${data}aggregated_acs_age_sex.dta"
		
			gen tag = 1 if _merge_acs == 3
			replace tag = 2 if _merge == 3 & tag == .
			
			keep geoid tag
			
			save "${data}geoid_tag.dta", replace
			
			
			use "${data}acs_age_sex.dta", clear
				merge 1:1 geoid using "${data}geoid_tag.dta"
				
				* keep if in original acs file
				keep if tag == 1 
				ren _merge _merge_acs
				
				save "${data}temp1.dta", replace
				
			use "${data}aggregated_acs_age_sex.dta", clear
				merge 1:1 geoid using "${data}geoid_tag.dta"
				
				* keep if in original acs file
				keep if tag == 2
				ren _merge _merge_aggregated_acs
				
				save "${data}temp2.dta", replace
				
			use "${data}temp1.dta", clear
				append using "{data}temp2.dta", generate(append2)
				
				save "${data}acs_age_data_cleaned.dta", replace
		
		
		restore
		*/
		
			merge 1:1 geoid using "${data}acs_age_data_cleaned.dta"
			drop _merge
			drop s0101_c01_001e_perc
			
			la var s0101_c01_001e "Total population"
			la var s0101_c01_002e "Total population!!AGE!!Under 5 years"
			la var s0101_c01_003e "Total population!!AGE!!5 to 9 years"
			la var s0101_c01_004e "Total population!!AGE!!10 to 14 years"
			la var s0101_c01_005e "Total population!!AGE!!15 to 19 years"
			la var s0101_c01_006e "Total population!!AGE!!20 to 24 years"
			la var s0101_c01_007e "Total population!!AGE!!25 to 29 years"
			la var s0101_c01_008e "Total population!!AGE!!30 to 34 years"
			la var s0101_c01_009e "Total population!!AGE!!35 to 39 years"
			la var s0101_c01_010e "Total population!!AGE!!40 to 44 years"
			la var s0101_c01_011e "Total population!!AGE!!45 to 49 years"
			la var s0101_c01_012e "Total population!!AGE!!50 to 54 years"
			la var s0101_c01_013e "Total population!!AGE!!55 to 59 years"
			la var s0101_c01_014e "Total population!!AGE!!60 to 64 years"
			la var s0101_c01_015e "Total population!!AGE!!65 to 69 years"
			la var s0101_c01_016e "Total population!!AGE!!70 to 74 years"
			la var s0101_c01_017e "Total population!!AGE!!75 to 79 years"
			la var s0101_c01_018e "Total population!!AGE!!80 to 84 years"
			la var s0101_c01_019e "Total population!!AGE!!85 years and over"
			
			
			
			gen hpi2_perc_trunc = round(hpi2_perc, 0.01)
			
			gen hpi_quartile_v2 = 1 if hpi2_perc_trunc <= 0.25
				replace hpi_quartile_v2 = 2 if hpi2_perc_trunc > 0.25 & hpi2_perc_trunc <= 0.50
				replace hpi_quartile_v2 = 3 if hpi2_perc_trunc > 0.50 & hpi2_perc_trunc <= 0.75
				replace hpi_quartile_v2 = 4 if hpi2_perc_trunc > 0.75

				order hpi2_perc_trunc hpi_quartile_v2, after(hpi2_perc)
		
		save "${data}master_tracts.dta", replace
		

		preserve
		
			merge 1:m geoid using "${data}index_comparisons/covid_outcomes.dta"
			drop _merge
			
			save "${data}master_covid_outcomes.dta", replace
		
		restore
	
				

		
		

*****************************************************
** analysis
*****************************************************
	
use "${data}master_tracts.dta", clear	
	
	gen hpi2_q1 = (hpi_quartile_v2 == 1)
	

	egen pop_0_19 = rowtotal(s0101_c01_002e s0101_c01_003e s0101_c01_004e s0101_c01_005e)
	egen pop_20_49 = rowtotal(s0101_c01_006e s0101_c01_007e s0101_c01_008e s0101_c01_009e s0101_c01_010e s0101_c01_011e)
	egen pop_50_64 = rowtotal(s0101_c01_012e s0101_c01_013e s0101_c01_014e)
	egen pop_65plus = rowtotal(s0101_c01_015e s0101_c01_016e s0101_c01_017e s0101_c01_018e s0101_c01_019e)
	
	gen perc_pop_0_19 = 100*round(pop_0_19 / s0101_c01_001e, 0.01)
	gen perc_pop_20_49 = 100*round(pop_20_49 / s0101_c01_001e, 0.01) 
	gen perc_pop_50_64 = 100*round(pop_50_64 / s0101_c01_001e, 0.01) 
	gen perc_pop_65plus = 100*round(pop_65plus / s0101_c01_001e, 0.01) 
	
	
	*pop_0_19 pop_20_49 pop_50_64 pop_65plus perc_pop_0_19 perc_pop_20_49 perc_pop_50_64 perc_pop_65plus
	
		ttest perc_pop_0_19, by(hpi2_q1)
		ttest perc_pop_20_49, by(hpi2_q1)
		ttest perc_pop_50_64, by(hpi2_q1)
		ttest perc_pop_65plus, by(hpi2_q1)
	
		
		
		
	tabstat s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc ///
	, stat(n mean sd semean) column(statistics) 
	
	tabstat s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc ///
	if hpi_quartile_v2 == 1, stat(n mean sd semean) column(statistics) 
	
	
	tabstat s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc ///
	if hpi_quartile_v2 == 2, stat(n mean sd semean) column(statistics) 
	
	
	tabstat s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc ///
	if hpi_quartile_v2 == 3, stat(n mean sd semean) column(statistics) 
	
	tabstat s0101_c01_002e_perc s0101_c01_003e_perc s0101_c01_004e_perc s0101_c01_005e_perc s0101_c01_006e_perc s0101_c01_007e_perc s0101_c01_008e_perc s0101_c01_009e_perc s0101_c01_010e_perc s0101_c01_011e_perc s0101_c01_012e_perc s0101_c01_013e_perc s0101_c01_014e_perc s0101_c01_015e_perc s0101_c01_016e_perc s0101_c01_017e_perc s0101_c01_018e_perc s0101_c01_019e_perc ///
	if hpi_quartile_v2 == 4, stat(n mean sd semean) column(statistics) 
	
	
	
	
	tabstat pop_0_19 pop_20_49 pop_50_64 pop_65plus perc_pop_0_19 perc_pop_20_49 perc_pop_50_64 perc_pop_65plus, stat(n mean sd semean) column(statistics)
	
	tabstat pop_0_19 pop_20_49 pop_50_64 pop_65plus perc_pop_0_19 perc_pop_20_49 perc_pop_50_64 perc_pop_65plus if hpi_quartile_v2 == 1, stat(n mean sd semean) column(statistics) 
	
	tabstat pop_0_19 pop_20_49 pop_50_64 pop_65plus perc_pop_0_19 perc_pop_20_49 perc_pop_50_64 perc_pop_65plus if hpi_quartile_v2 == 2, stat(n mean sd semean) column(statistics) 
	
	tabstat pop_0_19 pop_20_49 pop_50_64 pop_65plus perc_pop_0_19 perc_pop_20_49 perc_pop_50_64 perc_pop_65plus if hpi_quartile_v2 == 3, stat(n mean sd semean) column(statistics) 
	
	tabstat pop_0_19 pop_20_49 pop_50_64 pop_65plus perc_pop_0_19 perc_pop_20_49 perc_pop_50_64 perc_pop_65plus if hpi_quartile_v2 == 4, stat(n mean sd semean) column(statistics) 
		
		
		
	*0-24
	graph tw (lpoly s0101_c01_002e_perc hpi2_perc) ///
		(lpoly s0101_c01_003e_perc hpi2_perc) ///
		(lpoly s0101_c01_004e_perc hpi2_perc) ///
		(lpoly s0101_c01_005e_perc hpi2_perc) ///
		(lpoly s0101_c01_006e_perc hpi2_perc) 
		
	*25-64
	graph tw (lpoly s0101_c01_007e_perc hpi2_perc) ///
		(lpoly s0101_c01_008e_perc hpi2_perc) ///
		(lpoly s0101_c01_009e_perc hpi2_perc) ///
		(lpoly s0101_c01_010e_perc hpi2_perc) ///
		(lpoly s0101_c01_011e_perc hpi2_perc) ///
		(lpoly s0101_c01_012e_perc hpi2_perc) ///
		(lpoly s0101_c01_013e_perc hpi2_perc) ///
		(lpoly s0101_c01_014e_perc hpi2_perc)
	
	* 55-85+
	graph tw (lpoly s0101_c01_015e_perc hpi2_perc) ///
		(lpoly s0101_c01_016e_perc hpi2_perc) ///
		(lpoly s0101_c01_017e_perc hpi2_perc) ///
		(lpoly s0101_c01_018e_perc hpi2_perc) ///
		(lpoly s0101_c01_019e_perc hpi2_perc) 


	
	graph tw (lpoly s0101_c01_002e_perc hpi2_perc) ///
		(lpoly s0101_c01_003e_perc hpi2_perc) ///
		(lpoly s0101_c01_004e_perc hpi2_perc) ///
		(lpoly s0101_c01_005e_perc hpi2_perc) ///
		(lpoly s0101_c01_006e_perc hpi2_perc) ///
		(lpoly s0101_c01_007e_perc hpi2_perc) ///
		(lpoly s0101_c01_008e_perc hpi2_perc) ///
		(lpoly s0101_c01_009e_perc hpi2_perc) ///
		(lpoly s0101_c01_010e_perc hpi2_perc) ///
		(lpoly s0101_c01_011e_perc hpi2_perc) ///
		(lpoly s0101_c01_012e_perc hpi2_perc) ///
		(lpoly s0101_c01_013e_perc hpi2_perc) ///
		(lpoly s0101_c01_014e_perc hpi2_perc) ///
		(lpoly s0101_c01_015e_perc hpi2_perc) ///
		(lpoly s0101_c01_016e_perc hpi2_perc) ///
		(lpoly s0101_c01_017e_perc hpi2_perc) ///
		(lpoly s0101_c01_018e_perc hpi2_perc) ///
		(lpoly s0101_c01_019e_perc hpi2_perc) 
	
	
	
	
	
	
