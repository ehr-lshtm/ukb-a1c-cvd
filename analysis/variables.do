/*==========================================================================================================================================

CODE TO DEVELOP VARIABLES USED IN Rentsch CT, et al. Sex-specific risks for cardiovascular disease across the glycaemic spectrum: a population-based cohort study using the UK Biobank. 
DATE: 28/06/2023

=========================================================================================================================================================================================*/


*read in main UKB dataset
use "<main dataset>.dta", clear

**remove withdrawals ******
merge 1:1 n_eid using "<withdrawal list>.dta"
keep if _m == 1
drop _m


*****Male sex
rename n_31_0_0 male
lab var male "Male sex"
lab def sexlabel  1"Male" 0"Female"
lab val male sexlabel
tab male ,m


*****Age
gen age_bl=n_21003_0_0
lab var age_bl "Age at baseline assessment"


****Assessment centre
gen assess_country_bl=1
replace assess_country_bl=2 if n_54_0_0==11004 | n_54_0_0==11005
replace assess_country_bl=3 if n_54_0_0==11003 | n_54_0_0==11022| n_54_0_0==11023
replace assess_country_bl=. if n_54_0_0==.
lab var assess_country_bl "Country of baseline assessment"
lab def assess_country_bllab 1"England" 2"Scotland" 3"Wales"
lab val assess_country_bl assess_country_bllab
tab assess_country_bl


****Date of baseline assessment
gen date_assess_bl=ts_53_0_0
lab var date_assess_bl "Date of baseline assessment"


*****Ethnicity 
**All ethnic groups
gen ethnic_all=n_21000_0_0
label define ethnic_alllab 4001 "Caribbean" 3001 "Indian" 1 "White" 2001 "White and Black Caribbean" 1001 "British" 3002 "Pakistani" 2 "Mixed" 4002 "African" 1002 "Irish" 2002 "White and Black African" 3003 "Bangladeshi" 3 "Asian or Asian British" 4003 "Any other Black background" 1003 "Any other white background" 2003 "White and Asian" 3004 "Any other Asian background" 4 "Black or Black British" 2004 "Any other mixed background" 5 "Chinese" 6 "Other ethnic group" -1 "Do not know" -3 "Prefer not to answer"
lab val ethnic_all ethnic_alllab
**Main ethnic groups
gen  ethnic=n_21000_0_0
recode ethnic 1001=1 1002=1 1003=1 3=2 3001=2 3002=2 3003=2 4=3 4001=3 4002=3 4003=3 2=4 2001=4 2002=4 2003=4 2004=4 3004=4 5=4 6=4 -1=. -3=.
lab var ethnic "Ethnicity: main groups"
lab def ethniclab 1"White European" 2"South Asian" 3"African Caribbean" 4"Mixed or other"
lab val ethnic ethniclab


*****Smoking 
*1239: Current Tobacco Smoking
**Current - recoded so "occasionally" counts as current
gen currsmok_bl=n_1239_0_0
recode currsmok_bl -3=. 2=1
tab currsmok_bl
lab var currsmok_bl "Current smoker at baseline"
lab def currsmok_bllab 0"No" 1"Yes"
lab val currsmok_bl currsmok_bllab

**Former - derived from n_2867_0_0 "Age started smoking in former smokers" variable
***2867: Age started smoking in former smokers
gen prevsmok_bl=0
replace prevsmok_bl=1 if n_2867_0_0>0 & n_2867_0_0<.
replace prevsmok_bl=0 if currsmok_bl==1| currsmok_bl==.
replace prevsmok_bl=. if n_2867_0_0==-1 |n_2867_0_0==-3
replace prevsmok_bl=0 if currsmok_bl==1
tab prevsmok_bl, mis
lab var prevsmok_bl "Former smoker at baseline"
lab def prevsmok_bllab 0"No" 1"Yes"
lab val prevsmok_bl prevsmok_bllab
**Ever smoked
gen eversmok_bl=.
replace eversmok_bl=0 if prevsmok_bl==0 & currsmok_bl==0
replace eversmok_bl=1 if prevsmok_bl==1 | currsmok_bl==1
tab eversmok_bl, mis
lab var eversmok_bl "Ever smoked at baseline"
lab def eversmok_bllab 0"No" 1"Yes"
lab val eversmok_bl eversmok_bllab

**SMOKING CATEGORY
gen smoking_cat=eversmok_bl
replace smoking_cat=2 if currsmok_bl==1
label define smoking_cat 0"Never smoker" 1"Previous smoker" 2"Current smoker"
label values smoking_cat smoking_cat
tab smoking_cat




*Current insulin Rx
gen dm_drug_ins_men=0
replace dm_drug_ins_men=1 if n_6177_0_0==3|n_6177_0_1==3|n_6177_0_2==3
replace dm_drug_ins_men=. if n_6177_0_0==.|n_6177_0_0==-3|n_6177_0_0==-1
tab dm_drug_ins_men, mis
gen dm_drug_ins_women=0
replace dm_drug_ins_women=1 if n_6153_0_0==3|n_6153_0_1==3|n_6153_0_2==3
replace dm_drug_ins_women=. if n_6153_0_0==.|n_6153_0_0==-3|n_6153_0_0==-1
tab dm_drug_ins_women, mis

**insulin self report (sr)
gen dm_drug_ins_bl_sr=.
replace dm_drug_ins_bl_sr=0 if dm_drug_ins_men==0|dm_drug_ins_women==0
replace dm_drug_ins_bl_sr=1 if dm_drug_ins_men==1|dm_drug_ins_women==1
tab dm_drug_ins_bl_sr, mis
lab var dm_drug_ins_bl_sr "On insulin"
labe def dm_drug_ins_bl_srlab 0"No" 1"Yes"
lab val dm_drug_ins_bl_sr dm_drug_ins_bl_srlab
drop dm_drug_ins_men dm_drug_ins_women





************************** Family history of CVD
*Mother
gen famhx_cvd_mother = .
foreach X of varlist n_20110_0_0-n_20110_0_10 {
replace famhx_cvd_mother=1 if `X'==1 | `X'==2
}
recode famhx_cvd_mother .=0
tab famhx_cvd_mother,m

*Father
gen famhx_cvd_father = .
foreach X of varlist n_20107_0_0-n_20107_0_9 {
replace famhx_cvd_father=1 if `X'==1 | `X'==2
}
recode famhx_cvd_father .=0
tab famhx_cvd_father,m

*Siblings
gen famhx_cvd_siblings = .
foreach X of varlist n_20111_0_0-n_20111_0_11 {
replace famhx_cvd_siblings=1 if `X'==1 | `X'==2
}
recode famhx_cvd_siblings .=0
tab famhx_cvd_siblings,m

*Combine
gen famhx_cvd = 1 if famhx_cvd_mother == 1 | famhx_cvd_father == 1 | famhx_cvd_siblings == 1
recode famhx_cvd .=0
tab famhx_cvd,m
lab var famhx_cvd "Family history of CVD, self-report"








*Hypertension
*From touchscreen
gen hypten_bl_sr=2
replace hypten_bl_sr=1 if n_6150_0_0==4|n_6150_0_1==4|n_6150_0_2==4|n_6150_0_3==4
replace hypten_bl_sr=. if n_6150_0_0==.|n_6150_0_0==-3
replace hypten_bl_sr=0 if hypten_bl_sr!=1 & hypten_bl_sr!=.
tab hypten_bl_sr, mis
lab var hypten_bl_sr "Self-reported hypertension at baseline"
lab def hypten_bl_srlab 0"No" 1"Yes"
lab val hypten_bl_sr hypten_bl_srlab
*From nurse interview - code=1065 and 1072 for essential hypertension
gen hypten_ni_bl=.
foreach X of varlist n_20002_0_0-n_20002_0_28 {
replace hypten_ni_bl=1 if `X'==1065|`X'==1072
}

tab hypten_ni_bl
lab var hypten_ni_bl "Hypertension, baseline nurse interview"
lab def hypten_ni_bllab 1"Hypertension present"
lab val hypten_ni_bl hypten_ni_bllab

tab hypten_ni_bl hypten_bl_sr, mis

gen hyp_bl=0
replace hyp_bl=1 if hypten_bl_sr==1 | hypten_ni_bl==1
lab var hyp_bl "Hypertension, baseline nurse interview or self-report"
tab hyp_bl



*Waist 
///Cleaning - values<40cm  dropped (n=4)
gen ant_waist_bl=n_48_0_0
replace ant_waist_bl=. if ant_waist_bl<40
lab var ant_waist_bl "Baseline waist"
*Hip circumference
///Cleaning - values<50cm removed (n=5)
gen ant_hips_bl=n_49_0_0
replace ant_hips_bl=. if ant_hips_bl<50
lab var ant_hips_bl "Baseline hip circumference"
*WHR
///Cleaning - values> 1.3 removed (n=14)
gen ant_whr_bl=ant_waist_bl/ant_hips_bl
replace ant_whr_bl=. if ant_whr_bl>1.3 & ant_whr_bl<.
lab var ant_whr_bl"Baseline waist/hip ratio"
**Waist-hip ratio (The cut‐off for obesity for WHR was defined as ≥0.95 for men and ≥0.80 for women. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6662360/)
gen whr_cat=0
replace whr_cat=1 if ant_whr_bl>=0.95 & male==1 /*men*/
replace whr_cat=1 if ant_whr_bl>=0.80 & male==0 /*women*/
label define whr_cat 0"Normal WHR" 1"Adverse WHR"
label values whr_cat whr_cat
label var whr_cat "Baseline Waist-Hip Ratio"
replace whr_cat=. if ant_whr_bl==.
tab whr_cat,m
**waist-height ratio
gen ant_whtr_bl=ant_waist_bl/ant_height_bl
label var ant_whtr_bl "Baseline Waist-Height Ratio"


*Height
///Cleaning - values<135 (n=32) and >200 (n=72) removed
gen ant_height_bl=n_50_0_0
replace ant_height_bl=. if ant_height_bl<135|ant_height_bl>200
lab var ant_height_bl"Baseline height"
*Weight
///Cleaning nil values changed
gen ant_weight_bl=n_21002_0_0
lab var ant_weight_bl"Baseline weight"
*BMI
///Cleaning values changed in accordance with height, also took out values>60 (n=56)
gen ant_bmi_bl=n_21001_0_0
replace ant_bmi_bl=. if ant_height_bl<135|ant_height_bl>200
replace ant_bmi_bl=. if ant_bmi_bl>60 & ant_bmi_bl<.
lab var ant_bmi_bl"Baseline BMI"
**Ethnicity specific BMI
**White/Black/Other/Mixed
gen bmi_non_sa=0 if ant_bmi_bl<18.5 //underweight
replace bmi_non_sa=1 if ant_bmi_bl>=18.5 & ant_bmi_bl <25 //normal
replace bmi_non_sa=2 if ant_bmi_bl>=25 & ant_bmi_bl <30 //overweight
replace bmi_non_sa=3 if ant_bmi_bl>=30 //Obese
label define bmi_non_sa 0"Underweight" 1"Normal" 2"Overweight" 3"Obese"
label values bmi_non_sa bmi_non_sa
tab bmi_non_sa
bysort bmi_non_sa: sum ant_bmi_bl
**South Asian
gen bmi_sa=0 if ant_bmi_bl<18.5 //underweight
replace bmi_sa=1 if ant_bmi_bl>=18.5 & ant_bmi_bl <23 //normal
replace bmi_sa=2 if ant_bmi_bl>=23 & ant_bmi_bl <27.5 //overweight
replace bmi_sa=3 if ant_bmi_bl>=27.5 //Obese
label define bmi_sa 0"Underweight" 1"Normal" 2"Overweight" 3"Obese"
label values bmi_sa bmi_sa
tab bmi_sa
bysort bmi_sa: sum ant_bmi_bl
**combine BMI
gen bmi_cat=bmi_non_sa if ethnic!=2
replace bmi_cat=bmi_sa if ethnic==2
replace bmi_cat=. if ant_bmi_bl==.
drop bmi_non_sa bmi_sa
label define bmi_cat 0"Underweight" 1"Normal" 2"Overweight" 3"Obese"
label values bmi_cat bmi_cat
tab bmi_cat,m




*Physical activity 
*Number of days/week of moderate physical activity 10+ minutes
gen ls_pa_mod_days_bl=n_884_0_0 
recode ls_pa_mod_days_bl -7=. -1=. -3=.
lab var ls_pa_mod_days_bl "Baseline number of days/week moderate physical activity >10 mins"
tab ls_pa_mod_days_bl, m

*Number of days/week of vigorous physical activity 10+ minutes
gen ls_pa_vig_days_bl=n_904_0_0 
recode ls_pa_vig_days_bl -7=. -1=. -3=.
lab var ls_pa_vig_days_bl "Baseline number of days/week vigorous physical activity >10 mins"
tab ls_pa_vig_days_bl, m

***create condensed physical activity variables 
foreach a in ls_pa_mod_days_bl ls_pa_vig_days_bl {
gen `a'_original = `a'
recode `a' 2=1 4=2 3=2 7=3 6=3 5=3
label define `a' 0 "0" 1 "1-2" 2 "3-4" 3 "5-7"
label values `a' `a'
} 

**combine mod and vig 
gen ls_pa_mod_or_vig_days_bl = 0 if ls_pa_mod_days_bl_original == 0 | ls_pa_vig_days_bl_original == 0
replace ls_pa_mod_or_vig_days_bl = 1 if (ls_pa_mod_days_bl_original >= 1 & ls_pa_mod_days_bl_original <= 2) | (ls_pa_vig_days_bl_original >= 1 & ls_pa_vig_days_bl_original <= 2)
replace ls_pa_mod_or_vig_days_bl = 2 if (ls_pa_mod_days_bl_original >= 3 & ls_pa_mod_days_bl_original <= 7) | (ls_pa_vig_days_bl_original >= 3 & ls_pa_vig_days_bl_original <= 7)
lab var ls_pa_mod_or_vig_days_bl "Baseline number of days/week moderate or vigorous physical activity >10 mins"
label define ls_pa_mod_or_vig_days_bl 0 "0" 1 "1-2" 2 "3-7"
label values ls_pa_mod_or_vig_days_bl ls_pa_mod_or_vig_days_bl




*Days/week with intake of processed meats
replace n_1349_0_0 = . if n_1349_0_0 < 0
ren n_1349_0_0 bl_promeat_orcat
*combine 7 or more times per week with 5-6 times per week)
recode bl_promeat_orcat 5=4
*very few missing (0.44%), add to largest group (less than once per week)
recode bl_promeat_orcat .=1
label define bl_promeat_orcat 0 "Never" 1 "<1 per week" 2 "Once per week" 3 "2-4 times per week" 4 "5+ times a week"
label values bl_promeat_orcat bl_promeat_orcat
lab var bl_promeat_orcat "Baseline number of times/week intake processed meats"





*Number of fruits/veg consumed per day
replace n_1289_0_0 = 0 if n_1289_0_0==-10 
replace n_1289_0_0 = . if n_1289_0_0 < 0
ren n_1289_0_0 bl_cookedveg_cont
replace n_1299_0_0 = 0 if n_1299_0_0==-10 
replace n_1299_0_0 = . if n_1299_0_0 < 0
ren n_1299_0_0 bl_rawveg_cont
replace n_1309_0_0 = 0 if n_1309_0_0==-10 
replace n_1309_0_0 = . if n_1309_0_0 < 0
ren n_1309_0_0 bl_frfruit_cont
replace n_1319_0_0 = 0 if n_1319_0_0==-10 
replace n_1319_0_0 = . if n_1319_0_0 < 0
ren n_1319_0_0 bl_drfruit_cont
*add together to get total fruit and veg intake per day
egen tot_fruitveg = rowtotal(bl_cookedveg_cont bl_rawveg_cont bl_frfruit_cont bl_drfruit_cont)
gen tot_fruitveg_cat = 0 if tot_fruitveg == 0 
replace tot_fruitveg_cat = 1 if tot_fruitveg == 1 | tot_fruitveg == 2 
replace tot_fruitveg_cat = 2 if tot_fruitveg == 3 | tot_fruitveg == 4
replace tot_fruitveg_cat = 3 if tot_fruitveg >= 5
label define tot_fruitveg_cat 0 "None" 1 "1-2 per day" 2 "3-4 per day" 3 "5+ per day" 
label values tot_fruitveg_cat tot_fruitveg_cat
lab var tot_fruitveg_cat "Baseline total number of fruit/veg intake per day"
tab tot_fruitveg_cat , m 


*Alcohol consumption frequency 
gen ls_alc_freq_bl=n_1558_0_0
recode ls_alc_freq_bl -3=.    /*prefers not to answer set to missing*/
lab var ls_alc_freq_bl"Baseline alcohol intake frequency"
lab def ls_alc_freq_bllab 1"Daily or almost daily" 2"Three or four times a week" 3"Once or twice a week" 4"One to three times a month" 5"Special occasions only" 6"Never"
lab val ls_alc_freq_bl ls_alc_freq_bllab
gen ls_alc_freq3cats_bl=ls_alc_freq_bl
recode ls_alc_freq3cats_bl 5/6=0 3/4=1 1/2=2
lab var ls_alc_freq3cats_bl"Baseline alcohol intake frequency - 3 categories"
lab def ls_alc_freq3cats_bllab 0"Rarely or never" 1"1-8 times per month" 2"16 times per month- every day" 
lab val ls_alc_freq3cats_bl ls_alc_freq3cats_bllab
gen ls_bin_alc_freq_bl=ls_alc_freq_bl
replace ls_bin_alc_freq_bl=0 if ls_alc_freq_bl!=1
replace ls_bin_alc_freq_bl=. if ls_alc_freq_bl==.
tab ls_bin_alc_freq_bl
lab var ls_bin_alc_freq_bl"Alcohol intake daily/ almost vs. not"
lab def ls_bin_alc_freq_bllab 0"Alcohol intake less than daily" 1"Alcohol intake daily/ almost"
lab val ls_bin_alc_freq_bl ls_bin_alc_freq_bllab




****SEP
*Townsend dep index at baseline 
gen sep_townsend_bl=n_189_0_0
lab var sep_townsend_bl"Baseline Townsend deprivation index"
egen sep_townsendquint_bl=cut(sep_townsend_bl), group(5)
lab var sep_townsendquint_bl "Baseline quintiles Townsend deprivation index"
lab def sep_townsendquint_bllabel 0"Least deprived" 1"2nd least deprived" 2"Median deprivation level" 3"2nd most deprived" 4"Most deprived"
lab val sep_townsendquint_bl sep_townsendquint_bllabel

egen sep_townsendbin_bl=cut(sep_townsend_bl), group(2)
lab var sep_townsendbin_bl" Baseline Townsend deprivation score binarised"
lab def sep_townsendbin_bllab 0"Least deprived" 1"Most deprived"
lab val sep_townsendbin_bl sep_townsendbin_bllab


**********Derivation of classes:
///Insulins; dm_drug_ins_ni_bl
gen dm_drug_ins_ni_bl=.
forvalues i=0/47 {
replace dm_drug_ins_ni_bl=1 if n_20003_0_`i'==1140883066
}

*
lab var dm_drug_ins_ni_bl "Taking insulin, baseline nurse interview"
recode dm_drug_ins_ni_bl .=0
lab def dm_drug_ins_ni_bllab 0"No insulin" 1"On insulin"
lab val dm_drug_ins_ni_bl dm_drug_ins_ni_bllab
tab dm_drug_ins_ni_bl

gen insulin_bl=0
replace insulin_bl=1 if dm_drug_ins_bl_sr==1 | dm_drug_ins_ni_bl==1
tab insulin_bl



///Metformin; dm_drug_metf_ni_bl
gen dm_drug_metf_ni_bl=.
forvalues i=0/47 {
replace dm_drug_metf_ni_bl=1 if n_20003_0_`i'==1140884600 
replace dm_drug_metf_ni_bl=1 if n_20003_0_`i'==1140874686
replace dm_drug_metf_ni_bl=1 if n_20003_0_`i'==1141189090
}

*
lab var dm_drug_metf_ni_bl "Taking metformin, baseline nurse interview"
recode dm_drug_metf_ni_bl .=0
lab def dm_drug_metf_ni_bllab 0"No metformin" 1"On metformin"
lab val dm_drug_metf_ni_bl dm_drug_metf_ni_bllab
tab dm_drug_metf_ni_bl



///Sulfonylureas; dm_drug_su_ni_bl 
gen dm_drug_su_ni_bl=.
forvalues i=0/47 {
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874718 
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874744
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874746
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1141152590 
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1141156984
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874646
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1141157284 
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874652
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874674
replace dm_drug_su_ni_bl=1 if n_20003_0_`i'==1140874728 
}

*
lab var dm_drug_su_ni_bl "Taking sulfonylurea, baseline nurse interview"
recode dm_drug_su_ni_bl  .=0
lab def dm_drug_su_ni_bllab 0"No sulfonylurea" 1"On sulfonylurea"
lab val dm_drug_su_ni_bl dm_drug_su_ni_bllab
tab dm_drug_su_ni_bl


///Other OAD; dm_drug_other_oad_ni_bl
gen dm_drug_other_oad_ni_bl=.
forvalues i=0/47 {
replace dm_drug_other_oad_ni_bl=1 if n_20003_0_`i'==1140868902 
replace dm_drug_other_oad_ni_bl=1 if n_20003_0_`i'==1140868908
replace dm_drug_other_oad_ni_bl=1 if n_20003_0_`i'==1140857508
}

*
lab var dm_drug_other_oad_ni_bl "Taking other oral anti-diabetic (acarbose, guar gum), baseline nurse interview"
recode dm_drug_other_oad_ni_bl .=0
lab def dm_drug_other_oad_ni_bllab 0"Not on other oral anti-diabetic" 1"On other oral anti-diabetic"
lab val dm_drug_other_oad_ni_bl dm_drug_other_oad_ni_bllab
tab dm_drug_other_oad_ni_bl



///Meglitinides; dm_drug_meglit_ni_bl 
gen dm_drug_meglit_ni_bl=.
forvalues i=0/47 {
replace dm_drug_meglit_ni_bl=1 if n_20003_0_`i'==1141173882 
replace dm_drug_meglit_ni_bl=1 if n_20003_0_`i'==1141173786
replace dm_drug_meglit_ni_bl=1 if n_20003_0_`i'==1141168660
}

*
lab var dm_drug_meglit_ni_bl "Taking meglitinide, baseline nurse interview"
recode dm_drug_meglit_ni_bl .=0
lab def dm_drug_meglit_ni_bllab 0"No meglitinide" 1"On meglitinide"
lab val dm_drug_meglit_ni_bl dm_drug_meglit_ni_bllab
tab dm_drug_meglit_ni_bl


///Glitazones; dm_drug_glitaz_ni_bl 
gen dm_drug_glitaz_ni_bl=.
forvalues i=0/47 {
replace dm_drug_glitaz_ni_bl=1 if n_20003_0_`i'==1141171646 
replace dm_drug_glitaz_ni_bl=1 if n_20003_0_`i'==1141171652
replace dm_drug_glitaz_ni_bl=1 if n_20003_0_`i'==1141153254
replace dm_drug_glitaz_ni_bl=1 if n_20003_0_`i'==1141177600
replace dm_drug_glitaz_ni_bl=1 if n_20003_0_`i'==1141177606
}

*
lab var dm_drug_glitaz_ni_bl "Taking glitazone, baseline nurse interview"
recode dm_drug_glitaz_ni_bl .=0
lab def dm_drug_glitaz_ni_bllab 0"No glitazone" 1"On glitazone"
lab val dm_drug_glitaz_ni_bl dm_drug_glitaz_ni_bllab
tab dm_drug_glitaz_ni_bl


///Non-metformin OADs (including   above 4 drug classes): dm_drug_nonmetf_oad_ni_bl
gen dm_drug_nonmetf_oad_ni_bl=.
replace dm_drug_nonmetf_oad_ni_bl=1 if dm_drug_su_ni_bl==1 | dm_drug_other_oad_ni_bl==1==1 | dm_drug_meglit_ni_bl==1 | dm_drug_glitaz_ni_bl==1
lab var dm_drug_nonmetf_oad_ni_bl "Taking non-metformin oral anti-diabetic drug, baseline nurse interview"
recode dm_drug_nonmetf_oad_ni_bl .=0
lab def dm_drug_nonmetf_oad_ni_bllab 0"No non-metformin oral anti-diabetic drug" 1"On non-metformin oral anti-diabetic drug"
lab val dm_drug_nonmetf_oad_ni_bl dm_drug_nonmetf_oad_ni_bllab
tab dm_drug_nonmetf_oad_ni_bl



///Any medication
gen dm_anydmrx_ni_sr_bl=0
replace dm_anydmrx_ni_sr_bl=1 if dm_drug_ins_ni_bl==1|dm_drug_metf_ni_bl==1|dm_drug_nonmetf_oad_ni_bl==1|dm_drug_ins_bl_sr==1
lab var dm_anydmrx_ni_sr_bl "Any reported diabetes medication"
tab dm_anydmrx_ni_sr_bl













*****Current ACE inhibitor receipt
gen drug_acei_ni_bl=.
forvalues i=0/47 {
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860750
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860752
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141150328
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141150560
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860758
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141181186
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860764
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141170544
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860882
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860892
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140888552
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860776
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860790
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860784
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140888556
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860878
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141164148
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141164154
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860696
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860714
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860706
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140864618
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140923712
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140923718
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140888560
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860802
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141180592
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141180598
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860728
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140881706
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860738
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860736
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860806
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141188408
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141165476
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141165470
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860904
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141153328
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860912
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1140860918
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141165470
replace drug_acei_ni_bl=1 if n_20003_0_`i'==1141180592
}

*
lab var drug_acei_ni_bl "Taking ACE inhibitor, baseline nurse interview"
recode drug_acei_ni_bl .=0
lab def drug_acei_ni_bllab 0"No ACE inhibitor" 1"On ACE inhibitor"
lab val drug_acei_ni_bl drug_acei_ni_bllab
tab drug_acei_ni_bl



*****Current angiotensin receptor blocker (ARB) receipt
gen drug_arb_ni_bl=.
forvalues i=0/47 {
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141156836
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141156846
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141171336
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141171344
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141152998
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141153006
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141172682
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141172686
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1140916356
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141179974
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141151016
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1140916362
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141151018
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141193282
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141193346
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141166006
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141172492
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141187788
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141187790
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141145660
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1140866758
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141145668
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141201038
replace drug_arb_ni_bl=1 if n_20003_0_`i'==1141201040
}

*
lab var drug_arb_ni_bl "Taking angiotensin-II receptor antagonist (ARB), baseline nurse interview"
recode drug_arb_ni_bl .=0
lab def drug_arb_ni_bllab 0"No ARB" 1"On ARB"
lab val drug_arb_ni_bl drug_arb_ni_bllab
tab drug_arb_ni_bl



*****Current calcium channel blocker (CCB) receipt 
gen drug_ccb_ni_bl=.
forvalues i=0/47 {
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140879802
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861202
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141200400
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140888646
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141199858
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141188576
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141188152
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141188920
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141200782
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140868036
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141201814
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141190160
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140928212
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141165470
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861190
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861194
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861276
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861282
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141153032
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140879810
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861176
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861088
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140860426
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861090
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140881702
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140860356
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861110
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140923572
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861120
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861106
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141145870
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140861114
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141157140
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140927940
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141190548
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140872568
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140872472
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141165476
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1141165470
replace drug_ccb_ni_bl=1 if n_20003_0_`i'==1140860426
}

*
lab var drug_ccb_ni_bl "Taking calcium channel blocker (CCB), baseline nurse interview"
recode drug_ccb_ni_bl .=0
lab def drug_ccb_ni_bllab 0"No ARB" 1"On ARB"
lab val drug_ccb_ni_bl drug_ccb_ni_bllab
tab drug_ccb_ni_bl


*****Current thiazide-related diuretic (thiazide) receipt
gen drug_thiazide_ni_bl=.
forvalues i=0/47 {
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194794
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194800
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194804
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194808
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194810
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866128
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866136
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866446
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140909706
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141180778
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141180772
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866146
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140851364
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866156
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866422
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860334
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866158
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140851368
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866078
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141180592
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141146378
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866108
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140866110
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860790
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140864618
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141180592
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860738
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860736
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141172682
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141151016
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141187788
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141187790
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141201038
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141201040
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860418
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860394
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860396
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860422
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141146126
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194810
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860348
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860352
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141146128
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141180778
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141146124
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140864950
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860328
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860308
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860404
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860402
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860390
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860312
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860316
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194804
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194800
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860322
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860340
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1141194808
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860336
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860406
replace drug_thiazide_ni_bl=1 if n_20003_0_`i'==1140860342
}

*
lab var drug_thiazide_ni_bl "Taking thiazide diuretic, baseline nurse interview"
recode drug_thiazide_ni_bl .=0
lab def drug_thiazide_ni_bllab 0"No thiazide diuretic" 1"On thiazide diuretic"
lab val drug_thiazide_ni_bl drug_thiazide_ni_bllab
tab drug_thiazide_ni_bl


*****Current alpha-blocker receipt
gen drug_alpha_block_ni_bl=.
forvalues i=0/47 {
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140879778
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1141194372
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140860690
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140879782
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1141157490
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140860658
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140879794
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140860580
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140860590
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140879798
replace drug_alpha_block_ni_bl=1 if n_20003_0_`i'==1140860610
}

*
lab var drug_alpha_block_ni_bl "Taking alpha blocker, baseline nurse interview"
recode drug_alpha_block_ni_bl .=0
lab def drug_alpha_block_ni_bllab 0"No alpha blocker" 1"On alpha blocker"
lab val drug_alpha_block_ni_bl drug_alpha_block_ni_bllab
tab drug_alpha_block_ni_bl


*****Current beta-blocker receipt 
gen drug_beta_block_ni_bl=.
forvalues i=0/47 {
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140879842
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866804
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866800
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866784
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866798
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866778
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866766
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866764
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866704
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141172742
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140851556
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866802
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866782
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860418
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860394
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860396
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866724
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860422
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866726
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860314
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866738
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140866756
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860398
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860356
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141146126
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194810
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860348
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860352
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860426
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141146128
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141180778
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141146124
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860358
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860232
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140879760
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140864950
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141171152
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860492
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860434
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140909368
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140879762
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860498
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140923336
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860324
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860328
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860250
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140879818
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860386
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860266
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860278
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860308
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860404
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860402
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860274
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860192
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860194
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860390
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860312
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860316
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194804
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194800
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141164276
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141164280
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140879830
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860212
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860220
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860222
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860292
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860322
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860294
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860338
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140879866
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860340
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194808
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860336
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860406
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860342
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860380
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860382
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860410
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1140860426
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194804
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194808
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141194810
replace drug_beta_block_ni_bl=1 if n_20003_0_`i'==1141180778
}

*
lab var drug_beta_block_ni_bl "Taking beta blocker, baseline nurse interview"
recode drug_beta_block_ni_bl .=0
lab def drug_beta_block_ni_bllab 0"No beta blocker" 1"On beta blocker"
lab val drug_beta_block_ni_bl drug_beta_block_ni_bllab
tab drug_beta_block_ni_bl


****Any anti-hypertensive medication
gen drug_any_bpdrug_ni_bl=0
replace drug_any_bpdrug_ni_bl=1 if drug_acei_ni_bl==1| drug_arb_ni_bl==1| drug_ccb_ni_bl==1| drug_thiazide_ni_bl==1| drug_alpha_block_ni_bl==1| drug_beta_block_ni_bl==1
tab drug_any_bpdrug_ni_bl
lab var drug_any_bpdrug_ni_bl "On any anti-hypertensive at baseline, nurse interview"
lab def drug_any_bpdrug_ni_bllab 0"No anti-hypertensive" 1"On 1+ anti-hypertensive"
lab val drug_any_bpdrug_ni_bl drug_any_bpdrug_ni_bllab








*****Current statin receipt
gen drug_statin_ni_bl=.
forvalues i=0/47 {
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140861958
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1141188146
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140881748
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1141200040
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140888594
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140864592
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140888648
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140861970
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140910632
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140910654
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1141146234
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1141146138
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1141192410
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1141192414
replace drug_statin_ni_bl=1 if n_20003_0_`i'==1140910652
}

*
lab var drug_statin_ni_bl "Taking statin, baseline nurse interview"
recode drug_statin_ni_bl .=0
lab def drug_statin_ni_bllab 0"No statin" 1"On statin"
lab val drug_statin_ni_bl drug_statin_ni_bllab
tab drug_statin_ni_bl







*Hba1c
ren n_30750_0_0 hba1c_bl 
lab var hba1c_bl "Baseline HbA1c (mmol/mol)"
gen hba1c_raw = hba1c_bl
replace hba1c_bl =. if hba1c_bl>200


*c-reactive protien 
ren n_30710_0_0 c_reactive_bl 
lab var c_reactive_bl "Baseline C-reactive protein (mg/L)"

*Total Cholesterol 
ren n_30690_0_0 total_chol_bl 
lab var total_chol_bl "Baseline Total Cholesterol (mmol/L)"


*creatinine 
ren n_30700_0_0 creatinine_bl 
lab var creatinine_bl "Baseline Creatinine (umol/L)"
*eGFR
* Set implausible creatinine values to missing (Note: zero changed to missing)
replace creatinine_bl = . if !inrange(creatinine_bl, 20, 3000) 
*only 10 change

* Divide by 88.4 (to convert umol/l to mg/dl)
gen SCr_adj = creatinine_bl/88.4

gen min = .
replace min = SCr_adj/0.7 if male==0
replace min = SCr_adj/0.9 if male==1
replace min = min^-0.329  if male==0
replace min = min^-0.411  if male==1
replace min = 1 if min<1

gen max=.
replace max=SCr_adj/0.7 if male==0
replace max=SCr_adj/0.9 if male==1
replace max=max^-1.209
replace max=1 if max>1

gen egfr=min*max*141
replace egfr=egfr*(0.993^age_bl)
replace egfr=egfr*1.018 if male==0
label var egfr "egfr calculated using CKD-EPI formula with no eth"

tabstat egfr, s(n min p1 p25 median p75 p99 max) c(s)

* Categorise into ckd stages
egen egfr_cat_all = cut(egfr), at(0, 15, 30, 45, 60, 5000)
recode egfr_cat_all 0 = 5 15 = 4 30 = 3 45 = 2 60 = 0, generate(ckd_egfr)

/* 
0 "stage 1, eGFR>90"
1 "stage 2, eGFR 60-89" 	
2 "stage 3a, eGFR 45-59" 
3 "stage 3b, eGFR 30-44" 
4 "stage 4, eGFR 15-29" 
5 "stage 5, eGFR <15"
*/

*only 487/502,444 with eGFR <30, combine groups

gen egfr_cat = .
recode egfr_cat . = 3 if egfr < 60
recode egfr_cat . = 2 if egfr < 90
recode egfr_cat . = 1 if egfr < .
replace egfr_cat = . if egfr >= .

label define egfr_cat 	1 ">=90" 		///
						2 "60-89"		///
						3 "<60"			

label values egfr_cat egfr_cat
label variable egfr_cat "Baseline eGFR"




compress
save "<cleaned dataset>.dta", replace






