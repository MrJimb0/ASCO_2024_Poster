//SEER v1 
cd "/Users/jamesdickerson/Google Drive/Research/Active Projects/Badcare/ASCO 2024/"
pwd
dir
clear
set more off
capture log using ASCOOncoshare.log, replace

import delimited "/Users/jamesdickerson/Google Drive/Research/Active Projects/Badcare/ASCO 2024/onco_dickerson_denovo_mets_cohort_data.csv", varnames(1) encoding("utf-8") bindquote(strict) clear

//Get rid of all the Blanks strings=
ds, has(type string)
foreach var of varlist `r(varlist)' {
replace `var' = "." if `var' == ""
}

drop age_cat
drop site
drop tumor_id
drop dxzip_comment
drop cs_mets_dx
rename distance_from_stanford_km distance
label variable distance "distance in kilometers from Stanford"

rename ruca_quint ruca
rename adi_quint adi
rename ses_quint ses 
rename dxzip ZIP

/*This adds in the zip codes & median incomes
import excel "/Users/jamesdickerson/Google Drive/Research/Active Projects/Badcare/ASCO 2024/ZipFip.xlsx", sheet("Sheet1") firstrow
save ZIPFIP.dta, replace

import excel "/Users/jamesdickerson/Google Drive/Research/Active Projects/Badcare/ASCO 2024/DFwMedianIncome.xlsx", sheet("Sheet1") firstrow
save DFwMedianIncome.dta, replace
*/

merge m:m ZIP using ZIPFIP.dta
drop if _merge == 2

rename _merge zipmerge

merge 1:1 anon_id using DFwMedianIncome.dta
drop if _merge == 2

gen IncomeBucket = . 
replace IncomeBucket = 1 if MedIncome < 50000
replace IncomeBucket = 3 if MedIncome > 75000
replace IncomeBucket = 2 if IncomeBucket == .
replace IncomeBucket = . if MedIncome == .


//Create the same binning for rural/urban as we used in SEER 
tab RUCC_2013
gen ruralUrbanBucket = . 
replace ruralUrbanBucket = 1 if RUCC_2013 == 1 
replace ruralUrbanBucket = 2 if ruralUrbanBucket != 1
tab ruralUrbanBucket

label variable ruca "lower # = more urban"
label variable adi "lower # = more rich"
label variable ses "lower # = more rich"

gen diagnosis_date = date(datedx, "YMD")
format diagnosis_date %d

rename (mets_at_dx_bone) (bone)
rename (mets_at_dx_brain) (brain)
rename (mets_at_dx_liver) (liver)
rename (mets_at_dx_lung) (lung)
rename (mets_at_dx_distantln) (distantln)
rename (mets_at_dx_other) (other)
gen visceral = .
replace visceral = 1 if brain == 1 | liver == 1 | lung == 1 
replace visceral = 0 if visceral != 1

replace surgery_yn = "1" if surgery_yn == "Y"
replace surgery_yn = "0" if surgery_yn == "N"
replace surgery_yn = "0" if surgery_yn == "."
replace surgery_yn = "0" if surgery_type == "No surgical procedure"

destring surgery_yn, replace
tab surgery_yn yeardx, column

replace surgeon = "." if surgeon == "0"
replace surgeon = "." if surgeon == "99999999"
tab surgeon if surgery_yn == 1
//45% have listed surgeon if they had surgery. no surgeon did more than 6

replace radiation_yn = "1" if radiation_yn == "Y"
replace radiation_yn = "0" if radiation_yn == "N"
replace radiation_yn = "0" if radiation_yn == "."
destring radiation_yn, replace
tab radiation_yn
//30% had XRT

encode health_ins_type, gen(insurance_encode)
//private = 5 
encode marstat, gen(marital_encode)
//married = 3 
rename racegroup race
encode race, gen(race_encode)
//NHW = 6 

gen dead = . 
replace dead = 1 if death_date != "."
replace dead = 0 if dead != 1

gen last_followup = date(lfudate, "YMD")
format last_followup %d

//Lets make some bins
gen ruca_combo = . 
replace ruca_combo = 0 if ruca == 1
replace ruca_combo = 1 if ruca != 1

egen Tsize_bucket = cut(tumor_size), group(4) label icodes
egen year_bucket = cut(yeardx), at(2000,2010,2013,2016,2019,2022) label

egen age_groups = cut(age), at(30,40,50,60,70,80,90)
egen distance_bucket = cut(distance), group(4) label icodes
egen distance_50 = cut(distance), at(0,100, 5514) label icodes

egen Tsize_2 = cut(tumor_size), at(0,20,50,200)

gen lonelybucket = . 
replace lonelybucket = 0 if marital_encode != 3 
replace lonelybucket = 1 if lonelybucket != 0 

/*
//ALT BINS
egen Tsize_bucket = cut(tumor_size), group(2) label icodes
drop if yeardx <= 2009
drop if yeardx >= 2020
tab yeardx 
egen year_bucket = cut(yeardx), group(2) label
drop if year_bucket == . 

egen age_groups = cut(age), group(3) label
egen distance_bucket = cut(distance), group(2) label icodes
egen distance_50 = cut(distance), at(0,200, 5514) label icodes
*/

//Survival Analyses for sanity check 
/*
gen yearsofsurvival = last_followup - diagnosis_date
stset yearsofsurvival, failure(dead==1)
sts graph, title(OS) risktable 
sts graph, by(er) title(OS) risktable 
//ER+ lives longer
sts graph, by(age_groups) title(OS) risktable 
//older people die faster
sts graph, by(surgery_yn) title(OS) risktable 
//people that got surgery live longer
sts graph, by(year_bucket) title(OS) risktable 
//oddly no change over time 
sts graph, by(visceral) title(OS) risktable 
//oddly no difference
sts graph, by(ruca) title(OS) risktable 
//low data numbers for anything but group #1
sts graph, by(adi) title(OS) risktable 
//low data numbers for anything but group #1
sts graph, by(ses) title(OS) risktable 
//low data numbers for anything but group #1
sts graph, by(Tsize_bucket) title(OS) risktable 
//bigger tumors die quicker 
sts graph, by(radiation_yn) title(OS) risktable 
//no difference
sts graph, by(distance_50) title(OS) risktable 
//no difference when splot at 50 km or 100 km 
*/

drop if yeardx <= 2009
drop if yeardx == 2020
drop if yeardx == 2021


//ACTUAL ANALYSIS

tab surgery_yn 
//34% surgery rate
tab surgery_yn yeardx, column
tab surgery_yn year_bucket, column
//goes from 37% to 23% over time. but its pretty stable until 2017 at which point we see a step of to the low 20%s 
tab surgery_yn age_groups, column 
//goes from 49% to 21% steps down pretty well 

tab surgery_yn distance_50, column

tab surgery_yn race_encode, column
//all around 30% but 7% higher in hispanic persons 
tab surgery_yn IncomeBucket, column 
tab surgery_yn ruca, column
//for ruca we have sparse data in everything that isn't the most urban which is why i bin into ruca_combo for the second regression
tab surgery_yn adi, column
//adi is pretty okay for spread
tab surgery_yn ses, column
//SES is okay as well 
tab surgery_yn insurance_encode, column
//Medicare has a little less 
tab surgery_yn distance_50, column
tab surgery_yn distance_bucket, column
drop if distance > 500

//Univariates
logistic surgery_yn ib1.ruca, vce(r) 
logistic surgery_yn i.ruralUrbanBucket, vce(r) 
logistic surgery_yn distance
logistic surgery_yn i.distance_50
logistic surgery_yn i.distance_bucket

//we see OR of 3 for Ruca 4 only, not sure if this is signal bc otherwise z score is only 2.07 and the other are not in the same direction or showing a dose effect 
logistic surgery_yn i.ruca_combo, vce(r) 
logistic surgery_yn ib1.adi, vce(r) 
//all ADI have higher ORs (1.3-1.6) but all p values > 0.05 
logistic surgery_yn adi, vce(r) 
//forced linear p 0.10
logistic surgery_yn ib1.ses, vce(r) 
//we see less surgery in lower SES all non sig 
logistic surgery_yn ses, vce(r) 
//sample w forced linear 
logistic surgery_yn age_groups, vce(r) 
logistic surgery_yn age, vce(r) 
logistic surgery_yn i.year_bucket, vce(r) 
logistic surgery_yn yeardx, vce(r) 
//we see a sig trend for year of diagnosis and age
logistic surgery_yn i.visceral, vce(r) 
//not sig but less prob of surgery with visc 
logistic surgery_yn i.Tsize_bucket, vce(r) 
logistic surgery_yn tumor_size, vce(r) 
logistic surgery_yn i.Tsize_2, vce(r)
tab surgery_yn Tsize_2, column 
//we see more surgery with tumors > 1-2 cm but it isn't a linear dose effect. we see the highest in the mid-size tumors which you could argue makes the most sense clinically


//We need to discuss how to do data imputation because right now the models are running with ~500 patients rather than 700ish because of various variables missing (eg ER status for a couple patients)
  
//Everything models 
logistic surgery_yn i.ruca_combo i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

probit surgery_yn i.ruca_combo i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

//Backwards stepwise selection
stepwise, pr(.2): logistic surgery_yn i.ruca_combo i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

//Forward selection 
stepwise, pe(.1): logistic surgery_yn i.ruca_combo i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

//With distance and distance 50 and Rural/Urban buckets 
stepwise, pr(.2): logistic surgery_yn i.ruralUrbanBucket i.distance_50 i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.IncomeBucket i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

stepwise, pr(.2): logistic surgery_yn i.ruralUrbanBucket i.distance_bucket i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.IncomeBucket i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 


stepwise, pr(.2): logistic surgery_yn i.distance_bucket i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

stepwise, pr(.2): logistic surgery_yn i.distance_50 i.adi i.ses i.race_encode i.er i.pr i.her2 yeardx i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 

//Final model 
logistic surgery_yn i.ruralUrbanBucket i.distance_50 i.race_encode i.er i.her2 i.year_bucket i.IncomeBucket i.age_groups i.lonelybucket ib5.insurance_encode i.visceral i.Tsize_bucket, vce(r) 


/*	
surgery_yn 				Odds ratio	std. err.	z		P>z	[95% conf.	interval]
					
ruralUrbanBucket    	.7235726	.1964794	-1.19	0.233	.4249583	1.23202

distance_50 
100-     				2.033653	.6166973	2.34	0.019	1.122415	3.684685

race_encode 
American Indian            1	(empty)
Asian/Pacific Islander      .434746	.4850715	-0.75	0.455	.0488091	3.872316
Hispanic    			  .345943	.3893823	-0.94	0.346	.0380995	3.141156
Non-Hispanic Black   	  .3004472	.3563128	-1.01	0.311	.0293963	3.070744
Non-Hispanic White  	   .4263461	.4719085	-0.77	0.441	.0487074	3.731894
Unknown           			 1	(omitted)

1.er  					  .5034126	.1151455	-3.00	0.003	.3215354	.7881691
1.her2  				  .7306515	.1560389	-1.47	0.142	.480758	1.110437

year_bucket 
2013- 				    1.070284	.2556345	0.28	0.776	.6701806	1.709253
2016- 				    .8144572	.2014312	-0.83	0.407	.5015907	1.322474
2019- 				    .4437179	.2091861	-1.72	0.085	.1761234	1.117885

IncomeBucket 
2   				  .9477959	.2899936	-0.18	0.861	.5203263	1.726449
3 					    1.001126	.3229747	0.00	0.997	.5319627	1.884066

age_groups 
40 					    .6585418	.2201758	-1.25	0.212	.3419747	1.268156
50 					    .4647036	.1521398	-2.34	0.019	.2446246	.8827787
60 					    .3013492	.1076572	-3.36	0.001	.149616	.6069631
70 					    .2977259	.1298369	-2.78	0.005	.1266519	.6998768
80 					     .200163	.1113161	-2.89	0.004	.0672988	.5953334

1.lonelybucket 		   .9972838	.1982167	-0.01	0.989	.6755179	1.472315

insurance_encode 
.   					  .7352284	.4559279	-0.50	0.620	.2180607	2.478946
Medicaid     			1.502797	.4507679	1.36	0.174	.8347957	2.705331
Medicare			     1.316431	.3579584	1.01	0.312	.7725791	2.243124
Uninsured           	 1	(empty)

1.visceral    			.6021854	.1207324	-2.53	0.011	.4065112	.8920474

Tsize_bucket 
23.5-   				  1.881178	.4877698	2.44	0.015	1.131676	3.12707
39.5-  					   1.027368	.2948334	0.09	0.925	.5853947	1.803032
60-   					  1.823608	.4904024	2.23	0.025	1.076533	3.089126

_cons  					  5.365048	6.599586	1.37	0.172	.481398	59.79199
					
Note: _cons estimates baseline odds.


*/
//The Models with complete case show that visceral, ER status, tumor size, age all matter. Distance matters and I went and checked to make sure that the zip codes / addresses are really rural. they are. I exclude distance > 500 to cut out second opinions from LA / Hawaii etc and it doesn't change the results. I interpret this as biology and age remain consistent predictors of surgery decisions but rural california may play a role. I do not see the same correlation for RUCA and SES status, but I'm not sure we have enough data and that these could be sensitive to binning in SEER and we have slightly different bins here


save ASCO2024_Oncoshare.dta, replace

log close, replace
