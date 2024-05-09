//SEER v1 
cd "/Users/jamesdickerson/Google Drive/Research/Active Projects/Badcare/ASCO 2024/"
pwd
dir
clear
set more off
capture log using ASCOSEER.log, replace

use  SEER_ASCOv1.dta

//Jan 5 2024 analysis

//This is looking at surgery rates over time and shows that there are less surgeries if ER+ and that the rates decrease over time. There was a rough and ready analysis done in R for graphing purposes. 

gen surgeryvsnot = .
replace surgeryvsnot = 0 if mast_vs_lump == "."
replace surgeryvsnot = 1 if mast_vs_lump != "."
tab surgeryvsnot if SummaryStage == "Distant" 

tab Tsize if SummaryStage == "Distant" 
hist Tsize if SummaryStage == "Distant" 

replace Tsize = . if Tsize == 999
//998 means "diffuse" involvement per SEER coding. 999 means unknown 

egen Tsize_bucket4 = cut(Tsize), group(4) label icodes
//drop unknown residence 
drop if residenceencode == 6

//this cuts it at the SEER metro vs (really) rural cut off 
gen ruralUrbanBucket = . 
replace ruralUrbanBucket = 1 if residenceencode == 1 | residenceencode == 2 | residenceencode == 3
replace ruralUrbanBucket = 2 if ruralUrbanBucket != 1

//this cuts it at <50k, 50-75k, >75k of household income 
gen incomeBucket = . 
replace incomeBucket = 1 if incomeencode == 0
replace incomeBucket = 1 if incomeencode == 1 
replace incomeBucket = 1 if incomeencode == 2 
replace incomeBucket = 1 if incomeencode == 3 

replace incomeBucket = 2 if incomeencode == 4 
replace incomeBucket = 2 if incomeencode == 5 
replace incomeBucket = 2 if incomeencode == 6 
replace incomeBucket = 2 if incomeencode == 7
replace incomeBucket = 2 if incomeencode == 8

replace incomeBucket = 3 if incomeencode == 9
tab incomeBucket

//because all non-married have less surgery with roughly the same effect size, lummp them together for some analyses below
gen lonelybucket = . 
replace lonelybucket = 0 if maritalstatusencode != 2 
replace lonelybucket = 1 if lonelybucket != 0 

//ADDITIONAL ALTERNATIVE BINNING 
egen Tsize_bucket_alt = cut(Tsize), group(2) label icodes
egen Tsize_bucket_alt2 = cut(Tsize), group(8) label icodes
generate Tsize_log = log(Tsize)
egen age_groups_alt = cut(age), group(5) label

//very metro vs other 
gen ruralUrbanBucket_alt = . 
replace ruralUrbanBucket_alt = 1 if residenceencode == 1 
replace ruralUrbanBucket_alt = 2 if ruralUrbanBucket != 1


gen incomeBucket_alt = . 
replace incomeBucket_alt = 1 if incomeencode == 0
replace incomeBucket_alt = 1 if incomeencode == 1 
replace incomeBucket_alt = 2 if incomeencode == 2 
replace incomeBucket_alt = 2 if incomeencode == 3 

replace incomeBucket_alt = 2 if incomeencode == 4 
replace incomeBucket_alt = 2 if incomeencode == 5 
replace incomeBucket_alt = 3 if incomeencode == 6 
replace incomeBucket_alt = 3 if incomeencode == 7
replace incomeBucket_alt = 3 if incomeencode == 8

replace incomeBucket_alt = 4 if incomeencode == 9
tab incomeBucket_alt


gen incomeBucket_alt2 = . 
replace incomeBucket_alt2 = 0 if incomeencode == 0
replace incomeBucket_alt2 = 0 if incomeencode == 1 
replace incomeBucket_alt2 = 0 if incomeencode == 2 
replace incomeBucket_alt2 = 0 if incomeencode == 3 

replace incomeBucket_alt2 = 0 if incomeencode == 4 
replace incomeBucket_alt2 = 0 if incomeencode == 5 
replace incomeBucket_alt2 = 0 if incomeencode == 6 
replace incomeBucket_alt2 = 0 if incomeencode == 7
replace incomeBucket_alt2 = 0 if incomeencode == 8

replace incomeBucket_alt2 = 1 if incomeencode == 9

//ACTUAL ANALYSIS

tab surgeryvsnot if SummaryStage == "Distant"
//30% surgery rate
tab surgeryvsnot yearofdiagnosis if SummaryStage == "Distant", column
//goes from 41% to 21% over time 
tab surgeryvsnot age_groups if SummaryStage == "Distant", column
//44% in 30s to 19% in 80s

tab surgeryvsnot raceendode if SummaryStage == "Distant", column
//all about 30%

tab surgeryvsnot ruralUrbanBucket if SummaryStage == "Distant", column
//33.8 v 29.2

tab surgeryvsnot incomeBucket if SummaryStage == "Distant", column
//36% to 27%

//I run the same regression a bunch of different ways below. this is not to cherry pick but rather to ensure that the effect size and the reported urban/rural and income differences are robust to a bunch of specifications. These, along with visceral disease, ER status, age, marital status, and year are significant in every model below. 

//Univariates
logistic surgeryvsnot ib3.incomeBucket if SummaryStage == "Distant" , vce(r) 
//OR 1.5 w highest income (ib3) as reference group

logistic surgeryvsnot ib1.ruralUrbanBucket if SummaryStage == "Distant" , vce(r) 
//OR 1.23 w most urban as reference

logistic surgeryvsnot i.residenceencode if SummaryStage == "Distant" , vce(r) 
//most urban as reference OR goes 1.16-->1.28-->1.27-->1.31 as you get more rural

logistic surgeryvsnot ib0.incomeencode if SummaryStage == "Distant" , vce(r) 
//in this <35k is reference. Split out the only sig differences are the >70k and >75k categories but there is a graded effect for the odds ratios from 1.0 to .9 to .75 to .65 as you go across the income groups. I also dont thing there are huge differences between life at 54,999 USD and 55,001 USD so I think binning them into larger income buckets makes more sense

logistic surgeryvsnot incomeBucket if SummaryStage == "Distant" , vce(r) 
//when we force a linear then the OR is 0.8 and is significant (meaning less surgery as income goes up)

logistic surgeryvsnot ruralUrbanBucket if SummaryStage == "Distant" , vce(r) 
//when we force a linear then the OR is 1.2 and is significant (meaning more surgery as rurality goes up)

//less surgery for more wealth and more urban regardless of ordinal vs categorical and alt binning vs trad binning 

//Everything model  and final model, only removal was PR. age & year as linear 
logistic surgeryvsnot ib3.raceendode ib3.incomeBucket i.ruralUrbanBucket i0.lonelybucket yearofdiagnosis age ERendode HER2endode i.visceral_mets i.Tsize_bucket4 if SummaryStage == "Distant" , vce(r) 

//Full results 
/*
						
		
surgeryvsnot	Odds ratio	std. err.	z	P>z	[95% conf.	interval]
						
raceendode	
Black			.9404038	.0435306	-1.33	0.184	.8588412	1.029712
Latina			1.051179	.052476		1.00	0.317	.9531997	1.15923
Other			1.003703	.0546342	0.07	0.946	.9021364	1.116705
	
incomeBucket	
1				1.299225	.0795209	4.28	0.000	1.152352	1.464817
2				1.039679	.0361755	1.12	0.263	.9711396	1.113055
	
2.ruralUrbanBucket	
				1.121001	.0648972	1.97	0.048	1.000756	1.255694
0.lonelybucket	
				.7985775	.0254682	-7.05	0.000	.7501888	.8500873
yearofdiagnosis	
				.8964954	.0050347	-19.46	0.000	.8866816	.9064178
age				
				.97978		.001136		-17.62	0.000	.9775561	.982009
ERendode		
				.635359		.0233555	-12.34	0.000	.5911932	.6828242
HER2endode		 1.196404	.0430874	4.98	0.000	1.114865	1.283905
1.visceral_mets	.4251313	.0139483	-26.07	0.000	.3986536	.4533677
	
Tsize_bucket4	
11-	1.675468	.1640291	5.27	0.000	1.38294	2.029875
18-	2.219494	.1912156	9.25	0.000	1.874652	2.62777
30-	2.059475	.1655144	8.99	0.000	1.759332	2.410823
	
_cons	9.30e+95	1.05e+97	19.53	0.000	2.17e+86	4.0e+105
						

*/

//Alt specs
logistic surgeryvsnot ib3.raceendode ib1.incomeBucket_alt i.ruralUrbanBucket_alt i0.lonelybucket i.yearofdiagnosis i.age_groups_alt ERendode PRendode HER2endode visceral_mets i.Tsize_bucket_alt if SummaryStage == "Distant" , vce(r) 
//In the alt binning urban/rural keeps its 1.2 OR but the income loses significance 

logistic surgeryvsnot ib3.raceendode i.incomeBucket_alt2 i.ruralUrbanBucket_alt i0.lonelybucket i.yearofdiagnosis i.age_groups_alt ERendode PRendode HER2endode visceral_mets i.Tsize_bucket_alt if SummaryStage == "Distant" , vce(r) 
//same here. if we do <75k vs >75k then we dont see the relationship 

//Alt binning of the Tsize 
logistic surgeryvsnot ib3.raceendode i.incomeBucket_alt2 i.ruralUrbanBucket_alt i0.lonelybucket i.yearofdiagnosis i.age_groups_alt ERendode PRendode HER2endode visceral_mets i.Tsize_bucket_alt2 if SummaryStage == "Distant" , vce(r) 
//we continue to see that bigger tumors have more surgery 

//Forced linear for variables of interest 
logistic surgeryvsnot ib3.raceendode incomeBucket ruralUrbanBucket i0.lonelybucket i.yearofdiagnosis i.age_groups ERendode PRendode HER2endode i.visceral_mets Tsize_log if SummaryStage == "Distant" , vce(r) 
//here we see the relationship for income and rural/urban is significant. we see no relationship for tumor size if we log it (given the right tailed distribution) or dont log it 

//No binning beyond what SEER bins. I removed non-sig ß of 0 things (race, PR status) in earlier iterations. 
logistic surgeryvsnot ib9.incomeencode i.residenceencode i0.lonelybucket i.yearofdiagnosis i.age_groups ERendode HER2endode visceral_mets i.Tsize_bucket4 if SummaryStage == "Distant" , vce(r) 
//here the rural/urban remains significant and the big ∆ is at the 250k mark suggesting there is maybe a cutoff of size. we don't see much of a gradient by income 

//Probit 
probit surgeryvsnot ib3.raceendode ib3.incomeBucket i.ruralUrbanBucket ib2.maritalstatusencode i.yearofdiagnosis i.age_groups ERendode PRendode HER2endode visceral_mets Tsize_bucket4 if SummaryStage == "Distant" , vce(r) 

//Backwards stepwise selection
stepwise, pr(.2): logistic surgeryvsnot ib3.raceendode ib3.incomeBucket i.ruralUrbanBucket i.lonelybucket i.yearofdiagnosis i.age_groups ERendode PRendode HER2endode visceral_mets Tsize_bucket4 if SummaryStage == "Distant" , vce(r) 
//things that are kept: ER, HER2, marital status, age, year, Tsize, income, rural or urban

//Backwards alt
stepwise, pr(.2): logistic surgeryvsnot ib3.raceendode ib1.incomeBucket_alt i.ruralUrbanBucket_alt i0.lonelybucket i.yearofdiagnosis i.age_groups_alt ERendode PRendode HER2endode visceral_mets i.Tsize_bucket_alt if SummaryStage == "Distant" , vce(r) 
//keeps ER, age, year, lonely, rural/urban, income, Tsize, visceral, HER2. Here we do see the income difference with the alternate buckets for the highest two income groups

//Forward selection 
stepwise, pe(.1): logistic surgeryvsnot ib3.raceendode ib3.incomeBucket i.ruralUrbanBucket i.lonelybucket i.yearofdiagnosis i.age_groups ERendode PRendode HER2endode visceral_mets Tsize_bucket4 if SummaryStage == "Distant" , vce(r) 
//things that are kept: visceral, ER, age, year, tsize, lonely, income, rural/urban, HER2

//CONCLUSION
//CONCLUSION
//
//CONCLUSION
//While income rurality are sensitive to binning on forward and backward stepwise selection, probit and logistic models with either forced linear or at least two binning methods we see a significant relationship. Therefore, feel confident in reporting the results especially given the distance relationship in Oncoshare








//Survival analysis, not presented in the poster
//Similar to others surgery vs not has a big survival difference but there is a large issue of endogeneity. As you can see with the improving survival over the 2010s any intervention that went from 50% use to 20% use likely has a less profound effect than is estimated in these SEER analyses given issues around patient selection even with more sophisticated weights for covariates not available in SEER

survival analysis
gen tempvar=1
graph bar (count) tempvar, over(incomeencode)

stset yearsofsurvival, failure(thiscancerkilledthem==1)
sts graph if SummaryStage == "Distant", by(surgeryvsnot) title(Breast cancer specific survival) risktable

stset yearsofsurvival, failure(thiscancerkilledthem==1)
sts graph if SummaryStage == "Distant", by(age_groups) title(Breast cancer specific survival) risktable

//If age < 50 dx 2010 cox surgery vs not 2012 2015 
//plot hazard ratio by year 
//oncoshare other stuff 
//create KM curves 
local yearlist 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019
foreach year in `yearlist' {
    // Step 2: Run the regression and store the results
    stcox i.surgeryvsnot if SummaryStage == "Distant" & yearofdiagnosis == `year', vce(robust)
}

// Step 3: Plot the stored Hazard Ratios against years. For simplicity, we are using a scatterplot without error bars.
twoway line HR_results yearlist, title("Hazard Ratios by Year") ///
    xtitle("Year of Diagnosis") ytitle("Hazard Ratio") ///
    name(HRplot, replace)

sts graph if visceral_mets == 1, by(surgeryvsnot) title(Breast cancer specific survival) risktable

stcox i.surgeryvsnot i.yearofdiagnosis i.age_groups i.visceral_mets i.grade_combined ib9.incomeencode ib1.residenceencode ib2.maritalstatusencode ERendode PRendode HER2endode ib1.timetotreatment_bucket2 if SummaryStage == "Distant", vce(r) 


save SEER_ASCO_w_stats.dta, replace

log close, replace

