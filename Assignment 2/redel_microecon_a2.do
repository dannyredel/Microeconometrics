/*******************************************************************************
ASSIGNMENT 2: MICROECONOMETRICS
DANIEL REDEL S.
DATE: Fall 2022
*******************************************************************************/
clear all

*ssc install blindschemes, replace 
*ssc install grstyle, replace
*ssc install palettes, replace
set scheme tab2, permanently

/*******************************************************************************
1. SURVIVAL ANALYSIS
*******************************************************************************/
set maxvar 20000
*---------------------------------------------*
*--Question a.1. Data Cleaning: tracker2014 --*
*---------------------------------------------*
clear
use "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\data\tracker2014.dta"

keep HHID PN BIRTHYR DEGREE GENDER HISPANIC RACE NYEAR AIWWAVE
keep if AIWWAVE == 1

destring HHID PN, replace
gen long hhidpn = HHID*1000+PN
format hhidpn %12.0g

*	Export new database
save "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\processed_data\tracker2014v1.dta", replace

*---------------------------------------------*
*--Question a.2. Data Cleaning: randhrs2018 --*
*---------------------------------------------*
clear
use "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\data\randhrs2018.dta"
keep hhidpn r1iwstat r1momliv r1dadliv r1momage r1dadage r1smokev r1smoken r1drinkr r1bmi r1hibpe r1diabe r1cancre r1lunge r1hearte r1arthre
keep if r1iwstat==1

*	Export new database
save "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\processed_data\randhrs2018v1.dta", replace

*-----------------------------------------*
*--Question b. Merging the two datasets --*
*-----------------------------------------*
clear all
use "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\processed_data\tracker2014v1.dta"

merge 1:1 hhidpn using "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\processed_data\randhrs2018v1.dta"
keep if _merge==3 // we drop hhidpn= 20582020 and 204940020

* 	Export FINAL database: data1
save "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\processed_data\data1.dta", replace

*----------------------------------------*
*--Question c. Setting Survival Format --*
*----------------------------------------*
clear all
use "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A2\processed_data\data1.dta", replace
drop if BIRTHYR == 0 // we drop this observations, it's useless

*	Censored
gen censored = 0
replace censored = 1 if NYEAR > 0

*	Last Time observed
gen last_observed = NYEAR
replace last_observed = 2011 if NYEAR==0

*	Declare Survival Data:
stset last_observed, failure(censored) origin(time BIRTHYR) enter(NYEAR==1992) 

*---------------------------------------*
*--Question d. Kaplan Meier Estimator --*
*---------------------------------------*
grstyle init
grstyle set color tab2, opacity(70)

*	Kaplan-Meier Estimate:
sts graph ///
	, by(GENDER) noorigin tmax(100) tmin(55) xla(55(5)95) ///
	title("") xtitle("Duration in Years") ytitle("Survival Probability") ///
	legend(label(1 "Male") label(2 "Female")) 
	
gr export figure1.png, as(png) replace

*------------------------------------------*
*--Question e. Weibull PH Regression N°1 --*
*------------------------------------------*	
streg, distribution(weibull)
est store phr1
streg, distribution(weibull) nohr
est store reg1

*----------------------------------------*
*--Question e.i. Generating Covariates --*
*----------------------------------------*

*	Female
gen female = 0
replace female = 1 if GENDER==2
tab female

*	Black
tab RACE
gen black = 0
replace black = 1 if RACE==2
replace black = . if RACE==0 // we miss 1 value
tab black 

*	Hispanic
tab HISPANIC
gen hispanic = 0 
replace hispanic = 1 if HISPANIC < 5 
replace hispanic = . if HISPANIC==0 // we miss 11 values
tab hispanic 

* 	High-School Degree
tab DEGREE
gen highschool = 0 
replace highschool = 1 if DEGREE==1 | DEGREE==2
tab highschool

* 	College Degree
tab DEGREE
gen college = 0 
replace college = 1 if DEGREE==3 | DEGREE==4 | DEGREE==5 | DEGREE==6
tab college

*----------------------------------------------*
*--Question e.i.1. Weibull PH Regression N°2 --*
*----------------------------------------------*
*We save the controls in a vector:
global cov female black hispanic highschool college

streg $cov, distribution(weibull) 
est store phr2
streg $cov, distribution(weibull) nohr
est store reg2

*Some Graphs
stcurve, hazard at1(female=0) at2(female=1)
stcurve, hazard at1(black=0) at2(black=1)
stcurve, hazard at1(hispanic=0) at2(hispanic=1)
stcurve, hazard at1(highschool=0) at2(highschool=1)
stcurve, hazard at1(college=0) at2(college=1)

*------------------------------------------------*
*--Question e.ii. Parents Longevity Covariates --*
*------------------------------------------------*

* 	Father Longevity
sum r1dadage 
sum r1dadage if r1dadliv==0
tab r1dadage if r1dadliv==0

gen father = 0
replace father = 1 if r1dadage < 65 & r1dadliv==0
replace father = 0 if r1dadage >= 65 & r1dadliv==0
replace father = . if r1dadage == .m
tab father 


* 	Mother Longevity
table r1momage  
sum r1momage 
sum r1momage if r1momliv==0
tab r1momage if r1momliv==0

gen mother = 0
replace mother = 1 if r1momage < 70 & r1momliv==0
replace mother = 0 if r1momage >= 70 & r1momliv==0
replace mother = . if r1momage == .m
tab mother

*-----------------------------------------------*
*--Question e.ii.1. Weibull PH Regression N°3 --*
*-----------------------------------------------*
*We save the controls in a vector:
global parents father mother 

streg $cov $parents, distribution(weibull)
est store phr3
streg $cov $parents, distribution(weibull) nohr
est store reg3

*Graphs
stcurve, hazard at1(father=0) at2(father=1)
stcurve, hazard at1(mother=0) at2(mother=1)

*--------------------------------------*
*--Question e.iii. Health Covariates --*
*--------------------------------------*

* 	Current Smoker
tab r1smoken
gen current_smoker = 0 
replace current_smoker = 1 if r1smoken==1
tab current_smoker

* 	Past Smoker
table r1smokev r1smoken
gen past_soker = 0
replace past_soker = 1 if r1smokev==1 & r1smoken==0
tab past_soker

*	Heavy Drinker
tab r1drinkr
gen heavy_drinker = 0
replace heavy_drinker = 1 if r1drinkr == 3 | r1drinkr == 4
tab heavy_drinker

* 	Moderate Drinker
tab r1drinkr
gen mod_drinker = 0
replace mod_drinker = 1 if r1drinkr == 1 | r1drinkr == 2
tab mod_drinker

*	Overweight
tab r1bmi
gen overweight = 0 
replace overweight = 1 if r1bmi >= 25 & r1bmi <= 30
tab overweight

* 	Obese
tab r1bmi
gen obese = 0 
replace obese = 1 if r1bmi > 30
tab obese

*------------------------------------------------*
*--Question e.iii.1. Weibull PH Regression N°4 --*
*------------------------------------------------*
*We save the controls in a vector:
global health current_smoker past_soker heavy_drinker mod_drinker overweight obese

streg $cov $parents $health, distribution(weibull)
est store phr4
streg $cov $parents $health, distribution(weibull) nohr
est store reg4

*Graphs
stcurve, hazard at1(current_smoker=0) at2(current_smoker=1)
stcurve, hazard at1(obese=0) at2(obese=1)


*---------------------------------------------*
*--Question e.iv. Weibull PH Regression N°5 --*
*---------------------------------------------*
sum r1hibpe r1diabe r1cancre r1lunge r1hearte r1arthre

*We save the controls in a vector:
global chronic r1hibpe r1diabe r1cancre r1lunge r1hearte r1arthre

streg $cov $parents $health $chronic, distribution(weibull)
est store phr5
streg $cov $parents $health $chronic, distribution(weibull) nohr
est store reg5

*Graphs
stcurve, hazard at1(r1lunge=0) at2(r1lunge=1)
stcurve, hazard at1(r1diabe=0) at2(r1diabe=1)

*---------------------------------------------*
*--Question e.v.1. Results Table Coefficients --*
*---------------------------------------------*
* Results Table	(showing coefficients)		
esttab reg1 reg2 reg3 reg4 reg5, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
mtitles("(1)" "(2)" "(3)" "(4)" "(5)") nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(r2 N)
	
esttab reg1 reg2 reg3 reg4 reg5 using table1.tex, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
mtitles("(1)" "(2)" "(3)" "(4)" "(5)") nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(r2 N) replace

*------------------------------------------*
*--Question e.v.2. Results Table Hazards --*
*------------------------------------------*
* Results Table	(showing proportional hazard)	
esttab phr1 phr2 phr3 phr4 phr5, eform cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) /// 
nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(r2 N)
	
esttab phr1 phr2 phr3 phr4 phr5 using table2.tex, eform cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) /// 
nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(r2 N) replace
	

