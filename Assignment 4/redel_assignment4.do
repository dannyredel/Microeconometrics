/*******************************************************************************
ASSIGNMENT 4: MICROECONOMETRICS
DANIEL REDEL S.
DATE: Fall 2022
*******************************************************************************/
clear all

*ssc install blindschemes, replace 
*ssc install grstyle, replace
*ssc install palettes, replace
set scheme white_tableau, permanently

/*******************************************************************************
1. BIVARIATE PROBIT MODEL
*******************************************************************************/
set maxvar 20000
*-------------*
*--Load Data--*
*-------------*
clear
use "C:\Users\danny\OneDrive\Análisis Cuantitativo y Econometría\Microeconometrics\Courses\Microeconometrics - Tilburg\Class 2022\Assignments\A4\school.dta"

*---------------------------------------*
*--Question 1.1. Likelihood Function  --*
*---------------------------------------*
* Document

*---------------------------------------*
*--Question 1.2. Biprobit Estimation  --*
*---------------------------------------*
global covariates years logptax loginc

biprobit private vote $covariates
est store biprobit1

*-----------------------------------*
*--Question 1.2.1. Biprobit TABLE --*
*-----------------------------------*

esttab biprobit1 using table1.tex, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))  ///
mtitles("Biprobit") nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(N ll, fmt(a4 4 a4) ///
labels("Observations" "Log-Likelihood"))


/*******************************************************************************
2. BINARY CHOICE MODELS AND NONPARAMETRIC ESTIMATION
*******************************************************************************/
colorpalette viridis, n(6) nograph 
return list
local color1 = r(p1) // "68 1 84" -- dark purple
local color2 = r(p2) // "65 68 135" -- dark blue
local color3 = r(p3) // "42 120 142" -- dark green
local color4 = r(p4) // "34 168 132" -- green
local color5 = r(p5) // "122 209 81" -- light green
local color5 = r(p6) // "253 231 37" -- yellow

*---------------------------------------------*
*--Question 2.1. KERNEL DENSITY ESTIMATION  --*
*---------------------------------------------*

kdens loginc, ci normal bw(silverman) kernel(epanechnikov) /// ** default is silverman
	color("34 168 132") normopts(lcolor("68 1 84")) ///
	legend(pos(6) row(1)) xscale(range(8(1)12)) xlabel(8(1)12) ///
	xtitle("Log(Income)")
gr export figure1_1.png, as(png)  replace

kdens loginc, ci normal bw(oversmooth) kernel(epanechnikov) ///
	color("34 168 132") normopts(lcolor("68 1 84")) ///
	legend(pos(6) row(1)) xscale(range(8(1)12)) xlabel(8(1)12) ///
	xtitle("Log(Income)")
gr export figure1_2.png, as(png) replace

kdens loginc, ci normal bw(dp) kernel(epanechnikov) ///
	color("34 168 132") normopts(lcolor("68 1 84")) ///
	legend(pos(6) row(1)) xscale(range(8(1)12)) xlabel(8(1)12) ///
	xtitle("Log(Income)")
gr export figure1_3.png, as(png) replace


*-----------------------------------------------*
*--Question 2.2. SIMPLE BINARY CHOICE PROBIT  --*
*-----------------------------------------------*
probit vote $covariates
est store probit1

*--------------------------------------------------------------------*
*--Question 2.3. NONPARAMETRIC KERNEL REGRESSIONS: SIMPLE PROBIT   --*
*--------------------------------------------------------------------*
probit vote $covariates
*Normality Assumption:
predict y_hat, xb
predict y_prob, p

*----------------------------------------------------*
*--Question 2.3.1. Nadaray-Watson (Local Constant) --*
*----------------------------------------------------*
/* NPREGRESS: can be used to indicate a suitable bandwidth */

** NPREG **

npregress kernel vote y_hat, estimator(constant) vce(bootstrap, reps(100) seed(123)) noderivatives
npgraph, addplot(line y_prob y_hat, sort lcolor(blue))
* Optimal h = 0.3496

** Plot 1: Cross Validation h = 0.3496 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote y_hat, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Nadaraya-Watson") label(4 "Probit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote y_hat, degree(0) bw(0.3496) color("`color1'") alcolor(%10) fintensity(10) ///
	|| line y_prob y_hat, sort color("`color2'") // saving(Figure1)
gr export figure2_1.png, as(png)  replace
	
*----------------------------------------------------*
*--Question 2.3.2. Locar Polynomial (Local Linear) --*
*----------------------------------------------------*
** NPREG **
npregress kernel vote y_hat, estimator(linear) vce(bootstrap, reps(100) seed(123)) predict(lmean lderiv)
npregress kernel vote y_hat, estimator(linear) vce(bootstrap, reps(100) seed(123))
npgraph, addplot(line y_prob y_hat, sort lcolor(blue))
* Optimal h = 0.2746

** Plot 1: Cross Validation h = 0.2746 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote y_hat, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Local Linear") label(4 "Probit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote y_hat, degree(1) bw(0.2746) color("`color1'") alcolor(%10) fintensity(10) ///
	|| line y_prob y_hat, sort color("`color2'") // saving(Figure1)
gr export figure3_1.png, as(png)  replace


*---------------------------------------------*
*--Question 2.4. HETEROSKEDASTICITY PROBIT  --*
*---------------------------------------------*
hetprobit vote $covariates , het(years)
est store probit2

*---------------------------------------------------------------*
*--Question 2.5. NONPARAMETRIC KERNEL REGRESSIONS: HETPROBIT  --*
*---------------------------------------------------------------*
hetprobit vote $covariates , het(years)
*Normality Assumption:
predict hety_hat, xb
predict hety_prob, p

*-------------------------------*
*--Question 2.5.1. REGRESSION --*
*-------------------------------*
*-------------------------*
*--Question 2.5.2. PLOT --*
*-------------------------*
*----------------------------------------------------*
*--Question 2.5.2.a. Nadaray-Watson (Local Constant) --*
*----------------------------------------------------*
** NPREG **
npregress kernel vote hety_hat, estimator(constant) vce(bootstrap, reps(100) seed(123)) noderivatives predict(het_cmean)
npregress kernel vote hety_hat, estimator(constant) vce(bootstrap, reps(100) seed(123)) noderivatives
npgraph, addplot(line hety_prob hety_hat, sort lcolor(blue))
* Optimal h = 0.8906

** Plot 1: Cross Validation h = 0.8906 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote hety_hat, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Nadaraya-Watson") label(4 "HetProbit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote hety_hat, degree(0) bw(0.8906) color("`color1'") alcolor(%10) fintensity(10) ///
	|| line hety_prob hety_hat, sort color("`color2'") // saving(Figure1)
gr export figure4_1.png, as(png)  replace
	

*----------------------------------------------------*
*--Question 2.5.2.b Locar Polynomial (Local Linear) --*
*----------------------------------------------------*
** NPREG **
npregress kernel vote hety_hat, estimator(linear) vce(bootstrap, reps(100) seed(123)) predict(hetlmean hetlderiv)
npregress kernel vote hety_hat, estimator(linear) vce(bootstrap, reps(100) seed(123)) 
npgraph, addplot(line hety_prob hety_hat, sort lcolor(blue))
* Optimal h = 1.140637


** Plot 1: Cross Validation h = 1.1406 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote hety_hat, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Local Linear") label(4 "HetProbit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote hety_hat, degree(1) bw(1.140637) color("`color1'") alcolor(%10) fintensity(10) ///
	|| line hety_prob hety_hat, sort color("`color2'") // saving(Figure1)
gr export figure5_1.png, as(png)  replace
	

*---------------------------------------------------------------*
*--Question 2.6. SEMIPARAMETRIC BINARY CHOICE: KLEIN & SPADY  --*
*---------------------------------------------------------------*
sml vote year logptax, offset(loginc)
est store probit3

**Normalization: Probit**
*probit vote logptax loginc year
quietly: probit vote logptax loginc year
nlcom (year_loginc: _b[year] / _b[loginc]) /// 
	(logptax_loginc: _b[logptax] / _b[loginc]) ///
	(loginc_loginc: _b[loginc] / _b[loginc]) ///
	(cons_loginc: _b[_cons] / _b[loginc])
**Normalization: Het Probit**
*hetprobit vote logptax loginc year, het(years)
sml vote year logptax, offset(loginc)
quietly: hetprobit vote logptax loginc year, het(years)
nlcom (year_loginc: _b[year] / _b[loginc]) ///
	(logptax_loginc: _b[logptax] / _b[loginc]) ///
	(loginc_loginc: _b[loginc] / _b[loginc]) ///
	(cons_loginc: _b[_cons] / _b[loginc])

*-------------------*
*-- FINAL TABLE 1 --*
*-------------------*
esttab probit1 probit2 probit3 using table2.tex, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))  ///
mtitles("Probit" "Hetprobit" "Klein & Spady") nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(N ll, fmt(a4 4 a4) ///
labels("Observations" "Log-Likelihood"))

*--------------------------------------------------------------------*
*--Question 2.7. PLOT SEMIPARAMETRIC BINARY CHOICE: KLEIN & SPADY  --*
*--------------------------------------------------------------------*
predict ks_y_hat, xb

** NPREG **
npregress kernel vote ks_y_hat, estimator(constant) noderivatives predict(ks_cmean)
npregress kernel vote ks_y_hat, estimator(constant) noderivatives
npgraph
* Optimal h = 0.3567213

lpoly vote ks_y_hat, mcolor(black) msymbol(circle_hollow) addplot(lpoly vote hety_hat)

** Plot 1: Cross Validation h = 0.3567 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote ks_y_hat, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Klein & Spady") label(4 "HetProbit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote ks_y_hat, degree(0) bw(0.3567213) color("`color1'") alcolor(%10) fintensity(10) ///
	|| lpoly vote hety_hat, sort color("`color2'")
gr export figure6_1.png, as(png)  replace
		
*------------------------------------------------------------*
*--Question 2.8. STRANGE SHAPE IN DISTRIBUTION: OUTLIERS?  --*
*------------------------------------------------------------*
twoway kdensity ks_y_hat if vote==1

gen outlier = 0
replace outlier = 1 if ks_y_hat < -4.6

tabulate outlier, sum(year) 
tabulate outlier, sum(logptax)
tabulate outlier, sum(loginc)
tabulate outlier, sum(vote)
 

twoway kdensity years if vote==1, color("34 168 132") ///
	legend( pos(6) row(1) label(1 "Vote == 1") label(2 "Vote == 0") ) ///
	ytitle("Density") /// 
	|| kdensity years if vote==0, ///
	color("68 1 84") ///
	legend(pos(6) row(1)) ///
	xtitle("Years")
		
twoway kdensity years if ks_y_hat < -4.6, color("34 168 132") ///
	legend( pos(6) row(1) label(1 "Vote == 1") label(2 "Vote == 0") ) ///
	ytitle("Density") 
	
twoway kdensity years if outlier==1, color("34 168 132") ///
	legend( pos(6) row(1) label(1 "x'b<-5") label(2 "Vote x'b>-5") ) ///
	ytitle("Density") /// 
	|| kdensity years if outlier==0, ///
	color("68 1 84") ///
	legend(pos(6) row(1)) ///
	xtitle("Years")
	
	
*-------------------------------------------------------*
*--Question 2.9. BINARY CHOICE PROBIT: YEAR VARIABLE  --*
*-------------------------------------------------------*

*** PROBIT ***
**************
probit vote $covariates if years<25
est store probit4
**Normalization: Probit**
quietly: probit vote $covariates if years<25
nlcom (year_loginc: _b[year] / _b[loginc]) /// 
	(logptax_loginc: _b[logptax] / _b[loginc]) ///
	(loginc_loginc: _b[loginc] / _b[loginc]) ///
	(cons_loginc: _b[_cons] / _b[loginc])

*** HETPROBIT ***
*****************	
hetprobit vote $covariates if years<25, het(years)  
est store probit5 
**Normalization: Het Probit**
quietly: hetprobit vote $covariates if years<25, het(years) 
nlcom (year_loginc: _b[year] / _b[loginc]) ///
	(logptax_loginc: _b[logptax] / _b[loginc]) ///
	(loginc_loginc: _b[loginc] / _b[loginc]) ///
	(cons_loginc: _b[_cons] / _b[loginc])
 
*** KLEIN & SPADY ***
*********************	
sml vote year logptax if years<25, offset(loginc)
est store probit6


*-------------------*
*-- FINAL TABLE 2 --*
*-------------------*
esttab probit4 probit5 probit6 using table4.tex, cells(b(star fmt(%9.3f)) se(par fmt(%9.3f)))  ///
mtitles("Probit" "HetProbit" "Klein & Spady") nonumbers ///
starlevels(* 0.1 ** 0.05 *** 0.01) varwidth(16) modelwidth(12) stats(N ll , fmt(a4 4 a4) ///
labels("Observations" "Log-Likelihood"  ))

*-------------------*
*-- FINAL FIGURES --*
*-------------------*

*** PROBIT ***
**************
probit vote $covariates if years<25
predict y_hat1, xb
predict y_prob1, p

npregress kernel vote y_hat1 if years<25, estimator(constant) noderivatives
npgraph if years<25
* Optimal h = .3799008
** Plot 1: Cross Validation h = 0.3799008 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote y_hat1 if years<25, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Nadaraya-Watson") label(4 "Probit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote y_hat1 if years<25, degree(0) bw(0.3799008) color("`color1'") alcolor(%10) fintensity(10) ///
	|| line y_prob1 y_hat1 if years<25, sort color("`color2'") 
gr export figure9_1.png, as(png)  replace

*** HETPROBIT ***
*****************
hetprobit vote $covariates if years<25, het(years)  
predict hety_hat1, xb
predict hety_prob1, p
npregress kernel vote hety_hat1 if years<25, estimator(constant) noderivatives
npgraph
* Optimal h = .1877095
** Plot 1: Cross Validation h = 0.1877095 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote hety_hat1 if years<25, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Nadaraya-Watson") label(4 "HetProbit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote hety_hat1 if years<25, degree(0) bw(0.1877095) color("`color1'") alcolor(%10) fintensity(10) ///
	|| line hety_prob1 hety_hat1 if years<25, sort color("`color2'") // saving(Figure1)
gr export figure9_2.png, as(png)  replace


*** KLEIN & SPADY ***
*********************
sml vote year logptax if years<25, offset(loginc)
predict ks_y_hat1, xb
npregress kernel vote ks_y_hat1 if years<25, estimator(constant) noderivatives
npgraph
* Optimal h = .3605576
** Plot 1: Cross Validation h = 0.3605576 **
colorpalette viridis, n(3) nograph
return list
local color1 = r(p1)
local color2 = r(p2)
local color3 = r(p3)
scatter vote ks_y_hat1 if years<25, mcolor(black) msymbol(circle_hollow) ///
	legend( pos(6) row(1) label(1 "Vote") label(3 "Klein & Spady") label(4 "Probit")  ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote")  ///
	yscale(range(-0.5(0.5)1.5)) ylabel(-0.5(0.5)1.5) ///
	|| lpolyci vote ks_y_hat1 if years<25, degree(0) bw(0.3605576) color("`color1'") alcolor(%10) fintensity(10)

	**|| line y_prob1 y_hat1 if years<25, sort color("`color2'") 
gr export figure9_3.png, as(png)  replace

*--------------------------------------------------------------------*
*--Question 2.10. NONPARAMETRIC REGRESSION: LPOLY NADARAYA WATSON  --*
*--------------------------------------------------------------------*
*** PROBIT IS THE CHOOSED MODEL ***
probit vote $covariates if years<25
predict y_hat2 if years<25, xb
predict y_prob2 if years<25, p

***
sum y_hat2 if years<25, detail
kdens y_hat2 

**Nadaraya-Watson**
npregress kernel vote y_hat2 if years<25, estimator(constant) noderivatives
npgraph if years<25, addplot(line y_prob2 y_hat2, sort lcolor(blue))
* Constant: Optimal h = 0.3806142

** Local-Linear**
npregress kernel vote y_hat2 if years<25, estimator(linear) noderivatives
npgraph if years<25, addplot(line y_prob2 y_hat2, sort lcolor(blue))
* Linear: Optimal h = 0.7306142

** Quadratric**
lpoly vote y_hat2 if years<25, degree(2) ci mcolor(black) lcolor("122 209 81") msymbol(circle_hollow) legend( pos(6) row(1) ) ///
	addplot((line y_prob2 y_hat2 if years<25, sort color (black)))
return list
* Quadratic: Optimal h = 0.804374

*------------------*
*-- FINAL FIGURE --*
*------------------*
line y_prob2 y_hat2 if years<25, sort ///
	color(black) legend( pos(6) row(2) ///
	label(3 "Nadaraya-Watson") label(4 "Locar Linear") label(5 "Local Quadratic") label(6 "Density") ) ///
	title("") xtitle("Linear Prediction") ytitle("Vote") xscale(range(-2(0.5)2)) xlabel(-2(0.5)2)  ///
	|| ( scatter vote y_hat2 if years<25, mcolor(black) msymbol(circle_hollow)  ) ///
	|| ( lpoly vote y_hat2 if years<25, degree(0) bw(0.3806142) lpattern (dash_dot) lcolor("68 1 84") ) ///
	|| ( lpoly vote y_hat2 if years<25, degree(1) bw(0.7306142) lpattern (dash_dot) lcolor("42 120 142") ) ///
	|| ( lpoly vote y_hat2 if years<25, degree(2) bw(0.804374) lpattern (dash_dot) lcolor("122 209 81") ) ///
	|| (  kdens y_hat2 if y_hat2>-1.7 & y_hat2<1.7, color("34 168 132") bw(0.6) /// 
	lcolor(%25) lpattern(dash) xscale(range(-2(0.5)2)) xlabel(-2(0.5)2) )
gr export figure7.png, as(png)  replace


