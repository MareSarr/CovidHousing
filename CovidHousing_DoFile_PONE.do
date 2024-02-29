******************************************************************************************
** Association of Race/Ethnicity and Severe Housing Problems with COVID-19 Deaths in the United States: Analysis of the First Three Waves **
******************************************************************************************

*************************** Figure 1: Time Series Moving Average Daily Deaths ***********

use "MA_COVID_PONE.dta"

tsset date, daily
twoway (tsline deaths_ma7), ///
    xtitle("") ///
    ytitle("COVID-19 Deaths Count", size(3)) ///
    title("{bf} 7-day Moving Average COVID-19 Deaths Count", size(4)) ///
	graphregion(fcolor(white) margin(3 3 10 0))

clear
exit

*****************************************************************************************

***************************        REGRESSION TABLES   **********************************

*****************************************************************************************

use "COVIDHousingData_PONE.dta"

** SES & Risk exposure  -- Standardized Variables
global zsrace zsblack_rate zshisp_rate zsaian_rate zsaapi_rate
global zshouspb zshousingpbs 
global zsdemo zsover65 zsfemale 
global zssocioecon zsmedhhinc zshighsch
global zshealth_risk zsuninsured_rate zsanycondition_prevalence zslogageadjdeath
global zsgeo zsrural_rate zsper_gop2016 zspopdensity2019 

*** Non-standardized variables
global race black_rate hisp_rate aian_rate aapi_rate
global houspb housingpbs 
global demo over65 female 
global socioecon medhhinc highsch
global health_risk uninsured_rate anycondition_prevalence logageadjdeath 
global geo per_gop2016 rural_rate popdensity2019 


**************************************************************************************
*** Table 3: Note that zero-inflated stage for Wave1 and Wave2 is not reported in Table 3. Note that we report the full 2 stages with the zero-inflated stage for Wave1 and Wave2 as Supplementary Material S1 Table **
**************************************************************************************

**** Panel A: Incidence-Rate Ratios (IRR) ****
nbreg death_31Mar2021  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
eststo ytdmarch
zinb wave1  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
eststo wave1
zinb wave2  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if wave2>=0 & state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
eststo wave2
nbreg wave3  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate)$zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if wave3>=0 & state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
eststo wave3
esttab using Waves_MainReg.tex, varwidth(10) ///
nobaselevels interaction(" $\times$ ")style(tex) replace ///
ci(2) b(2) r2 constant sfmt(%6.4f) scalars("N Observations" "pr2 Pseudo-R2" "chi2_cp Test alpha" "FE StateFixedEffect" )  ///
title(Negative binomial Regression\label{Housing Problems YTD & Waves Deaths}) star(* 0.10 ** 0.05 *** 0.01) ///
mtitles("YTD March 31 2021" "Wave 1" "Wave 2" "Wave 3") eform drop(*.state50)   
eststo clear

**** Panel B: Average Marginal Effect
** Poole sample
nbreg death_31Mar2021  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Average marginal effect
margins, dydx(*)

**Wave 1
zinb wave1  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect
margins, dydx(*)

** Wave 2
zinb wave2  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if wave2>=0 & state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect
margins, dydx(*)

* Wave 3
nbreg wave3  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate)$zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if wave3>=0 & state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Average marginal effect
margins, dydx(*)


******************************************
**** Table 2: Descriptive Statistics *****
******************************************

tabstat death_31Mar2021 wave1 wave2 wave3, statistics(count p50 p25 p75 iqr), if e(sample)==1	

logout, save("SumstatCovHous") tex replace: sum $race $houspb $demo $socioecon $health_risk $geo if e(sample)==1



*********************************************************************************
** Table 4: Marginal Effect of Housing Quality at mean racial/ethnic composition *********************************************************************************

**For the pooled sample and each wave, run the negative binomial regression and then only report the Average marginal effect of Housing problems at mean racial composition

**Poole sample
nbreg death_31Mar2021  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Average marginal effect of Housing problems at mean racial composition to be reported in Table 4
margins, dydx(zshousingpbs) at(zsblack_rate=0 zshisp_rate=0 zsaian_rate=0 zsaapi_rate=0)

**Wave 1
zinb wave1  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect of Housing problems at mean racial composition to be reported in Table 4
margins, dydx(zshousingpbs) at(zsblack_rate=0 zshisp_rate=0 zsaian_rate=0 zsaapi_rate=0)

* Wave 2
zinb wave2  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if wave2>=0 & state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect of Housing problems at mean racial composition to be reported in Table 4
margins, dydx(zshousingpbs) at(zsblack_rate=0 zshisp_rate=0 zsaian_rate=0 zsaapi_rate=0)

* Wave 3
nbreg wave3  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate)$zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if wave3>=0 & state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Average marginal effect of Housing problems at mean racial composition to be reported in Table 4
margins, dydx(zshousingpbs) at(zsblack_rate=0 zshisp_rate=0 zsaian_rate=0 zsaapi_rate=0)



************************************************
******************* Figure 2 *******************
************************************************

** Poole sample
nbreg death_31Mar2021  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Average marginal effect of race at different values of Housing problems
margins, dydx(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate) at(zshousingpbs=(-1.717873 -1.281284 -1.062989 -.6263992 -.1898095 0 .465075 1.11996 1.774844 2.866318)) vsquish
* Graph Marginal Effect of race at different values of Housing problems
marginsplot, allsimplelabels ///
recast(line) ciopt(color(%15)) recastci(rarea)  ///
xlabel(-1.717873 "P1" -1.281284 "P5" -1.062989 "P10" -.6263992 "P25" -.1898095 "P50" 0 "   Mean" .465075 "P75" 1.11996 "P90" 1.774844 "P95" 2.866318 "P99", labsize(vsmall)) ///
ylabel(, angle(horizontal) labsize(vsmall)) ///
ytitle(" {&Delta}Predicted deaths per 100,000 people", size(small) margin(small) ) ///
xtitle("Percentiles & Mean Poor Housing Conditions", size(small)) ///
title(" ", span margin(medium)) /// 
legend(order(1 "Blacks" 2 "Hispanics" 3 "AIAN" 4 "AAPI") rows(1) ///
position(12) ring(0) size(*.7) region(lwidth(none))) ///
graphregion(fcolor(white) margin(3 3 10 0)) ///
name(Fig_MEALL, replace)
graph save Fig_MEALL.gph, replace
graph use "Fig_MEALL.gph", play("Fig_ME_CI.grec") 
graph save Fig_MEALL.gph, replace 

**Wave 1
zinb wave1  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect of race at different values of Housing problems
margins, dydx(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate) at(zshousingpbs=(-1.717873 -1.281284 -1.062989 -.6263992 -.1898095 0 .465075 1.11996 1.774844 2.866318)) vsquish
* Graph Marginal Effect of race at different values of Housing problems
marginsplot, allsimplelabels ///
recast(line) ciopt(color(%15)) recastci(rarea)  ///
xlabel(-1.717873 "P1" -1.281284 "P5" -1.062989 "P10" -.6263992 "P25" -.1898095 "P50" 0 "   Mean" .465075 "P75" 1.11996 "P90" 1.774844 "P95" 2.866318 "P99", labsize(vsmall)) ///
ylabel(, angle(horizontal) labsize(vsmall)) ///
ytitle(" {&Delta}Predicted deaths per 100,000 people", size(small) margin(small) ) ///
xtitle("Percentiles & Mean Poor Housing Conditions", size(small)) ///
title(" ", span margin(medium)) /// 
legend(order(1 "Blacks" 2 "Hispanics" 3 "AIAN" 4 "AAPI") rows(1) ///
position(12) ring(0) size(*.7) region(lwidth(none))) ///
graphregion(fcolor(white) margin(3 3 10 0)) ///
name(Wave1ME_b, replace)
graph save Wave1ME_b.gph, replace
graph use "Wave1ME_b.gph", play("Fig_ME_CI.grec") 
graph save Wave1ME_b.gph, replace 

* Wave 2
zinb wave2  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if wave2>=0 & state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect of race at different values of Housing problems
margins, dydx(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate) at(zshousingpbs=(-1.717873 -1.281284 -1.062989 -.6263992 -.1898095 0 .465075 1.11996 1.774844 2.866318)) vsquish
* Graph Marginal Effect of race at different values of Housing problems
marginsplot, allsimplelabels ///
recast(line) ciopt(color(%15)) recastci(rarea)  ///
xlabel(-1.717873 "P1" -1.281284 "P5" -1.062989 "P10" -.6263992 "P25" -.1898095 "P50" 0 "   Mean" .465075 "P75" 1.11996 "P90" 1.774844 "P95" 2.866318 "P99", labsize(vsmall)) ///
ylabel(, angle(horizontal) labsize(vsmall)) ///
ytitle(" {&Delta}Predicted deaths per 100,000 people", size(small) margin(small) ) ///
xtitle("Percentiles & Mean Poor Housing Conditions", size(small)) ///
title(" ", span margin(medium)) /// 
legend(order(1 "Blacks" 2 "Hispanics" 3 "AIAN" 4 "AAPI") rows(1) ///
position(12) ring(0) size(*.7) region(lwidth(none))) ///
graphregion(fcolor(white) margin(3 3 10 0)) ///
name(Wave2ME_b, replace)
graph save Wave2ME_b.gph, replace
graph use "Wave2ME_b.gph", play("Fig_ME_CI.grec") 
graph save Wave2ME_b.gph, replace 

* Wave 3
nbreg wave3  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate)$zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if wave3>=0 & state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Marginal effect of race at different values of Housing problems
margins, dydx(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate) at(zshousingpbs=(-1.717873 -1.281284 -1.062989 -.6263992 -.1898095 0 .465075 1.11996 1.774844 2.866318)) vsquish
* Graph Marginal Effect of race at different values of Housing problems
marginsplot, allsimplelabels ///
recast(line) ciopt(color(%15)) recastci(rarea)  ///
xlabel(-1.717873 "P1" -1.281284 "P5" -1.062989 "P10" -.6263992 "P25" -.1898095 "P50" 0 "   Mean" .465075 "P75" 1.11996 "P90" 1.774844 "P95" 2.866318 "P99", labsize(vsmall)) ///
ylabel(, angle(horizontal) labsize(vsmall)) ///
ytitle(" {&Delta}Predicted deaths per 100,000 people", size(small) margin(small) ) ///
xtitle("Percentiles & Mean Poor Housing Conditions", size(small)) ///
title(" ", span margin(medium)) /// 
legend(order(1 "Blacks" 2 "Hispanics" 3 "AIAN" 4 "AAPI") rows(1) ///
position(12) ring(0) size(*.7) region(lwidth(none))) ///
graphregion(fcolor(white) margin(3 3 10 0)) ///
name(Wave3ME_b, replace)
graph save Wave3ME_b.gph, replace
graph use "Wave3ME_b.gph", play("Fig_ME_CI.grec") 
graph save Wave3ME_b.gph, replace 



******************************************************************************************
******************************************************************************************


************S2 Table: ROBUSTNESS **********************
********* Panel A: Incidence-Rate Ratios (IRR) ***********
zinb surge1  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
eststo surge1
zinb surge2  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if surge2>=0 & state!="Alaska" & state!="Wyoming", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
eststo surge2
nbreg surge3  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate)$zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if surge3>=0 & state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
eststo surge3
esttab using Waves_MainRegS2.tex, varwidth(10) ///
nobaselevels interaction(" $\times$ ")style(tex) replace ///
ci(2) b(2) r2 constant sfmt(%6.4f) scalars("N Observations" "pr2 Pseudo-R2" "chi2_cp Test alpha" "FE StateFixedEffect" )  ///
title(Negative binomial Regression\label{Housing Problems YTD & Waves Deaths}) star(* 0.10 ** 0.05 *** 0.01) ///
mtitles("Wave 1" "Wave 2" "Wave 3") eform drop(*.state50)   
eststo clear


********* Panel B: Average Marginal Effect ********
** Run the neg binomial regression with surge1 and then run the average marginal effect command and report only the results from the AME in Panel B. Do the same for surge2 and surge3.  

**Surge 1
zinb surge1  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if state!="Alaska", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect to be reported in Panel B
margins, dydx(*)

* Surge 2 
zinb surge2  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate) $zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50  if surge2>=0 & state!="Alaska" & state!="Wyoming", irr inflate(zsblack_rate zshisp_rate zsaian_rate zsaapi_rate zsover65 zshighsch zsuninsured_rate zsrural_rate zspopdensity2019) offset(logpop_jhs) forcevuong zip
* Average marginal effect to be reported in Panel B
margins, dydx(*)

* Surge 3
nbreg surge3  c.zshousingpbs##(c.zsblack_rate c.zshisp_rate c.zsaian_rate c.zsaapi_rate)$zsdemo $zssocioecon $zshealth_risk $zsgeo i.state50 if surge3>=0 & state!="Alaska", irr offset(logpop_jhs) vce(cluster state50)
* Average marginal effect to be reported in Panel B
margins, dydx(*)



