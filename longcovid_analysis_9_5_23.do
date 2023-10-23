**For all positvie patients from 10/1/2021, included mutiple infection

*100 Data for analysis 
*1000  table 1
*1100  table 1 row percent

*1500 uni logistic model
*1600 multiple logistic model
*2000 plot univ 
*2100 plot multp logistic model
*2500 plot univ, exclude death <28 days
*2600 plot multp logistic model, exclude death <28 days
*3000 prevalence 




*100 Data for analysis 
cd  "P:\PI\long covid 2"
use long_covid_table1_edit_9_5_23, clear
replace inc=0 if index_dnum < date("2021-08-01", "YMD", 2010)

merge 1:1 scrssn using 1st_u99 // merge with outcome file
drop if _merge==2  //keep 560706
drop if exclude==1 // 27 before 10/1/2023

*replace inc=0 if recentpositive ~=firstpositive

encode ventilator60d, gen(vent_60d)
encode hospitaliza~60d , gen(hospital_60d)

gen t_u09=d_longcovid1 - index_dnum // follow-up dates from infection
drop _merge

/*
cd "P:\ORD_Jeyapalina_202006079D\long covid"
import delimited long_covid_all_positive_vaccine.csv,  clear varnames(1) delim("|")
*/

gen type1="MODERNA" if regexm(series1vaccinetype, "MODERNA")
replace type1="PFIZER" if regexm(series1vaccinetype, "PFIZER")
replace type1="JANSSEN" if regexm(series1vaccinetype, "JANSSEN")
replace type1="Unknown" if type1=="" & series1vaccinetype ~=""


gen type2="MODERNA" if regexm(series2vaccinetype, "MODERNA")
replace type2="PFIZER" if regexm(series2vaccinetype, "PFIZER")
replace type2="JANSSEN" if regexm(series2vaccinetype, "JANSSEN")
replace type2="Unknown" if type2=="" & series2vaccinetype ~=""



//vaccine date
gen  date_1=date(series1date, "YMD",2010)
gen  date_2=date(series2date, "YMD",2010)
format  date_1 date_2 %td
su date_1 date_2, f d
replace date_1=. if date_1 <date("2019-02-01", "YMD", 2010) // drop 28
replace date_2=. if date_2 <date("2019-02-01", "YMD", 2010) // drop 28


/*
keep if date_1~=. | date_2~=.

merge 1:1 patienticn using long_covid_all_positive_343922 // only 1 infected
*/
gen dose=0
replace dose=1 if type1 ~="" | type2~=""
replace dose=2 if type1 ~="" & type2~=""

ta type1 type2, m

gen type=0
replace type=1 if type1=="MODERNA" & type2=="MODERNA"
replace type=2 if type1=="PFIZER" & type2=="PFIZER"
replace type=3 if type1=="JANSSEN" | type2=="JANSSEN"
replace type=4 if (type1~="" | type2~="") &  type==0
lab def type 0 "No vaccine" 1 "2 Dose of Moderna" 2 "2 Dose of Pfizer" 3 "1 Dose of Janssen" 4 "1 or 2 Dose of Moderan / Pfizer /Unknown ", replace
lab val type type
ta type

gen d_index=date(indexdate, "YMD",2010)
format  d_index  %td

capture drop break
gen break=99 
replace break=1 if (d_index<date_1) & (date_1~=. & d_index~=.)  // before vaccine
replace break=2   if d_index>=date_1 & d_index~=. & date_1~=.  & d_index < date_2 & date_2~=. & regexm(type1, "JANSSEN")~=1 // between 1st and 2nd
replace break=3 if d_index>=date_1 & d_index~=. & date_1~=.  & date_2==. & regexm(type1, "JANSSEN")~=1 // with 1st but missing 2nd
replace break=4 if d_index>=date_1 & d_index~=. & regexm(type1, "JANSSEN")==1 	// after 1st for Janssen
replace break=4 if d_index>= date_2 & d_index~=. & date_2~=.  & regexm(type1, "JANSSEN")~=1 // after 2nd for not Janssen
replace break=5 if d_index>=date_1 +14 & d_index~=. & ((date_1~=.& date_2==. & regexm(type1, "JANSSEN")==1 ) | (date_2~=. & date_1==.  & regexm(type2, "JANSSEN")==1 )) // 14-60
replace break=5 if d_index>=date_2 +14 & d_index~=. & date_1~=. & date_2~=. & (regexm(type1, "JANSSEN")~=1 |regexm(type2, "JANSSEN")~=1 )
lab def break 99 "Positve unvaccinded" 1 "Positve before vaccinated" 2 "Positve between 1st and 2nd dose" 3 "Positve missed 2nd dose" 4 "Positve within 14D vaccinated" 5 "Positve after 14D vaccinated" , replace
lab val break break
ta break


capture drop break3
gen break3=1 if break<=4 
replace break3=2 if break==5
replace break3=3 if break==99
lab def break3 1 "Partitially vaccinated" 2 "Full vaccined" 3 "Unvaccinded", replace
lab val break3 break3
lab var break3 "Vaccine status"
ta break3

ren break vaccine_type1
ren break3 vaccine_type2


rename indexcasesta3n  sta3n

merge m:m sta3n using "P:\ORD_Jeyapalina_202006079D\long covid\sta3n"  // VISN 
drop if  _merge ==2
drop _merge
*keep if firstpositive ==recentpositive
*there is on scrrn with 2 patienticn
sort scrssn indexdate
bysort scrssn: gen aaa=_n
drop if aaa==2


gen age6=1 if ageatindexdate >=18 & ageatindexdate <=40
replace age6=2 if ageatindexdate >40 & ageatindexdate <=50
replace age6=3 if ageatindexdate >50 & ageatindexdate <=60
replace age6=4 if ageatindexdate >60 & ageatindexdate <=70
replace age6=5 if ageatindexdate >70 & ageatindexdate <=80
replace age6=6 if ageatindexdate >80 & ageatindexdate ~=.
tabstat ageatindexdate, by(age6) s(n min max)
lab  def age6 1 "18 to 40" 2 " 41 to 50" 3 "51 to 60" 4 "61 to 70" 5 "71 to 80" 6 "≥81", replace
lab val age6 age6
ta age6

gen multi_infection=0
replace multi_infection =1 if firstpositive ~=recentpositive

gen race3=1 if race_num==6
replace race3=2 if race_num==3
replace race3=3 if race_num==1 | race_num==2 | race_num==4
replace race3=4 if race_num==5 | race_num==.
lab  def race3 1 "White" 2"Black" 3 "Other race" 4 "Unknown" , replace
lab val race3 race3
ta race3

*recode race_num (7=0) (5=99) (4=99)
drop race_e
gen race_ethn=race_num
replace race_ethn=0 if race_num==6 //white
replace race_ethn=5 if  race_num==4 //unknow and hawaiian
replace race_ethn=4 if ethnicity=="Hispanic or Latino" | ethnicity=="HISPANIC OR LATINO"   //hispanic or latino
lab  def race_ethn 0 "White" 1 "American Indian" 2 "Asin" 3 "Black" 4 " Hispanic or Latino" 5 "Others" , replace
lab val race_ethn race_ethn
ta race_ethn


// comobidty
su diabetesany2y ckd2y
foreach y in  pulmonaryfibrosis2y asthma2y chf2y actmi2y cerebrovasc2y pad2y vte2y osa2y ohs2y  mdd2y ptsd2y bipolar2y {
	encode `y'rs, ge(`y')
	replace `y'=`y'-1
	su `y'
}


// medications
foreach y in opioid2y antipsychotic2y statin2y aceinhibitor2y arb2y  ccb2y {
		encode `y'rs, ge(`y')
	replace `y'=`y'-1
	su `y'
}




//missing values values


foreach y in ckd2y liver2y htn2y diabetesany2y copd2y {
	replace `y'=0 if `y'==.
}


encode rurality, gen(rural)
replace rural=99 if rural==.

replace region5=3 if region5==. // set missing to others
replace region5=9 if region5==3
replace region5=0 if region5==6
lab def region5 0 "West" 9 "Others/Unknown",  modify
lab val region5 region5


gen male=0
replace male=1 if sex=="M"

replace vaccine_type2=0 if vaccine_type2==2
lab def vaccine_type2 0 " Full Vaccined" 1 "Partitially vaccinated" 3 "Unvaccinded", replace 
lab val vaccine_type2 vaccine_type2
ta vaccine_type2

// treatment

foreach y in hospitalization ventilator noninvasiveventilation icu ecmo oxygenlowflow oxygenhighflow antibiotic antiviral  dexamethasone remdesivir ribavirin corticosteroid {
	capture drop `y'_60d
	gen `y'_60d=0 if `y'60d=="False"
	replace `y'_60d=1 if `y'60d=="True"
	replace `y'_60d=0 if `y'_60d==.
	ta `y'_60d
}

// ventilator_60d and noninvasivevent
gen vent_or_non_invasive_60d =max(ventilator_60d,noninvasiveventilation_60d )
gen oxygen_therapy_60d=max(ecmo_60d, oxygenlowflow_60d, oxygenhighflow_60d )
//combine hosp and icu
gen hospital_icu_60d=max(hospitalization_60d, icu_60d)

encode smoke2yrs , gen(smoke_2y)

save long_covid_editanalysis_9_5_23, replace //******************************************


*1000  table 1
cd "P:\PI\long covid 2"
use long_covid_editanalysis_9_5_23, clear
replace inc=0  if  d_index <date("2021-10-01", "YMD", 2021) // 548970
replace inc=0.5 if d_longcovid1 < d_index
keep  if inc==1

drop  if t_death <28 & evt_death==1 // drop N=7937, n=478,900
gen female=1-male
ta any_u09
su t_u09, d

*keep if flow_low==2
local fm  %9.1f
local fm2 %9.1f
#delimit ;
 table1_mc, by(any_u09) onecol total(before) 
 vars( 
	\age6				cat 	`fm2'	
	\female   			cat 	`fm2'
	\race_ethn	 			cat  	`fm2'  
	\region5             cat      `fm2'	
	\rural				cat 	`fm2'
	\bmi_5n  				cat 	`fm2'  
	\smoke_2y  				cat 	`fm2'  
	\cci2  				cat 	`fm2'  
	\ckd2y			bin 	`fm2'  
	\liver2y				bin		`fm2'	
	\htn2y		bin 	`fm2'		
	\diabetesany2y       bin 	`fm2' 
	\copd2y       bin 	`fm2' 
	\multi_infection    bin     `fm2'
	\vaccine_type2 		cat 	`fm2'
	\hospital_icu_60d 	bin		`fm2'	
	\vent_or_non_invasive_60d				bin 	`fm2'
	\oxygen_therapy_60d 				bin		`fm2'	
	\antibiotic_60d				bin 	`fm2'	
	\antiviral_60d             bin      `fm2'	
	\corticosteroid_60d             bin      `fm2' 
*/

 ) extraspace clear
;
#delimit cr
save table1_bbb, replace



cd "P:\PI\long covid 2"
use table1_bbb, clear // 28 death >28
putdocx begin,  font(arial, 11, black)  land
putdocx paragraph
*putdocx text ("Tables"), bold  font(arial, 15, cadetblue) linebreak
putdocx paragraph
putdocx text ("Table 1. Long covid"), bold  linebreak
putdocx table tbl1 = data("factor   any_u09_T  any_u09_0  any_u09_1    pvalue"),   ///
        border(start, nil) border(insideV, nil) border(end, nil) ///
note("") 

putdocx save "P:\PI\outcome\long_covid_description_9_21_23.docx", replace



*200 table 1 row percertage

cd "P:\PI\long covid 2"
use long_covid_editanalysis_9_5_23, clear
replace inc=0  if  d_index <date("2021-10-01", "YMD", 2021) // 548970
replace inc=0.5 if d_longcovid1 < d_index
keep  if inc==1

drop  if t_death <28 & evt_death==1 // drop N=7937, n=478,900

gen female=1-male

 ************************repeast discritive papaer - code: 
local fm  %9.1f
local fm2 %9.1f
#delimit ;
 table1_mc, by(any_u09) onecol total(before) catrowperc
 vars( 
	\age6				cat 	`fm2'	
	\female   			cat 	`fm2'
	\race_ethn	 			cat  	`fm2'  
	\region5             cat      `fm2'	
	\rural				cat 	`fm2'
	\bmi_5n  				cat 	`fm2'  
	\smoke_2y  				cat 	`fm2'  
	\ckd2y			cat 	`fm2'  
	\liver2y				bin		`fm2'	
	\htn2y		cat 	`fm2'		
	\diabetesany2y       cat 	`fm2' 
	\copd2y       cat 	`fm2' 
	\multi_infection    cat     `fm2'
	\vaccine_type2 		cat 	`fm2'
	\hospital_icu_60d 	cat		`fm2'	
	\vent_or_non_invasive_60d				cat 	`fm2'
	\oxygen_therapy_60d 				cat		`fm2'	
	\antibiotic_60d				cat 	`fm2'	
	\antiviral_60d             cat      `fm2'	
	\corticosteroid_60d             cat      `fm2' 

 ) extraspace clear
;
#delimit cr

*save table1_row, replace
save table1_row_bbb, replace

cd "P:\PI\long covid 2"
use table1_row, clear
putdocx begin,  font(arial, 11, black)  land
putdocx paragraph
*putdocx text ("Tables"), bold  font(arial, 15, cadetblue) linebreak
putdocx paragraph
putdocx text ("Table 1 rowpercent. Long covid"), bold  linebreak
putdocx table tbl1 = data("factor   any_u09_T  any_u09_0  any_u09_1    pvalue"),   ///
        border(start, nil) border(insideV, nil) border(end, nil) ///
note("") 

putdocx save "P:\PI\outcome\long_covid_description_9_21_23.docx", append


*1500 uni logistic
*********************************************************************************************
cd "P:\PI\long covid 2"
use long_covid_editanalysis_9_5_23, clear
replace inc=0  if  d_index <date("2021-10-01", "YMD", 2021) // 548970
replace inc=0.5 if d_longcovid1 < d_index
keep  if inc==1
drop  if t_death <28 & evt_death==1 // drop N=7937

gen female=1-male
set showbaselevels on

su ib1.age6 ib0.female ib0.race_ethn  ib0.region5 ib1.rural ib2.bmi_5n ib3.smoke_2y ckd2y ib0.cci2 liver2y htn2y diabetesany2y copd2y

local i=1

foreach y in ib1.age6 ib0.female ib0.race_ethn  ib0.region5 ib1.rural ib2.bmi_5n ib3.smoke_2y ckd2y /*ib0.cci2*/ liver2y htn2y diabetesany2y copd2y  ///
/*ib2.ast_4a ib2.wbc_4a ib1.proc_4a*/ multi_infection ib0.vaccine_type2 hospital_icu_60d  vent_or_non_invasive_60d ///
oxygen_therapy_60d antibiotic_60d antiviral_60d corticosteroid_60d {
tempfile tf`i'
parmby "logistic any_u09 `y'  ", lab format(estimate min95 max95 %8.4f p%6.3f) saving(`"`tf`i''"', replace) idn(`i') ids(`y')
local ++i
}


drop _all
forv k=1(1)`=`i'-1' {
	append using `"`tf`k''"'
	}
	
drop if regexm(parm, "_con")==1
//keep if parmseq < 5
replace est=exp(est)
replace min=exp(min)
replace max=exp(max)
gen HR=string(est, "%6.2f")+" (" +string(min, "%6.2f") +", " + string(max, "%6.2f") +")"
replace HR = "Reference" if stderr==0
gen pc=string(p, "%4.2f") if p>0.05
replace pc=string(p, "%4.3f") if p<=0.05 & p>=0.001
replace pc="<0.001" if p<0.001
replace pc="" if stderr==0
ren idnum Model 
ren idstr Outcome
ren parm Predictor
save logistic_u09_univ, replace
keep Pred label HR pc


*1600 multiple
*********************************************************************************************
cd "P:\PI\long covid 2"
use long_covid_editanalysis_9_5_23, clear
replace inc=0  if  d_index <date("2021-10-01", "YMD", 2021) // 548970
replace inc=0.5 if d_longcovid1 < d_index
keep  if inc==1
drop  if t_death <28 & evt_death==1 // drop N=7937

gen female=1-male

set showbaselevels on

local i=1
/*
tempfile tf`i'
parmby "xi: logistic any_u09 ib1.age6 ib0.female ib0.race_ethn  ib0.region5 ib1.rural ib2.bmi_5n ib3.smoke_2y ckd2y ib0.cci2 liver2y htn2y diabetesany2y copd2y  multi_infection ib0.vaccine_type2 hospital_icu_60d  vent_or_non_invasive_60d oxygen_therapy_60d antibiotic_60d antiviral_60d corticosteroid_60d ", lab format(estimate min95 max95 %8.4f p%6.3f) saving(`"`tf`i''"', replace) idn(`i') ids("mutipl")
local ++i
*/

tempfile tf`i'
parmby "xi: logistic any_u09 ib1.age6 ib0.female ib0.race_ethn  ib0.region5 ib1.rural ib2.bmi_5n ib3.smoke_2y ckd2y liver2y htn2y diabetesany2y copd2y  multi_infection ib0.vaccine_type2 hospital_icu_60d  vent_or_non_invasive_60d oxygen_therapy_60d antibiotic_60d antiviral_60d corticosteroid_60d ", lab format(estimate min95 max95 %8.4f p%6.3f) saving(`"`tf`i''"', replace) idn(`i') ids("mutipl")
local ++i



drop _all
forv k=1(1)`=`i'-1' {
	append using `"`tf`k''"'
	}
	
drop if regexm(parm, "_con")==1
//keep if parmseq < 5
replace est=exp(est)
replace min=exp(min)
replace max=exp(max)
gen HR=string(est, "%6.2f")+" (" +string(min, "%6.2f") +", " + string(max, "%6.2f") +")"
replace HR = "Reference" if stderr==0
gen pc=string(p, "%4.2f") if p>0.05
replace pc=string(p, "%4.3f") if p<=0.05 & p>=0.001
replace pc="<0.001" if p<0.001
replace pc="" if stderr==0
ren idnum Model 
ren idstr Outcome
ren parm Predictor
save logistic_u09_mutiple, replace
keep Pred label HR pc





*2000 plot
 ************************repeast discritive papaer - code: 
 cd "P:\PI\plot"
 use u09_logistic, replace  // run univ and mutipl logistic model 
 drop if _n<=2
 drop or2 pvalue2
 
 
 /*
 replace pc1="0.20" if pc1=="0.2"
 replace pc2="1.00" if pc2=="1"
 replace pc2="0.80" if pc2=="0.8"
 */
*ren idnum Model 
ren pred Predictor


// draw figure for age, comorb conditons
*drop if _n>=6  & _n<=19  
gen order=_n
gen estimate=substr(or1, 1, 4)
replace estimate="1" if estimate=="Refe"
gen min95=substr(or1, 7, 4)
replace min95="1" if min95=="nce"
gen max95=substr(or1, 13, 4)
replace max95="1" if or1=="Reference"
destring estimate min95 max95, replace
gen order_rev=_N-order+1

//adjust the est, looks better
replace estimate=0.105 if estimate<0.105 & regexm(Predictor,"Unknown")
gen min95_2=min95
replace min95_2=estimate-0.14 if max95-min95<0.29 & estimate >1 & estimate~=.
gen max95_2=max95
replace max95_2=estimate+0.14 if max95-min95<0.29 & estimate >1 & estimate~=.

	local panel1 "Dental implant failure (N=136,676)" 
	local panel2 "in a logistic regression model "
	
gen order2=order
	sum order2
local total=r(max)
	forv i=1(1)`=`total'' {
			local yaix_`=`total'-`i'+1'=order2[`i']
			*local yaix_var_`=`total'-`i'+1'=yaix_var[`i']			
			*local var_`=`total'-`i'+1'  =Lab[`i']
			local med_`=`total'-`i'+1'  =estimate[`i']
			local min_`=`total'-`i'+1'  =min95[`i']
			local max_`=`total'-`i'+1'  =max95[`i']
			*local p_`=`total'-`i'+1'    =pc[`i']

			*local i=`i'+1
	}
	
	gen yaix=`total'+1-order2
	gen yaix1=`total'+1-order2+0.0
	gen yaix2=`total'+1-order2-0.0
	local maxint=400
	local minint=0.003
	local size *0.8
	local shift1 0
	local ppost 4.8
	local plus=char(24)
	*local int_p = p_lrtest

gen xaix=`minint'+0.0001
gen xaix2=`minint'+0.001
gen xhr=3.9
gen xhrm=3.9+0
gen xpc=440
gen xc=0.01
gen xc1=0.21+0.07
#delimit ;
	twoway  (rcap  max95_2 min95_2 yaix  ,  horizontal  msize(*1.2) lwidth(*1) lcolor(black))	
			(scatter yaix estimate /*[w=weight]*/ ,   msymbol(circle) mlcolor(black) mfcolor(black) msize(*0.9) legend(off)) 
			/*(scatter yaix xaix  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.15) mlabel(head1) mlabposition(3) mlabc(black) legend(off)) 	*/
			(scatter yaix xaix2  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.2) mlabel(Pred) mlabposition(3) mlabc(black) legend(off)) 			
			(scatter yaix xhr  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.2) mlabel(or1) mlabc(black) legend(off))			
			(scatter yaix1 xhrm  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.2) mlabel(or1) mlabc(black) legend(off)) 
			(scatter yaix xpc  ,   msymbol(i) mlabposition(3) mlcolor(black) mfcolor(white) msize(*.5) mlabel(pvalue1)  mlabposition(9) mlabc(black) legend(off))			
									

			, legend(off)
	ytitle("" , size(*1.2))
	xtitle("{bf: Odds ratio of Long COVID by univariable model}" ,  height(10) size(*1.0) c(black))
	/*title("{bf:`panel1'}" /*"{bf:`panel2'}"*/, c(black) size(*0.8) ) */
 	xlabel(/*`=``y'minint''(2)`=``y'maxint''  */   4 "{bf:4}" 2 "{bf:2}"    1 "{bf:1}"  0.5 "{bf:0.5}" 
     , format(%9.0fc) labsize(*1.0) labc(black) labgap(1) tlength(small)  tlcolor(black)  nogrid )
      ylabel(0 "0", angle(horizontal) tlcolor(white) tlength(small)  labsize(small) labcolor(white) nogrid)
   yscale(lw(*1.5) lcolor(white) r(0 67.5) )  
   xline(1, lstyle(solid) lw(*1.5) lcolor(black) )
   xscale(titlegap(-3) lw(*2) lcolor(black) r(`minint' 90))  graphregion(fcolor(white) ) xscale(log)
   xsize(7) ysize(30)
   		/*text(57  `=`minint'-`shift1''		"{bf:Factors}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	*/
		text(67 3.5 	"{bf:OR(95% CI)}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)
		text(67 400	 "{bf:P value}", place(w) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
   		text(66  `=`minint'-`shift1''		"{bf:Age (Year)}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(59  `=`minint'-`shift1''		"{bf:Gender}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(56  `=`minint'-`shift1''		"{bf:Race}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
		text(49  `=`minint'-`shift1''		"{bf:Geographical region}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(42  `=`minint'-`shift1''		"{bf:Rurality}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(37  `=`minint'-`shift1''		"{bf:BMI groups(kg/m2)}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
		text(30  `=`minint'-`shift1''		"{bf:Smoking status}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
		text(25  `=`minint'-`shift1''		"{bf:Comorbidities}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(13  `=`minint'-`shift1''		"{bf:COVID-19 Positive  recode}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(11  `=`minint'-`shift1''		"{bf:Vaccine status}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	text(7  `=`minint'-`shift1''		"{bf:Treatment for COVID-19}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)			
				
	
	saving("P:\PI\plot\long_covid_uni_logistic.gph", replace)			 
  	;
	#delimit cr	   
	*P:\ORD_Jeyapalina_202006079D\long covid\plot.png", replace width(1200)
	graph export "P:\PI\plot\long_covid_uni_logistic.tif", replace width(3600)
	

*2100 plot for mulivariable model
 ************************repeast discritive papaer - code: 
  ************************repeast discritive papaer - code: 
 cd "P:\PI\plot"
 use u09_logistic, replace  // run univ and mutipl logistic model 
 drop if _n<=2
 drop or2 pvalue2 
 
 drop or1 pvalue1
*ren idnum Model 
ren pred Predictor

// draw figure for age, comorb conditons
*drop if _n>=6  & _n<=19  
gen order=_n
gen estimate=substr(or3, 1, 4)
replace estimate="1" if estimate=="Refe"
gen min95=substr(or3, 7, 4)
replace min95="1" if min95=="nce"
gen max95=substr(or3, 13, 4)
replace max95="1" if or3=="Reference"
destring estimate min95 max95, replace


gen order2=order
	sum order2
local total=r(max)
	forv i=1(1)`=`total'' {
			local yaix_`=`total'-`i'+1'=order2[`i']
			*local yaix_var_`=`total'-`i'+1'=yaix_var[`i']			
			*local var_`=`total'-`i'+1'  =Lab[`i']
			local med_`=`total'-`i'+1'  =estimate[`i']
			local min_`=`total'-`i'+1'  =min95[`i']
			local max_`=`total'-`i'+1'  =max95[`i']
			*local p_`=`total'-`i'+1'    =pc[`i']

			*local i=`i'+1
	}
	
	gen yaix=`total'+1-order2
	gen yaix1=`total'+1-order2+0.0
	gen yaix2=`total'+1-order2-0.0
	local maxint=400
	local minint=0.003
	local size *0.8
	local shift1 0
	local ppost 4
	local plus=char(24)
	*local int_p = p_lrtest

gen xaix=`minint'+0.0001
gen xaix2=`minint'+0.001
gen xhr=3.9
gen xhrm=3.9+0
gen xpc=440
gen xc=0.01
gen xc1=0.21+0.07
#delimit ;
	twoway  (rcap  max95_2 min95_2 yaix  ,  horizontal  msize(*1.2) lwidth(*1) lcolor(black))	
			(scatter yaix estimate /*[w=weight]*/ ,   msymbol(circle) mlcolor(black) mfcolor(black) msize(*0.9) legend(off)) 
/*			(scatter yaix xaix  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.15) mlabel(head2) mlabposition(3) mlabc(black) legend(off)) 	*/
			(scatter yaix xaix2  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.2) mlabel(Pred) mlabposition(3) mlabc(black) legend(off)) 			
			(scatter yaix xhr  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.2) mlabel(or3) mlabc(black) legend(off))			
			(scatter yaix1 xhrm  ,   msymbol(i) mlcolor(black) mfcolor(white) msize(*.2) mlabel(or3) mlabc(black) legend(off)) 
			(scatter yaix xpc  ,   msymbol(i) mlabposition(3) mlcolor(black) mfcolor(white) msize(*.5) mlabel(pvalue3)  mlabposition(9) mlabc(black) legend(off))			
									

			, legend(off)
	ytitle("" , size(*1.2))
	xtitle("{bf: Odds ratio of Long COVID by multivariable model}" ,  height(10) size(*1.0) c(black))
	/*title("{bf:`panel1'}" /*"{bf:`panel2'}"*/, c(black) size(*0.8) ) */
 	xlabel(/*`=``y'minint''(2)`=``y'maxint''  */   4 "{bf:4}" 2 "{bf:2}"    1 "{bf:1}"  0.5 "{bf:0.5}" 
     , format(%9.0fc) labsize(*1.0) labc(black) labgap(1) tlength(small)  tlcolor(black)  nogrid )
      ylabel(0 "0", angle(horizontal) tlcolor(white) tlength(small)  labsize(small) labcolor(white) nogrid)
   yscale(lw(*1.5) lcolor(white) r(0 67.5) )  
   xline(1, lstyle(solid) lw(*1.5) lcolor(black) )
   xscale(titlegap(-3) lw(*2) lcolor(black) r(`minint' 90))  graphregion(fcolor(white) ) xscale(log)
   xsize(7) ysize(30)
   			/*text(57  `=`minint'-`shift1''		"{bf:Factors}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	*/
		text(67 3.5 	"{bf:OR(95% CI)}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)
		text(67 400	 "{bf:P value}", place(w) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
   		text(66  `=`minint'-`shift1''		"{bf:Age (Year)}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(59  `=`minint'-`shift1''		"{bf:Gender}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(56  `=`minint'-`shift1''		"{bf:Race}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
		text(49  `=`minint'-`shift1''		"{bf:Geographical region}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(42  `=`minint'-`shift1''		"{bf:Rurality}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(37  `=`minint'-`shift1''		"{bf:BMI groups(kg/m2)}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
		text(30  `=`minint'-`shift1''		"{bf:Smoking status}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
		text(25  `=`minint'-`shift1''		"{bf:Comorbidities}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(13  `=`minint'-`shift1''		"{bf:COVID-19 Positive  recode}" , place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	   	text(11  `=`minint'-`shift1''		"{bf:Vaccine status}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)	
	text(7  `=`minint'-`shift1''		"{bf:Treatment for COVID-19}", place(e) size(`size') /*box*/ lw(*0) margin(t=0 b=0 l=0) /*lc(white) bc(white)*/)			
	saving("P:\PI\plot\long_covid_multi_logistic.gph", replace)			 
  	;
	#delimit cr	   
	*P:\ORD_Jeyapalina_202006079D\long covid\plot.png", replace width(1200)
	graph export "P:\PI\plot\long_covid_multi_logistic.tif", replace width(3600)
	
	
cd "P:\PI\plot"	
graph combine long_covid_uni_logistic.gph  long_covid_multi_logistic.gph, row(1) iscale(*.6) graphregion(fcolor(white)) xsize(4) ysize(5.5)
graph export long_covid_logistic_combine.tif , width(3600) replace


