**************************************
**************************************
*** Project: SEGAPEGO              ***
*** Author: GO, QR, RC             ***
*** Date: August 2023              ***
*** Topic: Descriptive analysis    ***
**************************************
**************************************


		
	* Open file
		clear
		cd "G:\Mi unidad\projets chili\SEG_APEG\code"	
		do "2. individual level variable creation.do"

	* Merge with variable on neighborhood NSE
		merge m:1 geocodigo year using "G:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\nse_barrio_vf.dta"
		drop _merge		
		destring idencuesta, replace
		
	* Generate quintile of neighborhood NSE
		egen quint_nse_barrio = xtile(nse_barrio), nq(5) by(year)

	* Generate quintile of individual NSE
		egen quint_nse_indiv = xtile(nse_indiv), nq(5) by(year)
		
	* Arrange variable sex to make it time-constant
		gen sex2 = .
		replace sex2 = sex if year==2016
		bys idencuesta : egen max_sex2 = max(sex2)
		label define max_sex2 1 "Men" 2 "Women"
		label val max_sex2 max_sex2
		drop sex sex2
		rename max_sex2 sex

	* Arrange variable married to make it time-constant
		gen married2 = .
		replace married2 = married if year==2016
		bys idencuesta : egen max_married2 = max(married2)
		label define max_married2 0 "Not married" 1 "Married"
		label val max_married2 max_married2
		drop married married2
		rename max_married2 married
	
*-------------------------------------------*
*-----      Descriptive statistics     -----*
*-------------------------------------------*
	
margins, at(educ=(1 2 3 4 5) nse_barrio=(.1 .9)  ) vsquish
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vsquish
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES")

*-------------------------------------------*
*-----      Pooled models              -----*
*-------------------------------------------*
	
	reg z_friends c.nse_barrio  avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimates store z_friends_m1

	reg z_friends c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimates store z_friends_m2
	
	reg z_friends c.nse_barrio##c.avg_isei  avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimates store z_friends_m3
	
*-------------------------------------------*
*-----      Fixed effect models        -----*
*-------------------------------------------*

xtset idencuesta year

	
	xtreg z_friends c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
	estimates store z_friends_m4
	
	xtreg z_altruistic c.nse_barrio##c.avg_isei sex age age_sq yr_address homeowner married  has_children i.year ,  fe
	estimates store z_friends_m5


		esttab z_friends_m1 z_friends_m2 z_friends_m3 z_friends_m4 z_friends_m5 using "G:\Mi unidad\projets chili\SEG_APEG\tables and figures\friends.rtf", ///
		starlevels(* 0.10 ** 0.05 *** 0.01) keep(nse_barrio avg_isei c.nse_barrio#c.avg_isei) stats(N r2) constant b(%5.3f) se(%5.2f) fonttbl(\f0\fnil Calibri; ) ///
		label nonumbers mtitles("Model 1" "Model 2" "Model 3" "Model 4 (FE)" "Model 5 (FE)") interaction("*")  varwidth(15) replace



		margins, at(nse_barrio=(0 .20 .40 .60 .80 1) avg_isei=(17 80)) noestimcheck
		marginsplot, ciopts(color(*.5)) recastci(rspike) graphregion(fcolor(white)) /// 
						  ci1opts(lcolor(dkorange)) ///
						  ci2opts(lcolor(dkgreen)) ///
						  plot1opts(msymbol(circle) mcolor(dkorange)) ///
						  plot2opts(msymbol(circle) mcolor(dkgreen)) ///
		legend(order(1 "Low social class" 2 "High social class")) xtitle("Neighborhood SES") level(90) yline(0, lcolor(black)) title("Altruistic dispositions. Average Marginal Effects", size(medlarge) color(black))
		
		gr export "G:\Mi unidad\projets chili\SEG_APEG\tables and figures\z_friends.png" ,  width(3000) replace





		
			xtreg z_justif_violence c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) noestimcheck
			marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90) yline(0) title("friends. Average Marginal Effects")

		
*----------------------------*
*----- POOLED OLS GRAPH -----*		
*----------------------------*

	* Cultural and relational
	reg z_identification c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m1

	reg z_friends c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m2

	reg z_size_network c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m3

	reg z_gen_trust c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m4

	reg z_trust_minorities c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m5
	
	reg z_trust_inst c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m6

	coefplot (m1, aseq("Identification")) (m2, aseq("Relationship")) (m3, aseq("Network size")) (m4, aseq("Gen. Trust")) (m5, aseq("Trust minorities")) (m6, aseq("Trust institutions")), keep(nse_barrio) yline(0, lcolor(black)) graphregion(color(white)) vertical aseq swapnames xtitle(Dependent variables) ytitle(Coefficients (std.)) xlabel(, angle(vertical)) legend(off) title(Pooled OLS. Emotional and Relational dimensions.)

	
	* Political and normative
      
	  	reg z_interest_pol c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m7

	reg z_satisf_demo c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m8

	reg z_conv_particip c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m9

	reg z_unconv_particip c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m10

	reg z_egalitarianism c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m11
	
	reg z_altruistic c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m12
	  
	 	reg z_prosoc_behave c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m13
	
		reg z_democracy_support c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m14
	
		reg z_justif_violence c.nse_barrio  avg_isei avg_educ avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
	estimate store m15
	

	coefplot (m7, aseq("Interest pol.")) (m8, aseq("Satisf. Demo.")) (m9, aseq("Conv. particip.")) (m10, aseq("Unconv. particip.")) (m11, aseq("Egalitarianism")) (m12, aseq("Altruistic dispo.")) (m13, aseq("Prosoc. Behav.")) (m14, aseq("Democracy support")) (m15, aseq("Justif. violence")), keep(nse_barrio) yline(0, lcolor(black)) graphregion(color(white)) vertical aseq swapnames xtitle(Dependent variables) ytitle(Coefficients (std.)) xlabel(, angle(vertical)) legend(off) title(Pooled OLS. Political and Normative dimensions.)

	          

*----------------------*
*----- TWFE GRAPH -----*		
*----------------------*			  


		xtreg z_identification c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m1

		xtreg z_friends c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m2

		xtreg z_size_network c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			 estimate store m3

		xtreg z_gen_trust c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m4

		xtreg z_trust_minorities c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m5

		xtreg z_trust_inst c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m6

	coefplot (m1, aseq("Identification")) (m2, aseq("Relationship")) (m3, aseq("Network size")) (m4, aseq("Gen. Trust")) (m5, aseq("Trust minorities")) (m6, aseq("Trust institutions")), keep(nse_barrio) yline(0, lcolor(black)) graphregion(color(white)) vertical aseq swapnames xtitle(Dependent variables) ytitle(Coefficients (std.)) xlabel(, angle(vertical)) legend(off) title(Two-way Fixed Effect OLS. Emotional and Relational dimensions.)

*********
		xtreg z_interest_pol c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m7

		xtreg z_satisf_demo c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m8

		xtreg z_conv_particip c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			 estimate store m9

		xtreg z_unconv_particip c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m10

		xtreg z_egalitarianism c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m11

		xtreg z_altruistic c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m12

		xtreg z_prosoc_behave c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m13
			
		xtreg z_democracy_support c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m14
			
		xtreg z_justif_violence c.nse_barrio sex age age_sq yr_address homeowner married  has_children i.year ,  fe
			estimate store m15
			
	coefplot (m7, aseq("Interest pol.")) (m8, aseq("Satisf. Demo.")) (m9, aseq("Conv. particip.")) (m10, aseq("Unconv. particip.")) (m11, aseq("Egalitarianism")) (m12, aseq("Altruistic dispo.")) (m13, aseq("Prosoc. Behav.")) (m14, aseq("Democracy support")) (m15, aseq("Justif. violence")), keep(nse_barrio) yline(0, lcolor(black)) graphregion(color(white)) vertical aseq swapnames xtitle(Dependent variables) ytitle(Coefficients (std.)) xlabel(, angle(vertical)) legend(off) title(Two-way Fixed Effect OLS. Political and Normative dimensions.)

	
	       
	
****************
* Cultural
***** Sense of belonging and identification Negative SIG
	* Pooled without interaction, NSE barrio continuous
		
		* Empty model
			reg z_identification nse_barrio, vce(cluster geocodigo) // it works Main effect is SIG

		* 3 predictors
			reg z_identification nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_identification nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
		
		* NSE indiv
			reg z_identification nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_identification nse_barrio  nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* NSE indiv discrete
			reg z_identification nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // NO
			reg z_identification nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* Only class
			reg z_identification nse_barrio avg_isei , vce(cluster geocodigo) // NO
			reg z_identification nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* Only class and educ
			reg z_identification nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_identification nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
			
	* Pooled without interaction, NSE barrio discrete
		
		* Empty model
			reg z_identification i.quint_nse_barrio, vce(cluster geocodigo) // NO

		* 3 predictors
			reg z_identification i.quint_nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
		
		* NSE indiv continuous
			reg z_identification i.quint_nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* NSE indiv discrete
			reg z_identification i.quint_nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* Only class
			reg z_identification i.quint_nse_barrio avg_isei , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG			

		* Only class and educ
			reg z_identification i.quint_nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_identification i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		
	* Pooled with interaction, NSE barrio continuous
		* Interaction with class + 0, 1, 2 other predictors
			reg z_identification c.nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

margins, at(nse_barrio=(0 .20 .40 .60 .80 1) avg_isei=(10(5)85)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_size_network

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_identification c.nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with income + 0, 1, 2 other predictors
			reg z_identification c.nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification c.nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_identification c.nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_identification c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vsquish
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_size_network
		

	* Pooled with interaction, NSE barrio discrete
		* Interaction with class + 0, 1, 2 other predictors
			reg z_identification i.quint_nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_identification i.quint_nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with income + 0, 1, 2 other predictors
			reg z_identification i.quint_nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_identification i.quint_nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_identification i.quint_nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_identification i.quint_nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			
/*
		reg z_identification c.nse_barrio##i.educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = Negative SIG
		reg z_identification c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_identification c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = GOOD SIG
		reg z_identification c.nse_barrio##c.isei ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = GOOD SIG
		reg z_identification c.nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_identification nse_barrio i.year sex age age_sq yr_address homeowner married  has_children ,  fe
*/
		xtreg z_identification c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vsquish
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


			reg z_friends c.nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
			xtreg z_friends c.nse_barrio##c.avg_isei sex age age_sq yr_address homeowner married  has_children i.year ,  fe



****************
* Relational
***** Number of friends Positive SIG
	* Pooled without interaction, NSE barrio continuous
		
		* Empty model
			reg z_friends nse_barrio, vce(cluster geocodigo) // it works Main effect is SIG

		* 3 predictors
			reg z_friends nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
		
		* NSE indiv
			reg z_friends nse_barrio nse_indiv , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio  nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* NSE indiv discrete
			reg z_friends nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* Only class
			reg z_friends nse_barrio avg_isei , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* Only class and educ
			reg z_friends nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
			
	* Pooled without interaction, NSE barrio discrete
		
		* Empty model
			reg z_friends i.quint_nse_barrio, vce(cluster geocodigo) // it works Main effect is SIG

		* 3 predictors
			reg z_friends i.quint_nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
		
		* NSE indiv continuous
			reg z_friends i.quint_nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* NSE indiv discrete
			reg z_friends i.quint_nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		* Only class
			reg z_friends i.quint_nse_barrio avg_isei , vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG			

		* Only class and educ
			reg z_friends i.quint_nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // it works Main effect is SIG
			reg z_friends i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG

		
	* Pooled with interaction, NSE barrio continuous
		* Interaction with class + 0, 1, 2 other predictors
			reg z_friends c.nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_friends c.nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with income + 0, 1, 2 other predictors
			reg z_friends c.nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends c.nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_friends c.nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_friends c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST THIS IS THE BEST
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vsquish vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_size_network
		
	* Pooled with interaction, NSE barrio discrete
		* Interaction with class + 0, 1, 2 other predictors
			reg z_friends i.quint_nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_friends i.quint_nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST THIS IS THE BEST

		* Interaction with income + 0, 1, 2 other predictors
			reg z_friends i.quint_nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_friends i.quint_nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_friends i.quint_nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_friends i.quint_nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			
	
/*		
		reg z_friends c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = GOOD
		reg z_friends c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_friends c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_friends c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_friends nse_barrio i.year ,  fe
*/

		xtreg z_friends c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5))
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)

	
***** Intimate network size NOSIG but SIG with interaction
	* Pooled without interaction, NSE barrio continuous
		
		* Empty model
			reg z_size_network nse_barrio, vce(cluster geocodigo) // it works Main effect is SIG

		* 3 predictors
			reg z_size_network nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // it works Main effect is SIG
		
		* NSE indiv
			reg z_size_network nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio  nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* NSE indiv discrete
			reg z_size_network nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Only class
			reg z_size_network nse_barrio avg_isei , vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST

		* Only class and educ
			reg z_size_network nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			
	* Pooled without interaction, NSE barrio discrete
		
		* Empty model
			reg z_size_network i.quint_nse_barrio, vce(cluster geocodigo) // NO

		* 3 predictors
			reg z_size_network i.quint_nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* NSE indiv continuous
			reg z_size_network i.quint_nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* NSE indiv discrete
			reg z_size_network i.quint_nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Only class
			reg z_size_network i.quint_nse_barrio avg_isei , vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO			

		* Only class and educ
			reg z_size_network i.quint_nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		
	* Pooled with interaction, NSE barrio continuous
		* Interaction with class + 0, 1, 2 other predictors
			reg z_size_network c.nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_size_network c.nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST
			reg z_size_network c.nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST

		* Interaction with income + 0, 1, 2 other predictors
			reg z_size_network c.nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network c.nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_size_network c.nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_size_network c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST THIS IS THE BEST
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_size_network
esttab z_size_network, starlevels(* 0.10 ** 0.05 *** 0.01)


		
	* Pooled with interaction, NSE barrio discrete
		* Interaction with class + 0, 1, 2 other predictors
			reg z_size_network i.quint_nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_size_network i.quint_nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with income + 0, 1, 2 other predictors
			reg z_size_network i.quint_nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_size_network i.quint_nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_size_network i.quint_nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_size_network i.quint_nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		
		
/*		
		reg z_size_network c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = GOOD
		reg z_size_network c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_size_network c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_size_network c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_size_network c.nse_barrio i.year ,  fe
*/
		xtreg z_size_network c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5))
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)

	
	* Generalized trust NOSIG
		
		* Empty model
			reg z_gen_trust nse_barrio, vce(cluster geocodigo) // it works Main effect is SIG

		* 3 predictors
			reg z_gen_trust nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // YES
			reg z_gen_trust nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* NSE indiv
			reg z_gen_trust nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio  nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* NSE indiv discrete
			reg z_gen_trust nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Only class
			reg z_gen_trust nse_barrio avg_isei , vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST

		* Only class and educ
			reg z_gen_trust nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			
	* Pooled without interaction, NSE barrio discrete
		
		* Empty model
			reg z_gen_trust i.quint_nse_barrio, vce(cluster geocodigo) // NO

		* 3 predictors
			reg z_gen_trust i.quint_nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* NSE indiv continuous
			reg z_gen_trust i.quint_nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* NSE indiv discrete
			reg z_gen_trust i.quint_nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Only class
			reg z_gen_trust i.quint_nse_barrio avg_isei , vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO			

		* Only class and educ
			reg z_gen_trust i.quint_nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		
	* Pooled with interaction, NSE barrio continuous
		* Interaction with class + 0, 1, 2 other predictors
			reg z_gen_trust c.nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_gen_trust c.nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST
			reg z_gen_trust c.nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST

		* Interaction with income + 0, 1, 2 other predictors
			reg z_gen_trust c.nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust c.nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_gen_trust c.nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_gen_trust c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST THIS IS THE BEST
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_gen_trust
esttab z_gen_trust, starlevels(* 0.10 ** 0.05 *** 0.01)


		
	* Pooled with interaction, NSE barrio discrete
		* Interaction with class + 0, 1, 2 other predictors
			reg z_gen_trust i.quint_nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_gen_trust i.quint_nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with income + 0, 1, 2 other predictors
			reg z_gen_trust i.quint_nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_gen_trust i.quint_nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_gen_trust i.quint_nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_gen_trust i.quint_nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO	
		
		
		
		/*reg z_gen_trust nse_barrio, vce(cluster geocodigo)
		reg z_gen_trust nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_gen_trust c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo)
		reg z_gen_trust c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
		reg z_gen_trust c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_gen_trust c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_gen_trust c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_gen_trust c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_gen_trust c.nse_barrio i.year ,  fe
*/
		xtreg z_gen_trust c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5))
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
***** Trust in social minorities Positive SIG

		
		* Empty model
			reg z_trust_minorities nse_barrio, vce(cluster geocodigo) // it works Main effect is SIG

		* 3 predictors
			reg z_trust_minorities nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_trust_minorities nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // YES
		
		* NSE indiv
			reg z_trust_minorities nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_trust_minorities nse_barrio  nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // YES

		* NSE indiv discrete
			reg z_trust_minorities nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // YES

		* Only class
			reg z_trust_minorities nse_barrio avg_isei , vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // YES

		* Only class and educ
			reg z_trust_minorities nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_trust_minorities nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // YES
			reg z_trust_minorities nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // YES
			
	* Pooled without interaction, NSE barrio discrete
		
		* Empty model
			reg z_trust_minorities i.quint_nse_barrio, vce(cluster geocodigo) // NO

		* 3 predictors
			reg z_trust_minorities i.quint_nse_barrio avg_educ avg_income avg_isei , vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio avg_educ avg_income avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* NSE indiv continuous
			reg z_trust_minorities i.quint_nse_barrio nse_indiv , vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* NSE indiv discrete
			reg z_trust_minorities i.quint_nse_barrio i.quint_nse_indiv , vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children , vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Only class
			reg z_trust_minorities i.quint_nse_barrio avg_isei , vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO			

		* Only class and educ
			reg z_trust_minorities i.quint_nse_barrio avg_isei avg_educ , vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio avg_isei avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		
	* Pooled with interaction, NSE barrio continuous
		* Interaction with class + 0, 1, 2 other predictors
			reg z_trust_minorities c.nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_trust_minorities c.nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST
			reg z_trust_minorities c.nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // ALMOST
			reg z_trust_minorities c.nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // YES

		* Interaction with income + 0, 1, 2 other predictors
			reg z_trust_minorities c.nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities c.nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_trust_minorities c.nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_trust_minorities c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_trust_minorities
esttab z_trust_minorities, starlevels(* 0.10 ** 0.05 *** 0.01)


		
	* Pooled with interaction, NSE barrio discrete
		* Interaction with class + 0, 1, 2 other predictors
			reg z_trust_minorities i.quint_nse_barrio##c.avg_isei avg_educ avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_isei avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_isei avg_educ  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_isei  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with educ + 0, 1, 2 other predictors
			reg z_trust_minorities i.quint_nse_barrio##c.avg_educ avg_isei  avg_income  sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_educ avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_educ avg_income   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_educ sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with income + 0, 1, 2 other predictors
			reg z_trust_minorities i.quint_nse_barrio##c.avg_income avg_educ avg_isei    sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_income avg_isei   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_income avg_educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
			reg z_trust_minorities i.quint_nse_barrio##c.avg_income sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		
		* Interaction with NSE indiv continuous
			reg z_trust_minorities i.quint_nse_barrio##c.nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

		* Interaction with NSE indiv discrete
			reg z_trust_minorities i.quint_nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO

/*
		reg z_trust_minorities nse_barrio, vce(cluster geocodigo)
		reg z_trust_minorities nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_trust_minorities c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children, vce(cluster geocodigo)
		reg z_trust_minorities c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo)
		reg z_trust_minorities c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = GOOD
		reg z_trust_minorities c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_trust_minorities c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_trust_minorities c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = GOOD
		xtreg z_trust_minorities c.nse_barrio i.year ,  fe
	*/	
	
		xtreg z_trust_minorities c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
***** Trust in major institutions Positive SIG
		reg z_trust_inst nse_barrio, vce(cluster geocodigo)
		reg z_trust_inst nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_trust_inst c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_trust_inst c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_trust_inst c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_trust_inst c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = GOOD
		reg z_trust_inst c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_trust_inst c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_trust_inst c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children ib3.year ,  fe
		
		* Interaction with NSE indiv discrete
		reg z_trust_inst c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
estimates store z_trust_minorities
esttab z_trust_minorities, starlevels(* 0.10 ** 0.05 *** 0.01)


****************		
* Political
***** Interest in political affairs Positive SIG
		reg z_interest_pol nse_barrio, vce(cluster geocodigo)
		reg z_interest_pol nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_interest_pol c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_interest_pol c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_interest_pol c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = GOOD
		reg z_interest_pol c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_interest_pol c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_interest_pol c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_interest_pol c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_interest_pol c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


***** Satisfaction with democracy Positive SIG
		reg z_satisf_demo nse_barrio, vce(cluster geocodigo)
		reg z_satisf_demo nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_satisf_demo c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_satisf_demo c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_satisf_demo c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_satisf_demo c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = GOOD
		reg z_satisf_demo c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_satisf_demo c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_satisf_demo c.nse_barrio i.year ,  fe
		xtreg z_satisf_demo c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_satisf_demo c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


***** Conventional political participation NOSIG
		reg z_conv_particip nse_barrio, vce(cluster geocodigo)
		reg z_conv_particip nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_conv_particip c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_conv_particip c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_conv_particip c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = GOOD
		reg z_conv_particip c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = GOOD
		reg z_conv_particip c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_conv_particip c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_conv_particip c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe

		reg z_conv_particip c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)

		
***** Unconventional political participation SIG with FE
		reg z_unconv_particip nse_barrio, vce(cluster geocodigo)
		reg z_unconv_particip nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_unconv_particip c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_unconv_particip c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_unconv_particip c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_unconv_particip c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = GOOD
		reg z_unconv_particip c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_unconv_particip c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = ALMOST GOOD
		xtreg z_unconv_particip c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_unconv_particip c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


	* Egalitarianism NOSIG
		reg z_egalitarianism nse_barrio, vce(cluster geocodigo)
		reg z_egalitarianism nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_egalitarianism c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_egalitarianism c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_egalitarianism c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_egalitarianism c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_egalitarianism c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_egalitarianism c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOT SIG
		xtreg z_egalitarianism c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_egalitarianism c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


	* Altruistic disposition NOSIG
		reg z_altruistic nse_barrio, vce(cluster geocodigo)
		reg z_altruistic nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_altruistic c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_altruistic c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_altruistic c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_altruistic c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_altruistic c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_altruistic c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOT SIG
		xtreg z_altruistic c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_altruistic c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)

		
	* Pro-social behavior NOSIG
		reg z_prosoc_behave nse_barrio, vce(cluster geocodigo)
		reg z_prosoc_behave nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_prosoc_behave c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_prosoc_behave c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_prosoc_behave c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_prosoc_behave c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_prosoc_behave c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_prosoc_behave c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOT SIG
		xtreg z_prosoc_behave c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_prosoc_behave c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


****************
* Normative	
***** Support for democracy NOSIG
		reg z_democracy_support nse_barrio, vce(cluster geocodigo)
		reg z_democracy_support nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_democracy_support c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_democracy_support c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_democracy_support c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = GOOD
		reg z_democracy_support c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_democracy_support c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_democracy_support c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = GOOD
		xtreg z_democracy_support c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe

		reg z_democracy_support c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)


	* Justificacion of violence NOSIG
		reg z_justif_violence nse_barrio, vce(cluster geocodigo)
		reg z_justif_violence nse_barrio i.educ ln_income i.class_7 , vce(cluster geocodigo)
		reg z_justif_violence c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children, vce(cluster geocodigo)
		reg z_justif_violence c.nse_barrio i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married has_children i.year, vce(cluster geocodigo)
		reg z_justif_violence c.nse_barrio##i.educ ln_income i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction EDUC = NOTSIG
		reg z_justif_violence c.nse_barrio##c.ln_income i.educ  i.class_7 sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction INCOME = NOTSIG
		reg z_justif_violence c.nse_barrio##i.class_7 ln_income i.educ   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // interaction CLASS = NOTSIG
		reg z_justif_violence c.nse_barrio##c.nse_indiv   sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NSE INDIV = NOTSIG
		xtreg z_justif_violence c.nse_barrio##i.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year ,  fe
		
		reg z_justif_violence c.nse_barrio##ib1.quint_nse_indiv sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
margins, at(nse_barrio=(0 .20 .40 .60 .80 1) quint_nse_indiv=(1 5)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
	

	
	
		reg z_justif_violence c.nse_barrio##c.avg_isei sex age age_sq yr_address homeowner married  has_children i.year, vce(cluster geocodigo) // NO
		margins , at(nse_barrio=(0 .20 .40 .60 .80 1) avg_isei=(105)) vce(unconditional)

		margins, at(nse_barrio=(0 .20 .40 .60 .80 1) avg_isei=(105)) vce(unconditional)
marginsplot, ciopts(color(*.5)) recastci(rspike) legend(order(1 "Poor individuals" 2 "Affluent individuals")) xtitle("Neighborhood SES") level(90)
	
	
	
	
	
	
	
	
	
	
	
* Analytical steps: pooled OLS, pooled OLS with year FE, two-way FE ++ Interaction













	* Cultural dimension
		reg z_belonging Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.2 0 .2 .4)) post
			marginsplot, title("Sentido de pertenencia e identificación") graphregion(fcolor(white))
				gr export "$main\pertenencia.png", width(1000) replace

			
	* Relationships
		reg z_friends Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.2 0 .2 .4)) post
			estimates store margins_friends
		reg z_size_network Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.2 0 .2 .4)) post
			estimates store margins_network
			
		coefplot /// 
			(margins_friends, at graphregion(fcolor(white)) ylab(, angle(0)) label(N amigos) ///
			lwidth(*1) lcolor(red*1.3) connect(l) msymbol(circle) mcolor(red*1.3) ciopts(color(red*1.3) recast(rcap))) ///
				(margins_network, ytitle("Linear prediction") xtitle("Hi") at xtitle(violence) label(Tamaño de la red)  lwidth(*1) lcolor(blue*1.3) connect(l) ///
				msymbol(circle) mcolor(blue*1.3) ciopts(color(blue*1.3) recast(rcap))) 
				
				gr export "$main\relaciones_sociales.png", width(1000) replace

	* Trust 
		reg z_trust_citizens Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.2 0 .2 .4)) post
			estimates store margins_trust_citizens
		reg z_trust_inst Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_trust_inst
		reg z_trust_minorities Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_trust_minorities
			
	coefplot /// 
		(margins_trust_citizens, at graphregion(fcolor(white)) ylab(, angle(0)) label(Interpersonal) ///
		lwidth(*1) lcolor(red*1.3) connect(l) msymbol(circle) mcolor(red*1.3) ciopts(color(red*1.3) recast(rcap))) ///
			(margins_trust_inst, at xtitle(violence) label(Instituciones)  lwidth(*1) lcolor(blue*1.3) connect(l) ///
			msymbol(circle) mcolor(blue*1.3) ciopts(color(blue*1.3) recast(rcap))) ///
				(margins_trust_minorities, at xtitle(violence) label(Minorías)  lwidth(*1) lcolor(black*1.3) connect(l) ///
				msymbol(circle) mcolor(black*1.3) ciopts(color(black*1.3) recast(rcap)))
	
					gr export "$main\confianza.png", width(1000) replace

	
	* Political dimension
		reg z_interest_pol Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.2 0 .2 .4)) post
			estimates store margins_interest_pol
			
		reg z_satisf_demo Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.2 0 .2 .4)) post
			estimates store margins_satisf_demo
			
		coefplot /// 
			(margins_interest_pol, at graphregion(fcolor(white)) ylab(, angle(0)) label(Interés en asuntos públicos ) ///
			lwidth(*1) lcolor(red*1.3) connect(l) msymbol(circle) mcolor(red*1.3) ciopts(color(red*1.3) recast(rcap))) ///
				(margins_satisf_demo, ytitle("Linear prediction") xtitle("Hi") at xtitle(violence) label(Satisfacción con la democracia)  lwidth(*1) lcolor(blue*1.3) connect(l) ///
				msymbol(circle) mcolor(blue*1.3) ciopts(color(blue*1.3) recast(rcap))) 

	
			
		reg z_conv_particip Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_conv_particip

		reg z_non_conv_particip Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_noconv_particip
			
		coefplot /// 
			(margins_conv_particip, at graphregion(fcolor(white)) ylab(, angle(0)) label(Participacíon convencional) ///
			lwidth(*1) lcolor(red*1.3) connect(l) msymbol(circle) mcolor(red*1.3) ciopts(color(red*1.3) recast(rcap))) ///
				(margins_noconv_particip, at xtitle(violence) label(Participacíon NC)  lwidth(*1) lcolor(blue*1.3) connect(l) ///
				msymbol(circle) mcolor(blue*1.3) ciopts(color(blue*1.3) recast(rcap)))
				
				gr export "$main\particip_pol.png", width(1000) replace

		
		reg z_solidarity_values Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_solidarity

		
		reg z_prosoc_behaviour Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_prosoc
		
		coefplot /// 
			(margins_solidarity, at graphregion(fcolor(white)) ylab(, angle(0)) label(Participacíon convencional) ///
			lwidth(*1) lcolor(red*1.3) connect(l) msymbol(circle) mcolor(red*1.3) ciopts(color(red*1.3) recast(rcap))) ///
				(margins_prosoc, at xtitle(violence) label(Participacíon NC)  lwidth(*1) lcolor(blue*1.3) connect(l) ///
				msymbol(circle) mcolor(blue*1.3) ciopts(color(blue*1.3) recast(rcap)))

		
	* Normative dimension
		reg z_democracy_support Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_demo_support
			
		reg z_justif_violence Hi i.estrato i.m0_sexo m0_edad ln_income educ2, robust
			margins, at( Hi = (-.4 -.3 -.2 -.1 0 .1 .2 .3 .4)) post
			estimates store margins_violence
	
		coefplot /// 
			(margins_demo_support, at graphregion(fcolor(white)) ylab(, angle(0)) label(Apoyo a la democracia) ///
			lwidth(*1) lcolor(red*1.3) connect(l) msymbol(circle) mcolor(red*1.3) ciopts(color(red*1.3) recast(rcap))) ///
				(margins_violence, at xtitle(violence) label(No justificación de la violencia)  lwidth(*1) lcolor(blue*1.3) connect(l) ///
				msymbol(circle) mcolor(blue*1.3) ciopts(color(blue*1.3) recast(rcap)))

		
		
****** With fixed effects

xtset idencuesta ola

xtreg z_belonging Hi i.m0_sexo m0_edad ln_income, fe robust
		
xtreg z_justif_violence Hi i.m0_sexo m0_edad ln_income, fe robust

reg z_size_network Hi i.estrato i.m0_sexo m0_edad ln_income, robust














		
