**************************************
**************************************
*** Project: SEGAPEGO              ***
*** Author: GO, QR                 ***
*** Date: August 2023              ***
*** Topic: variable segregation    ***
**************************************
**************************************


*--------------------------------------------------------------------*
* ----- Run the code to select the sample and make imputations ----- *
*--------------------------------------------------------------------*

	clear
	cd "C:\Work\Github\seg-apeg\input\data\pre-proc"
	use elsoc_wide_selected_imputed.dta	
	
*--------------------------------------------------------------------*
* -----               Convert to long format                   ----- *
*--------------------------------------------------------------------*
	
	* Convertir en panel long
		reshape long ola segmento fact_exp02 region_cod r13_nredes r15 c01 c02 c05_01 c05_02 c05_05 c05_07 c06_04 c06_05 ///
		c06_06 c07_04 c07_05 c08_01 c08_02 c08_03 c12_01 c12_03 c12_04 c12_05 c13 c18_02 c18_03 c25 c32_01 c32_02 ///
		d02_01 d02_02 d02_03 f05_01 f05_02 f05_03 m0_sexo m0_edad m01 ciuo08_m03 m29 m33 m34_03 m36 m37 geocodigo, i(idencuesta) j(year, string)

	* Good time-period variables
		label val ola
		tostring ola, replace
		replace ola = "Wave 01" if year =="_w01"
		replace ola = "Wave 04" if year =="_w04"
		replace ola = "Wave 06" if year =="_w06"

		replace year = "2016" if year =="_w01"
		replace year = "2019" if year =="_w04"
		replace year = "2022" if year =="_w06"
		destring year, replace

		order ola, before(year)
		order geocodigo, after(year)

*--------------------------------------------------------------------*
* -----            Create dependent variables                  ----- *
*--------------------------------------------------------------------*

***** Cultural dimension
	* Var 1: Sense of identification
		bys idencuesta year : gen identification = (c32_01 + c32_02)/2

***** Relational dimension 
	* Var 2: Number of friends
		rename r15 friends
	
	* Var 3: Intimate network size
		rename r13_nredes size_network
		
	* Var 4: Generalized trust
		rename c02 gen_trust
	
	* Var 5: Trust in social minorities
		bys idencuesta year : gen trust_minorities = (c06_04 + c06_05 + c06_06)/3
		
	* Var 6 : Trust in major institutions 
		bys idencuesta year : gen trust_inst = (c05_01 + c05_02 + c05_05 + c05_07)/4
	
***** Political dimension
	* Var 7: Interest in political affairs
		rename c13 interest_pol
	
	* Var 8: Satisfaction with democracy
		rename c01 satisf_demo
		
	* Var 9: Conventional political participation
		bys idencuesta year : gen conv_particip = (c12_01 + c12_03 + c12_04 + c12_05)/4
				
	* Var 10: Unconventional political participation
		bys idencuesta year : gen unconv_particip = (c08_01 + c08_02 + c08_03)/3
		
	* Var 11: Egalitarianism
		bys idencuesta year : gen egalitarianism = (d02_01 + d02_02 + d02_03)/3

	* Var 12: Altruistic disposition
		bys idencuesta year : gen altruistic = (c18_02 + c18_03)/2
		
	* Var 13: Pro-social behavior
		bys idencuesta year : gen prosoc_behave = (c07_04 + c07_05)/2

***** Normative dimension
	* Support to democracy
		rename c25 democracy_support
	
	* Justification of violence
		bys idencuesta year : gen justif_violence = (f05_01 + f05_02 + f05_03)/3

	* Order file
		order identification friends size_network gen_trust trust_minorities trust_inst interest_pol satisf_demo ///
		conv_particip unconv_particip egalitarianism altruistic prosoc_behave democracy_support justif_violence, after( geocodigo)
		
***** Standardisation of dependent variables by year
		
		foreach var of varlist identification friends size_network gen_trust trust_minorities trust_inst interest_pol satisf_demo ///
		conv_particip unconv_particip egalitarianism altruistic prosoc_behave democracy_support justif_violence {
			egen mean_`var' = mean(`var'), by(year)
			egen sd_`var' = sd(`var'), by(year)
			gen z_`var' = (`var' - mean_`var')/sd_`var'
		}

	drop mean_* sd_*
		
		
*--------------------------------------------------------------------*
* -----            Create individual-level moderator           ----- *
*--------------------------------------------------------------------*

	* Collapse education level in 5 categories
		gen educ = .
		replace educ = 1 if m01 <= 2
		replace educ = 2 if m01 == 3 | m01 == 4
		replace educ = 3 if m01 == 5 | m01 == 6
		replace educ = 4 if m01 == 7 | m01 == 8
		replace educ = 5 if m01 == 9 | m01 == 10
		
		label define educ 1 "No formal educ" 2 "Primary" 3 "Secondary" 4 "Vocational" 5 "University", replace
		label val educ educ	

	* Generate the natural logarithm of income by year
		bys year: gen ln_income = ln(m29)

	* Generate average education level
		bys idencuesta : egen avg_educ = mean(educ)
	
	* Generate average income level
		bys idencuesta : egen avg_income = mean(ln_income)
	
	* Generate discrete variable of income (quintiles)
		egen quint_inc = xtile(avg_income), nq(5)
		
	* Create social class (Oesch Scheme based on Isco 08)
		cd "C:\Work\Github\seg-apeg\docs\stata"
		rename ciuo08_m03 isco
		replace isco=2151	if idencuesta==	"13131014"
		replace isco=3313	if idencuesta==	"13201011"
		replace isco=7233	if idencuesta==	"13401034"
		replace isco=8332	if idencuesta==	"13116018"
		replace isco=5221	if idencuesta==	"13110111"
		do "0. isco into oesch.do"
		replace class = 17 if isco==15000 // retired
		replace class = 18 if isco==16000 // unemployed
		
	* Collapse class in 10 categories
		gen class_10 = 0
		replace class_10 = 1 if class == 1
		replace class_10 = 1 if class == 2
		replace class_10 = 2 if class == 3
		replace class_10 = 2 if class == 4
		replace class_10 = 3 if class == 5
		replace class_10 = 3 if class == 6
		replace class_10 = 4 if class == 7
		replace class_10 = 4 if class == 8
		replace class_10 = 5 if class == 9
		replace class_10 = 5 if class == 10
		replace class_10 = 6 if class == 11
		replace class_10 = 6 if class == 12
		replace class_10 = 7 if class == 13
		replace class_10 = 7 if class == 14		
		replace class_10 = 8 if class == 15
		replace class_10 = 8 if class == 16
		replace class_10 = 9 if class == 17
		replace class_10 = 10 if class == 18	
		
		label define class_10 1 "Self-employed professionals and large employers" 2 "Small business owners" 3 "Technical (semi-)professionals" ///
							  4 "Production workers" 5 "(Associate) managers" 6 "Clerks" 7 "Socio-cultural (semi-)professionals" ///
							  8 "Service workers" 9 "Retired" 10 "Unemployed", replace
		label val class_10 class_10	
		
	* Collapse class in 7 categories
		gen class_7 = 0
		replace class_7 = 1 if class == 1
		replace class_7 = 1 if class == 2
		replace class_7 = 1 if class == 5
		replace class_7 = 1 if class == 9
		replace class_7 = 1 if class == 13

		replace class_7 = 2 if class == 6
		replace class_7 = 2 if class == 10
		replace class_7 = 2 if class == 14
		
		replace class_7 = 3 if class == 3
		replace class_7 = 3 if class == 4
		
		replace class_7 = 4 if class == 7
		replace class_7 = 4 if class == 11
		replace class_7 = 4 if class == 15

		replace class_7 = 5 if class == 8
		replace class_7 = 5 if class == 12
		replace class_7 = 5 if class == 16
		
		replace class_7 = 6 if class == 17
		replace class_7 = 7 if class == 18	
		
		label define class_7 1 "Higher-grade service class" 2 "Lower-grade service class" 3 "Small business owners" ///
							  4 "Skilled workers" 5 "Unskilled workers" 8 "Service workers" 6 "Retired" 7 "Unemployed", replace
		label val class_7 class_7	

	* Create social class based on ISEI
		iscogen isei = isei(isco)
	
	* Predict ISEI for unemployed, retired and people with missing ISCO
		reg isei ln_income i.educ m0_edad
		predict predicted_isei
		replace isei = predicted_isei if isei==.
		drop predicted
	
	* Generate average ISEI
		bys idencuesta : egen avg_isei = mean(isei)
		
*--------------------------------------------------------------------*
* -----                Create individual-level NSE             ----- *
*--------------------------------------------------------------------*

	* Multiple Correspondance Analysis
		pca avg_inc avg_isei avg_educ
	
	* Store scores for all individuals on mca1 and mca2
		predict pc1, score

	* Individual plot 
		* scoreplot
		* screeplot
		*scatter mca2 mca1, msize(.5) xline(0) yline(0) 

	* Generate inverse score for mca1
		*gen inv_mca1_2016 = mca1_2016*-1
	
	* Normalize score
		egen max_pc1=max(pc1)
		egen min_pc1=min(pc1)
		gen nse_indiv=(pc1-min_pc1)/(max_pc1-min_pc1)
		
*--------------------------------------------------------------------*
* -----            Create individual-level covariates          ----- *
*--------------------------------------------------------------------*

	* Rename
		rename m0_edad age
		rename m0_sexo sex 
		rename m33 tenure
		rename m34_03 yr_address
		rename m36 marital_status
		rename m37 children
	
	* Generate age square
		gen age_sq = age*age
	
	* Generate dummies for housing tenure and presence of children
		gen homeowner = 0
		replace homeowner = 1 if tenure<=2
		
		gen married = 0
		replace married = 1 if marital_status==1
		replace married = 1 if marital_status==3
	
		gen has_children = 0
		replace has_children = 1 if children>=1
	
	* Drop unnecessary variables
		drop c05_01- f05_03 m01
		order sex age yr_address, after(ln_income)
		order quint_inc nse_indiv class*, after(ln_income)





