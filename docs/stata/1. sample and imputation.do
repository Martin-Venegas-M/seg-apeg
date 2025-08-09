**************************************
**************************************
*** Project: SEGAPEGO              ***
*** Author: GO, QR                 ***
*** Date: August 2023              ***
*** Topic: variable segregation    ***
**************************************
**************************************

*------------------------------------*
* ----- First sample selection ----- *
*------------------------------------*

	* Open ELSOC database (download here: https://dataverse.harvard.edu/file.xhtml?fileId=7245114&datasetVersionId=347941)
		clear
		cd "C:\Work\Github\seg-apeg\input\data\proc"
		use elsoc_vars_selected.dta

	* Select respondents who belong to the original sampling (n=2,928)
		keep if muestra==1

	* Select respondent who live in the Metropolitan Region of Santiago in 2016 (n=914)
		keep if region_cod_w01==13
	
	* Select respondents who appear in 2016, 2019 and 2022 (n=482)
		keep if ola_w01==1 & ola_w04==1 & ola_w06==1
		
	* Select respondents who also lived in the Metropolitan Region of Santiago in 2019 and 2022 (n=466)
		keep if region_cod_w04==13 & region_cod_w06==13 

	* Select relevant variables
		*keep idencuesta ola* ///
			 *c32_01* c32_02* /// 1. Sense of belonging and identification
			 *r15* /// 2. Number of friemds
			 *r13_nredes* /// 3. Intimate network size 
			 *c02* /// 4. Generalized trust
			 *c06_04* c06_05* c06_06* /// 5. Trust in social minorities
			 *c05_01* c05_02* c05_05* c05_07* /// 6. Trust in major institutions
			 *c13* /// 7. Interest in political affairs
			 *c01* /// 8. Satisfaction with democracy
			 *c12_01* c12_03* c12_04* c12_05* /// 9. Conventional political participation
			 *c08_01* c08_02* c08_03* /// 10. Unconventional political participation
			 *d02_01* d02_02* d02_03* /// 11. Egalitarianism
			 *c18_02* c18_03* /// 12. Altruistic disposition
			 *c07_04* c07_05* /// 13. Pro-social behavior
			 *c25* /// 14. Support for democracy
			 *f05_01* f05_02* f05_03* /// 15. Justificacion of violence
			 *m0_sexo* m0_edad* m01* m02* ciuo88_m03* ciuo08_m03* ciuo88_m22* ciuo08_m22* m19* m21* m29* m33* m34* m36* m37* /// control variables
			 *fact_exp02* segmento* // other variables
		
			*drop m33_otro* m36_otro*
			
	* Convert NS/NR as missing values for every dependent variables
		foreach var of varlist 	c32_01* c32_02* r15* r13_nredes* c02* c06_04* c06_05* c06_06* c05_01* c05_02* c05_05* c05_07* ///
			 c13* c01* c12_01* c12_03* c12_04* c12_05* c08_01* c08_02* c08_03* d02_01* d02_02* d02_03* c18_02* c18_03* c07_04* ///
			 c07_05* c25* f05_01* f05_02* f05_03* {    
			replace `var' = . if `var' == -888 | `var' == -999 | `var' == -666	
		}
		
*--------------------------------------------------------------------*
* ----- Imputation of values for dependent variables at Wave 1 ----- *
*--------------------------------------------------------------------*

	* For variables that have missing values for Wave 1 (_w01), we use value of Wave 2 (w_02). 
	* For some variables, we use directly Wave 3, because there is no measure for Wave 2 (c06 and c12)

		replace c32_01_w01 = c32_01_w02 if c32_01_w01==.
		replace c32_02_w01 = c32_02_w02 if c32_02_w01==.
		replace c02_w01    = c02_w02 if c02_w01==.
		replace c06_04_w01 = c06_04_w03 if c06_04_w01==.
		replace c06_05_w01 = c06_05_w03 if c06_05_w01==.
		replace c06_06_w01 = c06_06_w03 if c06_06_w01==.
		replace c05_01_w01 = c05_01_w02 if c05_01_w01==.
		replace c05_02_w01 = c05_02_w02 if c05_02_w01==.
		replace c05_05_w01 = c05_05_w02 if c05_05_w01==.
		replace c05_07_w01 = c05_07_w02 if c05_07_w01==.
		
		replace c13_w01    = c13_w02 if c13_w01==.
		replace c01_w01    = c01_w02 if c01_w01==.
		replace c12_01_w01 = c12_01_w03 if c12_01_w01==.
		replace c12_03_w01 = c12_03_w03 if c12_03_w01==.
		replace c12_04_w01 = c12_04_w03 if c12_04_w01==.
		replace c12_05_w01 = c12_05_w03 if c12_05_w01==.
		replace c08_01_w01 = c08_01_w02 if c08_01_w01==.
		replace c08_02_w01 = c08_02_w02 if c08_02_w01==.
		replace c08_03_w01 = c08_03_w02 if c08_03_w01==.
		replace d02_01_w01 = d02_01_w02 if d02_01_w01==.
		replace d02_02_w01 = d02_02_w02 if d02_02_w01==.
		replace d02_03_w01 = d02_03_w02 if d02_03_w01==.
		replace c18_02_w01 = c18_02_w02 if c18_02_w01==.
		replace c18_03_w01 = c18_03_w02 if c18_03_w01==.
		replace c07_04_w01 = c07_04_w02 if c07_04_w01==.
		
		replace c07_05_w01 = c07_05_w02 if c07_05_w01==.
		replace c25_w01    = c25_w02 if c25_w01==.
		replace f05_01_w01 = f05_01_w02 if f05_01_w01==.
		replace f05_02_w01 = f05_02_w02 if f05_02_w01==.
		replace f05_03_w01 = f05_03_w02 if f05_03_w01==.
	
	* For some variables, there are still a few missing values for Wave 1. We imput the values of Wave 3 or Wave 6 for c06 and c12.
		replace c32_01_w01 = c32_01_w03 if c32_01_w01==.
		replace c32_02_w01 = c32_02_w03 if c32_02_w01==.
		replace c02_w01    = c02_w03 if c02_w01==.
		replace c06_04_w01 = c06_04_w06 if c06_04_w01==.
		replace c06_05_w01 = c06_05_w06 if c06_05_w01==.
		replace c06_06_w01 = c06_06_w06 if c06_06_w01==.
		replace c05_01_w01 = c05_01_w03 if c05_01_w01==.
		replace c05_02_w01 = c05_02_w03 if c05_02_w01==.
		replace c05_05_w01 = c05_05_w03 if c05_05_w01==.
		replace c05_07_w01 = c05_07_w03 if c05_07_w01==.
		
		replace c13_w01    = c13_w03 if c13_w01==.
		replace c01_w01    = c01_w03 if c01_w01==.
		replace c12_01_w01 = c12_01_w06 if c12_01_w01==.
		replace c12_03_w01 = c12_03_w06 if c12_03_w01==.
		replace c12_04_w01 = c12_04_w06 if c12_04_w01==.
		replace c12_05_w01 = c12_05_w06 if c12_05_w01==.
		replace c08_01_w01 = c08_01_w03 if c08_01_w01==.
		replace c08_02_w01 = c08_02_w03 if c08_02_w01==.
		replace c08_03_w01 = c08_03_w03 if c08_03_w01==.
		replace d02_01_w01 = d02_01_w03 if d02_01_w01==.
		replace d02_02_w01 = d02_02_w03 if d02_02_w01==.
		replace d02_03_w01 = d02_03_w03 if d02_03_w01==.
		replace c18_02_w01 = c18_02_w03 if c18_02_w01==.
		replace c18_03_w01 = c18_03_w03 if c18_03_w01==.
		replace c07_04_w01 = c07_04_w03 if c07_04_w01==.
		
		replace c07_05_w01 = c07_05_w03 if c07_05_w01==.
		replace c25_w01    = c25_w03 if c25_w01==.
		replace f05_01_w01 = f05_01_w03 if f05_01_w01==.
		replace f05_02_w01 = f05_02_w03 if f05_02_w01==.
		replace f05_03_w01 = f05_03_w03 if f05_03_w01==.

	* For some variables, there are still a few missing values for Wave 1. We imput the values of Wave 4
		replace c32_01_w01 = c32_01_w04 if c32_01_w01==.
		replace c32_02_w01 = c32_02_w04 if c32_02_w01==.
		replace c02_w01    = c02_w04 if c02_w01==.
		replace c06_04_w01 = c06_04_w06 if c06_04_w01==.
		replace c06_05_w01 = c06_05_w06 if c06_05_w01==.
		replace c06_06_w01 = c06_06_w06 if c06_06_w01==.
		replace c05_01_w01 = c05_01_w04 if c05_01_w01==.
		replace c05_02_w01 = c05_02_w04 if c05_02_w01==.
		replace c05_05_w01 = c05_05_w04 if c05_05_w01==.
		replace c05_07_w01 = c05_07_w04 if c05_07_w01==.
		
		replace c13_w01    = c13_w04 if c13_w01==.
		replace c01_w01    = c01_w04 if c01_w01==.
		replace c12_01_w01 = c12_01_w06 if c12_01_w01==.
		replace c12_03_w01 = c12_03_w06 if c12_03_w01==.
		replace c12_04_w01 = c12_04_w06 if c12_04_w01==.
		replace c12_05_w01 = c12_05_w06 if c12_05_w01==.
		replace c08_01_w01 = c08_01_w04 if c08_01_w01==.
		replace c08_02_w01 = c08_02_w04 if c08_02_w01==.
		replace c08_03_w01 = c08_03_w04 if c08_03_w01==.
		replace d02_01_w01 = d02_01_w04 if d02_01_w01==.
		replace d02_02_w01 = d02_02_w04 if d02_02_w01==.
		replace d02_03_w01 = d02_03_w04 if d02_03_w01==.
		replace c18_02_w01 = c18_02_w04 if c18_02_w01==.
		replace c18_03_w01 = c18_03_w04 if c18_03_w01==.
		replace c07_04_w01 = c07_04_w04 if c07_04_w01==.
		
		replace c07_05_w01 = c07_05_w04 if c07_05_w01==.
		replace c25_w01    = c25_w04 if c25_w01==.
		replace f05_01_w01 = f05_01_w04 if f05_01_w01==.
		replace f05_02_w01 = f05_02_w04 if f05_02_w01==.
		replace f05_03_w01 = f05_03_w04 if f05_03_w01==.

	* Two variables have no measurement in Wave 1 (r13_nredes_w01 and r15_w01) so we create it based on Wave 2 and Wave 4 values
		gen r15_w01 = r15_w02
		replace r15_w01 = r15_w04 if r15_w01==.
		order r15_w01, before(r15_w02)
		gen r13_nredes_w01 = r13_nredes_w02
		replace r13_nredes_w01 = r13_nredes_w04 if r13_nredes_w01==.
		order r13_nredes_w01, before(r13_nredes_w02)

*--------------------------------------------------------------------*
* ----- Imputation of values for dependent variables at Wave 4 ----- *
*--------------------------------------------------------------------*

	* For variables that have missing values for Wave 4 (_w04), we use the average of Wave 3 and Wave 5 (w_03 and w_05). 
		replace c32_01_w04 = (c32_01_w03 + c32_01_w05)/2 if c32_01_w04==.
		replace c32_02_w04 = (c32_02_w03 + c32_02_w05)/2 if c32_02_w04==.
		replace c02_w04 = (c02_w03 + c02_w05)/2 if c02_w04==.
		replace c05_01_w04 = (c05_01_w03 + c05_01_w05)/2 if c05_01_w04==.
		replace c05_02_w04 = (c05_02_w03 + c05_02_w05)/2 if c05_02_w04==.
		replace c05_05_w04 = (c05_05_w03 + c05_05_w05)/2 if c05_05_w04==.
		replace c05_07_w04 = (c05_07_w03 + c05_07_w05)/2 if c05_07_w04==.
		replace c13_w04 = (c13_w03 + c13_w05)/2 if c13_w04==.
		replace c01_w04 = (c01_w03 + c01_w05)/2 if c01_w04==.
		replace c08_02_w04 = (c08_02_w03 + c08_02_w05)/2 if c08_02_w04==.
		replace c07_04_w04 = (c07_04_w03 + c07_04_w05)/2 if c07_04_w04==.
		replace c07_05_w04 = (c07_05_w03 + c07_05_w05)/2 if c07_05_w04==.
		replace c25_w04 = (c25_w03 + c25_w05)/2 if c25_w04==.
		replace f05_03_w04= (f05_03_w03 + f05_03_w05)/2 if f05_03_w04==.
		
	* For some variables, there is no value for Wave 5. Therefore we use only the value of Wave 3
		replace c01_w04 = c01_w03 if c01_w04==.
		replace c08_01_w04 = c08_01_w03 if c08_01_w04==.
		replace c08_03_w04 = c08_03_w03 if c08_03_w04==.
		replace d02_01_w04 = d02_01_w03 if d02_01_w04==.
		replace d02_02_w04 = d02_02_w03 if d02_02_w04==.
		replace d02_03_w04 = d02_03_w03 if d02_03_w04==.
		replace c18_02_w04 = c18_02_w03 if c18_02_w04==.
		replace c18_03_w04 = c18_03_w03 if c18_03_w04==.
		replace c25_w04 = c25_w03 if c25_w04==.
		replace f05_01_w04 = f05_01_w03 if f05_01_w04==.
		replace f05_02_w04 = f05_02_w03 if f05_02_w04==.	

	* For other variables, there is no value for Wave 3. Therefore we use only the value of Wave 5
		replace c32_01_w04 = c32_01_w05 if c32_01_w04==.
		replace c32_02_w04 = c32_02_w05 if c32_02_w04==.
		replace c02_w04 = c02_w05 if c02_w04==.
		replace c05_01_w04 = c05_01_w05 if c05_01_w04==.
		replace c05_02_w04 = c05_02_w05 if c05_02_w04==.
		replace c05_05_w04 = c05_05_w05 if c05_05_w04==.
		replace c05_07_w04 = c05_07_w05 if c05_07_w04==.
		replace c13_w04 = c13_w05 if c13_w04==.
		replace c01_w04 = c01_w05 if c01_w04==.
		replace c07_04_w04 = c07_04_w05 if c07_04_w04==.
		replace c07_05_w04 = c07_05_w05 if c07_05_w04==.
		replace c25_w04 = c25_w05 if c25_w04==.
		replace f05_03_w04 = f05_03_w05 if f05_03_w04==.
	
	* There are still a few missing values for some variables, we have make averages of more distant years
		replace r15_w04 = (r15_w02+r15_w06)/2 if r15_w04==.
		replace c05_01_w04 = (c05_01_w02+c05_01_w06)/2 if c05_01_w04==.
		replace c05_02_w04 = (c05_02_w02+c05_02_w06)/2 if c05_02_w04==.
		replace c01_w04 = (c01_w02+c01_w06)/2 if c01_w04==.
		replace c01_w04 = (c01_w01+c01_w06)/2 if c01_w04==.

	* Some variables have no measurement in Wave 4, so we create it based on Wave 3 and Wave 6 values
		gen c06_04_w04 = (c06_04_w03+c06_04_w06)/2
		gen c06_05_w04 = (c06_05_w03+c06_05_w06)/2
		gen c06_06_w04 = (c06_06_w03+c06_06_w06)/2
		gen c12_01_w04 = (c12_01_w03+c12_01_w06)/2
		gen c12_03_w04 = (c12_03_w03+c12_03_w06)/2
		gen c12_04_w04 = (c12_04_w03+c12_04_w06)/2
		gen c12_05_w04 = (c12_05_w03+c12_05_w06)/2
	
	* These newly create variables have some missing values, either because they do not have value for wave 3 or for wave 6. 
	* We replace by value of Wave 3, then by value of Wave 6 and finally by value of Wave 1
		replace c06_04_w04 = c06_04_w03 if c06_04_w04==.
		replace c06_05_w04 = c06_05_w03 if c06_05_w04==.
		replace c06_06_w04 = c06_06_w03 if c06_06_w04==.
		replace c12_01_w04 = c12_01_w03 if c12_01_w04==.
		replace c12_03_w04 = c12_03_w03 if c12_03_w04==.
		replace c12_04_w04 = c12_04_w03 if c12_04_w04==.
		replace c12_05_w04 = c12_05_w03 if c12_05_w04==.
		
		replace c06_04_w04 = c06_04_w06 if c06_04_w04==.
		replace c06_05_w04 = c06_05_w06 if c06_05_w04==.
		replace c06_06_w04 = c06_06_w06 if c06_06_w04==.
		replace c12_01_w04 = c12_01_w06 if c12_01_w04==.
		replace c12_03_w04 = c12_03_w06 if c12_03_w04==.
		replace c12_04_w04 = c12_04_w06 if c12_04_w04==.
		replace c12_05_w04 = c12_05_w06 if c12_05_w04==.
		
		replace c06_04_w04 = c06_04_w01 if c06_04_w04==.
		replace c06_05_w04 = c06_05_w01 if c06_05_w04==.
		replace c06_06_w04 = c06_06_w01 if c06_06_w04==.
		replace c12_01_w04 = c12_01_w01 if c12_01_w04==.
		replace c12_03_w04 = c12_03_w01 if c12_03_w04==.
		replace c12_04_w04 = c12_04_w01 if c12_04_w04==.
		replace c12_05_w04 = c12_05_w01 if c12_05_w04==.

*--------------------------------------------------------------------*
* ----- Imputation of values for dependent variables at Wave 6 ----- *
*--------------------------------------------------------------------*

	* For variables that have missing values for Wave 6 (_w06), we use the values of Wave 5 (w_05). 
		replace c32_01_w06 = c32_01_w05 if c32_01_w06==.
		replace c32_02_w06 = c32_02_w05 if c32_02_w06==.
		replace c02_w06 = c02_w05 if c02_w06==.
		replace c05_01_w06 = c05_01_w05 if c05_01_w06==.
		replace c05_02_w06 = c05_02_w05 if c05_02_w06==.
		replace c05_05_w06 = c05_05_w05 if c05_05_w06==.
		replace c05_07_w06 = c05_07_w05 if c05_07_w06==.
		replace c13_w06 = c13_w05 if c13_w06==.
		replace c01_w06 = c01_w05 if c01_w06==.
		replace c25_w06 = c25_w05 if c25_w06==.
		
	* Some variables have still missing values for Wave 6 (_w06), because they are not measured at wave 5 or have missing values at wave 5. We use the values of Wave 4 (w_04). 
		replace c32_01_w06 = c32_01_w04 if c32_01_w06==.
		replace c32_02_w06 = c32_02_w04 if c32_02_w06==.
		replace c05_05_w06 = c05_05_w04 if c05_05_w06==.
		replace c01_w06 = c01_w04 if c01_w06==.
		replace c25_w06 = c25_w04 if c25_w06==.
		replace c08_01_w06 = c08_01_w04 if c08_01_w06==.
		replace c08_02_w06 = c08_02_w04 if c08_02_w06==.
		replace c08_03_w06 = c08_03_w04 if c08_03_w06==.
		replace d02_01_w06 = d02_01_w04 if d02_01_w06==.
		replace d02_02_w06 = d02_02_w04 if d02_02_w06==.
		replace d02_03_w06 = d02_03_w04 if d02_03_w06==.
		replace f05_01_w06 = f05_01_w04 if f05_01_w06==.
		replace f05_02_w06 = f05_02_w04 if f05_02_w06==.
		replace f05_03_w06 =  f05_03_w04 if f05_03_w06==.		
		replace r15_w06 = r15_w04 if r15_w06==.
		replace r13_nredes_w06 = r13_nredes_w04 if r13_nredes_w06==.
		replace c06_04_w06 = c06_04_w04 if c06_04_w06==.
		replace c06_05_w06 = c06_05_w04 if c06_05_w06==.
		replace c06_06_w06 = c06_06_w04 if c06_06_w06==.
		
	* Some variables have still missing values for Wave 6 (_w06). We use the values of Wave 3 (w_03). 
		replace c12_01_w06 = c12_01_w03 if c12_01_w06==.
		replace c12_03_w06 = c12_03_w03 if c12_03_w06==.
		replace c12_04_w06 = c12_04_w03 if c12_04_w06==.
		replace c12_05_w06 = c12_05_w03 if c12_05_w06==.
		
	* Some variables have no measurement in Wave 6, so we create it based on Wave 5 or Wave 4
		gen c18_02_w06 = c18_02_w04
		gen c18_03_w06 = c18_03_w04
		gen c07_04_w06 = c07_04_w05
		gen c07_05_w06 = c07_05_w05
		replace c07_04_w06 = c07_04_w04 if c07_04_w06==.
		replace c07_05_w06 = c07_05_w04 if c07_05_w06==.

	* Check that there is no missing values for dependent variables
	* ssc install mdesc
	mdesc r15_w01 r13_nredes_w01 c32_01_w01 c32_02_w01  c02_w01 c06_04_w01 c06_05_w01 c06_06_w01 c05_01_w01 c05_02_w01 c05_05_w01 c05_07_w01 c13_w01 c01_w01 c12_01_w01 c12_03_w01 c12_04_w01 c12_05_w01 c08_01_w01 c08_02_w01 c08_03_w01 d02_01_w01 d02_02_w01 d02_03_w01 c18_02_w01 c18_03_w01 c07_04_w01 c07_05_w01 c25_w01 f05_01_w01 f05_02_w01 f05_03_w01 r15_w04 r13_nredes_w04 c32_01_w04 c32_02_w04  c02_w04  c05_01_w04 c05_02_w04 c05_05_w04 c05_07_w04 c13_w04 c01_w04 c08_01_w04 c08_02_w04 c08_03_w04 d02_01_w04 d02_02_w04 d02_03_w04 c18_02_w04 c18_03_w04 c07_04_w04 c07_05_w04 c25_w04 f05_01_w04 f05_02_w04 f05_03_w04 c06_04_w04 c06_05_w04 c06_06_w04 c12_01_w04 c12_03_w04 c12_04_w04 c12_05_w04 r15_w06 r13_nredes_w06 c32_01_w06 c32_02_w06  c02_w06  c05_01_w06 c05_02_w06 c05_05_w06 c05_07_w06 c13_w06 c01_w06 c08_01_w06 c08_02_w06 c08_03_w06 d02_01_w06 d02_02_w06 d02_03_w06 c25_w06 f05_01_w06 f05_02_w06 f05_03_w06 c06_04_w06 c06_05_w06 c06_06_w06 c12_01_w06 c12_03_w06 c12_04_w06 c12_05_w06 c18_02_w06 c18_03_w06 c07_04_w06 c07_05_w06

*------------------------------------------------------------*
* ----- Imputation of values for independent variables ----- *
*------------------------------------------------------------*

	* Convert NS/NR as missing values for every independent variables
		foreach var of varlist m0_sexo* m0_edad* m01* m02* ciuo88_m03* ciuo08_m03* ciuo88_m22* ///
							  ciuo08_m22* m19* m21* m29* m33* m34* m36* m37* /// 
							  fact_exp02* segmento* {    
			replace `var' = . if `var' == -888 | `var' == -999 | `var' == -666	
		}
		
	* Imputation for income: for missing values in Wave 1, we begin by imputing values from Wave 2
			
		* Assign missing values ton incoce=0
			foreach var of varlist m29* {
			replace `var' = . if `var'==0
			}	
		
		replace m29_w01=m29_w02 if m29_w01==.
		replace m29_w01=m29_w03 if m29_w01==.
		replace m29_w01=(m29_w04+m29_w05)/2 if m29_w01==. & m29_w04!=. & m29_w05!=.
		replace m29_w01=(m29_w04+m29_w06)/2 if m29_w01==. & m29_w04!=. & m29_w06!=.
		replace m29_w01=m29_w05 if m29_w01==.

		replace m29_w04 = (m29_w03+m29_w05)/2 if m29_w04==. & m29_w03!=. & m29_w05!=.
		replace m29_w04 = m29_w03 if m29_w04==. 
		replace m29_w04 = m29_w05 if m29_w04==. 
		replace m29_w04 = (m29_w01+m29_w06)/2 if m29_w04==. 

		replace m29_w06 = m29_w05 if m29_w06==. 
		replace m29_w06 = m29_w04 if m29_w06==.

	* Imputation for education (m01): there is only 1 missing value at each wave
		replace m01_w01=m01_w02 if m01_w01==.
		replace m01_w04=m01_w03 if m01_w04==.
		replace m01_w06=m01_w05 if m01_w06==.

	* Imputation for housing tenure (we make it time-constant)
		gen m33_w04 = m33_w01
		gen m33_w06 = m33_w01
	
	* Predict years at current neighborhood for waves 4 and 6 from the value of wave 1 (which has no missing value)
		gen m34_03_w04 = m34_03_w01+3
		gen m34_03_w06 = m34_03_w01+6
	
	* Imputation for marital status (m36): there is only 1 missing value at each waves
		replace m36_w01=m36_w03 if m36_w01==.
		replace m36_w04=m36_w03 if m36_w04==.
		replace m36_w06=m36_w05 if m36_w06==.
	
	* Imputation for number of children (use only wave 1), it is time-constant. We sum sons and daughters
		gen m37_w01 = (m37_01_w01+m37_02_w01)
		gen m37_w04 = m37_w01
		gen m37_w06 = m37_w01

		
*-------------------------------------------------*
* ----- Imputation of values for occupation ----- *
*-------------------------------------------------*

	* Convert ISCO 88 into ISCO 08 for Wave 1
	
replace ciuo88_m03_w01=	1111	if ciuo88_m03_w01==	1110
replace ciuo88_m03_w01=	1112	if ciuo88_m03_w01==	1120
replace ciuo88_m03_w01=	1113	if ciuo88_m03_w01==	1130
replace ciuo88_m03_w01=	1114	if ciuo88_m03_w01==	1141
replace ciuo88_m03_w01=	1114	if ciuo88_m03_w01==	1142
replace ciuo88_m03_w01=	1114	if ciuo88_m03_w01==	1143
replace ciuo88_m03_w01=	1120	if ciuo88_m03_w01==	1210
replace ciuo88_m03_w01=	1211	if ciuo88_m03_w01==	1231
replace ciuo88_m03_w01=	1211	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1212	if ciuo88_m03_w01==	1232
replace ciuo88_m03_w01=	1212	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1213	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1213	if ciuo88_m03_w01==	1239
replace ciuo88_m03_w01=	1219	if ciuo88_m03_w01==	1227
replace ciuo88_m03_w01=	1219	if ciuo88_m03_w01==	1228
replace ciuo88_m03_w01=	1219	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1219	if ciuo88_m03_w01==	1231
replace ciuo88_m03_w01=	1219	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1219	if ciuo88_m03_w01==	1318
replace ciuo88_m03_w01=	1221	if ciuo88_m03_w01==	1233
replace ciuo88_m03_w01=	1221	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1222	if ciuo88_m03_w01==	1234
replace ciuo88_m03_w01=	1222	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1223	if ciuo88_m03_w01==	1237
replace ciuo88_m03_w01=	1223	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1311	if ciuo88_m03_w01==	1221
replace ciuo88_m03_w01=	1312	if ciuo88_m03_w01==	1221
replace ciuo88_m03_w01=	1321	if ciuo88_m03_w01==	1222
replace ciuo88_m03_w01=	1321	if ciuo88_m03_w01==	1312
replace ciuo88_m03_w01=	1322	if ciuo88_m03_w01==	1222
replace ciuo88_m03_w01=	1322	if ciuo88_m03_w01==	1312
replace ciuo88_m03_w01=	1323	if ciuo88_m03_w01==	1223
replace ciuo88_m03_w01=	1323	if ciuo88_m03_w01==	1313
replace ciuo88_m03_w01=	1324	if ciuo88_m03_w01==	1226
replace ciuo88_m03_w01=	1324	if ciuo88_m03_w01==	1235
replace ciuo88_m03_w01=	1324	if ciuo88_m03_w01==	1316
replace ciuo88_m03_w01=	1330	if ciuo88_m03_w01==	1226
replace ciuo88_m03_w01=	1330	if ciuo88_m03_w01==	1236
replace ciuo88_m03_w01=	1330	if ciuo88_m03_w01==	1316
replace ciuo88_m03_w01=	1330	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1341	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1341	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1342	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1342	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1342	if ciuo88_m03_w01==	2230
replace ciuo88_m03_w01=	1343	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1343	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1343	if ciuo88_m03_w01==	2230
replace ciuo88_m03_w01=	1344	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1344	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1345	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1345	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1346	if ciuo88_m03_w01==	1227
replace ciuo88_m03_w01=	1346	if ciuo88_m03_w01==	1317
replace ciuo88_m03_w01=	1349	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1349	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1411	if ciuo88_m03_w01==	1225
replace ciuo88_m03_w01=	1411	if ciuo88_m03_w01==	1315
replace ciuo88_m03_w01=	1412	if ciuo88_m03_w01==	1225
replace ciuo88_m03_w01=	1412	if ciuo88_m03_w01==	1315
replace ciuo88_m03_w01=	1420	if ciuo88_m03_w01==	1224
replace ciuo88_m03_w01=	1420	if ciuo88_m03_w01==	1314
replace ciuo88_m03_w01=	1431	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	1439	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	1439	if ciuo88_m03_w01==	1319
replace ciuo88_m03_w01=	2111	if ciuo88_m03_w01==	2111
replace ciuo88_m03_w01=	2112	if ciuo88_m03_w01==	2112
replace ciuo88_m03_w01=	2113	if ciuo88_m03_w01==	2113
replace ciuo88_m03_w01=	2114	if ciuo88_m03_w01==	2114
replace ciuo88_m03_w01=	2120	if ciuo88_m03_w01==	2121
replace ciuo88_m03_w01=	2120	if ciuo88_m03_w01==	2122
replace ciuo88_m03_w01=	2131	if ciuo88_m03_w01==	2211
replace ciuo88_m03_w01=	2131	if ciuo88_m03_w01==	2212
replace ciuo88_m03_w01=	2132	if ciuo88_m03_w01==	2213
replace ciuo88_m03_w01=	2132	if ciuo88_m03_w01==	3213
replace ciuo88_m03_w01=	2133	if ciuo88_m03_w01==	2211
replace ciuo88_m03_w01=	2141	if ciuo88_m03_w01==	2149
replace ciuo88_m03_w01=	2142	if ciuo88_m03_w01==	2142
replace ciuo88_m03_w01=	2143	if ciuo88_m03_w01==	2149
replace ciuo88_m03_w01=	2144	if ciuo88_m03_w01==	2145
replace ciuo88_m03_w01=	2145	if ciuo88_m03_w01==	2146
replace ciuo88_m03_w01=	2146	if ciuo88_m03_w01==	2147
replace ciuo88_m03_w01=	2149	if ciuo88_m03_w01==	2149
replace ciuo88_m03_w01=	2151	if ciuo88_m03_w01==	2143
replace ciuo88_m03_w01=	2152	if ciuo88_m03_w01==	2144
replace ciuo88_m03_w01=	2153	if ciuo88_m03_w01==	2144
replace ciuo88_m03_w01=	2161	if ciuo88_m03_w01==	2141
replace ciuo88_m03_w01=	2162	if ciuo88_m03_w01==	2141
replace ciuo88_m03_w01=	2163	if ciuo88_m03_w01==	3471
replace ciuo88_m03_w01=	2164	if ciuo88_m03_w01==	2141
replace ciuo88_m03_w01=	2165	if ciuo88_m03_w01==	2148
replace ciuo88_m03_w01=	2166	if ciuo88_m03_w01==	3471
replace ciuo88_m03_w01=	2211	if ciuo88_m03_w01==	2221
replace ciuo88_m03_w01=	2212	if ciuo88_m03_w01==	2212
replace ciuo88_m03_w01=	2212	if ciuo88_m03_w01==	2221
replace ciuo88_m03_w01=	2221	if ciuo88_m03_w01==	2230
replace ciuo88_m03_w01=	2222	if ciuo88_m03_w01==	2230
replace ciuo88_m03_w01=	2230	if ciuo88_m03_w01==	3229
replace ciuo88_m03_w01=	2230	if ciuo88_m03_w01==	3241
replace ciuo88_m03_w01=	2240	if ciuo88_m03_w01==	3221
replace ciuo88_m03_w01=	2250	if ciuo88_m03_w01==	2212
replace ciuo88_m03_w01=	2250	if ciuo88_m03_w01==	2223
replace ciuo88_m03_w01=	2261	if ciuo88_m03_w01==	2222
replace ciuo88_m03_w01=	2262	if ciuo88_m03_w01==	2113
replace ciuo88_m03_w01=	2262	if ciuo88_m03_w01==	2224
replace ciuo88_m03_w01=	2263	if ciuo88_m03_w01==	2229
replace ciuo88_m03_w01=	2263	if ciuo88_m03_w01==	2412
replace ciuo88_m03_w01=	2263	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	2263	if ciuo88_m03_w01==	3222
replace ciuo88_m03_w01=	2264	if ciuo88_m03_w01==	3226
replace ciuo88_m03_w01=	2265	if ciuo88_m03_w01==	3223
replace ciuo88_m03_w01=	2266	if ciuo88_m03_w01==	3229
replace ciuo88_m03_w01=	2267	if ciuo88_m03_w01==	3224
replace ciuo88_m03_w01=	2267	if ciuo88_m03_w01==	3229
replace ciuo88_m03_w01=	2269	if ciuo88_m03_w01==	2229
replace ciuo88_m03_w01=	2269	if ciuo88_m03_w01==	3226
replace ciuo88_m03_w01=	2269	if ciuo88_m03_w01==	3229
replace ciuo88_m03_w01=	2310	if ciuo88_m03_w01==	2310
replace ciuo88_m03_w01=	2320	if ciuo88_m03_w01==	2310
replace ciuo88_m03_w01=	2320	if ciuo88_m03_w01==	2320
replace ciuo88_m03_w01=	2330	if ciuo88_m03_w01==	2320
replace ciuo88_m03_w01=	2341	if ciuo88_m03_w01==	2331
replace ciuo88_m03_w01=	2341	if ciuo88_m03_w01==	3310
replace ciuo88_m03_w01=	2342	if ciuo88_m03_w01==	2332
replace ciuo88_m03_w01=	2342	if ciuo88_m03_w01==	3320
replace ciuo88_m03_w01=	2351	if ciuo88_m03_w01==	2351
replace ciuo88_m03_w01=	2351	if ciuo88_m03_w01==	2352
replace ciuo88_m03_w01=	2352	if ciuo88_m03_w01==	2340
replace ciuo88_m03_w01=	2352	if ciuo88_m03_w01==	3330
replace ciuo88_m03_w01=	2353	if ciuo88_m03_w01==	2359
replace ciuo88_m03_w01=	2353	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	2354	if ciuo88_m03_w01==	2359
replace ciuo88_m03_w01=	2355	if ciuo88_m03_w01==	2359
replace ciuo88_m03_w01=	2355	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	2356	if ciuo88_m03_w01==	2359
replace ciuo88_m03_w01=	2356	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	2359	if ciuo88_m03_w01==	2359
replace ciuo88_m03_w01=	2359	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	2411	if ciuo88_m03_w01==	2411
replace ciuo88_m03_w01=	2412	if ciuo88_m03_w01==	2411
replace ciuo88_m03_w01=	2412	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	2412	if ciuo88_m03_w01==	3411
replace ciuo88_m03_w01=	2413	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	2421	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	2422	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	2423	if ciuo88_m03_w01==	2412
replace ciuo88_m03_w01=	2424	if ciuo88_m03_w01==	2412
replace ciuo88_m03_w01=	2431	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	2432	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	2433	if ciuo88_m03_w01==	3415
replace ciuo88_m03_w01=	2434	if ciuo88_m03_w01==	3415
replace ciuo88_m03_w01=	2511	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2512	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2513	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2513	if ciuo88_m03_w01==	2132
replace ciuo88_m03_w01=	2513	if ciuo88_m03_w01==	2139
replace ciuo88_m03_w01=	2514	if ciuo88_m03_w01==	2132
replace ciuo88_m03_w01=	2519	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2519	if ciuo88_m03_w01==	2132
replace ciuo88_m03_w01=	2519	if ciuo88_m03_w01==	2139
replace ciuo88_m03_w01=	2521	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2522	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2523	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2529	if ciuo88_m03_w01==	2131
replace ciuo88_m03_w01=	2529	if ciuo88_m03_w01==	2132
replace ciuo88_m03_w01=	2529	if ciuo88_m03_w01==	2139
replace ciuo88_m03_w01=	2611	if ciuo88_m03_w01==	2421
replace ciuo88_m03_w01=	2612	if ciuo88_m03_w01==	2422
replace ciuo88_m03_w01=	2619	if ciuo88_m03_w01==	2429
replace ciuo88_m03_w01=	2621	if ciuo88_m03_w01==	2431
replace ciuo88_m03_w01=	2622	if ciuo88_m03_w01==	2432
replace ciuo88_m03_w01=	2631	if ciuo88_m03_w01==	2441
replace ciuo88_m03_w01=	2632	if ciuo88_m03_w01==	2442
replace ciuo88_m03_w01=	2633	if ciuo88_m03_w01==	2443
replace ciuo88_m03_w01=	2634	if ciuo88_m03_w01==	2445
replace ciuo88_m03_w01=	2635	if ciuo88_m03_w01==	2446
replace ciuo88_m03_w01=	2636	if ciuo88_m03_w01==	2460
replace ciuo88_m03_w01=	2641	if ciuo88_m03_w01==	2451
replace ciuo88_m03_w01=	2642	if ciuo88_m03_w01==	2451
replace ciuo88_m03_w01=	2642	if ciuo88_m03_w01==	3472
replace ciuo88_m03_w01=	2643	if ciuo88_m03_w01==	2444
replace ciuo88_m03_w01=	2651	if ciuo88_m03_w01==	2452
replace ciuo88_m03_w01=	2652	if ciuo88_m03_w01==	2453
replace ciuo88_m03_w01=	2652	if ciuo88_m03_w01==	3473
replace ciuo88_m03_w01=	2653	if ciuo88_m03_w01==	2454
replace ciuo88_m03_w01=	2653	if ciuo88_m03_w01==	3473
replace ciuo88_m03_w01=	2654	if ciuo88_m03_w01==	1229
replace ciuo88_m03_w01=	2654	if ciuo88_m03_w01==	2455
replace ciuo88_m03_w01=	2655	if ciuo88_m03_w01==	2455
replace ciuo88_m03_w01=	2656	if ciuo88_m03_w01==	3472
replace ciuo88_m03_w01=	2659	if ciuo88_m03_w01==	3474
replace ciuo88_m03_w01=	3111	if ciuo88_m03_w01==	3111
replace ciuo88_m03_w01=	3112	if ciuo88_m03_w01==	3112
replace ciuo88_m03_w01=	3112	if ciuo88_m03_w01==	3151
replace ciuo88_m03_w01=	3113	if ciuo88_m03_w01==	3113
replace ciuo88_m03_w01=	3113	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	3114	if ciuo88_m03_w01==	3114
replace ciuo88_m03_w01=	3114	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	3115	if ciuo88_m03_w01==	3115
replace ciuo88_m03_w01=	3115	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	3116	if ciuo88_m03_w01==	3116
replace ciuo88_m03_w01=	3117	if ciuo88_m03_w01==	3117
replace ciuo88_m03_w01=	3117	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	3118	if ciuo88_m03_w01==	3118
replace ciuo88_m03_w01=	3119	if ciuo88_m03_w01==	3119
replace ciuo88_m03_w01=	3121	if ciuo88_m03_w01==	7111
replace ciuo88_m03_w01=	3121	if ciuo88_m03_w01==	8111
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8171
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8172
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8211
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8221
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8222
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8223
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8224
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8229
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8231
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8232
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8232
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8232
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8240
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8251
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8252
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8253
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8261
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8262
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8263
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8264
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8265
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8266
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8269
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8271
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8272
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8273
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8274
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8275
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8276
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8277
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8278
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8279
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8281
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8282
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8283
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8284
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8285
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8286
replace ciuo88_m03_w01=	3122	if ciuo88_m03_w01==	8290
replace ciuo88_m03_w01=	3123	if ciuo88_m03_w01==	1223
replace ciuo88_m03_w01=	3123	if ciuo88_m03_w01==	7129
replace ciuo88_m03_w01=	3131	if ciuo88_m03_w01==	8161
replace ciuo88_m03_w01=	3132	if ciuo88_m03_w01==	8163
replace ciuo88_m03_w01=	3133	if ciuo88_m03_w01==	8152
replace ciuo88_m03_w01=	3133	if ciuo88_m03_w01==	8153
replace ciuo88_m03_w01=	3133	if ciuo88_m03_w01==	8154
replace ciuo88_m03_w01=	3133	if ciuo88_m03_w01==	8159
replace ciuo88_m03_w01=	3134	if ciuo88_m03_w01==	8155
replace ciuo88_m03_w01=	3135	if ciuo88_m03_w01==	8121
replace ciuo88_m03_w01=	3135	if ciuo88_m03_w01==	8122
replace ciuo88_m03_w01=	3135	if ciuo88_m03_w01==	8123
replace ciuo88_m03_w01=	3135	if ciuo88_m03_w01==	8124
replace ciuo88_m03_w01=	3139	if ciuo88_m03_w01==	3123
replace ciuo88_m03_w01=	3139	if ciuo88_m03_w01==	8142
replace ciuo88_m03_w01=	3139	if ciuo88_m03_w01==	8143
replace ciuo88_m03_w01=	3139	if ciuo88_m03_w01==	8171
replace ciuo88_m03_w01=	3139	if ciuo88_m03_w01==	8172
replace ciuo88_m03_w01=	3141	if ciuo88_m03_w01==	3211
replace ciuo88_m03_w01=	3142	if ciuo88_m03_w01==	3212
replace ciuo88_m03_w01=	3143	if ciuo88_m03_w01==	3212
replace ciuo88_m03_w01=	3151	if ciuo88_m03_w01==	3141
replace ciuo88_m03_w01=	3152	if ciuo88_m03_w01==	3142
replace ciuo88_m03_w01=	3153	if ciuo88_m03_w01==	3143
replace ciuo88_m03_w01=	3153	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	3154	if ciuo88_m03_w01==	3144
replace ciuo88_m03_w01=	3155	if ciuo88_m03_w01==	3145
replace ciuo88_m03_w01=	3211	if ciuo88_m03_w01==	3133
replace ciuo88_m03_w01=	3212	if ciuo88_m03_w01==	3211
replace ciuo88_m03_w01=	3213	if ciuo88_m03_w01==	3228
replace ciuo88_m03_w01=	3214	if ciuo88_m03_w01==	7311
replace ciuo88_m03_w01=	3221	if ciuo88_m03_w01==	2230
replace ciuo88_m03_w01=	3221	if ciuo88_m03_w01==	3231
replace ciuo88_m03_w01=	3222	if ciuo88_m03_w01==	2230
replace ciuo88_m03_w01=	3222	if ciuo88_m03_w01==	3232
replace ciuo88_m03_w01=	3230	if ciuo88_m03_w01==	3241
replace ciuo88_m03_w01=	3240	if ciuo88_m03_w01==	3227
replace ciuo88_m03_w01=	3251	if ciuo88_m03_w01==	3225
replace ciuo88_m03_w01=	3252	if ciuo88_m03_w01==	4143
replace ciuo88_m03_w01=	3253	if ciuo88_m03_w01==	3221
replace ciuo88_m03_w01=	3254	if ciuo88_m03_w01==	3224
replace ciuo88_m03_w01=	3255	if ciuo88_m03_w01==	3226
replace ciuo88_m03_w01=	3256	if ciuo88_m03_w01==	3221
replace ciuo88_m03_w01=	3257	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	3257	if ciuo88_m03_w01==	3222
replace ciuo88_m03_w01=	3258	if ciuo88_m03_w01==	5132
replace ciuo88_m03_w01=	3259	if ciuo88_m03_w01==	3226
replace ciuo88_m03_w01=	3259	if ciuo88_m03_w01==	3229
replace ciuo88_m03_w01=	3311	if ciuo88_m03_w01==	3411
replace ciuo88_m03_w01=	3312	if ciuo88_m03_w01==	3419
replace ciuo88_m03_w01=	3313	if ciuo88_m03_w01==	3433
replace ciuo88_m03_w01=	3313	if ciuo88_m03_w01==	3434
replace ciuo88_m03_w01=	3314	if ciuo88_m03_w01==	3434
replace ciuo88_m03_w01=	3315	if ciuo88_m03_w01==	3417
replace ciuo88_m03_w01=	3321	if ciuo88_m03_w01==	3412
replace ciuo88_m03_w01=	3322	if ciuo88_m03_w01==	3415
replace ciuo88_m03_w01=	3323	if ciuo88_m03_w01==	3416
replace ciuo88_m03_w01=	3324	if ciuo88_m03_w01==	3421
replace ciuo88_m03_w01=	3331	if ciuo88_m03_w01==	3422
replace ciuo88_m03_w01=	3332	if ciuo88_m03_w01==	3414
replace ciuo88_m03_w01=	3332	if ciuo88_m03_w01==	3439
replace ciuo88_m03_w01=	3333	if ciuo88_m03_w01==	3423
replace ciuo88_m03_w01=	3334	if ciuo88_m03_w01==	3413
replace ciuo88_m03_w01=	3339	if ciuo88_m03_w01==	2419
replace ciuo88_m03_w01=	3339	if ciuo88_m03_w01==	3414
replace ciuo88_m03_w01=	3339	if ciuo88_m03_w01==	3417
replace ciuo88_m03_w01=	3339	if ciuo88_m03_w01==	3429
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	3431
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4111
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4112
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4114
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4115
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4115
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4121
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4122
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4131
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4132
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4133
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4141
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4142
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4143
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4144
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4190
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4222
replace ciuo88_m03_w01=	3341	if ciuo88_m03_w01==	4223
replace ciuo88_m03_w01=	3342	if ciuo88_m03_w01==	3431
replace ciuo88_m03_w01=	3342	if ciuo88_m03_w01==	4115
replace ciuo88_m03_w01=	3343	if ciuo88_m03_w01==	3431
replace ciuo88_m03_w01=	3343	if ciuo88_m03_w01==	3439
replace ciuo88_m03_w01=	3344	if ciuo88_m03_w01==	3431
replace ciuo88_m03_w01=	3344	if ciuo88_m03_w01==	4115
replace ciuo88_m03_w01=	3351	if ciuo88_m03_w01==	3441
replace ciuo88_m03_w01=	3352	if ciuo88_m03_w01==	3442
replace ciuo88_m03_w01=	3353	if ciuo88_m03_w01==	3443
replace ciuo88_m03_w01=	3354	if ciuo88_m03_w01==	3444
replace ciuo88_m03_w01=	3355	if ciuo88_m03_w01==	3450
replace ciuo88_m03_w01=	3359	if ciuo88_m03_w01==	3151
replace ciuo88_m03_w01=	3359	if ciuo88_m03_w01==	3439
replace ciuo88_m03_w01=	3359	if ciuo88_m03_w01==	3449
replace ciuo88_m03_w01=	3411	if ciuo88_m03_w01==	3432
replace ciuo88_m03_w01=	3411	if ciuo88_m03_w01==	3450
replace ciuo88_m03_w01=	3412	if ciuo88_m03_w01==	3460
replace ciuo88_m03_w01=	3413	if ciuo88_m03_w01==	3480
replace ciuo88_m03_w01=	3421	if ciuo88_m03_w01==	3475
replace ciuo88_m03_w01=	3422	if ciuo88_m03_w01==	3475
replace ciuo88_m03_w01=	3423	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	3423	if ciuo88_m03_w01==	3475
replace ciuo88_m03_w01=	3431	if ciuo88_m03_w01==	3131
replace ciuo88_m03_w01=	3432	if ciuo88_m03_w01==	3471
replace ciuo88_m03_w01=	3433	if ciuo88_m03_w01==	3439
replace ciuo88_m03_w01=	3433	if ciuo88_m03_w01==	3471
replace ciuo88_m03_w01=	3434	if ciuo88_m03_w01==	5122
replace ciuo88_m03_w01=	3435	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	3511	if ciuo88_m03_w01==	3122
replace ciuo88_m03_w01=	3512	if ciuo88_m03_w01==	3121
replace ciuo88_m03_w01=	3513	if ciuo88_m03_w01==	2139
replace ciuo88_m03_w01=	3513	if ciuo88_m03_w01==	3121
replace ciuo88_m03_w01=	3514	if ciuo88_m03_w01==	3121
replace ciuo88_m03_w01=	3514	if ciuo88_m03_w01==	3122
replace ciuo88_m03_w01=	3521	if ciuo88_m03_w01==	3131
replace ciuo88_m03_w01=	3521	if ciuo88_m03_w01==	3132
replace ciuo88_m03_w01=	3522	if ciuo88_m03_w01==	3114
replace ciuo88_m03_w01=	3522	if ciuo88_m03_w01==	3132
replace ciuo88_m03_w01=	4110	if ciuo88_m03_w01==	4190
replace ciuo88_m03_w01=	4120	if ciuo88_m03_w01==	4115
replace ciuo88_m03_w01=	4131	if ciuo88_m03_w01==	4111
replace ciuo88_m03_w01=	4131	if ciuo88_m03_w01==	4112
replace ciuo88_m03_w01=	4132	if ciuo88_m03_w01==	4113
replace ciuo88_m03_w01=	4132	if ciuo88_m03_w01==	4114
replace ciuo88_m03_w01=	4211	if ciuo88_m03_w01==	4211
replace ciuo88_m03_w01=	4211	if ciuo88_m03_w01==	4212
replace ciuo88_m03_w01=	4212	if ciuo88_m03_w01==	4211
replace ciuo88_m03_w01=	4212	if ciuo88_m03_w01==	4213
replace ciuo88_m03_w01=	4213	if ciuo88_m03_w01==	4214
replace ciuo88_m03_w01=	4214	if ciuo88_m03_w01==	4215
replace ciuo88_m03_w01=	4221	if ciuo88_m03_w01==	3414
replace ciuo88_m03_w01=	4221	if ciuo88_m03_w01==	4221
replace ciuo88_m03_w01=	4222	if ciuo88_m03_w01==	4222
replace ciuo88_m03_w01=	4223	if ciuo88_m03_w01==	4223
replace ciuo88_m03_w01=	4224	if ciuo88_m03_w01==	4222
replace ciuo88_m03_w01=	4225	if ciuo88_m03_w01==	4222
replace ciuo88_m03_w01=	4226	if ciuo88_m03_w01==	4222
replace ciuo88_m03_w01=	4227	if ciuo88_m03_w01==	4190
replace ciuo88_m03_w01=	4229	if ciuo88_m03_w01==	4222
replace ciuo88_m03_w01=	4311	if ciuo88_m03_w01==	4121
replace ciuo88_m03_w01=	4312	if ciuo88_m03_w01==	4122
replace ciuo88_m03_w01=	4313	if ciuo88_m03_w01==	4121
replace ciuo88_m03_w01=	4321	if ciuo88_m03_w01==	4131
replace ciuo88_m03_w01=	4322	if ciuo88_m03_w01==	4132
replace ciuo88_m03_w01=	4323	if ciuo88_m03_w01==	4133
replace ciuo88_m03_w01=	4411	if ciuo88_m03_w01==	4141
replace ciuo88_m03_w01=	4412	if ciuo88_m03_w01==	4142
replace ciuo88_m03_w01=	4413	if ciuo88_m03_w01==	4143
replace ciuo88_m03_w01=	4414	if ciuo88_m03_w01==	4144
replace ciuo88_m03_w01=	4415	if ciuo88_m03_w01==	4141
replace ciuo88_m03_w01=	4416	if ciuo88_m03_w01==	4190
replace ciuo88_m03_w01=	4419	if ciuo88_m03_w01==	4190
replace ciuo88_m03_w01=	5111	if ciuo88_m03_w01==	5111
replace ciuo88_m03_w01=	5112	if ciuo88_m03_w01==	5112
replace ciuo88_m03_w01=	5113	if ciuo88_m03_w01==	5113
replace ciuo88_m03_w01=	5120	if ciuo88_m03_w01==	5122
replace ciuo88_m03_w01=	5131	if ciuo88_m03_w01==	5123
replace ciuo88_m03_w01=	5132	if ciuo88_m03_w01==	5123
replace ciuo88_m03_w01=	5141	if ciuo88_m03_w01==	5141
replace ciuo88_m03_w01=	5142	if ciuo88_m03_w01==	5141
replace ciuo88_m03_w01=	5151	if ciuo88_m03_w01==	5121
replace ciuo88_m03_w01=	5152	if ciuo88_m03_w01==	5121
replace ciuo88_m03_w01=	5153	if ciuo88_m03_w01==	9141
replace ciuo88_m03_w01=	5161	if ciuo88_m03_w01==	5151
replace ciuo88_m03_w01=	5161	if ciuo88_m03_w01==	5152
replace ciuo88_m03_w01=	5162	if ciuo88_m03_w01==	5142
replace ciuo88_m03_w01=	5163	if ciuo88_m03_w01==	5143
replace ciuo88_m03_w01=	5164	if ciuo88_m03_w01==	5139
replace ciuo88_m03_w01=	5164	if ciuo88_m03_w01==	6129
replace ciuo88_m03_w01=	5165	if ciuo88_m03_w01==	3340
replace ciuo88_m03_w01=	5169	if ciuo88_m03_w01==	5149
replace ciuo88_m03_w01=	5169	if ciuo88_m03_w01==	3242
replace ciuo88_m03_w01=	5211	if ciuo88_m03_w01==	5230
replace ciuo88_m03_w01=	5212	if ciuo88_m03_w01==	9111
replace ciuo88_m03_w01=	5221	if ciuo88_m03_w01==	1314
replace ciuo88_m03_w01=	5222	if ciuo88_m03_w01==	5220
replace ciuo88_m03_w01=	5223	if ciuo88_m03_w01==	5220
replace ciuo88_m03_w01=	5230	if ciuo88_m03_w01==	4211
replace ciuo88_m03_w01=	5241	if ciuo88_m03_w01==	5210
replace ciuo88_m03_w01=	5242	if ciuo88_m03_w01==	5220
replace ciuo88_m03_w01=	5243	if ciuo88_m03_w01==	9113
replace ciuo88_m03_w01=	5244	if ciuo88_m03_w01==	9113
replace ciuo88_m03_w01=	5245	if ciuo88_m03_w01==	5220
replace ciuo88_m03_w01=	5246	if ciuo88_m03_w01==	5220
replace ciuo88_m03_w01=	5246	if ciuo88_m03_w01==	5230
replace ciuo88_m03_w01=	5249	if ciuo88_m03_w01==	5220
replace ciuo88_m03_w01=	5311	if ciuo88_m03_w01==	5131
replace ciuo88_m03_w01=	5312	if ciuo88_m03_w01==	5131
replace ciuo88_m03_w01=	5321	if ciuo88_m03_w01==	5132
replace ciuo88_m03_w01=	5322	if ciuo88_m03_w01==	5133
replace ciuo88_m03_w01=	5329	if ciuo88_m03_w01==	5132
replace ciuo88_m03_w01=	5329	if ciuo88_m03_w01==	5139
replace ciuo88_m03_w01=	5411	if ciuo88_m03_w01==	5161
replace ciuo88_m03_w01=	5412	if ciuo88_m03_w01==	5162
replace ciuo88_m03_w01=	5413	if ciuo88_m03_w01==	5163
replace ciuo88_m03_w01=	5414	if ciuo88_m03_w01==	5169
replace ciuo88_m03_w01=	5414	if ciuo88_m03_w01==	9152
replace ciuo88_m03_w01=	5419	if ciuo88_m03_w01==	5169
replace ciuo88_m03_w01=	6111	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6111	if ciuo88_m03_w01==	6111
replace ciuo88_m03_w01=	6112	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6112	if ciuo88_m03_w01==	6112
replace ciuo88_m03_w01=	6113	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6113	if ciuo88_m03_w01==	6113
replace ciuo88_m03_w01=	6114	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6114	if ciuo88_m03_w01==	6114
replace ciuo88_m03_w01=	6121	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6121	if ciuo88_m03_w01==	6121
replace ciuo88_m03_w01=	6121	if ciuo88_m03_w01==	6124
replace ciuo88_m03_w01=	6122	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6122	if ciuo88_m03_w01==	6122
replace ciuo88_m03_w01=	6122	if ciuo88_m03_w01==	6124
replace ciuo88_m03_w01=	6123	if ciuo88_m03_w01==	6123
replace ciuo88_m03_w01=	6123	if ciuo88_m03_w01==	6124
replace ciuo88_m03_w01=	6129	if ciuo88_m03_w01==	6129
replace ciuo88_m03_w01=	6130	if ciuo88_m03_w01==	1311
replace ciuo88_m03_w01=	6130	if ciuo88_m03_w01==	6130
replace ciuo88_m03_w01=	6210	if ciuo88_m03_w01==	6141
replace ciuo88_m03_w01=	6210	if ciuo88_m03_w01==	6142
replace ciuo88_m03_w01=	6221	if ciuo88_m03_w01==	6151
replace ciuo88_m03_w01=	6222	if ciuo88_m03_w01==	6152
replace ciuo88_m03_w01=	6223	if ciuo88_m03_w01==	6153
replace ciuo88_m03_w01=	6224	if ciuo88_m03_w01==	6154
replace ciuo88_m03_w01=	6310	if ciuo88_m03_w01==	6210
replace ciuo88_m03_w01=	6320	if ciuo88_m03_w01==	6210
replace ciuo88_m03_w01=	6330	if ciuo88_m03_w01==	6210
replace ciuo88_m03_w01=	6340	if ciuo88_m03_w01==	6210
replace ciuo88_m03_w01=	7111	if ciuo88_m03_w01==	7121
replace ciuo88_m03_w01=	7111	if ciuo88_m03_w01==	7129
replace ciuo88_m03_w01=	7112	if ciuo88_m03_w01==	7122
replace ciuo88_m03_w01=	7113	if ciuo88_m03_w01==	7113
replace ciuo88_m03_w01=	7113	if ciuo88_m03_w01==	7122
replace ciuo88_m03_w01=	7114	if ciuo88_m03_w01==	7123
replace ciuo88_m03_w01=	7115	if ciuo88_m03_w01==	7124
replace ciuo88_m03_w01=	7119	if ciuo88_m03_w01==	7129
replace ciuo88_m03_w01=	7121	if ciuo88_m03_w01==	7131
replace ciuo88_m03_w01=	7122	if ciuo88_m03_w01==	7132
replace ciuo88_m03_w01=	7123	if ciuo88_m03_w01==	7133
replace ciuo88_m03_w01=	7124	if ciuo88_m03_w01==	7134
replace ciuo88_m03_w01=	7125	if ciuo88_m03_w01==	7135
replace ciuo88_m03_w01=	7126	if ciuo88_m03_w01==	7136
replace ciuo88_m03_w01=	7127	if ciuo88_m03_w01==	7136
replace ciuo88_m03_w01=	7127	if ciuo88_m03_w01==	7233
replace ciuo88_m03_w01=	7131	if ciuo88_m03_w01==	7141
replace ciuo88_m03_w01=	7132	if ciuo88_m03_w01==	7142
replace ciuo88_m03_w01=	7133	if ciuo88_m03_w01==	7143
replace ciuo88_m03_w01=	7211	if ciuo88_m03_w01==	7211
replace ciuo88_m03_w01=	7212	if ciuo88_m03_w01==	7212
replace ciuo88_m03_w01=	7213	if ciuo88_m03_w01==	7213
replace ciuo88_m03_w01=	7214	if ciuo88_m03_w01==	7214
replace ciuo88_m03_w01=	7215	if ciuo88_m03_w01==	7215
replace ciuo88_m03_w01=	7221	if ciuo88_m03_w01==	7221
replace ciuo88_m03_w01=	7222	if ciuo88_m03_w01==	7222
replace ciuo88_m03_w01=	7223	if ciuo88_m03_w01==	7223
replace ciuo88_m03_w01=	7223	if ciuo88_m03_w01==	8211
replace ciuo88_m03_w01=	7224	if ciuo88_m03_w01==	7224
replace ciuo88_m03_w01=	7231	if ciuo88_m03_w01==	7231
replace ciuo88_m03_w01=	7232	if ciuo88_m03_w01==	7232
replace ciuo88_m03_w01=	7233	if ciuo88_m03_w01==	7233
replace ciuo88_m03_w01=	7234	if ciuo88_m03_w01==	7231
replace ciuo88_m03_w01=	7311	if ciuo88_m03_w01==	7311
replace ciuo88_m03_w01=	7312	if ciuo88_m03_w01==	7312
replace ciuo88_m03_w01=	7313	if ciuo88_m03_w01==	7313
replace ciuo88_m03_w01=	7314	if ciuo88_m03_w01==	7321
replace ciuo88_m03_w01=	7315	if ciuo88_m03_w01==	7322
replace ciuo88_m03_w01=	7316	if ciuo88_m03_w01==	3471
replace ciuo88_m03_w01=	7316	if ciuo88_m03_w01==	7323
replace ciuo88_m03_w01=	7316	if ciuo88_m03_w01==	7324
replace ciuo88_m03_w01=	7317	if ciuo88_m03_w01==	7331
replace ciuo88_m03_w01=	7317	if ciuo88_m03_w01==	7424
replace ciuo88_m03_w01=	7318	if ciuo88_m03_w01==	7332
replace ciuo88_m03_w01=	7318	if ciuo88_m03_w01==	7431
replace ciuo88_m03_w01=	7318	if ciuo88_m03_w01==	7432
replace ciuo88_m03_w01=	7319	if ciuo88_m03_w01==	7331
replace ciuo88_m03_w01=	7321	if ciuo88_m03_w01==	7341
replace ciuo88_m03_w01=	7321	if ciuo88_m03_w01==	7342
replace ciuo88_m03_w01=	7321	if ciuo88_m03_w01==	7343
replace ciuo88_m03_w01=	7322	if ciuo88_m03_w01==	7341
replace ciuo88_m03_w01=	7322	if ciuo88_m03_w01==	7346
replace ciuo88_m03_w01=	7322	if ciuo88_m03_w01==	8251
replace ciuo88_m03_w01=	7323	if ciuo88_m03_w01==	7345
replace ciuo88_m03_w01=	7323	if ciuo88_m03_w01==	8252
replace ciuo88_m03_w01=	7411	if ciuo88_m03_w01==	7137
replace ciuo88_m03_w01=	7412	if ciuo88_m03_w01==	7241
replace ciuo88_m03_w01=	7413	if ciuo88_m03_w01==	7245
replace ciuo88_m03_w01=	7421	if ciuo88_m03_w01==	7242
replace ciuo88_m03_w01=	7421	if ciuo88_m03_w01==	7243
replace ciuo88_m03_w01=	7422	if ciuo88_m03_w01==	7242
replace ciuo88_m03_w01=	7422	if ciuo88_m03_w01==	7243
replace ciuo88_m03_w01=	7422	if ciuo88_m03_w01==	7244
replace ciuo88_m03_w01=	7422	if ciuo88_m03_w01==	7245
replace ciuo88_m03_w01=	7511	if ciuo88_m03_w01==	7411
replace ciuo88_m03_w01=	7512	if ciuo88_m03_w01==	7412
replace ciuo88_m03_w01=	7513	if ciuo88_m03_w01==	7413
replace ciuo88_m03_w01=	7514	if ciuo88_m03_w01==	7414
replace ciuo88_m03_w01=	7515	if ciuo88_m03_w01==	7415
replace ciuo88_m03_w01=	7516	if ciuo88_m03_w01==	7416
replace ciuo88_m03_w01=	7521	if ciuo88_m03_w01==	7421
replace ciuo88_m03_w01=	7522	if ciuo88_m03_w01==	7422
replace ciuo88_m03_w01=	7523	if ciuo88_m03_w01==	7423
replace ciuo88_m03_w01=	7523	if ciuo88_m03_w01==	8240
replace ciuo88_m03_w01=	7531	if ciuo88_m03_w01==	7433
replace ciuo88_m03_w01=	7531	if ciuo88_m03_w01==	7434
replace ciuo88_m03_w01=	7532	if ciuo88_m03_w01==	7435
replace ciuo88_m03_w01=	7533	if ciuo88_m03_w01==	7436
replace ciuo88_m03_w01=	7534	if ciuo88_m03_w01==	7437
replace ciuo88_m03_w01=	7535	if ciuo88_m03_w01==	7441
replace ciuo88_m03_w01=	7536	if ciuo88_m03_w01==	7442
replace ciuo88_m03_w01=	7541	if ciuo88_m03_w01==	6152
replace ciuo88_m03_w01=	7541	if ciuo88_m03_w01==	7216
replace ciuo88_m03_w01=	7542	if ciuo88_m03_w01==	7112
replace ciuo88_m03_w01=	7543	if ciuo88_m03_w01==	3152
replace ciuo88_m03_w01=	7544	if ciuo88_m03_w01==	7143
replace ciuo88_m03_w01=	7549	if ciuo88_m03_w01==	7322
replace ciuo88_m03_w01=	8111	if ciuo88_m03_w01==	7111
replace ciuo88_m03_w01=	8111	if ciuo88_m03_w01==	8111
replace ciuo88_m03_w01=	8112	if ciuo88_m03_w01==	8112
replace ciuo88_m03_w01=	8113	if ciuo88_m03_w01==	8113
replace ciuo88_m03_w01=	8114	if ciuo88_m03_w01==	8212
replace ciuo88_m03_w01=	8121	if ciuo88_m03_w01==	8121
replace ciuo88_m03_w01=	8121	if ciuo88_m03_w01==	8122
replace ciuo88_m03_w01=	8121	if ciuo88_m03_w01==	8123
replace ciuo88_m03_w01=	8121	if ciuo88_m03_w01==	8124
replace ciuo88_m03_w01=	8122	if ciuo88_m03_w01==	8223
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8151
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8152
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8153
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8154
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8155
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8159
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8221
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8222
replace ciuo88_m03_w01=	8131	if ciuo88_m03_w01==	8229
replace ciuo88_m03_w01=	8132	if ciuo88_m03_w01==	7344
replace ciuo88_m03_w01=	8132	if ciuo88_m03_w01==	8224
replace ciuo88_m03_w01=	8141	if ciuo88_m03_w01==	8231
replace ciuo88_m03_w01=	8142	if ciuo88_m03_w01==	8232
replace ciuo88_m03_w01=	8143	if ciuo88_m03_w01==	8253
replace ciuo88_m03_w01=	8151	if ciuo88_m03_w01==	8261
replace ciuo88_m03_w01=	8152	if ciuo88_m03_w01==	7432
replace ciuo88_m03_w01=	8152	if ciuo88_m03_w01==	8262
replace ciuo88_m03_w01=	8153	if ciuo88_m03_w01==	8263
replace ciuo88_m03_w01=	8154	if ciuo88_m03_w01==	8264
replace ciuo88_m03_w01=	8155	if ciuo88_m03_w01==	8265
replace ciuo88_m03_w01=	8156	if ciuo88_m03_w01==	8266
replace ciuo88_m03_w01=	8157	if ciuo88_m03_w01==	8264
replace ciuo88_m03_w01=	8159	if ciuo88_m03_w01==	8269
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8271
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8272
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8273
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8274
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8275
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8276
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8277
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8278
replace ciuo88_m03_w01=	8160	if ciuo88_m03_w01==	8279
replace ciuo88_m03_w01=	8171	if ciuo88_m03_w01==	8142
replace ciuo88_m03_w01=	8171	if ciuo88_m03_w01==	8143
replace ciuo88_m03_w01=	8172	if ciuo88_m03_w01==	8141
replace ciuo88_m03_w01=	8181	if ciuo88_m03_w01==	8131
replace ciuo88_m03_w01=	8181	if ciuo88_m03_w01==	8139
replace ciuo88_m03_w01=	8182	if ciuo88_m03_w01==	8162
replace ciuo88_m03_w01=	8183	if ciuo88_m03_w01==	8290
replace ciuo88_m03_w01=	8189	if ciuo88_m03_w01==	8290
replace ciuo88_m03_w01=	8211	if ciuo88_m03_w01==	8281
replace ciuo88_m03_w01=	8212	if ciuo88_m03_w01==	8282
replace ciuo88_m03_w01=	8212	if ciuo88_m03_w01==	8283
replace ciuo88_m03_w01=	8219	if ciuo88_m03_w01==	8284
replace ciuo88_m03_w01=	8219	if ciuo88_m03_w01==	8285
replace ciuo88_m03_w01=	8219	if ciuo88_m03_w01==	8286
replace ciuo88_m03_w01=	8219	if ciuo88_m03_w01==	8290
replace ciuo88_m03_w01=	8311	if ciuo88_m03_w01==	8311
replace ciuo88_m03_w01=	8312	if ciuo88_m03_w01==	8312
replace ciuo88_m03_w01=	8321	if ciuo88_m03_w01==	8321
replace ciuo88_m03_w01=	8322	if ciuo88_m03_w01==	8322
replace ciuo88_m03_w01=	8331	if ciuo88_m03_w01==	8323
replace ciuo88_m03_w01=	8332	if ciuo88_m03_w01==	8324
replace ciuo88_m03_w01=	8341	if ciuo88_m03_w01==	8331
replace ciuo88_m03_w01=	8342	if ciuo88_m03_w01==	8332
replace ciuo88_m03_w01=	8343	if ciuo88_m03_w01==	8333
replace ciuo88_m03_w01=	8344	if ciuo88_m03_w01==	8334
replace ciuo88_m03_w01=	8350	if ciuo88_m03_w01==	8340
replace ciuo88_m03_w01=	9111	if ciuo88_m03_w01==	9131
replace ciuo88_m03_w01=	9112	if ciuo88_m03_w01==	9132
replace ciuo88_m03_w01=	9121	if ciuo88_m03_w01==	9133
replace ciuo88_m03_w01=	9122	if ciuo88_m03_w01==	9142
replace ciuo88_m03_w01=	9123	if ciuo88_m03_w01==	9142
replace ciuo88_m03_w01=	9129	if ciuo88_m03_w01==	9142
replace ciuo88_m03_w01=	9211	if ciuo88_m03_w01==	9211
replace ciuo88_m03_w01=	9212	if ciuo88_m03_w01==	9211
replace ciuo88_m03_w01=	9213	if ciuo88_m03_w01==	9211
replace ciuo88_m03_w01=	9214	if ciuo88_m03_w01==	6113
replace ciuo88_m03_w01=	9214	if ciuo88_m03_w01==	9211
replace ciuo88_m03_w01=	9215	if ciuo88_m03_w01==	9212
replace ciuo88_m03_w01=	9216	if ciuo88_m03_w01==	9213
replace ciuo88_m03_w01=	9311	if ciuo88_m03_w01==	9311
replace ciuo88_m03_w01=	9312	if ciuo88_m03_w01==	9312
replace ciuo88_m03_w01=	9313	if ciuo88_m03_w01==	9313
replace ciuo88_m03_w01=	9321	if ciuo88_m03_w01==	9322
replace ciuo88_m03_w01=	9329	if ciuo88_m03_w01==	9321
replace ciuo88_m03_w01=	9329	if ciuo88_m03_w01==	9322
replace ciuo88_m03_w01=	9331	if ciuo88_m03_w01==	9331
replace ciuo88_m03_w01=	9332	if ciuo88_m03_w01==	9332
replace ciuo88_m03_w01=	9333	if ciuo88_m03_w01==	9333
replace ciuo88_m03_w01=	9334	if ciuo88_m03_w01==	9333
replace ciuo88_m03_w01=	9411	if ciuo88_m03_w01==	5122
replace ciuo88_m03_w01=	9412	if ciuo88_m03_w01==	9132
replace ciuo88_m03_w01=	9510	if ciuo88_m03_w01==	9120
replace ciuo88_m03_w01=	9520	if ciuo88_m03_w01==	9112
replace ciuo88_m03_w01=	9611	if ciuo88_m03_w01==	9161
replace ciuo88_m03_w01=	9612	if ciuo88_m03_w01==	9161
replace ciuo88_m03_w01=	9612	if ciuo88_m03_w01==	9321
replace ciuo88_m03_w01=	9613	if ciuo88_m03_w01==	9162
replace ciuo88_m03_w01=	9621	if ciuo88_m03_w01==	9151
replace ciuo88_m03_w01=	9621	if ciuo88_m03_w01==	9152
replace ciuo88_m03_w01=	9622	if ciuo88_m03_w01==	9162
replace ciuo88_m03_w01=	9623	if ciuo88_m03_w01==	9153
replace ciuo88_m03_w01=	9624	if ciuo88_m03_w01==	9162
replace ciuo88_m03_w01=	9629	if ciuo88_m03_w01==	9152
replace ciuo88_m03_w01=	0110	if ciuo88_m03_w01==	0110
replace ciuo88_m03_w01=	0210	if ciuo88_m03_w01==	0110
replace ciuo88_m03_w01=	0310	if ciuo88_m03_w01==	0110

replace ciuo88_m22_w01 =	1111	if ciuo88_m22_w01==	1110
replace ciuo88_m22_w01 =	1112	if ciuo88_m22_w01==	1120
replace ciuo88_m22_w01 =	1113	if ciuo88_m22_w01==	1130
replace ciuo88_m22_w01 =	1114	if ciuo88_m22_w01==	1141
replace ciuo88_m22_w01 =	1114	if ciuo88_m22_w01==	1142
replace ciuo88_m22_w01 =	1114	if ciuo88_m22_w01==	1143
replace ciuo88_m22_w01 =	1120	if ciuo88_m22_w01==	1210
replace ciuo88_m22_w01 =	1211	if ciuo88_m22_w01==	1231
replace ciuo88_m22_w01 =	1211	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1212	if ciuo88_m22_w01==	1232
replace ciuo88_m22_w01 =	1212	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1213	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1213	if ciuo88_m22_w01==	1239
replace ciuo88_m22_w01 =	1219	if ciuo88_m22_w01==	1227
replace ciuo88_m22_w01 =	1219	if ciuo88_m22_w01==	1228
replace ciuo88_m22_w01 =	1219	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1219	if ciuo88_m22_w01==	1231
replace ciuo88_m22_w01 =	1219	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1219	if ciuo88_m22_w01==	1318
replace ciuo88_m22_w01 =	1221	if ciuo88_m22_w01==	1233
replace ciuo88_m22_w01 =	1221	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1222	if ciuo88_m22_w01==	1234
replace ciuo88_m22_w01 =	1222	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1223	if ciuo88_m22_w01==	1237
replace ciuo88_m22_w01 =	1223	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1311	if ciuo88_m22_w01==	1221
replace ciuo88_m22_w01 =	1312	if ciuo88_m22_w01==	1221
replace ciuo88_m22_w01 =	1321	if ciuo88_m22_w01==	1222
replace ciuo88_m22_w01 =	1321	if ciuo88_m22_w01==	1312
replace ciuo88_m22_w01 =	1322	if ciuo88_m22_w01==	1222
replace ciuo88_m22_w01 =	1322	if ciuo88_m22_w01==	1312
replace ciuo88_m22_w01 =	1323	if ciuo88_m22_w01==	1223
replace ciuo88_m22_w01 =	1323	if ciuo88_m22_w01==	1313
replace ciuo88_m22_w01 =	1324	if ciuo88_m22_w01==	1226
replace ciuo88_m22_w01 =	1324	if ciuo88_m22_w01==	1235
replace ciuo88_m22_w01 =	1324	if ciuo88_m22_w01==	1316
replace ciuo88_m22_w01 =	1330	if ciuo88_m22_w01==	1226
replace ciuo88_m22_w01 =	1330	if ciuo88_m22_w01==	1236
replace ciuo88_m22_w01 =	1330	if ciuo88_m22_w01==	1316
replace ciuo88_m22_w01 =	1330	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1341	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1341	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1342	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1342	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1342	if ciuo88_m22_w01==	2230
replace ciuo88_m22_w01 =	1343	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1343	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1343	if ciuo88_m22_w01==	2230
replace ciuo88_m22_w01 =	1344	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1344	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1345	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1345	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1346	if ciuo88_m22_w01==	1227
replace ciuo88_m22_w01 =	1346	if ciuo88_m22_w01==	1317
replace ciuo88_m22_w01 =	1349	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1349	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1411	if ciuo88_m22_w01==	1225
replace ciuo88_m22_w01 =	1411	if ciuo88_m22_w01==	1315
replace ciuo88_m22_w01 =	1412	if ciuo88_m22_w01==	1225
replace ciuo88_m22_w01 =	1412	if ciuo88_m22_w01==	1315
replace ciuo88_m22_w01 =	1420	if ciuo88_m22_w01==	1224
replace ciuo88_m22_w01 =	1420	if ciuo88_m22_w01==	1314
replace ciuo88_m22_w01 =	1431	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	1439	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	1439	if ciuo88_m22_w01==	1319
replace ciuo88_m22_w01 =	2111	if ciuo88_m22_w01==	2111
replace ciuo88_m22_w01 =	2112	if ciuo88_m22_w01==	2112
replace ciuo88_m22_w01 =	2113	if ciuo88_m22_w01==	2113
replace ciuo88_m22_w01 =	2114	if ciuo88_m22_w01==	2114
replace ciuo88_m22_w01 =	2120	if ciuo88_m22_w01==	2121
replace ciuo88_m22_w01 =	2120	if ciuo88_m22_w01==	2122
replace ciuo88_m22_w01 =	2131	if ciuo88_m22_w01==	2211
replace ciuo88_m22_w01 =	2131	if ciuo88_m22_w01==	2212
replace ciuo88_m22_w01 =	2132	if ciuo88_m22_w01==	2213
replace ciuo88_m22_w01 =	2132	if ciuo88_m22_w01==	3213
replace ciuo88_m22_w01 =	2133	if ciuo88_m22_w01==	2211
replace ciuo88_m22_w01 =	2141	if ciuo88_m22_w01==	2149
replace ciuo88_m22_w01 =	2142	if ciuo88_m22_w01==	2142
replace ciuo88_m22_w01 =	2143	if ciuo88_m22_w01==	2149
replace ciuo88_m22_w01 =	2144	if ciuo88_m22_w01==	2145
replace ciuo88_m22_w01 =	2145	if ciuo88_m22_w01==	2146
replace ciuo88_m22_w01 =	2146	if ciuo88_m22_w01==	2147
replace ciuo88_m22_w01 =	2149	if ciuo88_m22_w01==	2149
replace ciuo88_m22_w01 =	2151	if ciuo88_m22_w01==	2143
replace ciuo88_m22_w01 =	2152	if ciuo88_m22_w01==	2144
replace ciuo88_m22_w01 =	2153	if ciuo88_m22_w01==	2144
replace ciuo88_m22_w01 =	2161	if ciuo88_m22_w01==	2141
replace ciuo88_m22_w01 =	2162	if ciuo88_m22_w01==	2141
replace ciuo88_m22_w01 =	2163	if ciuo88_m22_w01==	3471
replace ciuo88_m22_w01 =	2164	if ciuo88_m22_w01==	2141
replace ciuo88_m22_w01 =	2165	if ciuo88_m22_w01==	2148
replace ciuo88_m22_w01 =	2166	if ciuo88_m22_w01==	3471
replace ciuo88_m22_w01 =	2211	if ciuo88_m22_w01==	2221
replace ciuo88_m22_w01 =	2212	if ciuo88_m22_w01==	2212
replace ciuo88_m22_w01 =	2212	if ciuo88_m22_w01==	2221
replace ciuo88_m22_w01 =	2221	if ciuo88_m22_w01==	2230
replace ciuo88_m22_w01 =	2222	if ciuo88_m22_w01==	2230
replace ciuo88_m22_w01 =	2230	if ciuo88_m22_w01==	3229
replace ciuo88_m22_w01 =	2230	if ciuo88_m22_w01==	3241
replace ciuo88_m22_w01 =	2240	if ciuo88_m22_w01==	3221
replace ciuo88_m22_w01 =	2250	if ciuo88_m22_w01==	2212
replace ciuo88_m22_w01 =	2250	if ciuo88_m22_w01==	2223
replace ciuo88_m22_w01 =	2261	if ciuo88_m22_w01==	2222
replace ciuo88_m22_w01 =	2262	if ciuo88_m22_w01==	2113
replace ciuo88_m22_w01 =	2262	if ciuo88_m22_w01==	2224
replace ciuo88_m22_w01 =	2263	if ciuo88_m22_w01==	2229
replace ciuo88_m22_w01 =	2263	if ciuo88_m22_w01==	2412
replace ciuo88_m22_w01 =	2263	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	2263	if ciuo88_m22_w01==	3222
replace ciuo88_m22_w01 =	2264	if ciuo88_m22_w01==	3226
replace ciuo88_m22_w01 =	2265	if ciuo88_m22_w01==	3223
replace ciuo88_m22_w01 =	2266	if ciuo88_m22_w01==	3229
replace ciuo88_m22_w01 =	2267	if ciuo88_m22_w01==	3224
replace ciuo88_m22_w01 =	2267	if ciuo88_m22_w01==	3229
replace ciuo88_m22_w01 =	2269	if ciuo88_m22_w01==	2229
replace ciuo88_m22_w01 =	2269	if ciuo88_m22_w01==	3226
replace ciuo88_m22_w01 =	2269	if ciuo88_m22_w01==	3229
replace ciuo88_m22_w01 =	2310	if ciuo88_m22_w01==	2310
replace ciuo88_m22_w01 =	2320	if ciuo88_m22_w01==	2310
replace ciuo88_m22_w01 =	2320	if ciuo88_m22_w01==	2320
replace ciuo88_m22_w01 =	2330	if ciuo88_m22_w01==	2320
replace ciuo88_m22_w01 =	2341	if ciuo88_m22_w01==	2331
replace ciuo88_m22_w01 =	2341	if ciuo88_m22_w01==	3310
replace ciuo88_m22_w01 =	2342	if ciuo88_m22_w01==	2332
replace ciuo88_m22_w01 =	2342	if ciuo88_m22_w01==	3320
replace ciuo88_m22_w01 =	2351	if ciuo88_m22_w01==	2351
replace ciuo88_m22_w01 =	2351	if ciuo88_m22_w01==	2352
replace ciuo88_m22_w01 =	2352	if ciuo88_m22_w01==	2340
replace ciuo88_m22_w01 =	2352	if ciuo88_m22_w01==	3330
replace ciuo88_m22_w01 =	2353	if ciuo88_m22_w01==	2359
replace ciuo88_m22_w01 =	2353	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	2354	if ciuo88_m22_w01==	2359
replace ciuo88_m22_w01 =	2355	if ciuo88_m22_w01==	2359
replace ciuo88_m22_w01 =	2355	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	2356	if ciuo88_m22_w01==	2359
replace ciuo88_m22_w01 =	2356	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	2359	if ciuo88_m22_w01==	2359
replace ciuo88_m22_w01 =	2359	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	2411	if ciuo88_m22_w01==	2411
replace ciuo88_m22_w01 =	2412	if ciuo88_m22_w01==	2411
replace ciuo88_m22_w01 =	2412	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	2412	if ciuo88_m22_w01==	3411
replace ciuo88_m22_w01 =	2413	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	2421	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	2422	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	2423	if ciuo88_m22_w01==	2412
replace ciuo88_m22_w01 =	2424	if ciuo88_m22_w01==	2412
replace ciuo88_m22_w01 =	2431	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	2432	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	2433	if ciuo88_m22_w01==	3415
replace ciuo88_m22_w01 =	2434	if ciuo88_m22_w01==	3415
replace ciuo88_m22_w01 =	2511	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2512	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2513	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2513	if ciuo88_m22_w01==	2132
replace ciuo88_m22_w01 =	2513	if ciuo88_m22_w01==	2139
replace ciuo88_m22_w01 =	2514	if ciuo88_m22_w01==	2132
replace ciuo88_m22_w01 =	2519	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2519	if ciuo88_m22_w01==	2132
replace ciuo88_m22_w01 =	2519	if ciuo88_m22_w01==	2139
replace ciuo88_m22_w01 =	2521	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2522	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2523	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2529	if ciuo88_m22_w01==	2131
replace ciuo88_m22_w01 =	2529	if ciuo88_m22_w01==	2132
replace ciuo88_m22_w01 =	2529	if ciuo88_m22_w01==	2139
replace ciuo88_m22_w01 =	2611	if ciuo88_m22_w01==	2421
replace ciuo88_m22_w01 =	2612	if ciuo88_m22_w01==	2422
replace ciuo88_m22_w01 =	2619	if ciuo88_m22_w01==	2429
replace ciuo88_m22_w01 =	2621	if ciuo88_m22_w01==	2431
replace ciuo88_m22_w01 =	2622	if ciuo88_m22_w01==	2432
replace ciuo88_m22_w01 =	2631	if ciuo88_m22_w01==	2441
replace ciuo88_m22_w01 =	2632	if ciuo88_m22_w01==	2442
replace ciuo88_m22_w01 =	2633	if ciuo88_m22_w01==	2443
replace ciuo88_m22_w01 =	2634	if ciuo88_m22_w01==	2445
replace ciuo88_m22_w01 =	2635	if ciuo88_m22_w01==	2446
replace ciuo88_m22_w01 =	2636	if ciuo88_m22_w01==	2460
replace ciuo88_m22_w01 =	2641	if ciuo88_m22_w01==	2451
replace ciuo88_m22_w01 =	2642	if ciuo88_m22_w01==	2451
replace ciuo88_m22_w01 =	2642	if ciuo88_m22_w01==	3472
replace ciuo88_m22_w01 =	2643	if ciuo88_m22_w01==	2444
replace ciuo88_m22_w01 =	2651	if ciuo88_m22_w01==	2452
replace ciuo88_m22_w01 =	2652	if ciuo88_m22_w01==	2453
replace ciuo88_m22_w01 =	2652	if ciuo88_m22_w01==	3473
replace ciuo88_m22_w01 =	2653	if ciuo88_m22_w01==	2454
replace ciuo88_m22_w01 =	2653	if ciuo88_m22_w01==	3473
replace ciuo88_m22_w01 =	2654	if ciuo88_m22_w01==	1229
replace ciuo88_m22_w01 =	2654	if ciuo88_m22_w01==	2455
replace ciuo88_m22_w01 =	2655	if ciuo88_m22_w01==	2455
replace ciuo88_m22_w01 =	2656	if ciuo88_m22_w01==	3472
replace ciuo88_m22_w01 =	2659	if ciuo88_m22_w01==	3474
replace ciuo88_m22_w01 =	3111	if ciuo88_m22_w01==	3111
replace ciuo88_m22_w01 =	3112	if ciuo88_m22_w01==	3112
replace ciuo88_m22_w01 =	3112	if ciuo88_m22_w01==	3151
replace ciuo88_m22_w01 =	3113	if ciuo88_m22_w01==	3113
replace ciuo88_m22_w01 =	3113	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	3114	if ciuo88_m22_w01==	3114
replace ciuo88_m22_w01 =	3114	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	3115	if ciuo88_m22_w01==	3115
replace ciuo88_m22_w01 =	3115	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	3116	if ciuo88_m22_w01==	3116
replace ciuo88_m22_w01 =	3117	if ciuo88_m22_w01==	3117
replace ciuo88_m22_w01 =	3117	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	3118	if ciuo88_m22_w01==	3118
replace ciuo88_m22_w01 =	3119	if ciuo88_m22_w01==	3119
replace ciuo88_m22_w01 =	3121	if ciuo88_m22_w01==	7111
replace ciuo88_m22_w01 =	3121	if ciuo88_m22_w01==	8111
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8171
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8172
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8211
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8221
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8222
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8223
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8224
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8229
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8231
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8232
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8232
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8232
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8240
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8251
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8252
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8253
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8261
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8262
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8263
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8264
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8265
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8266
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8269
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8271
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8272
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8273
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8274
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8275
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8276
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8277
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8278
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8279
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8281
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8282
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8283
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8284
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8285
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8286
replace ciuo88_m22_w01 =	3122	if ciuo88_m22_w01==	8290
replace ciuo88_m22_w01 =	3123	if ciuo88_m22_w01==	1223
replace ciuo88_m22_w01 =	3123	if ciuo88_m22_w01==	7129
replace ciuo88_m22_w01 =	3131	if ciuo88_m22_w01==	8161
replace ciuo88_m22_w01 =	3132	if ciuo88_m22_w01==	8163
replace ciuo88_m22_w01 =	3133	if ciuo88_m22_w01==	8152
replace ciuo88_m22_w01 =	3133	if ciuo88_m22_w01==	8153
replace ciuo88_m22_w01 =	3133	if ciuo88_m22_w01==	8154
replace ciuo88_m22_w01 =	3133	if ciuo88_m22_w01==	8159
replace ciuo88_m22_w01 =	3134	if ciuo88_m22_w01==	8155
replace ciuo88_m22_w01 =	3135	if ciuo88_m22_w01==	8121
replace ciuo88_m22_w01 =	3135	if ciuo88_m22_w01==	8122
replace ciuo88_m22_w01 =	3135	if ciuo88_m22_w01==	8123
replace ciuo88_m22_w01 =	3135	if ciuo88_m22_w01==	8124
replace ciuo88_m22_w01 =	3139	if ciuo88_m22_w01==	3123
replace ciuo88_m22_w01 =	3139	if ciuo88_m22_w01==	8142
replace ciuo88_m22_w01 =	3139	if ciuo88_m22_w01==	8143
replace ciuo88_m22_w01 =	3139	if ciuo88_m22_w01==	8171
replace ciuo88_m22_w01 =	3139	if ciuo88_m22_w01==	8172
replace ciuo88_m22_w01 =	3141	if ciuo88_m22_w01==	3211
replace ciuo88_m22_w01 =	3142	if ciuo88_m22_w01==	3212
replace ciuo88_m22_w01 =	3143	if ciuo88_m22_w01==	3212
replace ciuo88_m22_w01 =	3151	if ciuo88_m22_w01==	3141
replace ciuo88_m22_w01 =	3152	if ciuo88_m22_w01==	3142
replace ciuo88_m22_w01 =	3153	if ciuo88_m22_w01==	3143
replace ciuo88_m22_w01 =	3153	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	3154	if ciuo88_m22_w01==	3144
replace ciuo88_m22_w01 =	3155	if ciuo88_m22_w01==	3145
replace ciuo88_m22_w01 =	3211	if ciuo88_m22_w01==	3133
replace ciuo88_m22_w01 =	3212	if ciuo88_m22_w01==	3211
replace ciuo88_m22_w01 =	3213	if ciuo88_m22_w01==	3228
replace ciuo88_m22_w01 =	3214	if ciuo88_m22_w01==	7311
replace ciuo88_m22_w01 =	3221	if ciuo88_m22_w01==	2230
replace ciuo88_m22_w01 =	3221	if ciuo88_m22_w01==	3231
replace ciuo88_m22_w01 =	3222	if ciuo88_m22_w01==	2230
replace ciuo88_m22_w01 =	3222	if ciuo88_m22_w01==	3232
replace ciuo88_m22_w01 =	3230	if ciuo88_m22_w01==	3241
replace ciuo88_m22_w01 =	3240	if ciuo88_m22_w01==	3227
replace ciuo88_m22_w01 =	3251	if ciuo88_m22_w01==	3225
replace ciuo88_m22_w01 =	3252	if ciuo88_m22_w01==	4143
replace ciuo88_m22_w01 =	3253	if ciuo88_m22_w01==	3221
replace ciuo88_m22_w01 =	3254	if ciuo88_m22_w01==	3224
replace ciuo88_m22_w01 =	3255	if ciuo88_m22_w01==	3226
replace ciuo88_m22_w01 =	3256	if ciuo88_m22_w01==	3221
replace ciuo88_m22_w01 =	3257	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	3257	if ciuo88_m22_w01==	3222
replace ciuo88_m22_w01 =	3258	if ciuo88_m22_w01==	5132
replace ciuo88_m22_w01 =	3259	if ciuo88_m22_w01==	3226
replace ciuo88_m22_w01 =	3259	if ciuo88_m22_w01==	3229
replace ciuo88_m22_w01 =	3311	if ciuo88_m22_w01==	3411
replace ciuo88_m22_w01 =	3312	if ciuo88_m22_w01==	3419
replace ciuo88_m22_w01 =	3313	if ciuo88_m22_w01==	3433
replace ciuo88_m22_w01 =	3313	if ciuo88_m22_w01==	3434
replace ciuo88_m22_w01 =	3314	if ciuo88_m22_w01==	3434
replace ciuo88_m22_w01 =	3315	if ciuo88_m22_w01==	3417
replace ciuo88_m22_w01 =	3321	if ciuo88_m22_w01==	3412
replace ciuo88_m22_w01 =	3322	if ciuo88_m22_w01==	3415
replace ciuo88_m22_w01 =	3323	if ciuo88_m22_w01==	3416
replace ciuo88_m22_w01 =	3324	if ciuo88_m22_w01==	3421
replace ciuo88_m22_w01 =	3331	if ciuo88_m22_w01==	3422
replace ciuo88_m22_w01 =	3332	if ciuo88_m22_w01==	3414
replace ciuo88_m22_w01 =	3332	if ciuo88_m22_w01==	3439
replace ciuo88_m22_w01 =	3333	if ciuo88_m22_w01==	3423
replace ciuo88_m22_w01 =	3334	if ciuo88_m22_w01==	3413
replace ciuo88_m22_w01 =	3339	if ciuo88_m22_w01==	2419
replace ciuo88_m22_w01 =	3339	if ciuo88_m22_w01==	3414
replace ciuo88_m22_w01 =	3339	if ciuo88_m22_w01==	3417
replace ciuo88_m22_w01 =	3339	if ciuo88_m22_w01==	3429
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	3431
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4111
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4112
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4114
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4115
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4115
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4121
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4122
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4131
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4132
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4133
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4141
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4142
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4143
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4144
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4190
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4222
replace ciuo88_m22_w01 =	3341	if ciuo88_m22_w01==	4223
replace ciuo88_m22_w01 =	3342	if ciuo88_m22_w01==	3431
replace ciuo88_m22_w01 =	3342	if ciuo88_m22_w01==	4115
replace ciuo88_m22_w01 =	3343	if ciuo88_m22_w01==	3431
replace ciuo88_m22_w01 =	3343	if ciuo88_m22_w01==	3439
replace ciuo88_m22_w01 =	3344	if ciuo88_m22_w01==	3431
replace ciuo88_m22_w01 =	3344	if ciuo88_m22_w01==	4115
replace ciuo88_m22_w01 =	3351	if ciuo88_m22_w01==	3441
replace ciuo88_m22_w01 =	3352	if ciuo88_m22_w01==	3442
replace ciuo88_m22_w01 =	3353	if ciuo88_m22_w01==	3443
replace ciuo88_m22_w01 =	3354	if ciuo88_m22_w01==	3444
replace ciuo88_m22_w01 =	3355	if ciuo88_m22_w01==	3450
replace ciuo88_m22_w01 =	3359	if ciuo88_m22_w01==	3151
replace ciuo88_m22_w01 =	3359	if ciuo88_m22_w01==	3439
replace ciuo88_m22_w01 =	3359	if ciuo88_m22_w01==	3449
replace ciuo88_m22_w01 =	3411	if ciuo88_m22_w01==	3432
replace ciuo88_m22_w01 =	3411	if ciuo88_m22_w01==	3450
replace ciuo88_m22_w01 =	3412	if ciuo88_m22_w01==	3460
replace ciuo88_m22_w01 =	3413	if ciuo88_m22_w01==	3480
replace ciuo88_m22_w01 =	3421	if ciuo88_m22_w01==	3475
replace ciuo88_m22_w01 =	3422	if ciuo88_m22_w01==	3475
replace ciuo88_m22_w01 =	3423	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	3423	if ciuo88_m22_w01==	3475
replace ciuo88_m22_w01 =	3431	if ciuo88_m22_w01==	3131
replace ciuo88_m22_w01 =	3432	if ciuo88_m22_w01==	3471
replace ciuo88_m22_w01 =	3433	if ciuo88_m22_w01==	3439
replace ciuo88_m22_w01 =	3433	if ciuo88_m22_w01==	3471
replace ciuo88_m22_w01 =	3434	if ciuo88_m22_w01==	5122
replace ciuo88_m22_w01 =	3435	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	3511	if ciuo88_m22_w01==	3122
replace ciuo88_m22_w01 =	3512	if ciuo88_m22_w01==	3121
replace ciuo88_m22_w01 =	3513	if ciuo88_m22_w01==	2139
replace ciuo88_m22_w01 =	3513	if ciuo88_m22_w01==	3121
replace ciuo88_m22_w01 =	3514	if ciuo88_m22_w01==	3121
replace ciuo88_m22_w01 =	3514	if ciuo88_m22_w01==	3122
replace ciuo88_m22_w01 =	3521	if ciuo88_m22_w01==	3131
replace ciuo88_m22_w01 =	3521	if ciuo88_m22_w01==	3132
replace ciuo88_m22_w01 =	3522	if ciuo88_m22_w01==	3114
replace ciuo88_m22_w01 =	3522	if ciuo88_m22_w01==	3132
replace ciuo88_m22_w01 =	4110	if ciuo88_m22_w01==	4190
replace ciuo88_m22_w01 =	4120	if ciuo88_m22_w01==	4115
replace ciuo88_m22_w01 =	4131	if ciuo88_m22_w01==	4111
replace ciuo88_m22_w01 =	4131	if ciuo88_m22_w01==	4112
replace ciuo88_m22_w01 =	4132	if ciuo88_m22_w01==	4113
replace ciuo88_m22_w01 =	4132	if ciuo88_m22_w01==	4114
replace ciuo88_m22_w01 =	4211	if ciuo88_m22_w01==	4211
replace ciuo88_m22_w01 =	4211	if ciuo88_m22_w01==	4212
replace ciuo88_m22_w01 =	4212	if ciuo88_m22_w01==	4211
replace ciuo88_m22_w01 =	4212	if ciuo88_m22_w01==	4213
replace ciuo88_m22_w01 =	4213	if ciuo88_m22_w01==	4214
replace ciuo88_m22_w01 =	4214	if ciuo88_m22_w01==	4215
replace ciuo88_m22_w01 =	4221	if ciuo88_m22_w01==	3414
replace ciuo88_m22_w01 =	4221	if ciuo88_m22_w01==	4221
replace ciuo88_m22_w01 =	4222	if ciuo88_m22_w01==	4222
replace ciuo88_m22_w01 =	4223	if ciuo88_m22_w01==	4223
replace ciuo88_m22_w01 =	4224	if ciuo88_m22_w01==	4222
replace ciuo88_m22_w01 =	4225	if ciuo88_m22_w01==	4222
replace ciuo88_m22_w01 =	4226	if ciuo88_m22_w01==	4222
replace ciuo88_m22_w01 =	4227	if ciuo88_m22_w01==	4190
replace ciuo88_m22_w01 =	4229	if ciuo88_m22_w01==	4222
replace ciuo88_m22_w01 =	4311	if ciuo88_m22_w01==	4121
replace ciuo88_m22_w01 =	4312	if ciuo88_m22_w01==	4122
replace ciuo88_m22_w01 =	4313	if ciuo88_m22_w01==	4121
replace ciuo88_m22_w01 =	4321	if ciuo88_m22_w01==	4131
replace ciuo88_m22_w01 =	4322	if ciuo88_m22_w01==	4132
replace ciuo88_m22_w01 =	4323	if ciuo88_m22_w01==	4133
replace ciuo88_m22_w01 =	4411	if ciuo88_m22_w01==	4141
replace ciuo88_m22_w01 =	4412	if ciuo88_m22_w01==	4142
replace ciuo88_m22_w01 =	4413	if ciuo88_m22_w01==	4143
replace ciuo88_m22_w01 =	4414	if ciuo88_m22_w01==	4144
replace ciuo88_m22_w01 =	4415	if ciuo88_m22_w01==	4141
replace ciuo88_m22_w01 =	4416	if ciuo88_m22_w01==	4190
replace ciuo88_m22_w01 =	4419	if ciuo88_m22_w01==	4190
replace ciuo88_m22_w01 =	5111	if ciuo88_m22_w01==	5111
replace ciuo88_m22_w01 =	5112	if ciuo88_m22_w01==	5112
replace ciuo88_m22_w01 =	5113	if ciuo88_m22_w01==	5113
replace ciuo88_m22_w01 =	5120	if ciuo88_m22_w01==	5122
replace ciuo88_m22_w01 =	5131	if ciuo88_m22_w01==	5123
replace ciuo88_m22_w01 =	5132	if ciuo88_m22_w01==	5123
replace ciuo88_m22_w01 =	5141	if ciuo88_m22_w01==	5141
replace ciuo88_m22_w01 =	5142	if ciuo88_m22_w01==	5141
replace ciuo88_m22_w01 =	5151	if ciuo88_m22_w01==	5121
replace ciuo88_m22_w01 =	5152	if ciuo88_m22_w01==	5121
replace ciuo88_m22_w01 =	5153	if ciuo88_m22_w01==	9141
replace ciuo88_m22_w01 =	5161	if ciuo88_m22_w01==	5151
replace ciuo88_m22_w01 =	5161	if ciuo88_m22_w01==	5152
replace ciuo88_m22_w01 =	5162	if ciuo88_m22_w01==	5142
replace ciuo88_m22_w01 =	5163	if ciuo88_m22_w01==	5143
replace ciuo88_m22_w01 =	5164	if ciuo88_m22_w01==	5139
replace ciuo88_m22_w01 =	5164	if ciuo88_m22_w01==	6129
replace ciuo88_m22_w01 =	5165	if ciuo88_m22_w01==	3340
replace ciuo88_m22_w01 =	5169	if ciuo88_m22_w01==	5149
replace ciuo88_m22_w01 =	5169	if ciuo88_m22_w01==	3242
replace ciuo88_m22_w01 =	5211	if ciuo88_m22_w01==	5230
replace ciuo88_m22_w01 =	5212	if ciuo88_m22_w01==	9111
replace ciuo88_m22_w01 =	5221	if ciuo88_m22_w01==	1314
replace ciuo88_m22_w01 =	5222	if ciuo88_m22_w01==	5220
replace ciuo88_m22_w01 =	5223	if ciuo88_m22_w01==	5220
replace ciuo88_m22_w01 =	5230	if ciuo88_m22_w01==	4211
replace ciuo88_m22_w01 =	5241	if ciuo88_m22_w01==	5210
replace ciuo88_m22_w01 =	5242	if ciuo88_m22_w01==	5220
replace ciuo88_m22_w01 =	5243	if ciuo88_m22_w01==	9113
replace ciuo88_m22_w01 =	5244	if ciuo88_m22_w01==	9113
replace ciuo88_m22_w01 =	5245	if ciuo88_m22_w01==	5220
replace ciuo88_m22_w01 =	5246	if ciuo88_m22_w01==	5220
replace ciuo88_m22_w01 =	5246	if ciuo88_m22_w01==	5230
replace ciuo88_m22_w01 =	5249	if ciuo88_m22_w01==	5220
replace ciuo88_m22_w01 =	5311	if ciuo88_m22_w01==	5131
replace ciuo88_m22_w01 =	5312	if ciuo88_m22_w01==	5131
replace ciuo88_m22_w01 =	5321	if ciuo88_m22_w01==	5132
replace ciuo88_m22_w01 =	5322	if ciuo88_m22_w01==	5133
replace ciuo88_m22_w01 =	5329	if ciuo88_m22_w01==	5132
replace ciuo88_m22_w01 =	5329	if ciuo88_m22_w01==	5139
replace ciuo88_m22_w01 =	5411	if ciuo88_m22_w01==	5161
replace ciuo88_m22_w01 =	5412	if ciuo88_m22_w01==	5162
replace ciuo88_m22_w01 =	5413	if ciuo88_m22_w01==	5163
replace ciuo88_m22_w01 =	5414	if ciuo88_m22_w01==	5169
replace ciuo88_m22_w01 =	5414	if ciuo88_m22_w01==	9152
replace ciuo88_m22_w01 =	5419	if ciuo88_m22_w01==	5169
replace ciuo88_m22_w01 =	6111	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6111	if ciuo88_m22_w01==	6111
replace ciuo88_m22_w01 =	6112	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6112	if ciuo88_m22_w01==	6112
replace ciuo88_m22_w01 =	6113	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6113	if ciuo88_m22_w01==	6113
replace ciuo88_m22_w01 =	6114	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6114	if ciuo88_m22_w01==	6114
replace ciuo88_m22_w01 =	6121	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6121	if ciuo88_m22_w01==	6121
replace ciuo88_m22_w01 =	6121	if ciuo88_m22_w01==	6124
replace ciuo88_m22_w01 =	6122	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6122	if ciuo88_m22_w01==	6122
replace ciuo88_m22_w01 =	6122	if ciuo88_m22_w01==	6124
replace ciuo88_m22_w01 =	6123	if ciuo88_m22_w01==	6123
replace ciuo88_m22_w01 =	6123	if ciuo88_m22_w01==	6124
replace ciuo88_m22_w01 =	6129	if ciuo88_m22_w01==	6129
replace ciuo88_m22_w01 =	6130	if ciuo88_m22_w01==	1311
replace ciuo88_m22_w01 =	6130	if ciuo88_m22_w01==	6130
replace ciuo88_m22_w01 =	6210	if ciuo88_m22_w01==	6141
replace ciuo88_m22_w01 =	6210	if ciuo88_m22_w01==	6142
replace ciuo88_m22_w01 =	6221	if ciuo88_m22_w01==	6151
replace ciuo88_m22_w01 =	6222	if ciuo88_m22_w01==	6152
replace ciuo88_m22_w01 =	6223	if ciuo88_m22_w01==	6153
replace ciuo88_m22_w01 =	6224	if ciuo88_m22_w01==	6154
replace ciuo88_m22_w01 =	6310	if ciuo88_m22_w01==	6210
replace ciuo88_m22_w01 =	6320	if ciuo88_m22_w01==	6210
replace ciuo88_m22_w01 =	6330	if ciuo88_m22_w01==	6210
replace ciuo88_m22_w01 =	6340	if ciuo88_m22_w01==	6210
replace ciuo88_m22_w01 =	7111	if ciuo88_m22_w01==	7121
replace ciuo88_m22_w01 =	7111	if ciuo88_m22_w01==	7129
replace ciuo88_m22_w01 =	7112	if ciuo88_m22_w01==	7122
replace ciuo88_m22_w01 =	7113	if ciuo88_m22_w01==	7113
replace ciuo88_m22_w01 =	7113	if ciuo88_m22_w01==	7122
replace ciuo88_m22_w01 =	7114	if ciuo88_m22_w01==	7123
replace ciuo88_m22_w01 =	7115	if ciuo88_m22_w01==	7124
replace ciuo88_m22_w01 =	7119	if ciuo88_m22_w01==	7129
replace ciuo88_m22_w01 =	7121	if ciuo88_m22_w01==	7131
replace ciuo88_m22_w01 =	7122	if ciuo88_m22_w01==	7132
replace ciuo88_m22_w01 =	7123	if ciuo88_m22_w01==	7133
replace ciuo88_m22_w01 =	7124	if ciuo88_m22_w01==	7134
replace ciuo88_m22_w01 =	7125	if ciuo88_m22_w01==	7135
replace ciuo88_m22_w01 =	7126	if ciuo88_m22_w01==	7136
replace ciuo88_m22_w01 =	7127	if ciuo88_m22_w01==	7136
replace ciuo88_m22_w01 =	7127	if ciuo88_m22_w01==	7233
replace ciuo88_m22_w01 =	7131	if ciuo88_m22_w01==	7141
replace ciuo88_m22_w01 =	7132	if ciuo88_m22_w01==	7142
replace ciuo88_m22_w01 =	7133	if ciuo88_m22_w01==	7143
replace ciuo88_m22_w01 =	7211	if ciuo88_m22_w01==	7211
replace ciuo88_m22_w01 =	7212	if ciuo88_m22_w01==	7212
replace ciuo88_m22_w01 =	7213	if ciuo88_m22_w01==	7213
replace ciuo88_m22_w01 =	7214	if ciuo88_m22_w01==	7214
replace ciuo88_m22_w01 =	7215	if ciuo88_m22_w01==	7215
replace ciuo88_m22_w01 =	7221	if ciuo88_m22_w01==	7221
replace ciuo88_m22_w01 =	7222	if ciuo88_m22_w01==	7222
replace ciuo88_m22_w01 =	7223	if ciuo88_m22_w01==	7223
replace ciuo88_m22_w01 =	7223	if ciuo88_m22_w01==	8211
replace ciuo88_m22_w01 =	7224	if ciuo88_m22_w01==	7224
replace ciuo88_m22_w01 =	7231	if ciuo88_m22_w01==	7231
replace ciuo88_m22_w01 =	7232	if ciuo88_m22_w01==	7232
replace ciuo88_m22_w01 =	7233	if ciuo88_m22_w01==	7233
replace ciuo88_m22_w01 =	7234	if ciuo88_m22_w01==	7231
replace ciuo88_m22_w01 =	7311	if ciuo88_m22_w01==	7311
replace ciuo88_m22_w01 =	7312	if ciuo88_m22_w01==	7312
replace ciuo88_m22_w01 =	7313	if ciuo88_m22_w01==	7313
replace ciuo88_m22_w01 =	7314	if ciuo88_m22_w01==	7321
replace ciuo88_m22_w01 =	7315	if ciuo88_m22_w01==	7322
replace ciuo88_m22_w01 =	7316	if ciuo88_m22_w01==	3471
replace ciuo88_m22_w01 =	7316	if ciuo88_m22_w01==	7323
replace ciuo88_m22_w01 =	7316	if ciuo88_m22_w01==	7324
replace ciuo88_m22_w01 =	7317	if ciuo88_m22_w01==	7331
replace ciuo88_m22_w01 =	7317	if ciuo88_m22_w01==	7424
replace ciuo88_m22_w01 =	7318	if ciuo88_m22_w01==	7332
replace ciuo88_m22_w01 =	7318	if ciuo88_m22_w01==	7431
replace ciuo88_m22_w01 =	7318	if ciuo88_m22_w01==	7432
replace ciuo88_m22_w01 =	7319	if ciuo88_m22_w01==	7331
replace ciuo88_m22_w01 =	7321	if ciuo88_m22_w01==	7341
replace ciuo88_m22_w01 =	7321	if ciuo88_m22_w01==	7342
replace ciuo88_m22_w01 =	7321	if ciuo88_m22_w01==	7343
replace ciuo88_m22_w01 =	7322	if ciuo88_m22_w01==	7341
replace ciuo88_m22_w01 =	7322	if ciuo88_m22_w01==	7346
replace ciuo88_m22_w01 =	7322	if ciuo88_m22_w01==	8251
replace ciuo88_m22_w01 =	7323	if ciuo88_m22_w01==	7345
replace ciuo88_m22_w01 =	7323	if ciuo88_m22_w01==	8252
replace ciuo88_m22_w01 =	7411	if ciuo88_m22_w01==	7137
replace ciuo88_m22_w01 =	7412	if ciuo88_m22_w01==	7241
replace ciuo88_m22_w01 =	7413	if ciuo88_m22_w01==	7245
replace ciuo88_m22_w01 =	7421	if ciuo88_m22_w01==	7242
replace ciuo88_m22_w01 =	7421	if ciuo88_m22_w01==	7243
replace ciuo88_m22_w01 =	7422	if ciuo88_m22_w01==	7242
replace ciuo88_m22_w01 =	7422	if ciuo88_m22_w01==	7243
replace ciuo88_m22_w01 =	7422	if ciuo88_m22_w01==	7244
replace ciuo88_m22_w01 =	7422	if ciuo88_m22_w01==	7245
replace ciuo88_m22_w01 =	7511	if ciuo88_m22_w01==	7411
replace ciuo88_m22_w01 =	7512	if ciuo88_m22_w01==	7412
replace ciuo88_m22_w01 =	7513	if ciuo88_m22_w01==	7413
replace ciuo88_m22_w01 =	7514	if ciuo88_m22_w01==	7414
replace ciuo88_m22_w01 =	7515	if ciuo88_m22_w01==	7415
replace ciuo88_m22_w01 =	7516	if ciuo88_m22_w01==	7416
replace ciuo88_m22_w01 =	7521	if ciuo88_m22_w01==	7421
replace ciuo88_m22_w01 =	7522	if ciuo88_m22_w01==	7422
replace ciuo88_m22_w01 =	7523	if ciuo88_m22_w01==	7423
replace ciuo88_m22_w01 =	7523	if ciuo88_m22_w01==	8240
replace ciuo88_m22_w01 =	7531	if ciuo88_m22_w01==	7433
replace ciuo88_m22_w01 =	7531	if ciuo88_m22_w01==	7434
replace ciuo88_m22_w01 =	7532	if ciuo88_m22_w01==	7435
replace ciuo88_m22_w01 =	7533	if ciuo88_m22_w01==	7436
replace ciuo88_m22_w01 =	7534	if ciuo88_m22_w01==	7437
replace ciuo88_m22_w01 =	7535	if ciuo88_m22_w01==	7441
replace ciuo88_m22_w01 =	7536	if ciuo88_m22_w01==	7442
replace ciuo88_m22_w01 =	7541	if ciuo88_m22_w01==	6152
replace ciuo88_m22_w01 =	7541	if ciuo88_m22_w01==	7216
replace ciuo88_m22_w01 =	7542	if ciuo88_m22_w01==	7112
replace ciuo88_m22_w01 =	7543	if ciuo88_m22_w01==	3152
replace ciuo88_m22_w01 =	7544	if ciuo88_m22_w01==	7143
replace ciuo88_m22_w01 =	7549	if ciuo88_m22_w01==	7322
replace ciuo88_m22_w01 =	8111	if ciuo88_m22_w01==	7111
replace ciuo88_m22_w01 =	8111	if ciuo88_m22_w01==	8111
replace ciuo88_m22_w01 =	8112	if ciuo88_m22_w01==	8112
replace ciuo88_m22_w01 =	8113	if ciuo88_m22_w01==	8113
replace ciuo88_m22_w01 =	8114	if ciuo88_m22_w01==	8212
replace ciuo88_m22_w01 =	8121	if ciuo88_m22_w01==	8121
replace ciuo88_m22_w01 =	8121	if ciuo88_m22_w01==	8122
replace ciuo88_m22_w01 =	8121	if ciuo88_m22_w01==	8123
replace ciuo88_m22_w01 =	8121	if ciuo88_m22_w01==	8124
replace ciuo88_m22_w01 =	8122	if ciuo88_m22_w01==	8223
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8151
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8152
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8153
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8154
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8155
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8159
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8221
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8222
replace ciuo88_m22_w01 =	8131	if ciuo88_m22_w01==	8229
replace ciuo88_m22_w01 =	8132	if ciuo88_m22_w01==	7344
replace ciuo88_m22_w01 =	8132	if ciuo88_m22_w01==	8224
replace ciuo88_m22_w01 =	8141	if ciuo88_m22_w01==	8231
replace ciuo88_m22_w01 =	8142	if ciuo88_m22_w01==	8232
replace ciuo88_m22_w01 =	8143	if ciuo88_m22_w01==	8253
replace ciuo88_m22_w01 =	8151	if ciuo88_m22_w01==	8261
replace ciuo88_m22_w01 =	8152	if ciuo88_m22_w01==	7432
replace ciuo88_m22_w01 =	8152	if ciuo88_m22_w01==	8262
replace ciuo88_m22_w01 =	8153	if ciuo88_m22_w01==	8263
replace ciuo88_m22_w01 =	8154	if ciuo88_m22_w01==	8264
replace ciuo88_m22_w01 =	8155	if ciuo88_m22_w01==	8265
replace ciuo88_m22_w01 =	8156	if ciuo88_m22_w01==	8266
replace ciuo88_m22_w01 =	8157	if ciuo88_m22_w01==	8264
replace ciuo88_m22_w01 =	8159	if ciuo88_m22_w01==	8269
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8271
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8272
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8273
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8274
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8275
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8276
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8277
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8278
replace ciuo88_m22_w01 =	8160	if ciuo88_m22_w01==	8279
replace ciuo88_m22_w01 =	8171	if ciuo88_m22_w01==	8142
replace ciuo88_m22_w01 =	8171	if ciuo88_m22_w01==	8143
replace ciuo88_m22_w01 =	8172	if ciuo88_m22_w01==	8141
replace ciuo88_m22_w01 =	8181	if ciuo88_m22_w01==	8131
replace ciuo88_m22_w01 =	8181	if ciuo88_m22_w01==	8139
replace ciuo88_m22_w01 =	8182	if ciuo88_m22_w01==	8162
replace ciuo88_m22_w01 =	8183	if ciuo88_m22_w01==	8290
replace ciuo88_m22_w01 =	8189	if ciuo88_m22_w01==	8290
replace ciuo88_m22_w01 =	8211	if ciuo88_m22_w01==	8281
replace ciuo88_m22_w01 =	8212	if ciuo88_m22_w01==	8282
replace ciuo88_m22_w01 =	8212	if ciuo88_m22_w01==	8283
replace ciuo88_m22_w01 =	8219	if ciuo88_m22_w01==	8284
replace ciuo88_m22_w01 =	8219	if ciuo88_m22_w01==	8285
replace ciuo88_m22_w01 =	8219	if ciuo88_m22_w01==	8286
replace ciuo88_m22_w01 =	8219	if ciuo88_m22_w01==	8290
replace ciuo88_m22_w01 =	8311	if ciuo88_m22_w01==	8311
replace ciuo88_m22_w01 =	8312	if ciuo88_m22_w01==	8312
replace ciuo88_m22_w01 =	8321	if ciuo88_m22_w01==	8321
replace ciuo88_m22_w01 =	8322	if ciuo88_m22_w01==	8322
replace ciuo88_m22_w01 =	8331	if ciuo88_m22_w01==	8323
replace ciuo88_m22_w01 =	8332	if ciuo88_m22_w01==	8324
replace ciuo88_m22_w01 =	8341	if ciuo88_m22_w01==	8331
replace ciuo88_m22_w01 =	8342	if ciuo88_m22_w01==	8332
replace ciuo88_m22_w01 =	8343	if ciuo88_m22_w01==	8333
replace ciuo88_m22_w01 =	8344	if ciuo88_m22_w01==	8334
replace ciuo88_m22_w01 =	8350	if ciuo88_m22_w01==	8340
replace ciuo88_m22_w01 =	9111	if ciuo88_m22_w01==	9131
replace ciuo88_m22_w01 =	9112	if ciuo88_m22_w01==	9132
replace ciuo88_m22_w01 =	9121	if ciuo88_m22_w01==	9133
replace ciuo88_m22_w01 =	9122	if ciuo88_m22_w01==	9142
replace ciuo88_m22_w01 =	9123	if ciuo88_m22_w01==	9142
replace ciuo88_m22_w01 =	9129	if ciuo88_m22_w01==	9142
replace ciuo88_m22_w01 =	9211	if ciuo88_m22_w01==	9211
replace ciuo88_m22_w01 =	9212	if ciuo88_m22_w01==	9211
replace ciuo88_m22_w01 =	9213	if ciuo88_m22_w01==	9211
replace ciuo88_m22_w01 =	9214	if ciuo88_m22_w01==	6113
replace ciuo88_m22_w01 =	9214	if ciuo88_m22_w01==	9211
replace ciuo88_m22_w01 =	9215	if ciuo88_m22_w01==	9212
replace ciuo88_m22_w01 =	9216	if ciuo88_m22_w01==	9213
replace ciuo88_m22_w01 =	9311	if ciuo88_m22_w01==	9311
replace ciuo88_m22_w01 =	9312	if ciuo88_m22_w01==	9312
replace ciuo88_m22_w01 =	9313	if ciuo88_m22_w01==	9313
replace ciuo88_m22_w01 =	9321	if ciuo88_m22_w01==	9322
replace ciuo88_m22_w01 =	9329	if ciuo88_m22_w01==	9321
replace ciuo88_m22_w01 =	9329	if ciuo88_m22_w01==	9322
replace ciuo88_m22_w01 =	9331	if ciuo88_m22_w01==	9331
replace ciuo88_m22_w01 =	9332	if ciuo88_m22_w01==	9332
replace ciuo88_m22_w01 =	9333	if ciuo88_m22_w01==	9333
replace ciuo88_m22_w01 =	9334	if ciuo88_m22_w01==	9333
replace ciuo88_m22_w01 =	9411	if ciuo88_m22_w01==	5122
replace ciuo88_m22_w01 =	9412	if ciuo88_m22_w01==	9132
replace ciuo88_m22_w01 =	9510	if ciuo88_m22_w01==	9120
replace ciuo88_m22_w01 =	9520	if ciuo88_m22_w01==	9112
replace ciuo88_m22_w01 =	9611	if ciuo88_m22_w01==	9161
replace ciuo88_m22_w01 =	9612	if ciuo88_m22_w01==	9161
replace ciuo88_m22_w01 =	9612	if ciuo88_m22_w01==	9321
replace ciuo88_m22_w01 =	9613	if ciuo88_m22_w01==	9162
replace ciuo88_m22_w01 =	9621	if ciuo88_m22_w01==	9151
replace ciuo88_m22_w01 =	9621	if ciuo88_m22_w01==	9152
replace ciuo88_m22_w01 =	9622	if ciuo88_m22_w01==	9162
replace ciuo88_m22_w01 =	9623	if ciuo88_m22_w01==	9153
replace ciuo88_m22_w01 =	9624	if ciuo88_m22_w01==	9162
replace ciuo88_m22_w01 =	9629	if ciuo88_m22_w01==	9152
replace ciuo88_m22_w01 =	0110	if ciuo88_m22_w01==	0110
replace ciuo88_m22_w01 =	0210	if ciuo88_m22_w01==	0110
replace ciuo88_m22_w01 =	0310	if ciuo88_m22_w01==	0110

rename ciuo88_m22_w01 ciuo08_m22_w01
rename ciuo88_m03_w01 ciuo08_m03_w01

	* Generate variables for wave 4 (based on wave 3) and wave 6 (based on wave 5)
		gen ciuo08_m03_w04 = ciuo08_m03_w03 
		gen ciuo08_m03_w06 = ciuo08_m03_w05
		order ciuo08_m03_w04, after(ciuo08_m03_w03)
		order ciuo08_m03_w06, after(ciuo08_m03_w05)
		
	* Imputation for WAVE 1
		* for wave 1: inactive and unemployed
		replace ciuo08_m03_w01=15000 if ciuo08_m03_w01==. & m02_w01==5 // retired
		replace ciuo08_m03_w01=16000 if ciuo08_m03_w01==. & m02_w01==6 // unemployed

		* for wave 1: assign household head occupation to inactive (student, domestic, disability, nini)
		replace ciuo08_m03_w01=ciuo08_m22_w01 if ciuo08_m03_w01==. 

		* for wave 1: assign individual occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
		replace ciuo08_m03_w01=ciuo08_m03_w03 if ciuo08_m03_w01==.

		* for wave 1: assign household head occupation of wave 3 to inactive at wave 1 (student, domestic, disability, nini)
		replace ciuo08_m03_w01=ciuo08_m22_w03 if ciuo08_m03_w01==.
		
		* for wave 1: assign individual occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
		replace ciuo08_m03_w01=ciuo08_m03_w05 if ciuo08_m03_w01==. 
		
		* for wave 1: assign household head occupation of wave 5 to inactive at wave 1 (student, domestic, disability, nini)
		replace ciuo08_m03_w01=ciuo08_m22_w05 if ciuo08_m03_w01==. 
		
	* Imputation for WAVE 4
		* Inactive and unemployed
		replace ciuo08_m03_w04=15000 if ciuo08_m03_w04==. & m02_w04==5 // retired
		replace ciuo08_m03_w04=16000 if ciuo08_m03_w04==. & m02_w04==6 // unemployed

		* Assign household head occupation of Wave 3 to inactive 
		replace ciuo08_m03_w04=ciuo08_m22_w03 if ciuo08_m03_w04==. 

		* Assign individual occupation of wave 1 
		replace ciuo08_m03_w04=ciuo08_m03_w01 if ciuo08_m03_w04==.

		* Assign household head occupation of wave 1
		replace ciuo08_m03_w04=ciuo08_m22_w01 if ciuo08_m03_w04==.
		
		* Assign individual occupation of wave 5 
		replace ciuo08_m03_w04=ciuo08_m03_w05 if ciuo08_m03_w04==.

		* Assign household head occupation of wave 5
		replace ciuo08_m03_w04=ciuo08_m22_w05 if ciuo08_m03_w04==.
		

	* Imputation for WAVE 6
		* Inactive and unemployed
		replace ciuo08_m03_w06=15000 if ciuo08_m03_w06==. & m02_w06==5 // retired
		replace ciuo08_m03_w06=16000 if ciuo08_m03_w06==. & m02_w06==6 // unemployed

		* Assign household head occupation of Wave 5 
		replace ciuo08_m03_w06=ciuo08_m22_w05 if ciuo08_m03_w06==.

		* Assign individual occupation of wave 3 and 4
		replace ciuo08_m03_w06=ciuo08_m03_w04 if ciuo08_m03_w06==.

		* Assign household head occupation of wave 3 
		replace ciuo08_m03_w06=ciuo08_m22_w03 if ciuo08_m03_w06==.
		
		* Assign individual occupation of wave 1
		replace ciuo08_m03_w06=ciuo08_m03_w01 if ciuo08_m03_w06==.

		* Assign household head occupation of wave 1
		replace ciuo08_m03_w06=ciuo08_m22_w01 if ciuo08_m03_w06==.
		
		* For the remaining missing values, we imput the values of 2016 (they are all unemployed or retired)
			replace ciuo08_m03_w01 = ciuo08_m03_w06 if ciuo08_m03_w01==.
			replace ciuo08_m03_w04 = ciuo08_m03_w06 if ciuo08_m03_w04==.

		* We drop the individuals without occupation (n=462)
			drop if ciuo08_m03_w01==.
		
*-----------------------*
*----- FINAL SAMPLE ----*
*-----------------------*
	
	* Drop useless variables from waves 2, 3 and 5
		drop *_w02
		drop *_w03
		drop *_w05

	* Drop other useless variables
		drop m19_w01 m21_w01 ciuo08_m22_w01 m34_01* m34_02* m02_w01 m02_w04 m02_w06 m37_01_w01 m37_02_w01 segmento_disenno
		
	* Merge with geocodigo (census tract if)
		tostring idencuesta, replace
		merge 1:1 idencuesta using "C:\Work\Github\seg-apeg\input\data\original\zona censal muestra inicial.dta"
		keep if _merge==3
		drop _merge

*Number of cases = 462
*Number of observation = 462*3 = 1386
