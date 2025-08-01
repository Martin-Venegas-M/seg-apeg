**********************************************
**********************************************
*** Project: NSE-COB                       ***
*** Author: QR + GO                        ***
*** Date: April 2024 					   ***
**********************************************
**********************************************

*** This dofile do three things for 2012 and 2017:
	* 1) Creates variables to measure neighborhood socioeconomic context
	* 2) Run PCA to reduce dimensionality
	* 3) Store the predicted score for each observation on component 1 in a new variables
	* 4) Normalize the predicted score (0-1) to get a clearer NSE neighborhood variable
	**** Note: neighborhood is defined as Zonas Censales
	
	
*---------------------------------*
***----- Neighborhood 2017 -----***
*---------------------------------*


*-------------------------------*
*** Step 1: variable creation ***
*-------------------------------*

******* File preparation
program define file_prepa

	* Open file
		clear
		import delimited "G:\Mi unidad\data chili\chile_data\censo\censo2017\archivos originales\Microdato_Censo2017-Personas\Microdato_Censo2017-Personas.csv"
		
	* Create household ID
		egen id_hogar = concat(id_zona_loc nviv nhogar), punct("_")

	* Create census tract id
		tostring comuna, gen(str_comuna)
		gen str_distrito=string(int(dc),"%02.0f")
		tostring area, gen(str_area)
		gen str_zonloc = string(int(zc_loc),"%03.0f") 
		egen geocodigo=concat(str_comuna str_distrito str_area str_zonloc)		
	
	* Replace urban area for this census tract to be match with elsoc
		replace area=1 if geocodigo=="13125022002"
		replace area=1 if geocodigo=="13603012004"
		replace area=1 if geocodigo=="13603022003"
		replace area=1 if geocodigo=="13603022004"
		replace area=1 if geocodigo=="13503011001" 

 	* Remove people living in rural areas
		keep if area==1
		
	* Remove people living in "undetermined" census tract
		drop if str_zonloc=="999"
	
	* Remove individuals that are not permanent household member
		drop if p07>=16
	
	* Keep only residents of Santiago Metropolitan Region
		keep if region==13
end

	
******* Unemployment rate
	
	* Prepare file
		file_prepa
	
	* Keep individuals aged 15 and above
		keep if p09>=15
		
	* Create dummies for categories	
		gen employed=(p17==1 | p17==3)
		gen unemployed=(p17==4)
		gen inactive=(p17==2 | p17==5 | p17==6 | p17==7 | p17==8)
		gen active=(employed==1 | unemployed==1) & inactive==0
	
	* Total unemployed and active population by census tract
		foreach var of varlist unemployed active {
			bys geocodigo : egen tot_`var' = total(`var')
		}

	* Unemployment rate by census tract	
		gen unemployment = tot_unemployed/tot_active

	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo unemployment
		
	* Save temporary file
		tempfile unemployment
		save `unemployment'

******* Highly and poorly educated people

	* Prepare file
		file_prepa

	* Keep individuals aged 15 and above 
		keep if p09>=15
	
	* Remove individuals without information on education level
		drop if p15==99 | p15==98
		drop if p15a==99 | p15a==98

	* Create educational level
		gen educ_level = 0 
		
		* No education, primary complete and secondary incomplete (level 1)
			replace educ_level = 1 if p15<=4 
			replace educ_level = 1 if p15==5 & p15a==2
			replace educ_level = 1 if p15==6 & p15a==2

			replace educ_level = 1 if p15==5 & p15a==1
			replace educ_level = 1 if p15==6 & p15a==1
			replace educ_level = 1 if p15==7 & p15a==2
			replace educ_level = 1 if p15==8 & p15a==2
			replace educ_level = 1 if p15==9 & p15a==2
			replace educ_level = 1 if p15==10 & p15a==2

		* Secondary complete and tecnical incomplete (level 2)
			replace educ_level = 2 if p15==7 & p15a==1
			replace educ_level = 2 if p15==8 & p15a==1
			replace educ_level = 2 if p15==9 & p15a==1
			replace educ_level = 2 if p15==10 & p15a==1
			replace educ_level = 2 if p15==11 & p15a==2
		
		* Tecnical complete and profesional incomplete (level 3)
			replace educ_level = 3 if p15==11 & p15a==1
			replace educ_level = 3 if p15==12 & p15a==2

		* Profesional complete and postgraduate (level 4)
			replace educ_level = 4 if p15==12 & p15a==1
			replace educ_level = 4 if p15==13 & p15a==2
			replace educ_level = 4 if p15==14 & p15a==2
			replace educ_level = 4 if p15==13 & p15a==1
			replace educ_level = 4 if p15==14 & p15a==1
		
	* Generate dummies
		tabulate educ_level , generate(d_education)
		drop d_education2 d_education3
		gen obs = 1
	
	* Total by census tract
		foreach var of varlist d_education1 d_education4 obs {
			bys geocodigo : egen tot_`var' = total(`var')
		}
	
	* Share of people without secondary education
		gen low_educ = tot_d_education1/tot_obs
		
	* Share of people with college and postgraduate educational
		gen high_educ = tot_d_education4/tot_obs

	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo low_educ high_educ
		
	* Save temporary file
		tempfile educ
		save `educ'

******* Youth nini

	* Prepare file
		file_prepa
		
	* Keep only youth nini
		keep if p09>=16 & p09<=24
	
	* Dummy for not in education of employment work or study
		gen no_study_or_work=0
		replace no_study_or_work = 1 if p17==2
		replace no_study_or_work = 1 if p17==4
		replace no_study_or_work = 1 if p17==6
		replace no_study_or_work = 1 if p17==7
		replace no_study_or_work = 1 if p17==8
		drop if p17==99
	
	* Observation variable
		gen obs=1
	
	* Total by census tract
		foreach var of varlist no_study_or_work obs {
				bys geocodigo: egen tot_`var'=total(`var')
			}
	
	* Proporcion por zonas
		gen youth_nini = tot_no_study_or_work/tot_obs
	
	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo youth_nini
			
	* Save intermediate file
		tempfile youth_nini
		save `youth_nini'

******* Teenager pregnancy

	* Prepare file
		file_prepa
	
	* Keep only teenager women
		keep if p09>=13 & p09<=19
	
	* Keep only women
		keep if p08==2
		
	* Maternidad adolescente (% de mujeres entre 13 y 19 años que son madres)
		gen has_children=0
		replace has_children=1 if p20>0 & p20<98
	
	* Observation variable
		gen obs=1
		
	* Total census tract
			foreach var of varlist has_children obs {
				bys geocodigo: egen tot_`var'=total(`var')
			}

	* Proporcion por zonas
		gen teen_pregnancy = tot_has_children/tot_obs

	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo teen_pregnancy
			
	* Save intermediate file
		tempfile teen_pregnancy
		save `teen_pregnancy'		

******* Merge all census variables
	merge 1:1 geocodigo using `unemployment'
	keep if _merge==3
	drop _merge
	merge 1:1 geocodigo using `educ'
	keep if _merge==3
	drop _merge
	merge 1:1 geocodigo using `youth_nini'
	keep if _merge==3
	drop _merge

******* Merge with land value and make imputation variable

	* Make the merge
		merge 1:1 geocodigo using "E:\Mi unidad\data chili\chile_data\ismt\ismt.dta", keepusing(uf2018)
		drop if _merge==2
		drop _merge

	* Estimate land values in dollar (en promedio en 2018: 1 UF = 27166 CLP, fuente SII, and 1 dollar = 640 CLP)
		gen land_value = (uf2018*27166)/640
		drop uf2018

	* Handle missing values with simple imputation (the mean of the COMUNA)
		replace land =. if land==0
		gen comuna = substr(geocodigo, 1, 5)
		bys comuna: egen mean_land = mean(land)
		replace land = mean_land if land==.
		drop mean_land
		
		egen mean_land = mean(land_value)
		replace land_value = mean_land if land_value==.
		drop mean_land
		
	* Merge with elsoc sample (n=462) in the year 2019 and 2022
		merge 1:m geocodigo using "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\sample georef 2019 2022.dta"
		keep if _merge==3
		drop _merge
	
	* Keep only one observation per census tract
		bys geocodigo: gen dup = _n
		keep if dup==1
		drop dup
	
	* Drop useless variables
		keep unemployment low_educ high_educ youth_nini teen_pregnancy land_value geocodigo
		
*----------------------------------------------*
*** Step 2, 3, and 4: PCA and score creation ***
*----------------------------------------------*

	* Run PCA
		pca unemployment low_educ high_educ youth_nini teen_pregnancy land_value
	
	* Plots
		screeplot, yline(1) ci(het)
		scoreplot, msize(.5) xline(0) yline(0)

	* Store scores for all neighborhood on pc1
		predict pc1, score
		gen inv_pc1 = pc1*-1
	
	* Normalize score
		egen max_inv_pc1=max(inv_pc1)
		egen min_inv_pc1=min(inv_pc1)
		gen nse_barrio=(inv_pc1-min_inv_pc1)/(max_inv_pc1-min_inv_pc1)
	
	* Keep only relevant variables
		keep geocodigo nse_barrio
	
	* Save file
		gen year = 2019
		save "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\nse_barrio_2019.dta", replace
		drop year
		gen year = 2022
		save "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\nse_barrio_2022.dta", replace
		clear

*---------------------------------*
***----- Neighborhood 2012 -----***
*---------------------------------*

******* File preparation
program define file_prepa

	* Open file
		clear
		use "E:\Mi unidad\data chili\chile_data\censo\censo2012\stata\indiv.dta"
		
	* Create housing unit ID
		tostring folio, replace
		tostring nviv, replace
		egen id_viv = concat(folio nviv), punct("_")
	
	* Merge with geographic infomation
		merge m:1 id_viv using "E:\Mi unidad\data chili\chile_data\censo\censo2012\stata\idgeo completo.dta"
		keep if _merge==3
		drop _merge
		
	* Keep Metropolitan Region right now to speed up
		keep if region=="13"

	* Create census tract id
		gen zonloc = ""
		replace zonloc = zona if area=="1"
		replace zonloc = localidad if area=="2"
		egen geocodigo=concat(com dto area zonloc)		
	
	* Replace urban area for this census tract to be match with elsoc
		*replace area=1 if geocodigo=="13125022002"

 	* Remove people living in rural areas
		keep if area=="1"
		
	* Remove people living in "undetermined" census tract
		drop if zonloc=="999"
	
	* Remove individuals that are not permanent household member
		drop if dpar>=16

end

	
******* Unemployment rate
	
	* Prepare file
		file_prepa
	
	* Keep individuals aged 15 and above
		keep if p20c>=15
		
	* Create dummies for categories	
		gen employed=(p36==1 | p36==2)
		gen unemployed=(p36==5)
		gen inactive=(p36==3 | p36==4 | p36==6 | p36==7 | p36==8)
		gen active=(employed==1 | unemployed==1) & inactive==0
	
	* Total unemployed and active population by census tract
		foreach var of varlist unemployed active {
			bys geocodigo : egen tot_`var' = total(`var')
		}

	* Unemployment rate by census tract	
		gen unemployment = tot_unemployed/tot_active

	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo unemployment
		
	* Save temporary file
		tempfile unemployment
		save `unemployment'

******* Highly and poorly educated people

	* Prepare file
		file_prepa

	* Keep individuals aged 15 and above 
		keep if p20c>=15
	
	* Remove individuals without information on education level
		drop if p29==0
		
	* Create educational level
		gen educ_level = 0 
		
		* No education, primary complete and secondary incomplete (level 1)
			replace educ_level = 1 if p28<=5 
			replace educ_level = 1 if p28==6 & p29==2
			replace educ_level = 1 if p28==7 & p29==2

		* Secondary complete and tecnical incomplete (level 2)
			replace educ_level = 2 if p28==6 & p29==1
			replace educ_level = 2 if p28==7 & p29==1
			replace educ_level = 2 if p28==8 & p29==2
			
		* Tecnical complete and profesional incomplete (level 3)
			replace educ_level = 3 if p28==8 & p29==1
			replace educ_level = 3 if p28==9 & p29==2

		* Profesional complete and postgraduate (level 4)
			replace educ_level = 4 if p28==9 & p29==1
			replace educ_level = 4 if p28==10
			replace educ_level = 4 if p28==11
			replace educ_level = 4 if p28==12
	
	* Generate dummies
		tabulate educ_level , generate(d_education)
		drop d_education2 d_education3
		gen obs = 1
	
	* Total by census tract
		foreach var of varlist d_education1 d_education4 obs {
			bys geocodigo : egen tot_`var' = total(`var')
		}
	
	* Share of people without secondary education
		gen low_educ = tot_d_education1/tot_obs
		
	* Share of people with college and postgraduate educational
		gen high_educ = tot_d_education4/tot_obs

	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo low_educ high_educ
		
	* Save temporary file
		tempfile educ
		save `educ'

******* Youth nini

	* Prepare file
		file_prepa
		
	* Keep only youth nini
		keep if p20c>=16 & p20c<=24
	
	* Dummy for not in education of employment work or study
		gen no_study_or_work=0
		replace no_study_or_work = 1 if p36==3
		replace no_study_or_work = 1 if p36==5
		replace no_study_or_work = 1 if p36==6
		replace no_study_or_work = 1 if p36==7
		replace no_study_or_work = 1 if p36==8
		drop if p36==0
	
	* Observation variable
		gen obs=1
	
	* Total by census tract
		foreach var of varlist no_study_or_work obs {
				bys geocodigo: egen tot_`var'=total(`var')
			}
	
	* Proporcion por zonas
		gen youth_nini = tot_no_study_or_work/tot_obs
	
	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo youth_nini
			
	* Save intermediate file
		tempfile youth_nini
		save `youth_nini'
		
******* Teenager pregnancy

	* Prepare file
		file_prepa
	
	* Keep only teenager women
		keep if p20c>=13 & p20c<=19
	
	* Keep only women
		keep if p19==2
		
	* Maternidad adolescente (% de mujeres entre 13 y 19 años que son madres)
		drop if p41=="NA"
		destring p40 p41, replace
		gen has_children=0
		replace has_children=1 if p41>0 
	
	* Observation variable
		gen obs=1
		
	* Total census tract
			foreach var of varlist has_children obs {
				bys geocodigo: egen tot_`var'=total(`var')
			}

	* Proporcion por zonas
		gen teen_pregnancy = tot_has_children/tot_obs

	* Keep one observation by census tract
		bys geocodigo : gen dup = _n
		keep if dup==1
		
	* Keep relevant variables
		keep geocodigo teen_pregnancy
			
	* Save intermediate file
		tempfile teen_pregnancy
		save `teen_pregnancy'		

******* Merge all census variables
	merge 1:1 geocodigo using `unemployment'
	keep if _merge==3
	drop _merge
	merge 1:1 geocodigo using `educ'
	keep if _merge==3
	drop _merge
	merge 1:1 geocodigo using `youth_nini'
	keep if _merge==3
	drop _merge

******* Merge with land value and make imputation variable

	* Make the merge
		merge 1:1 geocodigo using "E:\Mi unidad\projets chili\SEG_APEG\data\land_values_2012\land_value_2012.dta", keepusing(mean_ufm2)
		drop if _merge==2
		drop _merge

	* Estimate land values in dollar (en promedio en 2012: 1 UF = 22597 CLP, fuente SII, and 1 dollar = 487 CLP)
		gen land_value = (mean_ufm2*22597)/487
		drop mean_ufm2

	* Handle missing values with simple imputation (the mean of the COMUNA)
		replace land_value =. if land_value==0
		gen comuna = substr(geocodigo, 1, 5)
		bys comuna: egen mean_land = mean(land_value)
		replace land_value = mean_land if land_value==.
		drop mean_land
	
		egen mean_land = mean(land_value)
		replace land_value = mean_land if land_value==.
		drop mean_land
	
	* Merge with elsoc sample (n=462) in the year 2016
		merge 1:m geocodigo using "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\sample georef 2016.dta"
		keep if _merge==3
		drop _merge
	
	* Keep only one observation per census tract
		bys geocodigo: gen dup = _n
		keep if dup==1
		drop dup
	
	* Drop useless variables
		keep unemployment low_educ high_educ youth_nini teen_pregnancy land_value geocodigo

*----------------------------------------------*
*** Step 2, 3, and 4: PCA and score creation ***
*----------------------------------------------*

	* Run PCA
		pca unemployment low_educ high_educ youth_nini teen_pregnancy land_value
	
	* Plots
		screeplot, yline(1) ci(het)
		scoreplot, msize(.5) xline(0) yline(0)

	* Store scores for all neighborhood on pc1
		predict pc1, score
		gen inv_pc1 = pc1*-1
	
	* Normalize score
		egen max_inv_pc1=max(inv_pc1)
		egen min_inv_pc1=min(inv_pc1)
		gen nse_barrio=(inv_pc1-min_inv_pc1)/(max_inv_pc1-min_inv_pc1)
	
	* Keep only relevant variables
		keep geocodigo nse_barrio
	
	* Save file
		gen year = 2016
		save "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\nse_barrio_2016.dta", replace
		
*---------------------------------------------------*
*----- Append the three files of neighborhoods -----*
*---------------------------------------------------*
	
	* Append
		cd "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios"
		append using nse_barrio_2019 nse_barrio_2022
	
	* Save final file barrios
		save  "E:\Mi unidad\projets chili\SEG_APEG\data\variables barrios\nse_barrio_vf.dta", replace
		
		
		
		