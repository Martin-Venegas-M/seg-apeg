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
		cd "C:\Work\Github\seg-apeg\processing\"	
		do "2_individual_level_variable_creation.do"

	* Merge with variable on neighborhood NSE
		merge m:1 geocodigo year using "C:\Work\Github\seg-apeg\input\data\original\nse_barrio_vf.dta"
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
	
	* Save final df
		save "C:\Work\Github\seg-apeg\input\data\proc\elsoc_proc_crossectional.dta", replace
	