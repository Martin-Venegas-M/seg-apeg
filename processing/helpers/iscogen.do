	***********************************************************************************************************
	*************************************** PROCESSING ELSOC 2016 *********************************************
	***********************************************************************************************************
	clear
	cd "C:\Work\Github\seg-apeg\input\data\pre-proc"
	use elsoc_2016_created_variables_BEFORE_ISEI.dta

	* Create social class based on ISEI
		iscogen isei = isei(isco)

	* Predict ISEI for unemployed, retired and people with missing ISCO
		reg isei ln_income i.educ m0_edad
		predict predicted_isei
		replace isei = predicted_isei if isei==.
		drop predicted

	save "C:\Work\Github\seg-apeg\input\data\pre-proc\elsoc_2016_created_variables_AFTER_ISEI.dta", replace

	***********************************************************************************************************
	*************************************** PROCESSING ELSOC 2019 *********************************************
	***********************************************************************************************************

	clear
	cd "C:\Work\Github\seg-apeg\input\data\pre-proc"
	use elsoc_2019_created_variables_BEFORE_ISEI.dta

	* Create social class based on ISEI
		iscogen isei = isei(isco)

	* Predict ISEI for unemployed, retired and people with missing ISCO
		reg isei ln_income i.educ m0_edad
		predict predicted_isei
		replace isei = predicted_isei if isei==.
		drop predicted

	save "C:\Work\Github\seg-apeg\input\data\pre-proc\elsoc_2019_created_variables_AFTER_ISEI.dta", replace

	***********************************************************************************************************
	*************************************** PROCESSING ELSOC 2022 *********************************************
	***********************************************************************************************************
	clear
	cd "C:\Work\Github\seg-apeg\input\data\pre-proc"
	use elsoc_2022_created_variables_BEFORE_ISEI.dta

	* Create social class based on ISEI
		iscogen isei = isei(isco)

	* Predict ISEI for unemployed, retired and people with missing ISCO
		reg isei ln_income i.educ m0_edad
		predict predicted_isei
		replace isei = predicted_isei if isei==.
		drop predicted

	save "C:\Work\Github\seg-apeg\input\data\pre-proc\elsoc_2022_created_variables_AFTER_ISEI.dta", replace