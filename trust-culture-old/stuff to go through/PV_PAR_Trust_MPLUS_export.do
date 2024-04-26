/*********************************************************
Export PV PAR data for mplus.
**************/

* Import the competencies.
import 	delimited "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/Projects/PAR_Trust/comps.csv" , clear
drop 	v1
rename 	v2 id
rename 	v3 competence
sort 	id
save 	"/Users/jyoung20/Dropbox (ASU)/Perryville PAR/Projects/PAR_Trust/comps.dta" , replace

* Get the full data file.
cd 		"/Users/jyoung20/Dropbox (ASU)/Perryville PAR/"
use 	"PV PAR Interviews Full_CLEANED.dta" , clear
sort 	id

* merge the competence.
merge 		id using "/Users/jyoung20/Dropbox (ASU)/Perryville PAR/Projects/PAR_Trust/comps.dta" 
tab 		_merge
drop 		_merge


* Take a look at the models.
egen relhlth = rowmean( S3SS2_1-S3SS2_14 )
egen safety  = rowmean( S3SS3_1-S3SS3_7 )

reg relhlth competence InterviewerType Randomize S4Q1 white black hispanic , robust
reg safety competence InterviewerType Randomize S4Q1 white black hispanic , robust



* Keep just these for the export.
keep 	S3SS1_1-S3SS3_7 competence


/*

I just added this reverse coding to the main file

* Reverse code items for the relational health and psychological safety scales.
foreach var of varlist S3SS2_4 S3SS2_7 S3SS2_9 S3SS2_10 S3SS3_1 S3SS3_3 S3SS3_5 {
	recode  `var' (5 = 1) (4 = 2) (3 = 3) (2 = 4) (1 = 5)
}

*/

* Export to mplus.
stata2mplus using "PV PAR Interviews Full_CLEANED" , replace

/**************
End of do file.
**************/

