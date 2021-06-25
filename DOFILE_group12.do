clear

use "C:\Users\trecc\OneDrive\Desktop\Gr 12 - Ateco 26.51 (1).dta"





** We deleted the following variables since theye are not provided in the text of the assignment 
drop ldebt debtonprof
**The following command provides summary statistics, in order to take a quick look to the data. From the table we observed that the variables 'oprof'& 'va' have both positive and negative values, but this is okay since they could be both.
summarize

** Since the following variables have to be positive with this command we deleted those did not meet this criteria. We could not do this step since the previous table shows that the following variables have not negative values. 
keep if empl>=0
keep if ts>=0
keep if rpub>=0
keep if lic>=0
keep if pat>=0
keep if sdebt>=0
keep if debtons>=0

** Since 'intang' is the sum of 'rpub', 'pat' and 'lic', so the sum can not be greater than intang. Then we deleted those that not respect this criteria.
keep if intang>=rpub+pat+lic

** The following command we observed the frequency of each firm
tab firm
** With this command we genarated a new variable with the frequency of the firms
bysort firm: gen firm_freq=_N
** We eliminated the firms that have less than 4 years available data
keep if firm_freq>4
browse

**Here we checked which variables have missing values
misstable summarize
**We replaced the missing values with the mean value of the previous and successive.
bysort firm: replace empl =(empl[_n-1]+empl[_n+1])/2 if empl==.
bysort firm: replace rawm=(rawm[_n-1]+rawm[_n+1])/2 if rawm==.
bysort firm: replace debtons=(debtons[_n-1]+debtons[_n+1])/2 if debtons==.

bysort firm: egen y = mean(empl)
replace empl = y if empl == .
replace empl=round(empl)
replace empl=empl+1
drop y

bysort firm: egen y = mean(rawm) 
replace rawm = y if rawm == .
drop y

bysort firm: egen y = mean(debtons) 
replace debtons = y if debtons == .
replace debtons=round(debtons,0.01)
drop y

drop if debtons==. | rawm==.

rename debtons bdebt

** SECOND ASSIGNMENT
gen labprod = ts/empl

bysort firm: gen grts = (ts[_n]-ts[_n-1])/ts[_n]

bysort firm: gen gremp = (empl[_n]-empl[_n-1])
bysort firm: gen dint = intang[_n]-intang[_n-1]
replace dint=0 if dint==.

label variable labprod "Labour Productivity"
label variable grts "annual growth rate in total sales"
label variable gremp "Annual growth rate in number of employees"
label variable dint "difference in Intangible assets at time t and at time t-1"

tabstat empl ts oprof intang rpub pat lic sdebt  labprod grts gremp dint, statistics(count mean median sd max min kurtosis skewness p1 p5 p25 p75 p95 p99) columns(statistics)

sort ID years

*3rd assignment
**es.1

ssc instal distplot

gen empl2008=empl if years==2008

distplot empl2008, rev xscale(log) yscale(log) midpoint xlabel(0 1 10 100 1000 10000) ylabel(0 0.01 0.1 1) ytitle(Right distribution cumulative function) xtitle(Employment 2008) title(Firm size distribution)

gen empl2017=empl if years==2017

distplot empl2017, rev xscale(log) yscale(log) midpoint xlabel(0 1 10 100 1000 10000) ylabel(0 0.01 0.1 1) ytitle(Right distribution cumulative function) xtitle(Employment 2017) title(Firm size distribution)

distplot empl, rev xscale(log) yscale(log) midpoint xlabel(0 1 10 100 1000 10000) ylabel(0 0.01 0.1 1) ytitle(Right distribution cumulative function) xtitle(Employment 2008-2017) title(Firm size distribution)

**2.a

xtset ID years

**Testing for Gibrat law. (proxy of size = ts)

bysort ID: gen logts=ln(ts)
bysort ID: gen logtsrate=ln(ts[_n]/ts[_n-1])
bysort ID: gen logtsratet_1=logtsrate[_n-1]

** Application of different econometric models.

*bysort ID: gen logtst_1=ln(ts[_n-1])

*reg logts logtst_1
*xtreg logts logtst_1

xtabond2 logts l.logts, gmm(l.logts)

outreg2 using tabella2.doc, word replace ctitle(Firm size (Total sales))


**2.b
** 

bysort ID: gen yr=year(year)
bysort ID: gen age=years-yr
bysort ID: gen logage=ln(age)

correlate logtsrate empl logage oprof rpub lic pat gremp intang grts dint rawm 

generate y_08 = 0 
replace y_08 = 1 if years==2008
generate y_09 = 0 
replace y_09 = 1 if years==2009
generate y_10 = 0 
replace y_10 = 1 if years==2010
generate y_11 = 0 
replace y_11 = 1 if years==2011
generate y_12 = 0 
replace y_12 = 1 if years==2012
generate y_13 = 0 
replace y_13 = 1 if years==2013
generate y_14 = 0 
replace y_14 = 1 if years==2014
generate y_15 = 0 
replace y_15 = 1 if years==2015
generate y_16 = 0 
replace y_16 = 1 if years==2016
generate y_17 = 0 
replace y_17 = 1 if years==2017

xtreg logtsrate logtsratet_1 oprof rpub pat lic sdebt labprod logage, fe

estimates store FE

xtreg logtsrate logtsratet_1 oprof rpub pat lic sdebt labprod logage, re

estimates store RE

hausman FE RE

** 5

xtabond2 logtsrate l.logtsrate oprof rpub pat lic sdebt labprod logage y_*, gmm(l.logtsrate) iv(oprof rpub pat lic sdebt labprod logage y_*) robust small

outreg2 using FINALEdoc, word replace ctitle(Firm growth (Total sales))

** 4th assignment

gen survival= strpos(firm,"LIQUIDAZIONE")
gen survival1= 1 if survival==0
replace survival1= 0 if survival1==.
drop survival
gen logempl= ln(empl)

heckman logtsrate oprof rpub pat lic sdebt labprod logage y_*, select(survival1= oprof rpub pat lic labprod logage sdebt logempl) twostep

outreg2 using HECHMANdocx, word replace ctitle(heckman)
