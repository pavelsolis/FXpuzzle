/* Codes for 'Does the Exchange Rate Respond to Monetary Policy in Mexico? Solving an
 Exchange Rate Puzzle in Emerging Markets' by Pavel Solís (pavel.solis@gmail.com) */
* \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\//////////////////////////////////////////
* ==============================================================================


* ==============================================================================
* Preamble
* ==============================================================================
* Define (platform-independent) directories
cd ~
cd Documents
cd FXpuzzle								// adjust as necessary
cd Data
cd Analytic
global pathdata `c(pwd)'
cd ..
cd ..
cd Docs
cd Tables
global pathtbls `c(pwd)'
cd ..
cd Figures
global pathfigs `c(pwd)'
cd $pathdata

 * Load dataset
import delimited data.csv, clear
	gen date2 = date(date, "DM20Y")
	format date2 %td
	drop date
	rename date2 date
	order date, first
	gen datem = mofd(date)				// for graphs
	format datem %tmCCYY
save data.dta, replace

/* Notes
Below are the definitions of (already included) key variables in the dataset:
gen byte regular = !inlist(date,td(27apr2004),td(17feb2016),td(20mar2020),td(21apr2020))
gen byte idx04 	 = date >= td(1jan2004)
gen byte idx11 	 = date >= td(1jan2011)
gen trmsprd10_2	 = gmxn10yr - gmxn02yr
gen trmsprd10_1	 = gmxn10yr - gmxn01yr
gen expcttg 	 = mxonbr - mpswc		// expected part of policy rate announcement
gen expcttg_ttdm = mxonbr - mpswc_ttdm
gen merhs 		 = mpswc_ttdm - mpswc	// ME in PRS: noisy measure minus 'true'
gen melhsfx 	 = usdmxn_ttdm - usdmxn	// ME in FX */


* ==============================================================================
* Policy Rate Surprises
* ==============================================================================

* ------------------------------------------------------------------------------
* Figure 1. Policy Rate in Mexico: Change vs. Surprises
use data.dta, replace
local figlbl "prshocks"
label variable mxonbr "Rate Change"
label variable mpswc "Policy Rate Surprise"
forval i = 2011(2)2021 {
	local x = m(`i'm1)
	local X "`X'`x' " 
 }
twoway 	line mpswc datem if idx11, yaxis(1) lcolor(black) || ///
		line mxonbr datem if idx11, yaxis(2) lpattern(dash) lcolor(gs5) ||, ///
		yscale(range(-50 50) axis(1) outergap(-24)) ytitle("Basis Points", axis(1) placement(n) orient(horizontal)) ylabel(-50(25)50, axis(1)) ///
		yscale(range(-50 50) axis(2)) ytitle("", axis(2)) ylabel(, nolabels axis(2)) ///
		xtitle("") xlabel(`X') legend(ring(0) position(6) bmargin(small))
gr_edit yaxis1.title.DragBy 4 29
quietly graph export $pathfigs/`figlbl'.eps , replace
* ------------------------------------------------------------------------------


* ------------------------------------------------------------------------------
* Table 1. Summary Statistics for Asset Price Changes
use data.dta, replace
local tbllbl "f_prssumm"

* Set variables used in summary
matrix drop _all
local vars1 mpswc
local vars2 usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr
local vars3 mpswc_ttdm 
local vars4 usdmxn_ttdm gmxn02yr_ttdm gmxn05yr_ttdm gmxn10yr_ttdm gmxn30yr_ttdm

* Intraday
quiet estpost summarize `vars1' if idx11 & regular
matrix t1 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )
matrix rownames t1 = "Mean" "Std. Dev." "Min." "Max." "Obs"
matrix t1 = t1'
quiet estpost summarize `vars1' if `vars1' > 0 & idx11 & regular
matrix t2 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
quiet estpost summarize `vars1' if `vars1' < 0 & idx11 & regular
matrix t3 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
quiet estpost summarize `vars2' if idx11 & regular
matrix t4 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
matrix tableid = ( t1 \ t2 \ t3 \ t4 )

* Daily
quiet estpost summarize `vars3' if idx11 & regular
matrix t1 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
quiet estpost summarize `vars3' if `vars3' > 0 & idx11 & regular
matrix t2 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
quiet estpost summarize `vars3' if `vars3' < 0 & idx11 & regular
matrix t3 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
quiet estpost summarize `vars4' if idx11 & regular
matrix t4 = ( e(mean) \ e(sd) \ e(min) \ e(max) \ e(count) )'
matrix tabledy = ( t1 \ t2 \ t3 \ t4 )

matrix tableboth = ( tableid \ tabledy )
matrix rownames tableboth = "Intraday:Policy rate surprises" "Intraday:\quad PRS \(>\) 0" "Intraday:\quad PRS \(<\) 0" "Intraday:FX returns" "Intraday:\(\Delta\) 2-year yield" "Intraday:\(\Delta\) 5-year yield" "Intraday:\(\Delta\) 10-year yield" "Intraday:\(\Delta\) 30-year yield" "Daily:Policy rate surprises" "Daily:\quad PRS \(>\) 0" "Daily:\quad PRS \(<\) 0" "Daily:FX returns" "Daily:\(\Delta\) 2-year yield" "Daily:\(\Delta\) 5-year yield" "Daily:\(\Delta\) 10-year yield" "Daily:\(\Delta\) 30-year yield"

esttab matrix(tableboth, fmt(1 1 1 1 0)) using "$pathtbls/`tbllbl'.tex", replace fragment nomtitles nonumbers nonotes noobs label booktabs varlabels(, elist(gmxn30yr \midrule))
* ------------------------------------------------------------------------------


* ==============================================================================
* Intaday and Daily Regressions
* ==============================================================================

* ------------------------------------------------------------------------------
* Table 2. The Response of Asset Prices to PRS: Intraday Data
use data.dta, replace
local tbllbl "f_factorid"

label variable mpswc "PR surprise"
label variable expcttg "PR expected"

local depvars "usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr"
local indvars "mpswc"

eststo clear
local j = 0
foreach depvar of varlist `depvars' {
	local ++j
	eststo mdl`j': quiet regress `depvar' `indvars' if idx11 & regular, robust nocons
	local ++j
	eststo mdl`j': quiet regress `depvar' `indvars' expcttg if idx11 & regular, robust nocons
}
esttab mdl* using "$pathtbls/`tbllbl'.tex", replace fragment cells(b(fmt(a2) star) se(fmt(%4.2fc) par)) ///
starl( * 0.10 ** 0.05 *** 0.010) keep(`indvars' expcttg) nomtitles nonumbers nonotes nolines noobs label booktabs collabels(none) stats(N r2, fmt(%4.0fc %4.2fc) labels("Observations" "R-squared")) ///
mgroups("\(\Delta\) FX" "\(\Delta\) 2Y yield" "\(\Delta\) 5Y yield" "\(\Delta\) 10Y yield" "\(\Delta\) 30Y yield", pattern(1 0 1 0 1 0 1 0 1 0) ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
varlabels(, elist(expcttg \midrule))
* ------------------------------------------------------------------------------


* ------------------------------------------------------------------------------
* Table 3. The Response of Asset Prices to PRS: Daily Data
use data.dta, replace
local tbllbl "f_factordy"
local depvars "usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr"
local indvars "mpswc"
local opts_tbl "parentheses(stderr) format(%3.2fc) asterisk()"

tempfile tmpfile1
local repapp "replace"
foreach depvar of varlist `depvars' {
	quiet regress `depvar'_ttdm `indvars'_ttdm if idx11 & regular & `depvar' != ., robust nocons
	regsave using "`tmpfile1'", table(`depvar'_11, `opts_tbl') `repapp'
	local repapp "append"
	quiet regress `depvar'_ttdm `indvars'_ttdm if idx04 & regular, robust nocons
	regsave using "`tmpfile1'", table(`depvar'_04, `opts_tbl') `repapp'
}

* Format results
use "`tmpfile1'", clear
drop if strpos(var,"_cons") != 0
replace var = "" if strpos(var,"stderr")
replace var = subinstr(var,"_coef","",.)
replace var = "Observations" if var == "N"
replace var = "R-squared" if var == "r2"
replace var = "PR surprise" if strpos(var,"_ttdm")
expand 3 in 3				// duplicate observations to highlight difference in sample period
expand 2 in 4
drop if inlist(_n,3,4)
foreach var of varlist *_04 {
	replace `var' = "" in 3
	}
foreach var of varlist *_11 {
	replace `var' = "" in 4
	}
replace var = "Obs. since 2011" if var == "Observations" in 3
replace var = "Obs. since 2004" if var == "Observations" in 4

* Outsheet results
local tblcap "The Response of Asset Prices to Policy Rate Surprises: Daily Data"
local hdrln1 "&\multicolumn{2}{c}{\(\Delta\)-FX}&\multicolumn{2}{c}{\(\Delta\)-2Y-yield}&\multicolumn{2}{c}{\(\Delta\)-5Y-yield}&\multicolumn{2}{c}{\(\Delta\)-10Y-yield}&\multicolumn{2}{c}{\(\Delta\)-30Y-yield}"
local hdrln2 "\cmidrule(lr){2-3}\cmidrule(lr){4-5}\cmidrule(lr){6-7}\cmidrule(lr){8-9}\cmidrule(lr){10-11}"
tempfile x y z
texsave using `x', replace frag headerlines(`hdrln1' `hdrln2') hlines(-3) nonames
filefilter `x' `y', from("\BSbegin") to("%\BSbegin") replace
filefilter `y' `z', from("\BSend") to("%\BSend") replace
filefilter `z' `y', from("\BSnewcolumntype") to("%\BSnewcolumntype") replace
filefilter `y' `z', from("\BStoprule") to("%\BStoprule") replace
filefilter `z' `y', from("\BSmidrule \BSadd") to("%\BSmidrule \BSadd") replace
filefilter `y' `z', from("Y-") to("Y ") replace
filefilter `z' `x', from("\BS)-") to("\BS) ") replace
filefilter `x' "$pathtbls/`tbllbl'.tex", from("\BSbottomrule") to("%\BSbottomrule") replace
* ------------------------------------------------------------------------------


* ==============================================================================
* Persistence
* ==============================================================================

* ------------------------------------------------------------------------------
* Figures 2 & D1. Persistence of the Exchange Rate & Yield Curve Response to PRS
use data.dta, replace
label variable usdmxn ""
local depvars "usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr"
local indvars "mpswc"
local figroot "persistprs"
foreach depvar of varlist `depvars' {
	local varlbl: variable label `depvar'
	forvalues i = 0/10 {
		quiet regress p`i'`depvar' `indvars' if idx11 & regular, robust nocons
		estimates store d_`i'
	}
	coefplot d_*, keep(mpswc) vertical yline(0) legend(off) pstyle(p1) nooffsets ///
		asequation swapnames eqrename(^d_([0-9][0]?)$ = \1\2, regex) ///
		ytitle("Basis Points", placement(n) orient(horizontal)) xtitle("Days") title("`varlbl'") ///
		yscale(range(0 2) outergap(-24)) cirecast(rcap) name(target, replace) ciopts(lcolor(black)) mcolor(black)
	gr_edit yaxis1.title.DragBy 4 29
	quiet graph export $pathfigs/`figroot'`depvar'.eps, replace
}
* ------------------------------------------------------------------------------


* ==============================================================================
* FX Puzzle
* ==============================================================================

* ------------------------------------------------------------------------------
* Table 4. The Response of the Exchange Rate to PRS
use data.dta, replace
local tbllbl "f_factorfx"

label variable mpswc "Intraday PRS"
label variable mpswc_ttdm "Daily PRS"

local depvars "usdmxn usdmxn_ttdm"
local indvars "mpswc mpswc_ttdm"

eststo clear
local j = 0
foreach depvar of varlist `depvars' {
	foreach indvar of varlist `indvars' {
		local ++j
		eststo mdl`j': quiet regress `depvar' `indvar' if idx11 & regular, robust nocons
	}
}
esttab mdl* using "$pathtbls/`tbllbl'.tex", replace fragment cells(b(fmt(a2) star) se(fmt(a2) par)) ///
starl( * 0.10 ** 0.05 *** 0.010) keep(`indvars') nomtitles nonumbers nonotes nolines noobs label booktabs collabels(none) stats(N r2, fmt(%4.0fc %4.2fc) labels("Observations" "R-squared")) ///
mgroups("Intraday FX returns" "Daily FX returns", pattern(1 0 1 0) ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
varlabels(, elist(mpswc_ttdm \midrule))
* ------------------------------------------------------------------------------


* ==============================================================================
* Potential Omitted Variables
* ==============================================================================

* ------------------------------------------------------------------------------
* Tables 5 & F1. FX and 10Y Yield Response to PRS and Omitted Variables
use data.dta, clear

label variable mpswc "Intraday PRS"
label variable mpswc_ttdm "Daily PRS"
label variable dvix "\(\Delta\)VIX"
label variable dh15t2y "\(\Delta\) 2Y yield"
label variable levelwti "WTI price"
label variable iclmsurp "IJC surprise"

local x1 "mpswc"
local x2 "mpswc_ttdm"
local z1 "dvix"
local z2 "dh15t2y"
local z3 "levelwti"
local z4 "iclmsurp"
local x3 "`x1' `z1' `z2' `z3' `z4'"
local x4 "`x2' `z1' `z2' `z3' `z4'"

eststo clear
local j = 0
forvalues k = 1/4 {
	local ++j
	eststo mdl`j': quiet regress usdmxn_ttdm `x`k'' if regular & idx11, robust nocons
	eststo apx`j': quiet regress gmxn10yr_ttdm `x`k'' if regular & idx11 & date >= td(1jan2013), robust nocons
}
local tbllbl "f_factorov_fx"
esttab mdl* using "$pathtbls/`tbllbl'.tex", replace fragment cells(b(fmt(a2) star) se(fmt(a2) par)) ///
starl( * 0.10 ** 0.05 *** 0.010) nomtitles nonumbers nonotes nolines noobs label booktabs collabels(none) stats(N r2, fmt(%4.0fc %4.2fc) labels("Observations" "R-squared")) ///
order(mpswc mpswc_ttdm) mgroups("Daily FX returns", pattern(1 0 0 0 0) ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
varlabels(, elist(`z4' \midrule))

local tbllbl "f_factorov_10y"
esttab apx* using "$pathtbls/`tbllbl'.tex", replace fragment cells(b(fmt(a2) star) se(fmt(a2) par)) ///
starl( * 0.10 ** 0.05 *** 0.010) nomtitles nonumbers nonotes nolines noobs label booktabs collabels(none) stats(N r2, fmt(%4.0fc %4.2fc) labels("Observations" "R-squared")) ///
order(mpswc mpswc_ttdm) mgroups("Daily change in 10-year yield", pattern(1 0 0 0 0) ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
varlabels(, elist(`z4' \midrule))
* ------------------------------------------------------------------------------


* ==============================================================================
* Appendix
* ==============================================================================

* ------------------------------------------------------------------------------
* Table E1. Assessment of Assumptions in Classical Measurement Error Models
use data.dta, replace

* Sigmas and lambdas
quiet summ melhsfx if idx11 & regular
scalar sigma_v = r(sd)		// in text
quiet summ merhs if idx11 & regular
scalar sigma_u = r(sd)
quiet summ mpswc if idx11 & regular
scalar s_xstar = r(sd)		// in table notes
quiet summ mpswc_ttdm if idx11 & regular
scalar s_x = r(sd)
scalar lambda1 = (s_xstar^2)/(sigma_u^2+s_xstar^2)
scalar lambda2 = (s_xstar^2)/(s_x^2)

* Rho's and p's
quiet pwcorr melhsfx mpswc if idx11 & regular, sig
scalar rho_vxstar = r(C)[2,1]
scalar p_vxstar = r(sig)[2,1]
quiet pwcorr melhsfx usdmxn if idx11 & regular, sig
scalar rho_vystar = r(C)[2,1]
scalar p_vystar = r(sig)[2,1]
quiet pwcorr merhs mpswc if idx11 & regular, sig
scalar rho_uxstar = r(C)[2,1]
scalar p_uxstar = r(sig)[2,1]
quiet pwcorr merhs usdmxn if idx11 & regular, sig
scalar rho_uystar = r(C)[2,1]
scalar p_uystar = r(sig)[2,1]

matrix A = (sigma_u,. \ rho_vxstar,p_vxstar \ rho_vystar,p_vystar \ sigma_v,. \ rho_uxstar,p_uxstar \ rho_uystar,p_uystar \ lambda1,.)
matrix list A,format(%4.3f)
di lambda2

* Summary
summ merhs melhsfx mpswc mpswc_ttdm if idx11 & regular
pwcorr melhsfx merhs mpswc usdmxn if idx11 & regular, sig
* ------------------------------------------------------------------------------


* ------------------------------------------------------------------------------
* Table E2. Correlation of Potential OV with PRS
use data.dta, clear
local tbllbl "f_prsov"

label variable mpswc "Intraday PRS"
label variable mpswc_ttdm "Daily PRS"

local depvars "dvix dh15t2y levelwti iclmsurp"

eststo clear
local j = 0
foreach depvar of varlist `depvars' {
	local ++j
	eststo mdl`j': quiet regress `depvar' mpswc if regular & idx11, robust
	local ++j
	eststo mdl`j': quiet regress `depvar' mpswc_ttdm if regular & idx11, robust
}
esttab mdl* using "$pathtbls/`tbllbl'.tex", replace fragment cells(b(fmt(a2) star) se(fmt(%4.2fc) par)) ///
starl( * 0.10 ** 0.05 *** 0.010) order(mpswc mpswc_ttdm) nomtitles nonumbers nonotes nolines noobs label booktabs collabels(none) stats(N r2, fmt(%4.0fc %4.2fc) labels("Observations" "R-squared")) ///
mgroups("\(\Delta\)VIX" "\(\Delta\) 2Y yield" "WTI price" "IJC surprise", pattern(1 0 1 0 1 0 1 0) ///
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
varlabels(_cons Constant, elist(_cons \midrule))
* ------------------------------------------------------------------------------


* ==============================================================================
* References in Manuscript
* ==============================================================================

* ------------------------------------------------------------------------------
* Tables 2 to 4 including Mar & Apr 2020 (footnote 35 in appendix B)
use data.dta, replace
foreach depvar of varlist usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr {
	regress `depvar' mpswc if idx11, robust nocons
	regress `depvar' mpswc expcttg if idx11, robust nocons
}
foreach depvar of varlist usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr {
	regress `depvar'_ttdm mpswc_ttdm if idx11 & !inlist(date,td(27apr2004)) & `depvar' != ., robust nocons
	regress `depvar'_ttdm mpswc_ttdm if idx04 & !inlist(date,td(27apr2004)), robust nocons
}
foreach depvar of varlist usdmxn usdmxn_ttdm {
	foreach indvar of varlist mpswc mpswc_ttdm {
		regress `depvar' `indvar' if idx11, robust nocons
	}
}
* ------------------------------------------------------------------------------

* ------------------------------------------------------------------------------
* Comments in text and footnotes
use data.dta, replace

* Number of regularly scheduled announcements (last paragraph in section 2.1)
summ date if date >= td(1jan2004) & regular
summ date if date >= td(1jan2011) & regular

* Correlation between swap-based and suvey-based PRS (second paragraph in section 2.2)
corr svyshkav mpswc mpswc_ttdm if idx11 & regular

* Correlation between daily changes in 1M and 3M swaps (second paragraph in section 2.2)
corr mpsw28t_ttdm mpswc_ttdm if idx11 & regular

* Effect on the term spread (footnote 16 in section 3.2.1)
regress trmsprd10_2 mpswc if idx11 & regular, robust nocons
regress trmsprd10_2 mpswc expcttg if idx11 & regular, robust nocons

* Response to raw changes in the policy rate (footnote 19 in section 3.2.1)
foreach depvar of varlist usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr {
	regress `depvar' mxonbr if idx11 & regular, robust nocons
}

* Effects on FX and YC of wide and suvey-based PRS (robustness checks in section 3.2.1)
foreach depvar of varlist usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr {
	regress `depvar' mpswc_ttwd if idx11 & regular, robust nocons
	regress `depvar' svyshkav if idx11 & regular, robust nocons
}

* ME is larger in dependent variable (footnote 21 in section 4.1)
regress mpswc_ttdm mpswc if idx11 & regular, robust
regress usdmxn_ttdm usdmxn if idx11 & regular, robust

* Confirm small attenuation bias in YC (footnotes 24 in section 4.1 and 43 in appendix E)
foreach depvar of varlist gmxn02yr gmxn05yr gmxn10yr gmxn30yr {
	regress `depvar' mpswc if idx11 & regular, robust nocons
	regress `depvar' mpswc_ttdm if idx11 & regular, robust nocons
}

* ME in FX is not related to PRS (last sentence of second paragraph in section 4.2)
reg melhsfx mpswc, r
reg melhsfx mpswc_ttdm, r

* Dates with Banxico announcement and US jobs release (sentence before footnote 25 in section 4.3)
summ date melhsfx nfpaysurp unempsurp iclmsurp usdmxn_ttdm mpswc_ttdm if usjobsday == 1 & date >= td(1jan2011) & date < td(1jan2015)
summ date melhsfx nfpaysurp unempsurp iclmsurp usdmxn_ttdm mpswc_ttdm if usjobsday == 1 & date >= td(1jan2015)

* Effect of IJC vs NFP vs UNE (sentence after footnote 28 in section 4.3)
reg usdmxn_ttdm mpswc_ttdm dvix dh15t2y levelwti iclmsurp if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dvix dh15t2y levelwti nfpaysurp if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dvix dh15t2y levelwti unempsurp if regular & idx11, r nocons

* Effect of each potential omitted variable (auxiliary to table 5 in section 4.3)
reg usdmxn_ttdm mpswc_ttdm 			if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm iclmsurp if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm levelwti if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dvix 	if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dh15t10y if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dusgg10yr if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dh15t2y 	if regular & idx11, r nocons
reg usdmxn_ttdm mpswc_ttdm dusgg2yr if regular & idx11, r nocons

* Suggested OVs explain some variability in both ME (footnote 31 in section 4.3)
reg melhsfx dvix dh15t2y levelwti iclmsurp if regular & idx11, r
reg merhs dvix dh15t2y levelwti iclmsurp if regular & idx11, r

* Correlation of PRS computed from TIIE, swaps and surveys (last paragraph in appendix C)
corr mxibtiiecmp mpswc mpswc_ttdm mpsw28t_ttdm svyshkav if regular & idx11

* Effects on FX and YC of TIIE-based PRS (last paragraph in appendix C)
foreach depvar of varlist usdmxn gmxn02yr gmxn05yr gmxn10yr gmxn30yr {
	regress `depvar' mxibtiiecmp if idx11 & regular, robust nocons
	regress `depvar'_ttdm mxibtiiecmp if idx11 & regular, robust nocons
}

* Null hypotheses of zero means not rejected (footnote 41 in appendix E)
* 	H0: mean_u = 0 vs. H1: mean_u != 0
*	H0: mean_v = 0 vs. H1: mean_v != 0
ttest merhs == 0
di r(p)
ttest melhsfx == 0
di r(p)

* No correlation b/w MEs (footnote 41 in appendix E)
pwcorr melhsfx merhs if regular, sig

* ME on RHS correlates w/ RHS var measured w/ error -> CEV assumption (footnote 42 in appendix E)
pwcorr merhs mpswc_ttdm if regular, sig

* Std of ME in 10Y yield (footnote 44 in appendix E)
gen melhs10y = gmxn10yr_ttdm - gmxn10yr 
summ melhs10y if idx11 & regular

* ME on LHS does not correlate w/ RHS var measured w/ error -> no endogeneity bias (paragraph after footnote 44 in appendix E)
pwcorr melhsfx mpswc mpswc_ttdm if regular, sig

* Proxy variable is related to the 'true' variable
reg mpswc mpswc_ttdm if regular & idx11, r

* OVB disappears after controlling for melhsfx
reg usdmxn_ttdm mpswc_ttdm melhsfx if regular & idx11, r nocons

* Qualitative effect of OV is similar if data since 2004
reg usdmxn_ttdm mpswc_ttdm if regular & idx04, r nocons
reg usdmxn_ttdm mpswc_ttdm dvix dh15t2y levelwti iclmsurp if regular & idx04, r nocons
* ------------------------------------------------------------------------------
