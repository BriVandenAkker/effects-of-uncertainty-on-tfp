
ssc install estout, replace

*Define main instrument set
global iv "l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock"
****************************************************************************************************************************
*TABLE 1: Stock Market Levels and Volatility
****************************************************************************************************************************


*Second Stage
*Col 1: OLS -  Bloom/Baker (Directly from Bloom/Baker, just using data of only 28 countries)
areg iydgdp l1lavgvol l1avgret yy*,ab(country) cluster(country)
estimates store A2

*Col 2 : - IV -  Bloom/Baker (Directly from Bloom/Baker, again the only difference is the data)
ivreg2 iydgdp cc* yy* (l1avgret l1lavgvol=$iv), cluster(country) first partial(yy* cc*)
estimates store B2

*Col 3: - IV - TFP
ivreg2 tfp_g cc* yy* (l1avgret l1lavgvol=$iv), cluster(country) first partial (yy* cc*)
estimates store C2

*Col 4: - IV - TFP/ Nat Shock Magnitudes as interaction
ivreg2 tfp_g cc* yy* (l1avgret l1lavgvol= c.l1savgnatshock##c.disindexla l1savgpolshock l1savgrevshock l1savgtershock), cluster(country) first partial (yy* cc*)
estimates store D2

*Col 5 - IV - Investment
ivreg2 k_g cc* yy* (l1avgret l1lavgvol=$iv), cluster(country) first partial (yy* cc*)
estimates store E2

*Col 6 -IV- Investment/ Nat Shock Magnitudes
ivreg2 k_g cc* yy* (l1avgret l1lavgvol= c.l1savgnatshock##c.disindexla l1savgpolshock l1savgrevshock l1savgtershock), cluster(country) first partial (yy* cc*)
estimates store F2

esttab A2 B2 C2 D2 E2 F2,b(a2) se compress noconstant noobs label title("Table 1: Two-Stage Least Squares")


*First Stage Levels: Stocks

*No Nat Shock Magnitudes - Bloom Baker
reg l1avgret l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store A1


*Nat Shock Magnitudes
reg l1avgret c.l1savgnatshock##c.disindexla l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store B1


*First Stage Volatility: Stocks
*No Nat Shocks Magitudes
reg l1lavgvol l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store C1

*Nat Shocks Magnitudes
reg l1lavgvol c.l1savgnatshock##c.disindexla l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store D1

esttab A1 A1 B1 A1 B1, se compress noconstant noobs label title("Table 1: First Stage - Level")
esttab C1 C1 D1 C1 D1, se compress noconstant noobs label title("Table 1: First Stage - Volatility")


******************************************************************************************************************************
*Table 2: Sovereign Bond Yields and Volatility
******************************************************************************************************************************

*Second Stage
*Col 1: OLS -  Bloom/Baker
areg iydgdp l1avgbondyield l1lavgbondvoly yy*,ab(country) cluster(country)
estimates store A20

*Col 2 : - IV -  Bloom/Baker
ivreg2 iydgdp cc* yy* (l1avgbondyield l1lavgbondvoly=$iv), cluster(country) first partial(yy* cc*)
estimates store B20

*Col 3: - IV - TFP
ivreg2 tfp_g cc* yy* (l1avgbondyield l1lavgbondvoly=$iv), cluster(country) first partial (yy* cc*)
estimates store C20

*Col 4: - IV - TFP/ Nat Shock Magnitudes
ivreg2 tfp_g cc* yy* (l1avgbondyield l1lavgbondvoly= c.l1savgnatshock##c.disindexla l1savgpolshock l1savgrevshock l1savgtershock), cluster(country) first partial (yy* cc*)
estimates store D20

*Col 5 - IV - Investment
ivreg2 k_g cc* yy* (l1avgbondyield l1lavgbondvoly=$iv), cluster(country) first partial (yy* cc*)
estimates store E20

*Col 6 -IV- Investment/ Nat Shock Magnitudes
ivreg2 k_g cc* yy* (l1avgbondyield l1lavgbondvoly= c.l1savgnatshock##c.disindexla l1savgpolshock l1savgrevshock l1savgtershock), cluster(country) first partial (yy* cc*)
estimates store F20

esttab A20 B20 C20 D20 E20 F20, se compress noconstant noobs label title("Table 2: Two-Stage Least Squares")

*First Stage Levels: Bonds
*No Nat Shock Magnitudes - Bloom BAker
reg l1avgbondyield l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store A10


*Nat Shock Magnitudes
reg l1avgbondyield c.disindexla##c.l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store B10


*First Stage Volatility: Bonds
*No Nat Shocks Magitudes
reg l1lavgbondvoly l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store C10

*Nat Shocks Magnitudes
reg l1lavgbondvoly c.disindexla##c.l1savgnatshock l1savgpolshock l1savgrevshock l1savgtershock yy*, ab(country) cluster(country)
estimates store D10

esttab A10 A10 B10 A10 B10, se compress noconstant noobs label title("Table 2: First Stage - Level")
esttab C10 C10 D10 C10 D10, se compress noconstant noobs label title("Table 2: First Stage - Volatility")





