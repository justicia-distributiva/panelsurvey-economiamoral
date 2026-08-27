

* Poverty treatment: wide12bP.dta
**************************************

* Origin
reg tau_hatPo ingresosL irt_z1 igualpov mayorpov ///
sexoWb edadW media suptec supun estlabB 

shapley2, stat(r2) group(ingresosL,irt_z1, ///
igualpov mayorpov,sexoWb, edadW, media suptec supun, estlabB)


* Inequality treatment: wide12bD.dta
**************************************

* Origin
reg tau_hatDo ingresosL irt_z1 igualpov mayorpov ///
sexoWb edadW media suptec supun estlabB 

shapley2, stat(r2) group(ingresosL,irt_z1, ///
igualpov mayorpov,sexoWb, edadW, media suptec supun, estlabB)

* Work effort
reg tau_hatDw ingresosL irt_z1 igualpov mayorpov ///
sexoWb edadW media suptec supun estlabB 

shapley2, stat(r2) group(ingresosL,irt_z1, ///
igualpov mayorpov,sexoWb, edadW, media suptec supun, estlabB)
