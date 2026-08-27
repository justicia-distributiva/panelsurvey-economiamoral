***********************************
* Fecha: Febrero 2020             *
* Topico: generacion ponderadores *
***********************************

* Sexo: 1=hombre, 2=mujer
*****************
* Poblacion
	* Hombre: 0.4827558 
	* Mujer: 0.5172442 

tab sexoW,m
gen sexoWM=0
replace sexoWM=1 if sexoW==2

gen sexoWH=0
replace sexoWH=1 if sexoW==1

tab sexoWM,m	
tab sexoWH,m		
	
* Edad
************
	* 1: 18-24
	* 2: 25-34
	* 3: 35-44
	* 4: 45-54
	* 5: 55 o más
* Poblacion:
	* 1: 0.1684972
	* 2: 0.1828651 
	* 3: 0.1797126 
	* 4: 0.1761730 
	* 5: 0.2927521  	
	
tab edadW,m
tabulate edadW, gen(edadWb)

tab1 edadWb*,m


* Educacion
************
	* 1: no estudio
	* 2: basica incompleta
	* 3: basica completa
	* 4: media incompleta
	* 5: media completa
	* 6: superior no universitaria incompleta
	* 7: superior no universitaria completa
	* 8: universitaria incompleta
	* 9: universitaria completa
	* 10: Postgrado
	
	* 1: basica o menos:0.2251607 
	* 2: media:0.4098993
	* 3: superior no universitaria:0.1564336 
	* 4: superior o mas:0.2085065  

tab edcepW,m	
gen educ=.
replace educ=1 if edcepW < 4 // basica o menos
replace educ=2 if edcepW > 3 & edcepW < 6 // media	
replace educ=3 if edcepW > 5 & edcepW < 8 // superior no universitaria
replace educ=4 if edcepW > 7 // superior universitaria		

tab educ,m
tab edcepW educ,m	
tabulate educ, gen(educb)	
		
tab1 educb*,m	
	

* Ingreso	
*************		
tab ingresosLb,m
tabulate ingresosLb, gen(inLb)

tab1 inLb*,m	

	
* Generacion de weights sin ingreso
************************************
* Note que se omite una categoria por colinealidad

ebalance sexoWH	///
edadWb2 edadWb3 edadWb4 edadWb5 ///
educb2 educb3 educb4, ///
manualtargets(0.48 0.18 0.18 0.18 0.29 0.41 0.16 0.21)	

* Sexo
tab sexoW
tab sexoW [aw=_webal]

* Edad
tab edadW
tab edadW [aw=_webal]

* Educ
tab educ
tab educ [aw=_webal]
	
rename _webal weight

* Eliminar variables
drop sexoW edadW edcepW sexoWM sexoWH educ ///
edadWb1 edadWb2 edadWb3 edadWb4 edadWb5 ///
educb1 educb2 educb3 educb4
	
* Generacion de weights con ingreso. sin educ
************************************************
* Note que se omite una categoria por colinealidad

ebalance sexoWH	///
edadWb2 edadWb3 edadWb4 edadWb5 ///
inLb2 inLb3 inLb4, ///
manualtargets(0.48 0.18 0.18 0.18 0.29 0.35 0.14 0.07)	

* Sexo
tab sexoW
tab sexoW [aw=_webal]

* Edad
tab edadW
tab edadW [aw=_webal]

* Ingresos
tab ingresosLb
tab ingresosLb [aw=_webal]	

rename _webal weightI

* Eliminar variables	
keep ID2 weightI
	
	
	
	
	
	
	
	
	
