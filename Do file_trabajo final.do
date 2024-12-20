
global trabajo "/Users/gabriel/Documents/ARCHIVOS CATO/2023-1/Fundamentos de econometría/Trabajo final"

************ PARTE 1 ******************

use "$trabajo/enaho01-2022-200.dta", clear
keep conglome vivienda hogar codperso ubigeo dominio estrato p203 p207 p208a p209 facpob07 
merge 1:1 conglome vivienda hogar codperso using "$trabajo/enaho01a-2022-300.dta" ,  keepus(p300a p301a p301b p301c p301d p301a0)
drop if _merge==1
drop _merge
merge 1:1 conglome vivienda hogar codperso using "$trabajo/enaho01a-2022-500.dta", keepus(p513t i524e1 i518 i513t i530a i538e1 i541a ocu500 ocupinf)
drop if _merge==1
drop _merge
tab p301a

recode p301a (1/4=0) (5/6=6) (7/10=11) (11=16) (.=.), gen(año_educ)
egen suma = rowtotal(p301b p301c)
gen educ = suma + año_educ

*Como son trabajadores dependientes, solo utilizamos algunas variables
gen horas_totales = p513*48
replace horas_totales=. if horas_totales == 0

gen Salario = i524e1/horas_totales
replace Salario=. if Salario == 0 

*Generamos variable dependiente
gen lsalario = ln(Salario)

*Generamos variable edad
drop if p208a < 24
drop if p208a > 65
rename p208a edad
gen edad2 = edad^2

*Generamos dummy de sexo, tomando como base a dummy de hombre
gen sexo = 0 if p207 == 1
replace sexo = 1 if p207 == 2
label define lsexo 0 "hombre" 1 "mujer"
label values sexo lsexo

*Generamos dummy interactiva de edad y sexo
gen sxeduc = sexo*educ 

*Regresionamos para observar primero resultados
reg lsalario educ edad edad2 sexo sxeduc 

* En primera instancia observamos que la variable sexo es la mas significativa, ///
* dado que tomamos como base la dummy de sexo cuando es hombre, observamos que ///
* cuando se es mujer la remuneracion es mas baja.
keep conglome vivienda hogar codperso ubigeo dominio estrato p203 p209 p300a ocu500 ocupinf Salario lsalario edad edad2 sexo educ sxeduc facpob07
save "$trabajo/Datos1", replace

************ PARTE 2 ******************

* Ahora procederemos a incluir otras posibles variables de interes, entre ellas ///
* la de region de procedencia, categoria ocupacional, lengua materna, entre otros
use "$trabajo/enaho01-2022-200.dta", clear
keep conglome vivienda hogar codperso ubigeo dominio estrato p203 p207 p208a p209 facpob07 
merge 1:1 conglome vivienda hogar codperso using "$trabajo/enaho01a-2022-300.dta" , keepus(p300a p301a p301b p301c p301d p301a0)
drop if _merge==1
drop _merge
merge 1:1 conglome vivienda hogar codperso using "$trabajo/enaho01a-2022-500.dta"
tab dominio /// region de procedencia 
tab p507 /// categoria ocupacional
tab p516r4 /// actividad de la empresa
tab p517d1 /// tamaño de la empresa
tab p209 /// estado civil
tab p300a /// lengua materna

***
drop if p507 == 1 /// eliminamos estas categorias dado que nos enfocamos en trabajadores dependientes
drop if p507 == 2

***** CATEGORIA OCUPACIONAL *****
* Definimo primero las categorias existentes, luego las agruparemos para formar dummy
gen cat_ocup = 1 if p507 == 3
replace cat_ocup = 2 if p507 == 4
replace cat_ocup = 3 if p507 == 5
replace cat_ocup = 4 if p507 == 6
replace cat_ocup = 5 if p507 == 7
label define locup 1 "empleado" 2 "obrero" 3 "familiar no remunerado" 4 "trabajador del hogar" 5 "otro"
label values cat_ocup locup
label var cat_ocup "Categoria ocupacional"

*Formamos dummy de categoria ocupacional
recode cat_ocup (1/2=0 "Empleado u obrero") (3/5=1 "Trabajador(a) del hogar u otro"), gen(ocupacion)

***** ACTIVIDAD DE LA EMPRESA *****
codebook p516r4
tab p516r4, nol
tab p516r4
*tab etiquetas, nol

recode p516r4 (111=1)(112=1)(113=1)(114=1)(116=1)(119=1)(121=1)(122=1)(123=1)(124=1)(125=1)(126=1)(127=1)(128=1)(129=1)(130=1)(141=1)(142=1)(143=1)(144=1)(145=1)(146=1)(149=1)(150=1)(161=1)(170=1)(220=1)(230=1)(311=2)(312=2)(322=2)(729=3)(810=3)(899=3)(1010=4)(1050=4)(1061=4)(1071=4)(1073=4)(1079=4)(1102=4)(1104=4)(1311=4)(1313=4)(1392=4)(1393=4)(1394=4)(1410=4)(1430=4)(1512=4)(1520=4)(1622=4)(1629=4)(1811=4)(2011=4)(2100=4)(2310=4)(2392=4)(2393=4)(2395=4)(2396=4)(2511=4)(2592=4)(2593=4)(2599=4)(3100=4)(3211=4)(3212=4)(3240=4)(3250=4)(3290=4)(3311=5)(3312=5)(3319=5)(3510=5)(3600=5)(3811=5)(4100=6)(4210=6)(4220=6)(4321=6)(4322=6)(4330=6)(4390=6)(4520=7)(4540=7)(4620=7)(4630=7)(4641=7)(4649=7)(4651=7)(4659=7)(4661=7)(4663=7)(4669=7)(4721=7)(4722=7)(4730=7)(4741=7)(4752=7)(4759=7)(4761=7)(4764=7)(4771=7)(4772=7)(4773=7)(4774=7)(4781=7)(4782=7)(4789=7)(4791=7)(4799=7)(4921=8)(4922=8)(4923=8)(5021=8)(5221=8)(5224=8)(5229=8)(5310=9)(5320=9)(5510=9)(5610=9)(5621=9)(5629=9)(5630=9)(5813=9)(5911=9)(6010=9)(6110=9)(6120=9)(6130=9)(6190=9)(6202=9)(6311=9)(6312=9)(else=10), gen(actividad)

* Observamos actividad economica desagregada, con finalidad de simplificar el analisis, se agrupara posteriormente algunas actividades para crear solo 3 categorias
label define etiquetas2 1 "Agropecuaria" 2 "Pesca" 3 "Extractiva" 4 "Manufactura" 5 "Mantenimiento" 6 "Construcción" 7 "Comercio" 8 "Transporte" 9 "Telecomunicaciones"10 "Servicios y diveros"
label values actividad etiquetas2
tab actividad

* Definimos dummy de tres categorias de actividad económica
recode actividad (1/3=1) (4/9=2) (10/10=3), gen(Actividad)
label define etiquetas3 1 "Agropecuario,pesca y extractivo" 2 "Manufactura, construcción,comercio,transporte y telecomunicaciones" 3 "Servicios y diversos"
label value Actividad etiquetas3

* Generamos dummy de actividad economica de la empresa 
gen actividad1 = (Actividad == 1) // dummy de agro pesca y extractivo
gen actividad2 = (Actividad == 2) // dummy de manu,cons,com, trans y tele
gen actividad3 = (Actividad == 3) // dummy de servicios y otros
label var actividad1 "Actividad economica 1 (agro,pesca y extrac)"
label var actividad2 "Actividad economica 2 (manu,cons,com,trans y tele)"
label var actividad3 "Actividad economica 3 (servs, otros)"

***** TAMAÑO DE LA EMPRESA *****
recode p512a (1/3=1) (4/5=0), gen(tamaño)
label define etiquetas4 1 "Pequeña y Mediana empresa" 0 "Gran empresa"
label value tamaño etiquetas4

***** LENGUA MATERNA *****
recode p300a (1/3=0) (8/14=0) (4/4=1) (5/7=.), gen(lengmat)
drop if lengmat == 15 /// eliminamos valores perdidos, y consideramos solo castellno y lengua nativa
label define etiquetas8 0 "Castellano" 1 "Lengua Nativa"
label value lengmat etiquetas8

***** ESTADO CIVIL *******
recode p209 (1/2=1 "Conviviente o Casado") (3/5=2 "Otro") (6/6=3 "Soltero"), gen(estado_civil)
gen est1_casado = (estado_civil == 1)
gen est1_soltero = (estado_civil == 3)
gen est1_otro = (estado_civil == 2)

keep conglome vivienda hogar codperso ubigeo estrato p207 p203 p208a p300a lengmat p512a tamaño p507 ocupacion dominio procedencia1 p1_* procedencia2 p2_* p516r4 Actividad activi* p209 estado_civil est1_* facpob07

save "$trabajo/Datos2", replace

* Finalmente mantenemos todas las nuevas variables creadas y hacemos un merge con la regresion inicial

merge 1:1 conglome vivienda hogar codperso using "$trabajo/Datos1", keep(matched)
keep conglome vivienda hogar codperso ubigeo estrato Salario lsalario sexo edad edad2 educ sxeduc p300a lengmat p512a tamaño p507 ocupacion dominio procedencia1 p1_* procedencia2 p2_* p516r4 Actividad activi* p209 estado_civil est1_* facpob07

* Al incluir todas las variables de interes, mas de la mitad de observaciones se reduce, lo que indicaria que al agregar mas variables exogenas, si bien aumenta la capacidad para explicar el logartimo del salario, la significancia de los regresores disminuye

* Para el analisis de las regresiones ya nos enfocamos en la area de estudio: Huanuco
keep if (_n >= 9835 & _n <= 10266 )
drop procedencia* p1_* p2_* 
drop dominio

save "$trabajo/Base de datos", replace

twoway histogram lsalario || normal lsalario , fcolor(ltbluishgray) lcolor(red) title("Logaritmo del Salario") ytitle("Porcentaje") 
save grafico0, replace	

*observamos que los primero dos modelos tendran mas oservaciones, entonces eliminaremos la diferencia para que haya mejor estimacion
predict ehat, resid
drop if ehat==. 
drop ehat

************ PARTE 3 ******************

*Estimando discrimnacion por sexo en el mercado laboral

*Mopdelo base
reg lsalario educ edad edad2 sexo sxeduc
outreg2 using "$trabajo/tabla2.xls", excel append ctitle("Modelo base")
predict ehat, resid
hist ehat, normal title("Distribución residual Modelo base") 
save grafico1, replace
vif
estat hettest, rhs
estat imtest, white


*Modelo 1: inclusion de actividad economica. Tomamos como base a actividad2
gen sxact1 = sexo*actividad1 
gen sxact3 = sexo*actividad3 
reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3
outreg2 using "$trabajo/tabla2.xls", excel append ctitle("Modelo 1")
vif
estat hettest, rhs
estat imtest, white

*Modelo 2: inclusion de tamaño de la empresa. Tomamos como base a Gran empresa
gen sxtam = sexo*tamaño
reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3 sxtam 
outreg2 using "$trabajo/tabla2.xls", excel append ctitle("Modelo 2")
vif
estat hettest, rhs
estat imtest, white
// parece ser una variable relevante

*Modelo 3: inclusión de lengua materna. Tomamos como base a Castellano
gen sxleng = sexo*lengmat
reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3 sxtam sxleng
outreg2 using "$trabajo/tabla2.xls", excel append ctitle("Modelo 3")
vif
estat hettest, rhs
estat imtest, white
// definitamente lengua materna no aporta a la estimacion (R igual y R cuadrado disminuye)

*Modelo 4: incluion de categoria ocupacional. Tomamos como base a empleado
gen sxocup = sexo*ocupacion
reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3 sxtam sxleng sxocup
outreg2 using "$trabajo/tabla2.xls", excel append ctitle("Modelo 4")
vif
estat hettest, rhs
estat imtest, white
// probablemente sea algo significativo, pero muchas variables disminuye su peso

*Modelo 5: inclusion de estado civil. Tomamos como base a "otros"
gen sxmarry = sexo*est1_casado
gen sxsing = sexo*est1_soltero
reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3 sxtam sxleng sxocup sxmarry sxsing
vif
estat hettest, rhs
estat imtest, white

test (educ=edad=edad2=sexo=sxeduc=sxact1=sxact3=sxtam=sxleng=sxocup=sxmarry=sxsing=0)
// definitamente estado civil no aumenta la estimacion, R y R cuadrado no suben nada

*Ahora corregimos toda las anteriore estimaciones y retiramos variables irrelevantes o que brindan informacion redundante
reg lsalario educ edad edad2 sexo sxeduc 
outreg2 using "$trabajo/mejor estimacion.xls", excel append ctitle("Modelo base")

reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3 sxtam sxocup
outreg2 using "$trabajo/mejor estimacion.xls", excel append ctitle("Modelo aprox")
vif
estat hettest, rhs // rechazamos homocedasticidad --> hay heterocedasticidad
estat imtest, white // no rechazamos homocedasticidad --> no es concluyente

*Solucionando no rechazo de homocedasticidad
reg lsalario educ edad edad2 sexo sxeduc sxact1 sxact3 sxtam sxocup, vce(robust)	

*Observamos que con aun con los mejores regresores, las variables mas ///
*relevantes son educ y sxtam, hacemos test de multicolinealidad observando F ///
*en la tabla, pero parece no haber multicolinealidad. Probablemente, para el ///
*caso de Huanuco, ni el hecho de ser mujer ni la cantidad de años de estudio /// 
*de las mujeres ni el tipo de actividad economica a la que su empleador trabaje ///
*ni la vejez ni la categoria ocupaccional influyen significativamente en su///
*salario. En cambio, si las mujeres trabajan en pequeñas y medianas empresas y ///
*los años de educación en general, independienemente de si sean mujeres, son ///
*los principales determinantes del salario en Huanuco

************ PARTE 4 ******************

* Estimando discriminacion por lengua materna en mercado laboral
* Debemos crear nuevas dummys interactivas para el estudio
gen lxeduc = lengmat*educ          // dummy interactiva educ
gen lxact1 = lengmat*actividad1    // dummy interactiva agro, pesca y extrac 
gen lxact3 = lengmat*actividad2    // dummy interactiva servicios y otros
gen lxtam = lengmat*tamaño         // dummy interactiva tamaño de la empresa
gen lxocup = lengmat*ocupacion     // dummy interactiva categoria ocupacional
gen lxmarry = lengmat*est1_casado  // dummy interactiva casado 
gen lxsing = lengmat*est1_soltero  // dummy interactiva soltero

*Se toma como base a actividad de manufactura, construccion, comercio, transporte y telecomunicaciones, base a gran empresa, base a empleado u obrero y de base a viudo, divorciado u otros.

* Modelo base
reg lsalario educ edad edad2 lengmat lxeduc 
outreg2 using "$trabajo/tabla3.xls", excel append ctitle("Modelo base")
vif
estat hettest, rhs
estat imtest, white

* Modelo 1: inclusion actividad economica
reg lsalario educ edad edad2 lengmat lxeduc lxact1 lxact3
outreg2 using "$trabajo/tabla3.xls", excel append ctitle("Modelo 1")
vif
estat hettest, rhs
estat imtest, white

* Modelo 2: inclusion de tamaño de la empresa
reg lsalario educ edad edad2 lengmat lxeduc lxact1 lxact3 lxtam
outreg2 using "$trabajo/tabla3.xls", excel append ctitle("Modelo 2")
vif
estat hettest, rhs
estat imtest, white

*No se incluye variable de sexo interactiva porque se mantiene misma conclusion///
*anterior de que el hecho de ser mujer con lengua materna nativa no influye///
*significativamente en el salario 

* Inclusion de categoria ocupacional 
reg lsalario educ edad edad2 lengmat lxeduc lxact1 lxact3 lxtam lxocup
*encontramos que en la muestra todos las personas con lengua materna nativa no tienen la ocupacion de trabajador(a) del hogar u otros, por lo que es una variable irrelevante

* Modelo 3: inclusion de estado civil 
reg lsalario educ edad edad2 lengmat lxeduc lxact1 lxact3 lxtam lxmarry lxsing 
outreg2 using "$trabajo/tabla3.xls", excel append ctitle("Modelo 3")
vif
estat hettest, rhs
estat imtest, white

* Mejorando estimacion y retirando variables irrelevantes 
reg lsalario educ edad edad2 lengmat lxeduc 
outreg2 using "$trabajo/mejor estimacion2.xls", excel append ctitle("Modelo base")

reg lsalario educ edad edad2 lengmat lxeduc lxact1 lxact3 lxtam 
outreg2 using "$trabajo/mejor estimacion2.xls", excel append ctitle("Modelo aprox")
vif
estat hettest, rhs // rechazamos homocedasticidad --> hay heterocedasticidad
estat imtest, white // tambien rechazamos homocedasticidad --> hay heterocedasticidad

*Reafirmando heterocedasitidad 
reg lsalario educ edad edad2 lengmat lxeduc lxact1 lxact3 lxtam, vce(robust)

*En esta estimacion para analizar la discriminacion por lengua materna, ///
*encontramos que los años de educacion, cuando se tiene idioma nativo y se ///
*trabaja en pequeñas y medianas empresas; y, cuando se tiene idioma nativo ///
*y la actividad economica de la empresa es en servicios u otros, son ///
*variables significativas que influyen en el cambio porcentual del salario.///
*En cambio, la tendencia a la desactualizacion (edad2); y, ni la lengua nativa ///
*por si misma, ni los con lengua nativa y con mayor educacion ni el hecho de ///
*tener lengua nativa y trabajan en el sector agropecuario, pesca y/o ///
*extractivo, son variables determianantes del salario. 
label define llxact3 0 "Otro sector" 1 "Servicios"
label values lxact3 llxact3

*Graficos de dispersion, pie y cajas modelo de dicriminacion por sexo
scatter lsalario educ, title("Dispersión de la educación") xtitle("Años de educacion") name(grafi1, replace)

label define lsxtam 0 "Hombre" 1 "Mujer"
label values sxtam lsxtam
graph pie, over(sxtam) plabel(_all percent, color(white) format(%9.1f) size(medsmall)) plotregion(lstyle(none)) title("Trabajadora(a) en pequeña y mediana empresa")///
subtitle("Composición por sexo") caption("Elaboracion Propia") name(grafi2, replace)

graph box lsalario, over(educ) graphregion(color(white)) ylabel(, nogrid)  note("En base a los años de educación") caption("Fuente: ENAHO (2022) Huánuco" "Elaboración Propia") name(grafi3, replace)

graph box lsalario, over(sxtam) graphregion(color(white)) ylabel(, nogrid)  note("En base al sexo") caption("Fuente: ENAHO (2022) Huánuco" "Elaboración Propia")///
name(grafi4, replace)

*Graficos de dispersion, pie y cajas modelo de discriminacion por lengua materna
label define lllxtam 0 "Castellano" 1 "Lengua Nativa"
label values lxtam lllxtam
graph pie, over(lxtam) plabel(_all percent, color(white) format(%9.1f) ///
size(medsmall)) plotregion(lstyle(none)) title("Trabajadora(a) en pequeña y mediana empresa") subtitle("Composición por lengua materna") ///
caption("Elaboracion Propia") name(grafi5, replace)

graph box lsalario, over(lxtam) graphregion(color(white)) ylabel(, nogrid)  note("En base a la lengua materna") caption("Fuente: ENAHO (2022) Huánuco" "Elaboración Propia") name(grafi6, replace)

graph box lsalario, over(lxact3) graphregion(color(white)) ylabel(, nogrid)  note("En base al sector económico") caption("Fuente: ENAHO (2022) Huánuco" "Elaboración Propia") name(grafi7, replace)

graph pie, over(lxact3) plabel(_all percent, color(white) format(%9.1f) ///
size(medsmall)) plotregion(lstyle(none)) title("Trabajadora(a) en Sector Servicios")subtitle("Composición por lengua materna") ///
caption("Elaboracion Propia") name(grafi8, replace)

graph combine grafi1 grafi3 
graph combine grafi4 grafi6 grafi7, title("Gráfico de cajas según sexo, lengua materna" "y actividad econónica") c(3)

* Realizando tablas bidimensioanles
table sxtam, c(mean lsalario median lsalario sd lsalario min lsalario max lsalario) format(%6.2f) col

table lxtam, c(mean lsalario median lsalario sd lsalario min lsalario max lsalario) format(%6.2f) col

table lxact3, c(mean lsalario median lsalario sd lsalario min lsalario max lsalario)format(%6.2f) col

