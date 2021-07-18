global DATA = "D:\Documentos\Maestría\UdeSA\4. Segunda parte\2. Herramientas computacionales\4. Data visualization\Tareas\Tarea 2\data"

cd "$DATA"
ssc install spmap
ssc install shp2dta
net install spwmatrix, from(http://fmwww.bc.edu/RePEc/bocode/s)


shp2dta using london_sport.shp, database(ls) coord(coord_ls) genc(c) genid(id) replace
use ls, clear
describe

use coord_ls, clear
describe

import delimited "mps-recordedcrime-borough.csv", clear 
rename borough name
keep if crimetype == "Theft & Handling"
collapse (sum) crimecount, by(name)
save "crime2.dta", replace
use ls, clear
merge 1:1 name using crime2.dta
drop _m
save london_crime_shp2.dta, replace
*Generamos la variables de crímenes cada 1000 habitantes y le ponemos label
gen Crimesper1000= (crimecount/Pop_2001)*1000
label var Crimesper1000 "Crímenes cada 1000 habitantes"

* Mapa de cuantiles
spmap Crimesper1000 using coord_ls, id(id) clmethod(q) cln(4) title("Número de crímenes cada 1000 habitantes") legend(size(medium) position(5) xoffset(15.05)) fcolor(YlOrRd) plotregion(margin(b+15)) name(g2,replace) note("Este mapa nos muestra el nivel de crímenes para los diferentes distritos de Londres")

*Mapa de desvíos
spmap crimecount using coord_ls, id(id) clmethod(s) title("Número de crímenes cada 1000 habitantes") legend(size(medium) position(5) xoffset(14.05)) fcolor(YlOrRd) plotregion(margin(b+15)) ndfcolor(gray)note("Este mapa nos muestra el nivel de crímenes para los diferentes distritos de Londres") 

*Mapa de intervalos
spmap crimecount using coord_ls, id(id) clmethod(e) cln(6) title("Número de crímenes cada 1000 habitantes") legend(size(medium) position(5) xoffset(17.05)) fcolor(OrRd) plotregion(margin(b+15)) ndfcolor(gray) note("Este mapa nos muestra el nivel de crímenes para los diferentes distritos de Londres")           
 
                 


