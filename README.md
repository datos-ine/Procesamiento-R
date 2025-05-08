**Introducción**
Los años de vida ajustados por discapacidad (AVAD) son un indicador que se utiliza para medir la carga de enfermedad mediante la cuantificación de la pérdida de buena salud.
Su cálculo se basa en la suma de sus dos componentes: los años de vida perdidos por muerte prematura (AVP) y los años de vida vividos con discapacidad (AVD). De esta manera, cada AVAD puede interpretarse como un año perdido de salud plena (* ).
La potencia de utilizar este indicador radica en la obtención de una mejor caracterización del perfil de enfermedades de una población, integrando no sólo la cantidad de fallecimientos a causa de las mismas sino el impacto de las muertes prematuras y la vivencia con algún grado de discapacidad asociada a ellas. La incorporación de estados no fatales en este indicador, a su vez, permite la comparación entre enfermedades con alta letalidad y bajo grado de discapacidad con aquellas que no causan fallecimientos pero sí se asocian a altos grados de discapacidad.
El desarrollo y utilización de este tipo de indicadores resulta de suma importancia para el estudio de enfermedades crónicas no transmisibles (ECNT) como la Diabetes Mellitus (DM), que pueden asociarse al desarrollo de complicaciones no necesariamente causales de muerte, pero que sí suponen la vivencia de diversos estados morbosos y la consecuente pérdida de buena salud en las personas que las padecen. 
Indagar acerca de la magnitud y evolución de indicadores de salud vinculados a ENCT permite fortalecer el conocimiento del comportamiento de estas enfermedades en la población y precisar la necesidad de desarrollar nuevas investigaciones e implementar políticas públicas que colaboren en el mejoramiento de la calidad de vida de las personas y su buena salud.
Con el objetivo de caracterizar la tendencia de carga de enfermedad por DM en nuestro país, se obtendrán los AVAD por DM por jurisdicción, sexo y grupo quinquenal de edad para los años 2005, 2009, 2013 y 2018.
Se analizará el comportamiento de este indicador en el tiempo, en función de las variables mencionadas, y el aporte de cada componente (AVP y AVD) en el mismo.
 
**AVAD = AVP + AVD**
Para el cálculo de los AVAD, se sumaron los datos de los AVP e AVD correspondientes, como se detalla a continuación.

*AVP = defunciones * esperanza de vida*

Los AVP para un evento se calculan como el producto entre cantidad de defunciones y esperanza de vida.

*Fuentes de datos utilizadas*
- Esperanza de vida
La función de pérdida se basó en la tabla de vida por sexo para Argentina correspondiente al año 2019 calculada por el Observatorio de Salud Global (Global Health Observatory, GHO) de la OMS.
Se eligió una tabla de vida para Argentina y no la sugerida por la OMS para la estimación de la carga de enfermedad porque se priorizó un análisis que refleje la situación local y su evolución en el tiempo.
Se utilizó la tabla de vida del 2019 por ser la última publicada por el GHO para Argentina y, a su vez, la correspondiente a cada sexo por los diferenciales de los AVAD para DM por sexo.

GHO | By category | Life tables by country—Argentina. (s. f.). WHO; World Health Organization. Recuperado 10 de marzo de 2025, de https://apps.who.int/gho/data/view.main.60050?lang=en

La base cruda fue bajada y guardada en “Bases de datos” (argentina_tabla de vida_GHO).
Se realizó un recorte con los datos de 2019 y del indicador “esperanza de vida”. Se calculó la esperanza de vida para el grupo “80 años y más” para homologar los grupos de edad con los definidos en las bases de defunciones. Para este cálculo, considerando que:

〖esperanza de vida〗_(grupo de edad X)  =  〖años-persona vividos〗_(grupo de edad X)/〖cantidad de personas vivas〗_(grupo de edad X) 

Para obtener la esperanza de vida para el grupo “80 años y más” se sumaron los valores del numerador y denominador de los grupos “80-84 años” y “85 años y más”.

Archivo: arg2019_espvida

- Defunciones por grupos de causas de muerte, jurisdicción y grupos quinquenales de edad, años 2005-2022
Las defunciones se obtuvieron a partir de las bases de datos de mortalidad publicadas por la Dirección de Estadísticas e Información de Salud (DEIS), considerando como causa básica de muerte los códigos E10 a E14 de la Décima Revisión de la Clasificación Estadística Internacional de Enfermedades y Problemas Relacionados con la Salud (CIE-10). 
Dirección de Estadísticas e Información en Salud - Ministerio de Salud de la Nación. (s. f.). Defunciones. Argentina.gob.ar. Recuperado el 23 de abril de 2025, de https://www.argentina.gob.ar/salud/deis/datos/defunciones

Las bases crudas fueron bajadas y guardadas en “Bases de datos/Defunciones” (una base por año, con datos por sexo, grupo de edad y código de causa de muerte).

Se armó un archivo con la serie del recuento de defunciones por las causas de muerte seleccionadas por:
	Año: 2005, 2006, 2008, 2009, 2010, 2012, 2013, 2014, 2017, 2018 y 2019 (selección de años necesarios para el cálculo del promedio de muertes por trienio).
	Sexo: femenino y masculino. 
	Provincia de residencia: 23 provincias y CABA.
	Grupos de edad: desde 15-19 años hasta 80 años y más

Archivo: serie_def.RData

*Cálculo de AVP*
Para calcular los AVP de DM, se tomaron las fuentes de datos previas y se obtuvo el indicador AVP para las siguientes categorías:
	Grupos quinquenales de edad: de “15-19” a “80 y más”.
	Sexo: varones; mujeres
	Jurisdicción: CABA, Buenos Aires, Catamarca, Chaco, Chubut, Córdoba, Corrientes, Entre Ríos, Formosa, Jujuy, La Pampa, La Rioja, Mendoza, Misiones, Neuquén, Río Negro, Salta, San Juan, San Luis, Santa Cruz, Santa Fe, Santiago del Estero, Tierra del Fuego, Tucumán.
	Año: 2005; 2009; 2013; 2018.
Para cada año de interés, se tomó el promedio de defunciones del trienio correspondiente, es decir, 2004-2006 para 2005, 2008-2010 para 2009, 2012-2014 para 2013 y 2017-2019 para 2018. Para el trienio 2004-2006 las defunciones del 2004 no están disponibles, se calculó el promedio entre 2005-2006.
En función de la combinación de las categorías de las variables contempladas, se cuenta con 2688 grupos posibles (24 provincias * 4 años * 2 sexos * 14 grupos de edad).
Para el caso de AVP, se obtuvo el dato para 2159 grupos. Existen 259 grupos en los que no hay registro de fallecidos, por lo que no se pueden calcular los AVP, se asumen con valor 0.

**AVD = prevalencia del evento * peso de la discapacidad asociada**
Los AVD se calculan como el producto entre la prevalencia del evento  y el peso de la discapacidad asociada al evento. Para calcular los AVD pertenecientes a una enfermedad se deben sumar los AVD para cada complicación asociada a esa enfermedad. 
Por esto, para el cálculo de AVD para diabetes mellitus se determinaron los AVD para retinopatía (proliferativa + no proliferativa), neuropatía periférica, nefropatía y diabetes mellitus tipo 2 sin complicaciones considerando la frecuencia de cada complicación según Wandurranga et al (2014) y el peso de la discapacidad asociada a cada una.
Las frecuencias de las complicaciones y pesos de la discapacidad correspondiente se asumieron de manera indistinta para cada sexo, grupo de edad, jurisdicción y año de estudio. 
Australian Institute of Health and Welfare. (2021). Australian Burden of Disease Study: Methods and supplementary material 2018. Retrieved from https://www.aihw.gov.au/reports/burden-of-disease/abds-methods-supplementary-material-2018 

*Fuentes de datos utilizadas*
- Prevalencia de Diabetes Mellitus
A partir de la información publicada por la Encuesta Nacional de Factores de Riesgo de los años 2005, 2009, 2013, 2018, se obtuvieron los datos de diabetes por autorreporte según:
	Sexo: varones, mujeres
	Edad: simple, luego se agrupa en grupos quinquenales definidos previamente
	Jurisdicción
A partir de estos datos, se calculó la prevalencia de Diabetes Mellitus según autorreporte por jurisdicción, sexo y grupo de edad para cada año en que se realizó la ENFR (2005, 2009, 2013 y 2018).
Como la información de las ENFR presenta datos para personas de 18 años y más, se calculó la prevalencia de DM desde el grupo de edad “15 a 19”, asumiendo para este grupo los valores del grupo de edad “18 a 19”. 

Base de datos usuario ENFR 2018, Base de datos usuario ENFR 2013, Base de datos usuario ENFR 2009, Base de datos usuario ENFR 2005. Disponible en: https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos-2 

- Frecuencia de secuelas asociadas a DM
Se utilizan datos pertenecientes al artículo: Calidad de atención de personas con diabetes tipo 2 en Latinoamérica: ¿hay evidencia de disparidad de género? (2014, EA, Wandurraga et al), por ser el artículo más reciente con información nacional y regional.
El artículo trata sobre un “estudio de corte transversal en el que participaron especialistas pertenecientes a Servicios de Diabetes del sector público y privado de diferentes países de Latinoamérica: 8 de Argentina, 1 de Chile, 3 de Colombia, 2 de Perú y 2 de Venezuela, que enviaron información a la base de datos QUALIDIAB. Los datos fueron originalmente registrados durante la consulta habitual en el período marzo 2012 – marzo 2014, en los formularios semestral y anual del QUALIDIAB, que incluyen indicadores clínicos, metabólicos y terapéuticos, complicaciones micro y macrovasculares, tasa de uso de elementos de diagnóstico y tratamiento y hospitalizaciones. En este estudio se consideraron los registros de pacientes que cumplían con los siguientes criterios de inclusión: pacientes adultos, de ambos sexos con diagnóstico de DMT2 y no menos de 2 años de atención en el servicio de origen. Se analizaron 2495 registros de personas con DMT2 aportados por los países según el detalle siguiente: Argentina 1101, Chile 190, Colombia 347, Perú 543 y Venezuela 314.”
En base a los datos publicados en el artículo se calculó la frecuencia de las complicaciones crónicas para ambos sexos (sumando los datos de varones y mujeres). Se tomaron las 3 complicaciones más frecuentes: neuropatía periférica (30,91%), nefropatía (19,48%) y retinopatía (proliferativa + no proliferativa) (12,46%). Si bien la disfunción eréctil también aparece entre las complicaciones más frecuentes, se la excluyó dado que afecta sólo a pacientes de sexo masculino.
La proporción restante se asumió como “diabetes mellitus tipo 2 sin complicaciones” (37,14%). 
EA, Wandurraga & Villena, Jaime & V, Stepenka & CL, Solís & D, Ramirez & F, Perez & MA, Padrón & H, Manrique & D, Lujan & de Lapertosa, Silvia & Gonzalez, Joaquín Pablo & Fuente, Graciela & Faingold, Maria & Commendatore, Victor & Aschner, Pablo & Gonzalez, Lorena & Elgart, Jorge & Gagliardino, Juan. (2014). Calidad de atención de personas con diabetes tipo 2 en Latinoamérica: ¿hay evidencia de disparidad de género? Revista de la ALAD. 4. 106-115. Disponible en: https://www.researchgate.net/publication/280309159_CALIDAD_DE_ATENCION_DE_PERSONAS_CON_DIABETES_TIPO_2_EN_LATINOAMERICA_HAY_EVIDENCIA_DE_DISPARIDAD_DE_GENERO

Otros artículos con información sobre secuelas:
	Registry of people with diabetes in three Latin American countries: a suitable approach to evaluate the quality of health care provided to people with type 2 diabetes (2013, Commendatore, V. et al).
Utilizan datos de QUALIDIAB provenientes de Argentina, Colombia y Perú. Se analizan 1118 registros.
	Evaluación de la calidad de la asistencia al paciente diabético en América Latina (2001, Gagliardino, J J et al).
“En esta ocasión presentamos el primer análisis descriptivo de los datos correspondientes a 13513 registros de personas con DM1 y DM2 provenientes de las unidades QUALIDIAB de Argentina, Brasil, Chile, Colombia, Paraguay y Uruguay.”
 
- Peso de secuelas asociadas a DM
Teniendo en cuenta el artículo anterior, se buscaron los pesos correspondientes a las complicaciones crónicas más frecuentes de la diabetes mellitus tipo 2:
	Retinopatía (proliferativa + no proliferativa) (Severe vision impairment due to diabetes mellitus type 2 retinopathy): 0,184
	Neuropatía periférica (Diabetic neuropathy due to diabetes mellitus type 2, without diabetic foot or amputation: 0,133
	Nefropatía:  0,309 (corresponde al promedio de las complicaciones crónicas vinculadas a enfermedad renal debidas a diabetes mellitus tipo 2)
	Diabetes mellitus tipo 2 sin complicaciones (Uncomplicated diabetes mellitus type 2): 0,049

Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2019 (GBD 2019) Disability Weights. Seattle, United States of America: Institute for Health Metrics and Evaluation (IHME), 2020. Disponible en: https://ghdx.healthdata.org/record/ihme-data/gbd-2019-disability-weights 

*Cálculo de AVD*
Para calcular los AVD de DM, se tomaron las fuentes de datos previas y se obtuvo el indicador AVD para las siguientes categorías:
	Grupos quinquenales de edad: de “15-19” a “80 y más”.
	Sexo: varones; mujeres
	Jurisdicción: CABA, Buenos Aires, Catamarca, Chaco, Chubut, Córdoba, Corrientes, Entre Ríos, Formosa, Jujuy, La Pampa, La Rioja, Mendoza, Misiones, Neuquén, Río Negro, Salta, San Juan, San Luis, Santa Cruz, Santa Fe, Santiago del Estero, Tierra del Fuego, Tucumán.
	Año: 2005; 2009; 2013; 2018.
En función de la combinación de las categorías de las variables contempladas, se cuenta con 2688 grupos posibles (24 provincias * 4 años * 2 sexos * 14 grupos de edad).
Para el caso de AVD, se obtuvo el dato para 2688 grupos. Existe dato de AVD para todos los grupos posibles.

**Cálculo de AVAD**
Para el cálculo de los AVAD, se sumaron los datos de los AVP e AVD correspondientes. 
Se obtuvieron datos para cada año, jurisdicción, sexo y grupo de edad (para los 2688 grupos posibles). Teniendo en cuenta la disponibilidad de datos de prevalencia de DM, se muestran los datos de los 3 indicadores a partir del grupo de edad de “15 a 19”. 
*AVP pudo calcularse con datos entre 15 y 19 años, AVD para ese grupo se calculó con datos entre 18 y 19 años.
Proporción de cada componente de AVAD

Considerando los cálculos previos de AVAD, AVP y AVD por año, provincia, sexo y grupo de edad, se calculó el aporte de cada componente de los AVAD como la proporción de AVP y de AVD para cada caso.

**Tasas brutas de AVAD, AVP e AVD**
A partir de los valores absolutos calculados de AVAD, AVAP y AVD y las proyecciones poblacionales de los años de interés (2005, 2009, 2013 y 2018), se calcularon las tasas brutas totales y específicas correspondientes para cada indicador según año, jurisdicción, sexo y grupo de edad.

*Fuentes de datos utilizadas*
- Proyecciones provinciales de población por sexo y por grupos de edad, 2001-2015
“La información utilizada en esta oportunidad proviene de los resultados definitivos de los dos últimos censos nacionales de población (1991 y 2001) y las estadísticas de los registros de nacimientos y defunciones de este último período intercensal.”
Se utilizó esta fuente para la obtención de datos de población del año 2005.
Estas proyecciones están desglosadas por quinquenios y grupos quinquenales según: “0 a 4”, “5 a 9”, …, “80 y más”. 
Se asume la población de “0 a 4” para los indicadores de “1 a 4”. 

Proyecciones provinciales de población por sexo y por grupos de edad, 2001-2015. Buenos Aires, 2005. Disponible en: https://www.indec.gob.ar/indec/web/Institucional-Indec-BibliotecaEnLinea
https://biblioteca.indec.gob.ar/bases/minde/4si20_31.pdf 
Archivo: proyec_2005 (recorte del pdf original “INDEC_proyec 2011-2015”)

- Proyecciones provinciales de población por sexo y grupo de edad, 2010-2040
Se utilizó esta fuente para la obtención de datos de población del año 2009, 2013 y 2018.
Estas proyecciones están desglosadas por año y grupos quinquenales según: “0 a 4”, “5 a 9”, …, “100 y más”.
Se suman los grupos de “80 a 84” a “100 y más” para obtener el grupo “80 y más”.
Se asume la población de “0 a 4” para los indicadores de “1 a 4”.
Se utilizan los datos del 2010 para los cálculos correspondientes al año 2009.  Para los años 2013 y 2018 se utilizan los datos correspondientes a esos años.

Proyecciones provinciales de población por sexo y grupo de edad, 2010-2040. Ciudad Autónoma de Buenos Aires: Instituto Nacional de Estadística y Censos - INDEC, 2013. E-Book. Disponible en: https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-24-85 
Archivo: proyec_2010_2018 (recorte del archivo original “c2_proyecciones_prov_2010_2040”)

**Tasas ajustadas de AVAD, AVP e AVD**
Las tasas ajustadas, por su parte, se calcularon mediante el método directo utilizando como población estándar a la población de cada sexo correspondiente al Censo Nacional de Personas, Hogares y Viviendas 2010 dado que representa la fuente censal más cercana a los años de interés.
A partir de las tasas específicas por año, provincia, sexo y grupo de edad, se calculó el recuento de AVAD, AVP y AVD considerando la población de referencia correspondiente a cada categoría.
recuento de AVAD;AVP;AVD=  (población de referencia*numerador de la tasa específica)/100.000 
Luego, para cada combinación de año, provincia y sexo se sumaron los recuentos de AVAD, AVP y AVD y se dividieron por el recuento de la población de referencia.
Así, se obtuvieron las tasas ajustadas de AVAD, AVP y AVD por año, provincia y sexo, cada 100.000 habitantes.
tasa ajustada de AVAD;AVP;AVD=(suma de AVAD;AVP;AVD)/(suma de población de referencia)*100000

*Fuentes de datos utilizadas*
- Proyecciones provinciales de población por sexo y grupo de edad, 2010-2040
Se seleccionó la distribución de la población por sexo y grupo de edad del año 2010 como población de referencia para el cálculo de las tasas ajustadas.
Se utilizó el mismo archivo mencionado previamente, filtrando por el año 2010.

Proyecciones provinciales de población por sexo y grupo de edad, 2010-2040. Ciudad Autónoma de Buenos Aires: Instituto Nacional de Estadística y Censos - INDEC, 2013. E-Book. Disponible en: https://www.indec.gob.ar/indec/web/Nivel4-Tema-2-24-85 
Archivo: proyec_2010_2018 (recorte del archivo original “c2_proyecciones_prov_2010_2040”)

