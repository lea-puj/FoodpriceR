# FoodpriceR

<p align="center">
<a name="top" href="#"> <img src="https://github.com/lea-puj/FoodpriceR/blob/main/logo_FoodPriceR.png" alt="mf-dots" height="40%" width="60%"/> </a>
</p>

---

### :computer: **Introducción**

En el presente repositorio encontrará la versión 1.0.0 del paquete **FoodpriceR**, el cual alberga **siete** funciones:

- **DataCol:** Proporciona la posibilidad de obtener y manipular datos de precios al por mayor SIPSA del Departamento Administrativo Nacional de Estadística (DANE) en Colombia, adecuados para un mes, año y ciudad específicos. Su resultado principal se centra en las estimaciones de precios al por menor, vinculando los nutrientes correspondientes para cada alimento.
- **CoCA:** Calcula el costo diario mínimo de una dieta adecuada en calorías para un individuo, basándose en su requerimiento energético estimado.
- **CoNA:** Calcula el costo diario mínimo de una dieta adecuada en nutrientes para un individuo según sus ingestas de referencia dietética.
- **CoRD:** Calcula el costo diario mínimo de una dieta recomendada o saludable que garantiza diversidad entre los grupos de alimentos, siguiendo las recomendaciones de grupos de alimentos proporcionadas por las Guías Alimentarias Basadas en Alimentos (GABA).
- **IncomeCol:** Carga y procesa los ingresos corrientes mensuales y la proporción destinada a alimentos para hogares urbanos de las 13 principales ciudades y áreas metropolitanas de Colombia (GEIH – marco 2018 y ECV).
- **HCost:** Integra los costos diarios mínimos de las tres dietas (CoCA, CoNA y CoRD) para un hogar representativo definido por edad y sexo de sus miembros, usando como insumo las mismas estructuras de datos de FoodpriceR (precios, composición nutricional, requerimientos energéticos y de nutrientes). Devuelve el costo total y per cápita de cada tipo de dieta para el hogar.
- **Afford:** A partir del costo mínimo de las dietas y de una base con ingresos y gasto en alimentos de los hogares, calcula indicadores de asequibilidad: proporción de hogares que no pueden costear la dieta, razón costo/gasto y brechas de acceso.

---

### :books: **Documentación**

- **Funciones para la estimación del costo de la dieta** (*DataCol, CoCA, CoNA, CoRD, HCost*):  
  [Foodprice – documentación técnica](https://github.com/lea-puj/FoodpriceR/blob/main/Foodprice-%20documentation.pdf)

- **Funciones para el análisis de asequibilidad** (*Afford e IncomeCol*):  
  [Foodprice v2 – documentación CRAN](https://github.com/lea-puj/FoodpriceR/blob/main/Foodpricev2-CRAN.pdf)

---

### :wrench: **Instrucciones de instalación y uso**

**1. Instalación y carga**

Instale y cargue el paquete alojado en el presente repositorio ejecutando en R:

```r
devtools::install_github("lea-puj/FoodpriceR"); library(FoodpriceR)


```

**2. Información**

Para conocer la información, el uso y las condiciones de funcionamiento de cada función use en R la función "help" o "??" para acceder a la documentación:

```
help(DataCol)

# ó

??DataCol

```


