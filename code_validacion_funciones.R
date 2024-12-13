
#-----------------------------------------------------#
# Comprobacion de la correcion de la funcion Incomcol #
#-----------------------------------------------------#

# 1) Se añadio un parametro llamado share.n ( proporcion de gasto en alimentacion)
# 2) Se corrigio un pequeño problema de indossitencia en un bucle


Proporción.G.Alimento <- c(0.39, 0.39, 0.36, 0.36,
                           0.35, 0.35, 0.32, 0.32, 0.26, 0.26)

# comprobamos el funcionaminto de la funcion Incomcol con el
# parametro  share.n y sin el parametro que calula los share promedios


Data22.Icol= IncomeCol1(Month=9,Year=2022,City="Cali",
                       Share.n = Proporción.G.Alimento )

Data22.Icol= IncomeCol1(Month=9,Year=2022,City="Cali" )

Data22.Icol <- Data22.Icol %>%
  mutate(deciles = factor(deciles,
                          levels = c("Decil 1", "Decil 2", "Decil 3", "Decil 4",
                                     "Decil 5", "Decil 6", "Decil 7", "Decil 8",
                                     "Decil 9", "Decil 10")))

# Tabla Descriptiva de "Per capital Incomen (COP)" pesos colombianos

resultados22.1 <- list() # lista vacía para almacenar los resultados

# Bucle for para crear tabla descriptiva (mean,sd,max) de los deciles 1:10 de "Per capital Incomen (USD)"
for (i in 1:10) {
  decil_name <- paste("Decil", i)
  # Filtrar los datos para el decil actual y calcular las estadísticas
  resultados22.1[[decil_name]] <- Data22.Icol %>%
    filter(deciles == decil_name) %>%
    summarise(
      media = mean(per_capita_income , na.rm = TRUE),  # Calcula la media ajustada
      desviacion = sd(per_capita_income , na.rm = TRUE),  # Calcula la desviación estándar ajustada
      maximo = max(per_capita_income , na.rm = TRUE)  # Calcula el valor máximo ajustado
    )
}

# Convertir la lista de resultados en un data frame
tab_desva.22Icol.p <- bind_rows(resultados22.1, .id = "Decil")


# Tabla Descriptiva de "Per Capita Food Expenditure (USD)" 3785 pesos/dolar

#Proporción de Gasto en alimentos
Proporción.G.Alimento <- c(0.39, 0.39, 0.36, 0.36,
                           0.35, 0.35, 0.32, 0.32, 0.26, 0.26)

resultados22.2 <- list() # lista vacía para almacenar los resultados

# Bucle for para crear tabla descriptiva (mean,sd,max) de los deciles 1:10 de "Per Capita Food Expenditure (USD)"
for (i in 1:10) {
  decil_name <- paste("Decil", i)
  # Filtrar los datos para el decil actual y calcular las estadísticas
  resultados22.2[[decil_name]] <- Data22.Icol %>%
    filter(deciles == decil_name) %>%
    summarise(
      media = mean(food_exp_per_capita , na.rm = TRUE),
      desviacion = sd(food_exp_per_capita, na.rm = TRUE),
      maximo = max(food_exp_per_capita , na.rm = TRUE),
      Proportion_of_Food_Expenditure = Proporción.G.Alimento[i]*100
    )
}

# Convertir la lista de resultados en un data frame
tab_desva.22Icol.fp <- bind_rows(resultados22.2, .id = "Decil")

# Unir ambas tablas por la columna "Decil"
tab_desva.22Icol <- left_join(tab_desva.22Icol.p, tab_desva.22Icol.fp, by = "Decil")
view(tab_desva.22Icol)



#------------------------------------------------------------------------------#



#----------------------------------------------#
# Validadcion de la fucnion Afford             #
#----------------------------------------------#


#...................................................#
# Hcost: Estimación de Métricas de Costo Mínimo     #
# para Dietas en Hogares                            #
#...................................................#

# Paquete: cargar y tambien para desactivar del R
library(FoodpriceR)
detach("package:FoodpriceR", unload = TRUE)



data22 = DataCol(Month = 9 , Year = 2022, City = "cali", Percentile = 0.25) # Base de datos de septiembre del 2022 de cali

# Nota: Estraigo los aliemntos que no necesito debido a que la funcion Hcost
#       no tiene el paremtro de exclude


data22.1 <- data22 %>%
  filter(!Food %in% c("Sal yodada", "Queso campesino", "Mayonesa doy pack"))



#REQUERIMIENTOS CALI
# 2022 septiembre  ciudad cali

library(readxl)

ruta <- file.path("C:", "Users", "portatil", "Downloads", "workFoodprice", "EER_Cali", "EER_Cali.xlsx")
EERCali.coca <- read_xlsx(ruta, sheet = "EERCali.coca")
EE.LLcali <- read_xlsx(ruta, sheet = "EE.LLcali")
ULcali <- read_xlsx(ruta, sheet = "ULcali")
serv.cali <- read_xlsx(ruta, sheet = "serv.cali")
data("diverse")



# VALIDADCION HACOST Parametro exclude

#EXCLUIMOS LA COMIDA PARA COMPORBAR SI FUNCIONA EL PAQUETE
comida_exluida <- c("Sal yodada", "Queso campesino", "Mayonesa doy pack")
class(comida_exluida)

Diet_HC2NEW = HCost(Data = data22,
                  Household = Household,
                  ERR = EERCali.coca, EER_LL = EE.LLcali,
                  UL = ULcali %>% select(-Energy),
                  Serv = serv.cali, Diverse= diverse,
                  exclude = c("Sal yodada", "Queso campesino", "Mayonesa doy pack")
                  )

Diet_HC2_pack = HCost(Data = data22.1,
                     Household = Household,
                     ERR = EERCali.coca, EER_LL = EE.LLcali,
                     UL = ULcali %>% select(-Energy),
                     Serv = serv.cali, Diverse= diverse
                     )

Diet_HC2_pack$Model_CoCA == Diet_HC2NEW$Model_CoCA
Diet_HC2_pack$Model_CoNA == Diet_HC2NEW$Model_CoNA
Diet_HC2_pack$Model_CoRD == Diet_HC2NEW$Model_CoRD


Diet_HC2NEW$Model_CoCA
Diet_HC2NEW$Model_CoNA
Diet_HC2NEW$Model_CoRD

# VALIDACION Afford

Data22.AffordNEW <- Afford1(Hexpense = Data22.Icol,
                         Model_CoCA = Diet_HC2$Model_CoCA,
                        Model_CoNA = Diet_HC2$Model_CoNA,
                        Model_CoRD = Diet_HC2$Model_CoRD)
library(FoodpriceR)

Data22.Afford_pack <- Afford(Hexpense = Data22.Icol,
                            Model_CoCA = Diet_HC2$Model_CoCA,
                            Model_CoNA = Diet_HC2$Model_CoNA,
                            Model_CoRD = Diet_HC2$Model_CoRD)


Data22.AffordNEW$Poverty_outcome == Data22.Afford_pack$Poverty_outcome
identical(Data22.AffordNEW$Poverty_outcome, Data22.Afford_pack$Poverty_outcome)


# Resultados de los indicadores de asequibilidad calculados para los modelos
#    CoCA, CoNA y CoRD, incluyendo tasas, brechas y severidades por decil.



#------------------------------------------------------------------#
#Validacion de la funcion CONA correger los requerimientos-------- #
#------------------------------------------------------------------#


library(FoodpriceR)

#Dieta CoNa
Modelo2.2=CoNA2(data=data22,
               EER_LL = EE.LLcali,
               UL= ULcali %>% select(-Energy),
               exclude = c("Sal yodada",
                           "Queso campesino",
                           "Mayonesa doy pack"))

Modelo2.2$cost

# Análisis: solución para el caso Age == >70 años & Sex == 0 (No hay convergencia)

EE.LLcali_op2 = EE.LLcali
EE.LLcali_op2[8,4:19] = EE.LLcali_op2[8,4:19]*0.97

Modelo1.2.2=CoNA(data=data22,
                 EER_LL = EE.LLcali_op2,
                 UL= ULcali %>% select(-Energy),
                 exclude = c("Sal yodada",
                             "Queso campesino",
                             "Mayonesa doy pack"))
Modelo1.2.2$cost # Se encuentra solución en todos los casos








EE.LLcali_op1 = EE.LLcali
EE.LLcali_op1$Magnesium[8] = EE.LLcali_op1$Magnesium[8]*0.97

Modelo1.2.1=CoNA(data=data22,
                 EER_LL = EE.LLcali_op1,
                 UL= ULcali %>% select(-Energy),
                 exclude = c("Sal yodada",
                             "Queso campesino",
                             "Mayonesa doy pack"))
Modelo1.2.1$cost # Se encuentra solución en todos los casos






Modelo2.2$cost == Modelo1.2.1$cost


