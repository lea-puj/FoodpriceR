#------------------------------------------------------------------------------------------#
#                    SEGUNDA FUNCIÓN: MODELO 2: DIETA ADEC EN NUTRIENTES                  #
#-----------------------------------------------------------------------------------------#


CoNA=function(data,EER_LL,UL,exclude=NULL){
  
  #------------------------------------------------------------------------------------------#
  #                       PRIMERA ETAPA: VALIDACIÓN DE LIBRERIAS                             #
  #-----------------------------------------------------------------------------------------#
  
Librerias_base = c("tidyverse","rio","janitor","stringdist","lpSolve","knitr")  # Nombra las librerias necesarias
  
  if (!require("pacman")) install.packages("pacman") # Paquete que simplifica la carga de librerias
  pacman::p_load(char = Librerias_base);Librerias_base_print = paste0(paste0("'", Librerias_base, "'"), collapse = ", ") # Instala si es necesario, o en su defecto, sólo llama los paquetes
  
  # Instala paquetes individualmente si no se han cargado correctamente
  paquetes_faltantes <- Librerias_base[!(Librerias_base %in% pacman::p_loaded())]
  for (paquete in paquetes_faltantes) {
    if (!require(paquete, character.only = TRUE)) {
      install.packages(paquete)
      library(paquete, character.only = TRUE)
    }
  }
  
  
  #cat("\n")
  #cat("Se instalaron y cargaron todas la librerias corectamente")
  #cat("\n")
  
  #------------------------------------------------------------------------------------------#
  #         SEGUNDA ETAPA: VALIDACIÓN DE PARÁMETROS OBLIGATORIOS Y OPCIONALES                #
  #-----------------------------------------------------------------------------------------#
  
# Function to validate the presence of columns in a data frame
  validate_columns <- function(df, required_columns, model_name) {
    if (!is.data.frame(df)) {
      stop(paste("Error:", model_name, "is not a data frame."))
    }
    
    if (ncol(df) <= length(required_columns)) {
      stop(paste("Error:", model_name, "must have at least", length(required_columns), "columns."))
    }
    
    missing_columns <- setdiff(required_columns, colnames(df))
    
    if (length(missing_columns) > 0) {
      stop(paste(model_name, "requires the following columns in the input data:", paste(missing_columns, collapse = ", "), ". Please refer to the documentation for the required column names for the model."))
    }
  }

  
  # Validar data
  validate_columns(data, c("Price_100g", "Food", "Energy"), "data")
  
  # Validar EER_LL
  validate_columns(EER_LL, c("Age","Energy"), "EER_LL")
  
  # Validar UL
  validate_columns(UL, c("Age"), "UL")
  
  
  # Verificar exclude
  if (!is.null(exclude)) {
    if (!is.vector(exclude)) {
      stop("Error: The 'exclude' parameter must be a vector.")
    }
    data <- data[!(data$Food %in% exclude), ]
  }
  
  
  #--------------------------------------------------------------------------------------#
  #                       TERCERA ETAPA: MODELO 2                                        #
  #------------------------------------------------------------------------------------#
  
  req_min_ent= EER_LL 
  req_max_ent= UL %>% select(-any_of(c("Age","Energy")))
  
  
  Req_entrantes=cbind(req_min_ent,req_max_ent)
  
  
  # Verificar si existe la columna "Sex"
  if ("Sex" %in% colnames(EER_LL)) {
    
    # Separar por sexo y obtener los nombres de sexo
    Sexos_min <- split(EER_LL, EER_LL$Sex)
    Sexos_max <- split(UL, UL$Sex)
    
    # Verificar si los vectores son iguales
    if (!identical(names(Sexos_min), names(Sexos_max))) {
      stop("Error: The genders in both requirements are not the same.")
    }
    
    
    # Seleccionar el primer nombre de sexo como referencia
    sexo_nombre <- names(Sexos_min)
    
  } else {
    
    # Si no existe la columna "Sex", establecer el nombre de sexo como "0"
    sexo_nombre <- "0"
    DRI_min_i <- EER_LL
    DRI_max_i <- UL;DRI_max_i[is.na(DRI_max_i)] = 999999
  }
  
  
  #--------------------------------------------------------#
  #               CLICLO PARA CADA SEXO                   #
  #-------------------------------------------------------#
  for (sexo_nombre in sexo_nombre) { 
    
    
    # ------------ PREPARACIÓN DEL MODELO E IDENTI DE NUTRIENTES
    
    if ("Sex" %in% colnames(EER_LL)){
      DRI_min_i <- Sexos_min[[sexo_nombre]]
      DRI_max_i <- Sexos_max[[sexo_nombre]];DRI_max_i[is.na(DRI_max_i)] = 999999}
    
    
    # Organizar ambos df iguales
    
    DRI_min_i <- arrange(DRI_min_i, Age)
    DRI_max_i <- arrange(DRI_max_i, Age)
    
    # Verificar si los vectores de edad son iguales
    if (!identical(levels(as.factor((DRI_min_i$Age))), levels(as.factor((DRI_max_i$Age))))) {
    stop("Error: The age groups in both requirements are not the same.")  
      }
    
    
    # Asignación de vectores
    Precio = data$Price_100g;Food=data$Food;Age=DRI_min_i$Age
    
    # DF de limitaciones en nutrientes y validación de nombres
    DRI_min_li= DRI_min_i %>% select(-any_of(c("Age","Energy","Sex")))
    DRI_max_li= DRI_max_i %>% select(-any_of(c("Age","Energy","Sex")))
    
    if (!identical(names(DRI_min_li), names(DRI_max_li))) {
stop("Error: UL and EER_LL data do not have the same nutrient names in the columns.")
    }
    
    # selecionar de EER_LL energía
    DRI_min_li= DRI_min_i %>% select(-any_of(c("Age","Sex")))
    DRI_max_li= DRI_max_i %>% select(-any_of(c("Age","Energy","Sex")))
    
    
    # Exraer los nutrientes de entrada que son distintos a las columnas: ("Cod_TCAC", "Food", "Serving", "Price_100g")
    DF_Nutrientes_ALimentos <- data %>% select(-any_of(c("Cod_TCAC", "Food", "Serving", "Price_100g")))
    
    
    
    # Identificar las columnas que son iguales de datos insumo y requerimientos y ordenarlas
    nombres_comunes <- intersect(names(DF_Nutrientes_ALimentos), names(DRI_min_li));
    
    # Ordenar los nombrespara el modelo
    DF_Nutrientes_ALimentos <- DF_Nutrientes_ALimentos %>% select(any_of(nombres_comunes));DRI_min_li <- DRI_min_li %>% select(any_of(nombres_comunes));  DRI_max_li <- DRI_max_li %>% select(any_of(nombres_comunes))
    
    
    # Unir los nutrientes de aliemntos en min y max
    Sin_EER= DF_Nutrientes_ALimentos %>% select(-Energy)
    DF_Nutrientes_ALimentos=cbind(DF_Nutrientes_ALimentos,Sin_EER)
    

    # Matriz de coef de restricción al modelo (ENERGIA y nutrientes)
    Coef.Restriq=DF_Nutrientes_ALimentos %>% as.matrix() %>% t()
    
    
    #signos de las restricciones
    constr_signs = c("=", rep(">=", ncol(DRI_min_li)-1), rep("<=", length(DRI_max_li)))
    
    #Unir los EER, minx y max
    Limitaciones=cbind(DRI_min_li,DRI_max_li)

    
    #------------------------------ Preparación de datos de resultados:
    #DF de la solución de intercambios
    Intercambios_CoNA <- data.frame(Food = character(), quantity = numeric(), Demo_Group = integer(), Sex = integer(), Group= character())
    
    #DF de la solución de csotos
    Costo_T <- data.frame(Demo_Group = integer(), Sex = integer(), Costo_dia = numeric())
    
    # df de nutrientes limitantes
    N_limit=data.frame()
    
    # DF de los precios sombra
    S_shadow = na.omit(data.frame(Age = NA,Sex = NA,Nutrients = NA,constraint = NA,value_constraint = NA,SP = NA, SPE = NA))
    # ------------ -------------------- SOLUCIÓN DEL MODELO
    
    for (i in seq_along(Age)) { #ciclo para cada edad
      
      
      
      CoNA = lp(direction = "min",
                objective.in = Precio,
                const.mat = Coef.Restriq,
                const.dir = constr_signs,
                const.rhs =as.vector(unlist(Limitaciones[i, , drop = FALSE])),
                compute.sens = TRUE)
      
      
      #--------------------------------------------------------#
      #               ETAPA DE ESTRUCTURA PLAZA                #
      #-------------------------------------------------------#
      if (CoNA$status == 0 & sum(CoNA$solution)!=0) {
        
        # Guardar estructura de intercambios
        costo <- sum(CoNA$solution * Precio)
        Alimentos_sol <- which(CoNA$solution != 0) # ALimento selexionados
        cantidades_intercambio <- CoNA$solution[Alimentos_sol] # intercambios
        
        if ("Group" %in% colnames(data)) {
          
          indices_coincidencia <- match(Food[Alimentos_sol], data$Food)
          
          Grupo_sex=data$Group[indices_coincidencia]
        } 
        
        # Crear un dataframe temporal de la estructura CIAT
        temp_df <- data.frame(Food = Food[Alimentos_sol],
                              quantity = (cantidades_intercambio*100),
                              Demo_Group = Age[i],
                              Sex = as.numeric(sexo_nombre),Group = if ("Group" %in% colnames(data)) Grupo_sex else NA)
        
        
        # Agregar los resultados al dataframe general
        Intercambios_CoNA <- merge(Intercambios_CoNA, temp_df, all = TRUE)
        
        # Guardar estructura de costo
        
        cost_day=sum(CoNA$solution * Precio)
        
        # Agregar los resultados al dataframe Costo_CoNA
        temp_df <- data.frame(Demo_Group = Age[i],
                              Sex = as.numeric(sexo_nombre),
                              cost_day = sum(CoNA$solution * Precio),
                              Cost_1000kcal=(CoNA$objval/as.vector(unlist(Limitaciones[i, , drop = FALSE])[1])*1000))
        
        # Agregar los resultados al dataframe general
        
        Costo_T=rbind(Costo_T, temp_df)
        
        #------------------------------------#
        #       NUTRIENTES LIMITANTES        #
        #------------------------------------#
        
        
        Nutrie_limit <- data.frame(
          Nutrients = rownames(Coef.Restriq[1:length(names(DRI_min_li)),] %*% as.matrix(CoNA$solution)), # se asumen nutrientes sin EER y sin repetir
          Opt = as.numeric(Coef.Restriq[1:length(names(DRI_min_li)),] %*% as.matrix(CoNA$solution))
        ) %>%  
          mutate(Rest = as.matrix(as.vector(unlist(DRI_min_li[i, , drop = FALSE]))),
                 Diff = round(((Opt - Rest) / Rest * 100),2),
                 Limiting = ifelse(Diff == 0, 1, 0),
                 Age = Age[i],
                 Sex = as.numeric(sexo_nombre)) %>% filter(Nutrients != "Energy") 
        N_limit = rbind(N_limit, Nutrie_limit)
        
        #------------------------------------#
        #       PRECIOS SOMBRA Y ELAST       #
        #------------------------------------#
        
        Spe <- data.frame(
          Age = Age[i],
          Sex = as.numeric(sexo_nombre),
          Nutrients = names(Limitaciones),
          constraint = constr_signs,
          value_constraint = as.vector(unlist(Limitaciones[i, , drop = FALSE])),
          SP = NA,
          SPE = NA
        ) %>%
          mutate(SP = CoNA$duals[1:length(constr_signs)],
                 SPE = ((SP/1) * (unlist(Limitaciones[i, , drop = FALSE])/CoNA$objval)))
        
        S_shadow <- rbind(S_shadow, Spe)
        
        
      }else { # CUANDO EL MODELO NO ENCUENTRE SOLUCIÓN EN ESA EDAD LLENAR CON PRINT
        
        
        temp_df <- data.frame(Food = NA,
                              quantity = NA,
                              Demo_Group = Age[i],
                              Sex = as.numeric(sexo_nombre),
                              Group = NA)
        Intercambios_CoNA <- merge(Intercambios_CoNA, temp_df, all = TRUE)
        
        
        
        # Agregar los resultados al dataframe Costo_CoNA
        temp_df <- data.frame(Demo_Group = Age[i],
                              Sex = as.numeric(sexo_nombre),
                              cost_day = NA,
                              Cost_1000kcal = NA)
        Costo_T <- rbind(Costo_T, temp_df)
        
        
        
        # Nutrients Limitantes
        Nutrie_limit <- data.frame(
          Nutrients = NA,
          Opt = NA,
          Rest = NA,
          Diff = NA,
          Limiting = NA,
          Age = Age[i],
          Sex = as.numeric(sexo_nombre))
        N_limit = rbind(N_limit, Nutrie_limit)
        
        # Precios Sombra y Elasticidades
        Spe <- data.frame(
          Age = Age[i],
          Sex = as.numeric(sexo_nombre),
          Nutrients = NA,
          constraint = NA,
          value_constraint = NA,
          SP = NA,
          SPE = NA)
        S_shadow <- rbind(S_shadow, Spe)
        
      }
      
      
      
    } #FIN DEL CICLO EN EDAD
    
    
    # Asignaciones por sexo
    assign(paste("CoNA_", sexo_nombre, sep = ""), Costo_T)
    assign(paste("Intercambios_CoNA_", sexo_nombre, sep = ""), Intercambios_CoNA)
    
    # asginaciones nutrientes limitantes por sexo
    assign(paste("N_limit_", sexo_nombre, sep = ""), N_limit)
    
    # asignaicoens precios sombra por sexo
    assign(paste("S_shadow_", sexo_nombre, sep = ""), S_shadow)
    #--------------------------------------------------------#
    #        FIND DEL       CLICLO PARA CADA SEXO            #
    #-------------------------------------------------------#
    
  }
  
  
  nombres_comunes_sin_energia <- setdiff(nombres_comunes, "Energy")
  cat("\n")
 cat("The nutrients to use in the model are:", paste(nombres_comunes_sin_energia, collapse = ", "), "\n")
  cat("\n")
  
  # Unir ambos df para cada sexo (si existe)
  if ("Sex" %in% colnames(EER_LL)) {
    
    Costo_CoNA=rbind(CoNA_1,CoNA_0)
    Alimentos_CoNA=rbind(Intercambios_CoNA_0,Intercambios_CoNA_1)
    CoNA_N_Limit=rbind(N_limit_0,N_limit_1) 
    CoNA_SP=rbind(S_shadow_0,S_shadow_1);CoNA_SP = CoNA_SP %>% filter(constraint != "=")
    
    CoNA_SP = CoNA_SP[c("Age", "Sex", "Nutrients", "SP", "SPE","constraint")]
    
    #Nutrients limtantes y precios sombre
    
  } else {
    
    Costo_CoNA <- CoNA_0 %>%
      select(-Sex)
    Alimentos_CoNA<- Intercambios_CoNA_0 %>%
      select(-Sex)
    CoNA_N_Limit<- N_limit_0 %>%
      select(-Sex)
    CoNA_SP<- S_shadow_0 %>%
      select(-Sex)
    
    CoNA_SP = CoNA_SP %>% filter(constraint != "=")
    CoNA_SP = CoNA_SP[c("Age", "Nutrients", "SP", "SPE","constraint")]
    
    
  }
  
  Alimentos_CoNA <- Alimentos_CoNA %>%
    select(-where(~all(is.na(.))))
  
  CoNA_SP <- CoNA_SP %>%
    mutate(constraint = case_when(
      constraint == ">=" ~ "Min",
      constraint == "<=" ~ "Max",
      TRUE ~ as.character(constraint)
    ))
  
  #------------------------------------------------------------------------------------------#
  #                       FIN DEL TERCER MÓDULO COMO FUNCIÓN                               #
  #-----------------------------------------------------------------------------------------#
  #----------------------------#
  #     ASGINACIONES DE LISTA  #
  #----------------------------#
  
  List_CoNA=list(Costo_CoNA,Alimentos_CoNA,CoNA_N_Limit,CoNA_SP,Precio,Food,Req_entrantes);names(List_CoNA)=c("cost","comp","limit","spe","p","x","constraints")
  
  # retorno
  
  cat("CoNA: Average daily cost per 1000 kilocalories is: ", mean(Costo_CoNA$Cost_1000kcal,na.rm=TRUE)) 
  
  return(invisible(List_CoNA))
  
  
}

