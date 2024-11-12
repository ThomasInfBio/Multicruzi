raw_data_Multicruzi_timepoint <- read_excel("C:/Users/Thomas/Desktop/Analyse_R/raw_data_Multicruzi_timepoint.xlsx")
rawdata <- raw_data_Multicruzi_timepoint
# Préparation des données
df_norm <- data.frame(
  PatientID = rawdata$PatientID,
  log2DF = log2(rawdata$dilution),
  timepoint = rawdata$timepoint,
  
  IBAG35n = ifelse(
    (rawdata$IBAG35 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG35 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG35 / rawdata$PC)) / (100 * rawdata$IBAG35 / rawdata$PC)))),
  IBAG36n = ifelse(
    (rawdata$IBAG36 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG36 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG36 / rawdata$PC)) / (100 * rawdata$IBAG36 / rawdata$PC)))),
  IBAG37n = ifelse(
    (rawdata$IBAG37 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG37 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG37 / rawdata$PC)) / (100 * rawdata$IBAG37 / rawdata$PC)))),
  IBAG38n = ifelse(
    (rawdata$IBAG38 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG38 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG38 / rawdata$PC)) / (100 * rawdata$IBAG38 / rawdata$PC)))),
  IBAG39n = ifelse(
    (rawdata$IBAG39 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG39 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG39 / rawdata$PC)) / (100 * rawdata$IBAG39 / rawdata$PC)))),
  IBAG95n = ifelse(
    (rawdata$IBAG95 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG95 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG95 / rawdata$PC)) / (100 * rawdata$IBAG95 / rawdata$PC)))),
  
  IBAG97n = ifelse(
    (rawdata$IBAG97 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG97 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG97 / rawdata$PC)) / (100 * rawdata$IBAG97 / rawdata$PC)))),
  
  IBAG99n = ifelse(
    (rawdata$IBAG99 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG99 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG99 / rawdata$PC)) / (100 * rawdata$IBAG99 / rawdata$PC)))),
  
  IBAG101n = ifelse(
    (rawdata$IBAG101 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG101 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG101 / rawdata$PC)) / (100 * rawdata$IBAG101 / rawdata$PC)))),
  
  IBAG112n = ifelse(
    (rawdata$IBAG112 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG112 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG112 / rawdata$PC)) / (100 * rawdata$IBAG112 / rawdata$PC)))),
  
  IBAG20n = ifelse(
    (rawdata$IBAG20 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG20 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG20 / rawdata$PC)) / (100 * rawdata$IBAG20 / rawdata$PC)))),
  
  IBAG134n = ifelse(
    (rawdata$IBAG134 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG134 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG134 / rawdata$PC)) / (100 * rawdata$IBAG134 / rawdata$PC)))),
  
  IBAG110n = ifelse(
    (rawdata$IBAG110 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG110 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG110 / rawdata$PC)) / (100 * rawdata$IBAG110 / rawdata$PC)))),
  
  IBAG108n = ifelse(
    (rawdata$IBAG108 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG108 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG108 / rawdata$PC)) / (100 * rawdata$IBAG108 / rawdata$PC)))),
  
  IBAG131n = ifelse(
    (rawdata$IBAG131 > rawdata$PC), log2((100 - 99) / 99),
    ifelse(rawdata$IBAG131 == 0, log2((100 - 0.01) / 0.01),
           log2((100 - (100 * rawdata$IBAG131 / rawdata$PC)) / (100 * rawdata$IBAG131 / rawdata$PC))))
)

# Calcul des DF50 pour chaque patient et chaque timepoint
results <- list()
patients <- unique(df_norm$PatientID)

for (patient in patients) {
  df_patient <- subset(df_norm, PatientID == patient)
  timepoints <- unique(df_patient$timepoint)
  
  for (tp in timepoints) {
    df_timepoint <- subset(df_patient, timepoint == tp)
    
    # Modèles de régression pour chaque IBAG
    LR35 <- lm(IBAG35n ~ log2DF, data = df_timepoint)
    LR36 <- lm(IBAG36n ~ log2DF, data = df_timepoint)
    LR37 <- lm(IBAG37n ~ log2DF, data = df_timepoint)
    LR38 <- lm(IBAG38n ~ log2DF, data = df_timepoint)
    LR39 <- lm(IBAG39n ~ log2DF, data = df_timepoint)
    LR95 <- lm(IBAG95n ~ log2DF, data = df_timepoint)
    LR97 <- lm(IBAG97n ~ log2DF, data = df_timepoint)
    LR99 <- lm(IBAG99n ~ log2DF, data = df_timepoint)
    LR101 <- lm(IBAG101n ~ log2DF, data = df_timepoint)
    LR112 <- lm(IBAG112n ~ log2DF, data = df_timepoint)
    LR20 <- lm(IBAG20n ~ log2DF, data = df_timepoint)
    LR134 <- lm(IBAG134n ~ log2DF, data = df_timepoint)
    LR110 <- lm(IBAG110n ~ log2DF, data = df_timepoint)
    LR108 <- lm(IBAG108n ~ log2DF, data = df_timepoint)
    LR131 <- lm(IBAG131n ~ log2DF, data = df_timepoint)
    
    print(LR134)