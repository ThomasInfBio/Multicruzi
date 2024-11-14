library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(DT)

# Utilisateurs et mots de passe autorisés (à modifier selon les besoins)
user_credentials <- data.frame(
  username = c("user1", "user2"),
  password = c("password1", "password2"),
  stringsAsFactors = FALSE
)

# Interface utilisateur avec un formulaire de connexion
ui <- fluidPage(
  uiOutput("auth_ui"),  # Composant conditionnel pour l'authentification
  conditionalPanel(
    condition = "output.authenticated == true",  # Affichage conditionné selon l'authentification
    # Application du style CSS pour centrer le titre
    tags$head(
      tags$style(HTML("
        .title-panel {
          text-align: left;
          width: 100%;
          font-size: 50px;
          font-weight: bold;
        }
      "))
    ),
    
    # Titre centré
    div(class = "title-panel", "MultiCruzi Data Analysis"),
    
    sidebarLayout(
      sidebarPanel(
        img(src = "https://le-cdn.website-editor.net/s/fbe17f898b624ad9a7e233e0ca024211/dms3rep/multi/opt/Logo-InfYnity-1920w.png?Expires=1733190112&Signature=dbZ3Myt0fv1o588CbDZCnKbtsgIbN-Wp9klvpK9QtwVGcgMPWzBnN3Bk5h5qSc6JPEr6fITbGMNFU5vhyGFXw3pQ8VwJQIuD313hL7qB7gvoJt8-ZBrgiNuWwJG1bZ7BbYD02AakxjY-khm9fFY2m7oBgWwC1ByGrvDgae8L7Vz6eA6tLqYE4-87a-tMoVBCDnxbEestrr16L8OB-DdicPcTj15LgN9EVpSsLFBmj-vyhlqG5xcylMgqny~pquy2UCXG~zHKD9DmLOXmEDbDFwFr5ENoniqhkyrNVCA6nz~FzkPiCix68eW6WTG-sVsUDopU8Ba16UbfyR2kW3gTqg__&Key-Pair-Id=K2NXBXLF010TJW", height = "100px", width = "auto"),
        # Titre de la section de chargement de fichier
        h4("File Upload"),
        fileInput("file", "Upload an Excel File", accept = c(".xlsx")),
        
        # Texte d'aide pour le format de fichier
        helpText("Please upload an Excel file containing the raw MultiCruzi data"),
        helpText("The file must include the columns: 'PatientID', 'Dilution', 'Timepoint', 'IBAGs' and 'PC'."),
        
        # Séparateur
        hr(),
        
        # Titre de la section pour le seuil
        h4("Analysis Parameters"),
        # Champ de mot de passe
        passwordInput("password", "Enter Password:", placeholder = "Enter password"),
        
        # Message d'erreur pour mot de passe incorrect
        textOutput("password_message"),
        
        # Champ numericInput protégé par le mot de passe
        uiOutput("seuil_ui"),
        
        
        
        hr(),
        
        # Dynamic text display
        uiOutput("dynamic_text"),  # Place to display the dynamic text
        
        # Séparateur
        hr(),
        
        # Bouton d'analyse
        actionButton("run_analysis", "Run Analysis", class = "btn-primary")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Raw Data", tableOutput("raw_data")),
          tabPanel("Calculated DF50", tableOutput("df50_calculated")),
          tabPanel("T6M Conclusion", DTOutput("summary_data")),
          tabPanel("T12M Conclusion", DTOutput("summary_data_12"))
        )
      )
    )
  )
)


# Serveur
server <- function(input, output, session) {
  # Variable réactive pour suivre l'état de l'authentification
  authenticated <- reactiveVal(FALSE)
  
  # Sortie de l'authentification pour le front-end
  output$auth_ui <- renderUI({
    if (!authenticated()) {
      tagList(
        h3("Please Log In"),
        textInput("username", "Username"),
        passwordInput("password", "Password"),
        actionButton("login_btn", "Log In")
      )
    }
  })
  
  # Vérification des informations de connexion
  observeEvent(input$login_btn, {
    if (input$username %in% user_credentials$username) {
      # Vérification du mot de passe associé
      correct_password <- user_credentials$password[user_credentials$username == input$username]
      if (input$password == correct_password) {
        authenticated(TRUE)
        showNotification("Login successful", type = "message")
      } else {
        showNotification("Incorrect password", type = "error")
      }
    } else {
      showNotification("Username not found", type = "error")
    }
  })
  
  
  
  
  
  
  # Permettre l'affichage conditionnel des éléments de l'application
  output$authenticated <- reactive({ authenticated() })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)
  
  # Mot de passe correct défini (ici "1234" comme exemple)
  correct_password <- "1234"
  

  
  

  # Afficher ou non les champs protégés en fonction du mot de passe
  output$seuil_ui <- renderUI({
    if (input$password == correct_password) {
      # Affiche les deux champs une fois le mot de passe vérifié
      tagList(
        numericInput("seuil", "DF50 Reduction Threshold", value = 0.3, min = 0, max = 2, step = 0.1),
        sliderInput("Plage", "Thresholds for Conclusion (%)", min = 0, max = 100, value = c(30, 50), step = 5)
      )
    }
  })
  
  output$dynamic_text <- renderUI({
    # Vérifier si le mot de passe est correct
    if (input$password == correct_password) {
      seuil_value <- round((1 - (1 / (2^(input$seuil)))) * 100, 2)  # Calculer la valeur dynamique à partir de numericInput
      slider_value1 <- input$Plage[1]
      slider_value2 <- input$Plage[2]  # Récupérer les valeurs des sliders
      
      # Créer la phrase dynamique avec des sauts de ligne
      HTML(paste(
        "<p><span style='color: green;'> 'Response to Treatment': </span> More than ", slider_value2, "% of baseline reactive antigens show a DF50 reduction greater than", seuil_value, "%</p>",
        "<p><span style='color: orange;'> 'Inconclusive': </span> Between ", slider_value1, "% & ", slider_value2, "% of baseline reactive antigens show a DF50 reduction greater than", seuil_value, "%</p>",
        "<p><span style='color: red;'> 'No Response to Treatment': </span> Less than ", slider_value1, "% of baseline reactive antigens show a DF50 reduction greater than", seuil_value, "%</p>"
      ))
    } else {
      # Si le mot de passe est incorrect ou absent, afficher des valeurs fixes
      seuil_value <- 18.77  # Valeur fixe pour seuil
      slider_value1 <- 30  # Valeur fixe pour slider 1
      slider_value2 <- 50  # Valeur fixe pour slider 2
      
      # Créer la phrase avec des valeurs fixes
      HTML(paste(
        "<p><span style='color: green;'> 'Response to Treatment': </span> More than ", slider_value2, "% of baseline reactive antigens show a DF50 reduction greater than", seuil_value, "%</p>",
        "<p><span style='color: orange;'> 'Inconclusive': </span> Between ", slider_value1, "% & ", slider_value2, "% of baseline reactive antigens show a DF50 reduction greater than", seuil_value, "%</p>",
        "<p><span style='color: red;'> 'No Response to Treatment': </span> Less than ", slider_value1, "% of baseline reactive antigens show a DF50 reduction greater than", seuil_value, "%</p>"
      ))
    }
  })
     
  observe({
    # Vérifier que input$seuil est numérique et non NULL avant d'appliquer la validation
    if (!is.null(input$seuil) && is.numeric(input$seuil)) {
      if (input$seuil < 0) {
        updateNumericInput(session, "seuil", value = 0)
      } else if (input$seuil > 10) {
        updateNumericInput(session, "seuil", value = 10)
      }
    }
  })
  
  # Fonction pour lire et traiter les données
  observeEvent(input$run_analysis, {
    req(input$file)
    
    # Charger les données
    rawdata <- read_excel(input$file$datapath)
    
    # Tableau 1: Données brutes
    output$raw_data <- renderTable({
      rawdata
    })
    
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
        
        
        DF50_IBAG35 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG35n > 2.5), 10, 2^((-1 * LR35$coefficients[1]) / LR35$coefficients[2]))
        DF50_IBAG36 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG36n > 2.5), 10, 2^((-1 * LR36$coefficients[1]) / LR36$coefficients[2]))
        DF50_IBAG37 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG37n > 2.5), 10, 2^((-1 * LR37$coefficients[1]) / LR37$coefficients[2]))
        DF50_IBAG38 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG38n > 2.5), 10, 2^((-1 * LR38$coefficients[1]) / LR38$coefficients[2]))
        DF50_IBAG39 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG39n > 2.5), 10, 2^((-1 * LR39$coefficients[1]) / LR39$coefficients[2]))
        DF50_IBAG95 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG95n > 2.5), 10, 2^((-1 * LR95$coefficients[1]) / LR95$coefficients[2]))
        DF50_IBAG97 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG97n > 2.5), 10, 2^((-1 * LR97$coefficients[1]) / LR97$coefficients[2]))
        DF50_IBAG99 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG99n > 2.5), 10, 2^((-1 * LR99$coefficients[1]) / LR99$coefficients[2]))
        DF50_IBAG101 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG101n > 2.5), 10, 2^((-1 * LR101$coefficients[1]) / LR101$coefficients[2]))
        DF50_IBAG112 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG112n > 2.5), 10,2^((-1 * LR112$coefficients[1]) / LR112$coefficients[2]))
        DF50_IBAG20 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG20n > 2.5), 10, 2^((-1 * LR20$coefficients[1]) / LR20$coefficients[2]))
        DF50_IBAG134 <-  ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG134n > 2.5), 10, 2^((-1 * LR134$coefficients[1]) / LR134$coefficients[2]))
        DF50_IBAG110 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG110n > 2.5), 10, 2^((-1 * LR110$coefficients[1]) / LR110$coefficients[2]))
        DF50_IBAG108 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG108n > 2.5), 10, 2^((-1 * LR108$coefficients[1]) / LR108$coefficients[2]))
        DF50_IBAG131 <- ifelse(any(rawdata$dilution == 50 & df_timepoint$IBAG131n > 2.5), 10, 2^((-1 * LR131$coefficients[1]) / LR131$coefficients[2]))
        
        # Limites aux DF50
        DF50_IBAG35 <- ifelse(LR35$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG35 < 10, 10, ifelse(DF50_IBAG35 > 6400, 6400, DF50_IBAG35)))
        DF50_IBAG36 <- ifelse(LR36$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG36 < 10, 10, ifelse(DF50_IBAG36 > 6400, 6400, DF50_IBAG36)))
        DF50_IBAG37 <- ifelse(LR37$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG37 < 10, 10, ifelse(DF50_IBAG37 > 6400, 6400, DF50_IBAG37)))
        DF50_IBAG38 <- ifelse(LR38$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG38 < 10, 10, ifelse(DF50_IBAG38 > 6400, 6400, DF50_IBAG38)))
        DF50_IBAG39 <- ifelse(LR39$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG39 < 10, 10, ifelse(DF50_IBAG39 > 6400, 6400, DF50_IBAG39)))
        DF50_IBAG95 <- ifelse(LR95$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG95 < 10, 10, ifelse(DF50_IBAG95 > 6400, 6400, DF50_IBAG95)))
        DF50_IBAG97 <- ifelse(LR97$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG97 < 10, 10, ifelse(DF50_IBAG97 > 6400, 6400, DF50_IBAG97)))
        DF50_IBAG99 <- ifelse(LR99$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG99 < 10, 10, ifelse(DF50_IBAG99 > 6400, 6400, DF50_IBAG99)))
        DF50_IBAG101 <- ifelse(LR101$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG101 < 10, 10, ifelse(DF50_IBAG101 > 6400, 6400, DF50_IBAG101)))
        DF50_IBAG112 <- ifelse(LR112$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG112 < 10, 10, ifelse(DF50_IBAG112 > 6400, 6400, DF50_IBAG112)))
        DF50_IBAG20 <- ifelse(LR20$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG20 < 10, 10, ifelse(DF50_IBAG20 > 6400, 6400, DF50_IBAG20)))
        DF50_IBAG134 <- ifelse(LR134$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG134 < 10, 10, ifelse(DF50_IBAG134 > 6400, 6400, DF50_IBAG134)))
        DF50_IBAG110 <- ifelse(LR110$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG110 < 10, 10, ifelse(DF50_IBAG110 > 6400, 6400, DF50_IBAG110)))
        DF50_IBAG108 <- ifelse(LR108$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG108 < 10, 10, ifelse(DF50_IBAG108 > 6400, 6400, DF50_IBAG108)))
        DF50_IBAG131 <- ifelse(LR131$coefficients[2] < -0.0001, 6400, ifelse(DF50_IBAG131 < 10, 10, ifelse(DF50_IBAG131 > 6400, 6400, DF50_IBAG131)))
        
        # Ajouter aux résultats
        results[[paste("Patient", patient, "Timepoint", tp)]] <- data.frame(
          PatientID = patient,
          Timepoint = tp,
          DF50_IBAG35 = DF50_IBAG35,
          DF50_IBAG36 = DF50_IBAG36,
          DF50_IBAG37 = DF50_IBAG37,
          DF50_IBAG38 = DF50_IBAG38,
          DF50_IBAG39 = DF50_IBAG39,
          DF50_IBAG95 = DF50_IBAG95,
          DF50_IBAG97 = DF50_IBAG97,
          DF50_IBAG99 = DF50_IBAG99,
          DF50_IBAG101 = DF50_IBAG101,
          DF50_IBAG112 = DF50_IBAG112,
          DF50_IBAG20 = DF50_IBAG20,
          DF50_IBAG134 = DF50_IBAG134,
          DF50_IBAG110 = DF50_IBAG110,
          DF50_IBAG108 = DF50_IBAG108,
          DF50_IBAG131 = DF50_IBAG131
        )
      }
    }
    
    df_df50 <- do.call(rbind, results)
    
    # Tableau 2: Résultats DF50 calculés
    output$df50_calculated <- renderTable({
      df_df50
    })
    
    # Étape 1 : Calcul des différences DF50 entre timepoints
    df_spread <- pivot_wider(df_df50, names_from = Timepoint, values_from = starts_with("DF50_IBAG"))
    df_spread$DF50_diff_IBAG35 <- log2(df_spread$DF50_IBAG35_0) - log2(df_spread$DF50_IBAG35_6)
    df_spread$DF50_diff_IBAG36 <- log2(df_spread$DF50_IBAG36_0) - log2(df_spread$DF50_IBAG36_6)
    df_spread$DF50_diff_IBAG37 <- log2(df_spread$DF50_IBAG37_0) - log2(df_spread$DF50_IBAG37_6)
    df_spread$DF50_diff_IBAG38 <- log2(df_spread$DF50_IBAG38_0) - log2(df_spread$DF50_IBAG38_6)
    df_spread$DF50_diff_IBAG39 <- log2(df_spread$DF50_IBAG39_0) - log2(df_spread$DF50_IBAG39_6)
    df_spread$DF50_diff_IBAG95 <- log2(df_spread$DF50_IBAG95_0) - log2(df_spread$DF50_IBAG95_6)
    df_spread$DF50_diff_IBAG97 <- log2(df_spread$DF50_IBAG97_0) - log2(df_spread$DF50_IBAG97_6)
    df_spread$DF50_diff_IBAG99 <- log2(df_spread$DF50_IBAG99_0) - log2(df_spread$DF50_IBAG99_6)
    df_spread$DF50_diff_IBAG101 <- log2(df_spread$DF50_IBAG101_0) - log2(df_spread$DF50_IBAG101_6)
    df_spread$DF50_diff_IBAG112 <- log2(df_spread$DF50_IBAG112_0) - log2(df_spread$DF50_IBAG112_6)
    df_spread$DF50_diff_IBAG20 <- log2(df_spread$DF50_IBAG20_0) - log2(df_spread$DF50_IBAG20_6)
    df_spread$DF50_diff_IBAG134 <- log2(df_spread$DF50_IBAG134_0) - log2(df_spread$DF50_IBAG134_6)
    df_spread$DF50_diff_IBAG110 <- log2(df_spread$DF50_IBAG110_0) - log2(df_spread$DF50_IBAG110_6)
    df_spread$DF50_diff_IBAG108 <- log2(df_spread$DF50_IBAG108_0) - log2(df_spread$DF50_IBAG108_6)
    df_spread$DF50_diff_IBAG131 <- log2(df_spread$DF50_IBAG131_0) - log2(df_spread$DF50_IBAG131_6)
    
    # Étape 1 : Calcul des différences DF50 entre timepoints
    df_spread1 <- pivot_wider(df_df50, names_from = Timepoint, values_from = starts_with("DF50_IBAG"))
    df_spread1$DF50_diff_IBAG35 <- log2(df_spread$DF50_IBAG35_0) - log2(df_spread$DF50_IBAG35_12)
    df_spread1$DF50_diff_IBAG36 <- log2(df_spread$DF50_IBAG36_0) - log2(df_spread$DF50_IBAG36_12)
    df_spread1$DF50_diff_IBAG37 <- log2(df_spread$DF50_IBAG37_0) - log2(df_spread$DF50_IBAG37_12)
    df_spread1$DF50_diff_IBAG38 <- log2(df_spread$DF50_IBAG38_0) - log2(df_spread$DF50_IBAG38_12)
    df_spread1$DF50_diff_IBAG39 <- log2(df_spread$DF50_IBAG39_0) - log2(df_spread$DF50_IBAG39_12)
    df_spread1$DF50_diff_IBAG95 <- log2(df_spread$DF50_IBAG95_0) - log2(df_spread$DF50_IBAG95_12)
    df_spread1$DF50_diff_IBAG97 <- log2(df_spread$DF50_IBAG97_0) - log2(df_spread$DF50_IBAG97_12)
    df_spread1$DF50_diff_IBAG99 <- log2(df_spread$DF50_IBAG99_0) - log2(df_spread$DF50_IBAG99_12)
    df_spread1$DF50_diff_IBAG101 <- log2(df_spread$DF50_IBAG101_0) - log2(df_spread$DF50_IBAG101_12)
    df_spread1$DF50_diff_IBAG112 <- log2(df_spread$DF50_IBAG112_0) - log2(df_spread$DF50_IBAG112_12)
    df_spread1$DF50_diff_IBAG20 <- log2(df_spread$DF50_IBAG20_0) - log2(df_spread$DF50_IBAG20_12)
    df_spread1$DF50_diff_IBAG134 <- log2(df_spread$DF50_IBAG134_0) - log2(df_spread$DF50_IBAG134_12)
    df_spread1$DF50_diff_IBAG110 <- log2(df_spread$DF50_IBAG110_0) - log2(df_spread$DF50_IBAG110_12)
    df_spread1$DF50_diff_IBAG108 <- log2(df_spread$DF50_IBAG108_0) - log2(df_spread$DF50_IBAG108_12)
    df_spread1$DF50_diff_IBAG131 <- log2(df_spread$DF50_IBAG131_0) - log2(df_spread$DF50_IBAG131_12)
    
    
    df_differences <- df_spread[, c("PatientID", 
                                    "DF50_diff_IBAG35", 
                                    "DF50_diff_IBAG36", 
                                    "DF50_diff_IBAG37", 
                                    "DF50_diff_IBAG38", 
                                    "DF50_diff_IBAG39",
                                    "DF50_diff_IBAG95",
                                    "DF50_diff_IBAG97",
                                    "DF50_diff_IBAG99",
                                    "DF50_diff_IBAG101",
                                    "DF50_diff_IBAG112",
                                    "DF50_diff_IBAG20",
                                    "DF50_diff_IBAG134",
                                    "DF50_diff_IBAG110",
                                    "DF50_diff_IBAG108",
                                    "DF50_diff_IBAG131"
    )]
    df_differences1 <- df_spread1[, c("PatientID", 
                                    "DF50_diff_IBAG35", 
                                    "DF50_diff_IBAG36", 
                                    "DF50_diff_IBAG37", 
                                    "DF50_diff_IBAG38", 
                                    "DF50_diff_IBAG39",
                                    "DF50_diff_IBAG95",
                                    "DF50_diff_IBAG97",
                                    "DF50_diff_IBAG99",
                                    "DF50_diff_IBAG101",
                                    "DF50_diff_IBAG112",
                                    "DF50_diff_IBAG20",
                                    "DF50_diff_IBAG134",
                                    "DF50_diff_IBAG110",
                                    "DF50_diff_IBAG108",
                                    "DF50_diff_IBAG131"
    )]
    # Étape 2 : Calcul des IBAGs au-dessus du seuil
    
    seuil_utilise <- ifelse(input$password == "1234", input$seuil, 0.3)
    
    df_differences$Nb_of_changes_above_Cutoff <- rowSums(df_differences[, -1] > seuil_utilise)
    
  
    
    
    df_differences1$Nb_of_changes_above_Cutoff <- rowSums(df_differences1[, -1] > seuil_utilise)
    
    
    
    
    
    
    # Étape 3 : Calcul du nombre d'IBAG avec DF50 > 10 au timepoint 0
    final_results_timepoint0 <- subset(df_df50, Timepoint == 0)
    final_results_timepoint0$Nb_of_Reactive_Antigens_at_Baseline <- rowSums(final_results_timepoint0[, -c(1, 2)] > 10)
    
    # Fusionner les deux tables T6M
    df_merged <- merge(df_differences[, c("PatientID", "Nb_of_changes_above_Cutoff")], 
                       final_results_timepoint0[, c("PatientID", "Nb_of_Reactive_Antigens_at_Baseline")], 
                       by = "PatientID")
    
    # Calculer la proportion T6M
    df_merged$proportion_above_threshold <- round(100*(df_merged$Nb_of_changes_above_Cutoff / df_merged$Nb_of_Reactive_Antigens_at_Baseline), 1)
    
    # Ajouter la colonne avec la classification en fonction de la proportion
  
    
    
    # Supposons que l'input du mot de passe soit 'input$password'
    password_correct <- "1234"  # Remplacez par le mot de passe correct
    Plage_default <- c(30, 50)  # Valeurs par défaut
    
    # Vérifier si le mot de passe est correct
    if (input$password == password_correct) {
      Plage <- input$Plage  # Si le mot de passe est correct, utiliser les valeurs du slider
    } else {
      Plage <- Plage_default  # Sinon, utiliser les valeurs par défaut
    }
    
    
    
    
    df_merged <- df_merged %>%
      rename("Proportion Above Threshold %" = proportion_above_threshold)
    
    # Ajouter la colonne avec la classification en fonction de la proportion
    df_merged$Conclusion <- cut(df_merged$'Proportion Above Threshold %',
                                breaks = c(-1, Plage[1], Plage[2], Inf),
                                labels = c("No Response to Treatment", "Inconclusive", "Response to treatment"),
                                right = FALSE)
    
    # Convertir Conclusion en caractère avant de modifier
    df_merged$Conclusion <- as.character(df_merged$Conclusion)
    
    # Ajouter la logique pour afficher un message spécial si le nombre d'antigènes réactifs est inférieur à 6
    df_merged$Conclusion[df_merged$Nb_of_Reactive_Antigens_at_Baseline < 6] <- "Less than 7 antigens reactive at baseline"
    
    # Convertir à nouveau en facteur avec tous les niveaux possibles
    df_merged$Conclusion <- factor(df_merged$Conclusion, levels = c("No Response to Treatment", "Inconclusive", "Response to treatment", "Less than 7 antigens reactive at baseline"))
    
    # Tableau 3: Récapitulatif des IBAGs avec la catégorie de réponse
    output$summary_data <- renderDataTable({
      datatable(df_merged, options = list(pageLength = 50)) %>%
        formatStyle('Conclusion', 
                    target = 'cell', 
                    backgroundColor = styleEqual(
                      c('Inconclusive', 'Response to treatment', 'No Response to Treatment', 'Less than 7 antigens reactive at baseline'), 
                      c('orange', 'lightgreen', 'lightcoral', 'yellow')))  # Ajouter un style pour le nouveau message
    })
    
    
    
    
    # Fusionner les deux tables T12M
    df_merged1 <- merge(df_differences1[, c("PatientID", "Nb_of_changes_above_Cutoff")], 
                       final_results_timepoint0[, c("PatientID", "Nb_of_Reactive_Antigens_at_Baseline")], 
                       by = "PatientID")
    
    # Calculer la proportion T12M
    df_merged1$proportion_above_threshold <- round(100*(df_merged1$Nb_of_changes_above_Cutoff / df_merged1$Nb_of_Reactive_Antigens_at_Baseline), 1)
    
    # Ajouter la colonne avec la classification en fonction de la proportion
    
    
    # Supposons que l'input du mot de passe soit 'input$password'
    password_correct <- "1234"  # Remplacez par le mot de passe correct
    Plage_default <- c(30, 50)  # Valeurs par défaut
    
    # Vérifier si le mot de passe est correct
    if (input$password == password_correct) {
      Plage <- input$Plage  # Si le mot de passe est correct, utiliser les valeurs du slider
    } else {
      Plage <- Plage_default  # Sinon, utiliser les valeurs par défaut
    }
    
    
    
  
    
    df_merged1 <- df_merged1 %>%
      rename("Proportion Above Threshold %" = proportion_above_threshold)
    
    df_merged1$Conclusion <- cut(df_merged1$'Proportion Above Threshold %',
                                breaks = c(-1, Plage[1], Plage[2], Inf),
                                labels = c("No Response to Treatment", "Inconclusive", "Response to treatment"),
                                right = FALSE)
    
    # Convertir Conclusion en caractère avant de modifier
    df_merged1$Conclusion <- as.character(df_merged1$Conclusion)
    
    # Ajouter la logique pour afficher un message spécial si le nombre d'antigènes réactifs est inférieur à 6
    df_merged1$Conclusion[df_merged1$Nb_of_Reactive_Antigens_at_Baseline < 6] <- "Less than 7 antigens reactive at baseline"
    
    # Convertir à nouveau en facteur avec tous les niveaux possibles
    df_merged1$Conclusion <- factor(df_merged1$Conclusion, levels = c("No Response to Treatment", "Inconclusive", "Response to treatment", "Less than 7 antigens reactive at baseline"))
    
    
    # Tableau 3: Récapitulatif des IBAGs avec la catégorie de réponse
    output$summary_data_12 <- renderDataTable({
      datatable(df_merged1, options = list(pageLength = 50)) %>%
        formatStyle('Conclusion', 
                    target = 'cell', 
                    backgroundColor = styleEqual(
                      c('Inconclusive', 'Response to treatment', 'No Response to Treatment', 'Less than 7 antigens reactive at baseline'), 
                      c('orange', 'lightgreen', 'lightcoral', 'yellow')))
    })
    
    
    })
}


# Lancer l'application
shinyApp(ui = ui, server = server)
