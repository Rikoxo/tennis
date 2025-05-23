# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyr)
library(plotly)

server <- function(input, output, session) {
  # Réactifs pour stocker les données
  reservations_data <- reactiveVal(list())
  inscrits_data <- reactiveVal(NULL)
  combined_data <- reactiveVal(NULL)
  grouped_tarifs <- reactiveVal(NULL)
  tarif_groups <- reactiveVal(list()) # Stockage des groupes de tarifs
  available_tarifs <- reactiveVal(NULL) # Tarifs disponibles (modifiable)
  applied_tarifs <- reactiveVal(data.frame(Groupe = character(), Tarifs = list(), stringsAsFactors = FALSE))  
  
  # Fonction de nettoyage des libellés
  clean_labels <- function(df, column_name = "Libellé") {
    if (!column_name %in% colnames(df)) {
      stop(paste("Colonne", column_name, "manquante dans le dataframe."))
    }
    
    df_clean <- df %>%
      mutate(individual_names = gsub("</p><p>", ",", gsub("<p>|</p>", ",", !!sym(column_name)))) %>%
      separate_rows(individual_names, sep = ",") %>%
      mutate(individual_names = trimws(individual_names)) # Nettoyer les espaces
    
    df_clean <- df_clean %>%
      rename(libellé_clean = individual_names) %>%
      select(-all_of(column_name)) # Supprimer l'ancien libellé si nécessaire
    
    return(df_clean)
  }
  
  # Traitement des fichiers de réservations et inscrits
  observeEvent(input$process, {
    req(input$reservations, input$inscrits)
    
    # Charger et combiner les fichiers de réservations
    reservation_files <- input$reservations$datapath
    reservations_list <- lapply(reservation_files, function(file) {
      df <- read.csv(file, header = TRUE, sep = ",", fileEncoding = "ISO-8859-13")
      
      if (!("Type" %in% colnames(df)) || !("Libellé" %in% colnames(df))) {
        warning(paste("Le fichier", file, "ne contient pas les colonnes nécessaires ('Type' ou 'Libellé')."))
        return(NULL)
      }
      
      df <- df[df$Type == "Réservation Joueur", ]
      
      if (nrow(df) == 0) {
        warning(paste("Le fichier", file, "n'a aucune ligne avec 'Type = Réservation Joueur'."))
        return(NULL)
      }
      
      df <- clean_labels(df, column_name = "Libellé")
      return(df)
    })
    
    reservations_list <- Filter(Negate(is.null), reservations_list)
    reservations_data(do.call(bind_rows, reservations_list))
    
    # Charger le fichier des inscrits
    inscrits_data(read.csv(input$inscrits$datapath, header = TRUE, sep = ",", fileEncoding = "ISO-8859-13"))
    
    # Combiner les réservations avec les inscrits
    combined <- reservations_data() %>%
      rowwise() %>%
      mutate(joueurs = strsplit(libellé_clean, ", ")) %>%
      unnest(cols = joueurs) %>%
      left_join(inscrits_data(), by = c("joueurs" = "Nom.Prénom"))
    
    combined_data(combined)
    
    # Mettre à jour les tarifs disponibles
    available_tarifs(combined_data() %>%
                       filter(Type == "Réservation Joueur") %>%
                       pull(`Tarif.attribué`) %>%
                       unique())
  })
  
  # Dynamique : Sélection des tarifs à regrouper
  output$tarif_selector <- renderUI({
    req(available_tarifs())
    checkboxGroupInput("tarif_choices", "Choisissez les tarifs à regrouper :", 
                       choices = available_tarifs(), 
                       selected = NULL)
  })
  
  # Dynamique : Sélection des tarifs à supprimer
  output$tarif_removal_selector <- renderUI({
    req(available_tarifs())
    checkboxGroupInput("tarif_to_remove", "Sélectionnez un tarif à supprimer :", 
                choices = available_tarifs(), 
                selected = NULL)
  })
  
  # Ajouter un groupe de tarifs
  observeEvent(input$add_group, {
    req(input$group_name, input$tarif_choices)
    
    current_groups <- tarif_groups()
    new_group <- list(
      group_name = input$group_name,
      tarifs = input$tarif_choices
    )
    tarif_groups(c(current_groups, list(new_group)))
    
    # Réinitialiser les champs
    updateTextInput(session, "group_name", value = "")
    updateCheckboxGroupInput(session, "tarif_choices", selected = NULL)
  })
  
  # Supprimer un tarif
  observeEvent(input$remove_tarif, {
    req(input$tarif_to_remove, available_tarifs())
    
    # Retirer le tarif sélectionné de la liste des tarifs disponibles
    updated_tarifs <- setdiff(available_tarifs(), input$tarif_to_remove)
    available_tarifs(updated_tarifs)
    
    # Supprimer le tarif des données combinées
    combined <- combined_data() %>%
      filter(`Tarif.attribué` != input$tarif_to_remove)
    combined_data(combined)
    
    # Supprimer le tarif des groupes existants
    updated_groups <- tarif_groups() %>%
      purrr::map(function(group) {
        group$tarifs <- setdiff(group$tarifs, input$tarif_to_remove)
        group
      }) %>%
      purrr::discard(~length(.x$tarifs) == 0) # Supprimer les groupes vides
    tarif_groups(updated_groups)
  })
  
  # Aperçu des regroupements
  output$preview_tarifs <- renderTable({
    req(tarif_groups())
    
    tarif_groups() %>%
      purrr::map_df(~data.frame(Groupe = .x$group_name, Tarifs = paste(.x$tarifs, collapse = " , ")))
  })
  
  
  # Fonction pour attribuer les groupes aux données combinées
  map_groups_to_combined <- function(combined, groups) {
    combined %>%
      rowwise() %>%
      mutate(
        Groupe = {
          matching_groups <- purrr::keep(groups, ~ `Tarif.attribué` %in% .x$tarifs)
          if (length(matching_groups) > 0) matching_groups[[1]]$group_name else NA
        }
      )
  }
  
  # Appliquer les groupes de tarifs aux données combinées
  observeEvent(input$apply_tarif_settings, {
    req(combined_data(), tarif_groups())
    
    groups <- tarif_groups()
    combined <- combined_data()
    
    # Mapper les groupes sur les données combinées
    combined <- map_groups_to_combined(combined, groups)
    
    combined_data(combined) # Mettre à jour les données combinées avec les groupes
  })
  
  
  
  
  # Appliquer les regroupements de tarifs
  # observeEvent(input$apply_tarif_settings, {
  #   req(combined_data(), tarif_groups())
  #   
  #   grouped <- combined_data()
  #   for (group in tarif_groups()) {
  #     grouped <- grouped %>%
  #       mutate(`Tarif.attribué` = ifelse(`Tarif.attribué` %in% group$tarifs, group$group_name, `Tarif.attribué`))
  #   }
  #   
  #   grouped_tarifs(grouped)
  # })
  
  # Aperçu des données combinées
  output$preview_reservations <- renderTable({
    req(reservations_data())
    head(reservations_data())
  })
  
  output$preview_inscrits <- renderTable({
    req(inscrits_data())
    head(inscrits_data())
  })
  
  # output$preview_combined <- renderTable({
  #   req(combined_data())
  #   head(combined_data(),30)
  # })
  
  
  
  transformed_data <- reactive({
    req(combined_data())
    combined_data() %>% 
      mutate(
        Date = as.Date(Date, format = "%d/%m/%Y"),
        Jour = weekdays(Date)
      ) %>% 
      filter(!is.na(Groupe)) %>% distinct(Date, Horaires, joueurs, .keep_all = TRUE) # Garder uniquement les lignes avec des groupes valides
  })
  
  output$preview_combined <- renderTable({
    req(transformed_data())
    head(transformed_data(),30)
  })

  
  # 1. Nb de réservation total par semaine
  
 

  # output$plot_total <- renderPlotly({
  #   req(transformed_data())
  #   data <- transformed_data()
  # 
  #   # Extraire la période de l'analyse
  #   date_min <- min(data$Date, na.rm = TRUE)
  #   date_max <- max(data$Date, na.rm = TRUE)
  # 
  #   # Générer le titre avec la période
  #   title_with_period <- paste("Réservations Totales du", format(date_min, "%d/%m/%Y"),
  #                              "au", format(date_max, "%d/%m/%Y"))
  # 
  #   # Regrouper les données par semaine et ajouter le mois
  #   data_weekly <- data %>%
  #     mutate(
  #       Semaine = format(Date, "%Y-%U"), # Année + numéro de semaine
  #       Mois = factor(format(Date, "%m"),
  #                     levels = sprintf("%02d", 1:12),
  #                     labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", "Juil", "Août", "Sep", "Oct", "Nov", "Déc"))
  #     ) %>%
  #     count(Semaine, Mois)  # Compter les réservations par semaine et mois
  # 
  #   # Création du graphique avec valeurs sur les barres
  #   plot_ly(data_weekly, x = ~Semaine, y = ~n, color = ~Mois, type = "bar",
  #           text = ~n, textposition = 'outside') %>%
  #     layout(
  #       title = title_with_period,
  #       xaxis = list(title = "Semaine", tickangle = -45),
  #       yaxis = list(title = "Nombre de Réservations"),
  #       bargap = 0.2, # Espacement des barres
  #       showlegend = TRUE
  #     )
  # })
  # 
  
  
  
  output$plot_total <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()

    # Extraire la période de l'analyse
    date_min <- min(data$Date, na.rm = TRUE)
    date_max <- max(data$Date, na.rm = TRUE)

    # Titre avec période
    title_with_period <- paste("Réservations Totales du", format(date_min, "%d/%m/%Y"),
                               "au", format(date_max, "%d/%m/%Y"))

    # Générer les semaines (même coupure pour tout)
    data <- data %>%
      mutate(
        WeekStart = cut(Date, breaks = "week", start.on.monday = TRUE),
        Mois = format(Date, "%m")
      )

    # Trouver le mois majoritaire par semaine
    week_month <- data %>%
      group_by(WeekStart, Mois) %>%
      summarise(nb = n(), .groups = "drop") %>%
      group_by(WeekStart) %>%
      slice_max(nb, n = 1, with_ties = FALSE) %>%
      ungroup()

    # Compter les réservations par semaine
    data_weekly <- data %>%
      count(WeekStart) %>%
      left_join(week_month, by = "WeekStart") %>%
      # mutate(
      #   Mois = factor(Mois, levels = sprintf("%02d", 1:12),
      #                 labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin",
      #                            "Juil", "Août", "Sep", "Oct", "Nov", "Déc")),
      #   WeekStart = as.Date(WeekStart)
      # )
      
      mutate(
        Mois = factor(Mois, levels = sprintf("%02d", 1:12),
                      labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                 "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")),
        WeekStart = as.Date(WeekStart),
        NumSemaine = strftime(WeekStart,  format( "%Y-%U"))  # Numéro de semaine ISO
      ) %>%
      mutate(Mois = factor(Mois, levels = unique(Mois)))  # <- Ordre dynamique
    

    # Graphique
    plot_ly(data_weekly, x = ~NumSemaine, y = ~n, color = ~Mois, type = "bar",
            text = ~n, textposition = 'outside') %>%
      layout(
        title = title_with_period,
        xaxis = list(title = "Semaine", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        bargap = 0.2,
        showlegend = TRUE
      )
  })


  
  
  

  
  # 2. Nb de réservation tot H et F

  
  
  # output$plot_total_hf <- renderPlotly({
  #   req(transformed_data())
  #   data <- transformed_data() %>%
  #     count(Sexe)  # Compter le nombre de réservations par genre
  #   
  #   couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
  #     
  #   
  #   plot_ly(
  #     data, 
  #     x = ~Sexe, 
  #     y = ~n, 
  #     type = 'bar', 
  #     color = ~Sexe,
  #     colors = couleurs_sexe,
  #     #marker = list(color = c("pink", "skyblue")),  # Bleu pour H, Rose pour F
  #     text = ~n, 
  #     textposition = 'auto',
  #     textfont = list(size = 14, color = "black")  # Texte plus grand et noir
  #   ) %>%
  #     layout(
  #       title = input$title_total_hf,
  #       xaxis = list(title = "Genre"),
  #       yaxis = list(title = "Nombre de Réservations"),
  #       showlegend = TRUE
  #     )
  # })
  
  
  output$plot_total_hf <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Sexe)  # Comptage des réservations par sexe

    total <- sum(data$n)  # Nombre total de réservations

    # if (input$affichage_total_hf == "percentage") {
    #   data <- data %>%
    #     mutate(n = round((n / total) * 100, 2))  # Convertir en pourcentage
    # }

    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")

    plot_ly(
      data,
      x = ~Sexe,
      y = ~n,
      type = 'bar',
      color = ~Sexe,
      colors = couleurs_sexe,
      text = ~round(n, 2),  # Arrondir le texte affiché
      #text = ~ifelse(input$affichage_total_hf == "percentage", paste0(round(n, 1), "%"), n),
      textposition = 'auto',
      textfont = list(size = 14, color = "black")
    ) %>%
      layout(
        title = input$title_total_hf,
        xaxis = list(title = "Genre"),
        #yaxis = list(title = ifelse(input$affichage_total_hf == "percentage", "Pourcentage (%)", "Nombre de Réservations")),
        yaxis = list(title = "Nombre de Réservations"),
        
        showlegend = TRUE
      )
  })


  output$plot_total_hf_perc <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Sexe)  # Comptage des réservations par sexe
    
    total <- sum(data$n)  # Nombre total de réservations


    data <- data %>%
      mutate(n = round((n / total) * 100, 2))  # Convertir en pourcentage
    
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(
      data,
      x = ~Sexe,
      y = ~n,
      type = 'bar',
      color = ~Sexe,
      colors = couleurs_sexe,
      text = ~round(n, 2),  # Arrondir le texte affiché
      #text = ~ifelse(input$affichage_total_hf == "percentage", paste0(round(n, 1), "%"), n),
      textposition = 'auto',
      textfont = list(size = 14, color = "black")
    ) %>%
      layout(
        title = input$title_total_hf_perc_ens,
        xaxis = list(title = "Genre"),
        #yaxis = list(title = ifelse(input$affichage_total_hf == "percentage", "Pourcentage (%)", "Nombre de Réservations")),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  
  # output$plot_total_hf_perc_grp <- renderPlotly({
  #   req(transformed_data())
  #   
  #   data <- transformed_data() %>%
  #     count(Sexe)  # Comptage des réservations par sexe
  #   
  #   # total <- sum(data$n)  # Nombre total de réservations
  #   # 
  #   # 
  #   #   data <- data %>%
  #   #     mutate(n = round((n / total) * 100, 2))  # Convertir en pourcentage
  #     
  #     
  #     par_heures_categorie <- data %>%
  #       group_by(Groupe) %>%
  #       summarise(total = sum(n))
  # 
  #     # Calculer le pourcentage par catégorie
  #     data <- data %>%
  #       left_join(par_heures_categorie, by = "Groupe") %>%
  #       mutate(n = round((n / total) * 100, 2)) %>%
  #       select(-total)
  #   
  #   
  #   couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
  #   
  #   plot_ly(
  #     data,
  #     x = ~Sexe,
  #     y = ~n,
  #     type = 'bar',
  #     color = ~Sexe,
  #     colors = couleurs_sexe,
  #     text = ~round(n, 2),  # Arrondir le texte affiché
  #     #text = ~ifelse(input$affichage_total_hf == "percentage", paste0(round(n, 1), "%"), n),
  #     textposition = 'auto',
  #     textfont = list(size = 14, color = "black")
  #   ) %>%
  #     layout(
  #       title = input$title_total_hf_perc_grp,
  #       xaxis = list(title = "Genre"),
  #       #yaxis = list(title = ifelse(input$affichage_total_hf == "percentage", "Pourcentage (%)", "Nombre de Réservations")),
  #       yaxis = list(title = "Pourcentage (%)"),
  #       showlegend = TRUE
  #     )
  # })
  # 
  output$ratio <- renderText({
    req(transformed_data())
    data <- transformed_data() %>%
      select(Sexe)  # Comptage des réservations par sexe
    
    
    nbF <- sum(data$Sexe=="Femme")
    nbH <- sum(data$Sexe=="Homme")
    ratio <- max(nbF, nbH) / min(nbF, nbH)
    paste("Le ratio est de :", round(ratio,2),".\nC'est à dire il y a ",
          round(ratio,2),ifelse(max(nbF, nbH)==nbH,"Hommes","Femmes"),"pour 1", ifelse(min(nbF, nbH)==nbH,"Homme.","Femme.") )
  })

  
  
  # 3. Nb de réservation par catégorie

  
  output$plot_par_categorie <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Groupe)
    
    total <- sum(data$n)  # Nombre total de réservations
    
    # if (input$affichage_par_categorie == "percentage") {
    #   data <- data %>%
    #     mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
    #     
    # }
    
    plot_ly(data, x = ~Groupe,y =~n ,type = 'bar', color = ~Groupe,
            text = ~n, 
            textposition = 'auto',
            textfont = list(size = 14, color = "black") ) %>%
      layout(
        title = input$title_par_categorie,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title =  "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  
  
  output$plot_par_categorie_perc <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Groupe)
    
    total <- sum(data$n)  # Nombre total de réservations
    
    
      data <- data %>%
        mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
      
    
    
    plot_ly(data, x = ~Groupe,y =~n ,type = 'bar', color = ~Groupe,
            text = ~n, 
            textposition = 'auto',
            textfont = list(size = 14, color = "black") ) %>%
      layout(
        title = input$title_par_categorie_perc,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  
  # 4. Nb de réservation par catégorie H et F

  # Fais le pourcentage sur l'ensemble des personnes 
  # output$plot_par_categorie_hf <- renderPlotly({
  #   req(transformed_data())
  #   
  #   # Comptage par catégorie et sexe
  #   data <- transformed_data() %>%
  #     count(Groupe, Sexe)
  #   
  #   total <- sum(data$n)  # Nombre total de réservations
  #   
  #   if (input$affichage_par_categorie_hf == "percentage") {
  #     data <- data %>%
  #       mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
  #   }
  #   
  #   # Définition des couleurs (rose pour Femme, bleu pour Homme)
  #   couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
  # 
  #       plot_ly(data, 
  #           x = ~Groupe, 
  #           y = ~n, 
  #           color = ~Sexe, 
  #           colors = couleurs_sexe,  # Appliquer les couleurs
  #           type = 'bar', 
  #           text = ~n,  
  #           textposition = 'auto', 
  #           textfont = list(size = 14, color = "black")) %>%
  #     layout(
  #       title = input$title_par_categorie_hf,
  #       xaxis = list(title = "Catégorie"),
  #       #yaxis = list(title = "Nombre de Réservations"),
  #       yaxis = list(title = ifelse(input$affichage_par_categorie_hf == "percentage", "Pourcentage (%)", "Nombre de Réservations")),
  #       showlegend = TRUE
  #     )
  # })
  
  output$plot_par_categorie_hf <- renderPlotly({
    req(transformed_data())
    
    # Comptage par catégorie et sexe
    data <- transformed_data() %>%
      count(Groupe, Sexe)
    
    # if (input$affichage_par_categorie_hf == "percentage") {
    #   # Calculer le total par catégorie
    #   total_par_categorie <- data %>%
    #     group_by(Groupe) %>%
    #     summarise(total = sum(n))
    #   
    #   # Calculer le pourcentage par catégorie
    #   data <- data %>%
    #     left_join(total_par_categorie, by = "Groupe") %>%
    #     mutate(n = round((n / total) * 100, 2)) %>%
    #     select(-total)
    # }
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data,
            x = ~Groupe,
            y = ~n,
            color = ~Sexe,
            colors = couleurs_sexe,  # Appliquer les couleurs
            type = 'bar',
            text = ~n,
            textposition = 'auto',
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_categorie_hf,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title =  "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  
  
  
  
  output$plot_par_categorie_hf_perc_grp <- renderPlotly({
    req(transformed_data())
    
    # Comptage par catégorie et sexe
    data <- transformed_data() %>%
      count(Groupe, Sexe)
    
    #if (input$affichage_par_categorie_hf == "percentage") {
      # Calculer le total par catégorie
      total_par_categorie <- data %>%
        group_by(Groupe) %>%
        summarise(total = sum(n))
      
      # Calculer le pourcentage par catégorie
      data <- data %>%
        left_join(total_par_categorie, by = "Groupe") %>%
        mutate(n = round((n / total) * 100, 2)) %>%
        select(-total)
    #}
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data,
            x = ~Groupe,
            y = ~n,
            color = ~Sexe,
            colors = couleurs_sexe,  # Appliquer les couleurs
            type = 'bar',
            text = ~n,
            textposition = 'auto',
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_categorie_hf_perc_grp,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_categorie_hf_perc_ens <- renderPlotly({
    req(transformed_data())
    
    # Comptage par catégorie et sexe
    data <- transformed_data() %>%
      count(Groupe, Sexe)
    
    total <- sum(data$n) 
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    #if (input$affichage_par_categorie_hf == "percentage") {
    # Calculer le total par catégorie
    # total_par_categorie <- data %>%
    #   group_by(Groupe) %>%
    #   summarise(total = sum(n))
    # 
    # # Calculer le pourcentage par catégorie
    # data <- data %>%
    #   left_join(total_par_categorie, by = "Groupe") %>%
    #   mutate(n = round((n / total) * 100, 2)) %>%
    #   select(-total)
    #}
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data,
            x = ~Groupe,
            y = ~n,
            color = ~Sexe,
            colors = couleurs_sexe,  # Appliquer les couleurs
            type = 'bar',
            text = ~n,
            textposition = 'auto',
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_categorie_hf_perc_ens,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  
  # 5. Nb de réservation par heures

  
  output$plot_par_heures <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires) # Comptage par heure
    
    total <- sum(data$n)  # Nombre total de réservations
    
    # if (input$affichage_par_heures == "percentage") {
    #   data <- data %>%
    #     mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
    #   
    # }
    
    plot_ly(data, x = ~Horaires, 
            y = ~n, type = 'bar', 
            text = ~n, 
            textposition = 'auto', 
            textfont = list(size = 14, color = "black"),
            marker = list(color = "lightblue")) %>%
      layout(
        title = input$title_par_heures,
        xaxis = list(title = "Heures"),
        #yaxis = list(title = "Nombre de Réservations"),
        yaxis = list(title =  "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_par_heures_perc <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires) # Comptage par heure
    
    total <- sum(data$n)  # Nombre total de réservations
    
    #if (input$affichage_par_heures == "percentage") {
      data <- data %>%
        mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
      
    #}
    
    plot_ly(data, x = ~Horaires, 
            y = ~n, type = 'bar', 
            text = ~n, 
            textposition = 'auto', 
            textfont = list(size = 14, color = "black"),
            marker = list(color = "lightblue")) %>%
      layout(
        title = input$title_par_heures_perc,
        xaxis = list(title = "Heures"),
        #yaxis = list(title = "Nombre de Réservations"),
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = FALSE
      )
  })
  
  
  # output$plot_par_heures <- renderPlotly({
  #   req(transformed_data())
  #   data <- transformed_data()
  #   
  #   # Vérifier et convertir l'heure si nécessaire
  #   data <- data %>%
  #     mutate(
  #       HeureBrute = substr(Horaires, 1, 5),  # Prend uniquement "hh:mm"
  #       Heure = as.numeric(substr(HeureBrute, 1, 2)),  # Extraire l'heure (hh)
  #       Minutes = as.numeric(substr(HeureBrute, 4, 5))  # Extraire les minutes (mm)
  #     )
  #   
  #   # Vérifier si trop de réservations tombent à XXh30
  #   heure_counts <- data %>%
  #     group_by(Heure, Minutes) %>%
  #     summarise(N = n(), .groups = "drop") %>%
  #     pivot_wider(names_from = Minutes, values_from = N, values_fill = list(N = 0))
  #   
  #   # Déterminer si XXh30 doit être séparé ou fusionné
  #   # heure_counts <- heure_counts %>%
  #   #   mutate(
  #   #     Total = 0 + 30,  # Total des réservations pour chaque heure
  #   #     Ratio30 = ifelse(Total > 0, 30 / Total, 0),  # Proportion des XXh30
  #   #     HeureFinale = ifelse(Ratio30 > 0.3, paste0(Heure, "h30"), paste0(Heure, "h"))
  #   #   )
  #   
  #   # Arrondir les horaires et ajuster les heures pour la visualisation
  #   heure_counts <- transformed_data() %>%
  #     mutate(
  #       # Extraire l'heure et les minutes pour chaque réservation
  #       HeureArrondie = format(as.POSIXct(Horaires, format = "%H:%M"), "%H"),  # Récupère juste l'heure (en format "HH")
  #       
  #       # Arrondir si nécessaire (par exemple, 17h15 devient 17h00)
  #       HeureArrondie = ifelse(format(as.POSIXct(Horaires, format = "%H:%M"), "%M") >= 30, 
  #                              paste0(HeureArrondie, "h30"), 
  #                              paste0(HeureArrondie, "h")),
  #       
  #       # Pour vérifier si les erreurs d'horaire sont en dehors des plages acceptées, on peut ajouter un filtre ici
  #       HeureValidée = ifelse(format(as.POSIXct(Horaires, format = "%H:%M"), "%H") %in% 0:23, 
  #                             HeureArrondie, NA)
  #     )
  #   
  #   
  #   # Appliquer cette correction aux données
  #   data <- data %>%
  #     left_join(select(heure_counts, Heure, HeureFinale), by = "Heure") %>%
  #     mutate(HeureFinale = factor(HeureFinale, levels = unique(heure_counts$HeureFinale)))
  #   
  #   # Créer le graphique
  #   # plot_ly(data, x = ~HeureFinale, type = "histogram", name = "Réservations par Heures") %>%
  #   #   layout(
  #   #     title = input$title_par_heures,
  #   #     xaxis = list(title = "Heures"),
  #   #     yaxis = list(title = "Nombre de Réservations"),
  #   #     showlegend = FALSE
  #   #   )
  #   
  #   plot_ly(heure_counts, x = ~HeureArrondie, type = 'histogram', name = "Réservations par Heure") %>%
  #     layout(
  #       title = input$title_par_heures,
  #       xaxis = list(title = "Heure", tickmode = "array", tickvals = unique(heure_counts$HeureArrondie)),
  #       yaxis = list(title = "Nombre de Réservations"),
  #       showlegend = TRUE
  #     )
  # })
  
  
  # 6. Nb de réservation par heures et catégorie

  
  output$plot_par_heures_categorie <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires, Groupe) # Comptage par heure et catégorie
    
    # if (input$affichage_par_heures_categorie == "percentage") {
    #   # Calculer le total par catégorie
    #   par_heures_categorie <- data %>%
    #     group_by(Groupe) %>%
    #     summarise(total = sum(n))
    #   
    #   # Calculer le pourcentage par catégorie
    #   data <- data %>%
    #     left_join(par_heures_categorie, by = "Groupe") %>%
    #     mutate(n = round((n / total) * 100, 2)) %>%
    #     select(-total)
    # }
    
    plot_ly(data, x = ~Horaires, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_heures_categorie,
        xaxis = list(title = "Heures"),
        yaxis = list(title =  "Nombre de Réservations"),
        #yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  
  output$plot_par_heures_categorie_perc <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires, Groupe) # Comptage par heure et catégorie
    
    #if (input$affichage_par_heures_categorie == "percentage") {
      # Calculer le total par catégorie
      par_heures_categorie <- data %>%
        group_by(Groupe) %>%
        summarise(total = sum(n))
      
      # Calculer le pourcentage par catégorie
      data <- data %>%
        left_join(par_heures_categorie, by = "Groupe") %>%
        mutate(n = round((n / total) * 100, 2)) %>%
        select(-total)
    #}
    
    plot_ly(data, x = ~Horaires, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_heures_categorie_perc,
        xaxis = list(title = "Heures"),
        yaxis = list(title =  "Pourcentage (%)"),
        #yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  # 7. Nb de réservation par jour
 
  output$plot_par_jour <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    # Convertir la colonne Jour en facteur avec un ordre spécifique
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      count(Jour) # Comptage par jour
    
    total <- sum(data$n)
    
 
    
    plot_ly(data, 
            x = ~Jour, 
            y = ~n,
            type = 'bar', 
            text = ~n, 
            
            marker = list(color = rainbow(7)), #lightblue
            textposition = 'auto',
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_jour,
        xaxis = list(title = "Jour"),
        yaxis = list(title =  "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  
  output$plot_par_jour_perc <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    # Convertir la colonne Jour en facteur avec un ordre spécifique
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      count(Jour) # Comptage par jour
    
    total <- sum(data$n)
    

      data <- data %>%
        mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
      
    
    
    plot_ly(data, 
            x = ~Jour, 
            y = ~n,
            type = 'bar', 
            text = ~n, 
            
            marker = list(color = rainbow(7)), #lightblue
            textposition = 'auto',
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_jour_perc,
        xaxis = list(title = "Jour"),
        #yaxis = list(title = "Nombre de Réservations"),
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = FALSE
      )
  })

  
  # 8. Nb de réservation par jour et catégorie


  output$plot_par_jour_categorie <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      count(Jour, Groupe) # Comptage par jour et catégorie
    
    plot_ly(data, x = ~Jour, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_jour_categorie,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_jour_categorie_scat <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    library(tidyr)  # Assure-toi que ce package est chargé
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      count(Jour, Groupe) %>%
      complete(Jour = factor(days_order, levels = days_order), Groupe, fill = list(n = 0))  # Remplir les combinaisons manquantes avec n = 0
    
    plot_ly(data, x = ~Jour, y = ~n, color = ~Groupe, type = 'scatter', mode = 'markers+lines', text = ~n, textposition = 'top center') %>%
      layout(
        title = input$title_par_jour_categorie_scat,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  
  
  output$plot_par_jour_categorie_perc <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      count(Jour, Groupe) # Comptage par jour et catégorie
    
    total <- sum(data$n)
    
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2)) # Convertir en pourcentage 
    
    plot_ly(data, x = ~Jour, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_jour_categorie_perc,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_jour_categorie_perc_grp <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    # Compter les occurrences par jour et groupe
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      count(Jour, Groupe)
    
    # Calculer le total par groupe
    total_par_groupe <- data %>%
      group_by(Groupe) %>%
      summarise(total = sum(n), .groups = 'drop')
    
    # Calcul du pourcentage au sein de chaque groupe
    data <- data %>%
      left_join(total_par_groupe, by = "Groupe") %>%
      mutate(n = round((n / total) * 100, 2)) %>%
      select(-total)
    
    # Graphique
    plot_ly(data, x = ~Jour, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_jour_categorie_perc_grp,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  


  
  # 9. Nb de réservations par jour selon le genre
  
  output$plot_par_jour_sexe <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Jour, Sexe)
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order))
    
    plot_ly(data, 
            x = ~Jour, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            type = 'bar', 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_jour_genre,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_jour_sexe_perc <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Jour, Sexe)
    
    total <- sum(data$n)
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order))
    
    plot_ly(data, 
            x = ~Jour, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            type = 'bar', 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_jour_genre_perc,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  # 10. Nb de réservations par heure selon le genre
  
  output$plot_par_heures_sexe <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Horaires, Sexe)
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            type = 'bar', 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_heure_genre,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_heures_sexe_perc <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Horaires, Sexe)
    
    total <- sum(data$n)
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            type = 'bar', 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_heure_genre_perc,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })

  # 11. Nb de personnes par catégorie 
  
  output$plot_personnes_par_categorie <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Groupe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Groupe)  # Compter le nombre de personnes uniques par catégorie
    
    plot_ly(data, 
            x = ~Groupe, 
            y = ~n, 
            type = 'bar', 
            color = ~Groupe,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_nb_groupe,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })
  
  output$plot_personnes_par_categorie_perc <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Groupe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Groupe)  # Compter le nombre de personnes uniques par catégorie
    
    total <- sum(data$n)
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2)) 
    
    plot_ly(data, 
            x = ~Groupe, 
            y = ~n, 
            type = 'bar', 
            color = ~Groupe,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_nb_groupe_perc,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })
  
  # 12. Nb de personnes par catégorie selon le genre
  
  output$plot_personnes_par_categorie_par_genre <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Groupe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Groupe, Sexe)  # Compter le nombre de personnes uniques par catégorie
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    
    plot_ly(data, 
            x = ~Groupe, 
            y = ~n, 
            type = 'bar', 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_nb_groupe_genre,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })  

  output$plot_personnes_par_categorie_par_genre_perc <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Groupe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Groupe, Sexe)  # Compter le nombre de personnes uniques par catégorie
    
    total <- sum(data$n)

    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    
    
    
    # total_par_categorie <- data %>%
    #   group_by(Groupe) %>%
    #   summarise(total = sum(n))
    # 
    # # Calculer le pourcentage par catégorie
    # data <- data %>%
    #   left_join(total_par_categorie, by = "Groupe") %>%
    #   mutate(n = round((n / total) * 100, 2)) %>%
    #   select(-total)
    
    
    
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    
    plot_ly(data, 
            x = ~Groupe, 
            y = ~n, 
            type = 'bar', 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_nb_groupe_genre_perc,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  }) 
  
  
  # 13. Nb de réservation par jour et par horaire 
  
  output$plot_horaires_par_jour_lundi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "lundi") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            #name = "Lundi",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Lundi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_horaires_par_jour_mardi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "mardi") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            name = "Mardi",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Mardi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_horaires_par_jour_mercredi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "mercredi") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            name = "Mercredi",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Mercredi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_horaires_par_jour_jeudi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "jeudi") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            name = "Jeudi",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Jeudi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_horaires_par_jour_vendredi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "vendredi") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            name = "Vendredi",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Vendredi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_horaires_par_jour_samedi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "samedi") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            name = "Samedi",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Samedi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  output$plot_horaires_par_jour_dimanche <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "dimanche") %>%
      count(Horaires)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            type = 'bar', 
            name = "Dimanche",
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title =  "Dimanche",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })
  
  
  
  # 14. Nb de réservation par jour et par horaire et par catégorie
  
  
  
  output$plot_horaires_par_jour_categorie_lundi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "lundi") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar', 
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Lundi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_categorie_mardi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "mardi") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Mardi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_categorie_mercredi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "mercredi") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Mercredi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_categorie_jeudi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "jeudi") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar', 
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Jeudi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_categorie_vendredi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "vendredi") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Vendredi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_categorie_samedi <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "samedi") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Samedi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_categorie_dimanche <- renderPlotly({
    req(transformed_data())
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "dimanche") %>%
      count(Horaires,Groupe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title =  "Dimanche",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  
  
  # 15. Nb de réservation par jour et par horaire et par genre
  
  
  output$plot_horaires_par_jour_genre_lundi <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "lundi") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar', 
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Lundi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_genre_mardi <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "mardi") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Mardi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_genre_mercredi <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "mercredi") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Mercredi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_genre_jeudi <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "jeudi") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar', 
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Jeudi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_genre_vendredi <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "vendredi") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Vendredi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_genre_samedi <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "samedi") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = "Samedi",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_horaires_par_jour_genre_dimanche <- renderPlotly({
    req(transformed_data())
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    data_lundi <- transformed_data() %>%
      filter(Jour == "dimanche") %>%
      count(Horaires,Sexe)  # Compter le nombre de réservations par horaire
    
    plot_ly(data_lundi, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar',
            text = ~n, textposition = 'outside') %>% 
      layout(
        title =  "Dimanche",
        xaxis = list(title = "Horaires", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
 
  
  
  # 16. Classement de joueurs ayant plus joué
  
  output$table_top_joueurs <- renderDataTable({
    req(transformed_data())
    
    data <- transformed_data()  %>% 
      count(joueurs,Groupe, sort = TRUE) # Compter le nombre de parties par joueur 
    
    colnames(data) <- c("Joueurs","Catégorie", "Nombre")
    
    DT::datatable(data, options = list(pageLength = 20, autoWidth = TRUE))
  }) 
  
    
} # Crochet de FIN