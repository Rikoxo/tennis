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
  
  output$preview_combined <- renderTable({
    req(combined_data())
    head(combined_data(),30)
  })
  

  
  
  
  # transformed_data <- reactive({
  #   req(combined_data())
  #   combined_data() %>%
  #     mutate(Date = as.Date(Date, format = "%d/%m/%Y"),
  #            Jour = weekdays(Date))
  # })
  
  
  transformed_data <- reactive({
    req(combined_data())
    combined_data() %>%
      mutate(
        Date = as.Date(Date, format = "%d/%m/%Y"),
        Jour = weekdays(Date)
      ) %>%
      filter(!is.na(Groupe)) # Garder uniquement les lignes avec des groupes valides
  })
  
  
  # 1. Nb de réservation tot
  
 
  
  output$plot_total <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    # Extraire la période de l'analyse
    date_min <- min(data$Date, na.rm = TRUE)
    date_max <- max(data$Date, na.rm = TRUE)
    
    # Générer le titre avec la période
    title_with_period <- paste("Réservations Totales du", format(date_min, "%d/%m/%Y"),
                               "au", format(date_max, "%d/%m/%Y"))
    
    # Regrouper les données par semaine et ajouter le mois
    data_weekly <- data %>%
      mutate(
        Semaine = format(Date, "%Y-%U"), # Année + numéro de semaine
        Mois = factor(format(Date, "%m"),
                      levels = sprintf("%02d", 1:12),
                      labels = c("Jan", "Fév", "Mar", "Avr", "Mai", "Juin", "Juil", "Août", "Sep", "Oct", "Nov", "Déc"))
      ) %>%
      count(Semaine, Mois)  # Compter les réservations par semaine et mois
    
    # Création du graphique avec valeurs sur les barres
    plot_ly(data_weekly, x = ~Semaine, y = ~n, color = ~Mois, type = "bar",
            text = ~n, textposition = 'outside') %>%
      layout(
        title = title_with_period,
        xaxis = list(title = "Semaine", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        bargap = 0.2, # Espacement des barres
        showlegend = TRUE
      )
  })
  
  
  
  

  
  # 2. Nb de réservation tot H et F

  
  
  output$plot_total_hf <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Sexe)  # Compter le nombre de réservations par genre
    
    plot_ly(
      data, 
      x = ~Sexe, 
      y = ~n, 
      type = 'bar', 
      color = ~Sexe,
      marker = list(color = c("pink", "skyblue")),  # Bleu pour H, Rose pour F
      text = ~n, 
      textposition = 'auto',
      textfont = list(size = 14, color = "black")  # Texte plus grand et noir
    ) %>%
      layout(
        title = input$title_total_hf,
        xaxis = list(title = "Genre"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  
  # 3. Nb de réservation par catégorie

  
  output$plot_par_categorie <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Groupe)
    
    plot_ly(data, x = ~Groupe,y =~n ,type = 'bar', color = ~Groupe,
            text = ~n, 
            textposition = 'auto',
            textfont = list(size = 14, color = "black") ) %>%
      layout(
        title = input$title_par_categorie,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  # 4. Nb de réservation par catégorie H et F

  
  output$plot_par_categorie_hf <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Groupe, Sexe) # Comptage par catégorie et sexe
    
    plot_ly(data, x = ~Groupe, 
            y = ~n, 
            color = ~Sexe, 
            type = 'bar', 
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_categorie_hf,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  # 5. Nb de réservation par heures

  
  output$plot_par_heures <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires) # Comptage par heure
    
    plot_ly(data, x = ~Horaires, 
            y = ~n, type = 'bar', 
            text = ~n, 
            textposition = 'auto', 
            textfont = list(size = 14, color = "black"),
            marker = list(color = "lightblue")) %>%
      layout(
        title = input$title_par_heures,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Nombre de Réservations"),
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
    
    plot_ly(data, x = ~Horaires, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_heures_categorie,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Nombre de Réservations"),
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
    
    plot_ly(data, x = ~Jour, y = ~n, type = 'bar', text = ~n, textposition = 'auto') %>%
      layout(
        title = input$title_par_jour,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Nombre de Réservations"),
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
  
}