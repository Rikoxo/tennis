# Auteur: Richard SELVARADJOU

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
  horaire_groups <- reactiveVal(list())
  inscrits_data <- reactiveVal(NULL)
  combined_data <- reactiveVal(NULL)
  grouped_tarifs <- reactiveVal(NULL)
  tarif_groups <- reactiveVal(list()) # Stockage des groupes de tarifs
  available_tarifs <- reactiveVal(NULL) # Tarifs disponibles (modifiable)
  applied_tarifs <- reactiveVal(data.frame(Groupe = character(), Tarifs = list(), stringsAsFactors = FALSE))  
  titre_modifie_manuellement <- reactiveVal(FALSE)
  
  observeEvent(input$title_horaire_jour_cat, {
    # Si l'utilisateur modifie le titre manuellement, on le note
    titre_modifie_manuellement(TRUE)
  })
  
  
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
      df <- read.csv(file, header = TRUE, sep = input$sep_colonne, fileEncoding = "ISO-8859-13") #probleme sep="," ou \t
      
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
  
  observeEvent(input$reset_tarif_groupings, {
    tarif_groups(list())  # Réinitialise les regroupements
    showNotification("Les regroupements de tarifs ont été réinitialisés.", type = "message")
  })
  

  
  # Aperçu des données combinées
  output$preview_reservations <- renderTable({
    req(reservations_data())
    head(reservations_data())
  })
  
  output$preview_inscrits <- renderTable({
    req(inscrits_data())
    head(inscrits_data())
  })
  

  
  transformed_data <- reactive({
    req(combined_data())
    
    df <- combined_data() %>%
      mutate(
        Date = as.Date(Date, format = "%d/%m/%Y"),
        Jour = weekdays(Date)
      ) %>%
      filter(!is.na(Groupe)) %>%
      distinct(Date, Horaires, joueurs, .keep_all = TRUE)  # Suppression des doublons
    
    # Appliquer les regroupements si activé
    if (isTRUE(input$activate_grouping) &&
        !is.null(horaire_groups()) &&
        length(horaire_groups()) > 0) {
      
      regroupements <- horaire_groups()
      
      for (nom_nouveau in names(regroupements)) {
        df$Horaires[df$Horaires %in% regroupements[[nom_nouveau]]] <- nom_nouveau
      }
      
      
      
      original_order <- sort(unique(combined_data()$Horaires))
      
      # Étape 2 : Insérer les nouveaux horaires regroupés à la bonne position
      regroupements <- horaire_groups()
      
      # On construit le nouvel ordre manuellement
      new_order <- original_order
      
      for (nom_nouveau in names(regroupements)) {
        # Retirer les anciens horaires fusionnés
        new_order <- setdiff(new_order, regroupements[[nom_nouveau]])
        # Ajouter le nom du groupe à la 1ère position du bloc regroupé
        index_pos <- which(original_order %in% regroupements[[nom_nouveau]])[1]
        if (!is.na(index_pos)) {
          new_order <- append(new_order, nom_nouveau, after = index_pos - 1)
        }
      }
      
      # Étape 3 : Appliquer ce nouvel ordre comme facteur
      df$Horaires <- factor(df$Horaires, levels = new_order)
      
      
      
      # Optionnel : forcer un facteur propre avec un ordre
      #df$Horaires <- factor(df$Horaires, levels = unique(df$Horaires))
    }
    
    return(df)
  })
  

  
  output$preview_combined <- renderTable({
    req(transformed_data())
    head(transformed_data(),30)
  })

  
  # 1. Nb de réservation total par semaine
  
  
  output$title_with_period <- renderText({
    req(transformed_data())
    data <- transformed_data()
    
    # Extraire la période de l'analyse
    date_min <- min(data$Date, na.rm = TRUE)
    date_max <- max(data$Date, na.rm = TRUE)
    
    # Titre avec période
    paste("Nombre de Réservations par Semaine du", format(date_min, "%d/%m/%Y"),
                               "au", format(date_max, "%d/%m/%Y"))
    
  }) # Pour le titre (avoir le debut et la fin des semaines)
  
  
  output$plot_total <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()


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
        title = input$title_total,
        xaxis = list(title = "Semaine", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        bargap = 0.2,
        showlegend = TRUE
      )
  })
  
  
  output$plot_total_categorie <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    
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
      count(WeekStart, Groupe) %>%
      left_join(week_month, by = "WeekStart") %>%
   
      
      mutate(
        Mois = factor(Mois, levels = sprintf("%02d", 1:12),
                      labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                 "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")),
        WeekStart = as.Date(WeekStart),
        NumSemaine = strftime(WeekStart,  format( "%Y-%U"))  # Numéro de semaine ISO
      )  %>%
      mutate(Mois = factor(Mois, levels = unique(Mois))) %>%  # <- Ordre dynamique
      complete(Mois = factor(Mois, levels = unique(Mois)), Groupe, fill = list(n = 0)) 
    
    
    # Graphique
    plot_ly(data_weekly, x = ~NumSemaine, y = ~n, color = ~Groupe, type = "scatter", mode = 'markers+lines',
            text = ~n, textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_total_categorie,
        xaxis = list(title = "Semaine", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })

  
  output$plot_total_genre <- renderPlotly({
    req(transformed_data())
    data <- transformed_data()
    
    
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
      count(WeekStart, Sexe) %>%
      left_join(week_month, by = "WeekStart") %>%
      
     
      mutate(
        Mois = factor(Mois, levels = sprintf("%02d", 1:12),
                      labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", 
                                 "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")),
        WeekStart = as.Date(WeekStart),
        NumSemaine = strftime(WeekStart,  format( "%Y-%U"))  # Numéro de semaine ISO
      )  %>%
      mutate(Mois = factor(Mois, levels = unique(Mois))) %>% # <- Ordre dynamique
      complete(Mois = factor(Mois, levels = unique(Mois)), Sexe, fill = list(n = 0))  
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    # Graphique
    plot_ly(data_weekly, x = ~NumSemaine, y = ~n, color = ~Sexe, colors = couleurs_sexe, type = "scatter", mode = 'markers+lines',
            text = ~n, textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_total_genre,
        xaxis = list(title = "Semaine", tickangle = -45),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  # 2. Nb de réservation tot H et F

  
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
        title = input$title_total_hf_perc,
        xaxis = list(title = "Genre"),
        #yaxis = list(title = ifelse(input$affichage_total_hf == "percentage", "Pourcentage (%)", "Nombre de Réservations")),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })


  output$ratio <- renderText({
    req(transformed_data())
    data <- transformed_data() %>%
      select(Sexe)  # Comptage des réservations par sexe
    
    
    nbF <- sum(data$Sexe=="Femme")
    nbH <- sum(data$Sexe=="Homme")
    ratio <- max(nbF, nbH) / min(nbF, nbH)
    paste("Le ratio est de :", round(ratio,2),".\nC'est à dire, qu'il y a ",
          round(ratio,2),ifelse(max(nbF, nbH)==nbH,"réservations effectuées par les hommes pour une réservation effectuée par une femme.",
                                "réservation effectuées par les femmes pour une réservation effectué par un homme.") )
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

  
  output$plot_par_categorie_hf <- renderPlotly({
    req(transformed_data())
    
    # Comptage par catégorie et sexe
    data <- transformed_data() %>%
      count(Groupe, Sexe)

    
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
  
  
  output$plot_par_categorie_hf_perc_ens <- renderPlotly({
    req(transformed_data())
    
    # Comptage par catégorie et sexe
    data <- transformed_data() %>%
      count(Groupe, Sexe)
    
    total <- sum(data$n) 
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    
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
        yaxis = list(title = "Pourcentage (%)"),
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
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = FALSE
      )
  })
  
  
  
  
  # 6. Nb de réservation par heures et catégorie

  
  output$plot_par_heures_categorie <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires, Groupe) # Comptage par heure et catégorie
    
    
    plot_ly(data, x = ~Horaires, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_heures_categorie,
        xaxis = list(title = "Heures"),
        yaxis = list(title =  "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  output$plot_par_heures_categorie_scat <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires, Groupe) # Comptage par heure et catégorie
    
    hours_order <- sort(unique(data$Horaires))
    
    data <- data %>%
      mutate(Horaires = factor(Horaires, levels = hours_order)) %>%
      complete(Horaires = factor(hours_order, levels = hours_order), Groupe, fill = list(n = 0))
    
    
    plot_ly(data, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Groupe, 
            type = 'scatter', 
            mode = 'markers+lines', 
            text = ~n, 
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_heures_categorie_scat,
        xaxis = list(title = "Heures"),
        yaxis = list(title =  "Nombre de Réservations"),
        
        showlegend = TRUE
      )
  })
  
  
  output$plot_par_heures_categorie_perc_grp <- renderPlotly({
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
        title = input$title_par_heures_categorie_perc_grp,
        xaxis = list(title = "Heures"),
        yaxis = list(title =  "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  
  output$plot_par_heures_categorie_perc_ens <- renderPlotly({
    req(transformed_data())
    data <- transformed_data() %>%
      count(Horaires, Groupe) # Comptage par heure et catégorie
    
    #if (input$affichage_par_heures_categorie == "percentage") {
    # Calculer le total par catégorie
    # par_heures_categorie <- data %>%
    #   group_by(Groupe) %>%
    #   summarise(total = sum(n))
    # 
    # # Calculer le pourcentage par catégorie
    # data <- data %>%
    #   left_join(par_heures_categorie, by = "Groupe") %>%
    #   mutate(n = round((n / total) * 100, 2)) %>%
    #   select(-total)
    #}
    
    total <- sum(data$n)  # Nombre total de réservations
    
    #if (input$affichage_par_heures == "percentage") {
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    plot_ly(data, x = ~Horaires, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_heures_categorie_perc_ens,
        xaxis = list(title = "Heures"),
        yaxis = list(title =  "Pourcentage (%)"),
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
    
    plot_ly(data, x = ~Jour, y = ~n, color = ~Groupe, type = 'scatter', mode = 'markers+lines', text = ~n, textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_jour_categorie_scat,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  output$plot_par_jour_categorie_perc_ens <- renderPlotly({
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
        title = input$title_par_jour_categorie_perc_ens,
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
  
  
  output$plot_par_jour_categorie_perc_jour <- renderPlotly({
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
      group_by(Jour) %>%
      summarise(total = sum(n), .groups = 'drop')
    
    # Calcul du pourcentage au sein de chaque groupe
    data <- data %>%
      left_join(total_par_groupe, by = "Jour") %>%
      mutate(n = round((n / total) * 100, 2)) %>%
      select(-total)
    
    # Graphique
    plot_ly(data, x = ~Jour, y = ~n, color = ~Groupe, type = 'bar', text = ~n, textposition = 'outside') %>%
      layout(
        title = input$title_par_jour_categorie_perc_jour,
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
  
  output$plot_par_jour_sexe_scat <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Jour, Sexe)
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    # Définir l'ordre des jours de la semaine
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data <- data %>%
      mutate(Jour = factor(Jour, levels = days_order)) %>%
      complete(Jour = factor(days_order, levels = days_order), Sexe, fill = list(n = 0))
    
    plot_ly(data, 
            x = ~Jour, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            type = 'scatter', 
            mode = 'markers+lines',
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_jour_genre_scat,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_jour_sexe_perc_ens <- renderPlotly({
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
        title = input$title_par_jour_genre_perc_ens,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_jour_sexe_perc_grp <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Jour, Sexe)
    
    # par_heures_categorie <- data %>%
    #   group_by(Groupe) %>%
    #   summarise(total = sum(n))
    # 
    # # Calculer le pourcentage par catégorie
    # data <- data %>%
    #   left_join(par_heures_categorie, by = "Groupe") %>%
    #   mutate(n = round((n / total) * 100, 2)) %>%
    #   select(-total)
    # 
    
    total_par_groupe <- data %>%
      group_by(Sexe) %>%
      summarise(total = sum(n), .groups = 'drop')
    
    # Calcul du pourcentage au sein de chaque groupe
    data <- data %>%
      left_join(total_par_groupe, by = "Sexe") %>%
      mutate(n = round((n / total) * 100, 2)) %>%
      select(-total)
    
    # total <- sum(data$n)
    # 
    # data <- data %>%
    #   mutate(n = round((n / total) * 100, 2))
    
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
        title = input$title_par_jour_genre_perc_grp,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_jour_sexe_perc_jour <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Jour, Sexe)
    
    # par_heures_categorie <- data %>%
    #   group_by(Groupe) %>%
    #   summarise(total = sum(n))
    # 
    # # Calculer le pourcentage par catégorie
    # data <- data %>%
    #   left_join(par_heures_categorie, by = "Groupe") %>%
    #   mutate(n = round((n / total) * 100, 2)) %>%
    #   select(-total)
    # 
    
    total_par_groupe <- data %>%
      group_by(Jour) %>%
      summarise(total = sum(n), .groups = 'drop')
    
    # Calcul du pourcentage au sein de chaque groupe
    data <- data %>%
      left_join(total_par_groupe, by = "Jour") %>%
      mutate(n = round((n / total) * 100, 2)) %>%
      select(-total)
    
    # total <- sum(data$n)
    # 
    # data <- data %>%
    #   mutate(n = round((n / total) * 100, 2))
    
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
        title = input$title_par_jour_genre_perc_jour,
        xaxis = list(title = "Jour"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  # 10. Nb de réservations par heure selon le genre
  
  # output$plot_par_heures_sexe <- renderPlotly({
  #   req(transformed_data())
  #   
  #   data <- transformed_data() %>%
  #     count(Horaires, Sexe)
  #   
  #   hours_order <- sort(unique(data$Horaires))
  #   
  #   data <- data %>%
  #     mutate(Horaires = factor(Horaires, levels = hours_order)) %>%
  #     complete(Horaires = factor(hours_order, levels = hours_order), Sexe, fill = list(n = 0))
  #   
  #   # Définition des couleurs (rose pour Femme, bleu pour Homme)
  #   couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
  #   
  #   plot_ly(data, 
  #           x = ~Horaires, 
  #           y = ~n, 
  #           color = ~Sexe, 
  #           colors = couleurs_sexe, 
  #           type = 'bar', 
  #           text = ~n,  
  #           textposition = 'auto', 
  #           textfont = list(size = 14, color = "black")) %>%
  #     layout(
  #       title = input$title_par_heure_sexe,
  #       xaxis = list(title = "Heures"),
  #       yaxis = list(title = "Nombre de Réservations"),
  #       showlegend = TRUE
  #     )
  # })
  
  
  output$plot_par_heures_sexe <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Horaires, Sexe)
    
    # Construire l'ordre des horaires selon l'état du regroupement
    if (isTRUE(input$activate_grouping) && !is.null(input$selected_horaires)) {
      # Utiliser l'ordre des regroupements créés par l'utilisateur
      regroupements <- input$selected_horaires
      regrouped_labels <- names(regroupements)

      # Ajouter les horaires non regroupés à la fin (ordre alphabétique)
      autres_horaires <- setdiff(unique(data$Horaires), regrouped_labels)
      hours_order <- sort(c(regrouped_labels, (autres_horaires)))  #c(regrouped_labels, sort(autres_horaires))
    } else {
      # Pas de regroupement : ordre alphabétique classique
      hours_order <- sort(unique(data$Horaires))
    }
    
    data <- data %>%
      mutate(Horaires = factor(Horaires, levels = hours_order)) %>%
      complete(Horaires = factor(hours_order, levels = hours_order), Sexe, fill = list(n = 0))
    
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
        title = input$title_par_heure_sexe,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_heures_sexe_scat <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Horaires, Sexe) 
    
    # Construire l'ordre des horaires selon l'état du regroupement
    if (isTRUE(input$activate_grouping) && !is.null(input$selected_horaires)) {
      # Utiliser l'ordre des regroupements créés par l'utilisateur
      regroupements <- input$selected_horaires
      regrouped_labels <- names(regroupements)
      
      # Ajouter les horaires non regroupés à la fin (ordre alphabétique)
      autres_horaires <- setdiff(unique(data$Horaires), regrouped_labels)
      hours_order <- c(regrouped_labels, sort(autres_horaires))
    } else {
      # Pas de regroupement : ordre alphabétique classique
      hours_order <- sort(unique(data$Horaires))
    }
    
    data <- data %>%
      mutate(Horaires = factor(Horaires, levels = hours_order)) %>%
      complete(Horaires = factor(hours_order, levels = hours_order), Sexe, fill = list(n = 0))
    
    
    
    # Définition des couleurs (rose pour Femme, bleu pour Homme)
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data, 
            x = ~Horaires, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe, 
            type = 'scatter', 
            mode = 'markers+lines',
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_heures_sexe_scat,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  output$plot_par_heures_sexe_perc_ens <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Horaires, Sexe)
    
    total <- sum(data$n)
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    # Construire l'ordre des horaires selon l'état du regroupement
    if (isTRUE(input$activate_grouping) && !is.null(input$selected_horaires)) {
      # Utiliser l'ordre des regroupements créés par l'utilisateur
      regroupements <- input$selected_horaires
      regrouped_labels <- names(regroupements)
      
      # Ajouter les horaires non regroupés à la fin (ordre alphabétique)
      autres_horaires <- setdiff(unique(data$Horaires), regrouped_labels)
      hours_order <- c(regrouped_labels, sort(autres_horaires))
    } else {
      # Pas de regroupement : ordre alphabétique classique
      hours_order <- sort(unique(data$Horaires))
    }
    
    data <- data %>%
      mutate(Horaires = factor(Horaires, levels = hours_order)) %>%
      complete(Horaires = factor(hours_order, levels = hours_order), Sexe, fill = list(n = 0))
    
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
        title = input$title_par_heure_sexe_perc_ens,
        xaxis = list(title = "Heures"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })

  output$plot_par_heures_sexe_perc_grp <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      count(Horaires, Sexe)
    
    total <- sum(data$n)
    
    # data <- data %>%
    #   mutate(n = round((n / total) * 100, 2))
    
    total_par_categorie <- data %>%
      group_by(Sexe) %>%
      summarise(total = sum(n))
    
    # Calculer le pourcentage par catégorie
    data <- data %>%
      left_join(total_par_categorie, by = "Sexe") %>%
      mutate(n = round((n / total) * 100, 2)) %>%
      select(-total)
    
    
    # Construire l'ordre des horaires selon l'état du regroupement
    if (isTRUE(input$activate_grouping) && !is.null(input$selected_horaires)) {
      # Utiliser l'ordre des regroupements créés par l'utilisateur
      regroupements <- input$selected_horaires
      regrouped_labels <- names(regroupements)
      
      # Ajouter les horaires non regroupés à la fin (ordre alphabétique)
      autres_horaires <- setdiff(unique(data$Horaires), regrouped_labels)
      hours_order <- c(regrouped_labels, sort(autres_horaires))
    } else {
      # Pas de regroupement : ordre alphabétique classique
      hours_order <- sort(unique(data$Horaires))
    }
    
    data <- data %>%
      mutate(Horaires = factor(Horaires, levels = hours_order)) %>%
      complete(Horaires = factor(hours_order, levels = hours_order), Sexe, fill = list(n = 0))
    
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
        title = input$title_par_heures_sexe_perc_grp,
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
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  output$plot_personnes_par_genre <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Sexe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Sexe)  # Compter le nombre de personnes uniques par catégorie
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data, 
            x = ~Sexe, 
            y = ~n, 
            type = 'bar', 
            color = ~Sexe, 
            colors = couleurs_sexe,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_nb_genre,
        xaxis = list(title = "Genre"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })
  
  output$plot_personnes_par_genre_perc <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Sexe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Sexe)  # Compter le nombre de personnes uniques par catégorie
    
    total <- sum(data$n)
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2)) 
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data, 
            x = ~Sexe, 
            y = ~n, 
            type = 'bar', 
            color = ~Sexe, 
            colors = couleurs_sexe,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_par_nb_genre_perc,
        xaxis = list(title = "Genre"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  
  output$ratio2 <- renderText({
    req(transformed_data())
    data <- transformed_data() %>%
      distinct(joueurs, Sexe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      select(Sexe)  # Comptage de nombre de personnes par sexe


    nbF <- sum(data$Sexe=="Femme")
    nbH <- sum(data$Sexe=="Homme")
    ratio <- max(nbF, nbH) / min(nbF, nbH)
    paste("Le ratio est de :", round(ratio,2),".\nC'est à dire, qu'il y a ",
          round(ratio,2),ifelse(max(nbF, nbH)==nbH,"hommes pour une femme.",
                                "femmes pour un homme.") )
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

  output$plot_personnes_par_categorie_par_genre_perc_ens <- renderPlotly({
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
        title = input$title_par_nb_groupe_genre_perc_ens,
        xaxis = list(title = "Catégorie"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  }) 
  
  output$plot_personnes_par_categorie_par_genre_perc_grp <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Groupe, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Groupe, Sexe)  # Compter le nombre de personnes uniques par catégorie
    
    total <- sum(data$n)
    
    # data <- data %>%
    #   mutate(n = round((n / total) * 100, 2))
    
    total_par_categorie <- data %>%
      group_by(Groupe) %>%
      summarise(total = sum(n))

    # Calculer le pourcentage par catégorie
    data <- data %>%
      left_join(total_par_categorie, by = "Groupe") %>%
      mutate(n = round((n / total) * 100, 2)) %>%
      select(-total)
    
    
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
        title = input$title_par_nb_groupe_genre_perc_grp,
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
        title = input$title_horaires_par_jour_lundi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_mardi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_mercredi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_jeudi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_vendredi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_samedi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_dimanche,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_lundi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_mardi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_mercredi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_jeudi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_vendredi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_samedi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_categorie_dimanche,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_lundi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_mardi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_mercredi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_jeudi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_vendredi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_samedi,
        xaxis = list(title = "Horaires"),
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
        title = input$title_horaires_par_jour_genre_dimanche,
        xaxis = list(title = "Horaires"), #, tickangle = -45
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  

  # 17. Horaire et Jour
  
  #Pas effacer demander si preference en 1 colonne et en 2 colonnes
  
  # output$select_hours_ui <- renderUI({
  #   req(transformed_data()) 
  #   
  #   hours_choices <- transformed_data() %>%
  #     pull(Horaires) %>%
  #     unique() %>%
  #     sort()
  #   
  #   radioButtons("choice_hours", "Choix horaires :", choices = hours_choices)
  # })
  # 
  
  
  output$select_hours_ui <- renderUI({
    req(transformed_data())
    
    hours_choices <- transformed_data() %>%
      pull(Horaires) %>%
      unique() %>%
      sort()
    
    # Diviser les choix en deux colonnes
    half <- ceiling(length(hours_choices) / 2)
    col1 <- hours_choices[1:half]
    col2 <- hours_choices[(half + 1):length(hours_choices)]
    
    # Créer deux listes de radioButtons
    radio_col1 <- radioButtons("choice_hours", "Choix horaires :", choices = col1, inline = FALSE)
    radio_col2 <- radioButtons("choice_hours", NULL, choices = col2, inline = FALSE, selected =col1[1])
    
    # Utiliser div pour organiser en deux colonnes
    div(
      style = "display: flex;",
      div(style = "flex: 1;", radio_col1),
      div(style = "flex: 1;", radio_col2)
    )
  })
  
  
  output$plot_horaire_jour <- renderPlotly({
    req(transformed_data())
    
    
    
    data_test <- transformed_data() %>%
      filter(Horaires == input$choice_hours) %>% 
      count(Jour)  
    
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data_test <- data_test %>%
      mutate(Jour = factor(Jour, levels = days_order))
    
    plot_ly(data_test, 
            x = ~Jour, 
            y = ~n, 
            type = 'bar', 
            marker = list(color = 'lightblue'),
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = paste0("Nombre de réservations pour l'horaire: ",input$choice_hours,"."),
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = FALSE
      )
  })

  
  output$plot_horaire_jour_cat <- renderPlotly({
    req(transformed_data())
    
    
    
    data_test <- transformed_data() %>%
      filter(Horaires == input$choice_hours) %>% 
      count(Jour,Groupe)  
    
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data_test <- data_test %>%
      mutate(Jour = factor(Jour, levels = days_order))
    
    plot_ly(data_test, 
            x = ~Jour, 
            y = ~n, 
            color = ~Groupe,
            type = 'bar', 
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = paste0("Nombre de réservations pour l'horaire: ",input$choice_hours," en fonction des catégories."),
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })
  
  
  output$plot_horaire_jour_genre <- renderPlotly({
    req(transformed_data())
    
    
    
    data_test <- transformed_data() %>%
      filter(Horaires == input$choice_hours) %>% 
      count(Jour,Sexe)  
    
    days_order <- c("lundi", "mardi", "mercredi", "jeudi", "vendredi", "samedi", "dimanche")
    
    data_test <- data_test %>%
      mutate(Jour = factor(Jour, levels = days_order))
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data_test, 
            x = ~Jour, 
            y = ~n, 
            color = ~Sexe, 
            colors = couleurs_sexe,
            type = 'bar', 
            text = ~n, textposition = 'outside') %>% 
      layout(
        title = paste0("Nombre de réservations pour l'horaire: ",input$choice_hours," en fonction du genre."),
        xaxis = list(title = "Jours"),
        yaxis = list(title = "Nombre de Réservations"),
        showlegend = TRUE
      )
  })

  

  # Nouveaux adhérents
  
  output$plot_nv_adh <- renderPlotly({
    req(inscrits_data())
    
    data <- inscrits_data() %>%
      distinct(Nom.Prénom	, Nouvel.Adhérent, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Nouvel.Adhérent)  # Compter le nombre de personnes uniques par catégorie
    
    couleurs_nv_adh <- c("Oui" = "lightgreen", "Non" = "red")
    
    plot_ly(data, 
            x = ~Nouvel.Adhérent, 
            y = ~n, 
            type = 'bar', 
            color = ~Nouvel.Adhérent,
            colors = couleurs_nv_adh,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_nv_adh,
        xaxis = list(title = "Nouveau Adhérent"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })
  
  
  output$plot_nv_adh_perc <- renderPlotly({
    req(inscrits_data())
    
    data <- inscrits_data() %>%
      distinct(Nom.Prénom	, Nouvel.Adhérent, .keep_all = TRUE) %>%  # Garder une seule occurrence par joueur
      count(Nouvel.Adhérent)  # Compter le nombre de personnes uniques par catégorie
    
    
    total <- sum(data$n)
    
    data <- data %>%
      mutate(n = round((n / total) * 100, 2))
    
    couleurs_nv_adh <- c("Oui" = "lightgreen", "Non" = "red")
    
    plot_ly(data, 
            x = ~Nouvel.Adhérent, 
            y = ~n, 
            type = 'bar', 
            color = ~Nouvel.Adhérent,
            colors = couleurs_nv_adh,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_nv_adh_perc,
        xaxis = list(title = "Nouveau Adhérent"),
        yaxis = list(title = "Pourcentage (%)"),
        showlegend = TRUE
      )
  })
  

  output$plot_nv_adh_sexe <- renderPlotly({
    req(inscrits_data())
    
    data <- inscrits_data() %>%
      distinct(Nom.Prénom	, Nouvel.Adhérent, .keep_all = TRUE) %>% 
      count(Nouvel.Adhérent,Sexe)  
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    plot_ly(data, 
            x = ~Nouvel.Adhérent, 
            y = ~n, 
            type = 'bar', 
            color = ~Sexe, 
            colors = couleurs_sexe,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_nv_adh_sexe,
        xaxis = list(title = "Nouveau Adhérent"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })
  
  
  output$plot_nv_adh_cat <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs	, Nouvel.Adhérent	, .keep_all = TRUE) %>%  
      count(Nouvel.Adhérent	,Groupe) 
    
    couleurs_nv_adh <- c("Oui" = "lightgreen", "Non" = "red")
    
    plot_ly(data, 
            x = ~Groupe	, 
            y = ~n, 
            type = 'bar', 
            color = ~Nouvel.Adhérent,
            colors = couleurs_nv_adh,
            text = ~n,  
            textposition = 'auto', 
            textfont = list(size = 14, color = "black")) %>%
      layout(
        title = input$title_nv_adh_cat,
        xaxis = list(title = "Nouveau Adhérent"),
        yaxis = list(title = "Nombre de Personnes"),
        showlegend = TRUE
      )
  })

  
  
  output$plot_nv_adh_cat_sexe <- renderPlotly({
    req(transformed_data())
    
    data <- transformed_data() %>%
      distinct(joueurs, Nouvel.Adhérent, .keep_all = TRUE) %>%
      count(Nouvel.Adhérent, Groupe, Sexe)
    
    couleurs_sexe <- c("Femme" = "pink", "Homme" = "skyblue")
    
    # Concaténer Groupe et Nouvel.Adhérent pour l’axe X
    data <- data %>%
      mutate(
        x_label = paste(Groupe, Nouvel.Adhérent, sep = " - ")
      )
    
    plot_ly(
      data,
      x = ~x_label,
      y = ~n,
      type = 'bar',
      color = ~Sexe,
      colors = couleurs_sexe,
      text = ~n,
      textposition = 'auto',
      textfont = list(size = 14, color = "black")
    ) %>%
      layout(
        title = input$title_nv_adh_cat_sexe,
        xaxis = list(title = "Catégorie - Nouveau Adhérent", tickangle = -45),
        yaxis = list(title = "Nombre de Personnes"),
        barmode = "group",
        showlegend = TRUE
      )
  })
  

  
  # 16. Classement de joueurs ayant plus joué
  
  output$table_top_joueurs <- renderDataTable({
    req(transformed_data())
    
    data <- transformed_data()  %>% 
      count(joueurs,Groupe,Sexe, sort = TRUE) # Compter le nombre de parties par joueur 
    
    colnames(data) <- c("Joueurs","Catégorie","Genre", "Nombre")
    
    DT::datatable(data, options = list(pageLength = 20, autoWidth = TRUE))
  }) 
  
  
  
  # Regroupement horaires
  
  output$horaire_grouping_ui <- renderUI({
    req(transformed_data())
    horaires_dispo <- sort(unique(transformed_data()$Horaires))
    
    tagList(
      helpText(HTML("Créer des regroupements d'horaires. Exemple : 08:00 - 09:00 et 09:00 - 10:00 → 08:00 - 10:00.
                    <br> Si vous devez effectuer plusieurs regroupements, traitez d'abord ceux avec les horaires les plus tardifs, puis passez aux horaires les moins avancés. " )),
      selectizeInput("selected_horaires", "Choisir les horaires à regrouper :",
                     choices = horaires_dispo, multiple = TRUE),
      textInput("new_horaire_label", "Nom du nouvel horaire regroupé :", ""),
      actionButton("add_horaire_group", "Ajouter ce regroupement"),
      br(), br(), 
      uiOutput("defined_groups")

      #verbatimTextOutput("defined_groups")
    )
  })
  
  
  observeEvent(input$add_horaire_group, {
    req(input$selected_horaires, input$new_horaire_label)
    
    current <- horaire_groups()
    current[[input$new_horaire_label]] <- input$selected_horaires
    horaire_groups(current)
  })
  
  
  # output$defined_groups <- renderPrint({
  #   req(horaire_groups())
  #   horaire_groups()
  # })
  # 
  
  # output$defined_groups <- renderPrint({
  #   req(horaire_groups())
  #   cat("Groupes d'horaires définis :\n\n")
  #   lapply(names(horaire_groups()), function(nom_groupe) {
  #     cat(paste0("➤ ", nom_groupe, ": ", paste(horaire_groups()[[nom_groupe]], collapse = ", "), "\n"))
  #   })
  # })

  
  output$defined_groups <- renderUI({
    req(horaire_groups())
    tags$div(
      tags$h4("Groupes d'horaires redéfinis :"),
      tags$ul(
        lapply(names(horaire_groups()), function(nom) {
          tags$li(
            tags$b(nom), " : ",
            paste(horaire_groups()[[nom]], collapse = ", ")
          )
        })
      )
    )
  })
  
  
    
} # Crochet de FIN
