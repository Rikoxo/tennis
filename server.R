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

server <- function(input, output, session) {
  # Réactifs pour stocker les données
  reservations_data <- reactiveVal(list())
  inscrits_data <- reactiveVal(NULL)
  combined_data <- reactiveVal(NULL)
  grouped_tarifs <- reactiveVal(NULL)
  tarif_groups <- reactiveVal(list()) # Stockage des groupes de tarifs
  
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
  })
  
  # Dynamique : Sélection des tarifs
  output$tarif_selector <- renderUI({
    req(combined_data())
    tarifs <- unique(combined_data()$`Tarif.attribué`)
    checkboxGroupInput("tarif_choices", "Choisissez les tarifs à regrouper :", choices = tarifs, selected = NULL)
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
  
  # Aperçu des regroupements
  output$preview_tarifs <- renderTable({
    req(tarif_groups())
    
    # Créer un tableau qui affiche les groupes et les tarifs associés
    tarif_groups() %>%
      purrr::map_df(~data.frame(Groupe = .x$group_name, Tarifs = paste(.x$tarifs, collapse = ",  ")))
  })
  
  # Appliquer les regroupements de tarifs
  observeEvent(input$apply_tarif_settings, {
    req(combined_data(), tarif_groups())
    
    grouped <- combined_data()
    for (group in tarif_groups()) {
      grouped <- grouped %>%
        mutate(`Tarif.attribué` = ifelse(`Tarif.attribué` %in% group$tarifs, group$group_name, `Tarif.attribué`))
    }
    
    grouped_tarifs(grouped)
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
  
  output$preview_combined <- renderTable({
    req(combined_data())
    head(combined_data())
  })
}
