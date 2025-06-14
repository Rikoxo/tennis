#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(skin="yellow",
  dashboardHeader(title = "Analyse Tennis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement des fichiers", tabName = "upload", icon = icon("file-upload")),
      menuItem("Gestion des Tarifs", tabName = "tarifs", icon = icon("tags")),
      menuItem("Analyses", tabName = "analyses", icon = icon("chart-bar"),
               menuSubItem(HTML("Nb de réservation par semaine"), tabName = "total"),
               
               menuSubItem("Nb de réservation par catégorie", tabName = "par_categorie"),
               menuSubItem(HTML("Nb de réservation<br>par genre"), tabName = "total_hf"),
               menuSubItem(HTML("Nb de réservation<br>par catégorie et par genre"), tabName = "par_categorie_hf"),
               
               menuSubItem(HTML("Nb de personnes par catégorie"), tabName = "nb_groupe"),
               menuSubItem(HTML("Nb de personnes par genre"), tabName = "nb_genre"),
               menuSubItem(HTML("Nb de personnes par catégorie<br>et par genre"), tabName = "nb_groupe_genre"),
               
               menuSubItem("Nb de réservation par heures", tabName = "par_heures"),
               menuSubItem(HTML("Nb de réservation<br>par heures et par catégorie"), tabName = "par_heures_categorie"),
               menuSubItem(HTML("Nb de réservation<br>par heure et par genre"), tabName = "par_heure_genre"),
               
               menuSubItem("Nb de réservation par jour", tabName = "par_jour"),
               menuSubItem(HTML("Nb de réservation<br>par jour et par catégorie"), tabName = "par_jour_categorie"),
               menuSubItem(HTML("Nb de réservation<br>par jour et par genre"), tabName = "par_jour_genre"),
               
               menuSubItem(HTML("Jour et horaires"), tabName = "jour_horaire"),
               menuSubItem(HTML("Jour et horaires par catégorie"), tabName = "jour_horaire_categorie"),
               menuSubItem(HTML("Jour et horaires par genre"), tabName = "jour_horaire_genre"),
               menuSubItem(HTML("Horaires et jour"), tabName = "horaire_jour_all"),
               
               menuSubItem("Nb de nouveau adherent", tabName = "nv_adh")#,
               #menuSubItem(HTML("Nb de nouveau adherent par catégorie"), tabName = "nv_adh_categorie"),
               #menuSubItem(HTML("Nb de nouveau adherent par genre"), tabName = "nv_adh_genre")
               
              ),
      menuItem(HTML("Classement"), tabName = "classement", icon = icon("ranking-star"))
    )
  ),
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        .plot-spacing {
          margin-bottom: 20px
        }
      "))),
    
    tabItems(
      # Onglet de chargement des fichiers
      tabItem(tabName = "upload",
              fluidRow(
                box(
                  title = "Charger les fichiers CSV",
                  width = 4,
                  fileInput("reservations", 
                            "Fichiers des réservations (plusieurs fichiers possibles)",
                            accept = c(".csv"),
                            multiple = TRUE),
                  fileInput("inscrits", 
                            "Fichier des inscrits (un seul fichier)",
                            accept = c(".csv")),
                  actionButton("process", "Traiter les fichiers")
                ),
                box(
                  title = "Aperçu des réservations",
                  width = 8,
                  tableOutput("preview_reservations")
                ),
                box(
                  title = "Aperçu des inscrits",
                  width = 8,
                  tableOutput("preview_inscrits")
                ),
                box(
                  title = "Données combinées",
                  width = 12,
                  tableOutput("preview_combined")
                )
              )
      ),
      
      # Onglet de gestion des tarifs
      tabItem(tabName = "tarifs",
              fluidRow(
                box(
                  title = "Regrouper les Tarifs",
                  width = 6,
                  textInput("group_name", "Nom du groupe :", ""),
                  uiOutput("tarif_selector"),
                  actionButton("add_group", "Ajouter le groupe")
                ),
                box(
                  title = "Supprimer un Tarif",
                  width = 6,
                  uiOutput("tarif_removal_selector"),
                  actionButton("remove_tarif", "Supprimer le tarif")
                )
              ),
              fluidRow(
                box(
                  title = "Aperçu des Regroupements",
                  width = 8,
                  tableOutput("preview_tarifs"),
                  actionButton("apply_tarif_settings", "Appliquer les regroupements")
                )
              )
      ),
      
      # Onglets d'analyses
      tabItem(tabName = "total",
              fluidRow(
                box(title = "Graphique des Réservations Totales", width = 12,
                    textInput("title_total", "Titre du graphique :", "Nombre de Réservations Totales par semaine"),
                    plotlyOutput("plot_total"))
              )
      ),
      tabItem(tabName = "total_hf",
              fluidRow(
                box(title = "Réservations par Genre", width = 12,
                    textInput("title_total_hf", "Titre du graphique :", "Nombre de Réservations par Genre en effectif"),
                    # radioButtons("affichage_total_hf", "Affichage :", 
                    #              choices = c("Effectif" = "count", "Pourcentage" = "percentage"),
                    #              selected = "count"),
                    plotlyOutput("plot_total_hf"),br(), br(), br(), br(), br(), 
                    textInput("title_total_hf_perc", "Titre du graphique :", "Nombre de Réservations par Genre en pourcentage"),
                    plotlyOutput("plot_total_hf_perc"),br(), br(), br(), br(), br(), 
                    h3("Rapport du nombre de réservations entre les femmes et les hommes"),
                    verbatimTextOutput("ratio"))
              )
      ),
      tabItem(tabName = "par_categorie",
              fluidRow(
                box(title = "Graphique par Catégorie", width = 12,
                    textInput("title_par_categorie", "Titre du graphique :", "Nombre de Réservations par Catégorie"),
                    # radioButtons("affichage_par_categorie", "Affichage :", 
                    #              choices = c("Effectif" = "count", "Pourcentage" = "percentage"),
                    #              selected = "count"),
                    plotlyOutput("plot_par_categorie"),br(), br(), br(), br(), br(), 
                    textInput("title_par_categorie_perc", "Titre du graphique :", "Nombre de Réservations par Catégorie en pourcentage"),
                    plotlyOutput("plot_par_categorie_perc"))
              )
      ),
      tabItem(tabName = "par_categorie_hf",
              fluidRow(
                box(title = "Réservations par Catégorie et par Genre", width = 12,
                    textInput("title_par_categorie_hf", "Titre du graphique :", "Nombre de Réservations par Catégorie et par Genre"),
                    # radioButtons("affichage_par_categorie_hf", "Affichage :", 
                    #              choices = c("Effectif" = "count", "Pourcentage" = "percentage"),
                    #              selected = "count"),
                    plotlyOutput("plot_par_categorie_hf"),br(), br(), br(), br(), br(),
                    textInput("title_par_categorie_hf_perc_ens", "Titre du graphique :", "Nombre de Réservations par Catégorie et par Genre en pourcentage dans l'ensemble"),
                    plotlyOutput("plot_par_categorie_hf_perc_ens"),br(), br(), br(), br(), br(),
                    textInput("title_par_categorie_hf_perc_grp", "Titre du graphique :", "Nombre de Réservations par Catégorie et par Genre en pourcentage au sein des catégorie"),
                    plotlyOutput("plot_par_categorie_hf_perc_grp")
                    )
              )
      ),
      tabItem(tabName = "par_heures",
              fluidRow(
                box(title = "Graphique par Heures", width = 12,
                    textInput("title_par_heures", "Titre du graphique :", "Nombre de Réservations par Heures"),
                    # radioButtons("affichage_par_heures", "Affichage :", 
                    #              choices = c("Effectif" = "count", "Pourcentage" = "percentage"),
                    #              selected = "count"),
                    plotlyOutput("plot_par_heures"),br(), br(), br(), br(), br(), 
                    textInput("title_par_heures_perc", "Titre du graphique :", "Nombre de Réservations par Heures en pourcentage"),
                    plotlyOutput("plot_par_heures_perc"))
              )
      ),
      tabItem(tabName = "par_heures_categorie",
              fluidRow(
                box(title = "Graphique par Heures et par Catégorie", width = 12,
                    textInput("title_par_heures_categorie", "Titre du graphique :", "Nombre de Réservations par Heures et par Catégorie"),
                    plotlyOutput("plot_par_heures_categorie"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_heures_categorie_scat", "Titre du graphique :", "Nombre de Réservations par Heures et par Catégorie en courbe"),
                    plotlyOutput("plot_par_heures_categorie_scat"),br(), br(), br(), br(), br(),
                    
                    textInput("title_par_heures_categorie_perc_ens", "Titre du graphique :", "Nombre de Réservations par heures et par catégorie en pourcentage dans l'ensemble"),
                    plotlyOutput("plot_par_heures_categorie_perc_ens"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_heures_categorie_perc_grp", "Titre du graphique :", "Nombre de Réservations par heures et par catégorie en pourcentage au sein des catégorie"),
                    plotlyOutput("plot_par_heures_categorie_perc_grp"))
              )
      ),
      tabItem(tabName = "par_jour",
              fluidRow(
                box(title = "Graphique par Jour", width = 12,
                    textInput("title_par_jour", "Titre du graphique :", "Nombre de Réservations par Jour"),
                    # radioButtons("affichage_par_jour", "Affichage :", 
                    #              choices = c("Effectif" = "count", "Pourcentage" = "percentage"),
                    #              selected = "count"),
                    plotlyOutput("plot_par_jour"),br(), br(), br(), br(), br(), 
                    textInput("title_par_jour_perc", "Titre du graphique :", "Nombre de Réservations par Jour en pourcentage"),
                    plotlyOutput("plot_par_jour_perc"))
              )
      ),
      tabItem(tabName = "par_jour_categorie",
              fluidRow(
                box(title = "Graphique par Jour et par Catégorie", width = 12,
                    textInput("title_par_jour_categorie", "Titre du graphique :", "Nombre de Réservations par Jour et par Catégorie"),
                    plotlyOutput("plot_par_jour_categorie"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_jour_categorie_scat", "Titre du graphique :", "Nombre de Réservations par Jour et par Catégorie en courbe"),
                    plotlyOutput("plot_par_jour_categorie_scat"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_jour_categorie_perc_ens", "Titre du graphique :", "Nombre de Réservations par Jour et par Catégorie en pourcentage dans l'ensemble"),
                    plotlyOutput("plot_par_jour_categorie_perc_ens"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_jour_categorie_perc_grp", "Titre du graphique :", "Nombre de Réservations par Jour et par Catégorie en pourcentage au sein des groupes"),
                    plotlyOutput("plot_par_jour_categorie_perc_grp"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_jour_categorie_perc_jour", "Titre du graphique :", "Nombre de Réservations par Jour et par Catégorie en pourcentage par jour"),
                    plotlyOutput("plot_par_jour_categorie_perc_jour")
                    )
              )
      ),
      tabItem(tabName = "par_jour_genre",
                    fluidRow(
                      box(title = "Graphique par Jour et par Genre", width = 12,
                          textInput("title_par_jour_genre", "Titre du graphique :", "Nombre de Réservations par Jour et par Genre"),
                          plotlyOutput("plot_par_jour_sexe"),br(), br(), br(), br(), br(), 
                          
                          textInput("title_par_jour_genre_scat", "Titre du graphique :", "Nombre de Réservations par Jour et par Genre en courbe"),
                          plotlyOutput("plot_par_jour_sexe_scat"),br(), br(), br(), br(), br(), 
                          
                          textInput("title_par_jour_genre_perc_ens", "Titre du graphique :", "Nombre de Réservations par Jour et par Genre en pourcentage dans l'ensemble"),
                          plotlyOutput("plot_par_jour_sexe_perc_ens"),br(), br(), br(), br(), br(), 
                          
                          textInput("title_par_jour_genre_perc_grp", "Titre du graphique :", "Nombre de Réservations par Jour et par Genre en pourcentage par genre"),
                          plotlyOutput("plot_par_jour_sexe_perc_grp"),br(), br(), br(), br(), br(), 
                          
                          textInput("title_par_jour_genre_perc_jour", "Titre du graphique :", "Nombre de Réservations par Jour et par Genre en pourcentage par jour"),
                          plotlyOutput("plot_par_jour_sexe_perc_jour"))
                    )
            ),
      tabItem(tabName = "par_heure_genre",
              fluidRow(
                box(title = "Graphique par Heure et par Genre", width = 12,
                    textInput("title_par_heure_sexe", "Titre du graphique :", "Nombre de Réservations par Heure et par Genre"),
                    plotlyOutput("plot_par_heures_sexe"),br(), br(), br(), br(), br(), 
                    
                    textInput("title_par_heures_sexe_scat", "Titre du graphique :", "Nombre de Réservations par Heures et par Genre en courbe"),
                    plotlyOutput("plot_par_heures_sexe_scat"),br(), br(), br(), br(), br(),                    
                    
                    
                    textInput("title_par_heure_sexe_perc_ens", "Titre du graphique :", "Nombre de Réservations par Heure et par Genre en pourcentage dans l'ensemble"),
                    plotlyOutput("plot_par_heures_sexe_perc_ens"),br(), br(), br(), br(), br(), 
                
                    textInput("title_par_heures_sexe_perc_grp", "Titre du graphique :", "Nombre de Réservations par heures et par Genre en pourcentage au sein des horaires"),
                    plotlyOutput("plot_par_heures_sexe_perc_grp"))
                
                
              )
      ),
      tabItem(tabName = "nb_groupe",
              fluidRow(
                box(title = "Graphique du nombre de personnes par groupe", width = 12,
                    textInput("title_par_nb_groupe", "Titre du graphique :", "Nombre de personnes par catégorie"),
                    plotlyOutput("plot_personnes_par_categorie"),br(), br(), br(), br(), br(), 
                    textInput("title_par_nb_groupe_perc", "Titre du graphique :", "Nombre de personnes par catégorie en pourcentage"),
                    plotlyOutput("plot_personnes_par_categorie_perc"))
              )
      ),
      tabItem(tabName = "nb_genre",
              fluidRow(
                box(title = "Graphique du nombre de personnes selon le genre", width = 12,
                    textInput("title_par_nb_genre", "Titre du graphique :", "Nombre de personnes par genre"),
                    plotlyOutput("plot_personnes_par_genre"),br(), br(), br(), br(), br(), 
                    textInput("title_par_nb_genre_perc", "Titre du graphique :", "Nombre de personnes par genre en pourcentage"),
                    plotlyOutput("plot_personnes_par_genre_perc"),
                    h3("Rapport du nombre de personnes entres les femmes et les hommes"),
                    verbatimTextOutput("ratio2"))
              )
      ),
      tabItem(tabName = "nb_groupe_genre",
              fluidRow(
                box(title = "Graphique du nombre de personnes par groupe et par genre", width = 12,
                    textInput("title_par_nb_groupe_genre", "Titre du graphique :", "Nombre de personnes par catégorie et par genre"),
                    plotlyOutput("plot_personnes_par_categorie_par_genre"),br(), br(), br(), br(), br(), 
                    textInput("title_par_nb_groupe_genre_perc_ens", "Titre du graphique :", "Nombre de personnes par catégorie et par genre en pourcentage dans l'ensemble"),
                    plotlyOutput("plot_personnes_par_categorie_par_genre_perc_ens"),br(), br(), br(), br(), br(), 
                    textInput("title_par_nb_groupe_genre_perc_grp", "Titre du graphique :", "Nombre de personnes par catégorie et par genre en pourcentage au sein des catégories"),
                    plotlyOutput("plot_personnes_par_categorie_par_genre_perc_grp"))
              )
      ),
      
      tabItem(tabName = "jour_horaire",
              fluidRow(
                box(title = "Graphique des réservation par jour et par horaire", width = 12,
       
                    # radioButtons("choice_hours", "Choix horaires :", 
                    #              choices = sort(unique(data$Horaires))),
                    textInput("title_horaires_par_jour_lundi", "Titre du graphique :", "Nombre de réservation par horaire le Lundi"),
                    plotlyOutput("plot_horaires_par_jour_lundi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_mardi", "Titre du graphique :", "Nombre de réservation par horaire le Mardi"),
                    plotlyOutput("plot_horaires_par_jour_mardi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_mercredi", "Titre du graphique :", "Nombre de réservation par horaire le Mercredi"),
                    plotlyOutput("plot_horaires_par_jour_mercredi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_jeudi", "Titre du graphique :", "Nombre de réservation par horaire le Jeudi"),
                    plotlyOutput("plot_horaires_par_jour_jeudi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_vendredi", "Titre du graphique :", "Nombre de réservation par horaire le Vendredi"),
                    plotlyOutput("plot_horaires_par_jour_vendredi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_samedi", "Titre du graphique :", "Nombre de réservation par horaire le Samedi"),
                    plotlyOutput("plot_horaires_par_jour_samedi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_dimanche", "Titre du graphique :", "Nombre de réservation par horaire le Dimanche"),
                    plotlyOutput("plot_horaires_par_jour_dimanche")
                )
              )
          ),
      tabItem(tabName = "jour_horaire_categorie",
              fluidRow(
                box(title = "Graphique des réservation par jour et par horaire par catégorie", width = 12,
                    
                    
                    textInput("title_horaires_par_jour_categorie_lundi", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Lundi"),
                    plotlyOutput("plot_horaires_par_jour_categorie_lundi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_categorie_mardi", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Mardi"),
                    plotlyOutput("plot_horaires_par_jour_categorie_mardi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_categorie_mercredi", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Mercredi"),
                    plotlyOutput("plot_horaires_par_jour_categorie_mercredi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_categorie_jeudi", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Jeudi"),
                    plotlyOutput("plot_horaires_par_jour_categorie_jeudi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_categorie_vendredi", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Vendredi"),
                    plotlyOutput("plot_horaires_par_jour_categorie_vendredi"), br(), br(), br(), br(), br(), 
                    
                    textInput("title_horaires_par_jour_categorie_samedi", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Samedi"),
                    plotlyOutput("plot_horaires_par_jour_categorie_samedi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_categorie_dimanche", "Titre du graphique :", "Nombre de réservation par horaire et par catégorie le Dimanche"),
                    plotlyOutput("plot_horaires_par_jour_categorie_dimanche")
                )
              )
      ),
      tabItem(tabName = "jour_horaire_genre",
              fluidRow(
                box(title = "Graphique des réservation par jour et par horaire par genre", width = 12,
                    
                    textInput("title_horaires_par_jour_genre_lundi", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Lundi"),
                    plotlyOutput("plot_horaires_par_jour_genre_lundi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_genre_mardi", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Mardi"),
                    plotlyOutput("plot_horaires_par_jour_genre_mardi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_genre_mercredi", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Mercredi"),
                    plotlyOutput("plot_horaires_par_jour_genre_mercredi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_genre_jeudi", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Jeudi"),
                    plotlyOutput("plot_horaires_par_jour_genre_jeudi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_genre_vendredi", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Vendredi"),
                    plotlyOutput("plot_horaires_par_jour_genre_vendredi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_genre_samedi", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Samedi"),
                    plotlyOutput("plot_horaires_par_jour_genre_samedi"), br(), br(), br(), br(), br(),
                    
                    textInput("title_horaires_par_jour_genre_dimanche", "Titre du graphique :", "Nombre de réservation par horaire et par genre le Dimanche"),
                    plotlyOutput("plot_horaires_par_jour_genre_dimanche")
                )
              )
      ),
      tabItem(tabName = "horaire_jour_all",
              fluidRow(
                box(title = "Graphique des réservation en fonction des horaires et des jours", width = 12,
                    uiOutput("select_hours_ui"),
                    plotlyOutput("plot_horaire_jour"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaire_jour_cat"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaire_jour_genre")
                )
              )
      ),      
      
      tabItem(tabName = "nv_adh",
                      fluidRow(
                        box(title = "Graphique du nombre de nouveau adhérent", width = 12,
                            textInput("title_nv_adh", "Titre du graphique :", "Nombre de nouveaux adhérents"),
                            plotlyOutput("plot_nv_adh"),br(), br(), br(), br(), br(), 
                            
                            textInput("title_nv_adh_perc", "Titre du graphique :", "Nombre de nouveaux adhérents en pourcentage"),
                            plotlyOutput("plot_nv_adh_perc"),br(), br(), br(), br(), br(),
                            
                            textInput("title_nv_adh_sexe", "Titre du graphique :", "Nombre de nouveaux adhérents par genre"),
                            plotlyOutput("plot_nv_adh_sexe"),br(), br(), br(), br(), br(),
                            
                            textInput("title_nv_adh_cat", "Titre du graphique :", "Nombre de nouveaux adhérents par catégorie"),
                            plotlyOutput("plot_nv_adh_cat"))
                      )
      ),
      
      tabItem(tabName = "classement",
               dataTableOutput("table_top_joueurs")   
      )
      
      
      )
    ) #dashbody
  )

