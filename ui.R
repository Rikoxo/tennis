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
               menuSubItem("Nb de réservation total", tabName = "total"),
               menuSubItem(HTML("Nb de réservation<br>total par genre"), tabName = "total_hf"),
               menuSubItem("Nb de réservation par catégorie", tabName = "par_categorie"),
               menuSubItem(HTML("Nb de réservation<br>par catégorie et par genre"), tabName = "par_categorie_hf"),
               menuSubItem("Nb de réservation par heures", tabName = "par_heures"),
               menuSubItem(HTML("Nb de réservation<br>par heures et par catégorie"), tabName = "par_heures_categorie"),
               menuSubItem("Nb de réservation par jour", tabName = "par_jour"),
               menuSubItem(HTML("Nb de réservation<br>par jour et par catégorie"), tabName = "par_jour_categorie"),
               menuSubItem(HTML("Nb de réservation<br>par jour et par genre"), tabName = "par_jour_genre"),
               menuSubItem(HTML("Nb de réservation<br>par jour et par heure"), tabName = "par_heure_genre"),
               menuSubItem(HTML("Personnes par catégorie"), tabName = "nb_groupe"),
               menuSubItem(HTML("Personnes par catégorie<br>et par genre"), tabName = "nb_groupe_genre"),
               menuSubItem(HTML("Jour et horaires"), tabName = "jour_horaire"),
               menuSubItem(HTML("Jour et horaires par catégorie"), tabName = "jour_horaire_categorie"),
               menuSubItem(HTML("Jour et horaires par genre"), tabName = "jour_horaire_genre")
               #menuSubItem(HTML("Classement"), tabName = "classement")
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
                    textInput("title_total", "Titre du graphique :", "Réservations Totales par semaine"),
                    plotlyOutput("plot_total"))
              )
      ),
      tabItem(tabName = "total_hf",
              fluidRow(
                box(title = "Réservations Totales par Genre", width = 12,
                    textInput("title_total_hf", "Titre du graphique :", "Réservations Totales par Genre"),
                    plotlyOutput("plot_total_hf"))
              )
      ),
      tabItem(tabName = "par_categorie",
              fluidRow(
                box(title = "Graphique par Catégorie", width = 12,
                    textInput("title_par_categorie", "Titre du graphique :", "Réservations par Catégorie"),
                    plotlyOutput("plot_par_categorie"))
              )
      ),
      tabItem(tabName = "par_categorie_hf",
              fluidRow(
                box(title = "Réservations par Catégorie et par Genre", width = 12,
                    textInput("title_par_categorie_hf", "Titre du graphique :", "Réservations par Catégorie et par Genre"),
                    plotlyOutput("plot_par_categorie_hf"))
              )
      ),
      tabItem(tabName = "par_heures",
              fluidRow(
                box(title = "Graphique par Heures", width = 12,
                    textInput("title_par_heures", "Titre du graphique :", "Réservations par Heures"),
                    plotlyOutput("plot_par_heures"))
              )
      ),
      tabItem(tabName = "par_heures_categorie",
              fluidRow(
                box(title = "Graphique par Heures et par Catégorie", width = 12,
                    textInput("title_par_heures_categorie", "Titre du graphique :", "Réservations par Heures et par Catégorie"),
                    plotlyOutput("plot_par_heures_categorie"))
              )
      ),
      tabItem(tabName = "par_jour",
              fluidRow(
                box(title = "Graphique par Jour", width = 12,
                    textInput("title_par_jour", "Titre du graphique :", "Réservations par Jour"),
                    plotlyOutput("plot_par_jour"))
              )
      ),
      tabItem(tabName = "par_jour_categorie",
              fluidRow(
                box(title = "Graphique par Jour et par Catégorie", width = 12,
                    textInput("title_par_jour_categorie", "Titre du graphique :", "Réservations par Jour et par Catégorie"),
                    plotlyOutput("plot_par_jour_categorie"))
              )
      ),
      tabItem(tabName = "par_jour_genre",
                    fluidRow(
                      box(title = "Graphique par Jour et par Genre", width = 12,
                          textInput("title_par_jour_genre", "Titre du graphique :", "Réservations par Jour et par Genre"),
                          plotlyOutput("plot_par_jour_sexe"))
                    )
            ),
      tabItem(tabName = "par_heure_genre",
              fluidRow(
                box(title = "Graphique par Heure et par Genre", width = 12,
                    textInput("title_par_heure_genre", "Titre du graphique :", "Réservations par Heure et par Genre"),
                    plotlyOutput("plot_par_heures_sexe"))
              )
      ),
      tabItem(tabName = "nb_groupe",
              fluidRow(
                box(title = "Graphique du nombre de personnes par groupe", width = 12,
                    textInput("title_par_nb_groupe", "Titre du graphique :", "Nombre de personnes par catégorie"),
                    plotlyOutput("plot_personnes_par_categorie"))
              )
      ),
      tabItem(tabName = "nb_groupe_genre",
              fluidRow(
                box(title = "Graphique du nombre de personnes par groupe et par genre", width = 12,
                    textInput("title_par_nb_groupe_genre", "Titre du graphique :", "Nombre de personnes par catégorie et par genre"),
                    plotlyOutput("plot_personnes_par_categorie_par_genre"))
              )
      ),
      tabItem(tabName = "classement",
              dataTableOutput("table_top_joueurs")   
      ),
      tabItem(tabName = "jour_horaire",
              fluidRow(
                box(title = "Graphique des réservation par jour et par horaire", width = 12,
                    plotlyOutput("plot_horaires_par_jour_lundi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_mardi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_mercredi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_jeudi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_vendredi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_samedi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_dimanche")
                )
              )
          ),
      tabItem(tabName = "jour_horaire_categorie",
              fluidRow(
                box(title = "Graphique des réservation par jour et par horaire par catégorie", width = 12,
                    plotlyOutput("plot_horaires_par_jour_categorie_lundi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_categorie_mardi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_categorie_mercredi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_categorie_jeudi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_categorie_vendredi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_categorie_samedi"), br(), br(), br(), br(), br(), 
                    plotlyOutput("plot_horaires_par_jour_categorie_dimanche")
                )
              )
      ),
      tabItem(tabName = "jour_horaire_genre",
              fluidRow(
                box(title = "Graphique des réservation par jour et par horaire par genre", width = 12,
                    plotlyOutput("plot_horaires_par_jour_genre_lundi"), br(), br(), br(), br(), br(),
                    plotlyOutput("plot_horaires_par_jour_genre_mardi"), br(), br(), br(), br(), br(),
                    plotlyOutput("plot_horaires_par_jour_genre_mercredi"), br(), br(), br(), br(), br(),
                    plotlyOutput("plot_horaires_par_jour_genre_jeudi"), br(), br(), br(), br(), br(),
                    plotlyOutput("plot_horaires_par_jour_genre_vendredi"), br(), br(), br(), br(), br(),
                    plotlyOutput("plot_horaires_par_jour_genre_samedi"), br(), br(), br(), br(), br(),
                    plotlyOutput("plot_horaires_par_jour_genre_dimanche")
                )
              )
      )
      )
    ) #dashbody
  )

