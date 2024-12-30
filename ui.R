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

ui <- dashboardPage(
  dashboardHeader(title = "Analyse Tennis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement des fichiers", tabName = "upload", icon = icon("file-upload")),
      menuItem("Paramètres des tarifs", tabName = "tarifs", icon = icon("cogs")),
      menuItem("Statistiques", tabName = "stats", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Onglet Chargement des fichiers
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
      # Onglet Paramètres des tarifs
      tabItem(tabName = "tarifs",
              fluidRow(
                box(
                  title = "Combiner les tarifs",
                  width = 6,
                  textInput("group_name", "Nom du groupe :", placeholder = "Ex. Groupe A"),
                  uiOutput("tarif_selector"),
                  actionButton("add_group", "Ajouter au groupe"),
                  actionButton("apply_tarif_settings", "Appliquer les paramètres")
                ),
                box(
                  title = "Aperçu des regroupements",
                  width = 6,
                  tableOutput("preview_tarifs")
                )
              )
      ),
      
      # Onglet Statistiques
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  title = "Statistiques globales",
                  width = 12,
                  tableOutput("statistiques_table")
                )
              )
      )
    )
  )
)
