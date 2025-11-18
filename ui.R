library(shiny)
ui <- fluidPage(
  tags$style(HTML("
      table, th, td {
        border: 1px solid #999 !important;
        border-collapse: collapse !important;
        padding: 6px;
      }#Pour un tableau quadrillé avec colonnes distinctes
  ")),
  tags$img(src = "vente.jpg", width = "100%", height = "auto"),
  titlePanel("Application Webtendance "),
  sidebarLayout(
    sidebarPanel(
      selectInput("indicateur", "Choisir un indicateur :", 
                  choices = c("Nombre d'achats" = "Nb.Achat",
                              "Montant des achats" = "Mnt.Achat")),
      
      selectInput("site", "Choisir un site :", 
                  choices = c("Tous", unique(na.omit(data_joint$NOM_SITE)))),
      
      dateRangeInput("periode", "Période :", 
                     start = min(data_joint$Date.Achat),
                     end   = max(data_joint$Date.Achat)),
      
      radioButtons("comparaison", "Comparer par :", 
                   choices = c("Aucune" = "none",
                               "Sexe"   = "Sexe",
                               "Tranche d'âge" = "tranche_age"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Évolutions", plotOutput("evolutionPlot")),
        tabPanel("Résumé",    tableOutput("resumeTable")),
        tabPanel("Carte",     leafletOutput("mapPlot", height = 600))
      )
    )
  )
)

