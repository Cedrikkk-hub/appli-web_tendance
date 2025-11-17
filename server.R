server <- function(input, output, session) {
  
  # —- Filtrage réactif des données selon les choix utilisateur
  data_filtre <- reactive({
    df <- data_joint %>%
      filter(Date.Achat >= input$periode[1],
             Date.Achat <= input$periode[2])
    if (input$site != "Tous") {
      df <- df %>% filter(NOM_SITE == input$site)
    }
    df
  })
  
  # —- Graphique d’évolution
  output$evolutionPlot <- renderPlot({
    # Données filtrées selon site + période
    df <- data_filtre()
    ind <- input$indicateur
    comp <- input$comparaison
    
    # Créer une vraie date « début de mois » à partir de Année et Mois
    df2 <- df %>%
      mutate(YearMonth = lubridate::make_date(year = Annee, month = Mois, day = 1)) %>%
      arrange(YearMonth)
    
    # Vérifier que la colonne de comparaison existe, si besoin
    if (comp != "none") {
      validate(
        need(comp %in% names(df2),
             paste("La colonne de comparaison ‘", comp, "’ n'existe pas", sep = ""))
      )
    }
    
    if (comp == "none") {
      # Agréger par mois seulement
      df_plot <- df2 %>%
        group_by(YearMonth) %>%
        summarise(Valeur = sum(.data[[ind]], na.rm = TRUE),
                  .groups = "drop")
      
      ggplot(df_plot, aes(x = YearMonth, y = Valeur)) +
        geom_line(color = "blue") +
        geom_point(color = "blue") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(title = paste("Évolution de", ind),
             x = "Mois", y = ind) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else {
      # Agréger par mois + catégorie de comparaison
      df_plot <- df2 %>%
        group_by(YearMonth, .data[[comp]]) %>%
        summarise(Valeur = sum(.data[[ind]], na.rm = TRUE),
                  .groups = "drop")
      
      ggplot(df_plot, aes(x = YearMonth, y = Valeur, color = .data[[comp]])) +
        geom_line() +
        geom_point() +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(title = paste("Évolution de", ind, "par", comp),
             x = "Mois", y = ind, color = comp) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  # —- Tableau résumé par site
  output$resumeTable <- renderTable({
    df <- data_filtre()
    df %>%
      group_by(NOM_SITE) %>%
      summarise(
        Nb_Achats     = sum(Nb.Achat, na.rm = TRUE),
        Montant_Total = sum(Mnt.Achat, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # —- Carte interactive des ventes par département
  output$mapPlot <- renderLeaflet({
    df <- data_filtre()
    ind <- input$indicateur
    
    ventes_dept <- df %>%
      mutate(Dept = substr(COD_POSTAL, 1, 2)) %>%
      group_by(Dept) %>%
      summarise(Valeur = sum(.data[[ind]], na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Dept = as.character(Dept))
    
    fr_map <- fr_dept %>%
      mutate(code = as.character(code)) %>%
      left_join(ventes_dept, by = c("code" = "Dept"))
    
    pal <- colorNumeric("Blues", domain = fr_map$Valeur, na.color = "transparent")
    
    leaflet(fr_map) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(Valeur),
        weight      = 1,
        color       = "white",
        opacity     = 1,
        fillOpacity = 0.7,
        label       = ~paste0(nom, " : ", ifelse(is.na(Valeur), 0, Valeur))
      ) %>%
      addLegend(pal = pal,
                values = ~Valeur,
                opacity = 0.7,
                title = ind,
                position = "bottomright")
  })
  
}
