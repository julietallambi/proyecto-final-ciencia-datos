#Librerías

library(tidyverse)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(shiny)
library(shinydashboard)
library(DT)

#Cargar datos
data <- read.csv("International_Education_Costs.csv")

#Limpieza de datos

data= data |> select(Country, Program, Tuition_USD:Insurance_USD) |> 
  transmute(pais = as_factor(Country), 
            area = as_factor(Program),
            costo_estudios = Tuition_USD, 
            indice_costo_vivienda = Living_Cost_Index,
            alquiler = Rent_USD,
            costo_visa = Visa_Fee_USD,
            seguro_medico = Insurance_USD)

#Crear una nueva variable llamada continente
data <- data |> 
  mutate(continente = case_when(
    pais %in% c("USA", "Canada", "Mexico", "Brazil", "Argentina", "Colombia", 
                "Dominican Republic", "Peru", "Ecuador", "Uruguay", 
                "Panama", "El Salvador") ~ "América",
    pais %in% c("UK", "Germany", "Netherlands", "France", "Switzerland", "Sweden", 
                "Denmark", "Ireland", "Austria", "Belgium", "Portugal", 
                "Czech Republic", "Poland", "Spain", "Italy", "Finland", 
                "Norway", "Russia", "Greece", "Hungary", "Iceland", 
                "Romania", "Luxembourg", "Cyprus", "Croatia", "Bulgaria", 
                "Ukraine", "Slovenia", "Serbia") ~ "Europa",
    pais %in% c("Japan", "Singapore", "China", "South Korea", "Hong Kong", 
                "Israel", "Taiwan", "India", "Malaysia", "Turkey", "Thailand", 
                "UAE", "Indonesia", "Saudi Arabia", "Vietnam", "Lebanon", 
                "Bahrain", "Bangladesh", "Kuwait", "Uzbekistan", "Iran") ~ "Asia",
    pais %in% c("South Africa", "Egypt", "Nigeria", "Tunisia", "Morocco", 
                "Ghana", "Algeria") ~ "África",
    pais %in% c("Australia", "New Zealand") ~ "Oceanía",
    
    TRUE ~ "Otro" 
  ))

#Cambiar los continentes a factores
data <- data |> 
  mutate(continente = as_factor(continente))

#Dividir costos en terciles
terciles <- quantile(data$costo_estudios,
                     probs = c(1/3, 2/3))
data <- data |> mutate(nivel_costos = case_when(costo_estudios < 3800 ~ "Bajo",
                                               costo_estudios>=3800 & costo_estudios<26500 ~ "Medio",
                                               costo_estudios>=26500 ~"Alto"))


# Listas para selectores del shiny
top_5_paises <- data |> count(pais, sort = TRUE) |> slice_max(n, n = 5) |> pull(pais) |> as.character()
areas_disponibles <- c("Todas", sort(unique(as.character(data$area))))
continentes_lista <- c("Todos", sort(unique(as.character(data$continente))))


#SHINY

# --- 3. INTERFAZ DE USUARIO (UI) ---
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = span(icon("graduation-cap"), " EduCost Explorer")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa Global", tabName = "mapa", icon = icon("globe-americas")),
      menuItem("Análisis Estadístico", tabName = "analisis", icon = icon("chart-pie")),
      menuItem("Calculadora Pro", tabName = "calculadora", icon = icon("wallet"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap"),
      tags$style(HTML("
        * { font-family: 'Montserrat', sans-serif; }
        .box { border-radius: 12px; box-shadow: 0 4px 6px rgba(0,0,0,0.1); }
        
        /* CSS para controles púrpuras cohesivos */
        .irs-bar, .irs-bar-edge, .irs-single, .irs-from, .irs-to { background: #605ca8 !important; border-color: #605ca8 !important; }
        .selectize-input.full, .selectize-input.focus { border-color: #605ca8 !important; }
        .selectize-dropdown .active { background-color: #605ca8 !important; color: white; }
        .bg-aqua { background-color: #9370DB !important; } 
      "))
    ),
    
    tabItems(
      # PESTAÑA 1: MAPA
      tabItem(tabName = "mapa",
              fluidRow(
                box(title = "Filtro Geográfico", width = 4, status = "primary", solidHeader = TRUE,
                    selectInput("sel_cont_mapa", "Ver Continente:", choices = continentes_lista)),
                valueBoxOutput("box_pct", width = 4),
                valueBoxOutput("box_prom", width = 4)
              ),
              box(title = "Cuota de Mercado Estudiantil por País (%)", width = 12, plotlyOutput("mapa_interactivo"))
      ),
      
      # PESTAÑA 2: ANÁLISIS (Distribución 2x2 Simétrica)
      tabItem(tabName = "analisis",
              box(title = "Filtro Global de la Pestaña", width = 12, status = "info", solidHeader = TRUE,
                  selectInput("sel_cont_eda", "Filtrar Continente para Comparación:", choices = continentes_lista)),
              
              # Fila 1
              fluidRow(
                box(title = "Densidad de Matrículas", width = 6, plotOutput("plot_density")),
                box(title = "Niveles de Costo (Proporción)", width = 6, plotOutput("plot_barras"))
              ),
              
              # Fila 2
              fluidRow(
                box(title = "Relación Alquiler vs Matrícula", width = 6, plotlyOutput("plot_scatter")),
                box(title = "Top 5 Áreas por País", width = 6, 
                    selectInput("pais_popular", "Elegir País Top:", choices = top_5_paises),
                    plotOutput("plot_areas_dinamico"))
              )
      ),
      
      # PESTAÑA 3: CALCULADORA
      tabItem(tabName = "calculadora",
              fluidRow(
                box(title = "Personaliza tu Búsqueda", width = 12, status = "success", solidHeader = TRUE,
                    column(4, sliderInput("presupuesto", "Presupuesto Máximo (USD):", min = 0, max = 60000, value = 25000, step = 500)),
                    column(4, selectInput("sel_area_calc", "Área de estudio:", choices = areas_disponibles)),
                    column(4, selectInput("sel_cont_calc", "Región preferida:", choices = continentes_lista))
                )
              ),
              box(title = "Resultados de Oferta Educativa (Ordenado por mayor costo)", width = 12, DTOutput("tabla_interactiva"))
      )
    )
  )
)

# --- 4. SERVIDOR (SERVER) ---
server <- function(input, output) {
  
  # REACTIVOS INDEPENDIENTES
  datos_mapa <- reactive({
    df <- data
    if(input$sel_cont_mapa != "Todos") df <- df |> filter(continente == input$sel_cont_mapa)
    df
  })
  
  datos_eda <- reactive({
    df <- data
    if(input$sel_cont_eda != "Todos") df <- df |> filter(continente == input$sel_cont_eda)
    df
  })
  
  # OUTPUTS MAPA E INDICADORES
  output$box_pct <- renderValueBox({
    val <- (nrow(datos_mapa()) / nrow(data)) * 100
    valueBox(paste0(round(val, 1), "%"), "De la Oferta Global", icon = icon("percent"), color = "purple")
  })
  
  output$box_prom <- renderValueBox({
    prom <- mean(datos_mapa()$costo_estudios, na.rm = TRUE)
    if(is.nan(prom)) prom <- 0
    valueBox(paste0("$", round(prom)), "Promedio Matrícula", icon = icon("dollar-sign"), color = "olive")
  })
  
  output$mapa_interactivo <- renderPlotly({
    total_f <- nrow(datos_mapa())
    map_data <- datos_mapa() |> group_by(pais) |> summarise(n = n()) |> 
      mutate(porcentaje = (n/total_f)*100, pais = as.character(pais))
    plot_geo(map_data, locationmode = 'country names') |>
      add_trace(z = ~porcentaje, locations = ~pais, color = ~porcentaje, colors = 'Purples',
                text = ~paste("País:", pais, "<br>Cuota:", round(porcentaje, 2), "%")) |>
      layout(geo = list(projection = list(type = 'natural earth')))
  })
  
  # OUTPUTS ANÁLISIS (Colores unificados por Continente)
  output$plot_density <- renderPlot({
    ggplot(datos_eda(), aes(x = costo_estudios, fill = continente)) +
      geom_density(alpha = 0.7) + theme_minimal() + scale_fill_brewer(palette = "Set1") +
      labs(x = "Costo Matrícula (USD)", y = "Densidad")
  })
  
  output$plot_barras <- renderPlot({
    ggplot(datos_eda(), aes(x = continente, fill = nivel_costos)) +
      geom_bar(position = "fill") + scale_fill_brewer(palette = "Set2", drop = FALSE) +
      theme_minimal() + labs(x = "Continente", y = "Proporción", fill = "Nivel Costo")
  })
  
  output$plot_scatter <- renderPlotly({
    g <- ggplot(datos_eda(), aes(x = alquiler, y = costo_estudios, color = continente, text = pais)) +
      geom_point(alpha = 0.7, size = 2) + theme_light() + scale_color_brewer(palette = "Set1") +
      labs(x = "Alquiler Mensual", y = "Matrícula Anual")
    ggplotly(g)
  })
  
  output$plot_areas_dinamico <- renderPlot({
    req(input$pais_popular)
    df_p <- data |> filter(pais == input$pais_popular)
    top_5 <- df_p |> count(area, sort = TRUE) |> slice_max(n, n = 5, with_ties = FALSE)
    grafico_f <- df_p |> filter(area %in% top_5$area) |> count(pais, area)
    ggplot(grafico_f, aes(x = pais, y = n, fill = area)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal() + scale_fill_brewer(palette = "Pastel1") +
      labs(x = "País", y = "Distribución Áreas", fill = "Carrera")
  })
  
  # OUTPUT CALCULADORA (Triple filtro dinámico)
  output$tabla_interactiva <- renderDT({
    df_calc <- data |> filter(costo_estudios <= input$presupuesto)
    if(input$sel_area_calc != "Todas") df_calc <- df_calc |> filter(area == input$sel_area_calc)
    if(input$sel_cont_calc != "Todos") df_calc <- df_calc |> filter(continente == input$sel_cont_calc)
    df_calc |> select(pais, area, costo_estudios, nivel_costos) |> arrange(desc(costo_estudios))
  })
}

# --- 5. LANZAR APP ---
shinyApp(ui, server)
