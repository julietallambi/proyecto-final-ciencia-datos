#Librerías

library(tidyverse)
library(ggplot2)
library(plotly)
library(ggmosaic)
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


data <- data |> 
  mutate(continente = as_factor(continente))

#Análisis exploratorio

terciles <- quantile(data$costo_estudios,
                     probs = c(1/3, 2/3))

data = data |> mutate(nivel_costos = case_when(costo_estudios < 3800 ~ "Bajo",
                                               costo_estudios>=3800 & costo_estudios<26500 ~ "Medio",
                                               costo_estudios>=26500 ~"Alto"))

top_10_paises = data |> count(pais, sort=TRUE) |> slice_max(n, n=10)

top_10_paises |>
  ggplot(aes(x = reorder(pais, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title='Cantidad de estudiantes extranjeros por país',
       x='País',
       y='Cantidad de estudiantes')

top_15_iv <- data |> 
  group_by(pais) |> 
  summarise(indice_costo_vivienda = mean(indice_costo_vivienda)) |> 
  slice_max(order_by = indice_costo_vivienda, n = 15)

ggplot(top_15_iv,
       aes(x = indice_costo_vivienda,
           y = reorder(pais, indice_costo_vivienda))) +
  geom_col(fill = "#857FB0") +
  labs(title = "15 Países con mayor índice del costo de vivienda",
       x = "Índice del costo de vivenda",
       y = "País") +
  theme_minimal()

data_top10_paises <- data |>
  filter(pais %in% top_10_paises$pais) 

ggplot(data_top10_paises, aes(x = costo_estudios,
             y = alquiler,
             color = pais)) +
  geom_point(alpha=0.6, size=2) + labs(title = 'Relación entre el costo de alquiler y costo de los estudios por país', x = 'Costo estudios', y ='Costo alquiler', color = 'País') 
ggplotly(data_top10_paises) 

#Medidas de resumen y dispersión

resumen_costos <- data |> 
  group_by(continente) |> 
  summarise(
    costo_estudio_promedio = mean(costo_estudios), 
    costo_estudio_sd = sd(costo_estudios))


data_comparado <- data |> 
  filter(continente %in% c("Europa", "América"))

ggplot(data_comparado, aes(x = costo_estudios, fill = continente)) +
  geom_density(alpha = 0.5) + 
  theme_minimal() +
  labs(
    title = "Comparación de la distribución de costos de estudios",
    subtitle = "Europa vs. América",
    x = "Costo de estudios",
    y = "Densidad",
    fill = "Continente")

ggplot(data, aes(x = continente, fill = nivel_costos)) +
  geom_bar(position = "fill") +
  labs(
    x = "Continente",
    y = "Proporción",
    fill = "Nivel de costos en educación"
  ) +
  scale_y_continuous(labels = scales::percent) + labs(title = "Proporción de nivel de costos por continente") +
  theme_minimal() + scale_fill_brewer(palette = "Set2")


#SHINY

# --- 3. Interfaz de Usuario (UI) ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Costos de la educación internacional"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa Global", tabName = "mapa", icon = icon("globe")),
      menuItem("Análisis de datos", tabName = "analisis", icon = icon("chart-bar")),
      menuItem("Calculadora", tabName = "calculadora", icon = icon("calculator"))
    ),
    hr(),
    selectInput("sel_cont", "Seleccione continente:", choices = c("Todos", unique(as.character(data$continente)))),
    sliderInput("presupuesto", "Presupuesto (USD):", min = 0, max = 60000, value = 25000, step = 1000)
  ),
  
  dashboardBody(
    tabItems(
      # Pestaña Mapa
      tabItem(tabName = "mapa",
              fluidRow(
                valueBoxOutput("box_paises", width = 6),
                valueBoxOutput("box_promedio", width = 6)
              ),
              box(title = "Estudiantes extranjeros por país", width = 12, plotlyOutput("mapa_mundial"))
      ),
      # Pestaña Análisis EDA (Capturas 35, 36)
      tabItem(tabName = "analisis",
              fluidRow(
                box(title = "Distribución de costos", width = 6, plotOutput("plot_density")),
                box(title = "Nivel de costos por región", width = 6, plotOutput("plot_barras"))
              ),
              box(title = "Relación alquiler vs costos", width = 12, plotlyOutput("plot_scatter"))
      ),
      # Pestaña Tabla
      tabItem(tabName = "calculadora",
              box(title = "Opciones según tu presupuesto", width = 12, DTOutput("tabla_interactiva"))
      )
    )
  )
)

# --- 4. Servidor (Server) ---
server <- function(input, output) {
  
  # ESTACIÓN REACTIVA: Filtra tu objeto 'data' según la UI
  # Importante: se usa siempre con paréntesis: datos_f()
  datos_f <- reactive({
    req(input$sel_cont)
    df <- data
    if(input$sel_cont != "Todos") {
      df <- df |> filter(continente == input$sel_cont)
    }
    df |> filter(costo_estudios <= input$presupuesto)
  })
  
  output$box_paises <- renderValueBox({ valueBox(nrow(datos_f()), "Cantidad de estudiantes extranjeros", icon = icon("list"), color = "aqua") })
  output$box_promedio <- renderValueBox({
    prom <- mean(datos_f()$costo_estudios, na.rm = TRUE)
    valueBox(paste0("$", round(prom)), "Costo de estudio", icon = icon("university"), color = "green")
  })
  
 # Mapa interactivo corregido
  output$mapa_mundial <- renderPlotly({
    # 1. Preparamos los datos para el mapa agrupando por país
    # Usamos datos_f() que ya tiene los filtros de continente y presupuesto
    map_data <- datos_f() |> 
      group_by(pais) |> 
      summarise(
        cantidad = n(), # Cantidad de programas/estudiantes (Captura 33)
        costo_medio = mean(costo_estudios, na.rm = TRUE)
      ) |> 
      mutate(pais = as.character(pais)) # Plotly necesita texto, no factores
    
    # 2. Creamos el mapa coroplético (pinta los países por color)
    plot_geo(map_data, locationmode = 'country names') |>
      add_trace(
        z = ~cantidad, 
        locations = ~pais, 
        color = ~cantidad, 
        colors = 'Blues',
        text = ~paste("País:", pais, 
                      "<br>Programas:", cantidad, 
                      "<br>Costo matrícula promedio: $", round(costo_medio))
      ) |>
      layout(
        geo = list(
          projection = list(type = 'natural earth'),
          showlakes = TRUE, 
          lakecolor = toRGB('white')
        ),
        title = "Distribución global de oferta educativa"
      )
  })

  
  # Gráficos usando tus nombres de variables exactos
  output$plot_density <- renderPlot({
    ggplot(datos_f(), aes(x = costo_estudios, fill = continente)) +
      geom_density(alpha = 0.5) + theme_minimal()
  })
  
  output$plot_barras <- renderPlot({
    ggplot(datos_f(), aes(x = continente, fill = nivel_costos)) +
      geom_bar(position = "fill") + theme_minimal()
  })
  
  output$plot_scatter <- renderPlotly({
    g <- ggplot(datos_f(), aes(x = alquiler, y = costo_estudios, color = continente, text = pais)) +
      geom_point() + theme_light()
    ggplotly(g)
  })
  
  output$tabla_interactiva <- renderDT({
    datos_f() |> select(pais, area, costo_estudios, nivel_costos)
  })
}

# --- 5. Lanzar App ---
shinyApp(ui, server)
