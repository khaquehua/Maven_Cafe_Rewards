library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(readr)

customers <- read_csv("customers.csv")
events <- read_csv("events.csv")
offers <- read_csv("offers.csv")

# Función para crear la interfaz de usuario del dashboard
dashboard_ui <- function() {
  dashboardPage(
    title = "Titulo Fuente",
    dashboardHeader(
      title = dashboardBrand(
        title = h4("Titulo pagina"),
        href = "https://www.clinicalafuente.com",
        image = "https://inventario.lafuentecsi.com/favicon.png"
      )
    ),
    dashboardSidebar(
      uiOutput("sidebar_content")
    ),
    dashboardBody(
      uiOutput("content"), # Uso de uiOutput para el contenido condicionado
    ),
    footer = bs4DashFooter(
      left = "Developed by: Kevin Heberth Haquehua Apaza / Statistical / Mathematical / Data Science / Data Analyst / Systems analyst",
      right = tagList(
        tags$a(href = "https://www.linkedin.com/in/kevinhaquehua/", target = "_blank", 
               icon("linkedin", lib = "font-awesome"), style = "margin-left: 10px;"),
        tags$a(href = "https://github.com/khaquehua", target = "_blank", 
               icon("github", lib = "font-awesome"), style = "margin-left: 10px;"),
        tags$a(href = "https://wa.link/u69n53", target = "_blank", 
               icon("whatsapp", lib = "font-awesome"), style = "margin-left: 10px;")
      )
    )
  )
}

# Interfaz de usuario principal
ui <- dashboard_ui()

add_favicon <- function() {
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "https://www.encuestas.lafuentecsi.com/favicon.ico")
  )
}

# Lógica del servidor para manejar la autenticación y el contenido del dashboard
server <- function(input, output, session) {
  useAutoColor()
  
  output$sidebar_content <- renderUI({
    sidebarMenu(
      id = "sidebar",
      menuItem(
        text = "Introduction",
        tabName = "tab1",
        icon = icon("van-shuttle"),
        selected = TRUE
      ),
      menuItem(
        text = "Demographic Analysis",
        tabName = "tab2",
        icon = icon("shuttle-space")
      ),
      menuItem(
        text = "Data and customer analysis",
        tabName = "tab3",
        icon = icon("shuttle-space")
      ),
      menuItem(
        text = "About of me",
        tabName = "tab4",
        icon = icon("shuttle-space")
      )
    )
    
  })
  
  # Contenido del dashboard condicionado al estado de autenticación
  output$content <- renderUI({
    
    tabItems(
      tabItem(
        tabName = "tab1",
        "Tabla 1"
      ),
      tabItem(
        tabName = "tab2",
        "Tabla 2"
      ),
      tabItem(
        tabName = "tab3",
        "Tabla 3"
      ),
      tabItem(
        tabName = "tab4",
        "Tabla 4"
      )
    )
    
  })
  
  observe({
    updateTabItems(session, "sidebar", selected = "tab1")
  })
  
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)

