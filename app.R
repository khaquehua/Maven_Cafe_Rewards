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
      # Incluir el JavaScript directamente
      tags$head(
        tags$script(HTML(
          "
          document.addEventListener('DOMContentLoaded', function() {
            document.querySelectorAll('.sidebar .nav-item .nav-link').forEach(function(el) {
              if (el.textContent.includes('?')) {
                el.addEventListener('click', function() {
                  alert('Activaste el botón de ayuda!');
                });
              }
            });
          });
          "
        ))
      )
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
        icon = icon("info"),
        selected = TRUE
      ),
      menuItem(
        text = "Demographic Analysis",
        tabName = "tab2",
        icon = icon("users")
      ),
      menuItem(
        text = "Data and customer analysis",
        tabName = "tab3",
        icon = icon("mug-saucer")
      ),
      menuItem(
        text = "About of me",
        tabName = "tab4",
        icon = icon("user")
      )
    )
  })
  
  # Contenido del dashboard condicionado al estado de autenticación
  output$content <- renderUI({
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
          column(
            width = 12,
            h1("Hola Prueba"),
            h3("Objetivo del Proyecto"),
            p("El objetivo de este proyecto es ofrecer una plataforma interactiva para visualizar datos de manera efectiva. A continuación, se detalla la estructura del dashboard:"),
            tags$ul(
              tags$li("En la parte izquierda del dashboard se encuentra la navegación del tablero."),
              tags$li("En la parte central se mostrará el contenido y los datos visualizados."),
              tags$li("Explore las diferentes secciones para obtener información detallada.")
            ),
            img(src = "https://via.placeholder.com/600x300", alt = "Imagen de Ejemplo", class = "img-fluid"),
            p("Aquí puedes poner más detalles sobre cómo utilizar el dashboard y qué esperar de cada sección.")
          )
        )
      ),
      tabItem(
        tabName = "tab2",
        "Contenido para Demographic Analysis"
      ),
      tabItem(
        tabName = "tab3",
        "Contenido para Data and Customer Analysis"
      ),
      tabItem(
        tabName = "tab4",
        "Contenido para About Me"
      )
    )
  })
  
  observe({
    updateTabsetPanel(session, "sidebar", selected = "tab1")
  })
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)

