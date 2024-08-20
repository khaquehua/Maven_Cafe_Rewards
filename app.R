#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::    LIBRARIES TO USE     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
library(jsonlite)
library(tidyr)

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::       READ THE DATA     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
customers <- read_csv("customers.csv")
events <- read_csv("events.csv")
offers <- read_csv("offers.csv")

#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#::::::::::::::::::::::       PREPROCESSING     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#For offers
offers$channels_list <- lapply(offers$channels, function(x) fromJSON(gsub("'", '"', x)))
create_binary_columns <- function(channels_list) {
  # Define possible column names
  possible_channels <- c("email", "mobile", "web", "social")
  
  # Create an vector of 0 and 1
  binary_vector <- setNames(rep(0, length(possible_channels)), possible_channels)
  
  # Update the vector with 1s for chanels
  binary_vector[channels_list] <- 1
  
  return(binary_vector)
}
offers <- offers %>%
  rowwise() %>%
  mutate(binary_columns = list(create_binary_columns(channels_list))) %>%
  unnest_wider(binary_columns) %>%
  select(-channels, -channels_list) 

#For customers
customers$age <- ifelse(customers$age==118,NA,customers$age) #Replace 118 for NA
customers$register <- ifelse(is.na(customers$gender),"Not added","Added")

#For events
events$value <- lapply(events$value, function(x) fromJSON(gsub("'", '"', x)))
events <- events %>% unnest_wider(value)
events$offer_id <- ifelse(is.na(events$`offer id`) & is.na(events$offer_id),"None",
                    ifelse(is.na(events$`offer id`),events$offer_id,events$`offer id`))
events <- events %>% select(-`offer id`)
events$amount <- ifelse(is.na(events$amount),0,events$amount) #Is important evaluated the amount is only in transactions
events$reward <- ifelse(is.na(events$reward),0,events$reward) #Is important evaluated the reward is only in offer completed
events$mount <- events$amount + events$reward
events <- events %>% select(-amount, -reward)  

#Data merge
data <- merge(customers, events, by = "customer_id")
data <- merge(data, offers, by = "offer_id")


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#:::::::::::::::::::    DEVELOPMENT THE APP     ::::::::::::::::::::::::::::::::
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Function to create the dashboard user interface
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
      uiOutput("content")
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

