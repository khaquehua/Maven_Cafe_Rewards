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
library(DT)

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
customers$became_member_on <- as.Date(as.character(customers$became_member_on), format = "%Y%m%d")
customers$personal_id <- sprintf("CUS%06d", seq(1, 17000))

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
    header = dashboardHeader(
      title = dashboardBrand(
        title = h4("Titulo pagina"),
        href = "https://www.clinicalafuente.com",
        image = "https://inventario.lafuentecsi.com/favicon.png"
      )
    ),
    body = dashboardBody(
      tags$script(HTML("
  $(document).on('shiny:connected', function() {
    function updatePlotBackground() {
      if (typeof Plotly === 'undefined') {
        setTimeout(updatePlotBackground, 100);  // Revisa nuevamente en 100ms
        return;
      }
      
      var mode = $('body').hasClass('dark-mode') ? 'dark' : 'light';
      var bgColor = mode === 'dark' ? '#454D55' : '#FFFFFF';
      var lineColor = mode === 'dark' ? 'white' : '#002003';
      var textColor = mode === 'dark' ? 'white' : '#002003';
      
      var plotlyElement = document.getElementById('plotcustomer_time');
      if (plotlyElement) {
        Plotly.relayout(plotlyElement, {
          'paper_bgcolor': bgColor,
          'plot_bgcolor': bgColor,
          'xaxis.color': textColor,  // Cambiar color del eje x
          'yaxis.color': textColor,  // Cambiar color del eje y
          'xaxis.title.font.color': textColor,  // Cambiar color del título del eje x
          'yaxis.title.font.color': textColor,  // Cambiar color del título del eje y
          'title.font.color': textColor,  // Cambiar color del título del gráfico
          'xaxis.gridcolor': lineColor,  // Cambiar color de las líneas de la grilla del eje x
          'yaxis.gridcolor': lineColor   // Cambiar color de las líneas de la grilla del eje y
        });

        Plotly.restyle(plotlyElement, {
          'line.color': [lineColor],   // Cambiar color de las líneas
          'marker.color': [lineColor]  // Cambiar color de los puntos
        });
      }
    }

    // Actualización inicial
    updatePlotBackground();

    // Actualizar al cambiar el modo
    var observer = new MutationObserver(updatePlotBackground);
    observer.observe(document.body, { attributes: true, attributeFilter: ['class'] });
  });
")),
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
          tabName = "subtab2_1",
          h2("Overview"),
          h3("Datasets Analysis"),
          p("In this first part, customer statistics, offers and events carried out separately will be shown in order to explain 
            the characteristics that each DATASET has as well as see possible trends and recommendations that can be made. 
            Below is relevant information about each DATASET found:"),
          tags$ul(
            tags$li("CUSTOMERS"),
            p("The following pre-processing techniques had to be performed for data analysis:"),
            tags$ul(
              tags$li("Replace the values of 118 with NA in the age variable"),
              tags$li("There are clients who do not have records of gender, age and income; It was observed that this absence of data happens in the same client, 
                      which is why the variable register (NOT ADDED, ADDED) was added, important to see if there is a large amount of missing data"),
              tags$li("A Custom ID was inserted in order to be able to perform a personal control per customer that would be observed in the Customer Analysis part")
            ),
            tags$li("OFFERS"),
            p("The following pre-processing techniques had to be performed for data analysis:"),
            tags$ul(
              tags$li("The chanels variable was deployed to transform them into a list and later into separate variables: 
                      web, email, mobile and social, in which if it is found in that part a 1 is placed, otherwise a 0")
            ),
            tags$li("EVENTS"),
            p("The following pre-processing techniques had to be performed for data analysis:"),
            tags$ul(
              tags$li("Variables were created separately from the variable based on the JSON file"),
              tags$li("It was observed that amount is only available in transactions of the event variable, likewise with reward in offer completed of the event variable."),
              tags$li("Seeing that, the mount variable was created that groups amount and reward as a single price, in the other events it remains as 0")
            )
          )
        ),
        tabItem(
          tabName = "subtab2_2",
          h1("Customers"),
          fluidRow(
            column(4, tags$div(
              class = "input-group",
              tags$span(
                tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                "Became member"
              ),
              dateRangeInput("selectcustomer_date", label = NULL, start = min(customers$became_member_on), 
                             end = max(customers$became_member_on))
            )),
            column(4, tags$div(
              class = "input-group",
              tags$span(
                tags$img(icon("rectangle-ad", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                " Register"
              ),
              selectInput("selectcustomer_register", label = NULL, choices = c("All", unique(customers$register)), selected = "All")
            )),
            column(4, tags$div(
              class = "input-group",
              tags$span(
                tags$img(icon("venus-mars", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                " Gender"
              ),
              selectInput("selectcustomer_gender", label = NULL, choices = c("All", unique(customers$gender)), selected = "All", multiple = TRUE)
            )),
          ),
          fluidRow(
            column(6, tags$div(
              class = "input-group",
              tags$span(
                tags$img(icon("person", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                " Age"
              ),
              sliderInput("selectcustomer_age", "", 
                          min = min(customers$age, na.rm = TRUE), max = max(customers$age, na.rm = TRUE), 
                          value = c(min(customers$age, na.rm = TRUE), max(customers$age, na.rm = TRUE)))
            )),
            column(6, tags$div(
              class = "input-group",
              tags$span(
                tags$img(icon("hand-holding-dollar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
                " Income"
              ),
              sliderInput("selectcustomer_income", "", 
                          min = min(customers$income, na.rm = TRUE), max = max(customers$income, na.rm = TRUE), 
                          value = c(min(customers$income, na.rm = TRUE), max(customers$income, na.rm = TRUE)))
            ))
          ),
          fluidRow(
            column(6, infoBoxOutput("client", width = 12)),
            column(6, infoBoxOutput("amount", width = 12))
          ),
          div(
            fluidRow(
              column(8,
                     tabBox(
                       title = "Customers evolution",
                       selected = "Graph",
                       solidHeader = FALSE,
                       maximizable = TRUE,
                       width = 12,
                       type = "tabs",
                       tabPanel(
                         title = "Graph",
                         width = 12,
                         plotlyOutput("plotcustomer_time", height = "600px")
                       ),
                       tabPanel(
                         title = "Table",
                         width = 12,
                         downloadButton("downloadExcelcustomer_time", "Download Excel"),
                         dataTableOutput('tabcustomer_time')
                       ),
                       tabPanel(
                         title = "Overview",
                         width = 12,
                         p("Se encontro la tendencia")
                       )
                     )
              ),
              column(4,
                     tabBox(
                       title = "Age of customers",
                       selected = "Graph",
                       solidHeader = FALSE,
                       maximizable = TRUE,
                       width = 12,
                       type = "tabs",
                       tabPanel(
                         title = "Graph",
                         width = 12,
                         plotlyOutput("plotcustomers_age", height = "600px")
                       ),
                       tabPanel(
                         title = "Table",
                         width = 12,
                         downloadButton("downloadExcelcustomers_age", "Download Excel"),
                         dataTableOutput('tabcustomers_age')
                       ),
                       tabPanel(
                         title = "Overview",
                         width = 12,
                         p("Se encontro la tendencia")
                       )
                     )
                  )
            ),
            style = "margin-top: 20px;"
        ),
        div(
          fluidRow(
            column(6,
                   tabBox(
                     title = "Gender of customers",
                     selected = "Graph",
                     solidHeader = FALSE,
                     maximizable = TRUE,
                     width = 12,
                     type = "tabs",
                     tabPanel(
                       title = "Graph",
                       width = 12,
                       plotlyOutput("plotcustomers_gender", height = "600px")
                     ),
                     tabPanel(
                       title = "Table",
                       width = 12,
                       downloadButton("downloadExcelcustomers_gender", "Download Excel"),
                       dataTableOutput('tabcustomers_gender')
                     ),
                     tabPanel(
                       title = "Overview",
                       width = 12,
                       p("Se encontro la tendencia")
                     )
                   )
            ),
            column(6,
                   tabBox(
                     title = "Register of customers",
                     selected = "Graph",
                     solidHeader = FALSE,
                     maximizable = TRUE,
                     width = 12,
                     type = "tabs",
                     tabPanel(
                       title = "Graph",
                       width = 12,
                       plotlyOutput("plotcustomers_register", height = "600px")
                     ),
                     tabPanel(
                       title = "Table",
                       width = 12,
                       downloadButton("downloadExcelcustomers_register", "Download Excel"),
                       dataTableOutput('tabcustomers_register')
                     ),
                     tabPanel(
                       title = "Overview",
                       width = 12,
                       p("Se encontro la tendencia")
                     )
                   )
            )
          ),
          style = "margin-top: 20px;"
        )),
        tabItem(
          tabName = "subtab2_3",
          h2("Offers"),
          p("Aquí se muestra la distribución de edades.")
        ),
        tabItem(
          tabName = "subtab2_4",
          h2("Events"),
          p("Aquí se muestra la distribución de edades.")
        ),
        tabItem(
          tabName = "tab3",
          h2("Data and Customer Analysis"),
          p("Contenido para Data and Customer Analysis.")
        ),
        tabItem(
          tabName = "tab4",
          h2("About Me"),
          p("Contenido para About Me.")
        )
      )
    ),
    sidebar = dashboardSidebar(
      skin = "light",
      inputId = "sidebarState",
      sidebarMenu(
        id = "sidebar",
        menuItem(
          text = "Introduction",
          tabName = "tab1",
          icon = icon("info"),
          selected = TRUE  # Selecciona tab1 por defecto
        ),
        menuItem(
          text = "Demographic Analysis",
          icon = icon("users"),
          startExpanded = FALSE,  # No está expandido por defecto para permitir el comportamiento de expandir/contraer
          menuSubItem(
            text = "Overview",
            tabName = "subtab2_1",
            icon = icon("info")
          ),
          menuSubItem(
            text = "Customers",
            tabName = "subtab2_2",
            icon = icon("users")
          ),
          menuSubItem(
            text = "Offers",
            tabName = "subtab2_3",
            icon = icon("tty")
          ),
          menuSubItem(
            text = "Events",
            tabName = "subtab2_4",
            icon = icon("calendar-check")
          )
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

# Server
server <- function(input, output, session) {
  useAutoColor()
  theme_mode <- reactiveVal("light")
  
  observeEvent(session$clientData$themeMode, {
    theme_mode(session$clientData$themeMode)
  })
  
  filtered_customer <- reactive({
    selected_date <- input$selectcustomer_date
    selected_gender <- input$selectcustomer_gender
    selected_age <- input$selectcustomer_age
    selected_income <- input$selectcustomer_income
    selected_register <- input$selectcustomer_register
    
    filtered <- customers
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_gender) > 1,
      length(selected_register) > 1,
      !is.null(selected_date),
      !is.null(selected_age),
      !is.null(selected_income)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- customers
      
      if (!"All" %in% selected_gender) {
        filtered <- filtered[filtered$gender %in% selected_gender, ]
      }
      
      if (!"All" %in% selected_register) {
        filtered <- filtered[filtered$register == selected_register, ]
      }
      
      if (!is.null(selected_date)) {
        filtered <- filtered[filtered$became_member_on >= selected_date[1] & filtered$became_member_on <= selected_date[2], ]
      }
      
      if (!is.null(selected_age)) {
        filtered <- filtered[filtered$age >= selected_age[1] & filtered$age <= selected_age[2], ]
      }
      
      if (!is.null(selected_income)) {
        filtered <- filtered[filtered$income >= selected_income[1] & filtered$income <= selected_income[2], ]
      }
      
      
    } else {
      # Si no hay filtros, devolver el conjunto de datos completo
      filtered <- customers
    }
    
    return(filtered)
  })
  
  output$client <- renderInfoBox({
    
    filtered <- filtered_customer()
    
    a <- filtered %>% group_by(register) %>% summarise(Count = n()) %>% summarise(Count = sum(Count))
    a <- as.integer(a)
    infoBox(
      "Customers", a, " Customers registered", icon = icon("user-check"),
      color = "primary",
      width = NULL
    )
  })
  
  output$amount <- renderInfoBox({
    
    filtered <- filtered_customer()
    
    b <- sum(filtered$income, na.rm = TRUE)
    
    # Formatear la cantidad con el símbolo de dólar y separadores de miles
    formatted_b <- scales::dollar(b, accuracy = 1)
    
    infoBox(
      "Incomes", formatted_b, " Total", icon = icon("money-bill-trend-up"),
      color = "success",
      width = NULL
    )
  })
  
  output$plotcustomer_time <- renderPlotly({
    filtered <- customers
    
    if (nrow(filtered) == 0) {
      return(plot_ly(type = "scatter", mode = "lines+markers") %>%
               layout(title = "No data available\nfor selected filters"))
    }
    
    customers_per_day <- filtered %>%
      group_by(Year = format(became_member_on, "%Y-%m")) %>%
      summarise(Count = n(), Income = sum(income, na.rm = TRUE))
    
    Plot_customer_evolution <- ggplot(customers_per_day, aes(x = Year, y = Count, 
                                                             text = paste0(Count, " customers\n",
                                                                           Income, " income\n",
                                                                           Year))) + 
      geom_line(aes(group = 1, color = I("#002003"))) + 
      geom_point(aes(color = I("#002003")), size = 0.5, shape = 21, stroke = 2) +
      theme_minimal(base_family = "Arial", base_size = 15) +
      labs(x = "Date", y = "Count (n)", title = "Evolution of customers") +
      theme(
        plot.title = element_text(face = 'bold', hjust = 0.5, size = 15),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 8, angle = 90),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(face = 'bold', size = 14),
        axis.title.y = element_text(face = 'bold', size = 14)
      )
    
    ggplotly(Plot_customer_evolution, tooltip = "text")
  })
  
  output$downloadExcelcustomer_time <- downloadHandler(
    filename = function() {
      paste("Cust_Time", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      filtered <- customers
      
      customers_per_day <- filtered %>%
        group_by(Year = format(became_member_on, "%Y-%m")) %>%
        summarise(Count = n(), Income = sum(income, na.rm = TRUE))
      
      write.xlsx(customers_per_day, file, sheetName = "Cust_Time", row.names = FALSE)
    }
  )
  
  output$tabcustomer_time <- renderDataTable({
    
    filtered <- customers 
    
    customers_per_day <- filtered %>%
      group_by(Year = format(became_member_on, "%Y-%m")) %>%
      summarise(Count = n(), Income = sum(income, na.rm = TRUE))
    
    datatable(
      customers_per_day
    )
  })
  
  output$plotcustomers_age <- renderPlotly({
    
    filtered <- customers %>% filter(!is.na(age))
    
    if (nrow(filtered) == 0) {
      return(plot_ly(type = "histogram") %>%
               layout(title = "No data available\nfor selected filters"))
    }
    
    customer_unique <- filtered %>% distinct(customer_id, .keep_all = TRUE)
    
    Plot_customer_age <- ggplot(customer_unique, aes(x = age, y = after_stat(density))) +
      geom_histogram(binwidth = 10, boundary = 0.5, color = "#007bff", fill = "white", size = 1) +
      geom_density(color = "red", linewidth = 1.5) +
      scale_x_continuous(breaks = seq(0, 100, by = 10), labels = seq(0, 100, by = 10)) +
      labs(x = "Age", y = "", title = "Age of customers") +
      theme_economist() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.y = element_blank(),
            plot.background = element_rect(fill = "#DEEBF7"))
    
    ggplotly(Plot_customer_age, tooltip = "text")
  })
  
  output$downloadExcelcustomers_age <- downloadHandler(
    filename = function() {
      paste("Cust_Age", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      filtered <- customers
      
      customers_per_day <- filtered %>%
        group_by(Year = format(became_member_on, "%Y-%m")) %>%
        summarise(Count = n(), Income = sum(income, na.rm = TRUE))
      
      write.xlsx(customers_per_day, file, sheetName = "Cust_Age", row.names = FALSE)
    }
  )
  
  output$tabcustomers_age <- renderDataTable({
    
    filtered <- customers 
    
    customers_per_day <- filtered %>%
      group_by(Year = format(became_member_on, "%Y-%m")) %>%
      summarise(Count = n(), Income = sum(income, na.rm = TRUE))
    
    datatable(
      customers_per_day
    )
  })
  
  output$plotcustomers_gender <- renderPlotly({
    
    filtered <- customers
    
    if (nrow(filtered) == 0) {
      # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
      return(plot_ly(type = "bar") %>%
               layout(title = "No data available\nfor selected filters"))
    }
    
    gender <- filtered %>%
      group_by(gender) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = prop.table(Count)*100)
    
    colors <- colorRampPalette(c("white", "#007bff"))
    
    gender <- gender %>%
      mutate(Color = colors(length(unique(Count)))[as.numeric(factor(Count))])
    
    Plot_gender <- ggplot(gender, aes(x = reorder(gender, +Count), y = Count, 
                                text = paste0("Gender: ", gender, "\n",
                                              "Count: ", Count, "\n",
                                              "Percentage: ", round(Percentage,2), "%"))) +
      geom_bar(stat = "identity", aes(fill = Color)) +
      coord_flip() +
      geom_hline(aes(yintercept = mean(Count)),
                 color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
      labs(x = "", y = "Count", title = "Gender of customers") +
      theme_economist() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.background = element_rect(fill = "#DEEBF7"),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14)) +
      scale_fill_identity()
    
    ggplotly(Plot_gender, tooltip = "text")
  })
  
  output$downloadExcelcustomers_gender <- downloadHandler(
    filename = function() {
      paste("Cust_Age", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      filtered <- customers
      
      customers_per_day <- filtered %>%
        group_by(Year = format(became_member_on, "%Y-%m")) %>%
        summarise(Count = n(), Income = sum(income, na.rm = TRUE))
      
      write.xlsx(customers_per_day, file, sheetName = "Cust_Age", row.names = FALSE)
    }
  )
  
  output$tabcustomers_gender <- renderDataTable({
    
    filtered <- customers 
    
    customers_per_day <- filtered %>%
      group_by(Year = format(became_member_on, "%Y-%m")) %>%
      summarise(Count = n(), Income = sum(income, na.rm = TRUE))
    
    datatable(
      customers_per_day
    )
  })
  
  output$plotcustomers_register <- renderPlotly({
    
    filtered <- customers
    
    if (nrow(filtered) == 0) {
      # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
      return(plot_ly(type = "bar") %>%
               layout(title = "No data available\nfor selected filters"))
    }
    
    gender <- filtered %>%
      group_by(gender) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = prop.table(Count)*100)
    
    colors <- colorRampPalette(c("white", "#007bff"))
    
    gender <- gender %>%
      mutate(Color = colors(length(unique(Count)))[as.numeric(factor(Count))])
    
    Plot_gender <- ggplot(gender, aes(x = reorder(gender, +Count), y = Count, 
                                      text = paste0("Gender: ", gender, "\n",
                                                    "Count: ", Count, "\n",
                                                    "Percentage: ", round(Percentage,2), "%"))) +
      geom_bar(stat = "identity", aes(fill = Color)) +
      coord_flip() +
      geom_hline(aes(yintercept = mean(Count)),
                 color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
      labs(x = "", y = "Count", title = "Gender of customers") +
      theme_economist() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            plot.background = element_rect(fill = "#DEEBF7"),
            axis.title.x = element_text(face = 'bold', size = 14),
            axis.title.y = element_text(face = 'bold', size = 14)) +
      scale_fill_identity()
    
    ggplotly(Plot_gender, tooltip = "text")
  })
  
  output$downloadExcelcustomers_register <- downloadHandler(
    filename = function() {
      paste("Cust_Age", Sys.Date(), ".xlsx", sep = "_")
    },
    content = function(file) {
      filtered <- customers
      
      customers_per_day <- filtered %>%
        group_by(Year = format(became_member_on, "%Y-%m")) %>%
        summarise(Count = n(), Income = sum(income, na.rm = TRUE))
      
      write.xlsx(customers_per_day, file, sheetName = "Cust_Age", row.names = FALSE)
    }
  )
  
  output$tabcustomers_register <- renderDataTable({
    
    filtered <- customers 
    
    customers_per_day <- filtered %>%
      group_by(Year = format(became_member_on, "%Y-%m")) %>%
      summarise(Count = n(), Income = sum(income, na.rm = TRUE))
    
    datatable(
      customers_per_day
    )
  })
  
  
  
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)
