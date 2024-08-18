library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(DT)
library(tm)
library(DBI)
library(RMySQL)
library(htmltools)
library(digest)
library(treemapify)
library(openxlsx)

update_mysql_data <- function() {
  dbjosue <- dbConnect(MySQL(), user="yyliipmy_kevin", host="50.87.187.137", password="++LaFuente2023**", dbname="yyliipmy_inventario")
  users <- dbGetQuery(dbjosue, statement = "select * from users")
  users <- users %>% select(id, name, email, department_id)
  colnames(users) <- c("user_id","user_name","user_email","department_id")
  user_lend <- users %>% select(user_id, user_name)
  colnames(user_lend) <- c("user_lend","user_lend_name")
  user_receive <- users %>% select(user_id, user_name)
  colnames(user_receive) <- c("user_recieve","user_receive_name")
  
  departments <- dbGetQuery(dbjosue, statement = "select * from departments")
  departments <- departments %>% select(id, name, description, business)
  colnames(departments) <- c("department_id","department_name","department_description","department_business")
  
  u_d <- merge(users, departments, by = "department_id")
  colnames(u_d) <- c("department_user_id","user_id","user_name","user_email","department_user_name",
                     "department_user_description","department_user_business")
  
  brands <- dbGetQuery(dbjosue, statement = "select * from brands")
  brands <- brands %>% select(id, name, description)
  colnames(brands) <- c("brand_id","brand_name","brand_description")
  
  events <- dbGetQuery(dbjosue, statement = "select * from events")
  events <- events %>% select(id, title, start_date, end_date, allDay, type)
  colnames(events) <- c("event_id","event_title","event_start_date","event_end_date","event_allDay","event_type")
  
  categories <- dbGetQuery(dbjosue, statement = "select * from categories")
  categories <- categories %>% select(id, name, description)
  colnames(categories) <- c("category_id","categorie_name","categorie_description")
  
  devices <- dbGetQuery(dbjosue, statement = "select * from devices")
  devices <- devices %>% select(id, name, description, serial, model, enabled, observations, 
                                problem, code_bar, price, category_id, user_id, user_lend, 
                                user_recieve, brand_id, department_id, created_at) 
  colnames(devices) <- c("device_id","device_name","device_description","device_serial","device_model",
                         "device_enabled","device_observations","device_problem","device_code_bar","device_price",
                         "category_id","user_id","user_lend","user_recieve","brand_id","department_id", "created_at")
  
  data_merge <- merge(devices, categories, by = "category_id")
  data_merge <- merge(data_merge, u_d, by = "user_id")
  data_merge <- merge(data_merge, user_lend, by = "user_lend")
  data_merge <- merge(data_merge, user_receive, by = "user_recieve")
  data_merge <- merge(data_merge, brands, by = "brand_id")
  data_merge <- merge(data_merge, departments, by = "department_id")
  
  bd <- data_merge[,-1:-7]
  bd <- bd %>% select(-department_user_id)
  
  bd$device_enabled <- ifelse(bd$device_enabled == 0, "Inhabilitado", "Habilitado")
  bd$device_serial <- ifelse(is.na(bd$device_serial), "Sin serie", bd$device_serial)
  bd$device_model <- ifelse(is.na(bd$device_model), "Sin modelo", bd$device_model)
  bd$device_observations <- ifelse(is.na(bd$device_observations), "Sin observaciones", bd$device_observations)
  bd$device_problem <- ifelse(is.na(bd$device_problem), "Sin problemas", bd$device_problem)
  bd$created_at <- as.Date(bd$created_at)
  
  #CONDICIONAL POR USUARIO
  
  dbDisconnect(dbjosue)
  return(bd)
}

etiquetas <- function(x) {
  paste0("S/. ",x)
}

# Función para crear la interfaz de usuario del dashboard
dashboard_ui <- function() {
  dashboardPage(
    title = "La Fuente - Patrimonio",
    dashboardHeader(
      title = dashboardBrand(
        title = h4("Estadísticas"),
        href = "https://www.clinicalafuente.com",
        image = "https://inventario.lafuentecsi.com/favicon.png"
      )
    ),
    dashboardSidebar(
      uiOutput("sidebar_content")
    ),
    dashboardBody(
      tags$style(HTML("
        #updateButton-container {
        display: flex;
        justify-content: center;
        margin-top: 10px; /* Espacio arriba del botón */
        margin-bottom: 20px; /* Espacio abajo del botón */
        }
        
        .dynamic-title-light {color: #000000 !important;} /* Texto fijo en negro para modo claro */
        .dynamic-title-dark {color: #ffffff !important;} /* Texto fijo en blanco para modo oscuro */
        .registros {color: #007bff !important; text-decoration: underline;} /* Azul primario para número de registros en modo claro */
        .registros-dark {color: #C6FBFC !important; text-decoration: underline;} /* Gris oscuro para número de registros en modo oscuro */
      ")),
      tags$script(HTML("
    $(document).on('shiny:connected', function() {
      function updatePlotBackground() {
        var mode = $('body').hasClass('dark-mode') ? 'dark' : 'light';
        var bgColor = mode === 'dark' ? '#454D55' : '#FFFFFF';
        
        var plotlyElements = ['SJgraf', 'MGgraf'];  // Lista de IDs de elementos plotly
        
        plotlyElements.forEach(function(plotlyId) {
          var plotlyElement = document.getElementById(plotlyId);
          if (plotlyElement) {
            Plotly.relayout(plotlyElement, {
              'paper_bgcolor': bgColor,
              'plot_bgcolor': bgColor
            });
          }
        });
      }

      // Initial update
      updatePlotBackground();

      // Update on mode switch
      var observer = new MutationObserver(updatePlotBackground);
      observer.observe(document.body, { attributes: true, attributeFilter: ['class'] });
    });
  ")),
      uiOutput("content"), # Uso de uiOutput para el contenido condicionado
    ),
    footer = bs4DashFooter(
      left = "Developed by: Kevin Heberth Haquehua Apaza / Statistical / Mathematical / Data Science / Data Analyst / Systems analyst",
      right = tagList(
        tags$a(href = "https://www.linkedin.com/in/kevinhaquehua/", target = "_blank", 
               icon("linkedin", lib = "font-awesome"), style = "margin-left: 10px;"),
        tags$a(href = "https://github.com/khaquehua", target = "_blank", 
               icon("github", lib = "font-awesome"), style = "margin-left: 10px;")
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
  data <- reactiveValues(bd = NULL)
  
  update_data <- function() {
    data$bd <- update_mysql_data()
  }
  
  update_data()
  
  autoInvalidate <- reactiveTimer(300000) #5 minutos = 300000 milisegundos
  
  output$sidebar_content <- renderUI({
    req(data$bd)
    tagList(
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Fecha registro patrimonio"
        ),
        dateRangeInput("selectDate", label = NULL, start = min(data$bd$created_at), end = max(data$bd$created_at))
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("truck-arrow-right", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Usuario entregando"
        ),
        selectInput("selectUserlend", label = NULL, choices = c("All", names(table(data$bd$user_lend_name))), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("clipboard-check", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Usuario recibe"
        ),
        selectInput("selectUserreceive", label = NULL, choices = c("All", names(table(data$bd$user_receive_name))), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("location-dot", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Ubicación"
        ),
        selectInput("selectUbicacion", label = NULL, choices = c("All", unique(data$bd$department_business)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("house-medical", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Area"
        ),
        selectInput("selectArea", label = NULL, choices = c("All", unique(data$bd$department_name)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("barcode", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Categoria"
        ),
        selectInput("selectCategoria", label = NULL, choices = c("All", unique(data$bd$categorie_name)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("xmarks-lines", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Marca"
        ),
        selectInput("selectMarca", label = NULL, choices = c("All", unique(data$bd$brand_name)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("user", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Usuario"
        ),
        selectInput("selectUsuario", label = NULL, choices = c("All", unique(data$bd$user_name)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("toggle-on", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Estado"
        ),
        selectInput("selectHabilitado", label = NULL, choices = c("All", unique(data$bd$device_enabled)), selected = "All")
      ),
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      fluidRow(
        column(11,
               div(
                 actionButton("updateButton", "Actualizar datos", class = "btn btn-primary"),
                 id = "updateButton-container"
               )
        )
      )
    )
  })
  
  filtered_data <- reactive({
    
    selected_user_lend <- input$selectUserlend # Persona que envia
    selected_user_receive <- input$selectUserreceive # Persona que recibe
    selected_ubicacion <- input$selectUbicacion # Ubicacion
    selected_area <- input$selectArea # Area
    selected_categoria <- input$selectCategoria # Categoria
    selected_marca <- input$selectMarca # Marca
    selected_user <- input$selectUsuario # Usuario
    selected_enabled <- input$selectHabilitado # Habilitado
    selected_date <- input$selectDate # Fecha de registro
    
    filtered <- data$bd
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_user_lend) > 1,
      length(selected_user_receive) > 1,
      length(selected_ubicacion) > 1,
      length(selected_area) > 1,
      length(selected_categoria) > 1,
      length(selected_marca) > 1,
      length(selected_user) > 1,
      length(selected_enabled) > 1,
      !is.null(selected_date)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$bd
      
      if (!"All" %in% selected_user_lend) {
        filtered <- filtered[filtered$user_lend_name %in% selected_user_lend, ]
      }
      
      if (!"All" %in% selected_user_receive) {
        filtered <- filtered[filtered$user_receive_name == selected_user_receive, ]
      }
      
      if (!"All" %in% selected_ubicacion) {
        filtered <- filtered[filtered$department_business == selected_ubicacion, ]
      }
      
      if (!"All" %in% selected_area) {
        filtered <- filtered[filtered$department_name == selected_area, ]
      }
      
      if (!"All" %in% selected_categoria) {
        filtered <- filtered[filtered$categorie_name == selected_categoria, ]
      }
      
      if (!"All" %in% selected_marca) {
        filtered <- filtered[filtered$brand_name == selected_marca, ]
      }
      
      if (!"All" %in% selected_user) {
        filtered <- filtered[filtered$user_name == selected_user, ]
      }
      
      if (!"All" %in% selected_enabled) {
        filtered <- filtered[filtered$device_enabled == selected_enabled, ]
      }
      
      if (!is.null(selected_date)) {
        filtered <- filtered[filtered$created_at >= selected_date[1] & filtered$created_at <= selected_date[2], ]
      }
    } else {
      # Si no hay filtros, devolver el conjunto de datos completo
      filtered <- data$bd
    }
    
    return(filtered)
  })
  
  # Contenido del dashboard condicionado al estado de autenticación
  output$content <- renderUI({
    
    req(data$bd)
    
    tabsetPanel(
      type = "tabs",
      tabPanel(
        HTML(paste(icon("boxes-packing"), "Análisis registros")),
        fluidRow(
          column(12, uiOutput("dynamicTitle")),
          column(4, infoBoxOutput("habilitados", width = 12)),
          column(4, infoBoxOutput("sin_observaciones", width = 12)),
          column(4, infoBoxOutput("sin_problemas", width = 12))
        ),
        div(
          fluidRow(
            column(8, 
                   box(
                     title = "Evolución registros realizados",
                     status = "primary",
                     solidHeader = TRUE,
                     maximizable = TRUE,
                     width = 12,  # Asegúrate de que el ancho del box ocupe toda la columna
                     plotlyOutput("plot1")
                   )
            ),
            column(4, 
                   box(
                     title = "Montos registros por trabajador",
                     status = "primary",
                     solidHeader = TRUE,
                     maximizable = TRUE,
                     width = 12,  # Asegúrate de que el ancho del box ocupe toda la columna
                     plotlyOutput("plot2")
                   )
            )
          ),
          style = "margin-top: 20px;"  # Ajusta el valor según tus preferencias
        ),
        div(
          fluidRow(
            column(6, 
                   box(
                     title = "Registros por categoría",
                     status = "primary",
                     solidHeader = TRUE,
                     maximizable = TRUE,
                     width = 12,
                     downloadButton("downloadExcelcategorie", "Descargar Excel"),
                     dataTableOutput('tabreg_cat')
                   )
            ),
            column(6, 
                   box(
                     title = "Registros por marca",
                     status = "primary",
                     solidHeader = TRUE,
                     maximizable = TRUE,
                     width = 12,
                     downloadButton("downloadExcelbrand", "Descargar Excel"),
                     dataTableOutput('tabreg_brand')
                   )
            )
          ),
          style = "margin-top: 20px;"  # Ajusta el valor según tus preferencias
        )
      ),
      tabPanel(
        HTML(paste(icon("dolly"), "Análisis Inventario")),
        fluidRow(
          column(6, infoBoxOutput("monto_total", width = 12)),
          column(6, infoBoxOutput("cantidad", width = 12))
        ),
        div(
          fluidRow(
            column(6,
                   tabBox(
                     title = "San Jerónimo",
                     selected = "Grafico",
                     status = "primary",
                     solidHeader = FALSE,
                     maximizable = TRUE,
                     width = 12,
                     type = "tabs",
                     tabPanel(
                       title = "Grafico",
                       width = 12,
                       plotlyOutput("SJgraf", height = "600px")
                     ),
                     tabPanel(
                       title = "Tabla",
                       width = 12,
                       downloadButton("downloadExcelSJ", "Descargar Excel"),
                       dataTableOutput('tabSJ')
                     )
                   )
            ),
            column(6, 
                   tabBox(
                     title = "Magisterio",
                     selected = "Grafico",
                     status = "primary",
                     solidHeader = FALSE,
                     maximizable = TRUE,
                     width = 12,
                     type = "tabs",
                     tabPanel(
                       title = "Grafico",
                       width = 12,
                       plotlyOutput("MGgraf", height = "600px")
                     ),
                     tabPanel(
                       title = "Tabla",
                       width = 12,
                       downloadButton("downloadExcelMG", "Descargar Excel"),
                       dataTableOutput('tabMG')
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
                     title = "Trabajadores Entregaron",
                     selected = "Grafico",
                     status = "primary",
                     solidHeader = FALSE,
                     maximizable = TRUE,
                     width = 12,
                     type = "tabs",
                     tabPanel(
                       title = "Grafico",
                       width = 12,
                       plotlyOutput("Entgraf", height = "600px")
                     ),
                     tabPanel(
                       title = "Tabla",
                       width = 12,
                       downloadButton("downloadExcelEnt", "Descargar Excel"),
                       dataTableOutput('tabEnt')
                     )
                   )
            ),
            column(6,
                   tabBox(
                     title = "Trabajadores Recibieron",
                     selected = "Grafico",
                     status = "primary",
                     solidHeader = FALSE,
                     maximizable = TRUE,
                     width = 12,
                     type = "tabs",
                     tabPanel(
                       title = "Grafico",
                       width = 12,
                       plotlyOutput("Recgraf", height = "600px")
                     ),
                     tabPanel(
                       title = "Tabla",
                       width = 12,
                       downloadButton("downloadExcelRec", "Descargar Excel"),
                       dataTableOutput('tabRec')
                     )
                   )
            )
          ),
          style = "margin-top: 20px;"  # Ajusta el valor según tus preferencias
        )
      ),
      tabPanel(
        HTML(paste(icon("list-ul"), "Lista de Productos")),
        fluidRow(
          column(12, 
                 box(
                   title = "Lista Patrimonio",
                   status = "primary",
                   solidHeader = TRUE,
                   maximizable = TRUE,
                   width = 12,
                   downloadButton("downloadExcel3", "Descargar Tabla Excel"),
                   DT::dataTableOutput('tab2')
                 )
          )
        )
      )
    )
  })
  
  observeEvent(input$updateButton, {
    showModal(modalDialog(
      title = "Actualizando datos",
      "Por favor espere mientras se actualizan los datos.",
      easyClose = FALSE,
      footer = NULL
    ))
    update_data()
    
    removeModal()
  })
  
  # Datos y gráfico solo se renderizan si el usuario está autenticado
  observe({
    
    output$dynamicTitle <- renderUI({
      total_registros <- nrow(filtered_data())
      titulo <- paste0(
        "<span class='dynamic-title-light'>Total registros realizados: </span>",
        "<span class='registros'>",
        total_registros,
        " registros</span>"
      )
      
      if (isTRUE(input$dark_mode)) {
        titulo <- paste0(
          "<span class='dynamic-title-dark'>Total registros realizados: </span>",
          "<span class='registros-dark'>",
          total_registros,
          " registros</span>"
        )
      }
      
      h3(HTML(titulo), style = "font-weight: bold; text-align: center; margin-bottom: 20px;")
    })
    
    output$habilitados <- renderInfoBox({
      filtered <- filtered_data()
      
      enabled <- filtered %>% group_by(device_enabled) %>% summarise(Cantidad = n()) %>%
        mutate(Proporcion = prop.table(Cantidad)*100) 
      
      habilitados <- enabled %>% filter(device_enabled == "Habilitado")
      habilitado_cant <- ifelse(is.na(as.integer(habilitados[,2])),0,as.integer(habilitados[,2]))
      habilitado_prop <- ifelse(is.na(as.double(habilitados[,3])),0,as.double(habilitados[,3]))
      
      inhabilitados <- enabled %>% filter(device_enabled == "Inhabilitado")
      inhabilitado_cant <- ifelse(is.na(as.integer(inhabilitados[,2])),0,as.integer(inhabilitados[,2]))
      inhabilitado_prop <- ifelse(is.na(as.double(inhabilitados[,3])),0,as.double(inhabilitados[,3]))
      
      infoBox(
        paste0("Patrimonios habilitados ", habilitado_cant, " registros"), 
        paste0(round(habilitado_prop,2),"%"), paste0(inhabilitado_cant," registros inhabilitados: ",round(inhabilitado_prop,2),"%"), 
        icon = icon("toggle-on"),
        color = "orange",
        width = NULL
      )
    })
    
    output$sin_observaciones <- renderInfoBox({
      
      filtered <- filtered_data()
      
      observations <- filtered %>% group_by(device_observations) %>% summarise(Cantidad = n()) %>%
        mutate(Proporcion = prop.table(Cantidad)*100) 
      
      sin_observaciones <- observations %>% filter(device_observations == "Sin observaciones")
      sin_obs_cant <- ifelse(is.na(as.integer(sin_observaciones[,2])),0,as.integer(sin_observaciones[,2]))
      sin_obs_prop <- ifelse(is.na(as.double(sin_observaciones[,3])),0,as.double(sin_observaciones[,3]))
      
      observados <- observations %>% filter(device_observations != "Sin observaciones")
      observados_cant <- ifelse(is.na(as.integer(sum(observados$Cantidad))),0,as.integer(sum(observados$Cantidad)))
      observados_prop <- ifelse(is.na(as.double(sum(observados$Proporcion))),0,as.double(sum(observados$Proporcion)))
      
      infoBox(
        paste0("Sin observaciones ", sin_obs_cant, " registros"), 
        paste0(round(sin_obs_prop,2),"%"), 
        paste0(observados_cant," registros observados: ",round(observados_prop,2),"%"), 
        icon = icon("circle-exclamation"),
        color = "primary",
        width = NULL
      )
    })
    
    output$sin_problemas <- renderInfoBox({
      
      filtered <- filtered_data()
      
      problems <- filtered %>% group_by(device_problem) %>% summarise(Cantidad = n()) %>%
        mutate(Proporcion = prop.table(Cantidad)*100) 
      
      sin_problemas <- problems %>% filter(device_problem == "Sin problemas")
      sin_prob_cant <- ifelse(is.na(as.integer(sin_problemas[,2])),0,as.integer(sin_problemas[,2]))
      sin_prob_prop <- ifelse(is.na(as.double(sin_problemas[,3])),0,as.double(sin_problemas[,3]))
      
      problemas <- problems %>% filter(device_problem != "Sin problemas")
      problemas_cant <- ifelse(is.na(as.integer(sum(problemas$Cantidad))),0,as.integer(sum(problemas$Cantidad)))
      problemas_prop <- ifelse(is.na(as.double(sum(problemas$Proporcion))),0,as.double(sum(problemas$Proporcion)))
      
      infoBox(
        paste0("Sin problemas ", sin_prob_cant, " registros"), paste0(round(sin_prob_prop,2),"%"), 
        paste0(problemas_cant," registros con problemas: ",round(problemas_prop,2),"%"), 
        icon = icon("square-check"),
        color = "success",
        width = NULL
      )
    })
    
    output$porcentaje <- renderInfoBox({
      
      infoBox(
        "Porcentaje del total llenado", paste0("%"), paste0("% llenado por día"), icon = icon("user-pen"),
        color = "warning",
        width = NULL
      )
    })
    
    output$plot1 <- renderPlotly({
      
      filtered <- filtered_data() 
      
      if (nrow(filtered) == 0) {
        #Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
        return(plot_ly(type = "scatter", mode = "lines+markers") %>%
                 layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
      }
      
      users_time <- filtered %>% group_by(Trabajador = user_name, created_at) %>% summarise(Cantidad = n()) 
      
      GraficoB <- ggplot(users_time, aes(x = created_at, y = Cantidad, group = Trabajador, color = Trabajador, 
                                         text = paste0("Fecha: ",created_at,
                                                       "\nCantidad: ",Cantidad))) + 
        geom_line(linewidth = 1.5, size = 2, alpha = 0.8) + 
        geom_point(size = 4, shape = 21, stroke = 2, fill = "white") +
        theme_economist() +
        labs(x = "Fecha", y = "Cantidad", title = "Evolución registros por trabajador") +
        theme(legend.position = "right",
              plot.title = element_text(face = 'bold', hjust = 0.5, color = "#002003", size = 15),
              plot.subtitle = element_text(hjust = 0.5, color = "#002003", size = 12),
              axis.text.x = element_text(size = 11),
              axis.text.y = element_text(size = 14),
              axis.title.x = element_text(face = 'bold', size = 14),
              axis.title.y = element_text(face = 'bold', size = 14),
              legend.title = element_text(face = 'bold', size = 12),
              plot.background=element_rect(fill="#DEEBF7"))
      
      ggplotly(GraficoB, tooltip = "text")
    })
    
    output$plot2 <- renderPlotly({
      
      filtered <- filtered_data()
      
      if (nrow(filtered) == 0) {
        # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
        return(plot_ly(type = "bar") %>%
                 layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
      }
      
      mount_user <- filtered %>%
        group_by(user_name) %>%
        summarise(Monto = sum(device_price),
                  Cantidad = n()) %>%
        mutate(Porcentaje = prop.table(Cantidad)*100)
      
      colors <- colorRampPalette(c("white", "#007bff"))
      
      mount_user <- mount_user %>%
        mutate(Color = colors(length(unique(Monto)))[as.numeric(factor(Monto))])
      
      GraficoMount_user <- ggplot(mount_user, aes(x = reorder(user_name, +Monto), y = Monto, 
                                                  text = paste0("Monto: ", scales::dollar(Monto, prefix = "S/. "), "\n",
                                                                "Cantidad: ", Cantidad, "\n",
                                                                "Porcentaje: ", round(Porcentaje,2), "%"))) +
        geom_bar(stat = "identity", aes(fill = Color)) +
        coord_flip() +
        geom_hline(aes(yintercept = mean(Monto)),
                   color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
        labs(x = "", y = "Monto", title = "Montos acumulados") +
        theme_economist() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.background = element_rect(fill = "#DEEBF7"),
              axis.title.x = element_text(face = 'bold', size = 14),
              axis.title.y = element_text(face = 'bold', size = 14)) +
        scale_fill_identity() +
        scale_y_continuous(labels = etiquetas)
      
      ggplotly(GraficoMount_user, tooltip = "text")
    })
    
    renderProgressBar <- function(value) {
      tagList(
        tags$div(
          style = "background-color: #f3f3f3; width: 100%; height: 20px; border-radius: 10px; overflow: hidden;",
          tags$div(
            style = sprintf("width: %s%%; height: 100%%; background-color: #007bff;", value * 100)
          )
        )
      )
    }
    
    renderStars <- function(value) {
      filledStars <- floor(value)
      halfStar <- ifelse((value %% 1) >= 0.5, 1, 0)
      emptyStars <- 5 - filledStars - halfStar
      
      filledStarsHtml <- paste(rep("★", filledStars), collapse = "")
      halfStarHtml <- ifelse(halfStar == 1, "★", "")
      emptyStarsHtml <- paste(rep("☆", emptyStars), collapse = "")
      
      sprintf('<div style="color: gold; font-size: 20px;">%s%s%s</div>', filledStarsHtml, halfStarHtml, emptyStarsHtml)
    }
    
    renderMonto <- JS(
      "function(data, type, row, meta) {",
      "  var cuantil = row[4];",  #// Cuantil está en la cuarta columna (índice 3 en JS)
      "  var color = '';",
      "  var arrow = '';",
      "  if (cuantil == 1) { color = '#ff0000'; arrow = '↓'; }",  #// Rojo para el primer cuantil
      "  else if (cuantil == 2) { color = '#ff8000'; arrow = '→'; }", #// Naranja para el segundo cuantil
      "  else if (cuantil == 3) { color = '#ff8000'; arrow = '→'; }", #// Naranja para el tercer cuantil
      "  else if (cuantil == 4) { color = '#00b300'; arrow = '↑'; }",  #// Verde para el cuarto cuantil
      "  var formattedData = 'S/. ' + new Intl.NumberFormat('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}).format(data);",
      "  return '<span style=\"color:' + color + '\">' + formattedData + ' ' + arrow + '</span>';",
      "}"
    )
    
    renderMonto2 <- JS(
      "function(data, type, row, meta) {",
      "  var cuantil = row[12];",  #// 
      "  var color = '';",
      "  var arrow = '';",
      "  if (cuantil == 1) { color = '#ff0000'; arrow = '↓'; }",  #// Rojo para el primer cuantil
      "  else if (cuantil == 2) { color = '#ff8000'; arrow = '→'; }", #// Naranja para el segundo cuantil
      "  else if (cuantil == 3) { color = '#ff8000'; arrow = '→'; }", #// Naranja para el tercer cuantil
      "  else if (cuantil == 4) { color = '#00b300'; arrow = '↑'; }",  #// Verde para el cuarto cuantil
      "  var formattedData = 'S/. ' + new Intl.NumberFormat('en-US', {minimumFractionDigits: 2, maximumFractionDigits: 2}).format(data);",
      "  return '<span style=\"color:' + color + '\">' + formattedData + ' ' + arrow + '</span>';",
      "}"
    )
    
    output$downloadExcelcategorie <- downloadHandler(
      filename = function() {
        paste("Categorie_mount_count", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        filtered <- filtered_data()
        reg_cat <- filtered %>% group_by(Categoria = categorie_name) %>% summarise(Monto = sum(device_price),
                                                                                   Cantidad = n()) %>% arrange(desc(Monto))
        
        write.xlsx(reg_cat, file, sheetName = "Categoire_mount_count", row.names = FALSE)
      }
    )
    
    output$tabreg_cat <- renderDataTable({
      
      filtered <- filtered_data() 
      reg_cat <- filtered %>% group_by(Categoria = categorie_name) %>% summarise(Monto = sum(device_price),
                                                                                 Cantidad = n()) %>% arrange(desc(Monto))
      
      df <- reg_cat
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        # Dividir en cuantiles
        df$Cuantil <- cut(df$Monto, 
                          breaks = quantile(df$Monto, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
        
        max_cuantil <- max(df$Cuantil)
        df$Cuantil[df$Monto == max(df$Monto)] <- max_cuantil
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 1, # Índice de la columna nombre
              render = JS(
                "function(data, type, row) {",
                "  var imgSrc = '';",
                "  if (data == 'Computadoras') { imgSrc = 'https://c0.klipartz.com/pngpicture/113/983/gratis-png-carcasas-y-carcasas-de-computadoras-computadoras-portatiles-computadoras-de-escritorio-pc-thumbnail.png'; }",
                "  else if (data == 'Muebles en general') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRyMbWIYIsRWbZr4Q7gIzzhOUm1guNgISy8X_1FGi-sAFDPwD_sTlTDJ3YGbY_QJ1CkGeA&usqp=CAU'; }",
                "  else if (data == 'Impresoras a laser') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRuN0kC_Sep8cARfryCpV4-lvYShj8K3HEd2Q&s'; }",
                "  else if (data == \"Camaras ip's\") { imgSrc = 'https://gongusca.com/wp-content/uploads/2023/07/CAMARA-BALA-IP-HIKVISION.webp'; }",
                "  else if (data == 'Central Telefonica ip') { imgSrc = 'https://smarthold.cl/blog/wp-content/uploads/2015/12/product.png'; }",
                "  else if (data == 'combo teclado y mouse') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRlZem3cG4N5WtpZ5_rbmi_QpdvMJjVCJsxSg&s'; }",
                "  else if (data == 'Cortinas') { imgSrc = 'https://img.freepik.com/vector-premium/cortina-teatro-roja-portiere-glamour-escena-realista-sobre-fondo-transparente-cortinas-cine-o-circo-seda-lujo-o-terciopelo-abierto-cortinas-tela-realista-vector-escenario_176411-2024.jpg'; }",
                "  else if (data == \"DVR's o NVR 's\") { imgSrc = 'https://blog.zositech.com/wp-content/uploads/2022/09/dvr-vs-nvr-buying-guide.webp'; }",
                "  else if (data == 'ESCRITORIO') { imgSrc = 'https://img.freepik.com/psd-gratis/escritorio-aislado-sobre-fondo-transparente_191095-28817.jpg?semt=ais_user'; }",
                "  else if (data == 'Impresoras a tinta') { imgSrc = 'https://w7.pngwing.com/pngs/373/204/png-transparent-multi-function-printer-inkjet-printing-scanner-automatic-document-feeder-printer-ink-electronics-canon.png'; }",
                "  else if (data == 'Instrumental Quirúrgico - Refractiva') { imgSrc = 'https://www.medicalmix.com/images/images/carrusel/RETRACTORES-jpg-1539259679.jpg'; }",
                "  else if (data == 'Laptops') { imgSrc = 'https://img.freepik.com/psd-premium/laptop-png-sobre-fondo-transparente_915071-40089.jpg'; }",
                "  else if (data == 'Monitores') { imgSrc = 'https://w7.pngwing.com/pngs/821/743/png-transparent-computer-monitors-hewlett-packard-led-backlit-lcd-hp-es-series-ips-panel-shampoo-bottles-23-1-television-computer-monitor-accessory-computer-wallpaper.png'; }",
                "  else if (data == 'Mouses') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSWUo3uX8dQ1GO5O8g_a8-dycyBNoPH66xh6A&s'; }",
                "  else if (data == 'OTROS') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  else if (data == 'Pizarras') { imgSrc = 'https://w7.pngwing.com/pngs/949/740/png-transparent-whiteboard-white-board-dry-erase-dry-erase-dry-erase-board-dry-erase-marker-teaching-meeting-business-board-thumbnail.png'; }",
                "  else if (data == 'PLASTIQUERIAS') { imgSrc = 'https://w7.pngwing.com/pngs/933/290/png-transparent-plastic-packaging-and-labeling-lid-plastic-waste-label-lid-material.png'; }",
                "  else if (data == 'Routers') { imgSrc = 'https://w1.pngwing.com/pngs/1021/951/png-transparent-tplink-archer-c1200-router-tplink-archer-c5-tplink-archer-c7-tplink-archer-c59-tplink-archer-vr400-tplink-archer-c2300-wireless-router-wifi.png'; }",
                "  else if (data == 'SILLAS DE ESCRITORIO') { imgSrc = 'https://w7.pngwing.com/pngs/241/855/png-transparent-office-chair-desk-office-chair-angle-furniture-office-thumbnail.png'; }",
                "  else if (data == 'SWITCH POE') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTrYJdt6P4yQ2b4KWynGDjcXkxYK-SiIDKq7g&s'; }",
                "  else if (data == 'Switches') { imgSrc = 'https://c0.klipartz.com/pngpicture/653/979/gratis-png-gigabit-ethernet-switch-de-red-red-informatica-fast-ethernet-switch.png'; }",
                "  else if (data == 'Teclados') { imgSrc = 'https://img.freepik.com/vector-premium/teclado-computadora-aluminio-moderno-sobre-fondo-transparente_150973-75.jpg'; }",
                "  else if (data == 'Telefono voip') { imgSrc = 'https://e7.pngegg.com/pngimages/921/492/png-clipart-voip-phone-session-initiation-protocol-mobile-phones-voice-over-ip-telephone-telefono-computer-network-mobile-phones.png'; }",
                "  else if (data == 'Ticketeras') { imgSrc = 'https://grupomifac360.com/wp-content/uploads/2021/11/ticketera-termica-80cam11.jpg'; }",
                "  else if (data == 'Lector de código  de barras') { imgSrc = 'https://i0.wp.com/www.miyake.pe/wp-content/uploads/2019/05/XL-3100-1.jpg?fit=600%2C500&ssl=1'; }",
                "  else if (data == 'Lámparas de Hendidura') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQcplIv4gzcOwWu_IUeZ3uJJ8_lWso3fKPz0A&s'; }",
                "  else if (data == 'Unidad de Refracción') { imgSrc = 'https://leonard2000.com/wp-content/uploads/2020/01/prod_unidad_leonard1500.jpg'; }",
                "  else if (data == 'UPS') { imgSrc = 'https://w7.pngwing.com/pngs/513/693/png-transparent-trust-powertron-ups-india-voltage-regulator-computer-india-electronics-computer-india.png'; }",
                "  else { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
                "}"
              )
            ),
            list(
              targets = 2, # Índice de la columna progreso
              render = renderMonto
            ),
            list(
              targets = 4, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
    })
    
    output$downloadExcelbrand <- downloadHandler(
      filename = function() {
        paste("Brand_mount_count", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        filtered <- filtered_data()
        reg_brand <- filtered %>% group_by(Categoria = brand_name) %>% summarise(Monto = sum(device_price),
                                                                                 Cantidad = n()) %>% arrange(desc(Monto))
        
        write.xlsx(reg_brand, file, sheetName = "Brand_mount_count", row.names = FALSE)
      }
    )
    
    output$tabreg_brand <- renderDataTable({
      
      filtered <- filtered_data() 
      reg_brand <- filtered %>% group_by(Marca = brand_name) %>% summarise(Monto = sum(device_price),
                                                                           Cantidad = n()) %>% arrange(desc(Monto))
      
      df <- reg_brand
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        # Dividir en cuantiles
        df$Cuantil <- cut(df$Monto, 
                          breaks = quantile(df$Monto, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
        
        max_cuantil <- max(df$Cuantil)
        df$Cuantil[df$Monto == max(df$Monto)] <- max_cuantil
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 1, # Índice de la columna nombre
              render = JS(
                "function(data, type, row) {",
                "  var imgSrc = '';",
                "  if (data == 'Acer') { imgSrc = 'https://cdn.icon-icons.com/icons2/2699/PNG/512/acer_logo_icon_168703.png'; }",
                "  else if (data == 'ARRIS') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcR2_WjgTCxYaIkj32zMJzSUdhEMEzBH5pnqng&s'; }",
                "  else if (data == 'Asus') { imgSrc = 'https://1000logos.net/wp-content/uploads/2016/10/Asus-Logo-1995.png'; }",
                "  else if (data == 'BIXOLON') { imgSrc = 'https://www.gorspa.org/wp-content/uploads/Bixolon-Logo.png'; }",
                "  else if (data == 'Brother') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQErG0t82gq0UnVErcAtV-pKNtVJBW5a5U_Ew&s'; }",
                "  else if (data == 'D-LINK') { imgSrc = 'https://1000logos.net/wp-content/uploads/2017/12/D-Link-symbol.jpg'; }",
                "  else if (data == 'Data Prinss') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSEhYGySy7AnO7XjeBSKvmiNJAHZl-5piiw7A&s'; }",
                "  else if (data == 'Dell') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTquxSkfdAjlZsODlWSrihmndnCc1P-714MjA&s'; }",
                "  else if (data == 'DISPENSADOR DE PAPEL') { imgSrc = 'https://promart.vteximg.com.br/arquivos/ids/567902-1000-1000/108558.jpg?v=637393523116030000'; }",
                "  else if (data == 'Elo Touchsystems') { imgSrc = 'https://static.wikia.nocookie.net/logopedia/images/8/80/Elo_Logo.jpg/revision/latest?cb=20171109015920'; }",
                "  else if (data == 'Epson') { imgSrc = 'https://flexprintinc.com/wp-content/uploads/2020/06/Epson-Logo.png'; }",
                "  else if (data == 'Genérico') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  else if (data == 'Halion') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcT6kAAlX7cWUudFWNH8uHd-vSKH2N3tUY6wWw&s'; }",
                "  else if (data == 'hikvision') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQs2QBvsSCLm8GHml3yjUzyZ9KJQLUtpPQmMA&s'; }",
                "  else if (data == 'Hp hewlett packard') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRt7Zx1O8ICdQg6ElMalrqGV5Bd-LuE-s-i9Q&s'; }",
                "  else if (data == 'IEDA POWER SAFE LCR') { imgSrc = 'https://d2ulnfq8we0v3.cloudfront.net/cdn/584975/media/catalog/product/cache/1/image/1200x/040ec09b1e35df139433887a97daa66f/8/d/8dd852961d49017ce0a21f2a14ac81c6_2.jpg'; }",
                "  else if (data == 'Konica minolta') { imgSrc = 'https://cdn.freebiesupply.com/logos/large/2x/konica-minolta-1-logo-png-transparent.png'; }",
                "  else if (data == 'logitech') { imgSrc = 'https://media.wired.com/photos/59549ff25578bd7594c46820/master/w_1600%2Cc_limit/1997-Logo1.jpg'; }",
                "  else if (data == 'Malosa') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQoh-KUC1DZXaVFOFgH01I3JFly3-vqRhmmtA&s'; }",
                "  else if (data == 'Melamina') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRJZu8jc4RqbdZ0d_wv_XAtn268IRq7md8Q-w&s'; }",
                "  else if (data == 'microsoft') { imgSrc = 'https://upload.wikimedia.org/wikipedia/commons/thumb/4/44/Microsoft_logo.svg/1024px-Microsoft_logo.svg.png'; }",
                "  else if (data == 'mikrotik') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSD88ejeIus7dix9xPvoQD_O0f-THhMz9O_fw&s'; }",
                "  else if (data == 'MM') { imgSrc = 'https://www.shutterstock.com/shutterstock/photos/1609979119/display_1500/stock-vector-mm-letter-logo-mm-logo-vector-1609979119.jpg'; }",
                "  else if (data == 'PORTA PAPELERAS') { imgSrc = 'https://w7.pngwing.com/pngs/518/161/png-transparent-clipboard-computer-icons-encapsulated-postscript-symbol-distribution-miscellaneous-text-logo.png'; }",
                "  else if (data == 'SAMSUNG') { imgSrc = 'https://upload.wikimedia.org/wikipedia/commons/thumb/2/24/Samsung_Logo.svg/1024px-Samsung_Logo.svg.png'; }",
                "  else if (data == 'Silla de plástico tapizado') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRQOvNxt2AVc0X-7fyT3_j92kZA7ppjjCPaUg&s'; }",
                "  else if (data == 'Sin Marca') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  else if (data == 'teraware') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSrZsu2wTqWizJbR6-vcJasajXAgs8ym1YuIA&s'; }",
                "  else if (data == 'Teros') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRHfkIgJL96OS9dGO-6K4EL9U6bxy6dfZ0B8Q&s'; }",
                "  else if (data == 'toshiba') { imgSrc = 'https://static.vecteezy.com/system/resources/previews/021/514/814/original/toshiba-logo-brand-computer-symbol-white-design-french-laptop-illustration-with-red-background-free-vector.jpg'; }",
                "  else if (data == 'Tp-link') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTnSpES-5WTeshG8-QcbdULRi84ah2ELtF7Ww&s'; }",
                "  else if (data == 'YEALINK') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSnK6zN8_EbU-unJfTfba2rooi-dgi4H9qo-g&s'; }",
                "  else if (data == 'yeastar') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSec88fH4V4lbcmJT65fRNSynEZEVUuMW0Pbw&s'; }",
                "  else if (data == 'Duckworth & Kent') { imgSrc = 'https://media.licdn.com/dms/image/C4E0BAQE75_XIb0z9GA/company-logo_200_200/0/1675871713919/duckworth__kent_ltd_logo?e=2147483647&v=beta&t=krv7vN9dznsPM0a1R0u20hn24uIllnGkShYcoz1Gzl8'; }",
                "  else if (data == 'Prometeo') { imgSrc = 'https://www.shutterstock.com/image-vector/prometheus-greek-god-logo-icon-260nw-1091215514.jpg'; }",
                "  else if (data == 'TOPCON') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRe4JIjqaoRk54tdVME3L6WADELl8ZfnTgibw&s'; }",
                "  else if (data == 'IBM') { imgSrc = 'https://kreafolk.com/cdn/shop/articles/ibm-logo-design-history-and-evolution-kreafolk_f6963bf2-5011-41cd-a198-3b9b4dcff48d.jpg?v=1717725060&width=2048'; }",
                "  else if (data == 'strike me') { imgSrc = 'https://images.crunchbase.com/image/upload/c_pad,f_auto,q_auto:eco,dpr_1/om0ka9evowuamgdebe3i'; }",
                "  else { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
                "}"
              )
            ),
            list(
              targets = 2, # Índice de la columna progreso
              render = renderMonto
            ),
            list(
              targets = 4, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
    })
    
    output$monto_total <- renderInfoBox({
      
      filtered <- filtered_data()
      monto_acumulado <- filtered %>% filter(device_enabled != "Inhabilitado") %>% summarise(Monto = sum(device_price)) %>% as.numeric()
      
      monto_formateado <- paste("S/", format(monto_acumulado, big.mark = ",", decimal.mark = ".", scientific = FALSE))
      
      infoBox(
        "Monto acumulado en bienes", monto_formateado, paste0("No se toma en cuenta inhabilitados"), icon = icon("money-bill-wave"),
        color = "success",
        width = NULL
      )
    })
    
    output$cantidad <- renderInfoBox({
      
      filtered <- filtered_data()
      registro_acumulado <- filtered %>% filter(device_enabled != "Inhabilitado") %>% summarise(Cantidad = n()) %>% as.numeric()
      
      infoBox(
        "Bienes", paste0(registro_acumulado," bienes"), paste0("No se toma en cuenta inhabilitados"), icon = icon("box-open"),
        color = "primary",
        width = NULL
      )
    })
    
    output$SJgraf <- renderPlotly({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(department_business == "San Jeronimo" & device_enabled != "Inhabilitado")
      
      if (nrow(filtered) == 0)  {  
        # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
        return(plot_ly(type = "scatter", mode = "lines+markers") %>%
                 layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
      }
      
      colors <- colorRampPalette(c("white", "#007bff"))
      SJ_amount <- filtered %>% group_by(department_name) %>% summarise(amount = sum(device_price)) 
      SJ_amount <- SJ_amount %>% mutate(Color = colors(length(unique(amount)))[as.numeric(factor(amount))])
      
      # Formatear los valores
      SJ_amount <- SJ_amount %>% mutate(formatted_amount = paste0("S/ ", format(amount, big.mark = ",", scientific = FALSE, nsmall = 2)))
      
      plot <- plot_ly(
        SJ_amount,
        type = "treemap",
        labels = ~department_name,
        parents = NA,
        values = ~amount,
        textinfo = "label+text+percent entry",
        marker = list(colors = ~Color),
        textfont = list(size = 15),
        hoverinfo = "label+text+percent entry",
        hovertext = ~formatted_amount,
        text = ~formatted_amount
      ) %>%
        layout(
          title = list(
            text = "Montos Areas San Jerónimo",
            font = list(size = 15, color = "red", bold = TRUE)
          )
        )
      
      plot
    })
    
    output$tabSJ <- renderDataTable({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(department_business == "San Jeronimo" & device_enabled != "Inhabilitado")
      
      SJ_amount <- filtered %>% 
        group_by(Area = department_name) %>% 
        summarise(Cantidad = n(), Monto = sum(device_price)) %>% 
        arrange(desc(Monto))
      
      df <- SJ_amount
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        # Dividir en cuantiles
        df$Cuantil <- cut(df$Monto, 
                          breaks = quantile(df$Monto, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
        
        max_cuantil <- max(df$Cuantil)
        df$Cuantil[df$Monto == max(df$Monto)] <- max_cuantil
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 3, # Índice de la columna progreso
              render = renderMonto
            ),
            list(
              targets = 4, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
      
    })
    
    output$downloadExcelSJ <- downloadHandler(
      filename = function() {
        paste("SJ_Area", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        filtered <- filtered_data()
        filtered <- filtered %>% filter(department_business == "San Jeronimo" & device_enabled != "Inhabilitado")
        
        SJ_amount <- filtered %>% 
          group_by(Area = department_name) %>% 
          summarise(Cantidad = n(), Monto = sum(device_price)) %>% 
          arrange(desc(Monto)) 
        
        write.xlsx(SJ_amount, file, sheetName = "SJ_Area", row.names = FALSE)
      }
    )
    
    output$MGgraf <- renderPlotly({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(department_business == "Magisterio" & device_enabled != "Inhabilitado")
      
      if (nrow(filtered) == 0)  {  
        # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
        return(plot_ly(type = "scatter", mode = "lines+markers") %>%
                 layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
      }
      
      colors <- colorRampPalette(c("white", "#007bff"))
      MG_amount <- filtered %>% group_by(department_name) %>% summarise(amount = sum(device_price)) 
      MG_amount <- MG_amount %>% mutate(Color = colors(length(unique(amount)))[as.numeric(factor(amount))])
      
      # Formatear los valores
      MG_amount <- MG_amount %>% mutate(formatted_amount = paste0("S/ ", format(amount, big.mark = ",", scientific = FALSE, nsmall = 2)))
      
      plot <- plot_ly(
        MG_amount,
        type = "treemap",
        labels = ~department_name,
        parents = NA,
        values = ~amount,
        textinfo = "label+text+percent entry",
        marker = list(colors = ~Color),
        textfont = list(size = 15),
        hoverinfo = "label+text+percent entry",
        hovertext = ~formatted_amount,
        text = ~formatted_amount
      ) %>%
        layout(
          title = list(
            text = "Montos Areas Magisterio",
            font = list(size = 15, color = "red", bold = TRUE)
          )
        )
      
      plot
      
    })
    
    output$tabMG <- renderDataTable({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(department_business == "Magisterio" & device_enabled != "Inhabilitado")
      
      MG_amount <- filtered %>% 
        group_by(Area = department_name) %>% 
        summarise(Cantidad = n(), Monto = sum(device_price)) %>% 
        arrange(desc(Monto))
      
      df <- MG_amount
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else if (length(unique(df$Monto)) == 5){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        # Dividir en cuantiles
        df$Cuantil <- cut(df$Monto, 
                          breaks = quantile(df$Monto, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
        
        max_cuantil <- max(df$Cuantil)
        df$Cuantil[df$Monto == max(df$Monto)] <- max_cuantil
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 3, # Índice de la columna progreso
              render = renderMonto
            ),
            list(
              targets = 4, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
      
    })
    
    output$downloadExcelMG <- downloadHandler(
      filename = function() {
        paste("MG_Area", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        
        filtered <- filtered_data()
        filtered <- filtered %>% filter(department_business == "Magisterio" & device_enabled != "Inhabilitado")
        
        MG_amount <- filtered %>% 
          group_by(Area = department_name) %>% 
          summarise(Cantidad = n(), Monto = sum(device_price)) %>% 
          arrange(desc(Monto)) 
        
        
        write.xlsx(MG_amount, file, sheetName = "MG_Area", row.names = FALSE)
      }
    )
    
    #Trabajadores que entregaron
    
    output$Entgraf <- renderPlotly({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(device_enabled != "Inhabilitado")
      if (nrow(filtered) == 0) {
        # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
        return(plot_ly(type = "bar") %>%
                 layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
      }
      
      mount_user <- filtered %>%
        group_by(Trabajador = user_lend_name) %>%
        summarise(Monto = sum(device_price),
                  Cantidad = n()) %>%
        mutate(Porcentaje = prop.table(Cantidad)*100)
      
      colors <- colorRampPalette(c("white", "#007bff"))
      
      mount_user <- mount_user %>%
        mutate(Color = colors(length(unique(Monto)))[as.numeric(factor(Monto))])
      
      GraficoMount_user_lend <- ggplot(mount_user, aes(x = reorder(Trabajador, +Monto), y = Monto, 
                                                       text = paste0("Monto: ", scales::dollar(Monto, prefix = "S/. "), "\n",
                                                                     "Cantidad: ", Cantidad, "\n",
                                                                     "Porcentaje: ", round(Porcentaje,2), "%"))) +
        geom_bar(stat = "identity", aes(fill = Color)) +
        coord_flip() +
        geom_hline(aes(yintercept = mean(Monto)),
                   color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
        labs(x = "", y = "Monto", title = "Entregaron") +
        theme_economist() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.background = element_rect(fill = "#DEEBF7"),
              axis.title.x = element_text(face = 'bold', size = 14),
              axis.title.y = element_text(face = 'bold', size = 14)) +
        scale_fill_identity() +
        scale_y_continuous(labels = etiquetas)
      
      ggplotly(GraficoMount_user_lend, tooltip = "text")
    })
    
    output$tabEnt <- renderDataTable({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(device_enabled != "Inhabilitado")
      
      mount_user_lend <- filtered %>%
        group_by(Trabajador = user_lend_name) %>%
        summarise(Cantidad = n(),
                  Monto = sum(device_price)) %>%
        arrange(desc(Monto))
      
      df <- mount_user_lend
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else if (length(unique(df$Monto)) == 5){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        # Dividir en cuantiles
        df$Cuantil <- cut(df$Monto, 
                          breaks = quantile(df$Monto, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
        
        max_cuantil <- max(df$Cuantil)
        df$Cuantil[df$Monto == max(df$Monto)] <- max_cuantil
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 3, # Índice de la columna progreso
              render = renderMonto
            ),
            list(
              targets = 4, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
      
    })
    
    output$downloadExcelEnt <- downloadHandler(
      filename = function() {
        paste("Users_lend", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        
        filtered <- filtered_data()
        filtered <- filtered %>% filter(device_enabled != "Inhabilitado")
        
        mount_user_lend <- filtered %>%
          group_by(Trabajador = user_lend_name) %>%
          summarise(Monto = sum(device_price),
                    Cantidad = n()) %>%
          arrange(desc(Monto))
        
        
        write.xlsx(mount_user_lend, file, sheetName = "Users_lend", row.names = FALSE)
      }
    )
    
    #Trabajadores que recibieron
    
    output$Recgraf <- renderPlotly({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(device_enabled != "Inhabilitado")
      if (nrow(filtered) == 0) {
        # Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
        return(plot_ly(type = "bar") %>%
                 layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
      }
      
      mount_user <- filtered %>%
        group_by(Trabajador = user_receive_name) %>%
        summarise(Monto = sum(device_price),
                  Cantidad = n()) %>%
        mutate(Porcentaje = prop.table(Cantidad)*100)
      
      colors <- colorRampPalette(c("white", "#007bff"))
      
      mount_user <- mount_user %>%
        mutate(Color = colors(length(unique(Monto)))[as.numeric(factor(Monto))])
      
      GraficoMount_user_receive <- ggplot(mount_user, aes(x = reorder(Trabajador, +Monto), y = Monto, 
                                                          text = paste0("Monto: ", scales::dollar(Monto, prefix = "S/. "), "\n",
                                                                        "Cantidad: ", Cantidad, "\n",
                                                                        "Porcentaje: ", round(Porcentaje,2), "%"))) +
        geom_bar(stat = "identity", aes(fill = Color)) +
        coord_flip() +
        geom_hline(aes(yintercept = mean(Monto)),
                   color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
        labs(x = "", y = "Monto", title = "Recibieron") +
        theme_economist() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
              plot.background = element_rect(fill = "#DEEBF7"),
              axis.title.x = element_text(face = 'bold', size = 14),
              axis.title.y = element_text(face = 'bold', size = 14)) +
        scale_fill_identity() +
        scale_y_continuous(labels = etiquetas)
      
      ggplotly(GraficoMount_user_receive, tooltip = "text")
    })
    
    output$tabRec <- renderDataTable({
      
      filtered <- filtered_data()
      filtered <- filtered %>% filter(device_enabled != "Inhabilitado")
      
      mount_user_receive <- filtered %>%
        group_by(Trabajador = user_receive_name) %>%
        summarise(Cantidad = n(),
                  Monto = sum(device_price)) %>%
        arrange(desc(Monto))
      
      df <- mount_user_receive
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else if (length(unique(df$Monto)) == 5){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        # Dividir en cuantiles
        df$Cuantil <- cut(df$Monto, 
                          breaks = quantile(df$Monto, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                          include.lowest = TRUE, labels = FALSE)
        
        max_cuantil <- max(df$Cuantil)
        df$Cuantil[df$Monto == max(df$Monto)] <- max_cuantil
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          columnDefs = list(
            list(
              targets = 3, # Índice de la columna progreso
              render = renderMonto
            ),
            list(
              targets = 4, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
      
    })
    
    output$downloadExcelRec <- downloadHandler(
      filename = function() {
        paste("Users_receive", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        
        filtered <- filtered_data()
        filtered <- filtered %>% filter(device_enabled != "Inhabilitado")
        
        mount_user_receive <- filtered %>%
          group_by(Trabajador = user_receive_name) %>%
          summarise(Monto = sum(device_price),
                    Cantidad = n()) %>%
          arrange(desc(Monto))
        
        
        write.xlsx(mount_user_receive, file, sheetName = "Users_receive", row.names = FALSE)
      }
    )
    
    #::::::::::::::::::::   LISTA DE PRODUCTOS  ::::::::::::::::::::::::::::::::::::::::::::
    
    output$tab2 <- DT::renderDataTable({
      filtered <- filtered_data()
      resumen <- filtered %>% 
        select(Categoria = categorie_name, Marca = brand_name, Descripcion = device_name, 
               Serie = device_serial, Modelo = device_model, Observaciones = device_observations, 
               Problema = device_problem,
               Entregado_por = user_lend_name, Recibido_por = user_receive_name,Monto = device_price, Habilitado = device_enabled) %>% 
        arrange(desc(Monto))
      
      df <- resumen
      
      if (nrow(df) == 1) {
        df$Cuantil <- 4
      } else if (nrow(df) == 2){
        df$Cuantil <- c(4,3)
      } else if (nrow(df) == 3){
        df$Cuantil <- c(4,3,1)
      } else if (length(unique(df$Monto)) < 4){
        df$Cuantil <- ifelse(df$Monto == max(df$Monto), 4,
                             ifelse(df$Monto == min(df$Monto), 1, 3))
      } else {
        
        df$Monto_jitter <- jitter(df$Monto, factor = 1e-10)
        
        # Dividir en cuantiles
        df$Cuantil_jitter <- cut(df$Monto_jitter, 
                                 breaks = quantile(df$Monto_jitter, probs = seq(0, 1, by = 0.25), na.rm = TRUE),
                                 include.lowest = TRUE, labels = FALSE)
        
        df <- df %>% group_by(Monto) %>% mutate(Cuantil = max(Cuantil_jitter)) %>% ungroup()
        
        df$Monto_jitter <- NULL
        df$Cuantil_jitter <- NULL
      }
      
      datatable(
        df,
        escape = FALSE,
        options = list(
          pageLength = 20,
          rowCallback = JS(
            "function(row, data, index) {",
            "  var estado = data[11];", # Índice 3 corresponde a la columna `Estado de Qx.`
            "  if (estado == 'Inhabilitado') {",
            "    $('td', row).css('background-color', '#f8d7da').css('opacity', '0.3');", # Rojo claro
            "  }",
            "}"
          ),
          columnDefs = list(
            list(
              targets = 1, # Índice de la columna nombre
              render = JS(
                "function(data, type, row) {",
                "  var imgSrc = '';",
                "  if (data == 'Computadoras') { imgSrc = 'https://c0.klipartz.com/pngpicture/113/983/gratis-png-carcasas-y-carcasas-de-computadoras-computadoras-portatiles-computadoras-de-escritorio-pc-thumbnail.png'; }",
                "  else if (data == 'Muebles en general') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRyMbWIYIsRWbZr4Q7gIzzhOUm1guNgISy8X_1FGi-sAFDPwD_sTlTDJ3YGbY_QJ1CkGeA&usqp=CAU'; }",
                "  else if (data == 'Impresoras a laser') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRuN0kC_Sep8cARfryCpV4-lvYShj8K3HEd2Q&s'; }",
                "  else if (data == \"Camaras ip's\") { imgSrc = 'https://gongusca.com/wp-content/uploads/2023/07/CAMARA-BALA-IP-HIKVISION.webp'; }",
                "  else if (data == 'Central Telefonica ip') { imgSrc = 'https://smarthold.cl/blog/wp-content/uploads/2015/12/product.png'; }",
                "  else if (data == 'combo teclado y mouse') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRlZem3cG4N5WtpZ5_rbmi_QpdvMJjVCJsxSg&s'; }",
                "  else if (data == 'Cortinas') { imgSrc = 'https://img.freepik.com/vector-premium/cortina-teatro-roja-portiere-glamour-escena-realista-sobre-fondo-transparente-cortinas-cine-o-circo-seda-lujo-o-terciopelo-abierto-cortinas-tela-realista-vector-escenario_176411-2024.jpg'; }",
                "  else if (data == \"DVR's o NVR 's\") { imgSrc = 'https://blog.zositech.com/wp-content/uploads/2022/09/dvr-vs-nvr-buying-guide.webp'; }",
                "  else if (data == 'ESCRITORIO') { imgSrc = 'https://img.freepik.com/psd-gratis/escritorio-aislado-sobre-fondo-transparente_191095-28817.jpg?semt=ais_user'; }",
                "  else if (data == 'Impresoras a tinta') { imgSrc = 'https://w7.pngwing.com/pngs/373/204/png-transparent-multi-function-printer-inkjet-printing-scanner-automatic-document-feeder-printer-ink-electronics-canon.png'; }",
                "  else if (data == 'Instrumental Quirúrgico - Refractiva') { imgSrc = 'https://www.medicalmix.com/images/images/carrusel/RETRACTORES-jpg-1539259679.jpg'; }",
                "  else if (data == 'Laptops') { imgSrc = 'https://img.freepik.com/psd-premium/laptop-png-sobre-fondo-transparente_915071-40089.jpg'; }",
                "  else if (data == 'Monitores') { imgSrc = 'https://w7.pngwing.com/pngs/821/743/png-transparent-computer-monitors-hewlett-packard-led-backlit-lcd-hp-es-series-ips-panel-shampoo-bottles-23-1-television-computer-monitor-accessory-computer-wallpaper.png'; }",
                "  else if (data == 'Mouses') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcSWUo3uX8dQ1GO5O8g_a8-dycyBNoPH66xh6A&s'; }",
                "  else if (data == 'OTROS') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  else if (data == 'Pizarras') { imgSrc = 'https://w7.pngwing.com/pngs/949/740/png-transparent-whiteboard-white-board-dry-erase-dry-erase-dry-erase-board-dry-erase-marker-teaching-meeting-business-board-thumbnail.png'; }",
                "  else if (data == 'PLASTIQUERIAS') { imgSrc = 'https://w7.pngwing.com/pngs/933/290/png-transparent-plastic-packaging-and-labeling-lid-plastic-waste-label-lid-material.png'; }",
                "  else if (data == 'Routers') { imgSrc = 'https://w1.pngwing.com/pngs/1021/951/png-transparent-tplink-archer-c1200-router-tplink-archer-c5-tplink-archer-c7-tplink-archer-c59-tplink-archer-vr400-tplink-archer-c2300-wireless-router-wifi.png'; }",
                "  else if (data == 'SILLAS DE ESCRITORIO') { imgSrc = 'https://w7.pngwing.com/pngs/241/855/png-transparent-office-chair-desk-office-chair-angle-furniture-office-thumbnail.png'; }",
                "  else if (data == 'SWITCH POE') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTrYJdt6P4yQ2b4KWynGDjcXkxYK-SiIDKq7g&s'; }",
                "  else if (data == 'Switches') { imgSrc = 'https://c0.klipartz.com/pngpicture/653/979/gratis-png-gigabit-ethernet-switch-de-red-red-informatica-fast-ethernet-switch.png'; }",
                "  else if (data == 'Teclados') { imgSrc = 'https://img.freepik.com/vector-premium/teclado-computadora-aluminio-moderno-sobre-fondo-transparente_150973-75.jpg'; }",
                "  else if (data == 'Telefono voip') { imgSrc = 'https://e7.pngegg.com/pngimages/921/492/png-clipart-voip-phone-session-initiation-protocol-mobile-phones-voice-over-ip-telephone-telefono-computer-network-mobile-phones.png'; }",
                "  else if (data == 'Ticketeras') { imgSrc = 'https://grupomifac360.com/wp-content/uploads/2021/11/ticketera-termica-80cam11.jpg'; }",
                "  else if (data == 'Lector de código  de barras') { imgSrc = 'https://i0.wp.com/www.miyake.pe/wp-content/uploads/2019/05/XL-3100-1.jpg?fit=600%2C500&ssl=1'; }",
                "  else if (data == 'Lámparas de Hendidura') { imgSrc = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQcplIv4gzcOwWu_IUeZ3uJJ8_lWso3fKPz0A&s'; }",
                "  else if (data == 'Unidad de Refracción') { imgSrc = 'https://leonard2000.com/wp-content/uploads/2020/01/prod_unidad_leonard1500.jpg'; }",
                "  else if (data == 'UPS') { imgSrc = 'https://w7.pngwing.com/pngs/513/693/png-transparent-trust-powertron-ups-india-voltage-regulator-computer-india-electronics-computer-india.png'; }",
                "  else { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
                "}"
              )
            ),
            list(
              targets = 10, # Índice de la columna progreso
              render = renderMonto2
            ),
            list(
              targets = 11, # Índice de la columna Cuantil
              visible = FALSE # Ocultar la columna Cuantil
            ),
            list(
              targets = 12, # Índice de la columna enabled
              visible = FALSE # Ocultar la columna enabled
            )
          )
        ),
        callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
      )
      
    })
    
    output$downloadExcel3 <- downloadHandler(
      filename = function() {
        paste("lista_patrimonio", Sys.Date(), ".xlsx", sep = "_")
      },
      content = function(file) {
        isolate({
          filtered <- filtered_data()
          resumen <- filtered %>% 
            select(Categoria = categorie_name, Marca = brand_name, Descripcion = device_name, 
                   Serie = device_serial, Modelo = device_model, Observaciones = device_observations, 
                   Problema = device_problem,
                   Entregado_por = user_lend_name, Monto = device_price)
          write.xlsx(resumen, file, sheetName = "Lista_patrimonio", row.names = FALSE)
        })
      }
    )
    
    output$positivo <- renderInfoBox({
      
      filtered <- filtered_data() 
      
      
      infoBox(
        "Sentimientos Positivos", paste0(" comentarios"), paste0("%"),icon = icon("face-laugh-beam"),
        color = "success",
        width = NULL
      )
    })
    
    output$neutro <- renderInfoBox({
      
      filtered <- filtered_data() 
      
      
      infoBox(
        "Sentimientos Neutros", paste0(" comentarios"), paste0("%"),icon = icon("face-meh"),
        color = "primary",
        width = NULL
      )
    })
    
    output$negativo <- renderInfoBox({
      
      filtered <- filtered_data() 
      
      infoBox(
        "Sentimientos Negativos", paste0(" comentarios"), paste0("%"),icon = icon("face-angry"),
        color = "danger",
        width = NULL
      )
    })
    
  })
  
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)

