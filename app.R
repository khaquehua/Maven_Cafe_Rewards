library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(DT)
library(openxlsx)
library(googledrive)
library(tm)
library(wordcloud2)
library(DBI)
library(RMySQL)
library(htmltools)
library(digest)
library(lavaan)
library(semTools)

gs4_auth(path = ("united-concord-425513-a4-89e3755ac030.json"))

#Palabras positivas
pspa.txt <- readLines("pspa.txt",encoding="UTF-8")
pspa.txt = iconv(pspa.txt, to="ASCII//TRANSLIT")

#Palabras negativas
nspa.txt <- readLines("nspa.txt",encoding="UTF-8")
nspa.txt = iconv(nspa.txt, to="ASCII//TRANSLIT")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words){
    
    sentence = gsub('[[:punct:]]','',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)
    
    sentence = tolower(sentence)
    
    word.list = str_split(sentence, '\\s+')
    
    words = unlist(word.list)
    
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

update_google_sheets_data <- function() {
  enlace_google_sheets <- "https://docs.google.com/spreadsheets/d/1GXA7zeCozgE5UL9lTMZD1YxZdGR930THqkGE4ZFmvqw/edit?usp=sharing"
  hoja_calculo <- gs4_get(enlace_google_sheets)
  bd <- range_read(hoja_calculo, sheet = "Citas")
  
  # Transformación de datos
  bd$Fecha <- as.Date(bd$Fecha)
  bd$Codigo <- paste0(bd$Fecha,"-",bd$Doctor2)
  return(bd)
}

update_mysql_data <- function() {
  dbjosue <- dbConnect(MySQL(), user="yyliipmy_user_encuestas", host="50.87.187.137", password="++LaFuente2023", dbname="yyliipmy_encuestas")
  query <- dbGetQuery(dbjosue, statement = "SELECT 
    p.id AS patient_id,
    p.DNI,
    a.id as ID_ANSWER,
    MAX(a.created_at) AS CREADO_EL,
    MAX(a.doctor_id) AS doctor_id,
    MAX(CASE WHEN a.question_permanent = '¿Que tipo de cita solicitó?' THEN a.answer END) AS Tipo_Cita,
    MAX(CASE WHEN a.question_permanent = '¿Su oftalmólogo/a ha resuelto todas las dudas que tenía?' THEN a.answer END) AS Dudas_Resueltas,
    MAX(CASE WHEN a.question_permanent = '¿Volvería a solicitar una cita con el mismo oftalmólogo/a?' THEN a.answer END) AS Volveria_Solicitar_Cita,
    MAX(CASE WHEN a.question_permanent = '¿Cree usted que ha recibido suficiente información sobre las dudas que tenía al inicio de su consulta?' THEN a.answer END) AS Informacion_Suficiente,
    MAX(CASE WHEN a.question_permanent = '¿Cuáles son las probabilidades de que vuelva a solicitar nuestro servicio?' THEN a.answer END) AS Probabilidades_Volver,
    MAX(CASE WHEN a.question_permanent = '¿Cómo calificaria su experiencia en nuestro servicio de oftalmología?' THEN a.answer END) AS Calificacion_Experiencia,
    MAX(CASE WHEN a.question_permanent = '¿Tiene algún comentario o sugerencia para nosotros?' THEN a.answer END) AS Comentario,
    MAX(a.others) AS others
FROM 
    patients p
LEFT JOIN 
    answers a ON p.id = a.patient_id
WHERE 
    p.id <= 50000
    AND a.answer IS NOT NULL  
GROUP BY 
    p.id, p.DNI, a.created_at
ORDER BY CREADO_EL DESC")
  query$CREADO_EL <- as.Date(query$CREADO_EL)
  query$Doctor <- ifelse(query$doctor_id == 1, "Dr. Nathanael Henson",
                         ifelse(query$doctor_id == 2, "Dr. Christian Becker Contreras",
                                ifelse(query$doctor_id == 3, "Dra. Ayesha Vega",
                                       ifelse(query$doctor_id == 4, "Dra. Rosario del Pilar Peñafiel",
                                              ifelse(query$doctor_id == 5, "Dr. Djordje Velickovich",
                                                     ifelse(query$doctor_id == 6, "Dr. Alvaro Rivera Contreras",
                                                            ifelse(query$doctor_id == 7, "Dr. Rodrigo Rivera Contreras",
                                                                   ifelse(query$doctor_id == 8, "Dr. Jose Miguel Velasco Stoll",
                                                                          "No lo se"))))))))
  query$Tipo_comentario <- ifelse(is.na(query$others), "Comentario",
                                  ifelse(query$others == 2, "Sugerencia", "Comentario"))
  query$Codigo <- paste0(query$CREADO_EL,"-",query$Doctor)
  resultado <- score.sentiment(query$Comentario, pspa.txt, nspa.txt,.progress='text')
  query$Score = resultado$score
  query$Sentimiento <- ifelse(query$Score > 0, "Positivo",
                              ifelse(query$Score < 0, "Negativo", "Neutro"))
  query$Dudas_Resueltas <- as.integer(query$Dudas_Resueltas)
  query$Volveria_Solicitar_Cita <- as.integer(query$Volveria_Solicitar_Cita)
  query$Informacion_Suficiente <- as.integer(query$Informacion_Suficiente)
  query$Probabilidades_Volver <- as.integer(query$Probabilidades_Volver)
  query$Calificacion_Experiencia <- as.integer(query$Calificacion_Experiencia)
  query$Puntaje_promedio <- (query$Dudas_Resueltas + query$Volveria_Solicitar_Cita + query$Informacion_Suficiente + query$Probabilidades_Volver + query$Calificacion_Experiencia)/5
  dbDisconnect(dbjosue)
  return(query)
}

update_mysql_login <- function() {
  dbjosue2 <- dbConnect(MySQL(), user="yyliipmy_user_inventario", host="50.87.187.137", password="++LaFuente2023++", dbname="yyliipmy_inventario")
  query2 <- dbGetQuery(dbjosue2, statement = "select name, password from kusers")
  dbDisconnect(dbjosue2)
  return(query2)
}

# Función para crear la interfaz de usuario del dashboard
dashboard_ui <- function() {
  dashboardPage(
    title = "La Fuente - Análisis Encuestas",
    dashboardHeader(
      title = dashboardBrand(
        title = h4("Analisis Encuestas"),
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
        
        .positivo-light {background-color: #d4edda !important; color: black !important;}
        .neutro-light {background-color: #cce5ff !important; color: black !important;}
        .negativo-light {background-color: #f8d7da !important; color: black !important;}
        
        .positivo-dark {background-color: #155724 !important; color: white !important;}
        .neutro-dark {background-color: #004085 !important; color: white !important;}
        .negativo-dark {background-color: #721c24 !important; color: white !important;}
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
  data <- reactiveValues(bd = NULL, query = NULL, query2 = NULL)
  
  update_data <- function() {
    data$bd <- update_google_sheets_data()
    data$query <- update_mysql_data()
    data$query2 <- update_mysql_login()
  }
  
  update_data()
  
  autoInvalidate <- reactiveTimer(300000) #5 minutos = 300000 milisegundos
  
  # Credenciales de usuario
  user <- reactiveValues(authenticated = FALSE)
  
  output$sidebar_content <- renderUI({
    req(data$query)
    req(data$bd)
    tagList(
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("calendar", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Fecha"
        ),
        dateRangeInput("selectDate", label = NULL, start = min(data$query$CREADO_EL), end = max(data$query$CREADO_EL))
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("user-doctor", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Doctor"
        ),
        selectInput("selectSurgeon", label = NULL, choices = c("All", names(table(data$query$Doctor))), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("user-nurse", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Indicaciones"
        ),
        selectInput("selectIndicacion", label = NULL, choices = c("All", names(table(data$bd$Indicacion))), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("hospital-user", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Tipo cita"
        ),
        selectInput("selectTipoCita", label = NULL, choices = c("All", unique(data$query$Tipo_Cita)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("comment", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Tipo comentario"
        ),
        selectInput("selectTipoComentario", label = NULL, choices = c("All", unique(data$query$Tipo_comentario)), selected = "All")
      ),
      tags$div(
        class = "input-group",
        tags$span(
          tags$img(icon("face-grimace", lib = "font-awesome"), height = 30, style = "margin-right: 10px;"),
          " Sentimiento"
        ),
        selectInput("selectSentiment", label = NULL, choices = c("All", unique(data$query$Sentimiento)), selected = "All")
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
  
  # Mostrar modal de inicio de sesión al inicio
  showModal(
    modalDialog(
      div(
        class = "modal-header bg-primary text-center",
        tags$img(src = "https://inventario.lafuentecsi.com/favicon.png", height = 50, width = 50),
        tags$h4("Inicio de sesión", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
      ),
      textInput("user_name", "Username"),
      passwordInput("password", "Password"),
      actionButton("login_button", "Log in"),
      easyClose = FALSE,
      footer = NULL
    )
  )
  
  # Observar el botón de inicio de sesión
  observeEvent(input$login_button, {
    req(input$user_name, input$password)
    
    # Encontrar el hash de la contraseña correspondiente al email
    db_password_hash <- data$query2$password[data$query2$name == input$user_name]
    
    if (length(db_password_hash) == 1) {
      db_password_hash <- db_password_hash[1]
      input_password_hash <- digest(input$password, algo = "md5", serialize = FALSE)
      
      if (input_password_hash == db_password_hash) {
        user$authenticated <- TRUE
        user$name <- data$query2$name[data$query2$name == input$user_name]  # Almacenar el nombre de usuario
        
        showModal(
          modalDialog(
            div(
              class = "modal-header bg-success text-center",
              div(
                style = "background-color: #F1FECC; padding: 5px;",
                tags$img(src = "https://inventario.lafuentecsi.com/favicon.png", height = 30),
              ),
              tags$h4("Bienvenido", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
            ),
            user$name,
            easyClose = TRUE,
            footer = actionButton("close_welcome_modal", "Aceptar", class = "btn-success")
          )
        )
        
        removeModal()
      } else {
        showModal(
          modalDialog(
            div(
              class = "modal-header bg-danger text-center",
              div(
                style = "background-color: #FCDBDA; padding: 5px;",
                tags$img(src = "https://inventario.lafuentecsi.com/favicon.png", height = 30),
              ),
              tags$h4("Acceso Denegado", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
            ),
            "Nombre de usuario o contraseña incorrecta",
            easyClose = FALSE,
            footer = actionButton("try_again_button", "Volver a intentar", class = "btn-danger")
          )
        )
      }
    } else {
      showModal(
        modalDialog(
          div(
            class = "modal-header bg-danger text-center",
            div(
              style = "background-color: #FCDBDA; padding: 5px;",
              tags$img(src = "https://inventario.lafuentecsi.com/favicon.png", height = 30),
            ),
            tags$h4("Acceso Denegado", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
          ),
          "Nombre de usuario o contraseña incorrecta",
          easyClose = FALSE,
          footer = actionButton("try_again_button", "Volver a intentar", class = "btn-danger")
        )
      )
    }
  })
  
  # Observar el botón "Try Again" para volver a mostrar el modal de inicio de sesión
  observeEvent(input$try_again_button, {
    showModal(
      modalDialog(
        div(
          class = "modal-header bg-primary text-center",
          tags$img(src = "https://inventario.lafuentecsi.com/favicon.png", height = 50, width = 50),
          tags$h4("Inicio de sesión", style = "margin-top: 7px; margin-right: auto; margin-left: auto;")
        ),
        textInput("user_name", "Username"),
        passwordInput("password", "Password"),
        actionButton("login_button", "Log in"),
        easyClose = FALSE,
        footer = NULL
      )
    )
  })
  
  observeEvent(input$close_welcome_modal, {
    removeModal()
  })
  
  filtered_data <- reactive({
    
    selected_surgeon <- input$selectSurgeon # Doctor que deriva
    selected_indicacion <- input$selectIndicacion # Persona que da indicacion
    selected_tipo_cita <- input$selectTipoCita # Tipo cita
    selected_tipo_comentario <- input$selectTipoComentario # Tipo comentario
    selected_sentiment <- input$selectSentiment # Tipo comentario
    selected_date <- input$selectDate # Fecha de cotizacion
  
    filtered <- merge(data$query, data$bd, by = "Codigo", all = T)
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_surgeon) > 1,
      length(selected_indicacion) > 1,
      length(selected_tipo_cita) > 1,
      length(selected_tipo_comentario) > 1,
      length(selected_sentiment) > 1,
      !is.null(selected_date)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- merge(data$query, data$bd, by = "Codigo", all = T)
    
      if (!"All" %in% selected_surgeon) {
        filtered <- filtered[filtered$Doctor %in% selected_surgeon, ]
      }
      
      if (!"All" %in% selected_indicacion) {
        filtered <- filtered[filtered$Indicacion == selected_indicacion, ]
      }
      
      if (!"All" %in% selected_tipo_cita) {
        filtered <- filtered[filtered$Tipo_Cita == selected_tipo_cita, ]
      }
      
      if (!"All" %in% selected_tipo_comentario) {
        filtered <- filtered[filtered$Tipo_comentario == selected_tipo_comentario, ]
      }
      
      if (!"All" %in% selected_sentiment) {
        filtered <- filtered[filtered$Sentimiento == selected_sentiment, ]
      }
      
      if (!is.null(selected_date)) {
        filtered <- filtered[filtered$CREADO_EL >= selected_date[1] & filtered$CREADO_EL <= selected_date[2], ]
      }
    } else {
      # Si no hay filtros, devolver el conjunto de datos completo
      filtered <- merge(data$query, data$bd, by = "Codigo", all = T)
    }
    
    return(filtered)
  })
  
  filtered_data2 <- reactive({
    
    selected_surgeon <- input$selectSurgeon # Doctor que deriva
    selected_indicacion <- input$selectIndicacion # Persona que da indicacion
    selected_date <- input$selectDate # Fecha de cotizacion
    
    filtered <- data$bd
    
    # Verificar si al menos un filtro está activado
    any_filter_selected <- any(c(
      length(selected_surgeon) > 1,
      length(selected_indicacion) > 1,
      !is.null(selected_date)
    ))
    
    if (any_filter_selected) {
      # Aplicar los filtros solo si al menos uno está seleccionado
      filtered <- data$bd
      
      if (!"All" %in% selected_surgeon) {
        filtered <- filtered[filtered$Doctor2 %in% selected_surgeon, ]
      }
      
      if (!"All" %in% selected_indicacion) {
        filtered <- filtered[filtered$Indicacion == selected_indicacion, ]
      }
      
      if (!is.null(selected_date)) {
        filtered <- filtered[filtered$Fecha >= selected_date[1] & filtered$Fecha <= selected_date[2], ]
      }
    } else {
      # Si no hay filtros, devolver el conjunto de datos completo
      filtered <- data$bd
    }
    
    return(filtered)
  })
  
  # Contenido del dashboard condicionado al estado de autenticación
  output$content <- renderUI({
    
    req(data$bd, data$query, data$query2)
    
    if (user$authenticated) {
      tabsetPanel(
        type = "tabs",
        tabPanel(
          HTML(paste(icon("users"), "Encuestas Realizadas")),
          fluidRow(
            column(4, infoBoxOutput("atendidos", width = 12)),
            column(4, infoBoxOutput("encuestas", width = 12)),
            column(4, infoBoxOutput("porcentaje", width = 12))
          ),
          div(
            fluidRow(
              column(8, 
                     box(
                       title = "Evolución atendidos-encuestas",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,  # Asegúrate de que el ancho del box ocupe toda la columna
                       plotlyOutput("plot1")
                     )
              ),
              column(4, 
                     box(
                       title = "Cantidad Tipo Cita",
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
                       title = "Encuestas por médico",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       dataTableOutput('tabencuestamedico')
                     )
              ),
              column(6, 
                     box(
                       title = "Encuestas por indicaciones",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       dataTableOutput('tabencuestaindicacion')
                     )
              )
            ),
            style = "margin-top: 20px;"  # Ajusta el valor según tus preferencias
          )
        ),
        tabPanel(
          HTML(paste(icon("star"), "Análisis Calificación")),
          fluidRow(
            column(4, infoBoxOutput("encuestas2", width = 12)),
            column(4, infoBoxOutput("puntaje", width = 12)),
            column(4, infoBoxOutput("cronbach", width = 12))
          ),
          div(
            fluidRow(
              column(12, 
                     box(
                       title = "Evolución puntaje",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       plotlyOutput("plotevopunt", height = "600px")
                     )
              )
            ),
            style = "margin-top: 20px;"
          ),
          div(
            fluidRow(
              column(6, 
                     box(
                       title = "Calificación por médico",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       plotlyOutput("plotdep", height = "600px")
                     )
              ),
              column(6, 
                     box(
                       title = "Calificación por tipo cita",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       plotlyOutput("plotprov", height = "600px")
                     )
              )
            ),
            style = "margin-top: 20px;"  # Ajusta el valor según tus preferencias
          ),
          div(
            fluidRow(
              column(12, 
                     box(
                       title = "Calificación por pregunta",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       downloadButton("downloadExcel", "Descargar Excel"),
                       dataTableOutput('tabcalificacion')
                     )
              )
            ),
            style = "margin-top: 20px;"
          )
        ),
        tabPanel(
          HTML(paste(icon("file-alt"), "Análisis de Textos")),
          fluidRow(
            column(4, infoBoxOutput("positivo", width = 12)),
            column(4, infoBoxOutput("neutro", width = 12)),
            column(4, infoBoxOutput("negativo", width = 12))
          ),
          div(
            fluidRow(
              column(6, 
                     box(
                       title = "Nube de palabras",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       wordcloud2Output("plotnub", height = "600px")
                     )
              ),
              column(6, 
                     box(
                       title = "Recuento palabras",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       plotlyOutput("plotrecpal", height = "600px")
                     )
              )
            ),
            style = "margin-top: 20px;"
          ),
          div(
            fluidRow(
              column(12, 
                     box(
                       title = "Comentarios pacientes",
                       status = "primary",
                       solidHeader = TRUE,
                       maximizable = TRUE,
                       width = 12,
                       downloadButton("downloadExcel2", "Descargar Excel"),
                       DT::dataTableOutput('tabcomentarios')
                     )
              )
            ),
            style = "margin-top: 20px;"
          )
        ),
        tabPanel(
          HTML(paste(icon("list-ul"), "Lista de Pacientes")),
          fluidRow(
            column(12, 
                   box(
                     title = "Lista de pacientes",
                     status = "primary",
                     solidHeader = TRUE,
                     maximizable = TRUE,
                     width = 12,
                     downloadButton("downloadExcel3", "Descargar Pacientes Excel"),
                     DT::dataTableOutput('tab2')
                   )
            )
          )
        )
      )
    } else {
      fluidRow(
        box(
          title = "Acceso Denegado!!!",
          "Por favor vuelva a iniciar"
        )
      )
    }
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
    if (user$authenticated) {
      
      output$atendidos <- renderInfoBox({
        filtered <- filtered_data2() 
        atenciones_unicas <- filtered %>% distinct(Codigo, .keep_all = TRUE) %>% select(Cantidad_atendidos)
        cantidad <- sum(atenciones_unicas, na.rm = T)
        promedio <- mean(atenciones_unicas$Cantidad_atendidos, na.rm = T)
        
        infoBox(
          "Pacientes atendidos", paste0(cantidad," pacientes"), paste0(round(promedio,0)," pacientes atendidos por médico en el día"), icon = icon("user-check"),
          color = "primary",
          width = NULL
        )
      })
      
      output$encuestas <- renderInfoBox({
        
        filtered <- filtered_data() 
        
        encuestas <- filtered %>% filter(!is.na(patient_id)) %>% group_by(CREADO_EL) %>% dplyr::summarise(Cantidad = n())
        cantidad <- sum(encuestas$Cantidad)
        promedio <- mean(encuestas$Cantidad)
        
        infoBox(
          "Encuestas llenadas", paste0(cantidad, " encuestas"), paste0(round(promedio,0), " encuestas por médico en el día"), icon = icon("file-signature"),
          color = "success",
          width = NULL
        )
      })
      
      output$porcentaje <- renderInfoBox({
        
        filtered <- filtered_data() 
        encuestas <- filtered %>% filter(!is.na(patient_id)) %>% group_by(CREADO_EL) %>% dplyr::summarise(Cantidad = n())
        cantidad <- sum(encuestas$Cantidad)
        promedio <- mean(encuestas$Cantidad)
        
        filtered2 <- filtered_data2() 
        atenciones_unicas <- filtered2 %>% distinct(Codigo, .keep_all = TRUE) %>% select(Cantidad_atendidos)
        cantidad2 <- sum(atenciones_unicas, na.rm = T)
        promedio2 <- mean(atenciones_unicas$Cantidad_atendidos, na.rm = T)
        
        pct <- round((cantidad / cantidad2)*100,2)
        pct2 <- round((promedio / promedio2)*100,2)
        
        infoBox(
          "Porcentaje del total llenado", paste0(pct,"%"), paste0(pct2,"% llenado por día"), icon = icon("user-pen"),
          color = "warning",
          width = NULL
        )
      })
      
      output$plot1 <- renderPlotly({
        
        filtered <- filtered_data() 
        encuestas <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Fecha = CREADO_EL) %>% dplyr::summarise(Cantidad = n())
        encuestas$Tipo <- "Encuestas"
        
        filtered2 <- filtered_data2() 
        atenciones <- filtered2 %>% group_by(Fecha) %>% dplyr::summarise(Cantidad = sum(Cantidad_atendidos))
        atenciones$Tipo <- "Atendidos"
        
        serie <- rbind(encuestas, atenciones)
        
        if (nrow(filtered) == 0 || nrow(filtered2) == 0) {
           #Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
          return(plot_ly(type = "scatter", mode = "lines+markers") %>%
                   layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
        }

        GraficoB <- ggplot(serie, aes(x = Fecha, y = Cantidad, group = Tipo, color = Tipo,
                                                text = paste0(Cantidad, " registros\n",Fecha))) + 
          geom_line(linewidth = 1.5, size = 2, alpha = 0.8) + 
          geom_point(size = 1.5, shape = 21, stroke = 1.5, fill = "white") +
          theme_economist() +
          #scale_fill_manual(values=c()) + 
          scale_color_manual(values=c("blue","green")) +
          labs(x = "Fecha", y = "Cantidad", title = "Evolución encuestas realizadas por día") +
          theme(plot.title = element_text(face = 'bold', hjust = 0.5, color = "#002003", size = 15),
                plot.subtitle = element_text(hjust = 0.5, color = "#002003", size = 12),
                axis.text.x = element_text(size = 11, angle = 90),
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
        
        tipocita <- filtered %>% filter(!is.na(Tipo_Cita)) %>%
          group_by(Tipo_Cita) %>%
          dplyr::summarise(Cantidad = n()) %>%
          mutate(Porcentaje = prop.table(Cantidad)*100)
        
        colors <- colorRampPalette(c("white", "#007bff"))
        
        tipocita <- tipocita %>%
          mutate(Color = colors(length(unique(Cantidad)))[as.numeric(factor(Cantidad))])
      
        GraficoTipoCita <- ggplot(tipocita, aes(x = reorder(Tipo_Cita, +Cantidad), y = Cantidad, 
                                               text = paste0("Tipo cita: ", Tipo_Cita, "\n",
                                                            "Cantidad: ", Cantidad, "\n",
                                                            "Porcentaje: ", round(Porcentaje,2), "%"))) +
          geom_bar(stat = "identity", aes(fill = Color)) +
          coord_flip() +
          geom_hline(aes(yintercept = mean(Cantidad)),
                     color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
          labs(x = "", y = "Cantidad", title = "Motivo cita") +
          theme_economist() +
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.background = element_rect(fill = "#DEEBF7"),
                axis.title.x = element_text(face = 'bold', size = 14),
                axis.title.y = element_text(face = 'bold', size = 14)) +
          scale_fill_identity()
        
        ggplotly(GraficoTipoCita, tooltip = "text")
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
      
      output$tabencuestamedico <- renderDataTable({
        
        filtered <- filtered_data() 
        encuestas <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Doctor) %>% dplyr::summarise(Encuestas = n())
        
        filtered2 <- filtered_data2() 
        atenciones <- filtered2 %>% group_by(Doctor2) %>% dplyr::summarise(Atenciones = sum(Cantidad_atendidos))
        
        df <- merge(encuestas, atenciones, by.x = "Doctor", by.y = "Doctor2", all.y = TRUE)
        df$Encuestas <- ifelse(is.na(df$Encuestas),0,df$Encuestas)
        df <- df %>% select(Doctor, Atenciones, Encuestas) %>% 
          mutate(Progreso = Encuestas/Atenciones, Porcentaje = paste0(round(Progreso*100,2),"%")) %>%
          arrange(desc(Progreso))
        
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
                  "  if (data == 'Dr. Alvaro Rivera Contreras') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/alvaro.jpeg'; }",
                  "  else if (data == 'Dr. Christian Becker Contreras') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/becker.jpg'; }",
                  "  else if (data == 'Dr. Djordje Velickovich') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/beci.jpg'; }",
                  "  else if (data == 'Dr. Nathanael Henson') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/henson.jpg'; }",
                  "  else if (data == 'Dra. Ayesha Vega') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/vega.jpg'; }",
                  "  else if (data == 'Dra. Rosario del Pilar Peñafiel') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Dr. Rodrigo Rivera Contreras') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/rodrigo.jpeg'; }",
                  "  else if (data == 'Dr. Jose Miguel Velasco Stoll') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
                  "}"
                )
              ),
              list(
                targets = 4, # Índice de la columna progreso
                render = JS(
                  "function(data, type, row) {",
                  "  var color = '';",
                  "  if (data <= 0.3) { color = '#ff0000'; }",  #// Rojo para valores entre 0 y 0.3
                  "  else if (data <= 0.6) { color = '#ff8000'; }",# // Naranja para valores entre 0.3 y 0.6
                  "  else { color = '#00b300'; }",  #// Verde para valores entre 0.61 y 1
                  "  return '<div style=\"background-color: #f3f3f3; width: 100%; height: 20px; border-radius: 10px; overflow: hidden;\">'",
                  "    + '<div style=\"width:' + (data * 100) + '%; height: 100%; background-color:' + color + ';\"></div></div>';",
                  "}"
                )
              )
            )
          ),
          callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
        )
      })
      
      output$tabencuestaindicacion <- renderDataTable({
        
        filtered <- filtered_data() 
        encuestas <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Indicacion) %>% dplyr::summarise(Encuestas = n())
        
        filtered2 <- filtered_data2() 
        atenciones <- filtered2 %>% group_by(Indicacion) %>% dplyr::summarise(Atenciones = sum(Cantidad_atendidos))
        
        df <- merge(encuestas, atenciones, by.x = "Indicacion", all.y = T)
        df$Encuestas <- ifelse(is.na(df$Encuestas),0,df$Encuestas)
        df <- df %>% select(Indicacion, Atenciones, Encuestas) %>% 
          mutate(Progreso = Encuestas/Atenciones, Porcentaje = paste0(round(Progreso*100,2),"%")) %>%
          arrange(desc(Progreso))
        
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
                  "  if (data == 'Flor') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Juana') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Juana/Yomira') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Yessenia/Mildret') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Liz') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Mildret') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Yessenia') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Yomira') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
                  "}"
                )
              ),
              list(
                targets = 4, # Índice de la columna progreso
                render = JS(
                  "function(data, type, row) {",
                  "  var color = '';",
                  "  if (data <= 0.3) { color = '#ff0000'; }",  #// Rojo para valores entre 0 y 0.3
                  "  else if (data <= 0.6) { color = '#ff8000'; }",# // Naranja para valores entre 0.3 y 0.6
                  "  else { color = '#00b300'; }",  #// Verde para valores entre 0.61 y 1
                  "  return '<div style=\"background-color: #f3f3f3; width: 100%; height: 20px; border-radius: 10px; overflow: hidden;\">'",
                  "    + '<div style=\"width:' + (data * 100) + '%; height: 100%; background-color:' + color + ';\"></div></div>';",
                  "}"
                )
              )
            )
          ),
          callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
        )
      })
      
      output$encuestas2 <- renderInfoBox({
        
        filtered <- filtered_data() 
        
        encuestas <- filtered %>% filter(!is.na(patient_id)) %>% group_by(CREADO_EL) %>% dplyr::summarise(Cantidad = n())
        cantidad <- sum(encuestas$Cantidad)
        promedio <- mean(encuestas$Cantidad)
        
        infoBox(
          "Encuestas llenadas", paste0(cantidad, " encuestas"), paste0(round(promedio,0), " encuestas por médico en el día"), icon = icon("file-signature"),
          color = "success",
          width = NULL
        )
      })
      
      output$puntaje <- renderInfoBox({
        
        filtered <- filtered_data() 
        
        promedio <- filtered %>% filter(!is.na(patient_id)) %>% dplyr::summarise(Promedio = mean(Puntaje_promedio))
        promedio <- mean(promedio$Promedio)
        
        infoBox(
          "Puntaje promedio", paste0(round(promedio,2)," puntos"), "En una escala del 1 al 5", icon = icon("star-half-stroke"),
          color = "warning",
          width = NULL
        )
      })
      
      output$cronbach <- renderInfoBox({
        
        #filtered <- filtered_data() 
        
        # Analisis factorial confirmatorio
        CFA.Calidad<-"Calidad=~Dudas_Resueltas + Volveria_Solicitar_Cita + Informacion_Suficiente + Probabilidades_Volver + Calificacion_Experiencia"
        fit.CFA.Calidad<-cfa(CFA.Calidad,data=data$query)
        reliability_results <- reliability(fit.CFA.Calidad)
        cronbach_alpha <- reliability_results["alpha", ]
        
        
        infoBox(
          "Alpha de cronbach", paste0(round(cronbach_alpha,2)," entre 0 a 1"), "Si las preguntas miden calidad", icon = icon("list-check"),
          color = "maroon",
          width = NULL
        )
      })
      
      output$plotevopunt <- renderPlotly({
        
        filtered <- filtered_data() 
        evol_ptj <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Fecha = CREADO_EL) %>% dplyr::summarise(Puntaje = mean(Puntaje_promedio))
        
        if (nrow(filtered) == 0)  {  
          #Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
          return(plot_ly(type = "scatter", mode = "lines+markers") %>%
                   layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
        }
        
        GraficoEvolucion_ptj  <- ggplot(evol_ptj, aes(x = Fecha, y = Puntaje, 
                                                                  text = paste0(round(Puntaje,2), " puntos\n",Fecha))) + 
          geom_line(group=1, color="#0000CC", linetype="dashed") + 
          geom_point(size = 4, shape = 21, stroke = 2, fill = "#007bff", color = "#007bff") +
          theme_economist() +
          labs(x = "Fecha", y = "Puntaje", title = "Evolución puntaje en el tiempo") +
          theme(legend.position = "right",
                plot.title = element_text(face = 'bold', hjust = 0.5, color = "#002003", size = 15),
                plot.subtitle = element_text(hjust = 0.5, color = "#002003", size = 12),
                axis.text.x = element_text(size = 11),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(face = 'bold', size = 14),
                axis.title.y = element_text(face = 'bold', size = 14),
                legend.title = element_text(face = 'bold', size = 12),
                plot.background=element_rect(fill="#DEEBF7"))
        
        ggplotly(GraficoEvolucion_ptj, tooltip = "text")
      })
      
      output$plotdep <- renderPlotly({
        
        filtered <- filtered_data()
        
        if (nrow(filtered) == 0) {
          #Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
          return(plot_ly(type = "histogram") %>%
                   layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
        }
        
        # Agrupar promedios
        med_ptj <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Doctor) %>% dplyr::summarise(Puntaje = mean(Puntaje_promedio))
        
        colors <- colorRampPalette(c("white", "#007bff"))
        
        med_ptj <- med_ptj %>%
          mutate(Color = colors(length(unique(Puntaje)))[as.numeric(factor(Puntaje))])
        
        GraficoMedico_ptj <- ggplot(med_ptj, aes(x = reorder(Doctor, +Puntaje), y = Puntaje, 
                                                text = paste0("Doctor: ", Doctor, "\n",
                                                              "Puntaje: ", round(Puntaje,2)))) +
          geom_bar(stat = "identity", aes(fill = Color)) +
          coord_flip() +
          geom_hline(aes(yintercept = mean(Puntaje)),
                     color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
          labs(x = "", y = "Puntaje", title = "Puntaje médico") +
          theme_economist() +
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.background = element_rect(fill = "#DEEBF7"),
                axis.title.x = element_text(face = 'bold', size = 14),
                axis.title.y = element_text(face = 'bold', size = 14)) +
          scale_fill_identity()
      
        ggplotly(GraficoMedico_ptj, tooltip = "text")
        
        
      })
      
      output$plotprov <- renderPlotly({
        
        filtered <- filtered_data()
        
        if (nrow(filtered) == 0) {
          #Mostrar un mensaje o alerta en lugar de intentar crear el gráfico
          return(plot_ly(type = "histogram") %>%
                   layout(title = "No hay datos disponibles\npara los filtros seleccionados"))
        }
        
        # Agrupar promedios
        tipo_ptj <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Tipo_Cita) %>% dplyr::summarise(Puntaje = mean(Puntaje_promedio))
        
        colors <- colorRampPalette(c("white", "#007bff"))
        
        tipo_ptj <- tipo_ptj %>%
          mutate(Color = colors(length(unique(Puntaje)))[as.numeric(factor(Puntaje))])
        
        GraficoTipo_ptj <- ggplot(tipo_ptj, aes(x = reorder(Tipo_Cita, +Puntaje), y = Puntaje, 
                                                 text = paste0("Motivo: ", Tipo_Cita, "\n",
                                                               "Puntaje: ", round(Puntaje,2)))) +
          geom_bar(stat = "identity", aes(fill = Color)) +
          coord_flip() +
          geom_hline(aes(yintercept = mean(Puntaje)),
                     color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
          labs(x = "", y = "Puntaje", title = "Puntaje motivo") +
          theme_economist() +
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.background = element_rect(fill = "#DEEBF7"),
                axis.title.x = element_text(face = 'bold', size = 14),
                axis.title.y = element_text(face = 'bold', size = 14)) +
          scale_fill_identity()
        
        ggplotly(GraficoTipo_ptj, tooltip = "text")
      })
      
      output$tabcalificacion <- renderDataTable({
        
        filtered <- filtered_data() 
        ptj_med <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Doctor) %>% 
          dplyr::summarise(`Resolvió sus dudas` = mean(Dudas_Resueltas),
                           `Volvería a solicitar con el médico` = mean(Volveria_Solicitar_Cita),
                           `Recibió suficiente información` = mean(Informacion_Suficiente),
                           `Volverá a solicitar el servicio` = mean(Probabilidades_Volver),
                           `Calificación de su experiencia` = mean(Calificacion_Experiencia),
                           Puntaje = mean(Puntaje_promedio)) %>%
          arrange(desc(Puntaje))
        
        datatable(
          ptj_med,
          escape = FALSE,
          options = list(
            columnDefs = list(
              list(
                targets = 1, # Índice de la columna Doctor
                render = JS(
                  "function(data, type, row) {",
                  "  var imgSrc = '';",
                  "  if (data == 'Dr. Alvaro Rivera Contreras') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/alvaro.jpeg'; }",
                  "  else if (data == 'Dr. Christian Becker Contreras') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/becker.jpg'; }",
                  "  else if (data == 'Dr. Djordje Velickovich') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/beci.jpg'; }",
                  "  else if (data == 'Dr. Nathanael Henson') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/henson.jpg'; }",
                  "  else if (data == 'Dra. Ayesha Vega') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/vega.jpg'; }",
                  "  else if (data == 'Dra. Rosario del Pilar Peñafiel') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  else if (data == 'Dr. Rodrigo Rivera Contreras') { imgSrc = 'https://www.clinicalafuente.com/wp-content/uploads/2024/06/rodrigo.jpeg'; }",
                  "  else if (data == 'Dr. Jose Miguel Velasco Stoll') { imgSrc = 'https://inventario.lafuentecsi.com/favicon.png'; }",
                  "  return '<img src=\"' + imgSrc + '\" style=\"width: 40px; height: 40px; border-radius: 50%;\"> ' + data;",
                  "}"
                )
              ),
              list(
                targets = 2:7, # Índice de las columnas de puntaje
                render = JS(
                  "function(data, type, row) {",
                  "  var filledStars = Math.floor(data);",
                  "  var halfStar = (data % 1) >= 0.5 ? 1 : 0;",
                  "  var emptyStars = 5 - filledStars - halfStar;",
                  "  var starsHtml = '';",
                  "  for (var i = 0; i < filledStars; i++) { starsHtml += '★'; }",
                  "  if (halfStar) { starsHtml += '★'; }",
                  "  for (var i = 0; i < emptyStars; i++) { starsHtml += '☆'; }",
                  "  return '<div style=\"color: gold; font-size: 20px;\">' + starsHtml + '</div>';",
                  "}"
                )
              )
            )
          ),
          callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
        )
        
      })
      
      output$downloadExcel <- downloadHandler(
        filename = function() {
          paste("Calificacion_medico", Sys.Date(), ".xlsx", sep = "_")
        },
        content = function(file) {
          filtered <- filtered_data() 
          ptj_med <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Doctor) %>% 
            dplyr::summarise(`Resolvió sus dudas` = mean(Dudas_Resueltas),
                             `Volvería a solicitar con el médico` = mean(Volveria_Solicitar_Cita),
                             `Recibió suficiente información` = mean(Informacion_Suficiente),
                             `Volverá a solicitar el servicio` = mean(Probabilidades_Volver),
                             `Calificación de su experiencia` = mean(Calificacion_Experiencia),
                             Puntaje = mean(Puntaje_promedio)) %>%
            arrange(desc(Puntaje))
          write.xlsx(ptj_med, file, sheetName = "Calificacion", row.names = FALSE)
        }
      )
      
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      #:::::::::::::::::::       ANALISIS DE TEXTOS    :::::::::::::::::::::::::
      #:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
      
      output$plotnub <- renderWordcloud2({
        filtered <- filtered_data()
        #filtered <- filtered %>% filter(`Estado de Qx.` == "pendiente")
        if (nrow(filtered) == 0) {
          return(list(src = NULL, contentType = NULL, width = NULL, height = NULL))
        }
        filtered <- filtered %>% filter(!is.na(patient_id))
        corpus <- Corpus(VectorSource(filtered$Comentario))
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
        
        # Crear una matriz de términos de documento
        dtm <- DocumentTermMatrix(corpus)
        
        # Crear un dataframe con los términos y sus frecuencias
        word_freq <- data.frame(term = colnames(as.matrix(dtm)), freq = colSums(as.matrix(dtm)))
        
        wordcloud2(data = word_freq, size = 1.5, rotateRatio = 0, minRotation = 0, maxRotation = 0, 
                   color = "random-dark", backgroundColor = "#E5EAEC")
      })
      
      output$plotrecpal <- renderPlotly({
        
        filtered <- filtered_data()
        #filtered <- filtered %>% filter(`Estado de Qx.` == "pendiente")
        if (nrow(filtered) == 0) {
          return(list(src = NULL, contentType = NULL, width = NULL, height = NULL))
        }
        filtered <- filtered %>% filter(!is.na(patient_id))
        corpus <- Corpus(VectorSource(filtered$Comentario))
        corpus <- tm_map(corpus, content_transformer(tolower))
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, removeWords, stopwords("spanish"))
        
        # Crear una matriz de términos de documento
        dtm <- DocumentTermMatrix(corpus)
        
        # Crear un dataframe con los términos y sus frecuencias
        word_freq <- data.frame(term = colnames(as.matrix(dtm)), freq = colSums(as.matrix(dtm)))
        word_freq <- word_freq[order(-word_freq$freq), ]
        word_freq <- head(word_freq, 20)
        colors <- colorRampPalette(c("white", "#007bff"))
        
        word_freq <- word_freq %>%
          mutate(Color = colors(length(unique(freq)))[as.numeric(factor(freq))])
        
        GraficoPalabras <- ggplot(word_freq, aes(x = reorder(term, +freq), y = freq, 
                                                 text = paste0("Cantidad: ", freq))) +
          geom_bar(stat = "identity", aes(fill = Color)) +
          coord_flip() +
          geom_hline(aes(yintercept = mean(freq)),
                     color = "red", linetype = "solid", size = 1.5, show.legend = FALSE) +
          labs(x = "", y = "Cantidad", title = "Palabras mas frecuentes") +
          theme_economist() +
          theme(legend.position = "none",
                plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                plot.background = element_rect(fill = "#DEEBF7"),
                axis.title.x = element_text(face = 'bold', size = 14),
                axis.title.y = element_text(face = 'bold', size = 14)) +
          scale_fill_identity()
        
        ggplotly(GraficoPalabras, tooltip = "text")
      })
      
      output$tabcomentarios <- DT::renderDataTable({
        filtered <- filtered_data()
        resumen1 <- filtered %>% filter(!is.na(patient_id)) %>%
          select(DNI, Doctor, Tipo_Cita, Indicacion, Tipo_Cita, Tipo_comentario, Sentimiento, Puntaje_promedio, Comentario)
        
        DT::datatable(
          resumen1,
          options = list(
            pageLength = 30,
            rowCallback = JS(
              "function(row, data, index) {",
              "  var estado = data[6];", # Índice 11 corresponde a la columna `Estado de Qx.`
              "  var isDark = document.body.classList.contains('dark-mode');",
              "  var estadoClass = estado.toLowerCase().replace(' ', '-') + (isDark ? '-dark' : '-light');",
              "  $(row).addClass(estadoClass);",
              "}"
            )
          ),
          class = 'table table-striped table-hover'
        )
        
      })
      
      proxy <- dataTableProxy("tabcomentarios")
      
      output$downloadExcel2 <- downloadHandler(
        filename = function() {
          paste("comentarios", Sys.Date(), ".xlsx", sep = "_")
        },
        content = function(file) {
          isolate({
            # Obtener los datos filtrados actualmente mostrados en la tabla
            filtered <- filtered_data()
            resumen1 <- filtered %>% filter(!is.na(patient_id)) %>%
              select(DNI, Doctor, Tipo_Cita, Indicacion, Tipo_Cita, Tipo_comentario, Sentimiento, Puntaje_promedio, Comentario)
            
            current_data <- resumen1[input$tabcomentarios_rows_all,]
            
            write.xlsx(current_data, file, sheetName = "Comentarios", row.names = FALSE)
          })
        }
      )
      
      #LISTA DE PACIENTES
      
      output$tab2 <- DT::renderDataTable({
        filtered <- filtered_data()
        resumen <- filtered %>% filter(!is.na(patient_id)) %>%
          select(DNI, Date=CREADO_EL,Motivo=Tipo_Cita, 
                 Dudas_Resueltas, Volveria_Solicitar_Cita, Informacion_Suficiente, Probabilidades_Volver, Calificacion_Experiencia, Puntaje_promedio)
        
        datatable(
          resumen,
          escape = FALSE,
          options = list(
            pageLength = 20,
            columnDefs = list(
              list(
                targets = 4:8, # Índice de las columnas de puntaje
                render = JS(
                  "function(data, type, row) {",
                  "  var filledStars = Math.floor(data);",
                  "  var starColor = '';",
                  "  switch(filledStars) {",
                  "    case 1: starColor = 'red'; break;",
                  "    case 2: starColor = 'orange'; break;",
                  "    case 3: starColor = 'skyblue'; break;",
                  "    case 4: starColor = 'blue'; break;",
                  "    case 5: starColor = 'green'; break;",
                  "    default: starColor = 'black'; break;",  #// Default color if out of range
                  "  }",
                  "  var starsHtml = '';",
                  "  for (var i = 0; i < filledStars; i++) { starsHtml += '★'; }",
                  "  for (var i = filledStars; i < 5; i++) { starsHtml += '☆'; }",
                  "  return '<div style=\"color: ' + starColor + '; font-size: 20px;\">' + starsHtml + '</div>';",
                  "}"
                )
              ),
              list(
                targets = 9, # Índice de las columnas de puntaje
                render = JS(
                  "function(data, type, row) {",
                  "  var filledStars = Math.floor(data);",
                  "  var halfStar = (data % 1) >= 0.5 ? 1 : 0;",
                  "  var emptyStars = 5 - filledStars - halfStar;",
                  "  var starsHtml = '';",
                  "  for (var i = 0; i < filledStars; i++) { starsHtml += '★'; }",
                  "  if (halfStar) { starsHtml += '★'; }",
                  "  for (var i = 0; i < emptyStars; i++) { starsHtml += '☆'; }",
                  "  return '<div style=\"color: gold; font-size: 20px;\">' + starsHtml + '</div>';",
                  "}"
                )
              )
            )
          ),
          callback = JS("table.column(0).nodes().to$().css('vertical-align', 'middle');")
        )
      })
      
      output$downloadExcel3 <- downloadHandler(
        filename = function() {
          paste("lista_pacientes", Sys.Date(), ".xlsx", sep = "_")
        },
        content = function(file) {
          isolate({
            filtered <- filtered_data()
            resumen <- filtered %>% filter(!is.na(patient_id)) %>%
              select(DNI, Doctor, Date=CREADO_EL,Tipo_Cita, Indicacion, Tipo_Cita, Tipo_comentario, Sentimiento, Dudas_Resueltas, Volveria_Solicitar_Cita, Informacion_Suficiente, Probabilidades_Volver, Calificacion_Experiencia, Puntaje_promedio, Comentario)
            
            write.xlsx(resumen, file, sheetName = "Lista_paciente", row.names = FALSE)
          })
        }
      )
      
      output$positivo <- renderInfoBox({
        
        filtered <- filtered_data() 
        
        positivo <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Sentimiento) %>% 
          dplyr::summarise(Cantidad = n()) %>% mutate(Porcentaje=prop.table(Cantidad)*100) %>% filter(Sentimiento == "Positivo")
        positivo_cant <- positivo$Cantidad
        positivo_pct <- positivo$Porcentaje
        
        infoBox(
          "Sentimientos Positivos", paste0(positivo_cant," comentarios"), paste0(round(positivo_pct,2),"%"),icon = icon("face-laugh-beam"),
          color = "success",
          width = NULL
        )
      })
      
      output$neutro <- renderInfoBox({
        
        filtered <- filtered_data() 
        
        neutro <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Sentimiento) %>% 
          dplyr::summarise(Cantidad = n()) %>% mutate(Porcentaje=prop.table(Cantidad)*100) %>% filter(Sentimiento == "Neutro")
        neutro_cant <- neutro$Cantidad
        neutro_pct <- neutro$Porcentaje
        
        infoBox(
          "Sentimientos Neutros", paste0(neutro_cant," comentarios"), paste0(round(neutro_pct,2),"%"),icon = icon("face-meh"),
          color = "primary",
          width = NULL
        )
      })
      
      output$negativo <- renderInfoBox({
        
        filtered <- filtered_data() 
        
        negativo <- filtered %>% filter(!is.na(patient_id)) %>% group_by(Sentimiento) %>% 
          dplyr::summarise(Cantidad = n()) %>% mutate(Porcentaje=prop.table(Cantidad)*100) %>% filter(Sentimiento == "Negativo")
        negativo_cant <- negativo$Cantidad
        negativo_pct <- negativo$Porcentaje
        
        infoBox(
          "Sentimientos Negativos", paste0(negativo_cant," comentarios"), paste0(round(negativo_pct,2),"%"),icon = icon("face-angry"),
          color = "danger",
          width = NULL
        )
      })
      
    }
  })
  
}

shinyApp(ui = tagList(add_favicon(), ui), server = server)

