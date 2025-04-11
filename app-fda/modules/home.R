

box::use(
  bslib[card, layout_column_wrap, card_header, card_body, value_box, card_footer],
  shiny[NS, moduleServer, selectInput, tagList, radioButtons, reactive,
        renderText, textOutput, observe, bindEvent, modalDialog, showModal, tags,
        tagAppendAttributes, actionButton, actionLink, icon],
  bsicons[bs_icon],
  reactable[reactable, renderReactable, reactableOutput, colDef, colFormat],
  utils[head],
  scales[label_comma],
  echarts4r[e_add_nested, echarts4rOutput, e_x_axis, e_legend, e_color,
            e_labels, e_y_axis, e_charts, e_tooltip, e_bar, renderEcharts4r,
            e_grid],
  tidyr[pivot_longer],
  dplyr[filter, group_by, mutate, arrange, across, where],
  webshot2[webshot],
  htmlwidgets[saveWidget],
  shinychat[chat_ui, chat_clear, chat_append],
  ellmer[chat_openai, content_image_file]
)

ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    layout_column_wrap(fill = F,
      width = 1/3,
      value_box(
        title = "Total Reports",
        value = textOutput(ns("txt_total")),
        showcase = bs_icon("graph-up-arrow", size = 35),
        theme = "primary"
        ),
      value_box(
        title = "Serious Reports (excluding death)",
        value = textOutput(ns("txt_serious")),
        showcase = bs_icon("exclamation-circle-fill", size = 35),
        theme = "warning"
      ),
      value_box(
        title = "Death Reports",
        value = textOutput(ns("txt_death")),
        showcase = bs_icon("x-circle-fill", size = 35), 
        theme = "danger" 
      )
    ),
    layout_column_wrap(
      fill = F,
      fillable = F,
      width = 1/3,
      inline = T,
      selectInput(
        inputId = ns("reports"),
        label = "Reports by",
        choices = c(
          "Report Type" = "report_type",
          "Reporter" = "reporter",
          "Age Group" = "age_group",
          "Sex" = "sex",
          "Report Seriousness" = "report_seriousness",
          "Report Region" = "report_region"
          )
      ),
      radioButtons(
        inputId = ns("radio"),
        label = "Options",
        choices = list(
          "All Years" = "all_years",
          "Last 10 Years" = "last_10_years"
        )
      )
    ),
    layout_column_wrap(
      width = 1/2,
      card(
        full_screen = T,
        card_body(
          reactableOutput(ns("tbl_datos"))
        )
      ),
      card(
        full_screen = T,
        card_header(
          class = "d-flex justify-content-end",
          actionLink(
            inputId = ns("analyze_chart"),
            bs_icon("chat-text", size = "1.2em", title = "Analyze", class = "text-dark"),
            aria_label = "Analyze chart"
          )
        ),
        card_body(
          echarts4rOutput(ns("plt_datos"), height = "300px")
        )
      )
    )
  )
  
}

server <- function(id, db_report_type, db_reporter, db_reporter_region,
                   db_report_seriousness, db_report_age_group, db_sex){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns

    datos <- reactive({

      if(input$reports == "report_type"){

        datos <- db_report_type

      } else if(input$reports == "reporter"){
        
        datos <- db_reporter
        
      } else if(input$reports == "age_group"){
        
        datos <- db_report_age_group
        
      } else if(input$reports == "sex"){
        
        datos <- db_sex
        
      } else if(input$reports == "report_seriousness"){
        
        datos <- db_report_seriousness
        
      } else if(input$reports == "report_region"){
        
        datos <- db_reporter_region
        
      }
      
      if(input$radio == "all_years"){
        datos
      } else{
        datos <- datos |> 
          head(10)
      }

      datos
    })
    
    plt <- reactive({
      
      df_datos <- datos() |> 
        pivot_longer(-year, names_to = "names", values_to = "values") |> 
        filter(names != "total_reports") |> 
        mutate(year = as.character(year)) |> 
        arrange(year)
      
      df_datos |>
        group_by(names) |> 
        e_charts(year) |>
        e_bar(values, stack = "group") |> 
        e_tooltip() |> 
        e_grid(
          left = "5%", 
          right = "5%", 
          top = "10%", 
          bottom = "10%"
        )
      
      
    })
    
    output$txt_total   <- renderText({
      
      total <- sum(datos()$total_reports, na.rm = T)
      label_comma()(total)
      
    })
    output$txt_serious <- renderText({
      
      if(input$radio == "all_years"){total <- 16664479} else{total <- 10773787}
      label_comma()(total)
      
    })
    output$txt_death   <- renderText({
      
      if(input$radio == "all_years"){total <- 2722806} else{total <- 1685123}
      label_comma()(total)
      
    })
    output$tbl_datos   <- renderReactable({

      datos <- datos() |> 
        mutate(across(
          where(is.numeric) & !any_of("year"),
          label_comma()
        ))
      
      reactable(
        data = datos,
        compact = T,
        sortable = F,
        highlight = T,
        defaultPageSize = 60,
        style = list(fontSize = "0.8rem"),
        columns = list(
          year = colDef(name = "Year", width = 60),
          total_reports = colDef(name = "Total Reports", format = colFormat(separators = T))
        )
        )
      
    })
    
    output$plt_datos   <- renderEcharts4r({
      
      plt()

      
    })
    
    
    chat <- chat_openai(
      model = "gpt-4o-2024-11-20",
      system_prompt = "You are a data analyst from th U.S. Food & Drug Administration.
      You are analyzing the report counts based on the occupation of the
      Reporter, the person who submitted the report to FDA or the person who
      submitted the report to the manufacturer (who then sent the report to
      FDA). Physicians and pharmacists are the Healthcare Professionals
      (HCPs) who submit reports to FDA most frequently.  Additional HCPs
      include nurses, dentists and other medical personnel. Reporters may
      also be classified as 'Consumer', 'Other' for all other Reporters who
      are not documented as Healthcare Professionals or Consumers, and 'Not
      Specified' where the occupation of the Reporter was not provided.")
    
    observe({
      
      # This is faster when implementing ggplot or plotly
      saveWidget(widget = plt(), file = "plot.html")
      webshot('plot.html', file = 'www/plot.png')
      
      showModal(
        ui = modalDialog(
          style = "background-color: white;",
          easyClose = T,
          size = "l",
          title = NULL,
          tags$button(
            type = "button",
            class = "btn-close d-block ms-auto mb-3",
            `data-bs-dismiss` = "modal",
            aria_label = "Close",
          ),
          tags$img(
            src = paste0("plot.png?rand=", as.integer(Sys.time())),
            class = "d-block border mx-auto mb-3",
            style = "max-width: min(100%, 500px); background-color: white;"
          ),
          chat_ui(
            id = ns("chat"),
            style="max-height: min(60vh, 500px);"
          ), 
          footer =  actionButton(
            inputId = ns("clear"),
            label = "Clear",
            icon = icon("eraser")
          )
        ) |> 
          tagAppendAttributes(style = "--bs-modal-margin: 1.75rem;")
      )
      
      prompt_analisis <-"Analyze this chart and give two importants observations."
      
      stream <- chat$stream_async(
        content_image_file("www/plot.png"),
        prompt_analisis
      )
      chat_append("chat", stream)
      
    }) |> bindEvent(input$analyze_chart)
    
    observe({
      
      stream <- chat$stream_async(input$chat_user_input)
      chat_append("chat", stream)
      
    }) |> bindEvent(input$chat_user_input)
    
    observe({
      chat_clear("chat")
    }) |> bindEvent(input$clear)
    
    
    
  })
}