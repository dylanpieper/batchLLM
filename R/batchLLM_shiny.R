#' batchLLM Shiny Addin
#'
#' Launch a Shiny gadget that provides a user interface for batchLLM().
#'
#' @export
batchLLM_shiny <- function() {
  all_objects <- ls(envir = .GlobalEnv)
  
  if (length(all_objects) == 0) {
    stop("No objects found in the global environment. Create a data frame.")
  }
  
  df_objects <- Filter(function(x) {
    obj <- get(x, envir = .GlobalEnv)
    is.data.frame(obj) || inherits(obj, "data.frame")
  }, all_objects)
  
  if (length(df_objects) == 0) {
    stop("No data frame found in the global environment.")
  }
  
  ui <- shiny::fluidPage(
    shiny::titlePanel("batchLLM"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shinyWidgets::pickerInput(
          inputId = "llm_name",
          label = "LLM:",
          choices = c("OpenAI" = "openai", "Anthropic"  = "anthropic")
        ),
        
        shiny::textInput(
          inputId = "api_key",
          label = "API Key:",
          value = "",
          placeholder = "Enter your API key"
        ),
        
        shinyWidgets::pickerInput(
          inputId = "model",
          label = "Model:",
          choices = NULL,
          options = list(`live-search` = TRUE)
        ),
        
        shiny::sliderInput(
          inputId = "temperature",
          label = "Temperature:",
          min = 0,
          max = 1,
          value = 0.5,
          step = 0.1
        ),
        
        shinyWidgets::pickerInput(
          inputId = "df_name",
          label = "Data:",
          choices = df_objects,
          options = list(`live-search` = TRUE)
        ),
        
        shiny::uiOutput("col_name_ui"),
        
        shiny::textAreaInput(
          inputId = "prompt",
          label = "System Prompt:",
          placeholder = "What would you like the model to do?",
          rows = 3
        ),
        
        shiny::numericInput(
          inputId = "batch_size",
          label = "Batch Size (Rows per Batch):",
          value = 10,
          min = 1,
          step = 1
        ),
        
        shinyWidgets::radioGroupButtons(
          inputId = "case_convert",
          label = "Convert Text Case:",
          choices = c("None" = "none", "Uppercase" = "upper", "Lowercase" = "lower"),
          selected = "none",
          justified = TRUE,
          status = "primary"
        ),
        
        shiny::actionButton(
          inputId = "run_batchLLM",
          label = "Run batchLLM"
        )
      ),
      
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Data", DT::dataTableOutput("data_results")),
          shiny::tabPanel("Metadata", DT::dataTableOutput("metadata_table")),
          shiny::tabPanel("Get Batches", 
                          shiny::selectInput("batch_select", "Data:", choices = NULL),
                          shiny::actionButton("refresh_batches", "Refresh"),
                          shiny::hr(),
                          DT::dataTableOutput("batch_table")
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    all_objects <- shiny::reactive({
      ls(.GlobalEnv)
    })
    
    shiny::observe({
      df_objects <- Filter(function(x) {
        obj <- get(x, envir = .GlobalEnv)
        is.data.frame(obj) || inherits(obj, "data.frame")
      }, all_objects())
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "df_name",
        choices = df_objects
      )
    })
    
    shiny::observe({
      if(input$llm_name == "openai"){
        model_choices <- c(
          "GPT-4" = "gpt-4",
          "GPT-4o" = "gpt-4o",
          "GPT-4o Mini" = "gpt-4o-mini",
          "GPT-4 Turbo" = "gpt-4-turbo",
          "GPT-3.5 Turbo" = "gpt-3.5-turbo"
        )
        
        api_key <- Sys.getenv("OPENAI_API_KEY", "")
        
      } else if(input$llm_name == "anthropic"){
        model_choices <- c(
          "Claude 3.5 Sonnet" = "claude-3-5-sonnet-20240620",
          "Claude 3 Opus" = "claude-3-opus-20240229",
          "Claude 3 Sonnet" = "claude-3-sonnet-20240229",
          "Claude 3 Haiku" = "claude-3-haiku-20240307",
          "Claude 2.1" = "claude-2.1",
          "Claude 2.0" = "claude-2.0"
        )
        
        api_key <- Sys.getenv("ANTHROPIC_API_KEY", "")
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "model",
        choices = model_choices
      )
      
      shiny::updateTextInput(session, "api_key", value = api_key)
    })
    
    output$col_name_ui <- shiny::renderUI({
      shiny::req(input$df_name)
      df <- get(input$df_name, envir = .GlobalEnv)
      
      char_columns <- names(df)[sapply(df, is.character)]
      
      if (length(char_columns) == 0) {
        char_columns <- "No character columns available"
      }
      
      shiny::selectInput(
        inputId = "col_name",
        label = "Column:",
        choices = char_columns,
        selected = if (length(char_columns) > 0) char_columns[1] else NULL
      )
    })
    
    selected_data <- shiny::reactive({
      shiny::req(input$df_name)
      get(input$df_name, envir = .GlobalEnv)
    })
    
    current_metadata <- shiny::reactive({
      shiny::req(input$df_name, input$col_name)
      df <- selected_data()
      key <- digest::digest(df[[input$col_name]], algo = "crc32c")
      df_name_key <- paste0(input$df_name, "_", key)
      scrape_metadata(df_name = df_name_key)
    })
    
    result <- shiny::reactiveVal(NULL)
    
    output$data_results <- DT::renderDataTable({
      if (is.null(result())) {
        DT::datatable(selected_data(), options = list(
          autoWidth = FALSE,
          columnDefs = list(
            list(
              targets = "_all",
              render = DT::JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data != null ?",
                "'<div style=\"max-width: 250px; word-wrap: break-word; white-space: normal;\">' + data + '</div>' : data;",
                "}"
              ),
              createdCell = DT::JS(
                "function (td, cellData, rowData, row, col) {",
                "$(td).css({",
                "  'max-width': '250px',",
                "  'word-wrap': 'break-word',",
                "  'white-space': 'normal'",
                "});",
                "}"
              )
            )
          ),
          scrollX = TRUE,
          pageLength = 10
        ))
      } else {
        DT::datatable(result(), options = list(
          autoWidth = FALSE,
          columnDefs = list(
            list(
              targets = "_all",
              render = DT::JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data != null ?",
                "'<div style=\"max-width: 250px; word-wrap: break-word; white-space: normal;\">' + data + '</div>' : data;",
                "}"
              ),
              createdCell = DT::JS(
                "function (td, cellData, rowData, row, col) {",
                "$(td).css({",
                "  'max-width': '250px',",
                "  'word-wrap': 'break-word',",
                "  'white-space': 'normal'",
                "});",
                "}"
              )
            )
          ),
          scrollX = TRUE,
          pageLength = 10
        ))
      }
    })
    
    shiny::observeEvent(input$run_batchLLM, {
      shiny::req(input$df_name, input$col_name, input$prompt, input$api_key)
      
      api_key <- input$api_key
      
      if (input$llm_name == "openai") {
        Sys.setenv(OPENAI_API_KEY = api_key)
      } else {
        Sys.setenv(ANTHROPIC_API_KEY = api_key)
      }
      
      result_data <- spsComps::shinyCatch(
        batchLLM(
          LLM = input$llm_name,
          df_name = input$df_name,
          col_name = input$col_name,
          prompt = input$prompt,
          batch_size = input$batch_size,
          model = input$model,
          temperature = input$temperature,
          case_convert = input$case_convert
        ),
        prefix = ''
      )
      
      result(result_data)
    })
    
    output$metadata_table <- DT::renderDataTable({
      meta_data <- current_metadata()
      
      if (is.null(meta_data) || nrow(meta_data) == 0) {
        return(DT::datatable(data.frame(Message = "No metadata available for this data frame.")))
      }
      
      DT::datatable(meta_data, options = list(
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            render = DT::JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null ?",
              "'<div style=\"max-width: 250px; word-wrap: break-word; white-space: normal;\">' + data + '</div>' : data;",
              "}"
            ),
            createdCell = DT::JS(
              "function (td, cellData, rowData, row, col) {",
              "$(td).css({",
              "  'max-width': '250px',",
              "  'word-wrap': 'break-word',",
              "  'white-space': 'normal'",
              "});",
              "}"
            )
          )
        ),
        scrollX = TRUE,
        pageLength = 10
      ))
    })
    
    metadata <- shiny::reactive({
      scrape_metadata()
    })
    
    shiny::observe({
      shiny::req(metadata())
      batch_choices <- unique(metadata()$df_name)
      shiny::updateSelectInput(session, "batch_select", choices = batch_choices)
    })
    
    shiny::observeEvent(input$refresh_batches, {
      spsComps::shinyCatch({
        metadata <- scrape_metadata()
        batch_choices <- unique(metadata$df_name)
        
        current_selection <- shiny::isolate(input$batch_select)
        
        shiny::updateSelectInput(session, "batch_select", choices = batch_choices)
        
        shiny::updateSelectInput(session, "batch_select", selected = current_selection)
        
        message <- paste("Batches were successfully refreshed.")
        message(message)
      }, prefix = '')
    })
    
    selected_batch <- shiny::reactive({
      shiny::req(input$batch_select)
      get_batches(input$batch_select)
    })
    
    output$batch_table <- DT::renderDataTable({
      shiny::req(selected_batch())
      DT::datatable(selected_batch(), options = list(
        autoWidth = FALSE,
        columnDefs = list(
          list(
            targets = "_all",
            render = DT::JS(
              "function(data, type, row, meta) {",
              "return type === 'display' && data != null ?",
              "'<div style=\"max-width: 250px; word-wrap: break-word; white-space: normal;\">' + data + '</div>' : data;",
              "}"
            ),
            createdCell = DT::JS(
              "function (td, cellData, rowData, row, col) {",
              "$(td).css({",
              "  'max-width': '250px',",
              "  'word-wrap': 'break-word',",
              "  'white-space': 'normal'",
              "});",
              "}"
            )
          )
        ),
        scrollX = TRUE,
        pageLength = 10
      ))
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::browserViewer())
}