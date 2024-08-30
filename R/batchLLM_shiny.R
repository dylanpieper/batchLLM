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
    shinyjs::useShinyjs(),
    shiny::titlePanel("batchLLM"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shinyWidgets::pickerInput(
          inputId = "llm_name",
          label = "LLM:",
          choices = c(
            "OpenAI" = "openai",
            "Anthropic" = "anthropic",
            "Google" = "google"
          )
        ),
        shinyjs::hidden(
          shiny::textInput(
            inputId = "api_key",
            label = "API Key:",
            value = "",
            placeholder = "Enter your API key"
          )
        ),
        shiny::actionButton(
          inputId = "toggle_api_key",
          label = "Show / Hide API Key"
        ),
        shiny::HTML("</br></br>"),
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
        shinyWidgets::radioGroupButtons(
          inputId = "toggle_delay",
          label = "Batch Delay:",
          choices = c("Random" = "random", "1 Minute" = "1min", "30 Seconds" = "30sec"),
          selected = "random",
          justified = TRUE,
          status = "primary"
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
          id = "tabsetPanel",
          shiny::tabPanel("Data", DT::dataTableOutput("data_results")),
          shiny::tabPanel("Metadata", DT::dataTableOutput("metadata_table")),
          shiny::tabPanel(
            "Get Batches",
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
    shiny::observeEvent(input$toggle_api_key, {
      shinyjs::toggle("api_key")
    })
    
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
      if (input$llm_name == "openai") {
        model_choices <- c(
          "GPT-4" = "gpt-4",
          "GPT-4o" = "gpt-4o",
          "GPT-4o Mini" = "gpt-4o-mini",
          "GPT-4 Turbo" = "gpt-4-turbo",
          "GPT-3.5 Turbo" = "gpt-3.5-turbo"
        )
        
        api_key <- Sys.getenv("OPENAI_API_KEY", "")
      } else if (input$llm_name == "anthropic") {
        model_choices <- c(
          "Claude 3.5 Sonnet" = "claude-3-5-sonnet-20240620",
          "Claude 3 Opus" = "claude-3-opus-20240229",
          "Claude 3 Sonnet" = "claude-3-sonnet-20240229",
          "Claude 3 Haiku" = "claude-3-haiku-20240307",
          "Claude 2.1" = "claude-2.1",
          "Claude 2.0" = "claude-2.0"
        )
        
        api_key <- Sys.getenv("ANTHROPIC_API_KEY", "")
      } else if (input$llm_name == "google") {
        model_choices <- c(
          "Gemini 1.5 Pro" = "1.5-pro",
          "Gemini 1.5 Flash" = "1.5-flash",
          "Gemini 1 Pro" = "1.0-pro"
        )
        
        api_key <- Sys.getenv("GEMINI_API_KEY", "")
      }
      
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "model",
        choices = model_choices
      )
      
      shiny::updateTextInput(session, "api_key", value = api_key)
      
      if (api_key != "") {
        shinyjs::hide("api_key")
      } else {
        shinyjs::show("api_key")
      }
    })
    
    output$col_name_ui <- shiny::renderUI({
      shiny::req(input$df_name)
      df <- get(input$df_name, envir = .GlobalEnv)
      
      char_columns <- names(df)[sapply(df, is.character)]
      
      if (length(char_columns) == 0) {
        char_columns <- "No character columns available."
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
      } else if (input$llm_name == "anthropic") {
        Sys.setenv(ANTHROPIC_API_KEY = api_key)
      } else if (input$llm_name == "google") {
        Sys.setenv(GEMINI_API_KEY = api_key)
      }
      
      result_data <- spsComps::shinyCatch(
        batchLLM(
          LLM = input$llm_name,
          df_name = input$df_name,
          col_name = input$col_name,
          prompt = input$prompt,
          batch_delay = input$toggle_delay,
          batch_size = input$batch_size,
          model = input$model,
          temperature = input$temperature,
          case_convert = input$case_convert
        ),
        prefix = ""
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
    
    current_batch_data <- shiny::reactiveVal(NULL)
    
    update_batch_data <- function() {
      metadata <- scrape_metadata()
      batch_choices <- unique(metadata$df_name)
      shiny::updateSelectInput(session, "batch_select", choices = batch_choices)
      
      if (!is.null(input$batch_select) && input$batch_select %in% batch_choices) {
        batch_data <- get_batches(input$batch_select)
        current_batch_data(batch_data)
      } else if (length(batch_choices) > 0) {
        batch_data <- get_batches(batch_choices[1])
        current_batch_data(batch_data)
        shiny::updateSelectInput(session, "batch_select", selected = batch_choices[1])
      } else {
        current_batch_data(NULL)
      }
    }
    
    shiny::observeEvent(input$tabsetPanel,
                        {
                          if (input$tabsetPanel == "Get Batches") {
                            update_batch_data()
                          }
                        },
                        ignoreInit = TRUE
    )
    
    shiny::observeEvent(input$refresh_batches, {
      spsComps::shinyCatch(
        {
          update_batch_data()
          message("Batches were successfully refreshed.")
        },
        prefix = ""
      )
    })
    
    shiny::observeEvent(input$batch_select, {
      shiny::req(input$batch_select)
      batch_data <- get_batches(input$batch_select)
      current_batch_data(batch_data)
    })
    
    output$batch_table <- DT::renderDataTable({
      shiny::req(current_batch_data())
      DT::datatable(current_batch_data(), options = list(
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
