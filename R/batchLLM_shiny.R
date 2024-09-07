#'
#' Launch a Shiny gadget that provides a user interface for batchLLM().
#'
#' @export
#' @importFrom shiny fluidPage fluidRow column titlePanel tabPanel tabsetPanel conditionalPanel HTML sidebarLayout sidebarPanel
#' @importFrom shiny textInput numericInput downloadButton updateTextInput tags br hr h1 h4 img uiOutput textAreaInput
#' @importFrom shiny sliderInput actionButton icon mainPanel observe req
#' @importFrom shiny selectInput updateSelectInput renderUI observeEvent
#' @importFrom shiny runGadget paneViewer fileInput showNotification
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#' @importFrom shinydashboard sidebarMenu menuItem dashboardBody tabItems
#' @importFrom shinydashboard tabItem box
#' @importFrom shinyWidgets pickerInput updatePickerInput radioGroupButtons
#' @importFrom DT dataTableOutput renderDataTable datatable
#' @importFrom shinyjs useShinyjs toggle hidden show hide
#' @importFrom rlang sym eval_tidy
#' @importFrom digest digest
#' @importFrom spsComps shinyCatch
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom tools file_ext file_path_sans_ext
batchLLM_shiny <- function() {
  library(batchLLM)
  
  df_objects <- "beliefs"

  ui <- dashboardPage(
    skin = "black",
    dashboardHeader(title = "BatchLLM"),
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Run Batches", tabName = "run_batches", icon = icon("table")),
        menuItem("Get Batches", tabName = "get_batches", icon = icon("list-ol")),
        menuItem("Download Log", tabName = "download_log", icon = icon("download"))
      )
    ),
    dashboardBody(
      useShinyjs(),
      tabItems(
        tabItem(
          tabName = "home",
          fluidRow(
            column(
              width = 12,
              box(
                width = 6,
                solidHeader = TRUE,
                tags$div(
                  img(src = "https://raw.githubusercontent.com/dylanpieper/batchLLM/main/inst/batchLLM_hexLogo.png", height = "200px"),
                  h1("Welcome!"),
                  h4("Use this Shiny app to batch process Large Language Model (LLM) text completions by looping across the rows of a data frame column. This tool is an efficient solution for handling large datasets with the flexibility to configure multiple models and automate the storage of output and metadata."),
                  br(),
                  h4(tags$strong("Developer's Note:")),
                  h4("The initial inspiration for creating this tool came from my work on a complex classification problem involving court data. I faced the challenge of processing thousands of unique offense descriptions, and later, I tested the functionality to classify drug metabolites in toxicology data. The original function evolved significantly, and today, it powers this Shiny app designed to streamline and scale the use of LLMs across various datasets. I hope this tool proves as valuable to you as it has in my own projects."),
                  br(),
                  tags$a(
                    href = "https://platform.openai.com/login?launch",
                    target = "_blank",
                    shiny::icon("key"),
                    "OpenAI"
                  ),
                  tags$a(
                    href = "https://console.anthropic.com/",
                    target = "_blank",
                    shiny::icon("key"),
                    "Anthropic"
                  ),
                  tags$a(
                    href = "https://gemini.google.com/",
                    target = "_blank",
                    shiny::icon("key"),
                    "Google Gemini"
                  ),
                  br(), br(),
                  tags$a(
                    href = "https://github.com/dylanpieper/batchLLM",
                    target = "_blank",
                    shiny::icon("github"),
                    "GitHub Source Code"
                  )
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "run_batches",
          fluidRow(
            column(
              width = 4,
              box(
                width = NULL,
                title = "Upload Data File",
                solidHeader = TRUE,
                fileInput("datafile", "Choose CSV or Excel File", accept = c(".csv", ".xlsx"))
              ),
              box(
                width = NULL,
                title = "Configure BatchLLM",
                solidHeader = TRUE,
                pickerInput(
                  inputId = "df_name",
                  label = "Select Data:",
                  choices = df_objects,
                  options = list(`live-search` = TRUE)
                ),
                uiOutput("col_name_ui"),
                textAreaInput(
                  inputId = "prompt",
                  label = "System Prompt:",
                  placeholder = "e.g., classify as a fact or misinformation in one word",
                  rows = 1
                ),
                uiOutput("llm_configs"),
                actionButton(
                  inputId = "add_llm_config",
                  label = "Add LLM"
                ),
                br(), br(),
                uiOutput("api_key_inputs"),
                radioGroupButtons(
                  inputId = "toggle_delay",
                  label = "Batch Delay:",
                  choices = c("Random (~10 Sec)" = "random", "30 Sec" = "30sec", "1 Min" = "1min"),
                  selected = "random",
                  justified = TRUE
                ),
                numericInput(
                  inputId = "batch_size",
                  label = "Batch Size (Rows per Batch):",
                  value = 10,
                  min = 1,
                  step = 1
                ),
                radioGroupButtons(
                  inputId = "case_convert",
                  label = "Convert Text Case:",
                  choices = c("None" = "none", "Uppercase" = "upper", "Lowercase" = "lower"),
                  selected = "none",
                  justified = TRUE
                ),
                actionButton(
                  inputId = "run_batchLLM",
                  label = "Run BatchLLM"
                )
              )
            ),
            column(
              width = 8,
              box(
                width = NULL,
                title = "Viewer",
                solidHeader = TRUE,
                tabsetPanel(
                  tabPanel("Data", DT::dataTableOutput("data_results")),
                  tabPanel("Metadata", DT::dataTableOutput("metadata_table"))
                )
              )
            )
          )
        ),
        tabItem(
          tabName = "get_batches",
          fluidRow(
            box(
              width = 12,
              title = "Get Batches",
              solidHeader = TRUE,
              selectInput("batch_select", "Select Data:", choices = NULL),
              actionButton("refresh_batches", "Refresh"),
              hr(),
              DT::dataTableOutput("batch_table")
            )
          )
        ),
        tabItem(
          tabName = "download_log",
          fluidRow(
            box(
              width = 12,
              title = "Download Log",
              solidHeader = TRUE,
              tabItem(
                tabName = "download_log",
                fluidRow(
                  box(
                    width = 12,
                    solidHeader = TRUE,
                    conditionalPanel(
                      condition = "output.log_file_exists",
                      downloadButton("download_rds", "Download RDS"),
                      downloadButton("download_json", "Download JSON")
                    ),
                    conditionalPanel(
                      condition = "!output.log_file_exists",
                      h4("No log file available for download.")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    if (exists("beliefs")) {
      session$userData$beliefs <- beliefs
    }

    all_objects <- reactive({
      c(ls(envir = .GlobalEnv), ls(envir = session$userData))
    })

    observe({
      df_objects <- Filter(function(x) {
        obj <- if (exists(x, envir = session$userData)) {
          get(x, envir = session$userData)
        } else {
          get(x, envir = .GlobalEnv)
        }
        is.data.frame(obj) || inherits(obj, "data.frame")
      }, all_objects())

      updatePickerInput(
        session = session,
        inputId = "df_name",
        choices = df_objects
      )
    })

    observeEvent(input$datafile, {
      req(input$datafile)
      file <- input$datafile$datapath
      ext <- file_ext(file)
      df <- switch(ext,
        csv = readr::read_csv(file),
        xlsx = readxl::read_excel(file),
        stop("Invalid file; Please upload a .csv or .xlsx file")
      )
      df_name <- file_path_sans_ext(input$datafile$name)
      session$userData[[df_name]] <- df
      updatePickerInput(
        session = session,
        inputId = "df_name",
        choices = c(all_objects(), df_name),
        selected = df_name
      )
      message("Data file loaded successfully")
    })

    llm_configs <- reactiveVal(list())
    next_id <- reactiveVal(1)

    observeEvent(input$add_llm_config, {
      current_configs <- llm_configs()
      new_id <- next_id()

      new_config <- list(
        id = paste0("Config-", new_id),
        llm = NULL,
        model = NULL,
        temperature = 0.5
      )

      llm_configs(c(current_configs, list(new_config)))
      next_id(new_id + 1)
    })

    output$llm_configs <- renderUI({
      lapply(llm_configs(), function(config) {
        box(
          width = NULL,
          title = config$id,
          pickerInput(
            inputId = paste0(config$id, "_llm"),
            label = "LLM:",
            choices = c("OpenAI" = "openai", "Anthropic" = "anthropic", "Google" = "gemini")
          ),
          pickerInput(
            inputId = paste0(config$id, "_model"),
            label = "Model:",
            choices = NULL
          ),
          sliderInput(
            inputId = paste0(config$id, "_temperature"),
            label = "Temperature:",
            min = 0,
            max = 1,
            value = config$temperature,
            step = 0.1
          ),
          actionButton(
            inputId = paste0("remove_", config$id),
            label = "Remove LLM"
          )
        )
      })
    })

    output$api_key_inputs <- renderUI({
      req(length(llm_configs()) > 0)

      unique_llms <- unique(sapply(llm_configs(), function(config) input[[paste0(config$id, "_llm")]]))

      lapply(unique_llms, function(llm) {
        if (!is.null(llm)) {
          env_var_name <- paste0(toupper(llm), "_API_KEY")
          existing_key <- Sys.getenv(env_var_name)

          textInput(
            inputId = paste0(llm, "_api_key"),
            label = paste0(toupper(llm), " API KEY:"),
            value = existing_key,
            placeholder = paste("Enter your", toupper(llm), "API KEY")
          )
        }
      })
    })

    observe({
      lapply(llm_configs(), function(config) {
        observeEvent(input[[paste0(config$id, "_llm")]], {
          llm_name <- input[[paste0(config$id, "_llm")]]
          if (llm_name == "openai") {
            model_choices <- c(
              "GPT-4" = "gpt-4",
              "GPT-4o" = "gpt-4o",
              "GPT-4o Mini" = "gpt-4o-mini",
              "GPT-4 Turbo" = "gpt-4-turbo",
              "GPT-3.5 Turbo" = "gpt-3.5-turbo"
            )
          } else if (llm_name == "anthropic") {
            model_choices <- c(
              "Claude 3.5 Sonnet" = "claude-3-5-sonnet-20240620",
              "Claude 3 Opus" = "claude-3-opus-20240229",
              "Claude 3 Sonnet" = "claude-3-sonnet-20240229",
              "Claude 3 Haiku" = "claude-3-haiku-20240307",
              "Claude 2.1" = "claude-2.1",
              "Claude 2.0" = "claude-2.0"
            )
          } else if (llm_name == "gemini") {
            model_choices <- c(
              "Gemini 1.5 Pro" = "1.5-pro",
              "Gemini 1.5 Flash" = "1.5-flash",
              "Gemini 1 Pro" = "1.0-pro"
            )
          }
          updatePickerInput(
            session = session,
            inputId = paste0(config$id, "_model"),
            choices = model_choices
          )
        })

        observeEvent(input[[paste0("remove_", config$id)]], {
          current_configs <- llm_configs()
          updated_configs <- current_configs[!sapply(current_configs, function(x) x$id == config$id)]
          llm_configs(updated_configs)
        })
      })
    })

    output$col_name_ui <- renderUI({
      req(input$df_name)
      df <- if (exists(input$df_name, envir = session$userData)) {
        get(input$df_name, envir = session$userData)
      } else {
        get(input$df_name, envir = .GlobalEnv)
      }

      char_columns <- names(df)[sapply(df, is.character)]

      if (length(char_columns) == 0) {
        char_columns <- "No character columns available."
      }

      pickerInput(
        inputId = "col_name",
        label = "Column:",
        choices = char_columns,
        selected = if (length(char_columns) > 0) char_columns[1] else NULL
      )
    })

    selected_data <- reactive({
      req(input$df_name)
      if (exists(input$df_name, envir = session$userData)) {
        get(input$df_name, envir = session$userData)
      } else {
        get(input$df_name, envir = .GlobalEnv)
      }
    })

    metadata_trigger <- reactiveVal(0)

    current_metadata <- reactive({
      metadata_trigger()
      req(input$df_name, input$col_name)
      df <- selected_data()
      key <- digest::digest(df[[input$col_name]], algo = "crc32c")
      df_name_key <- paste0(input$df_name, "_", key)
      return(scrape_metadata(df_name = df_name_key))
    })

    observeEvent(input$tabs, {
      if (input$tabs == "metadata") {
        metadata_trigger(metadata_trigger() + 1)
      }
    })

    result <- reactiveVal(NULL)
    log_file_exists <- reactiveVal(FALSE)

    observeEvent(input$run_batchLLM, {
      req(input$df_name, input$col_name, input$prompt)

      configs <- lapply(llm_configs(), function(config) {
        list(
          LLM = input[[paste0(config$id, "_llm")]],
          model = input[[paste0(config$id, "_model")]],
          temperature = input[[paste0(config$id, "_temperature")]]
        )
      })

      missing_keys <- sapply(configs, function(config) {
        api_key_input <- input[[paste0(config$LLM, "_api_key")]]
        is.null(api_key_input) || api_key_input == ""
      })

      if (any(missing_keys)) {
        showNotification("Please enter API keys for all configurations.", type = "error")
        return()
      }

      shinyCatch(
        {
          df <- selected_data()

          for (config in configs) {
            env_var_name <- paste0(toupper(config$LLM), "_API_KEY")
            api_key <- input[[paste0(config$LLM, "_api_key")]]
            do.call("Sys.setenv", setNames(list(api_key), env_var_name))
          }

          for (config in configs) {
            df <- batchLLM(
              LLM = if (config$LLM == "gemini") "google" else config$LLM,
              df = df,
              df_name = input$df_name,
              col = !!sym(input$col_name),
              prompt = input$prompt,
              batch_delay = input$toggle_delay,
              batch_size = input$batch_size,
              model = config$model,
              temperature = config$temperature,
              case_convert = input$case_convert
            )
          }
          if (file.exists("batchLLM-log.rds")) {
            log_file_exists(TRUE)
          }
          result(df)
          metadata_trigger(metadata_trigger() + 1)
        },
        prefix = ""
      )
    })

    observe({
      lapply(c("openai", "anthropic", "google"), function(llm) {
        observeEvent(input[[paste0(llm, "_api_key")]], {
          env_var_name <- paste0(toupper(llm), "_API_KEY")
          api_key <- input[[paste0(llm, "_api_key")]]
          do.call("Sys.setenv", setNames(list(api_key), env_var_name))
        })
      })
    })

    output$data_results <- renderDataTable({
      uploaded_data <- selected_data()
      current_result <- result()

      if (!is.null(current_result) && ncol(current_result) > ncol(uploaded_data)) {
        data_to_show <- current_result
      } else if (!identical(uploaded_data, current_result)) {
        data_to_show <- uploaded_data
        result(NULL)
      } else {
        data_to_show <- if (is.null(current_result)) uploaded_data else current_result
      }

      datatable(data_to_show,
        extensions = "Buttons",
        options = list(
          dom = "Blfrtip",
          buttons = list("copy", list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          ))
        )
      )
    })

    output$metadata_table <- renderDataTable({
      meta_data <- current_metadata()

      if (is.null(meta_data) || nrow(meta_data) == 0) {
        return(datatable(data.frame(Message = "No metadata available.")))
      }

      datatable(meta_data,
        extensions = "Buttons",
        options = list(
          dom = "Blfrtip",
          buttons = list("copy", list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          ))
        )
      )
    })

    current_batch_data <- reactiveVal(NULL)

    update_batch_data <- function() {
      metadata <- scrape_metadata()
      batch_choices <- unique(metadata$df_name)
      updateSelectInput(session, "batch_select", choices = batch_choices)

      if (!is.null(input$batch_select) && input$batch_select %in% batch_choices) {
        batch_data <- get_batches(input$batch_select)
        current_batch_data(batch_data)
      } else if (length(batch_choices) > 0) {
        batch_data <- get_batches(batch_choices[1])
        current_batch_data(batch_data)
        updateSelectInput(session, "batch_select", selected = batch_choices[1])
      } else {
        current_batch_data(NULL)
      }
    }

    observeEvent(input$tabs,
      {
        if (input$tabs == "get_batches") {
          shinyCatch(
            {
              update_batch_data()
            },
            prefix = ""
          )
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(input$refresh_batches, {
      shinyCatch(
        {
          update_batch_data()
          message("Batches were successfully refreshed.")
        },
        prefix = ""
      )
    })

    observeEvent(input$batch_select, {
      req(input$batch_select)
      batch_data <- get_batches(input$batch_select)
      current_batch_data(batch_data)
    })

    output$batch_table <- renderDataTable({
      req(current_batch_data())
      datatable(current_batch_data(),
        extensions = "Buttons",
        options = list(
          dom = "Blfrtip",
          buttons = list("copy", list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          ))
        )
      )
    })

    observe({
      req(input$df_name)
      updateSelectInput(session, "col_name", selected = names(selected_data())[1])
    })

    output$log_file_exists <- reactive({
      log_file_exists()
    })

    outputOptions(output, "log_file_exists", suspendWhenHidden = FALSE)

    observe({
      output$download_log_tab <- renderUI({
        menuItem("Download Log", tabName = "download_log", icon = icon("download"))
      })
    })

    observe({
      if (log_file_exists()) {
        shinyjs::show("download_rds")
        shinyjs::show("download_json")
      } else {
        shinyjs::hide("download_rds")
        shinyjs::hide("download_json")
      }
    })

    output$download_log_tab <- renderUI({
      if (log_file_exists()) {
        menuItem("Download Log", tabName = "download_log", icon = icon("download"))
      }
    })

    output$download_rds <- downloadHandler(
      filename = function() {
        paste0("batchLLM-log-", Sys.Date(), ".rds")
      },
      content = function(file) {
        log_file <- "batchLLM-log.rds"
        if (file.exists(log_file)) {
          file.copy(log_file, file)
        } else {
          stop("No log file found to download.")
        }
      }
    )

    output$download_json <- downloadHandler(
      filename = function() {
        paste0("batchLLM-log-", Sys.Date(), ".json")
      },
      content = function(file) {
        log_file <- "batchLLM-log.rds"
        if (file.exists(log_file)) {
          log_data <- readRDS(log_file)
          write_json(log_data, path = file, pretty = TRUE, auto_unbox = TRUE)
        } else {
          stop("No log file found to download.")
        }
      }
    )
  }

  runGadget(ui, server, viewer = paneViewer())
}
