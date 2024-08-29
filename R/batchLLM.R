#' @title batchLLM
#'
#' @description Batch process Large Language Model (LLM) text generation models on data frames with local storage and metadata.
#' Effortlessly loop across rows of a column and generate text completions with minimal supervision.
#' The package currently supports OpenAI's GPT, Anthropic's Claude, and Google's Gemini models, with built-in delays for API etiquette.
#' The package provides advanced text processing features, including automatic logging of batches and metadata to local files, side-by-side comparison of outputs from different LLMs, and integration of a user-friendly Shiny App Addin.
#' Use cases include natural language processing tasks such as sentiment analysis, thematic analysis, classification, labeling or tagging, and language translation.
#'
#' @param df_name A string for the name of a data frame.
#' @param col_name A string for the name of a column.
#' @param prompt A system prompt for the GPT model.
#' @param LLM A string for the name of the LLM with the options: "openai", "anthropic", and "google". Default is "openai".
#' @param model A string for the name of the model from the LLM. Default is "gpt-4o-mini".
#' @param temperature A temperature for the GPT model. Default is .5.
#' @param batch_size The number of rows to process in each batch. Default is 10.
#' @param attempts The maximum number of loop retry attempts. Default is 1.
#' @param log_name A string for the name of the log without the \code{.rds} file extension. Default is "batchLLM-log".
#' @param hash_algo A string for a hashing algorithm from the 'digest' package. Default is \code{crc32c}.
#' @param case_convert A string for the case conversion of the output with the options: "upper", "lower", or NULL (no change). Default is NULL.
#' @return
#' Assigns a column with a hashed name containing the text completion output.
#' Writes the output and metadata to the log file after each batch in a nested list format:
#' \itemize{
#'   \item \code{data}: A list containing:
#'   \itemize{
#'     \item \code{phrases_533e145b}: A list representing a tibble with a hashed name:
#'     \itemize{
#'       \item \code{output}: A tibble with the original text and generated completions.
#'       \item \code{metadata}: A list with details for each generated column:
#'       \itemize{
#'         \item \code{user_2894a19c}: A list representing a generated column with a hashed name:
#'         \itemize{
#'           \item \code{batches}: A list of lists, each representing a batch, with elements:
#'           \itemize{
#'             \item \code{batch_number}: The batch number.
#'             \item \code{status}: The status of the batch (e.g., "In Progress", "Completed").
#'             \item \code{timestamp}: Timestamp of the batch processing.
#'             \item \code{total_time}: Total running time in the batches.
#'             \item \code{prompt}: The prompt used for the batch.
#'             \item \code{model}: The model used for generating completions.
#'             \item \code{temperature}: The temperature parameter used.
#'           }
#'           \item \code{last_batch}: The last successfully completed batch number.
#'           \item \code{total_time}: The last total time in the batches.
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' @export
#' @examples
#' library(batchLLM)
#'
#' Sys.setenv(OPENAI_API_KEY = "")
#'
#' phrases <- data.frame(
#'   user = c(
#'     "The world is a sphere, and I love it.",
#'     "The world is a sphere, and that is science.",
#'     "The world is flat, and round earth is a conspiracy."
#'   )
#' )
#'
#' batchLLM(
#'   df_name = "phrases",
#'   col_name = "user",
#'   prompt = "Classify the sentiment using one word: positive, negative, or neutral"
#' )
#'
#' print(phrases)
batchLLM <- function(df_name,
                     col_name,
                     prompt,
                     LLM = "openai",
                     model = "gpt-4o-mini",
                     temperature = .5,
                     batch_size = 10,
                     attempts = 1,
                     log_name = "batchLLM-log",
                     hash_algo = "crc32c",
                     case_convert = NULL,
                     ...) {
  save_progress <- function(df_name, col_name, df, last_batch, total_time, prompt, LLM, model, temperature, new_col_name, status, log_name) {
    log_file <- paste0(log_name, ".rds")

    batch_log <- if (file.exists(log_file)) {
      readRDS(log_file)
    } else {
      list(data = list())
    }

    param_id <- digest::digest(df[[col_name]], algo = hash_algo)
    df_key <- paste0(df_name, "_", param_id)

    if (is.null(batch_log$data[[df_key]])) {
      batch_log$data[[df_key]] <- list(
        output = df,
        metadata = list()
      )
    } else {
      existing_output <- batch_log$data[[df_key]]$output
      suppressMessages(
        suppressWarnings(
          batch_log$data[[df_key]]$output <- dplyr::left_join(existing_output, df, by = col_name) |>
            dplyr::select(-dplyr::ends_with(".x")) |>
            dplyr::rename_with(~ sub("\\.y$", "", .), dplyr::ends_with(".y"))
        )
      )
    }

    if (is.null(batch_log$data[[df_key]]$metadata[[new_col_name]])) {
      batch_log$data[[df_key]]$metadata[[new_col_name]] <- list(
        batches = list(),
        last_batch = last_batch,
        total_time = total_time
      )
    } else {
      batch_log$data[[df_key]]$metadata[[new_col_name]]$last_batch <- last_batch
      batch_log$data[[df_key]]$metadata[[new_col_name]]$total_time <- total_time
    }

    batch_log$data[[df_key]]$metadata[[new_col_name]]$batches[[as.character(last_batch)]] <- list(
      status = status,
      timestamp = Sys.time(),
      total_time = total_time,
      prompt = prompt,
      LLM = LLM,
      model = model,
      temperature = temperature
    )

    saveRDS(batch_log, log_file)
  }

  load_progress <- function(log_name) {
    log_file <- paste0(log_name, ".rds")
    if (file.exists(log_file)) {
      return(readRDS(log_file))
    } else {
      return(list(data = list()))
    }
  }

  batch_mutate <- function(df, df_col, system_prompt, batch_size, batch_num, batch_total, LLM, model, temperature, rows, log_name, case_convert, ...) {
    mutate_row <- function(df_row, content_input, system_prompt, LLM, model, temperature, log_name, case_convert, ...) {
      if (length(content_input) == 1 && !is.na(content_input)) {
        tryCatch(
          {
            content_output <- NULL

            if (grepl("openai", LLM)) {
              completion <- create_chat_completion(
                model = model,
                temperature = temperature,
                messages = list(
                  list(
                    "role" = "system",
                    "content" = system_prompt
                  ),
                  list(
                    "role" = "user",
                    "content" = as.character(content_input)
                  )
                ),
                ...
              )
              content_output <- trimws(completion$choices$message.content)
            } else if (grepl("anthropic", LLM)) {
              content_output <- claudeR(
                prompt = if (grepl("claude-2", model)) {
                  paste0(system_prompt, "(put response in XML tags <results>)", ":", as.character(content_input))
                } else if (grepl("claude-3", model)) {
                  list(list(role = "user", content = paste0(system_prompt, "(put response in XML tags <results>)", ":", as.character(content_input))))
                },
                model = model,
                temperature = temperature,
                ...
              )
              content_output <- sub(".*<results>(.*?)</results>.*", "\\1", content_output)
            } else if (grepl("google", LLM)) {
              content_output <- gemini.R::gemini_chat(
                prompt = paste0(system_prompt, "(put response in XML tags <results>)", ":", as.character(content_input)),
                model = model,
                temperature = temperature,
                ...
              )
              content_output <- sub(".*<results>(.*?)</results>.*", "\\1", content_output)
            }

            if (is.null(content_output)) {
              stop("Failed to obtain content_output. Check API responses and paths.")
            }

            if (!is.null(content_output) && length(content_output) > 0) {
              output_text <- content_output[1]
              if (!is.null(case_convert) || case_convert != "none") {
                if (case_convert == "upper") {
                  output_text <- toupper(output_text)
                } else if (case_convert == "lower") {
                  output_text <- tolower(output_text)
                }
              }
              return(output_text)
            } else {
              stop("Error: completion message content returned NULL or empty")
            }
          },
          error = function(e) {
            stop(paste("Error occurred in mutate_row:", conditionMessage(e)))
          }
        )
      } else {
        stop("Invalid input: content_input must be a single non-NA value")
      }
    }

    df <- df |> dplyr::mutate(row_number = dplyr::row_number())

    total_rows <- nrow(df)
    result <- vector("character", total_rows)

    for (i in seq_len(total_rows)) {
      if (!is.na(df_col[i])) {
        result[i] <- tryCatch(
          {
            mutate_row(df[i, , drop = FALSE], df_col[i], system_prompt, LLM, model, temperature, log_name, case_convert)
          },
          error = function(e) {
            stop(paste("Error in batch_mutate:", conditionMessage(e)))
          }
        )
        message(paste("Processed row", df$row_number[i], "of", rows))
      } else {
        message(paste("Skipping row", df$row_number[i], "(NA value)"))
      }
      Sys.sleep(runif(1, min = 0.1, max = 0.3))
    }

    if (batch_num < batch_total) {
      message("Taking a break to make the API happy 🤖❤️")
      pb <- txtProgressBar(min = 0, max = 100, style = 3)
      for (i in 1:100) {
        Sys.sleep(runif(1, min = 0.05, max = 0.15))
        setTxtProgressBar(pb, i)

        if (i %in% c(25, 50, 75)) {
          message(i, "% through the break")
        }
      }
      close(pb)
    }

    df$gpt_output <- result
    return(df)
  }

  if (!exists(df_name, envir = .GlobalEnv)) {
    stop(paste("Object", df_name, "does not exist in the global environment."))
  }

  df <- get(df_name, envir = .GlobalEnv)

  if (!is.data.frame(df) || !inherits(df, "data.frame")) {
    stop(paste(df_name, "is not a valid data frame."))
  }

  if (!col_name %in% colnames(df)) {
    stop(paste("Column", col_name, "does not exist in", df_name, "."))
  }

  param_id <- digest::digest(list(prompt, LLM, model, temperature, batch_size, attempts, log_name, case_convert), algo = hash_algo)
  new_col_name <- paste0(col_name, "_", param_id)

  param_id <- digest::digest(df[[col_name]], algo = hash_algo)
  new_df_key <- paste0(df_name, "_", param_id)

  log_file <- paste0(log_name, ".rds")
  batch_log <- load_progress(log_file)

  if (!is.null(batch_log$data) && !is.null(batch_log$data[[new_df_key]])) {
    progress <- batch_log$data[[new_df_key]]

    if (!is.null(progress$output) && new_col_name %in% colnames(progress$output)) {
      output <- progress$output
      last_batch <- progress$metadata[[new_col_name]]$last_batch
      total_time <- progress$metadata[[new_col_name]]$total_time

      if (nrow(output) == nrow(df) && !any(is.na(output[[new_col_name]]))) {
        message("All rows have already been processed for this column. No further processing needed.")
        df[[new_col_name]] <- output[[new_col_name]]
        if (!exists(new_df_key, envir = .GlobalEnv)) {
          assign(new_df_key, df, envir = .GlobalEnv)
        } else {
          message(paste("Object", new_df_key, "already exists in the global environment."))
        }
        return(invisible(df))
      }
    } else {
      output <- if (is.null(progress$output)) df else progress$output
      output[[new_col_name]] <- NA_character_
      last_batch <- 0
      total_time <- 0
    }
  } else {
    output <- df
    output[[new_col_name]] <- NA_character_
    last_batch <- 0
    total_time <- 0
  }

  start_time <- Sys.time()

  batch_total <- ceiling(nrow(df) / batch_size)

  for (batch_num in (last_batch + 1):batch_total) {
    message("Starting batch ", batch_num, " of ", batch_total)

    start_row <- (batch_num - 1) * batch_size + 1
    end_row <- min(batch_num * batch_size, nrow(df))

    rows_to_process <- which(is.na(output[start_row:end_row, new_col_name])) + start_row - 1

    if (length(rows_to_process) == 0) {
      message("Skipping batch ", batch_num, " of ", batch_total, " as all rows are already processed.")
      next
    }

    retry_flag <- TRUE
    counter <- 1
    while (retry_flag && (counter <= attempts)) {
      tryCatch(
        {
          save_progress(df_name, col_name, output, batch_num - 1, total_time, prompt, LLM, model, temperature, new_col_name, "In Progress", log_name)

          output_batch <- batch_mutate(df[rows_to_process, , drop = FALSE],
            df[[col_name]][rows_to_process],
            system_prompt = prompt,
            batch_size = length(rows_to_process),
            batch_num = batch_num,
            batch_total = batch_total,
            LLM = LLM,
            model = model,
            temperature = temperature,
            rows = nrow(df),
            log_name = log_name,
            case_convert = case_convert
          )

          output[rows_to_process, new_col_name] <- output_batch$gpt_output

          current_time <- Sys.time()
          time_elapsed_batch <- as.numeric(difftime(current_time, start_time, units = "secs"))
          total_time <- total_time + time_elapsed_batch

          save_progress(df_name, col_name, output, batch_num, total_time, prompt, LLM, model, temperature, new_col_name, "Completed", log_name)

          message("Completed batch ", batch_num, " of ", batch_total)
          retry_flag <- FALSE
        },
        error = function(e) {
          if (counter >= attempts) {
            save_progress(df_name, col_name, output, batch_num - 1, total_time, prompt, LLM, model, temperature, new_col_name, "Interrupted", log_name)
            stop(paste("Error in batch ", batch_num, " of ", batch_total, ": ", conditionMessage(e), sep = ""))
          } else {
            message(paste("Error occurred in batch ", batch_num, " of ", batch_total, ", trying again. Data processed up to row ", max(rows_to_process), ": ", conditionMessage(e), ". Attempt: ", counter, sep = ""))
            counter <- counter + 1
            Sys.sleep(runif(1, min = 1, max = 2))
          }
        }
      )
    }

    if (end_row == nrow(df)) {
      message("All ", batch_total, " batches processed")
      break
    }
    start_time <- Sys.time()
  }

  assign(df_name, output, envir = .GlobalEnv)

  invisible(output)
}

#' Get batches
#'
#' Get batches of generated output in a single data frame from the \code{.rds} log file.
#'
#' @param df_name A string to match the name of a processed \code{data.frame()} or \code{tibble()}.
#' @param log_name A string specifying the name of the log without the \code{.rds} file extension. Default is "batchLLM-log".
#' @return A data frame containing the generated output.
#' @export
get_batches <- function(df_name, log_name = "batchLLM-log") {
  log_file <- paste0(log_name, ".rds")

  if (!file.exists(log_file)) {
    stop("Log file does not exist.")
  }

  batch_log <- readRDS(log_file)

  if (!df_name %in% names(batch_log$data)) {
    stop(paste0("No data found in ", log_name, ".rds for the specified df_name."))
  }

  output <- batch_log$data[[df_name]]$output

  return(output)
}

#' Scrape metadata
#'
#' Scrape metadata from the \code{.rds} log file.
#'
#' @param df_name Optional. A string to match the name of a processed data frame.
#' @param log_name A string specifying the name of the log file without the extension. Default is "batchLLM-log".
#' @return A data frame containing metadata.
#' @export
scrape_metadata <- function(df_name = NULL, log_name = "batchLLM-log") {
  log_file <- paste0(log_name, ".rds")

  if (!file.exists(log_file)) {
    stop("Log file does not exist.")
  }

  batch_log <- readRDS(log_file)

  scrape_df <- function(df_name, df_data) {
    if (is.null(df_data$metadata) || length(df_data$metadata) == 0) {
      return(NULL)
    }

    metadata_list <- lapply(names(df_data$metadata), function(col_name) {
      column_data <- df_data$metadata[[col_name]]
      if (is.null(column_data$batches) || length(column_data$batches) == 0) {
        return(NULL)
      }

      lapply(names(column_data$batches), function(batch_num) {
        batch <- column_data$batches[[batch_num]]
        data.frame(
          df_name = df_name,
          col_name = col_name,
          batch_number = as.numeric(batch_num),
          status = batch$status,
          timestamp = batch$timestamp,
          total_time = round(batch$total_time, 2),
          prompt = batch$prompt,
          LLM = batch$LLM,
          model = batch$model,
          temperature = batch$temperature,
          stringsAsFactors = FALSE
        )
      })
    })

    do.call(rbind, unlist(metadata_list, recursive = FALSE))
  }

  if (!is.null(df_name)) {
    if (!df_name %in% names(batch_log$data)) {
      message(paste("No data found for", df_name, "in the log file."))
      return(NULL)
    }
    return(scrape_df(df_name, batch_log$data[[df_name]]))
  } else {
    metadata_list <- lapply(names(batch_log$data), function(df_name) {
      scrape_df(df_name, batch_log$data[[df_name]])
    })
    metadata_df <- do.call(rbind, metadata_list)
    return(metadata_df[!is.na(metadata_df$batch_number), ])
  }
}

#' Interact with Anthropic's Claude API
#'
#' This function was copied by yrvelez on GitHub (not currently available on CRAN).
#'
#' @param api_key Your API key for authentication.
#' @param prompt A string vector for Claude-2, or a list for Claude-3 specifying the input for the model.
#' @param model The model to use for the request. Default is the latest Claude-3 model.
#' @param max_tokens A maximum number of tokens to generate before stopping.
#' @param stop_sequences Optional. A list of strings upon which to stop generating.
#' @param temperature Optional.Amount of randomness injected into the response.
#' @param top_k Optional. Only sample from the top K options for each subsequent token.
#' @param top_p Optional. Does nucleus sampling.
#' @param system_prompt Optional. An optional system role specification.
#' @return The resulting completion up to and excluding the stop sequences.
#' @export
claudeR <- function(
    prompt, model = "claude-3-5-sonnet-20240620", max_tokens = 100,
    stop_sequences = NULL,
    temperature = .7, top_k = -1, top_p = -1,
    api_key = NULL, system_prompt = NULL) {
  if (grepl("claude-3", model) && !is.list(prompt)) {
    stop("Claude-3 requires the input in a list format, e.g., list(list(role = \"user\", content = \"What is the capital of France?\"))")
  }

  if (is.null(api_key)) {
    api_key <- Sys.getenv("ANTHROPIC_API_KEY")
    if (api_key == "") {
      stop("Please provide an API key or set it as the ANTHROPIC_API_KEY environment variable.")
    }
  }

  if (grepl("claude-2", model)) {
    url <- "https://api.anthropic.com/v1/complete"
    headers <- httr::add_headers(
      "X-API-Key" = api_key,
      "Content-Type" = "application/json",
      "anthropic-version" = "2023-06-01"
    )

    prompt <- paste0("\n\nHuman: ", prompt, "\n\nAssistant: ")

    stop_sequences <- "\n\nHuman: "

    body <- paste0('{
      "prompt": "', gsub("\n", "\\\\n", prompt), '",
      "model": "', model, '",
      "max_tokens_to_sample": ', max_tokens, ',
      "stop_sequences": ["', paste(gsub("\n", "\\\\n", stop_sequences), collapse = '", "'), '"],
      "temperature": ', temperature, ',
      "top_k": ', top_k, ',
      "top_p": ', top_p, "
    }")

    response <- httr::POST(url, headers, body = body)

    if (httr::http_status(response)$category == "Success") {
      result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      return(trimws(result$completion))
    } else {
      warning(paste("API request failed with status", httr::http_status(response)$message))
      cat(api_key)
      cat("Error details:\n", httr::content(response, "text", encoding = "UTF-8"), "\n")
      return(NULL)
    }
  }

  url <- "https://api.anthropic.com/v1/messages"

  headers <- httr::add_headers(
    "x-api-key" = api_key,
    "anthropic-version" = "2023-06-01",
    "Content-Type" = "application/json"
  )

  message_list <- lapply(prompt, function(msg) {
    list(role = msg$role, content = msg$content)
  })

  request_body_list <- list(
    model = model,
    max_tokens = max_tokens,
    temperature = temperature,
    top_k = top_k,
    top_p = top_p,
    messages = message_list
  )

  if (!is.null(system_prompt)) {
    request_body_list$system <- system_prompt
  }

  body <- jsonlite::toJSON(request_body_list, auto_unbox = TRUE)

  response <- httr::POST(url, headers, body = body)

  if (httr::http_status(response)$category == "Success") {
    result <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
    return(result$content$text)
  } else {
    warning(paste("API request failed with status", httr::http_status(response)$message))
    cat("Error details:\n", httr::content(response, "text", encoding = "UTF-8"), "\n")
    return(NULL)
  }
}
