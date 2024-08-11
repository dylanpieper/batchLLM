#' Batch GPT
#'
#' Use OpenAI's GPT model to conduct natural language tasks using rows from a column as the input.
#' Completions are batched to save your progress in case of an error or interruption.
#' For API etiquette, there is a delay between each completion and each batch.
#'
#' @param input A vector from a data frame defined using the $ operator. This function does not support piping.
#' @param prompt A system prompt for the GPT model.
#' @param batch_size The number of rows to process in each batch.
#' @param attempts The maximum number of attempts in case of errors.
#' @param model An OpenAI model.
#' @param temperature A temperature for the GPT model.
#' @return Returns the original data frame and adds a column with the GPT output (gpt_output). Writes progress to \code{gpt_output.RDS}.
#' @export
#' @examples
#' library(batchGPT)
#'
#' Sys.setenv(OPENAI_API_KEY = "...")
#'
#' phrases <- dplyr::tibble(user = c("love the world", "screw the world", "the world is a sphere"))
#'
#' batchGPT(
#'   input = phrases$user,
#'   prompt = "classify the sentiment using one word: positive, negative, or neutral"
#' )
#'
#' print(phrases)
batchGPT <- function(input, prompt, batch_size = 10, attempts = 1,
                     model = "gpt-3.5-turbo-0125",
                     temperature = .5) {

  # Validate the input format and extract data frame and column names
  df_name <- deparse(substitute(input))
  if (!grepl("\\$", df_name)) {
    stop("Input must include a $ operator.")
  }
  
  # Split input into data frame and column names
  df_parts <- strsplit(df_name, "\\$")[[1]]
  if (length(df_parts) != 2) {
    stop("Invalid input format. Ensure the input is in the format 'data_frame$column'.")
  }
  
  # Extract data frame and column names
  df_name <- df_parts[1]
  col_name <- df_parts[2]
  
  # Check if the data frame exists in the environment
  if (!exists(df_name, envir = .GlobalEnv)) {
    stop(paste("Data frame", df_name, "not found in the environment."))
  }
  
  # Get the data frame
  df <- get(df_name, envir = .GlobalEnv)
  
  # Validate that it's a data frame
  if (!is.data.frame(df)) {
    stop(paste(df_name, "is not a data frame."))
  }
  
  # Check if the column exists in the data frame
  if (!col_name %in% names(df)) {
    stop(paste("Column", col_name, "not found in", df_name))
  }
  
  # Check if the column is an atomic vector
  if (!is.atomic(df[[col_name]])) {
    stop(paste("Column", col_name, "in", df_name, "is not an atomic vector."))
  }
  
  # Calculate batch total if needed
  batch_total <- ceiling(nrow(df) / batch_size)

  progress <- load_saved_progress()

  if (!is.null(progress)) {
    output <- progress$output
    last_completed_batch <- progress$last_completed_batch
  } else {
    output <- NULL
    last_completed_batch <- 0
  }

  if (last_completed_batch == batch_total) {
    stop("All rows have been processed. Move or delete 'gpt_output.RDS' to start a new batch.")
  } else {
    for (batch_num in (last_completed_batch + 1):batch_total) {
      cat("Starting batch ", batch_num, "\n")
      start_row <- (batch_num - 1) * batch_size + 1
      end_row <- min(((batch_num) * batch_size), nrow(df))

      retry_flag <- TRUE
      counter <- 1
      while (retry_flag && (counter <= attempts)) {
        tryCatch(
          {
            output_batch <- batch_mutate(df[start_row:end_row, ],
              df_col = input[start_row:end_row],
              system_prompt = prompt,
              batch_size = batch_size,
              batch_num = batch_num,
              batch_total = batch_total,
              model = model,
              temperature = temperature,
              rows = nrow(df)
            )
            temp_output <- if (exists("output")) rbind(output, output_batch) else output_batch
            output <- temp_output[!duplicated(temp_output[[vector_name]]), ]

            save_progress(output, batch_num)
            cat("Completed batch", batch_num, "with", nrow(output), "total rows processed", "\n\n")
            retry_flag <- FALSE
          },
          error = function(e) {
            if (counter >= attempts) {
              print("Maximum attempts limit reached.")
              stop(paste(
                "Error occurred, trying again. Data processed up to row", end_row, ": ",
                conditionMessage(e), ". Attempt: ", counter, "."
              ))
            } else {
              print(paste(
                "Error occurred, trying again. Data processed up to row", end_row, ": ",
                conditionMessage(e), ". Attempt: ", counter, "."
              ))
              counter <- counter + 1
              Sys.sleep(runif(1, min = 1, max = 2))
            }
          }
        )
      }
      if (nrow(output) == nrow(df)) {
        cat("All rows processed\n")
        assign(as.character(parse(text = strsplit(df_name, "\\$")[[1]][1])),
               as.data.frame(output),
               envir = .GlobalEnv)
        break
      }
    }
  }
}

mutate_row <- function(df, df_col, system_prompt, model, temperature) {
  content_input <- as.character(df_col)

  if (length(content_input) > 0) {
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
          "content" = content_input
        )
      )
    )

    tryCatch(
      {
        content_output <- completion$choices$message.content

        if (!is.null(content_output)) {
          df$gpt_output <- rep(content_output, length.out = 1)
        } else {
          message("Error: completion message content returned NULL")
          df$gpt_output <- NA
        }
      },
      error = function(e) {
        message("Error occurred: ", conditionMessage(e))
        df$gpt_output <- NA
      }
    )
  } else {
    df$gpt_output <- NA
  }

  return(df)
}

save_progress <- function(output, last_completed_batch) {
  saveRDS(list(output = output, last_completed_batch = last_completed_batch), file = "gpt_output.RDS")
}

load_saved_progress <- function() {
  if (file.exists("gpt_output.RDS")) {
    return(readRDS(file = "gpt_output.RDS"))
  } else {
    return(NULL)
  }
}

batch_mutate <- function(df, df_col, system_prompt, batch_size, batch_num, batch_total, model, temperature, rows) {
  out <- vector("list", 0)
  rows_processed <- 0
  total_rows <- nrow(df)

  while (rows_processed < total_rows) {
    start_row <- rows_processed + 1
    end_row <- min(rows_processed + batch_size, total_rows)
    batch_out <- vector("list", end_row - start_row + 1)

    for (i in start_row:end_row) {
      batch_out[[i - start_row + 1]] <- mutate_row(df[i, ], df_col[i], system_prompt,
                                                   model = model,
                                                   temperature = temperature
      )
      cat("Processed row", i, "\n")
      Sys.sleep(runif(1, min = 0.1, max = 0.3))
    }

    rows_processed <- end_row
    out <- c(out, batch_out)

    if (batch_num < batch_total) {
      message("Taking a quick break to make the API happy...")
      pb <- txtProgressBar(min = 0, max = 100, style = 3)
      for(i in 1:100) {
        Sys.sleep(runif(1, min = 0.05, max = 0.15))
        setTxtProgressBar(pb, i)
      }
      close(pb)
    }
  }

  out <- do.call(rbind, out)
  return(out)
}
