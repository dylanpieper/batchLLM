# batchLLM <img src="inst/batchLLM_hexLogo.png" width="120" align="right"/>

Batch process Large Language Model (LLM) text completions by looping across the rows of a data frame column. This tool is an efficient solution for handling large datasets with the flexibility to configure multiple models and automate the storage of output and metadata.

The package currently supports **OpenAI's GPT**, **Anthropic's Claude**, and **Google's Gemini** models, with built-in delays for API rate limiting. The package provides advanced text processing features, including automatic logging of batches and metadata to local files, side-by-side comparison of outputs from different LLMs, and integration of a user-friendly Shiny App Addin.

Use cases include natural language processing tasks such as sentiment analysis, thematic analysis, classification, labeling or tagging, and language translation.

## Developer's Note

The initial inspiration for creating this tool came from my work on a complex classification problem involving court data. I faced the challenge of processing thousands of unique offense descriptions, and later, I tested the functionality to classify drug metabolites in toxicology data. The original function evolved significantly, and today, it powers this Shiny app designed to streamline and scale the use of LLMs across various datasets. I hope this tool proves as valuable to you as it has in my own projects.

## Supported LLMs

| LLM       | Models                                                                                                                        | Credit                                                                                  |
|------------------------|------------------------|------------------------|
| OpenAI    | gpt-4, gpt-4o, gpt-4o-mini, gpt-4-turbo, gpt-3.5-turbo                                                                        | <https://github.com/irudnyts/openai>                                                    |
| Anthropic | claude-3-5-sonnet-20240620, claude-3-opus-20240229, claude-3-sonnet-20240229, claude-3-haiku-20240307, claude-2.1, claude-2.0 | <https://github.com/yrvelez/claudeR> (copied to this package because it is not on CRAN) |
| Google    | 1.5-pro, 1.5-flash, 1.0-pro                                                                                                   | <https://github.com/jhk0530/gemini.R>                                                   |

## Installation

You can install **batchLLM** from GitHub using the `devtools` package:

``` r
install.packages("devtools")

devtools::install_github("dylanpieper/batchLLM")
```

## Basic Usage

``` r
# Set API keys
Sys.setenv(OPENAI_API_KEY = "your_openai_api_key")
Sys.setenv(ANTHROPIC_API_KEY = "your_anthropic_api_key")
Sys.setenv(GEMINI_API_KEY = "your_gemini_api_key")

# Create example data frame
beliefs <- data.frame(user = c(
  "The world is a sphere, and I love it.",
  "The world is a sphere, and that is science.",
  "The world is flat, and round earth is a conspiracy."
))

# Define LLM configurations
llm_configs <- list(
  list(LLM = "openai", model = "gpt-4o-mini"),
  list(LLM = "anthropic", model = "claude-3-haiku-20240307"),
  list(LLM = "google", model = "1.5-flash")
)

# Apply batchLLM function to each configuration
phrases <- lapply(llm_configs, function(config) {
  batchLLM(
    df = beliefs,
    col = user,
    prompt = "Classify the sentiment using one word: positive, negative, or neutral",
    LLM = config$LLM,
    model = config$model,
    case_convert = "lower"
  )
})[[length(llm_configs)]]

# Print the updated data frame
print(beliefs)
```

| user                                                | user_7dd87525 | user_fe2715be | user_e4cb64ba |
|-------------------|------------------|------------------|------------------|
| The world is a sphere, and I love it.               | positive      | positive      | positive      |
| The world is a sphere, and that is science.         | neutral       | neutral       | neutral       |
| The world is flat, and round earth is a conspiracy. | negative      | negative      | negative      |

## **Features**

-   **Batching**: The prompt is applied to a specified number of rows for each batch, with brief delays between individual rows/requests and longer delays between batches. Both the delay duration and batch size (i.e., number of rows or requests per batch) are customizable using the `batch_delay` and `batch_size` parameters. These settings can be adjusted to comply with API rate limits (e.g., requests per minute).

-   **Resume Progress**: Automatically resumes from the last completed batch if the process is stopped or interrupted, ensuring continuity and saving resources.

-   **Hashing**: Generates unique hashes for column names and data frames, preventing mutation conflicts. Uses the `digest` function and `crc32c` algorithm by default.

-   **Metadata Storage**: Saves metadata in a local log file with a nested list structure (`batchLLM-log.rds`), ensuring easy retrieval and documentation.

-   **Flexible Parameters**: Allows adjustment of parameters for data selection, LLM model configuration, batch size, retry attempts, log file name, case conversion, and hashing algorithm.

-   **Compatibility**: Supports any data frame or object that inherits from `data.frame`, ensuring broad applicability across different data types.

### Scraping Metadata

After processing multiple batches, you can use the `scrape_metadata()` function to return the metadata from `batchLLM-log.rds` into a single data frame. You may specify the name of a process column to return only the metadata from that batch (e.g., `scrape_metadata("user_fe2715be")`).

### Getting Batches

After processing multiple batches, you can use the `get_batches()` function to subset all of the generated output from `batchLLM-log.rds` into a single data frame (e.g., `get_batches("phrases_ddd071e0")`). Use the `scrape_metadata()` function to help you identify the hashed name of your data frame.

### Shiny Addin

You can use the `batchLLM_shiny()` Shiny Addin to quickly run batchLLM from the RStudio IDE. It includes interactive inputs, tables, and animated console messages with verbose feedback.

## Considerations

-   Be aware of your rate limits (e.g., requests per minute). Each row processed is one request. For this reason, premium plans are ideal for batch processing.
-   Be aware of the models you can access via your API key. Some models may have restricted access and throw an error.

## Contributing

Contribute to **batchLLM**. Add a new LLM, or expand the Shiny Addin by submitting a pull request. Here are some features coming soon:

-   Add default for `get_batches()` to get the latest batch if a data frame is not specified.
-   Add max tokens parameter (variation in default values):
    -   **openai**: max_tokens
    -   **claudeR**: max_tokens
    -   **gemini.R**: maxOutputTokens
