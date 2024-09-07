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

## Usage

``` r
library(batchLLM)

Sys.setenv(OPENAI_API_KEY = "your_openai_api_key")
Sys.setenv(ANTHROPIC_API_KEY = "your_anthropic_api_key")
Sys.setenv(GEMINI_API_KEY = "your_gemini_api_key")

llm_configs <- list(
  list(LLM = "openai", model = "gpt-4o-mini"),
  list(LLM = "anthropic", model = "claude-3-haiku-20240307"),
  list(LLM = "google", model = "1.5-flash")
)

beliefs <- lapply(llm_configs, function(config) {
  batchLLM(
    df = beliefs,
    col = statement,
    prompt = "Classify the sentiment using one word: positive, negative, or neutral",
    LLM = config$LLM,
    model = config$model,
    batch_delay = "1min",
    case_convert = "lower"
  )
})[[length(llm_configs)]]

print(beliefs)
```

| Statement                                                                       | statement_b8eaf401 | statement_ff26ea34 | statement_23eed5c3 |
|---------------------------------|-------------|-------------|-------------|
| The Earth is a sphere, as evidenced by observations from space.                 | neutral            | neutral            | neutral            |
| The Earth is flat, and observable evidence of the Earth’s curvature is lacking. | negative           | negative           | negative           |
| Vaccines are safe and effective.                                                | positive           | positive           | positive           |
| Vaccines cause more harm than good.                                             | negative           | negative           | negative           |
| Climate change is real and caused by human activity.                            | neutral            | negative           | neutral            |
| Climate change is a hoax.                                                       | negative           | negative           | negative           |
| The moon landing was real and a great achievement.                              | positive           | positive           | positive           |
| The moon landing was faked.                                                     | negative           | negative           | negative           |
| 5G technology is safe and improves communication.                               | positive           | positive           | positive           |
| 5G technology spreads COVID-19.                                                 | negative           | negative           | negative           |
| Evolution is a well-supported scientific theory.                                | positive           | positive           | neutral            |
| Evolution is just a theory and not proven.                                      | negative           | negative           | negative           |
| Chemtrails are just contrails from airplanes.                                   | neutral            | neutral            | neutral            |
| Chemtrails are chemicals sprayed by the government.                             | neutral            | negative           | negative           |
| The earth's climate has always changed naturally.                               | neutral            | neutral            | neutral            |
| Human activity is accelerating climate change.                                  | negative           | negative           | negative           |
| The government is hiding evidence of extraterrestrial life.                     | negative           | negative           | negative           |
| The government is transparent about extraterrestrial research.                  | positive           | neutral            | neutral            |
| Fluoride in water is safe and prevents tooth decay.                             | positive           | positive           | positive           |
| Fluoride in water is harmful and causes health problems.                        | negative           | negative           | negative           |

## **Features**

-   **Batching**: The prompt is applied to a specified number of rows for each batch, with brief delays between individual rows/requests and longer delays between batches. Both the delay duration and batch size (i.e., number of rows or requests per batch) can be set using the `batch_delay` and `batch_size` parameters. These settings can be adjusted to comply with API rate limits (e.g., requests per minute).

-   **Resume Progress**: Automatically resumes from the last completed batch if the process is stopped or interrupted, ensuring continuity and saving resources.

-   **Hashing**: Generates unique hashes for column names and data frames, preventing mutation conflicts. Uses the `digest` function and `crc32c` algorithm by default.

-   **Metadata Storage**: Saves metadata in a local log file with a nested list structure (`batchLLM-log.rds`), ensuring easy retrieval and documentation.

-   **Flexible Parameters**: Allows adjustment of parameters for data selection, LLM model configuration, batch size, retry attempts, log file name, case conversion, and hashing algorithm.

-   **Compatibility**: Supports any data frame or object that inherits from `data.frame`, ensuring broad applicability across different data types.

### Scraping Metadata

After processing multiple batches, you can use the `scrape_metadata()` function to return the metadata from `batchLLM-log.rds` into a single data frame. You may specify the name of a data frame to return only the metadata from that batch (e.g., `scrape_metadata("beliefs_40a3012b")`).

### `Gett`ing Batches

After processing multiple batches, you can use the `get_batches()` function to subset all of the generated output from `batchLLM-log.rds` into a single data frame (e.g., `get_batches("beliefs_40a3012b")`).

### Shiny Addin

You can use the `batchLLM_shiny()` Shiny Addin to quickly run batchLLM from the RStudio IDE. It includes interactive inputs, tables, and animated console messages with verbose feedback.

You can also test drive the Shiny App on [ShinyApps.io](https://dylan-pieper.shinyapps.io/BatchLLM/).

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
-   Write a function to analyze the agreement between the models
