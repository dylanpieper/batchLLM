# batchLLM 0.1.0

This is the initial release of the batchLLM package, offering a solution for batch processing text completions using large language models (LLMs) including OpenAI's GPT, Anthropic's Claude, and Google's Gemini. This package is designed to optimize text processing tasks by utilizing data frames and column rows as the input and a new column with the text completions as the output.

## New Functions

* `batchLLM()`: Main function to process text completions in batches. It accepts a data frame and processes each row based on the specified LLM configurations.

* `batchLLM_shiny()`: This function provides a user interface using Shiny to interact with the `batchLLM` package. It allows users to configure and execute batch processing through an interactive dashboard.

* `get_batches()`: Retrieves batches of generated output from the `.rds` log file, allowing for easy access to processed data.

* `scrape_metadata()`: Extracts metadata from the `.rds` log file, providing insights into the batch processing details such as timestamps, status, and configuration parameters.

* `claudeR()`: A function adapted from the [claudeR](https://github.com/yrvelez/claudeR) GitHub repository, allowing for interaction with Anthropic's Claude API for text generation.