# batchLLM <img src="inst/batchLLM_hexLogo.png" width="120" align="right"/>

[![CRAN status](https://www.r-pkg.org/badges/version/batchLLM)](https://CRAN.R-project.org/package=batchLLM)[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/batchLLM)](https://CRAN.R-project.org/package=batchLLM)[![R-CMD-check](https://github.com/dylanpieper/batchLLM/workflows/R-CMD-check/badge.svg)](https://github.com/dylanpieper/batchLLM/actions)[![GitHub stars](https://img.shields.io/github/stars/dylanpieper/batchLLM?style=social)](https://github.com/dylanpieper/batchLLM/stargazers)

Batch process large language model (LLM) text completions by looping across the rows of a data frame column. This package is designed to optimize text processing tasks by utilizing data frames and column rows as the input and a new column with the text completions as the output.

## 🚀 Features

-   Supports multiple LLMs: OpenAI's GPT, Anthropic's Claude, and Google's Gemini
-   Automatic logging of batches and metadata
-   Side-by-side comparison of outputs from different LLMs
-   User-friendly Shiny App Addin
-   Resumable batch processing
-   Flexible configuration options

## 📦 Installation

Production (CRAN):

``` r
install.packages("batchLLM")
```

Development (GitHub):

``` r
install.packages("devtools")
devtools::install_github("dylanpieper/batchLLM")
```

## 🛠️ Usage

``` r
library(batchLLM)

# Set up your API keys
Sys.setenv(OPENAI_API_KEY = "your_openai_api_key")
Sys.setenv(ANTHROPIC_API_KEY = "your_anthropic_api_key")
Sys.setenv(GEMINI_API_KEY = "your_gemini_api_key")

# Configure LLMs
llm_configs <- list(
  list(LLM = "openai", model = "gpt-4o-mini"),
  list(LLM = "anthropic", model = "claude-3-haiku-20240307"),
  list(LLM = "google", model = "1.5-flash")
)

# Process data
beliefs <- lapply(llm_configs, function(config) {
  batchLLM(
    df = beliefs,
    col = statement,
    prompt = "classify as a fact or misinformation in one word",
    LLM = config$LLM,
    model = config$model,
    max_tokens = 100,
    batch_size = 10,
    batch_delay = "1min",
    case_convert = "lower",
    sanitize = TRUE
  )
})[[length(llm_configs)]]

print(beliefs)
```

## 🤖 Supported LLMs

| LLM                                             | Models                                                                                                                        |
|----------------------|--------------------------------------------------|
| [OpenAI](https://github.com/irudnyts/openai)    | gpt-4, gpt-4o, gpt-4o-mini, gpt-4-turbo, gpt-3.5-turbo                                                                        |
| [Anthropic](https://github.com/yrvelez/claudeR) | claude-3-5-sonnet-20240620, claude-3-opus-20240229, claude-3-sonnet-20240229, claude-3-haiku-20240307, claude-2.1, claude-2.0 |
| [Google](https://github.com/jhk0530/gemini.R)   | 1.5-pro, 1.5-flash, 1.0-pro                                                                                                   |

## 🧰 Additional Tools

-   `scrape_metadata()`: Retrieve metadata from processed batches
-   `get_batches()`: Subset generated output from processed batches
-   `batchLLM_shiny()`: Shiny Addin for interactive use within RStudio IDE

## 🌟 Use Cases

-   Sentiment analysis
-   Thematic analysis
-   Classification
-   Labeling or tagging
-   Language translation
-   Refactoring variables

## ⚠️ Considerations

-   Be aware of your API rate limits
-   Check model accessibility with your API key

## 🤝 Contributing

Contributions are welcome! Here are some features ideas:

-   Function to analyze agreement between models

## 📄 License

This project is licensed under the [MIT License](LICENSE.md).

## 👨‍💻 Developer's Note

My work on a complex classification problem inspired me to create this tool. I was challenged with categorizing thousands of unique offense descriptions in court data, and later, I tested the functionality to classify drug metabolites to their drug categories in toxicology data. The original function evolved significantly, and today, it powers this Shiny app designed to streamline and scale the use of LLMs across various datasets. I hope this tool proves as valuable to you as it has in my own projects.

## 🔗 Links

-   [Demo on ShinyApps.io](https://dylan-pieper.shinyapps.io/BatchLLM/)
-   [Report an Issue](https://github.com/dylanpieper/batchLLM/issues)
