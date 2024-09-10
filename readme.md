# batchLLM <img src="inst/batchLLM_hexLogo.png" width="120" align="right"/>

[![GitHub stars](https://img.shields.io/github/stars/dylanpieper/batchLLM?style=social)](https://github.com/dylanpieper/batchLLM/stargazers)
[![R-CMD-check](https://github.com/dylanpieper/batchLLM/workflows/R-CMD-check/badge.svg)](https://github.com/dylanpieper/batchLLM/actions)

Batch process Large Language Model (LLM) text completions by looping across the rows of a data frame column.

## 🚀 Features

- Supports multiple LLMs: OpenAI's GPT, Anthropic's Claude, and Google's Gemini
- Automatic logging of batches and metadata
- Side-by-side comparison of outputs from different LLMs
- User-friendly Shiny App Addin
- Resumable batch processing
- Flexible configuration options

## 📦 Installation

```r
install.packages("devtools")
devtools::install_github("dylanpieper/batchLLM")
```

## 🛠️ Usage

```r
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
    prompt = "Classify the sentiment using one word: positive, negative, or neutral",
    LLM = config$LLM,
    model = config$model,
    batch_delay = "1min",
    case_convert = "lower"
  )
})[[length(llm_configs)]]

print(beliefs)
```

| LLM | Models |
|-----|--------|
| [OpenAI](https://github.com/irudnyts/openai) | gpt-4, gpt-4o, gpt-4o-mini, gpt-4-turbo, gpt-3.5-turbo |
| [Anthropic](https://github.com/yrvelez/claudeR) | claude-3-5-sonnet-20240620, claude-3-opus-20240229, claude-3-sonnet-20240229, claude-3-haiku-20240307, claude-2.1, claude-2.0 |
| [Google](https://github.com/jhk0530/gemini.R) | 1.5-pro, 1.5-flash, 1.0-pro |

## 🧰 Additional Tools

- `scrape_metadata()`: Retrieve metadata from processed batches
- `get_batches()`: Subset generated output from processed batches
- `batchLLM_shiny()`: Shiny Addin for interactive use within RStudio IDE

## 🌟 Use Cases

- Sentiment analysis
- Thematic analysis
- Classification
- Labeling or tagging
- Language translation

## ⚠️ Considerations

- Be aware of your API rate limits
- Check model accessibility with your API key

## 🤝 Contributing

Contributions are welcome! Here are some upcoming features:

- Default for `get_batches()` to retrieve the latest batch
- Add max tokens parameter for different LLMs
- Function to analyze agreement between models

## 📄 License

This project is licensed under the [MIT License](LICENSE).

## 👨‍💻 Developer's Note

The inspiration for creating this tool came from my work on a complex classification problem involving court data. I faced the challenge of processing thousands of unique offense descriptions, and later, I tested the functionality to classify drug metabolites in toxicology data. The original function evolved significantly, and today, it powers this Shiny app designed to streamline and scale the use of LLMs across various datasets. I hope this tool proves as valuable to you as it has in my own projects.

## 🔗 Links

- [Demo on ShinyApps.io](https://dylan-pieper.shinyapps.io/BatchLLM/)
- [Report an Issue](https://github.com/dylanpieper/batchLLM/issues)
