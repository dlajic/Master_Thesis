> **Note:**  
> This repository contains code and documentation from my Master's thesis, submitted in December 2023. It is shared here to illustrate my early work with large language models (LLMs) and digital trace data.  
> The **code was not designed as a reusable or production-ready software project**, but rather reflects an academic workflow, as it was developed as part of a university research setting and aligned with the working style of the supervising lab. Much of the analysis was done in Jupyter Notebooks, and parts rely on non-public data.  
> For more recent and structured work, feel free to explore the other projects on my GitHub profile.

# Master's Thesis: Deep Learning on News Consumption and Political Attitudes

This repository documents the codebase and methodology of my Master’s thesis (submitted December 2023), which investigates how the consumption of online news on migration—particularly from fake vs. traditional media—affects political preferences and attitudes toward migrants.

## Overview

The project integrates web tracking data (6 months), survey data (3 waves), and advanced NLP-based content analysis using BERT-based models.

The key goal: measure whether exposure to certain types of migration-related news influences AfD preference and anti-migrant sentiment over time.

## Key NLP Components

### Two BERT-based classifiers
- **Migration Topic Classifier**: F1 0.97  
- **Opinion vs. Descriptive Classifier**: F1 0.94  
→ Built via transfer learning on a custom-labeled dataset of over 200,000 scraped German news articles.

### Data Pipeline
- Scraped from Germany’s largest news portals.
- Automatically labeled using editorial metadata (e.g., "Migration", "Opinion").
- Evaluated using 5-fold cross-validation.

## Behavioral & Survey Data

- Web tracking data: Full browsing history over six months.
- Survey data: Three-wave panel with attitudinal and political variables.
- Combined to construct time-varying exposure indicators.

Due to privacy restrictions, the detailed tracking and survey data analyzed in this study are not published. However, the methodology and analysis scripts are shared.

## Analytical Model

A Within-Between random effects panel model was used to link media exposure patterns to changes in:
- Attitudes toward migrants  
- Party preferences (e.g., AfD)  

Control variables and pre-treatment baselines included.

## Additional Analyses

- BERT-based topic modeling to identify subtopics within migration-related articles.
- Named Entity Recognition and word cloud analysis to compare fake vs. conventional media coverage.

## Repository Structure (Partial)

- `bert_models/`: Training and evaluation scripts for both classifiers  
- `data_prep/`: Scraping, cleaning, preprocessing  
- `analysis_final/`: Final notebook integrating ML outputs with panel data  
- `results/`: Cross-validation logs, plots, model outputs  
- `thesis.pdf`: Full thesis document (non-public data redacted)

For a more recent and structured ML project, see the [Energy Forecasting Project](https://github.com/dlajic/energy-forecasting-transformer-lightgbm).
