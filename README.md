> **Note:**  
> This repository contains code and documentation from my Master's thesis, submitted in December 2023. It is shared here to illustrate my early work with large language models (LLMs) and digital trace data.  
> The project was **never intended to be a reusable or production-ready codebase**, as it was developed as part of a university research setting and aligned with the working style of the supervising lab. Much of the analysis was done in Jupyter Notebooks, and parts rely on non-public data.  
> For more recent and structured work, feel free to explore the other projects on my GitHub profile.

# Master Thesis: Exploring the Influence of Migration News Consumption on Respective Attitudes and Political Perspectives through Deep Learning and Digital Trace Data Analysis 

## Overview

This project investigates the influence of online news consumption, distinguishing between fake and conventional news, on attitudes towards migrants and certain political parties. A significant component of the study involved developing a BERT-based classifier to categorize news articles by themes related to migration and to identify opinion articles. This classifier was fine-tuned using a large dataset of over 100,000 news articles, which I scraped from major German news outlets.

## Key Components

### BERT Classifier
- **Description:** Central to this study was the development of a BERT-based classifier. This classifier was trained to distinguish between articles related to migration and others, and to identify whether an article is opinionated.
- **Training Data:** The classifier was trained using a dataset created from scraped news articles, tagged with themes as they appeared on various news platforms. This dataset is included in the repository for further exploration and use.

### Model Performance (Cross-Validation)

Both BERT-based classifiers demonstrated strong performance in five-fold cross-validation:

- **Migration Classifier:** Accuracy 97%, F1 0.98, AUROC 0.995  
- **Opinion Classifier:** Accuracy 94%, F1 0.98, AUROC 0.98

Full evaluation metrics are available in the thesis PDF.

### Data Sources and Usage
**Web Tracking Data and Survey Data:**
- Due to privacy restrictions, the detailed tracking and survey data analyzed in this study are not published. However, the methodology and analysis scripts are shared.
- **Web Tracking Data:** Includes complete internet usage data over six months.
- **Survey Data:** Comprises responses from three surveys.

## Repository Structure

### Analysis_final
- **Description:** Contains the final analytical scripts that utilize outputs from the BERT classifier to correlate news consumption patterns with shifts in public opinion.

### BERT Topic Models and Word Clouds
- **Description:** Utilizes BERT models to generate thematic subgroups. Using NER to visualize word clouds that illustrate the prevalence of certain themes in fake versus conventional news.
