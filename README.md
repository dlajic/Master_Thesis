# Master Thesis: Exploring the Influence of Migration News Consumption on Respective Attitudes and Political Perspectives through Deep Learning and Digital Trace Data Analysis 

## Overview

This project investigates the influence of online news consumption, distinguishing between fake and conventional news, on attitudes towards migrants and certain political parties. A significant component of the study involved developing a BERT-based classifier to categorize news articles by themes related to migration and to identify opinion articles. This classifier was fine-tuned using a large dataset of over 100,000 news articles, which I scraped from major German news outlets.

## Key Components

### BERT Classifier
- **Description:** Central to this study was the development of a BERT-based classifier. This classifier was trained to distinguish between articles related to migration and others, and to identify whether an article is opinionated.
- **Training Data:** The classifier was trained using a dataset created from scraped news articles, tagged with themes as they appeared on various news platforms. This dataset is included in the repository for further exploration and use.

### Data Sources and Usage
**Web Tracking Data and Survey Data:**
- Due to privacy restrictions, the detailed tracking and survey data analyzed in this study are not published. However, the methodology and analysis scripts are shared.
- **Web Tracking Data:** Includes complete internet usage data over six months.
- **Survey Data:** Comprises responses from three surveys.

## Repository Structure

### Analysis_final
- **Description:** Contains the final analytical scripts that utilize outputs from the BERT classifier to correlate news consumption patterns with shifts in public opinion.

### Web_scraping
- **Description:** Scripts to scrape news articles for training the BERT classifier. This forms the base for the training dataset.
- **Contents:** Includes scripts for automated scraping, cleaning, and initial processing of data.

### BERT Topic Models and Word Clouds
- **Description:** Utilizes BERT models to generate thematic subgroups and visual word clouds that illustrate the prevalence of certain themes in fake versus conventional news.
