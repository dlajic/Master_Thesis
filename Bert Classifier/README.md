# BERT Classifier Description

This directory contains scripts and resources for training two distinct BERT-based classifiers developed as part of the thesis:

### Web_scraping
- **Description:** Scripts to scrape news articles for training the BERT classifier. This forms the base for the training dataset.
- **Contents:** Includes scripts for automated scraping, cleaning, and initial processing of data.

## Classifiers

### 1. Migration Topic Classifier (`mig`)
- **Purpose:** Identifies whether news articles are related to migration topics versus other topics.
- **Validation:** Uses cross-validation techniques, including a sliding window approach, to ensure robustness and generalizability.

### 2. Opinion Article Classifier (`opi`)
- **Purpose:** Distinguishes between opinionated and descriptive articles.
- **Validation:** Employs both sliding window and standard cross-validation methods to optimize performance and accuracy.

## Data

- **Training Data:** The training datasets consist of over 100,000 news articles scraped from major German news outlets. These datasets are specifically prepared and annotated to train the classifiers.
- **Location:** Training data can be found in the `../Data` directory within this repository.

## Outputs

- **Final Models:** After validation, the final models are saved and can be used for further analysis or deployment.
- **Location:** Final models are stored in the `models` folder.

## Methodology

1. **Data Collection:** Utilizes custom scripts to scrape and preprocess news articles from various online platforms.
2. **Model Training:**
   - Initial training with a subset of data to determine baseline model performance.
   - Extensive cross-validation, including variations with and without a sliding window and different batch sizes, to refine the models.
3. **Model Finalization:**
   - The best-performing models from the validation phase are selected.
   - Final models are trained on the complete dataset and saved in the `models` directory for future use.
