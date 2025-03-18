# Analysis_final Directory

## Data Privacy Notice
Please note that due to strict data privacy regulations, the detailed tracking and survey data used in these analyses are not shared in this repository. The scripts provided here are for methodological transparency and can be adapted for use with other datasets that do not violate confidentiality agreements.

This directory contains the scripts used for the final analysis phase of the project. Each script should be run sequentially as they build upon the results of the previous script.

## Scripts Overview

### 1. 0_1_webtracking
- **Purpose:** Constructs the `df_clean` dataframe, which includes only new_ids from individuals who have read at least one classified article within the analysis time frame. This script also generates initial tables and figures.
- **Execution:** Run this script first to prepare the dataset for further analysis.

### 2. 0_2_prep_data
- **Purpose:** Utilizes the `df_clean` to filter data relevant for the study and constructs aggregate measures such as `cum.dur` (cumulative duration) and `cum.n` (cumulative count), which are used in regression analyses.
- **Output:** Saves the dataset `track_survey_all3_waves_new_5_june_only_classified` with aggregate measures.
- **Execution:** Run this script second to build upon the cleaned data from the first script.

### 3. 0_3_survey_data_prep
- **Purpose:** Reads the `track_survey_all3_waves_new_5_june_only_classified` dataset to incorporate aggregate measures and matches them with the survey data.
- **Output:** Produces `df_analysis.xlsx` which contains all cleaned variables for regression analyses. This dataset includes only participants who were involved in at least two survey waves and have read classified articles.
- **Execution:** Run this script third to prepare your survey data for the final analysis.

### 4. 0_4_survey_analysis
- **Purpose:** Uses `df_analysis.xlsx` and `df_clean` to determine the final sample of individuals who have read at least one classified article during the analysis period and participated in at least two survey waves.
- **Execution:** This script performs the regression analyses and generates the final tables and figures.
- **Note:** This script should be executed last as it depends on all previous scripts for its inputs.

