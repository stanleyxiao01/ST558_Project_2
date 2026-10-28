# ST 558 Project 2: Bank Marketing Analytics Dashboard

**Author:** Stanley Xiao  
**Course:** ST 558 - Data Science for Statisticians  
**Institution:** North Carolina State University  

## Project Overview

This project is an interactive R Shiny dashboard designed to explore the Bank Marketing dataset. The application allows users to investigate customer demographics, financial characteristics, marketing campaign interactions, and term deposit subscription outcomes.

The project also includes a Quarto exploratory data analysis (EDA) report containing descriptive statistics, visualizations, and interpretations of patterns in the data.

## Dataset

The dataset contains **45,211 observations and 17 variables** from a Portuguese banking institution's direct marketing campaigns.

The response variable, `y`, indicates whether a customer subscribed to a term deposit (`yes` or `no`).

**Data sources:**

- [Kaggle: Banking Dataset (Marketing Targets)](https://www.kaggle.com/datasets/prakharrathi25/banking-dataset-marketing-targets)
- [UCI Machine Learning Repository: Bank Marketing](https://archive.ics.uci.edu/dataset/222/bank+marketing)

The original training and testing datasets were combined, duplicate observations were removed, and the cleaned data were saved as `Dataset/bank_clean.csv`.

## Dashboard Features

### 1. About

Provides an introduction to the dataset, project objectives, and instructions for using the dashboard.

### 2. EDA Report

Displays a Quarto exploratory report within the Shiny application. Users can also open the full report in a separate browser tab.

The report includes:
- Dataset dimensions, variable types, and missing-value summaries
- Categorical frequency tables and cross-tabulations
- Numerical descriptive statistics
- Six visualizations exploring customer characteristics and subscription outcomes
- Discussion and interpretation of the findings

### 3. Data Download

Allows users to:
- Explore the dataset through an interactive table
- Search and sort observations
- Download the full dataset or the currently filtered dataset as a CSV file

### 4. Data Exploration

Allows users to:
- Generate one-way and two-way categorical frequency tables
- Calculate numerical summaries with optional grouping
- Create histograms, boxplots, bar charts, and scatterplots
- Customize plots using grouping and faceting options

### Interactive Filtering

The sidebar provides:
- Job and education filters
- Two selectable numerical variables with adjustable range sliders
- An **Apply Filters** button to update the dataset
- A display of the current number of observations

## Programs Used

- **R** - Data processing and statistical analysis
- **Shiny** - Interactive web application
- **ggplot2** - Data visualization
- **DT** - Interactive data tables
- **shinycssloaders** - Loading indicators
- **dplyr** - Data manipulation
- **Quarto** - Exploratory data analysis report

## Running the Application Locally

1. Clone or download this Github repository.
2. Open the RStudio project.
3. Install the required R packages if they are not already installed: "shiny", "ggplot2", "DT", "shinycssloaders", "dplyr"
4. Run the application from the project root: shiny::runApp(".")

## Exploratory Data Analysis Report

The source file for the report is `eda.qmd`. The report is rendered to HTML using Quarto and is included in the Shiny dashboard through the `EDA Report` tab.

## Live Application

[Launch the Bank Marketing Analytics Dashboard](https://stanleyxiao01.shinyapps.io/ST558_Project_2/)

