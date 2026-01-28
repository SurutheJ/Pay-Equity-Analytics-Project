# Pay Equity Analytics Project

A comprehensive statistical analysis of gender-based pay disparities using Glassdoor compensation data. This project employs multiple statistical and predictive modeling techniques to investigate the existence, magnitude, and drivers of pay inequity in the workplace.

## Project Overview

This analysis examines whether gender-based pay disparities exist in the workplace and identifies the key factors that contribute to salary differences between men and women.

## Research Questions

| RQ | Question | Method |
|----|----------|--------|
| **RQ1** | Is there evidence of a gender pay gap across job titles, departments, and education levels? | T-test, Scheirer-Ray-Hare Test, ANOVA |
| **RQ2** | Which organizational and employee factors best explain variations in the gender pay gap? | Multiple Linear Regression |
| **RQ3** | Does higher education or performance evaluation mitigate the gender pay gap? | Interaction Analysis |
| **RQ4** | Are women less likely to be among the top earners? | Logistic Regression |
| **RQ5** | Does gender predict job titles? | Multinomial Logistic Regression |

## Key Findings

- **Job Title is the primary driver** of salary differences, not gender directly
- When controlling for job title, department, and other factors, **gender alone shows no significant direct effect** on salary
- **Education ROI is equal** across genders (parallel lines in interaction analysis)
- Structural/organizational factors (job role, department) have a much larger impact than demographic factors

## Project Structure

```
Pay-Equity-Analytics-Project/
├── README.md                 # Project documentation
├── .gitignore               # Git ignore rules
├── data/
│   └── glassdoor-gender-pay-gap.csv    # Source dataset
├── analysis/
│   ├── final-project.Rmd    # R Markdown analysis
│   └── final-project.html   # Rendered HTML report
├── dashboard/
│   └── app.R                # Interactive Shiny Dashboard
└── docs/
    └── GenderPay Gap.pdf    # Project report
```

## Interactive Dashboard

This project includes an interactive Shiny dashboard with:
- **Overview**: Key metrics, pay gap statistics, and findings summary
- **Salary Analysis**: Distribution plots, scatter plots by age/seniority
- **Job & Department**: Analysis across roles and departments
- **Education Impact**: ROI analysis by education level
- **Top Earners**: Analysis of top 10% earners
- **Data Explorer**: Interactive data table with filters

### Run the Dashboard

```r
# Install required packages
install.packages(c("shiny", "shinydashboard", "plotly", "DT"))

# Run the dashboard
shiny::runApp("dashboard")
```

## Dataset

- **Source**: [Kaggle - Glassdoor Analyze Gender Pay Gap](https://www.kaggle.com/datasets/nilimajauhari/glassdoor-analyze-gender-pay-gap)
- **Records**: 1000 employees
- **Variables**: JobTitle, Gender, Age, PerfEval, Education, Dept, Seniority, BasePay, Bonus

### Variables Description

| Variable | Description |
|----------|-------------|
| JobTitle | Employee's job position |
| Gender | Male or Female |
| Age | Employee's age |
| PerfEval | Performance evaluation score |
| Education | Highest education level (High School, College, Masters, PhD) |
| Dept | Department (Engineering, Sales, Operations, Management, Administration) |
| Seniority | Years of seniority |
| BasePay | Base salary |
| Bonus | Bonus amount |

## How to Run

### Prerequisites

1. Install [R](https://cran.r-project.org/) (version 4.0+)
2. Install [RStudio](https://posit.co/download/rstudio-desktop/) (recommended)

### Install Required Packages

```r
install.packages(c(
  "dplyr",
  "ggplot2",
  "relaimpo",
  "car",
  "effectsize",
  "rcompanion",
  "ARTool",
  "rstatix",
  "rsq",
  "nnet",
  "psych",
  "apaTables",
  "knitr",
  "ggcorrplot",
  "broom",
  "stringr"
))
```

### Run the Analysis

1. Clone this repository
2. Open `analysis/final-project.Rmd` in RStudio
3. Click "Knit" to generate the HTML report

## Statistical Methods Used

- **Descriptive Statistics**: Distribution analysis, correlation matrix
- **T-tests**: Comparing mean salaries between genders
- **ANOVA / Scheirer-Ray-Hare Test**: Non-parametric two-way analysis
- **Multiple Linear Regression**: Identifying salary predictors
- **Logistic Regression**: Predicting top earner status
- **Multinomial Logistic Regression**: Predicting job titles from demographics

## Visualizations

The analysis includes several key visualizations:
- Distribution of job titles by gender
- Salary distribution boxplots
- Correlation matrix heatmap
- Coefficient plots for regression models
- Odds ratio forest plots

## Author

**Suruthe Jayachandran**

## License

This project is for educational purposes. The dataset is publicly available on Kaggle.

## Acknowledgments

- Data source: Glassdoor via Kaggle
- Course: IS507 - Information Science Research Methods
