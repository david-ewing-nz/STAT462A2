# STAT462 Assignment 2 – Predicting Life Expectancy
# STAT462 ASSIGNMENT 2

## 📄 Overview

This project forms **Assignment 2** for **STAT462**, focusing on predictive modelling using **R**. The aim is to develop and compare statistical models that predict **life expectancy** across 97 countries using socio-economic and health-related indicators.

## 👥 Authors

-   David Ewing (82171165)\
-   Xia Yu (62380486)

## 🎯 Objectives

-   Build **at least three different models** to predict life expectancy.
-   Include **at least one Generalised Linear Model (GLM)**.
-   Apply and compare a variety of modelling techniques:
    -   Linear regression
    -   Polynomial terms
    -   Variable transformations
    -   Step-wise selection
    -   Ridge/Lasso regression
    -   Interaction terms

## 📊 Dataset

-   **File**: `life.csv`
-   **Observations**: 97 countries
-   **Response Variable**: `Life.expectancy`
-   **Predictors**:
    -   `Income`: Gross national income per capita
    -   `Literacy`: Adult literacy rate
    -   `Under5.mortality`: Child deaths per 1,000 under age 5
    -   `HIV`: HIV prevalence (%)
    -   `T.B.`: Tuberculosis incidence per 100,000
    -   `Health.exp`: Health expenditure as % of GDP

## 📁 Report Structure

The final PDF report includes:

1.  **Introduction** – problem overview and modelling goals\
2.  **Exploratory Data Analysis** – visualisations, summaries, and data preparation\
3.  **Modelling** – description, fitting, diagnostics, and comparison of at least three models\
4.  **Model Selection** – justification for the final model choice\
5.  **Conclusion** – summary of findings and implications

All R code is embedded inline, with clear labels and comments using **British English conventions**.

## 🔁 Reproducibility

-   A **random seed** of either `82171165` or `62380486` is used consistently to ensure reproducibility.
-   All code is written in **R**, using only permitted libraries such as `glm`, `dplyr`, and `ggplot2`.

## 📅 Submission Details

-   **Due Date**: Friday 18th April 2025, 3:00 pm\
-   **Format**: A4 PDF report, max 4 pages (including code and plots)\
-   **Font**: Minimum 11 pt with 2.5 cm margins

------------------------------------------------------------------------
