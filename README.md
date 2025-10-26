# B2B-auto-fleet-segmentation
Using R for RFM Clustering and Random Forest regression to optimize B2B automotive fleet sales and segment corporate clients.

This repository contains an R-based data mining project aimed at solving a strategic challenge for an automotive group's B2B fleet sales division. The project moves the company from a reactive, one-size-fits-all client management strategy to a proactive, data-driven approach.

The analysis is twofold:

**Clustering**: To segment corporate clients into actionable tiers.

**Regression**: To build a predictive model to forecast order value and identify key drivers.

# Business Context & Problem

The company's fleet sales division serves a diverse corporate client base (rental agencies, large corporations, etc.) but treats all clients similarly. This leads to missed opportunities with high-value partners and a risk of losing smaller accounts.

The goal is to use data mining to segment these B2B clients and understand the key drivers of large fleet orders, enabling a strategic, tiered service model.

# Tech Stack

**Language**: R

**Data Manipulation**: tidyverse, lubridate

**Clustering**: cluster, factoextra

**Prediction (Regression)**: randomForest, rsample, yardstick, vip

**Visualization**: ggplot2, gridExtra

# Methodology and Analysis

## 1. Exploratory Data Analysis (EDA)

After cleaning and preparing the data, initial exploration revealed key business patterns:

**Seasonal Peaks**: A strong seasonal pattern with revenue peaking in Q4 (Oct-Nov), likely due to clients spending remaining annual budgets.

**Top Market**: The USA is by far the largest single market.

**Top Product**: "Classic Cars" (likely a flagship model category) is the primary revenue driver.

## 2. Part 1: B2B Corporate Client Segmentation (Clustering)

The first objective was to segment clients based on their purchasing behavior.

**RFM Analysis**: We calculated Recency, Frequency, and Monetary (RFM) values for each of the 89 unique corporate clients.

**Optimal Clusters**: The silhouette method was used to determine the optimal number of clusters, which was clearly identified as k=3.

**K-Means Clustering**: A k-means algorithm was applied to segment the clients.

### Client Segments Defined:

The analysis revealed three distinct, actionable client tiers:

**Cluster 2**: "Strategic Partners" (2 Clients): An elite group with exceptionally high frequency and monetary value.

**Cluster 1**: "Regular Corporate Clients" (57 Clients): The core base of clients who make regular, high-value purchases.

**Cluster 3**: "Lapsed Clients" (30 Clients): A high-risk group that has not purchased in a long time (avg. 740 days) and may have switched to a competitor.

## 3. Part 2: Fleet Order Value Forecasting (Regression)

The second objective was to predict the SALES value of any given order line and identify its key drivers.

**Model**: A Random Forest regression model was trained on 80% of the data.

**Performance**: The model's performance on the 20% unseen test set was outstanding:

**R-squared**: 98.4% (The model explains 98.4% of the variance in sales).

**RMSE**: $250 (The average prediction error is only $250).

**Key Drivers**: The model's feature importance was analyzed to find the top drivers of order value.

### Top Drivers of Order Value:

QUANTITYORDERED

PRICEEACH

DEALSIZE

MSRP

This confirms that order volume and price are the dominant factors, while geography (COUNTRY) has very little importance.

# Outcomes & Business Value

This project provides two main data-driven tools that create significant business value:

A Strategic Client Segmentation: The sales division can now move from a reactive to a proactive, tiered service model. "Strategic Partners" can be assigned dedicated account managers, "Regular Clients" can be enrolled in loyalty programs, and "Lapsed Clients" can be targeted with specific re-engagement campaigns.

A Highly Accurate Predictive Model: The sales team gains a tool to rapidly and accurately build quotes for complex fleet deals. The finance department gains a reliable model for revenue forecasting and budgeting, reducing uncertainty and financial risk.

# Data Source

The analysis is based on the Auto Sales data.csv dataset taken from kaggle (https://www.kaggle.com/datasets/ddosad/auto-sales-data).

# How to Use

Clone this repository.

Place the Auto Sales data.csv file in the main directory.

Install the required R packages (listed in the "Tech Stack" section).

Run the .Rmd or .R script to perform the analysis and reproduce the results.
