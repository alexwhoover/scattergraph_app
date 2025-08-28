# Interactive Scatter Plot Generator

This RShiny application allows users to upload flow monitoring data (CSV format), process and visualize velocity–depth relationships, and generate summary reports and scattergraph data for hydraulic analysis.

It is designed for analyzing sewer flow monitoring data, including calculating Manning’s coefficients using multiple methods and plotting iso-curves and Froude lines for hydraulic interpretation.

## Features
- Upload CSV files
- Visualize Scattergraph
- Visualize Weekly Scattergraphs
- Export Summary Reports

## Requirements
The app requires the following R packages:
- `shiny`
- `shinyjs`
- `plotly`
- `dplyr`
- `testthat`

Install them with:

```R
install.packages(c("shiny", "shinyjs", "plotly", "dplyr", "testthat"))
```

## How to Use
1. Upload Data
    - Click Upload CSV File and select your dataset.
    - Specify the number of rows to skip if your CSV has header rows.
    - Select the correct timestamp, depth, velocity, and flow columns from dropdowns.
    - Click Confirm Data.
2. Adjust Parameters
3. Plot
    - Click Render Plot to calculate and plot.
    - View plots in "Plot" or "Weekly Plot" tabs.
4. Export Report
    - Generate Summary Excel exports a summary excel sheet to be used for COV's Site Analysis Summaries.
    - Generate Scattergraph Excel fills out the old COV scattergraph template, if this format is required for archiving.

## Running the App
1. Clone this repository into a working directory.
2. Open project file in RStudio and click Run App.

Alternatively, one can create a batch file in a format similar to:

```VBA
@echo off
Rscript -e "shiny::runApp("path_to_app_folder", launch.browser = TRUE)"
```

Or, simply run in terminal

```
shiny::runApp("path_to_app_folder", launch.browser = TRUE)
```