# Plate Reader Web Application
https://aubrey-schrameck.shinyapps.io/plate-reader-app/

## Overview
The Plate Reader Web Application is an interactive Shiny-based tool designed for the inspection, analysis, and management of multi-well plate data. It provides an intuitive workflow for selecting plates, labeling wells, normalizing data, performing statistical analysis, and generating publication-ready outputs.

Key features include:

- **Dynamic plate selection** – View and switch between multiple experimental plates
- **Interactive labeling system** – Assign and update well labels (controls, blanks, treatments, etc.)
- **Data normalization pipeline** – Built-in blank correction and control-based scaling
- **Statistical analysis module** – Flexible selection of statistical tests
- **Data visualization tools** – Generate publication-ready figures
- **Automated reporting** – Export a structured teaching/documentation PDF
- **User guide integration** – Built-in step-by-step usage documentation

---

## Table of Contents
1. [Installation](#installation)
2. [Usage](#usage)
3. [User Guide](#user-guide)
4. [Web Deployment](#web-deployment)
5. [Project Structure](#project-structure)
---

## Installation

1. Clone the repository:

```bash
git clone https://github.com/yourusername/plate-reader-web-app.git
cd plate-reader-web-app
```

2. Install R packages (or dependencies for your environment):
```bash
install.packages(c("shiny", "tidyverse", "shinyWidgets"))
```
4. Run the application locally:
```bash
shiny::runApp("app.R")
```

## Usage
1. Launch the app in RStudio or via command line
2. Upload plate reader data files
3. Navigate through the workflow screens:
  - Upload data
  - Inspect and label wells
  - Normalize data
  - Perform statistical analysis
  - View results and visualizations
4. Download outputs:
  - Processed datasets
  - Figures
  - Teaching/documentation PDF

## User Guide
The application includes a built-in User Guide designed to support new users and ensure reproducibility.

**Contents of the User Guide:**
- Overview of the full analysis pipeline
- How-to video
- Step-by-step walkthrough of each screen
- Explanation of labeling conventions (controls, blanks, treatments)
- Guidance on normalization methods
- Description of statistical tests available
- Tips for generating publication-quality figures
- Troubleshooting common issues

**Accessing the User Guide:**
- Available directly within the app navigation panel
- Designed for both teaching and independent use

## Web Deployment
The application has been deployed as a web-based Shiny application.

**Option 1: Access via URL**
https://aubrey-schrameck.shinyapps.io/plate-reader-app/

**Option 2: Deploy via shinyapps.io**
1. Install deployment package:
```bash
install.packages("rsconnect")
```
2. Authenticate account:
```bash
rsconnect::setAccountInfo(name='your_account',
                          token='your_token',
                          secret='your_secret')
```
3. Deploy app:
```bash
rsconnect::deployApp("path/to/plate-reader-web-app")
```

## Structure
```
plate-reader-web-app/
|
├─ R/                       # Shiny application source files
│   ├─ imports.R            # Package dependencies
│   ├─ main_server.R        # Overall backend logic
│   ├─ main_ui.R            # Overall user interface
│   ├─ run_app.R            # Launch application
│   ├─ screens_analysis.R   # Statistical test selection UI
│   ├─ screens_inspect.R    # Well labeling interface
│   ├─ screens_normalize.R  # Data normalization interface
│   ├─ screens_results.R    # Results and visualization UI
│   ├─ screens_upload.R     # Data upload interface
│   ├─ server_analysis.R    # Statistical analysis backend
│   ├─ server_inspect.R     # Labeling backend logic
│   ├─ server_normalize.R   # Normalization backend logic
│   ├─ server_results.R     # Results generation backend
│   ├─ server_upload.R      # Data ingestion backend
|
├─ man/                     # Documentation files
│   ├─ run_plate_reader_app.Rd
|
├─ www/guide/               # User guide materials
|
├─ .Rbuildignore
├─ .Rhistory
├─ .gitignore
├─ DESCRIPTION
├─ NAMESPACE
├─ Project Proposal.pdf
├─ plateReaderApp.proj
```
