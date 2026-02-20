# Plate Reader Web Application

## Overview
The Plate Reader Web Application is an interactive tool designed for the inspection, analysis, and management of multi-well plate data. It provides an intuitive interface to select plates, apply labels, and process experimental data while maintaining a clean, reproducible workflow.

Key features include:

- **Dynamic plate selection** – Users can view and select from multiple plates.
- **Labeling system** – Apply and update labels to wells, including controls and blanks.
- **Statistical analysis** – Choose from a variety of statistical tests to analyze data.
- **Data visualization** – Generate standardized, publication-ready figures.
- **Teaching document** – Step-by-step documentation output in a clean PDF format.

---

## Table of Contents
1. [Installation](#installation)
2. [Usage](#usage)
3. [Project Structure](#project-structure)
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
4. Run the app:
```bash
shiny::runApp("app.R")
```

## Usage
- Launch the app in RStudio or from the command line.
- Upload plate data files.
- Apply labels for each test group.
- Download processed data if required.
- Select desired statistical tests and visualizations.
- Returns publication-ready results and teaching document.

plate-reader-web-app/
├─ R/                       # Shiny application source files
│   ├─ imports.R            # Package dependencies
│   ├─ main_server.R        # Overall backend logic
│   ├─ main_ui.R            # Overall user interface
│   ├─ run_app.R            # Launch application
│   ├─ screens_analysis.R   # Screen 4 UI: statistical test selection
│   ├─ screens_inspect.R    # Screen 2 UI: labelling wells
│   ├─ screens_normalize.R  # Screen 3 UI: data normalization
│   ├─ screens_results.R    # Screen 5 UI: results and visualizations
│   ├─ screens_upload.R     # Screen 1 UI: raw data upload
│   ├─ server_analysis.R    # Statistical analysis backend
│   ├─ server_inspect.R     # Well labelling backend
│   ├─ server_normalize.R   # Data normalization backend
│   ├─ server_results.R     # Publication-ready results backend
│   ├─ server_upload.R      # Raw data processing backend
├─ man/                     # Documentation
│   ├─ run_plate_reader_app.Rd
├─ .Rbuildignore
├─ .Rhistory
├─ .gitignore
├─ DESCRIPTION
├─ NAMESPACE
├─ Project Proposal.pdf    # Written project proposal
├─ plateReaderApp.proj     # R directory
