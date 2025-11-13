# WhistleGen

## Description

**WhistleGen** is an interactive R Shiny web application for generating **artificial dolphin whistles**.  
Developed by **Ocean Science Analytics LLC**, this tool allows users to simulate, visualize, and export synthetic whistle contours and audio files.  

WhistleGen provides a flexible, user-friendly interface built with Shiny and integrates acoustic signal generation and analysis tools from several R packages, including **soundgen**, **seewave**, and **tuneR**. It is ideal for researchers, educators, and developers interested in bioacoustics, marine mammal communication, or signal synthesis.

---

## Authors

**Designer:**  
Peter Sugarman  

**Developers:**  
Jared Stephens  
Jennifer Pettis Schallert

---

## Installation Instructions

### 1. Clone the repository

Download the development version of **WhistleGen** from GitHub:

```bash
git clone https://github.com/Ocean-Science-Analytics/WhistleGen.git
```

### 2. Install Required Packages

```bash
install.packages(c(
  "shiny", "shinyjs", "shinyBS", "bslib", "DT",
  "tuneR", "seewave", "RcppAlgos", "shinyWidgets",
  "soundgen", "shinyFiles", "R.devices", "signal"
))
```

### 3. Run the Shiny App

```bash
shiny::runApp()
```

---

## Usage

The most up-to-date working application is on the main branch. Other branches are used for development purposes and may not be in working order.

---

## Contact Information

If you have questions, bugs or want further information about this application, please reach out to either:

Peter Sugarman: [petersu18\@msn.com](mailto:petersu18@msn.com)\
Jared Stephens: [jstephens\@oceanscienceanalytics.com](mailto:jstephens@oceanscienceanalytics.com)\
Liz Ferguson: [eferguson\@oceanscienceanalytics.com](mailto:eferguson@oceanscienceanalytics.com)

---

## Copyright

Modified work Copyright © 2025 by Peter Sugarman, Jared Stephens, Elizabeth Ferguson, & Jennifer Pettis Schallert All Rights Reserved.

