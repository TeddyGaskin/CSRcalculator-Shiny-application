# file upload limit
options(shiny.maxRequestSize = 30 * 1024^2)  # 15 MB

# Load required libraries
library(colorspace)
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(DT)
library(plotly)
library(data.table)
library(readxl)
library(CSRcalculator)

# Source UI and server logic
source("csrFunctions.R")
source("csrUi.R")
source("csrServer.R")

# Define UI and server
ui <- csrUi

server <- function(input, output, session) {
  csrServer(input, output, session)
}

shinyApp(ui = ui, server = server)
