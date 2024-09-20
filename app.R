library(shiny)
library(shinythemes)
library(magrittr)
library(DT)
library(RColorBrewer)
library(plotly)
library(VIM)
library(dplyr)
library(limma)
library(matrixStats)
library(pcaMethods)
library(vsn)
library(reshape)
library(reshape2)
library(laeken)
library(ggplot2)
library(shinyBS)
library(shinyjs)
library(Hmisc)
library(stats)
library(tidyr)
library(readxl)
library(Matrix)
library(openxlsx)
source("source code.R")
options(shiny.maxRequestSize = 30*1024^2)

#Define UI
ui <- navbarPage(
  header = tagList(
    tags$head(
      tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-6LS933L73K"),
      tags$script(
        'window.dataLayer = window.dataLayer || [];
       function gtag(){dataLayer.push(arguments);}
       gtag("js", new Date());
       gtag("config", "G-6LS933L73K");'
      )
    ),
    tags$head(
      tags$style(HTML("
      body {
        background-image: url('background.jpg');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        height: 100%; /* Set the height of the background */
      }
    "))
    ),
    # Custom CSS for the tooltip
    tags$style(HTML("
    .tooltip-custom {
      position: relative;
      display: inline-block;
      cursor: pointer;
    }
    
    .tooltip-custom .tooltip-text {
      visibility: hidden;
      width: 200px;
      background-color: #555;
      color: #fff;
      text-align: center;
      border-radius: 6px;
      padding: 5px;
      position: absolute;
      z-index: 1;
      top: 50%;
      left: 105%; /* Position the tooltip to the right of the icon */
      margin-left: 5px; /* Optional: Adjust space between icon and tooltip */
      opacity: 0;
      transition: opacity 0.3s;
      transform: translateY(-50%); /* Center the tooltip vertically relative to the icon */
    }
    
    .tooltip-custom:hover .tooltip-text {
      visibility: visible;
      opacity: 1;
    }
  ")),
    
    tags$style(HTML("
  #best_combinations {
    font-size: 18px;  /* Increase font size for better readability */
    color: #000;  /* Change text color to black for high contrast */
    border-collapse: separate;  /* Use separate borders for differentiation */
    border-spacing: 0;  /* Remove space between cells */
    width: 100%;  /* Make the table full width */
  }
  #best_combinations th, #best_combinations td {
    border-right: 2px solid #cce5ff;  /* Light blue vertical borders between columns */
    padding: 12px;  /* Add padding to table cells */
    text-align: center;  /* Align text to the center in cells */
  }
  #best_combinations th {
    background-color: #b3d9ff;  /* Light blue background for headers */
    color: #000080;  /* Dark blue text color for contrast */
    border-bottom: 2px solid #cce5ff;  /* Light blue bottom border for headers */
  }
  #best_combinations td:last-child {
    border-right: 2px solid #cce5ff;  /* Remove the right border for the last column */
  }
  #best_combinations tr:nth-child(even) {
    background-color: #e6f2ff;  /* Very light blue background for even rows */
  }
  #best_combinations tr:nth-child(odd) {
    background-color: #ffffff;  /* White background for odd rows */
  }
  #best_combinations td {
    border-left: 2px solid #cce5ff;  /* Light blue vertical borders on the left side of cells */
  }
  #best_combinations th {
    border-left: 2px solid #cce5ff;  /* Light blue vertical borders on the left side of headers */
  }
  #best_combinations td {
    border-bottom: 2px solid #cce5ff;  /* Light blue bottom borders on the left side of headers */
  }
"))
    ),
  useShinyjs(),  # Enable shinyjs for dynamic control
  
  title = div(
    strong("lfproQC"), 
    style = "font-size:28px; color: blue; margin-bottom: 0px;"
  ),
  
  tabPanel(
    HTML('<p style="font-size:16px;"> Home </p>'),
    
    tags$div(
      style = "text-align: center;",
      tags$img(src = "lfproQC-home.png", height = "100px", width = "400px")
    ),
    
    HTML('<p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 20px; margin-bottom: 10px; font-family: palatino linotype;">
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Label-free bottom-up proteomics expression data is often affected by data heterogeneity and missing values...
    </p>'),
    
    HTML('
    <div style="display: flex; justify-content: space-between;">
      <div style="flex: 1;">
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 10px;"><strong>Three normalization methods:</strong> </p> 
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 5px;font-family: palatino linotype;">1. Robust Linear Regression (RLR) </p> 
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 5px; margin-bottom: 5px;font-family: palatino linotype;">2. Variance Stabilization Normalization (VSN) </p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 5px; margin-bottom: 10px;font-family: palatino linotype;">3. LOcally Weighted linear regreSSion (LOWESS/LOESS)</p> 
      </div>
      <div style="flex: 1;">
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 10px;"><strong>Three imputation methods:</strong> </p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 5px;font-family: palatino linotype;">1. k-Nearest Neighbour (KNN)</p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 5px; margin-bottom: 5px;font-family: palatino linotype;">2. Singular Value Decomposition (SVD)</p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 5px; margin-bottom: 10px;font-family: palatino linotype;">3. Local Least Squares (LLS)</p>
      </div>
      <div style="flex: 1;">
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 10px;"><strong>Three evaluation measures:</strong> </p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 5px;font-family: palatino linotype;">1. Pooled Co-efficient of Variance (PCV)</p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 5px; margin-bottom: 5px;font-family: palatino linotype;">2. Pooled Estimate of Variance (PEV)</p>
        <p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 5px; margin-bottom: 10px;font-family: palatino linotype;">3. Pooled Median Absolute Deviation (PMAD)</p>
      </div>
    </div>
  '),
    HTML('<p style="font-size: 18px; color: #333333; line-height: 1.5; margin-top: 10px; margin-bottom: 15px;font-family: palatino linotype;">
        The user can also visualize the results by using various available exploratory plots. This tool also provides an option to conduct a differential expression analysis between two sample groups. The chosen three normalization methods, three imputation methods, and three evaluation measures were selected for this study based on the research papers published by <a href="https://doi.org/10.1093/bib/bbw095" style="color: #1a0dab;"> Välikangas et al. (2016) </a>,  <a href="https://doi.org/10.1038/s41598-021-81279-4" style="color: #1a0dab;"> Jin et al. (2021) </a>, and <a href="http://dx.doi.org/10.2174/1574893618666230223150253" style="color: #1a0dab;"> Srivastava et al. (2023) </a>. The user can also access these functionalities through the R package named <a href="https://cran.r-project.org/web/packages/lfproQC/index.html" style="color: #1a0dab;"> lfproQC </a>. 
      </p>'
    ),
    HTML('<p style="font-size: 30px; color: #1d2951; line-height: 1.5; margin-top: 30px; margin-bottom: 30px;text-align: center;font-family: palatino linotype;">
        <span style="background-color: #ffebcd ;"><strong>Methodology of lfproQC</strong></span>
      </p>'
    ),
    
    tags$div(
      style = "text-align: center;",
      tags$img(src = "graphical_abstract-shiny.jpg", height = "450px", width = "800px")
    )
  ),
  
  tabPanel(
    HTML('<p style="font-size:16px;"> Data upload </p>'),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        fileInput("file1", label = h4(strong("Choose the proteomics data")), accept = c(".csv", ".xlsx")),
        
        radioButtons(
          inputId = "data_type", 
          label = HTML('<p style="font-size:14px; color:#49796b;">Choose data type</p>'), 
          choices = c("Peptide", "Protein"),
          inline = TRUE
        ),
        
        # Conditionally show aggregation method if 'Peptide' is selected
        conditionalPanel(
          condition = "input.data_type == 'Peptide'",
          selectInput("aggr_method", 
                      HTML('<p style="font-size:14px; color:#49796b;"> Choose peptide aggregation method</p>'),
                      choices = c("sum", "mean", "median")),
          bsTooltip("aggr_method", "Choose a method for aggregating peptide data to calculate the corresponding protein values", placement = "right", options = list(container = "body"))
        ),
        actionButton("takeDataset", "Use the example dataset", class = "btn btn-primary btn-info"),
        span(class = "tooltip-custom", icon("question-circle"),
             span(class = "tooltip-text", "Click here to upload the example protein dataset")),
        br(),
        br(),
        fileInput("file2", h4(strong("Choose the group information")), accept = c(".csv", ".xlsx")),
        actionButton("takeDataGroup", "Use the example datagroup", class = "btn btn-primary btn-info"),
        span(class = "tooltip-custom", icon("question-circle"),
             span(class = "tooltip-text", "Click here to upload the example datagroup")),
        br(),
        br(),
        actionButton("btn_input", "Submit", class = "btn btn-success btn-block", icon = icon("thumbs-up")),
        helpText("Note: Upload either .csv, .xlsx, or .txt files")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Selected data", dataTableOutput("input_data"), downloadButton("download_selected_data", "Download Selected Data")),
          tabPanel("Selected group information", dataTableOutput("input_groups"), downloadButton("download_group_data", "Download Group Information")),
          tabPanel("Rollup protein data", "The processed peptide data will appear below. After uploading the peptide data and datagroups click 'Submit' button and wait for sometime.", dataTableOutput("rollup_protein"),
                   downloadButton("download_rollup_protein", "Download Rollup Protein Data"))
        )
      )
    )
  ),
  tabPanel(
    HTML('<p style="font-size:16px;"> Results </p>'),
    navlistPanel(
      widths = c(3, 8),
      
      # Best combinations tab
      tabPanel(
        HTML('<p style = "font-size:18px;color:#3d0c02;border: 2px solid #3d0c02;padding: 10px;background-color: #ffe6cc;"><strong> Best combinations </strong> </p>'),
        
        mainPanel(
          useShinyjs(),  # Initialize shinyjs
          fluidRow(
            # Title and description
            column(
              width = 12,
              HTML('<p style="font-size:18px;color:#3d0c02;text-align: left;">The suitable combinations, along with the PCV, PEV, PMAD, and NRMSE values for all combinations, are presented below.(Please wait a moment...)</p>'),
              tableOutput("best_combinations"),
              tags$hr()  # Horizontal line for separation
            )
          ),
          fluidRow(
            # First Row: PCV Table and Download Button
            column(
              width = 6,
              HTML('<p style="font-size:14px;color:#3d0c02;text-align: left;">PCV Values</p>'),
              actionButton("toggle_pcv_table", label = NULL, icon = icon("eye"), class = "btn btn-light"),
              div(
                id = "pcv_container",  # Div containing the table and download button
                style = "display: none;",  # Initially hidden
                dataTableOutput("pcv_result"),
                downloadButton("download_pcv_result", "Download PCV values", class = "btn btn-secondary"),
                tags$hr()  # Horizontal line for separation
              )
            )
          ),
          
          fluidRow(
            # Second Row: PEV Table and Download Button
            column(
              width = 6,
              HTML('<p style="font-size:14px;color:#3d0c02;text-align: left;">PEV Values</p>'),
              actionButton("toggle_pev_table", label = NULL, icon = icon("eye"), class = "btn btn-light"),
              div(
                id = "pev_container",  # Div containing the table and download button
                style = "display: none;",  # Initially hidden
                dataTableOutput("pev_result"),
                downloadButton("download_pev_result", "Download PEV values", class = "btn btn-secondary"),
                tags$hr()  # Horizontal line for separation
              )
            )
          ),
          
          fluidRow(
            # Third Row: PMAD Table and Download Button
            column(
              width = 6,
              HTML('<p style="font-size:14px;color:#3d0c02;text-align: left;">PMAD Values</p>'),
              actionButton("toggle_pmad_table", label = NULL, icon = icon("eye"), class = "btn btn-light"),
              div(
                id = "pmad_container",  # Div containing the table and download button
                style = "display: none;",  # Initially hidden
                dataTableOutput("pmad_result"),
                downloadButton("download_pmad_result", "Download PMAD values", class = "btn btn-secondary"),
                tags$hr()  # Horizontal line for separation
              )
            )
          ),
          
          fluidRow(
            # Fourth Row: NRMSE Table and Download Button
            column(
              width = 6,
              HTML('<p style="font-size:14px;color:#3d0c02;text-align: left;">NRMSE Values</p>'),
              actionButton("toggle_nrmse_table", label = NULL, icon = icon("eye"), class = "btn btn-light"),
              div(
                id = "nrmse_container",  # Div containing the table and download button
                style = "display: none;",  # Initially hidden
                dataTableOutput("nrmse_result"),
                downloadButton("download_nrmse_result", "Download NRMSE values", class = "btn btn-secondary"),
                tags$hr()  # Horizontal line for separation
              )
            )
          )
        )
      ),
      # Normalized datasets section
      HTML('<p style = "font-size: 20px; color: #3d0c02"><strong> Normalized datasets </strong></p>'),
      tabPanel("vsn normalized data", dataTableOutput("vsn_data"), downloadButton("download_vsn_Data", "Download Data")),
      tabPanel("loess normalized data", dataTableOutput("loess_data"), downloadButton("download_loess_Data", "Download Data")),
      tabPanel("rlr normalized data", dataTableOutput("rlr_data"), downloadButton("download_rlr_Data", "Download Data")),
      
      # Normalized and MVs imputed datasets
      HTML('<p style="font-size: 20px; color: #3d0c02; line-height: 1.5; margin: 0; padding: 0;"><strong>Normalized and MVs imputed datasets</strong></p>'),
      HTML('<br>'),  # Adds a blank line below the text
      
      tabPanel("vsn_knn", dataTableOutput("vsn_knn_data"), downloadButton("download_vsn_knn_Data", "Download Data")),
      tabPanel("vsn_lls", dataTableOutput("vsn_lls_data"), downloadButton("download_vsn_lls_Data", "Download Data")),
      tabPanel("vsn_svd", dataTableOutput("vsn_svd_data"), downloadButton("download_vsn_svd_Data", "Download Data")),
      
      tabPanel("loess_knn", dataTableOutput("loess_knn_data"), downloadButton("download_loess_knn_Data", "Download Data")),
      tabPanel("loess_lls", dataTableOutput("loess_lls_data"), downloadButton("download_loess_lls_Data", "Download Data")),
      tabPanel("loess_svd", dataTableOutput("loess_svd_data"), downloadButton("download_loess_svd_Data", "Download Data")),
      
      tabPanel("rlr_knn", dataTableOutput("rlr_knn_data"), downloadButton("download_rlr_knn_Data", "Download Data")),
      tabPanel("rlr_lls", dataTableOutput("rlr_lls_data"), downloadButton("download_rlr_lls_Data", "Download Data")),
      tabPanel("rlr_svd", dataTableOutput("rlr_svd_data"), downloadButton("download_rlr_svd_Data", "Download Data"))
    )
    ),
  tabPanel(HTML('<p style="font-size:16px;"> Exploratory plots </p>'),
           #                       
           navlistPanel(
             HTML('<p style = "font-size: 20px; color: #3d0c02"><strong>Choose combination type</strong></p>'), widths = c(3,9),
             tabPanel("vsn_knn", 
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_vsn_knn", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_vsn_knn", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_vsn_knn", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_vsn_knn", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_vsn_knn", width = "100%", height = "800px")),
                      )),
             tabPanel("vsn_lls",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_vsn_lls", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_vsn_lls", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_vsn_lls", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_vsn_lls", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_vsn_lls", width = "100%", height = "800px")),
                      )),
             tabPanel("vsn_svd",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_vsn_svd", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_vsn_svd", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_vsn_svd", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_vsn_svd", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_vsn_svd", width = "100%", height = "800px")),
                      )),
             tabPanel("loess_knn",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_loess_knn", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_loess_knn", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_loess_knn", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_loess_knn", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_loess_knn", width = "100%", height = "800px")),
                      )),
             tabPanel("loess_lls",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_loess_lls", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_loess_lls", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_loess_lls", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_loess_lls", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_loess_lls", width = "100%", height = "800px")),
                      )),
             tabPanel("loess_svd",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_loess_svd", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_loess_svd", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_loess_svd", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_loess_svd", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_loess_svd", width = "100%", height = "800px")),
                      )),
             tabPanel("rlr_knn",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_rlr_knn", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_rlr_knn", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_rlr_knn", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_rlr_knn", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_rlr_knn", width = "100%", height = "800px")),
                      )),
             tabPanel("rlr_lls",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_rlr_lls", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_rlr_lls", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_rlr_lls", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_rlr_lls", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_rlr_lls", width = "100%", height = "800px")),
                      )),
             tabPanel("rlr_svd",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_rlr_svd", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_rlr_svd", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_rlr_svd", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_rlr_svd", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_rlr_svd", width = "100%", height = "800px")),
                      )),
             tabPanel("Original data",
                      tabsetPanel(
                        tabPanel("Boxplot", plotlyOutput("Boxplot_data_original", width = "100%", height = "800px")),
                        tabPanel("Density plot", plotlyOutput("Densityplot_data_original", width = "100%", height = "800px")),
                        tabPanel("Correlation heatmap", plotlyOutput("Corrplot_data_original", width = "100%", height = "800px")),
                        tabPanel("QQ plot", plotlyOutput("QQplot_data_original", width = "100%", height = "800px")),
                        tabPanel("MDS plot", plotlyOutput("MDSplot_data_original", width = "100%", height = "800px")),
                      ))
             )
           ),
  tabPanel(HTML('<p style="font-size:16px;"> Differential expression analysis </p>'),
           tabsetPanel(
             tabPanel(HTML('<p style="font-size: 18px; color: #c0362c;"> <strong>MA plot</strong></p>'),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     HTML('<p style="font-size: 20px; color: red;">MA plot</p>'),
                                     selectInput("combination_ma", HTML('<p style="font-size:18px; color:#49796b;"> Choose combination</p>'),
                                                 c("vsn_knn", "vsn_lls", "vsn_svd",
                                                   "loess_knn", "loess_lls", "loess_svd",
                                                   "rlr_knn", "rlr_lls", "rlr_svd", "Original_data")),
                                     HTML('<p style="font-size: 16px; color: #5a4fcf;"> <strong> The number of groups in the dataset is:</strong></p>'),
                                     verbatimTextOutput("gr_num_ma"),
                                     HTML('<p style="font-size: 16px; color: #5a4fcf;"> <strong>Pairwise DE analysis values (Ex:2 vs 1)</strong></p>'),
                                     numericInput("ch_gr1_ma", tags$span(style = "font-weight:normal;", "Choose the first group value"), value = 0),
                                     bsTooltip("ch_gr1_ma", "Numerical value of Test sample group, e.g., enter 2 for 2 vs 1 comparison", placement = "right", options = list(container = "body")),                                
                                     numericInput("ch_gr2_ma", tags$span(style = "font-weight:normal;", "Choose the second group value"), value = 0),
                                     bsTooltip("ch_gr2_ma", "Numerical value of Control sample group, e.g., enter 1 for 2 vs 1 comparison", placement = "right", options = list(container = "body")),                                
                                     checkboxInput("usevars_ma",HTML('<p style="font-size: 16px; color: #cc0000 ;margin-bottom: 2px;"> <strong> Override cut-off limits and p-value </strong></p>')),
                                     bsTooltip("usevars_ma", "Click this checkbox if you want to visualize and get the results of up and down-regulated proteins. Otherwise a simple plot will appear", placement = "right", options = list(container = "body")),                                
                                     
                                     conditionalPanel(
                                       condition = "input.usevars_ma == true",
                                       numericInput("x1_ma", HTML('<p style="font-size: 14px; margin-top: 2px; margin-bottom: 0px;"> Cut-off limit for down-regulated</p>'), value = 0),
                                       bsTooltip("x1_ma", "A log-fold change less than this input value is considered down-regulated. Default: -1", placement = "right", options = list(container = "body")),                                
                                       numericInput("x2_ma", HTML('<p style="font-size: 14px; margin-top: 0px; margin-bottom: 0px;"> Cut-off limit for up-regulated</p>'), value = 0),
                                       bsTooltip("x2_ma", "A log-fold change more than this input value is considered up-regulated. Default: 1", placement = "right", options = list(container = "body")),                                
                                       numericInput("p_ma", HTML('<p style="font-size: 14px; margin-top: 0px; margin-bottom: 2px;"> Choose p-value (Default:0.05)</p>'), value = 0.05),
                                       bsTooltip("p_ma", "p-value less than this input value is statistically significant", placement = "right", options = list(container = "body"))                                
                                     ),
                                     actionButton("btn_ma", "Plot!", class = "btn btn-success btn-block", icon = icon("thumbs-up")),
                        ),
                        mainPanel(
                          plotly::plotlyOutput("plot_ma", width = "100%", height = "600px"),
                          tabsetPanel(
                            tabPanel(h4("Result"),
                                     dataTableOutput("result_ma"),downloadButton("download_result_ma", "Download Data")),
                            tabPanel(h4("Up-Regulated"),
                                     dataTableOutput("upreg_ma"),downloadButton("download_upreg_ma", "Download Data")),
                            tabPanel(h4("Down-Regulated"),
                                     dataTableOutput("downreg_ma"),downloadButton("download_downreg_ma", "Download Data")),
                            tabPanel(h4("Non-significant"),
                                     dataTableOutput("nonsignif_ma"),downloadButton("download_nonsignif_ma", "Download Data")),
                          )
                        )
                      )
             ),
             tabPanel(HTML('<p style="font-size: 18px; color: #c0362c;"> <strong>Volcano plot</strong></p>'),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     HTML('<p style="font-size: 20px; color: red;">Volcano plot</p>'),
                                     selectInput("combination_volcano", HTML('<p style="font-size:18px; color:#49796b;"> Choose combination</p>'),
                                                 c("vsn_knn", "vsn_lls", "vsn_svd",
                                                   "loess_knn", "loess_lls", "loess_svd",
                                                   "rlr_knn", "rlr_lls", "rlr_svd", "Original_data")),
                                     HTML('<p style="font-size: 16px; color: #5a4fcf;"> <strong> The number of groups in the dataset is:</strong></p>'),
                                     verbatimTextOutput("gr_num_volcano"),
                                     HTML('<p style="font-size: 16px; color: #5a4fcf;"> <strong>Pairwise DE analysis values (Ex:2 vs 1) </strong></p>'),
                                     numericInput("ch_gr1_volcano", tags$span(style = "font-weight:normal;", "Choose the first group value"), value = 0),
                                     bsTooltip("ch_gr1_volcano", "Numerical value of Test sample group, e.g., enter 2 for 2 vs 1 comparison", placement = "right", options = list(container = "body")),                                
                                     numericInput("ch_gr2_volcano", tags$span(style = "font-weight:normal;", "Choose the second group value"), value = 0),
                                     bsTooltip("ch_gr2_volcano", "Numerical value of Control sample group, e.g., enter 1 for 2 vs 1 comparison", placement = "right", options = list(container = "body")),                                
                                     checkboxInput("usevars_volcano",HTML('<p style="font-size: 16px; color: #cc0000;margin-bottom: 2px;"> <strong> Override cut-off limits and p-value </strong></p>')),
                                     bsTooltip("usevars_volcano", "Click this checkbox if you want to visualize and get the results of up and down-regulated proteins. Otherwise a simple plot will appear", placement = "right", options = list(container = "body")),                                
                                     
                                     conditionalPanel(
                                       condition = "input.usevars_volcano == true",
                                       numericInput("x1_volcano", HTML('<p style="font-size: 14px; margin-top: 2px; margin-bottom: 0px;"> Cut-off limit for down-regulated</p>'), value = 0),
                                       bsTooltip("x1_volcano", "A log-fold change less than this input value is considered down-regulated. Default: -1", placement = "right", options = list(container = "body")),                                
                                       numericInput("x2_volcano", HTML('<p style="font-size: 14px; margin-top: 0px; margin-bottom: 0px;"> Cut-off limit for up-regulated</p>'), value = 0),
                                       bsTooltip("x1_volcano", "A log-fold change less than this input value is considered down-regulated. Default: -1", placement = "right", options = list(container = "body")),                                
                                       numericInput("p_volcano", HTML('<p style="font-size: 14px; margin-top: 0px; margin-bottom: 2px;"> Choose p-value (Default:0.05)</p>'), value = 0.05),
                                       bsTooltip("p_volcano", "p-value less than this input value is statistically significant", placement = "right", options = list(container = "body"))                                
                                     ),
                                     actionButton("btn_volcano", "Plot!", class = "btn btn-success btn-block", icon = icon("thumbs-up")),
                        ),
                        mainPanel(
                          plotly::plotlyOutput("plot_volcano", width = "100%", height = "600px"),
                          tabsetPanel(
                            tabPanel(h4("Result"),
                                     dataTableOutput("result_volcano"),downloadButton("download_result_volcano", "Download Data")),
                            tabPanel(h4("Up-Regulated"),
                                     dataTableOutput("upreg_volcano"),downloadButton("download_upreg_volcano", "Download Data")),
                            tabPanel(h4("Down-Regulated"),
                                     dataTableOutput("downreg_volcano"),downloadButton("download_downreg_volcano", "Download Data")),
                            tabPanel(h4("Non-significant"),
                                     dataTableOutput("nonsignif_volcano"),downloadButton("download_nonsignif_volcano", "Download Data")),
                          )
                        )
                      )
             )
           )
           
  ),
  tabPanel(
    title = HTML('<p style="font-size:16px;">User Manual</p>'),
    
    # Container for iframe and download button
    div(
      style = "position: relative;",
      
      # Download User Manual button (HTML)
      div(
        style = "position: absolute; top: 10px; right: 10px;",
        downloadButton("downloadManualPDF", "Download User Manual (HTML)")
      ),
      
      # Iframe for embedding the user manual HTML
      tags$iframe(
        src = "User_manual.html",  # Use the new resource path
        style = "width: 100%; height: 800px; border: none;"  # Adjust styling for iframe
      )
    )
  ),
  
  tabPanel(
    title = HTML('<p style="font-size:16px;"> Team & Contact Info </p>'),
    fluidPage(
      fluidRow(
        column(width = 2, align = "center",
               img(src = "icar_logo.jpg", height = 150, width = 150)),
        column(width = 8, align = "center",
               HTML('<p style="font-size: 34px; color: #da9100 ;text-align: center; margin-bottom: 0px;"><strong>Division of Agricultural Bioinformatics</strong></p>'),
               HTML('<p style="font-size: 28px; color: #85754e ;text-align: center;margin-bottom: 0px;"><strong>ICAR - Indian Agricultural Statistics Research Institute (IASRI)</strong></p>'),
               HTML('<p style="font-size: 24px; color: #85754e ;text-align: center;"><strong>New Delhi, India</strong></p>')),
        column(width = 2, align = "center",
               img(src = "iasri_logo.png", height = 150, width = 150, align = "center"))
      )
    ),
    fluidPage(
      fluidRow(
        tags$head(
          tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
        ),
        column(width = 1), # This column creates a blank space
        column(width = 2, align = "center", 
               tags$img(src = "kabilan.JPG", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Sakthivel Kabilan</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Ph.D. Bioinformatics </p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> kabilan151414@gmail.com</p>')),
        column(width = 2, align = "center", 
               tags$img(src = "sb_lal_sir.JPG", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Shashi Bhushan Lal</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Principal Scientist</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> sb.lal@icar.gov.in</p>')),
        column(width = 2, align = "center", 
               tags$img(src = "sudhir_sir.JPG", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Sudhir Srivastava</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Senior Scientist</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> Sudhir.Srivastava@icar.gov.in</p>')),
        column(width = 2, align = "center", 
               tags$img(src = "kkcsir.JPG", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Krishna Kumar Chaturvedi</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Principal Scientist</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> kk.chaturvedi@icar.gov.in</p>')),
        column(width = 2, align = "center", 
               tags$img(src = "Mishra Sir.jpg", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Dwijesh Chandra Mishra</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Senior Scientist</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> dwij.mishra@gmail.com</p>')),
        column(width = 1) # This column creates a blank space
        
      ),

      fluidRow(
        
        column(width = 2), # This column creates a blank space
        
        column(width = 2, align = "center", 
               tags$img(src = "yasin mam.JPG", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Yasin Jeshima K</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Senior Scientist</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> yasinlab1.icar@gmail.com</p>')
        ),
        
        column(width = 2, align = "center", 
               tags$img(src = "rama_sir.jpg", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Vaidhyanathan Ramasubramanian</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Principal Scientist</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> R.Subramanian@icar.gov.in</p>')
        ),
        
        column(width = 2, align = "center", 
               tags$img(src = "girish_jha_sir.png", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Girish Kumar Jha</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Professor</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> girish.jha@icar.gov.in</p>')
        ),
        
        column(width = 2, align = "center", 
               tags$img(src = "sharan_photo.jpg", height = 200, width = 150, style = "border: 2px solid #3d0c02;"),
               HTML('<p style="font-size:18px; margin-top: 5px; margin-bottom: 0px;"> <strong>Dr. Sharanbasappa</strong></p>'),
               HTML('<p style="font-size:16px; margin-bottom: 4px;"> Project Scientist - I</p>'),
               HTML('<p style="font-size:12px; margin-bottom: 40px;"> <i class="fas fa-envelope"></i> smadival509@gmail.com</p>')
        ),
        ),
      HTML('
  <div style="text-align: center; font-size: 18px; color: #1a1a1a; font-family: Arial, sans-serif; margin: 20px 0;">
    <strong>For feedback, bug reports, or suggestions for improvements,</strong><br>
    please contact us through our GitHub page: 
    <a href="https://github.com/kabilansbio" style="color: #007bff; text-decoration: underline;">https://github.com/kabilansbio</a>.
  </div>
  <div style="margin-bottom: 40px;"></div>
'),
      )
    ),
  footer = tags$footer(
    HTML('<p style="font-size: 16px; text-align: center; color: #1d2951; font-family: calibri; background-color: #f4f0ec; padding: 10px; margin: 0; position: relative; width: 100%;">
         <strong>Copyright &copy; 2024. Division of Agricultural Bioinformatics, ICAR-Indian Agricultural Statistics Research Institute, New Delhi, India. All rights reserved.</strong>
         </p>')
  )
  )




#Define server logic
server <- function(input,output, session){
  
  # Provide the download for the User Manual (HTML)
  output$downloadManualPDF <- downloadHandler(
    filename = function() {
      "User_Manual.html"
    },
    content = function(file) {
      file.copy("www/User_manual.html", file)
    }
  )
  
  Data <- reactiveVal(NULL)
  DataGroup <- reactiveVal(NULL)
  
  # Observe for uploaded proteomics data
  observeEvent(input$file1, {
    req(input$file1)
    
    # Load the file based on extension
    dat <- switch(tools::file_ext(input$file1$name),
                  "csv" = read.csv(input$file1$datapath),
                  "xlsx" = read_excel(input$file1$datapath),
                  showModal(modalDialog(
                    title = "Unsupported Format",
                    "Please upload either a .csv or .xlsx file.",
                    easyClose = TRUE
                  ))
    )
    
    Data(dat)  # Store the data in the reactive value
  })
  
  # Load sample dataset
  observeEvent(input$takeDataset, {
    dat <- read.xlsx(file.path(getwd(), "www/sample_data.xlsx"))
    Data(dat)  # Load and clean the sample data
  })
  
  # Observe group information upload
  observeEvent(input$file2, {
    req(input$file2)
    
    # Load the file based on extension
    dat <- switch(tools::file_ext(input$file2$name),
                  "csv" = read.csv(input$file2$datapath),
                  "xlsx" = read_excel(input$file2$datapath),
                  showModal(modalDialog(
                    title = "Unsupported Format",
                    "Please upload either a .csv or .xlsx file.",
                    easyClose = TRUE
                  ))
    )
    
    DataGroup(dat)  # Store the group data
  })
  
  # Load sample group information
  observeEvent(input$takeDataGroup, {
    dat <- read.xlsx(file.path(getwd(), "www/sample_groups.xlsx"))
    DataGroup(dat)  # Load the sample group information
  })
  
  # Render the group information table
  output$input_groups <- renderDataTable({
    req(DataGroup())
    datatable(DataGroup())
  })
  
  # Reactive result based on inputs
  result <- reactive({
    req(Data(), DataGroup(), input$data_type)
    best_combination(Data(), DataGroup(), input$data_type, input$aggr_method)
  })
  
  # Render uploaded proteomics data
  output$input_data <- renderDataTable({
    req(Data())
    datatable(Data())
  })
  
  # Render rollup protein data
  output$rollup_protein <- renderDataTable({
    req(result()$`rollup_protein`)
    datatable(result()$`rollup_protein`)
  })
  
  # Download selected data
  output$download_selected_data <- downloadHandler(
    filename = function() {
      paste("selected_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(Data(), file, row.names = FALSE)
    }
  )
  
  # Download group information
  output$download_group_data <- downloadHandler(
    filename = function() {
      paste("group_information-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(DataGroup(), file, row.names = FALSE)
    }
  )
  
  # Download rollup protein data
  output$download_rollup_protein <- downloadHandler(
    filename = function() {
      paste("rollup_protein_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(result()$`rollup_protein`, file, row.names = FALSE)
    }
  )
  
  # Download user manual
  output$downloadManualPDF <- downloadHandler(
    filename = function() {
      "User_manual.html"  # Name of the file to download
    },
    content = function(file) {
      file.copy("www/User_manual.html", file)  # Ensure the file is in the www directory
    }
  )
  
  output$best_combinations <- renderTable({
    return(result()$`Best combinations`)
  })
  
  output$vsn_data <- renderDataTable({
    return(result()$`vsn_data`)
  })
  
  output$download_vsn_Data <- downloadHandler(
    filename = function(){
      paste("vsn-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`vsn_data`, file, row.names = FALSE)
    }
  )
  
  output$loess_data <- renderDataTable({
    return(result()$`loess_data`)
  })
  
  output$download_loess_Data <- downloadHandler(
    filename = function(){
      paste("loess-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`loess_data`, file, row.names = FALSE)
    }
  )
  
  output$rlr_data <- renderDataTable({
    return(result()$`rlr_data`)
  })
  
  output$download_rlr_Data <- downloadHandler(
    filename = function(){
      paste("rlr-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`rlr_data`, file, row.names = FALSE)
    }
  )
  
  output$vsn_knn_data <- renderDataTable({
    return(result()$`vsn_knn_data`)
  })
  
  output$download_vsn_knn_Data <- downloadHandler(
    filename = function(){
      paste("vsn_knn-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`vsn_knn_data`, file, row.names = FALSE)
    }
  )
  
  output$vsn_lls_data <- renderDataTable({
    return(result()$`vsn_lls_data`)
  })
  
  output$download_vsn_lls_Data <- downloadHandler(
    filename = function(){
      paste("vsn_lls-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`vsn_lls_data`, file, row.names = FALSE)
    }
  )
  
  output$vsn_svd_data <- renderDataTable({
    return(result()$`vsn_svd_data`)
  })
  
  output$download_vsn_svd_Data <- downloadHandler(
    filename = function(){
      paste("vsn_svd-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`vsn_svd_data`, file, row.names = FALSE)
    }
  )
  output$loess_knn_data <- renderDataTable({
    return(result()$`loess_knn_data`)
  })
  
  output$download_loess_knn_Data <- downloadHandler(
    filename = function(){
      paste("loess_knn-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`loess_knn_data`, file, row.names = FALSE)
    }
  )
  
  output$loess_lls_data <- renderDataTable({
    return(result()$`loess_lls_data`)
  })
  
  output$download_loess_lls_Data <- downloadHandler(
    filename = function(){
      paste("loess_lls-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`loess_lls_data`, file, row.names = FALSE)
    }
  )
  
  output$loess_svd_data <- renderDataTable({
    return(result()$`loess_svd_data`)
  })
  
  output$download_loess_svd_Data <- downloadHandler(
    filename = function(){
      paste("loess_svd-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`loess_svd_data`, file, row.names = FALSE)
    }
  )
  
  output$rlr_knn_data <- renderDataTable({
    return(result()$`rlr_knn_data`)
  })
  
  output$download_rlr_knn_Data <- downloadHandler(
    filename = function(){
      paste("rlr_knn-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`rlr_knn_data`, file, row.names = FALSE)
    }
  )
  
  output$rlr_lls_data <- renderDataTable({
    return(result()$`rlr_lls_data`)
  })
  
  output$download_rlr_lls_Data <- downloadHandler(
    filename = function(){
      paste("rlr_lls-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`rlr_lls_data`, file, row.names = FALSE)
    }
  )
  
  output$rlr_svd_data <- renderDataTable({
    return(result()$`rlr_svd_data`)
  })
  
  output$download_rlr_svd_Data <- downloadHandler(
    filename = function(){
      paste("rlr_svd-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`rlr_svd_data`, file, row.names = FALSE)
    }
  )
  
  output$pcv_result <- renderDataTable({
    datatable(
      result()$`PCV Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    )
  })
  
  # Download handler for PCV data
  output$download_pcv_result <- downloadHandler(
    filename = function() {
      paste("pcv_result-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result()$`PCV Result`, file, row.names = FALSE)
    }
  )
  
  # Render the PEV table
  output$pev_result <- renderDataTable({
    datatable(
      result()$`PEV Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    )
  })
  
  # Download handler for PEV data
  output$download_pev_result <- downloadHandler(
    filename = function() {
      paste("pev_result-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result()$`PEV Result`, file, row.names = FALSE)
    }
  )
  
  # Render the PMAD table
  output$pmad_result <- renderDataTable({
    datatable(
      result()$`PMAD Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    )
  })
  
  # Download handler for PMAD data
  output$download_pmad_result <- downloadHandler(
    filename = function() {
      paste("pmad_result-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result()$`PMAD Result`, file, row.names = FALSE)
    }
  )
  
  # Render the NRMSE table
  output$nrmse_result <- renderDataTable({
    datatable(
      result()$`NRMSE Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE
      )
    )
  })
  
  # Download handler for NRMSE data
  output$download_nrmse_result <- downloadHandler(
    filename = function() {
      paste("nrmse_result-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(result()$`NRMSE Result`, file, row.names = FALSE)
    }
  )
  
  # JavaScript to toggle the table visibility
  observeEvent(input$toggle_pcv_table, {
    shinyjs::toggle("pcv_container")
  })
  
  observeEvent(input$toggle_pev_table, {
    shinyjs::toggle("pev_container")
  })
  
  observeEvent(input$toggle_pmad_table, {
    shinyjs::toggle("pmad_container")
  })
  
  observeEvent(input$toggle_nrmse_table, {
    shinyjs::toggle("nrmse_container")
  })
  
  
  #Exploratory plots (Boxplot)
  output$Boxplot_data_vsn_knn <- renderPlotly({
    Boxplot_data(result()$`vsn_knn_data`)
   })
 
  output$Boxplot_data_vsn_lls <- renderPlotly({
    Boxplot_data(result()$`vsn_lls_data`)
   })
 
  output$Boxplot_data_vsn_svd <- renderPlotly({
    Boxplot_data(result()$`vsn_svd_data`)
   })
 
  output$Boxplot_data_loess_knn <- renderPlotly({
    Boxplot_data(result()$`loess_knn_data`)
   })
 
  output$Boxplot_data_loess_lls <- renderPlotly({
    Boxplot_data(result()$`loess_lls_data`)
   })
 
  output$Boxplot_data_loess_svd <- renderPlotly({
    Boxplot_data(result()$`loess_svd_data`)
   })
 
  output$Boxplot_data_rlr_knn <- renderPlotly({
    Boxplot_data(result()$`rlr_knn_data`)
   })
 
  output$Boxplot_data_rlr_lls <- renderPlotly({
    Boxplot_data(result()$`rlr_lls_data`)
   })
 
  output$Boxplot_data_rlr_svd <- renderPlotly({
    Boxplot_data(result()$`rlr_svd_data`)
   })
  
  output$Boxplot_data_original <- renderPlotly({
    Boxplot_data(original_data())
  })
 
 #Exploratory plots (Density plot)
  output$Densityplot_data_vsn_knn <- renderPlotly({
    Densityplot_data(result()$`vsn_knn_data`)
  })
  
  output$Densityplot_data_vsn_lls <- renderPlotly({
    Densityplot_data(result()$`vsn_lls_data`)
  })
  
  output$Densityplot_data_vsn_svd <- renderPlotly({
    Densityplot_data(result()$`vsn_svd_data`)
  })
  
  output$Densityplot_data_loess_knn <- renderPlotly({
    Densityplot_data(result()$`loess_knn_data`)
  })
  
  output$Densityplot_data_loess_lls <- renderPlotly({
    Densityplot_data(result()$`loess_lls_data`)
  })
  
  output$Densityplot_data_loess_svd <- renderPlotly({
    Densityplot_data(result()$`loess_svd_data`)
  })
  
  output$Densityplot_data_rlr_knn <- renderPlotly({
    Densityplot_data(result()$`rlr_knn_data`)
  })
  
  output$Densityplot_data_rlr_lls <- renderPlotly({
    Densityplot_data(result()$`rlr_lls_data`)
  })
  
  output$Densityplot_data_rlr_svd <- renderPlotly({
    Densityplot_data(result()$`rlr_svd_data`)
  })
  
  output$Densityplot_data_original <- renderPlotly({
    Densityplot_data(original_data())
  })
  
  #Exploratory plots (Correlation heatmap plot)
  output$Corrplot_data_vsn_knn <- renderPlotly({
    Corrplot_data(result()$`vsn_knn_data`)
  })
  
  output$Corrplot_data_vsn_lls <- renderPlotly({
    Corrplot_data(result()$`vsn_lls_data`)
  })
  
  output$Corrplot_data_vsn_svd <- renderPlotly({
    Corrplot_data(result()$`vsn_svd_data`)
  })
  
  output$Corrplot_data_loess_knn <- renderPlotly({
    Corrplot_data(result()$`loess_knn_data`)
  })
  
  output$Corrplot_data_loess_lls <- renderPlotly({
    Corrplot_data(result()$`loess_lls_data`)
  })
  
  output$Corrplot_data_loess_svd <- renderPlotly({
    Corrplot_data(result()$`loess_svd_data`)
  })
  
  output$Corrplot_data_rlr_knn <- renderPlotly({
    Corrplot_data(result()$`rlr_knn_data`)
  })
  
  output$Corrplot_data_rlr_lls <- renderPlotly({
    Corrplot_data(result()$`rlr_lls_data`)
  })
  
  output$Corrplot_data_rlr_svd <- renderPlotly({
    Corrplot_data(result()$`rlr_svd_data`)
  })
  
  output$Corrplot_data_original <- renderPlotly({
    Corrplot_data(original_data())
  })
  
  #Exploratory plots (QQ-plot)
  output$QQplot_data_vsn_knn <- renderPlotly({
    QQplot_data(result()$`vsn_knn_data`)
  })
  
  output$QQplot_data_vsn_lls <- renderPlotly({
    QQplot_data(result()$`vsn_lls_data`)
  })
  
  output$QQplot_data_vsn_svd <- renderPlotly({
    QQplot_data(result()$`vsn_svd_data`)
  })
  
  output$QQplot_data_loess_knn <- renderPlotly({
    QQplot_data(result()$`loess_knn_data`)
  })
  
  output$QQplot_data_loess_lls <- renderPlotly({
    QQplot_data(result()$`loess_lls_data`)
  })
  
  output$QQplot_data_loess_svd <- renderPlotly({
    QQplot_data(result()$`loess_svd_data`)
  })
  
  output$QQplot_data_rlr_knn <- renderPlotly({
    QQplot_data(result()$`rlr_knn_data`)
  })
  
  output$QQplot_data_rlr_lls <- renderPlotly({
    QQplot_data(result()$`rlr_lls_data`)
  })
  
  output$QQplot_data_rlr_svd <- renderPlotly({
    QQplot_data(result()$`rlr_svd_data`)
  })
  
  output$QQplot_data_original <- renderPlotly({
    QQplot_data(original_data())
  })
  
  #Exploratory plots (MDS plot)
  output$MDSplot_data_vsn_knn <- renderPlotly({
    MDSplot_data(result()$`vsn_knn_data`)
  })
  
  output$MDSplot_data_vsn_lls <- renderPlotly({
    MDSplot_data(result()$`vsn_lls_data`)
  })
  
  output$MDSplot_data_vsn_svd <- renderPlotly({
    MDSplot_data(result()$`vsn_svd_data`)
  })
  
  output$MDSplot_data_loess_knn <- renderPlotly({
    MDSplot_data(result()$`loess_knn_data`)
  })
  
  output$MDSplot_data_loess_lls <- renderPlotly({
    MDSplot_data(result()$`loess_lls_data`)
  })
  
  output$MDSplot_data_loess_svd <- renderPlotly({
    MDSplot_data(result()$`loess_svd_data`)
  })
  
  output$MDSplot_data_rlr_knn <- renderPlotly({
    MDSplot_data(result()$`rlr_knn_data`)
  })
  
  output$MDSplot_data_rlr_lls <- renderPlotly({
    MDSplot_data(result()$`rlr_lls_data`)
  })
  
  output$MDSplot_data_rlr_svd <- renderPlotly({
    MDSplot_data(result()$`rlr_svd_data`)
  })
  
  output$MDSplot_data_original <- renderPlotly({
    MDSplot_data(original_data())
  })
  
  #DE analysis (MA plot)
  
  output$gr_num_ma <- renderText({
    grps <- length(unique(DataGroup()$Groups))
    grps
  })
  
  comb_data_ma <- reactive({
    switch(input$combination_ma,
           "vsn_knn" =  top_table_fn(result()$`vsn_knn_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "vsn_lls" =  top_table_fn(result()$`vsn_lls_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "vsn_svd" =  top_table_fn(result()$`vsn_svd_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "loess_knn" =  top_table_fn(result()$`loess_knn_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "loess_lls" =  top_table_fn(result()$`loess_lls_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "loess_svd" =  top_table_fn(result()$`loess_svd_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "rlr_knn" =  top_table_fn(result()$`rlr_knn_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "rlr_lls" =  top_table_fn(result()$`rlr_lls_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "rlr_svd" =  top_table_fn(result()$`rlr_svd_data`, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "Original_data" =  top_table_fn(original_data(), DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma))
  })
  results_ma <- reactive({
    req(input$btn_ma)
    if (isolate(input$usevars_ma)) {
      x1 <- isolate(input$x1_ma)
      x2 <- isolate(input$x2_ma)
      p <- isolate(input$p_ma)
       MAplot_DE_fn(comb_data_ma(), x1=input$x1_ma, x2=input$x2_ma, p=input$p_ma)
    } else {
      list("MA Plot" =  MAplot_DE_fn(comb_data_ma()))
    }
  })
  output$plot_ma <- plotly::renderPlotly({
    req(results_ma())
    results_ma()[["MA Plot"]]
  })
  output$result_ma <- renderDataTable({
    res <- req(results_ma())
    validate(
      need(isTRUE(isolate(input$usevars_ma)), "'Results' not applicable without x1, x2, and p values")
    )
    res[["Result"]]
  })
  output$upreg_ma <- renderDataTable({
    res <- req(results_ma())
    validate(
      need(isTRUE(isolate(input$usevars_ma)), "'Up-Regulated' not applicable without x1, x2, and p values")
    )
    res[["Up-regulated"]]
  })
  output$downreg_ma <- renderDataTable({
    res <- req(results_ma())
    validate(
      need(isolate(input$usevars_ma), "'Down-Regulated' not applicable without x1, x2, and p values")
    )
    res[["Down-regulated"]]
  })
  output$signif_ma <- renderDataTable({
    res <- req(results_ma())
    validate(
      need(isolate(input$usevars_ma), "'Significant' not applicable without x1, x2, and p values")
    )
    res[["Significant"]]
  })
  output$nonsignif_ma <- renderDataTable({
    res <- req(results_ma())
    validate(
      need(isolate(input$usevars_ma), "'Non-significant' not applicable without x1, x2, and p values")
    )
    res[["Non-significant"]]
  })
  
  output$download_result_ma <- downloadHandler(
    filename = function(){
      paste("result_ma-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(results_ma()$`Result`, file, row.names = TRUE)
    }
  )
  
  output$download_upreg_ma <- downloadHandler(
    filename = function(){
      paste("upreg_ma-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(results_ma()$`Up-regulated`, file, row.names = TRUE)
    }
  )
  
  output$download_downreg_ma <- downloadHandler(
    filename = function(){
      paste("downreg_ma-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(results_ma()$`Down-regulated`, file, row.names = TRUE)
    }
  )

  output$download_signif_ma <- downloadHandler(
    filename = function(){
      paste("signif_ma-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(results_ma()$`Significant`, file, row.names = TRUE)
    }
  )
  
  output$download_nonsignif_ma <- downloadHandler(
    filename = function(){
      paste("nonsignif_ma-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(results_ma()$`Non-significant`, file, row.names = TRUE)
    }
  )
#DE analysis (Volcano plot)
  
  output$gr_num_volcano <- renderText({
    grps <- length(unique(DataGroup()$Groups))
    grps
  })
  
  comb_data_volcano <- reactive({
    switch(input$combination_volcano,
           "vsn_knn" =  top_table_fn(result()$`vsn_knn_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "vsn_lls" =  top_table_fn(result()$`vsn_lls_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "vsn_svd" =  top_table_fn(result()$`vsn_svd_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "loess_knn" =  top_table_fn(result()$`loess_knn_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "loess_lls" =  top_table_fn(result()$`loess_lls_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "loess_svd" =  top_table_fn(result()$`loess_svd_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "rlr_knn" =  top_table_fn(result()$`rlr_knn_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "rlr_lls" =  top_table_fn(result()$`rlr_lls_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "rlr_svd" =  top_table_fn(result()$`rlr_svd_data`, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "Original_data" =  top_table_fn(original_data(), DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano))
  })
  results_volcano <- reactive({
    req(input$btn_volcano)
    if (isolate(input$usevars_volcano)) {
      x1 <- isolate(input$x1_volcano)
      x2 <- isolate(input$x2_volcano)
      p <- isolate(input$p_volcano)
       volcanoplot_DE_fn(comb_data_volcano(), x1=input$x1_volcano, x2=input$x2_volcano, p=input$p_volcano)
    } else {
      list("Volcano Plot" =  volcanoplot_DE_fn(comb_data_volcano()))
    }
  })
  output$plot_volcano <- plotly::renderPlotly({
    req(results_volcano())
    results_volcano()[["Volcano Plot"]]
  })
  output$result_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(
      need(isTRUE(isolate(input$usevars_volcano)), "'Results' not applicable without x1, x2, and p values")
    )
    res[["Result"]]
  })
  output$upreg_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(
      need(isTRUE(isolate(input$usevars_volcano)), "'Up-Regulated' not applicable without x1, x2, and p values")
    )
    res[["Up-regulated"]]
  })
  output$downreg_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(
      need(isolate(input$usevars_volcano), "'Down-Regulated' not applicable without x1, x2, and p values")
    )
    res[["Down-regulated"]]
  })
  output$signif_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(
      need(isolate(input$usevars_volcano), "'Significant' not applicable without x1, x2, and p values")
    )
    res[["Significant"]]
  })
  output$nonsignif_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(
      need(isolate(input$usevars_volcano), "'Non-significant' not applicable without x1, x2, and p values")
    )
    res[["Non-significant"]]
  })
  
  output$download_result_volcano <- downloadHandler(
    filename = function(){
      paste("result_volcano-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`svd_loess_data`, file, row.names = FALSE)
    }
  )
  
  output$download_upreg_volcano <- downloadHandler(
    filename = function(){
      paste("upreg_volcano-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`svd_loess_data`, file, row.names = FALSE)
    }
  )
  
  output$download_downreg_volcano <- downloadHandler(
    filename = function(){
      paste("downreg_volcano-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`svd_loess_data`, file, row.names = FALSE)
    }
  )
  
  output$download_signif_volcano <- downloadHandler(
    filename = function(){
      paste("signif_volcano-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`svd_loess_data`, file, row.names = FALSE)
    }
  )
  
  output$download_nonsignif_volcano <- downloadHandler(
    filename = function(){
      paste("nonsignif_volcano-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(result()$`svd_loess_data`, file, row.names = FALSE)
    }
  )
}

#Run the app

shinyApp(ui=ui, server=server)
 
