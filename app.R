library(shiny)
library(shinythemes)
library(magrittr)
library(DT)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(matrixStats)
library(ggplot2)
library(shinyBS)
library(shinyjs)
library(stats)
library(tidyr)
library(readxl)
library(reshape)
library(reshape2)
library(googleAnalyticsR)
library(googleAuthR)
library(laeken)
library(Hmisc)
library(Matrix)
library(openxlsx)
library(MASS)
library(VIM)
library(limma)
library(pcaMethods)
library(vsn)
library(shinycssloaders)
source("source code.R")
options(shiny.maxRequestSize = 30*1024^2)

googleAuthR::gar_auth_service("www/lfproqc-e71e8eac2a9d.json")

#Define UI
ui <- navbarPage(
  header = tagList(
    useShinyjs(),
    tags$head(
      # Google Analytics
      tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=G-LGZ2EVZFQ1"),
      tags$script('
        window.dataLayer = window.dataLayer || [];
        function gtag(){dataLayer.push(arguments);}
        gtag("js", new Date());
        gtag("config", "G-LGZ2EVZFQ1");
      '),
      
      # Replace the CSS section in your header with this updated professional color scheme
      
      tags$style(HTML("
        body {
          background: linear-gradient(135deg, #e8f0f8 0%, #d4e4f0 100%);
          background-attachment: fixed;
          height: 100%;
          margin: 0;
          padding: 0;
        }
        
        /* Main container styling */
        .container-fluid {
          background-color: transparent;
        }
        
        /* ===== PAGE VIEW COUNTER ===== */
        .pageview-box {
          position: fixed;
          bottom: 20px;
          right: 20px;
          background-color: rgba(255, 255, 255, 0.96);
          border: 1px solid #c5d5e6;
          border-radius: 6px;
          padding: 8px 12px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.08);
          z-index: 1000;
          width: 155px;
          text-align: center;
          transition: all 0.2s ease;
        }
        .pageview-box:hover {
          transform: translateY(-1px);
          box-shadow: 0 4px 12px rgba(0,0,0,0.12);
          border-color: #89b4d4;
        }
        .pageview-title {
          font-weight: 500;
          margin-bottom: 4px;
          font-size: 11px;
          color: #5a6e7c;
          letter-spacing: 0.3px;
        }
        .pageview-count {
          font-size: 18px;
          font-weight: 600;
          color: #2c6e9e;
        }
        
        /* ===== TOOLTIP STYLING ===== */
        .tooltip-custom {
          position: relative;
          display: inline-block;
          cursor: help;
        }
        .tooltip-custom .tooltip-text {
          visibility: hidden;
          width: 220px;
          background-color: #2c6e9e;
          color: #fff;
          text-align: center;
          border-radius: 4px;
          padding: 8px 12px;
          position: absolute;
          z-index: 100;
          top: 50%;
          left: 105%;
          margin-left: 8px;
          opacity: 0;
          transition: opacity 0.25s ease;
          transform: translateY(-50%);
          font-size: 12px;
          font-family: 'Segoe UI', Arial, sans-serif;
          line-height: 1.4;
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
          pointer-events: none;
        }
        .tooltip-custom:hover .tooltip-text {
          visibility: visible;
          opacity: 1;
        }
        
        /* ===== BEST COMBINATIONS TABLE ===== */
        #best_combinations {
          background-color: #fffde7 !important;
          border: 2px solid #ffc107 !important;
          font-size: 15px;
          width: 100%;
          border-collapse: collapse;
        }
        #best_combinations th {
          background-color: #ffc107 !important;
          color: #1a5276 !important;
          font-weight: bold !important;
          padding: 10px !important;
          border: 1px solid #ffc107 !important;
        }
        #best_combinations td {
          background-color: #ffffff !important;
          border: 1px solid #ffe082 !important;
          padding: 8px !important;
        }
        #best_combinations tr:hover td {
          background-color: #fff9c4 !important;
        }
        
        /* ===== DATA TABLE ENHANCEMENTS ===== */
        .dataTables_wrapper .dataTables_scroll {
          overflow-x: auto;
        }
        .dataTables_scrollBody {
          overflow-x: auto !important;
        }
        .dataTable {
          font-family: 'Segoe UI', Arial, sans-serif;
          font-size: 12px;
          border-collapse: collapse;
        }
        .dataTable thead th {
          background-color: #e8f0f8;
          color: #2c6e9e;
          font-weight: 600;
          padding: 10px 12px;
          border-bottom: 2px solid #c5d5e6;
        }
        .dataTable tbody td {
          padding: 8px 12px;
          border-bottom: 1px solid #e8f0f8;
        }
        .dataTable tbody tr:hover {
          background-color: #f0f6fc !important;
        }
        
        /* ===== BUTTON STYLES ===== */
        .btn-action {
          transition: all 0.2s ease;
          border-radius: 4px;
        }
        .btn-action:hover {
          transform: translateY(-1px);
          box-shadow: 0 2px 6px rgba(0,0,0,0.1);
        }
        .btn-primary {
          background-color: #2c6e9e;
          border-color: #2c6e9e;
        }
        .btn-primary:hover {
          background-color: #1a5276;
          border-color: #1a5276;
        }
        .btn-success {
          background-color: #3a7ca5;
          border-color: #3a7ca5;
        }
        .btn-success:hover {
          background-color: #2c6e9e;
          border-color: #2c6e9e;
        }
        .btn-info {
          background-color: #5a9bc2;
          border-color: #5a9bc2;
          color: white;
        }
        .btn-info:hover {
          background-color: #2c6e9e;
          border-color: #2c6e9e;
          color: white;
        }
        
        /* ===== INFO CARDS ===== */
        .info-card {
          background: rgba(255, 255, 255, 0.92);
          border: 1px solid #c5d5e6;
          border-radius: 8px;
          padding: 15px;
          margin: 10px;
          transition: all 0.2s ease;
          box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        }
        .info-card:hover {
          box-shadow: 0 4px 12px rgba(0,0,0,0.1);
          border-color: #89b4d4;
          background: rgba(255, 255, 255, 0.98);
        }
        
        /* ===== TAB NAVIGATION ===== */
        .nav-tabs {
          border-bottom: 1px solid #c5d5e6;
        }
        .nav-tabs > li > a {
          transition: all 0.2s ease;
          color: #2c3e50;
          margin: 0;
          font-size: 13px;
          border-radius: 4px 4px 0 0;
          margin-right: 3px;
          background-color: rgba(232, 240, 248, 0.6);
          border-color: #c5d5e6 #c5d5e6 transparent;
        }
        .nav-tabs > li > a:hover {
          background-color: #e8f0f8;
          color: #2c6e9e;
          border-color: #c5d5e6 #c5d5e6 transparent;
        }
        /* Fix active tab colors */
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:hover,
        .nav-tabs > li.active > a:focus {
          background-color: #2c6e9e !important;
          color: white !important;
          border-color: #2c6e9e #2c6e9e transparent !important;
        }
        
        .nav-list .nav > li.active > a,
        .nav-list .nav > li.active > a:hover,
        .nav-list .nav > li.active > a:focus {
          background-color: #2c6e9e !important;
          color: white !important;
        }
        
        /* ===== SIDEBAR NAVIGATION ===== */
        .nav-list .nav > li > a {
          transition: all 0.2s ease;
          padding: 8px 12px;
          font-size: 13px;
          color: #3a5a7a;
          border-radius: 4px;
          margin-bottom: 2px;
          background-color: transparent;
        }
        .nav-list .nav > li > a:hover {
          background-color: #e8f0f8;
          color: #2c6e9e;
          padding-left: 18px;
        }
        .nav-list .nav > li.active > a {
          background-color: #2c6e9e;
          color: white !important;
          font-weight: 500;
        }
        
        /* ===== NOTIFICATIONS ===== */
        .shiny-notification {
          border-radius: 4px !important;
          box-shadow: 0 4px 12px rgba(0,0,0,0.12) !important;
          font-size: 13px !important;
          font-family: 'Segoe UI', Arial, sans-serif !important;
          border-left: 4px solid #2c6e9e !important;
          background-color: white !important;
          color: #2c3e50 !important;
        }
        
        /* ===== PROGRESS BAR ===== */
        .progress {
          height: 4px;
          border-radius: 2px;
          margin-top: 10px;
          background-color: #e8f0f8;
        }
        .progress-bar {
          background: linear-gradient(90deg, #2c6e9e, #5a9bc2);
          transition: width 0.3s ease;
        }
        
        /* ===== FILE INPUT ===== */
        .shiny-file-input-progress {
          margin-top: 5px;
        }
        .btn-file {
          background-color: #f0f6fc;
          border: 1px solid #c5d5e6;
          transition: all 0.2s ease;
          color: #3a5a7a;
        }
        .btn-file:hover {
          background-color: #e8f0f8;
          border-color: #89b4d4;
        }
        
        /* ===== HELP TEXT ===== */
        .help-block {
          font-size: 11px;
          color: #7a8e9e;
          margin-top: 8px;
        }
        
        /* ===== SIDEBAR PANEL ===== */
        .well {
          background-color: rgba(255, 255, 255, 0.88);
          border: 1px solid #c5d5e6;
          border-radius: 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.05);
        }
        
        /* ===== MAIN PANEL ===== */
        .tab-content {
          background-color: rgba(255, 255, 255, 0.88);
          border-radius: 8px;
          padding: 15px;
          margin-top: 5px;
        }
        
        /* ===== NAVBAR STYLING ===== */
        .navbar {
          background: linear-gradient(135deg, #ffffff, #f8fafc);
          border-bottom: 1px solid #c5d5e6;
          border-radius: 0;
          box-shadow: 0 2px 6px rgba(0,0,0,0.05);
        }
        .navbar-default .navbar-nav > li > a {
          color: #3a5a7a;
          font-weight: 500;
        }
        .navbar-default .navbar-nav > li > a:hover {
          color: #2c6e9e;
          background-color: #e8f0f8;
        }
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:hover,
        .navbar-default .navbar-nav > .active > a:focus {
          color: #2c6e9e;
          background-color: #e8f0f8;
          border-bottom: 2px solid #2c6e9e;
        }
        
        /* ===== HEADINGS ===== */
        h1, h2, h3, h4, h5, h6 {
          color: #2c6e9e;
        }
        
        /* ===== LINKS ===== */
        a {
          color: #2c6e9e;
          text-decoration: none;
        }
        a:hover {
          color: #1a5276;
          text-decoration: underline;
        }
        
        /* ===== ANIMATIONS ===== */
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(10px); }
          to { opacity: 1; transform: translateY(0); }
        }
        .fade-in {
          animation: fadeIn 0.3s ease;
        }
        
        /* ===== SCROLLBAR STYLING ===== */
        ::-webkit-scrollbar {
          width: 8px;
          height: 8px;
        }
        ::-webkit-scrollbar-track {
          background: #e8f0f8;
          border-radius: 4px;
        }
        ::-webkit-scrollbar-thumb {
          background: #89b4d4;
          border-radius: 4px;
        }
        ::-webkit-scrollbar-thumb:hover {
          background: #2c6e9e;
        }
        
        /* Footer always at bottom */
          html, body {
          height: 100%;
        }
        .navbar-page {
          min-height: 100%;
          display: flex;
          flex-direction: column;
        }
        .navbar-page > .container-fluid {
          flex: 1;
        }
        footer {
          margin-top: auto;
          position: relative;
        }
      "))
    )
  ),
   
  title = div(
    strong("lfproQC"), 
    style = "font-size:28px; color: blue; margin-bottom: 0px; text-shadow: 2px 2px 4px rgba(0,0,0,0.1);"
  ),
  
  # Home Tab with enhanced animations
  tabPanel(
    HTML('<p style="font-size:16px;"> Home </p>'),
    value = "home",
    
    tags$div(
      style = "text-align: center; animation: fadeIn 0.6s ease;",
      tags$img(src = "lfproQC-home.png", height = "100px", width = "400px")
    ),
    
    HTML('<p style="font-size: 18px; color: #333333; line-height: 1.6; margin-top: 20px; margin-bottom: 10px; font-family: palatino linotype; animation: fadeIn 0.8s ease;">
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Label-free bottom-up proteomics expression data is often affected by heterogeneity and missing values. Normalization and missing value imputation are commonly applied to address these issues and prepare the dataset for downstream analysis. This Shiny application provides an optimal combination of normalization and imputation methods for label-free proteomics expression data. It utilizes three commonly used normalization methods and three imputation methods. Additionally, three statistical evaluation measures are applied to select the best combination of normalization and imputation methods for the dataset.
    </p>'),
    
    HTML('
    <div style="display: flex; justify-content: space-between; gap: 20px; animation: fadeIn 1s ease;">
      <div class="info-card" style="flex: 1;">
        <p style="font-size: 18px; color: #333333; margin-bottom: 10px;"><strong>📊 Three normalization methods:</strong></p> 
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">1. Robust Linear Regression (RLR) </p> 
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">2. Variance Stabilization Normalization (VSN) </p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">3. LOcally Weighted linear regreSSion (LOWESS/LOESS)</p> 
      </div>
      <div class="info-card" style="flex: 1;">
        <p style="font-size: 18px; color: #333333; margin-bottom: 10px;"><strong>🔧 Three imputation methods:</strong></p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">1. k-Nearest Neighbour (KNN)</p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">2. Singular Value Decomposition (SVD)</p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">3. Local Least Squares (LLS)</p>
      </div>
      <div class="info-card" style="flex: 1;">
        <p style="font-size: 18px; color: #333333; margin-bottom: 10px;"><strong>📈 Three evaluation measures:</strong></p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">1. Pooled Co-efficient of Variance (PCV)</p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">2. Pooled Estimate of Variance (PEV)</p>
        <p style="font-size: 18px; color: #333333; margin: 5px 0;font-family: palatino linotype;">3. Pooled Median Absolute Deviation (PMAD)</p>
      </div>
    </div>
  '),
    HTML('<p style="font-size: 18px; color: #333333; line-height: 1.6; margin-top: 10px; margin-bottom: 15px;font-family: palatino linotype; animation: fadeIn 1.2s ease;">
        The user can also visualize the results by using various available exploratory plots. This tool also provides an option to conduct a differential expression analysis between two sample groups. The chosen three normalization methods, three imputation methods, and three evaluation measures were selected for this study based on the research papers published by <a href="https://doi.org/10.1093/bib/bbw095" style="color: #1a0dab;" target="_blank"> Välikangas et al. (2016) </a>,  <a href="https://doi.org/10.1038/s41598-021-81279-4" style="color: #1a0dab;" target="_blank"> Jin et al. (2021) </a>, and <a href="http://dx.doi.org/10.2174/1574893618666230223150253" style="color: #1a0dab;" target="_blank"> Srivastava et al. (2023) </a>. The detailed workflow of this web application is described in our publication <a href="https://doi.org/10.1021/acs.jproteome.4c00552" style="color: #1a0dab;" target="_blank"> Sakthivel et al. (2025). </a> 
      </p>'
    ),
    HTML('<p style="font-size: 30px; color: #1d2951; line-height: 1.5; margin-top: 30px; margin-bottom: 30px;text-align: center;font-family: palatino linotype; animation: fadeIn 1.4s ease;">
        <span style="background-color: #ffebcd; padding: 8px 20px; border-radius: 50px;"><strong>📖 Methodology of lfproQC</strong></span>
      </p>'
    ),
    
    tags$div(
      style = "text-align: center; animation: fadeIn 1.6s ease;",
      tags$div(
        style = "display: inline-block; background-color: white; padding: 20px 30px; box-shadow: 0 8px 25px rgba(0,0,0,0.15);",
        tags$img(src = "graphical_abstract-shiny.png", height = "800px", width = "500px", 
                 style = "display: block;")
      )
    ),
    # Enhanced Box for Total Page Views
    div(class = "pageview-box",
        div(class = "pageview-title", "👁️ Total Page Views"),
        div(class = "pageview-count", textOutput("total_pageviews")),
        div(style = "font-size: 10px; opacity: 0.7; margin-top: 5px;", "All Time")
    )
  ),
  
  # Data Upload Tab with enhanced interactivity
  tabPanel(
    HTML('<p style="font-size:16px;"> Data upload </p>'),
    value = "data_upload",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "background: rgba(255,255,255,0.9); border-radius: 12px; padding: 20px;",
        
        # Enhanced file input with visual feedback
        div(
          style = "border: 2px dashed #ccc; border-radius: 12px; padding: 15px; text-align: center; margin-bottom: 15px; transition: all 0.3s ease;",
          fileInput("file1", label = h4(strong("📊 Choose proteomics data")), accept = c(".csv", ".xlsx"), 
                    buttonLabel = "Browse", placeholder = "No file selected")
        ),
        
        radioButtons(
          inputId = "data_type", 
          label = HTML('<p style="font-size:14px; color:#49796b;">🔬 Choose data type</p>'), 
          choices = c("Protein", "Peptide"),
          inline = TRUE,
          selected = "Protein"
        ),
        
        conditionalPanel(
          condition = "input.data_type == 'Peptide'",
          selectInput("aggr_method", 
                      HTML('<p style="font-size:14px; color:#49796b;"> 🔄 Choose peptide aggregation method</p>'),
                      choices = c("sum", "mean", "median")),
          bsTooltip("aggr_method", "Choose a method for aggregating peptide data to calculate the corresponding protein values", placement = "right", options = list(container = "body"))
        ),
        
        # Enhanced buttons with icons
        actionButton("takeDataset", "📋 Use example dataset", class = "btn btn-primary btn-info btn-action", style = "width: 100%; margin-bottom: 5px;"),
        div(style = "text-align: right; margin-bottom: 15px;",
            span(class = "tooltip-custom", icon("question-circle", style = "color: #007bff;"),
                 span(class = "tooltip-text", "Click here to upload the example protein dataset"))
        ),
        
        div(
          style = "border: 2px dashed #ccc; border-radius: 12px; padding: 15px; text-align: center; margin-bottom: 15px; transition: all 0.3s ease;",
          fileInput("file2", h4(strong("👥 Choose group information")), accept = c(".csv", ".xlsx"),
                    buttonLabel = "Browse", placeholder = "No file selected"),
        ),
        
        actionButton("takeDataGroup", "📋 Use example datagroup", class = "btn btn-primary btn-info btn-action", style = "width: 100%; margin-bottom: 5px;"),
        div(style = "text-align: right; margin-bottom: 15px;",
            span(class = "tooltip-custom", icon("question-circle", style = "color: #007bff;"),
                 span(class = "tooltip-text", "Click here to upload the example datagroup"))
        ),
        
        hr(),
        
        actionButton("btn_input", "🚀 Submit & Process", class = "btn btn-success btn-block btn-action", icon = icon("play"), 
                     style = "font-size: 16px; padding: 10px;"),
        
        # Progress indicator (hidden initially)
        div(id = "submit_progress", style = "display: none; margin-top: 10px; text-align: center;",
            div(class = "progress", div(class = "progress-bar progress-bar-striped active", style = "width: 100%;", "Processing..."))
        ),
        
        helpText("ℹ️ Note: Upload either .csv, .xlsx, or .txt files")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("📄 Selected data", 
                   br(),
                   DTOutput("input_data") %>% withSpinner(color = "#007bff"),
                   br(),
                   downloadButton("download_selected_data", "💾 Download Selected Data", class = "btn-sm")),
          tabPanel("📋 Selected group information", 
                   br(),
                   DTOutput("input_groups") %>% withSpinner(color = "#007bff"),
                   br(),
                   downloadButton("download_group_data", "💾 Download Group Information", class = "btn-sm")),
          tabPanel("🔄 Rollup protein data", 
                   br(),
                   p("The processed peptide data will appear below. After uploading the peptide data and datagroups click 'Submit' button and wait for sometime.", 
                     style = "color: #666; font-style: italic;"),
                   DTOutput("rollup_protein") %>% withSpinner(color = "#007bff"),
                   br(),
                   downloadButton("download_rollup_protein", "💾 Download Rollup Protein Data", class = "btn-sm"))
        )
      )
    )
  ),
  
  # Results Tab - Enhanced Professional Scientific Design
  tabPanel(
    HTML('<p style="font-size:16px; font-weight:500;"> Results </p>'),
    value = "results",
    navlistPanel(
      widths = c(3, 9),
      well = FALSE,
      fluid = FALSE,
      
      # ===== SIDEBAR SECTION HEADERS (now visible) =====
      h5(style = "margin-top: 10px; margin-bottom: 5px; padding: 6px 8px; background-color: #e8f0f8; border-left: 4px solid #2c6e9e; font-weight: 600; color: #1a5276;",
         icon("trophy"), " BEST COMBINATIONS"),
      
      # Best combinations tab
      tabPanel(
        HTML('<span style="font-size:15px; font-weight:500;">🏆 Best Combinations</span>'),
        br(),
        
        fluidRow(
          column(
            width = 12,
            div(
              style = "background-color: #f8f9fa; border-left: 4px solid #2c6e9e; padding: 12px 15px; margin-bottom: 20px;",
              p(style = "margin: 0; color: #2c3e50; font-size: 14px; font-family: 'Segoe UI', Arial, sans-serif;",
                icon("info-circle"), " The optimal normalization and imputation combinations are ranked based on PCV, PEV, PMAD, and NRMSE metrics. Lower values indicate better performance."
              )
            ),
            
            # Bright Best Combinations Table
            div(style = "overflow-x: auto; margin-bottom: 25px;", 
                tableOutput("best_combinations")),
            
            hr(style = "border-top: 1px solid #e0e0e0; margin: 20px 0;")
          )
        ),
        
        # Evaluation Metrics Section
        fluidRow(
          column(
            width = 12,
            h4(style = "color: #2c6e9e; font-weight: 600; margin-bottom: 15px; border-bottom: 2px solid #2c6e9e; padding-bottom: 5px; display: inline-block;",
               icon("chart-bar"), " Evaluation Metrics"),
            
            # PCV Metric
            div(style = "border: 1px solid #d0d0d0; border-radius: 6px; margin-bottom: 12px; background-color: #ffffff;",
                div(style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #d0d0d0; cursor: pointer;", 
                    onclick = "Shiny.setInputValue('toggle_pcv', Math.random())",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        div(style = "display: flex; align-items: center; gap: 10px;",
                            span(style = "font-size: 18px;", "📊"),
                            span(style = "font-weight: 600; color: #2c6e9e; font-size: 14px;", "Pooled Coefficient of Variation (PCV)")
                        ),
                        icon("chevron-down", style = "color: #7f8c8d;")
                    )
                ),
                div(id = "pcv_container", style = "display: none; padding: 15px;",
                    div(style = "overflow-x: auto; width: 100%;", DTOutput("pcv_result")),
                    br(),
                    downloadButton("download_pcv_result", "Download CSV", 
                                   class = "btn btn-default btn-sm",
                                   style = "background-color: #f8f9fa; border: 1px solid #ccc; font-size: 12px;")
                )
            ),
            
            # PEV Metric
            div(style = "border: 1px solid #d0d0d0; border-radius: 6px; margin-bottom: 12px; background-color: #ffffff;",
                div(style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #d0d0d0; cursor: pointer;",
                    onclick = "Shiny.setInputValue('toggle_pev', Math.random())",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        div(style = "display: flex; align-items: center; gap: 10px;",
                            span(style = "font-size: 18px;", "📈"),
                            span(style = "font-weight: 600; color: #2c6e9e; font-size: 14px;", "Pooled Estimate of Variance (PEV)")
                        ),
                        icon("chevron-down", style = "color: #7f8c8d;")
                    )
                ),
                div(id = "pev_container", style = "display: none; padding: 15px;",
                    div(style = "overflow-x: auto; width: 100%;", DTOutput("pev_result")),
                    br(),
                    downloadButton("download_pev_result", "Download CSV",
                                   class = "btn btn-default btn-sm",
                                   style = "background-color: #f8f9fa; border: 1px solid #ccc; font-size: 12px;")
                )
            ),
            
            # PMAD Metric
            div(style = "border: 1px solid #d0d0d0; border-radius: 6px; margin-bottom: 12px; background-color: #ffffff;",
                div(style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #d0d0d0; cursor: pointer;",
                    onclick = "Shiny.setInputValue('toggle_pmad', Math.random())",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        div(style = "display: flex; align-items: center; gap: 10px;",
                            span(style = "font-size: 18px;", "📉"),
                            span(style = "font-weight: 600; color: #2c6e9e; font-size: 14px;", "Pooled Median Absolute Deviation (PMAD)")
                        ),
                        icon("chevron-down", style = "color: #7f8c8d;")
                    )
                ),
                div(id = "pmad_container", style = "display: none; padding: 15px;",
                    div(style = "overflow-x: auto; width: 100%;", DTOutput("pmad_result")),
                    br(),
                    downloadButton("download_pmad_result", "Download CSV",
                                   class = "btn btn-default btn-sm",
                                   style = "background-color: #f8f9fa; border: 1px solid #ccc; font-size: 12px;")
                )
            ),
            
            # NRMSE Metric
            div(style = "border: 1px solid #d0d0d0; border-radius: 6px; margin-bottom: 12px; background-color: #ffffff;",
                div(style = "background-color: #f5f5f5; padding: 10px 15px; border-bottom: 1px solid #d0d0d0; cursor: pointer;",
                    onclick = "Shiny.setInputValue('toggle_nrmse', Math.random())",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                        div(style = "display: flex; align-items: center; gap: 10px;",
                            span(style = "font-size: 18px;", "📐"),
                            span(style = "font-weight: 600; color: #2c6e9e; font-size: 14px;", "Normalized Root Mean Square Error (NRMSE)")
                        ),
                        icon("chevron-down", style = "color: #7f8c8d;")
                    )
                ),
                div(id = "nrmse_container", style = "display: none; padding: 15px;",
                    div(style = "overflow-x: auto; width: 100%;", DTOutput("nrmse_result")),
                    br(),
                    downloadButton("download_nrmse_result", "Download CSV",
                                   class = "btn btn-default btn-sm",
                                   style = "background-color: #f8f9fa; border: 1px solid #ccc; font-size: 12px;")
                )
            )
          )
        ),
        br()
      ),
      
      # ===== SIDEBAR SECTION HEADER: NORMALIZED DATASETS =====
      h5(style = "margin-top: 20px; margin-bottom: 5px; padding: 6px 8px; background-color: #e8f0f8; border-left: 4px solid #3498db; font-weight: 600; color: #1a5276;",
         icon("database"), " NORMALIZED DATASETS"),
      
      tabPanel("vsn normalized data", 
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #3498db; padding: 8px 12px; margin-bottom: 15px;",
                   icon("chart-line"), " Variance Stabilization Normalization"
               ),
               div(style = "overflow-x: auto;", DTOutput("vsn_data")), 
               br(), 
               downloadButton("download_vsn_Data", "Download CSV", 
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("loess normalized data",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #27ae60; padding: 8px 12px; margin-bottom: 15px;",
                   icon("chart-line"), " LOESS (Locally Estimated Scatterplot Smoothing)"
               ),
               div(style = "overflow-x: auto;", DTOutput("loess_data")), 
               br(), 
               downloadButton("download_loess_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("rlr normalized data",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #e74c3c; padding: 8px 12px; margin-bottom: 15px;",
                   icon("chart-line"), " Robust Linear Regression"
               ),
               div(style = "overflow-x: auto;", DTOutput("rlr_data")), 
               br(), 
               downloadButton("download_rlr_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      # ===== SIDEBAR SECTION HEADER: IMPUTED DATASETS =====
      h5(style = "margin-top: 20px; margin-bottom: 5px; padding: 6px 8px; background-color: #e8f0f8; border-left: 4px solid #2c6e9e; font-weight: 600; color: #1a5276;",
         icon("microchip"), " NORMALIZED & IMPUTED DATASETS"),
      
      tabPanel("vsn_knn",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #3498db; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " VSN + K-Nearest Neighbors"
               ),
               div(style = "overflow-x: auto;", DTOutput("vsn_knn_data")), 
               br(), 
               downloadButton("download_vsn_knn_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("vsn_lls",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #3498db; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " VSN + Local Least Squares"
               ),
               div(style = "overflow-x: auto;", DTOutput("vsn_lls_data")), 
               br(), 
               downloadButton("download_vsn_lls_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("vsn_svd",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #3498db; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " VSN + Singular Value Decomposition"
               ),
               div(style = "overflow-x: auto;", DTOutput("vsn_svd_data")), 
               br(), 
               downloadButton("download_vsn_svd_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("loess_knn",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #27ae60; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " LOESS + K-Nearest Neighbors"
               ),
               div(style = "overflow-x: auto;", DTOutput("loess_knn_data")), 
               br(), 
               downloadButton("download_loess_knn_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("loess_lls",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #27ae60; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " LOESS + Local Least Squares"
               ),
               div(style = "overflow-x: auto;", DTOutput("loess_lls_data")), 
               br(), 
               downloadButton("download_loess_lls_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("loess_svd",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #27ae60; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " LOESS + Singular Value Decomposition"
               ),
               div(style = "overflow-x: auto;", DTOutput("loess_svd_data")), 
               br(), 
               downloadButton("download_loess_svd_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("rlr_knn",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #e74c3c; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " RLR + K-Nearest Neighbors"
               ),
               div(style = "overflow-x: auto;", DTOutput("rlr_knn_data")), 
               br(), 
               downloadButton("download_rlr_knn_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("rlr_lls",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #e74c3c; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " RLR + Local Least Squares"
               ),
               div(style = "overflow-x: auto;", DTOutput("rlr_lls_data")), 
               br(), 
               downloadButton("download_rlr_lls_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;")),
      
      tabPanel("rlr_svd",
               br(),
               div(style = "background-color: #f8f9fa; border-left: 3px solid #e74c3c; padding: 8px 12px; margin-bottom: 15px;",
                   icon("project-diagram"), " RLR + Singular Value Decomposition"
               ),
               div(style = "overflow-x: auto;", DTOutput("rlr_svd_data")), 
               br(), 
               downloadButton("download_rlr_svd_Data", "Download CSV",
                              class = "btn btn-default btn-sm",
                              style = "background-color: #f8f9fa; border: 1px solid #ccc;"))
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
  
  # Differential Expression Analysis Tab
  tabPanel(
    HTML('<p style="font-size:16px;"> Differential expression analysis </p>'),
    value = "de_analysis",
    tabsetPanel(
      tabPanel(HTML('<p style="font-size: 18px; color: #ffbf00;"><strong>📈 MA plot</strong></p>'),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              style = "background: rgba(255,255,255,0.9); border-radius: 12px; padding: 15px;",
                              h4(icon("chart-line"), "MA Plot", style = "color: #c0362c;"),
                              selectInput("combination_ma", HTML('<p style="font-size:14px; color:#49796b;">🔧 Choose combination</p>'),
                                          c("vsn_knn", "vsn_lls", "vsn_svd", "loess_knn", "loess_lls", "loess_svd",
                                            "rlr_knn", "rlr_lls", "rlr_svd", "Original_data")),
                              HTML('<p style="font-size: 14px; color: #5a4fcf;">📊 The number of groups in the dataset is:</p>'),
                              verbatimTextOutput("gr_num_ma"),
                              hr(),
                              HTML('<p style="font-size: 14px; color: #5a4fcf;">⚖️ Pairwise DE analysis values (Ex:2 vs 1)</p>'),
                              numericInput("ch_gr1_ma", "Test group value", value = 0, step = 1),
                              numericInput("ch_gr2_ma", "Control group value", value = 0, step = 1),
                              checkboxInput("usevars_ma", HTML('<span style="color: #cc0000;">🎯 Override cut-off limits and p-value</span>'), value = FALSE),
                              conditionalPanel(
                                condition = "input.usevars_ma == true",
                                numericInput("x1_ma", "Cut-off for down-regulated (logFC ≤)", value = -1, step = 0.5),
                                numericInput("x2_ma", "Cut-off for up-regulated (logFC ≥)", value = 1, step = 0.5),
                                numericInput("p_ma", "Significance threshold (p-value)", value = 0.05, step = 0.01)
                              ),
                              actionButton("btn_ma", "🎨 Generate Plot", class = "btn btn-success btn-block btn-action", icon = icon("play")),
                              br(),
                              p("💡 Tip: Adjust cut-offs to identify significant proteins", style = "font-size: 11px; color: #666; text-align: center;")
                 ),
                 mainPanel(
                   withSpinner(plotlyOutput("plot_ma", height = "550px"), color = "#c0362c"),
                   br(),
                   tabsetPanel(
                     tabPanel("📋 Results", DTOutput("result_ma"), downloadButton("download_result_ma", "💾 Download")),
                     tabPanel("⬆️ Up-Regulated", DTOutput("upreg_ma"), downloadButton("download_upreg_ma", "💾 Download")),
                     tabPanel("⬇️ Down-Regulated", DTOutput("downreg_ma"), downloadButton("download_downreg_ma", "💾 Download")),
                     tabPanel("⚪ Non-significant", DTOutput("nonsignif_ma"), downloadButton("download_nonsignif_ma", "💾 Download"))
                   )
                 )
               )
      ),
      tabPanel(HTML('<p style="font-size: 18px; color: #ffbf00;"><strong>🌋 Volcano plot</strong></p>'),
               sidebarLayout(
                 sidebarPanel(width = 3,
                              style = "background: rgba(255,255,255,0.9); border-radius: 12px; padding: 15px;",
                              h4(icon("chart-area"), "Volcano Plot", style = "color: #c0362c;"),
                              selectInput("combination_volcano", HTML('<p style="font-size:14px; color:#49796b;">🔧 Choose combination</p>'),
                                          c("vsn_knn", "vsn_lls", "vsn_svd", "loess_knn", "loess_lls", "loess_svd",
                                            "rlr_knn", "rlr_lls", "rlr_svd", "Original_data")),
                              HTML('<p style="font-size: 14px; color: #5a4fcf;">📊 The number of groups in the dataset is:</p>'),
                              verbatimTextOutput("gr_num_volcano"),
                              hr(),
                              HTML('<p style="font-size: 14px; color: #5a4fcf;">⚖️ Pairwise DE analysis values (Ex:2 vs 1)</p>'),
                              numericInput("ch_gr1_volcano", "Test group value", value = 0, step = 1),
                              numericInput("ch_gr2_volcano", "Control group value", value = 0, step = 1),
                              checkboxInput("usevars_volcano", HTML('<span style="color: #cc0000;">🎯 Override cut-off limits and p-value</span>'), value = FALSE),
                              conditionalPanel(
                                condition = "input.usevars_volcano == true",
                                numericInput("x1_volcano", "Cut-off for down-regulated (logFC ≤)", value = -1, step = 0.5),
                                numericInput("x2_volcano", "Cut-off for up-regulated (logFC ≥)", value = 1, step = 0.5),
                                numericInput("p_volcano", "Significance threshold (p-value)", value = 0.05, step = 0.01)
                              ),
                              actionButton("btn_volcano", "🎨 Generate Plot", class = "btn btn-success btn-block btn-action", icon = icon("play"))
                 ),
                 mainPanel(
                   withSpinner(plotlyOutput("plot_volcano", height = "550px"), color = "#c0362c"),
                   br(),
                   tabsetPanel(
                     tabPanel("📋 Results", DTOutput("result_volcano"), downloadButton("download_result_volcano", "💾 Download")),
                     tabPanel("⬆️ Up-Regulated", DTOutput("upreg_volcano"), downloadButton("download_upreg_volcano", "💾 Download")),
                     tabPanel("⬇️ Down-Regulated", DTOutput("downreg_volcano"), downloadButton("download_downreg_volcano", "💾 Download")),
                     tabPanel("⚪ Non-significant", DTOutput("nonsignif_volcano"), downloadButton("download_nonsignif_volcano", "💾 Download"))
                   )
                 )
               )
      )
    )
  ),
  
  # User Manual Tab
  tabPanel(
    title = HTML('<p style="font-size:16px;"> 📖 User Manual </p>'),
    value = "manual",
    div(
      style = "position: relative;",
      div(
        style = "position: absolute; top: 10px; right: 10px; z-index: 10;",
        downloadButton("downloadManualPDF", "📥 Download User Manual (HTML)", class = "btn-info btn-sm")
      ),
      tags$iframe(
        src = "User_manual.html",
        style = "width: 100%; height: 800px; border: none; border-radius: 8px;"
      )
    )
  ),
  
  # Team & Contact Info Tab
  tabPanel(
    title = HTML('<p style="font-size:16px;"> 👥 Team & Contact Info </p>'),
    value = "team",
    fluidPage(
      fluidRow(
        column(width = 2, align = "center", img(src = "icar_logo.jpg", height = 130, width = 130, style = "border-radius: 12px;")),
        column(width = 8, align = "center",
               HTML('<p style="font-size: 32px; color: #da9100; margin-bottom: 0px;"><strong>Division of Agricultural Bioinformatics</strong></p>'),
               HTML('<p style="font-size: 26px; color: #85754e; margin-bottom: 0px;"><strong>ICAR - Indian Agricultural Statistics Research Institute (IASRI)</strong></p>'),
               HTML('<p style="font-size: 22px; color: #85754e;"><strong>New Delhi, India</strong></p>')),
        column(width = 2, align = "center", img(src = "iasri_logo.png", height = 130, width = 130, style = "border-radius: 12px;"))
      )
    ),
    fluidPage(
      fluidRow(
        tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")),
        column(width = 1),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "kabilan.JPG", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Sakthivel Kabilan</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Ph.D. Bioinformatics</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> kabilan151414@gmail.com</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "sb_lal_sir.JPG", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Shashi Bhushan Lal</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Principal Scientist</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> sb.lal@icar.gov.in</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "sudhir_sir.JPG", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Sudhir Srivastava</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Senior Scientist</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> Sudhir.Srivastava@icar.gov.in</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "kkcsir.JPG", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Krishna Kumar Chaturvedi</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Principal Scientist</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> kk.chaturvedi@icar.gov.in</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "Mishra Sir.jpg", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Dwijesh Chandra Mishra</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Senior Scientist</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> dwij.mishra@gmail.com</p>'))),
        column(width = 1)
      ),
      
      fluidRow(
        column(width = 2),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "yasin mam.JPG", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Yasin Jeshima K</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Senior Scientist</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> yasinlab1.icar@gmail.com</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "rama_sir.jpg", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Vaidhyanathan Ramasubramanian</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Principal Scientist</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> R.Subramanian@icar.gov.in</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "girish_jha_sir.png", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Girish Kumar Jha</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Professor</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> girish.jha@icar.gov.in</p>'))),
        column(width = 2, align = "center", 
               div(class = "info-card", style = "padding: 10px;",
                   tags$img(src = "sharan_photo.jpg", height = 180, width = 140, style = "border-radius: 12px; border: 2px solid #3d0c02;"),
                   HTML('<p style="font-size:16px; margin-top: 8px; margin-bottom: 2px;"><strong>Dr. Sharanbasappa</strong></p>'),
                   HTML('<p style="font-size:13px; margin-bottom: 2px;">Project Scientist - I</p>'),
                   HTML('<p style="font-size:11px;"><i class="fas fa-envelope"></i> smadival509@gmail.com</p>'))),
        column(width = 2)
      ),
      
      HTML('
        <div style="text-align: center; font-size: 16px; color: #1a1a1a; font-family: Arial, sans-serif; margin: 30px 0 20px 0; padding: 15px; background: linear-gradient(135deg, #f5f5f5, #e8e8e8); border-radius: 12px;">
          <strong>💬 For feedback, bug reports, or suggestions for improvements,</strong><br>
          please contact us through our GitHub page: 
          <a href="https://github.com/kabilansbio" target="_blank" style="color: #007bff; text-decoration: none;">🔗 https://github.com/kabilansbio</a>
        </div>
      ')
    )
  ),
  
  footer = tags$footer(
    HTML('<p style="font-size: 14px; text-align: center; color: #1d2951; font-family: calibri; background: linear-gradient(135deg, #f4f0ec, #e8e4e0); padding: 12px; margin: 0; position: relative; width: 100%;">
         <strong>🔬 Division of Agricultural Bioinformatics, ICAR-Indian Agricultural Statistics Research Institute, New Delhi, India.</strong>
         </p>')
  )
)


#Define server logic
server <- function(input,output, session){
  
  # Helper function for consistent DT options with horizontal scrolling
  get_dt_options <- function(page_length = 10, scroll_y = "400px", dom = 'lfrtip') {
    list(
      pageLength = page_length,
      lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "All")),
      scrollX = TRUE,  # Enable horizontal scrolling
      scrollY = scroll_y,
      scrollCollapse = TRUE,
      searching = TRUE,
      ordering = TRUE,
      paging = TRUE,
      autoWidth = FALSE,  # Allow horizontal scroll instead of auto-width
      dom = dom
    )
  }
  
  pageviews_data <- reactive({
    property_id <- "443474629"  # Replace with your actual GA4 property ID
    data <- ga_data(
      propertyId = property_id,
      metrics = "screenPageViews",
      date_range = c("2020-01-01", "today")  # Fetch data from an early start date
    )
    sum(data$screenPageViews)  # Calculate total pageviews
  })
  
  # Display total page views in the Shiny app
  output$total_pageviews <- renderText({
    paste(pageviews_data())
  })
  # Provide the download for the User Manual (HTML)
  output$downloadManualPDF <- downloadHandler(
    filename = function() {
      "User_Manual_lfproQC.html"
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
    
    dat <- switch(tools::file_ext(input$file1$name),
                  "csv" = read.csv(input$file1$datapath),
                  "xlsx" = read_excel(input$file1$datapath),
                  showModal(modalDialog(
                    title = "Unsupported Format",
                    "Please upload either a .csv or .xlsx file.",
                    easyClose = TRUE
                  ))
    )
    Data(dat)
  })
  
  # Load sample dataset
  observeEvent(input$takeDataset, {
    dat <- read.xlsx(file.path(getwd(), "www/sample_data.xlsx"))
    Data(dat)
  })
  
  # Observe group information upload
  observeEvent(input$file2, {
    req(input$file2)
    
    dat <- switch(tools::file_ext(input$file2$name),
                  "csv" = read.csv(input$file2$datapath),
                  "xlsx" = read_excel(input$file2$datapath),
                  showModal(modalDialog(
                    title = "Unsupported Format",
                    "Please upload either a .csv or .xlsx file.",
                    easyClose = TRUE
                  ))
    )
    DataGroup(dat)
  })
  
  # Load sample group information
  observeEvent(input$takeDataGroup, {
    dat <- read.xlsx(file.path(getwd(), "www/sample_groups.xlsx"))
    DataGroup(dat)
  })
  
  # Render the group information table with horizontal scroll
  output$input_groups <- renderDataTable({
    req(DataGroup())
    datatable(
      DataGroup(),
      options = get_dt_options(page_length = 10, scroll_y = "400px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  result <- reactive({
    req(Data(), DataGroup(), input$data_type)
    
    showNotification("Processing data... This may take a few moments.", 
                     type = "message", duration = NULL, id = "proc_notif")
    
    res <- withProgress(message = 'Computing best combinations...', value = 0, {
      incProgress(0.3, detail = "Normalizing data")
      best_combination(Data(), DataGroup(), input$data_type, input$aggr_method)
    })
    
    removeNotification(id = "proc_notif")
    showNotification("Processing completed successfully!", type = "message", duration = 3)
    res
  })
  
  observeEvent(input$btn_input, {
    shinyjs::show("submit_progress")
    # Force result() to re-run
    result()
    shinyjs::hide("submit_progress")
  })
  
  # Render uploaded proteomics data with horizontal scroll
  output$input_data <- renderDataTable({
    req(Data())
    datatable(
      Data(),
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  # Render rollup protein data with horizontal scroll
  output$rollup_protein <- renderDataTable({
    req(result()$`rollup_protein`)
    datatable(
      result()$`rollup_protein`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  # Download handlers
  output$download_selected_data <- downloadHandler(
    filename = function() { paste("selected_data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(Data(), file, row.names = FALSE) }
  )
  
  output$download_group_data <- downloadHandler(
    filename = function() { paste("group_information-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(DataGroup(), file, row.names = FALSE) }
  )
  
  output$download_rollup_protein <- downloadHandler(
    filename = function() { paste("rollup_protein_data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(result()$`rollup_protein`, file, row.names = FALSE) }
  )
  
  output$downloadManualPDF <- downloadHandler(
    filename = function() { "User_manual.html" },
    content = function(file) { file.copy("www/User_manual.html", file) }
  )
  
  #Best Combinations output
  output$best_combinations <- renderTable({
    req(result()$`Best combinations`)
    result()$`Best combinations`
  }, bordered = TRUE, hover = TRUE, striped = TRUE, align = 'c', width = '100%', 
  na = "-", digits = 4)
  
  # ============ NORMALIZED DATASETS WITH HORIZONTAL SCROLL ============
  
  output$vsn_data <- renderDataTable({
    req(result()$`vsn_data`)
    datatable(
      result()$`vsn_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$loess_data <- renderDataTable({
    req(result()$`loess_data`)
    datatable(
      result()$`loess_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$rlr_data <- renderDataTable({
    req(result()$`rlr_data`)
    datatable(
      result()$`rlr_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  # ============ IMPUTED DATASETS WITH HORIZONTAL SCROLL ============
  
  output$vsn_knn_data <- renderDataTable({
    req(result()$`vsn_knn_data`)
    datatable(
      result()$`vsn_knn_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$vsn_lls_data <- renderDataTable({
    req(result()$`vsn_lls_data`)
    datatable(
      result()$`vsn_lls_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$vsn_svd_data <- renderDataTable({
    req(result()$`vsn_svd_data`)
    datatable(
      result()$`vsn_svd_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$loess_knn_data <- renderDataTable({
    req(result()$`loess_knn_data`)
    datatable(
      result()$`loess_knn_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$loess_lls_data <- renderDataTable({
    req(result()$`loess_lls_data`)
    datatable(
      result()$`loess_lls_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$loess_svd_data <- renderDataTable({
    req(result()$`loess_svd_data`)
    datatable(
      result()$`loess_svd_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$rlr_knn_data <- renderDataTable({
    req(result()$`rlr_knn_data`)
    datatable(
      result()$`rlr_knn_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$rlr_lls_data <- renderDataTable({
    req(result()$`rlr_lls_data`)
    datatable(
      result()$`rlr_lls_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  output$rlr_svd_data <- renderDataTable({
    req(result()$`rlr_svd_data`)
    datatable(
      result()$`rlr_svd_data`,
      options = get_dt_options(page_length = 15, scroll_y = "500px"),
      class = "display compact stripe hover",
      rownames = FALSE
    )
  })
  
  # ============ EVALUATION METRICS TABLES (compact, no pagination needed) ============
  
  output$pcv_result <- renderDataTable({
    req(result()$`PCV Result`)
    datatable(
      result()$`PCV Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE  # Enable horizontal scroll for wide tables
      ),
      class = "compact stripe hover"
    )
  })
  
  output$pev_result <- renderDataTable({
    req(result()$`PEV Result`)
    datatable(
      result()$`PEV Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      ),
      class = "compact stripe hover"
    )
  })
  
  output$pmad_result <- renderDataTable({
    req(result()$`PMAD Result`)
    datatable(
      result()$`PMAD Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      ),
      class = "compact stripe hover"
    )
  })
  
  output$nrmse_result <- renderDataTable({
    req(result()$`NRMSE Result`)
    datatable(
      result()$`NRMSE Result`,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      ),
      class = "compact stripe hover"
    )
  })
  
  # Toggle handlers for collapsible metric sections
  observeEvent(input$toggle_pcv, {
    shinyjs::toggle("pcv_container", anim = TRUE, animType = "slide")
  })
  
  observeEvent(input$toggle_pev, {
    shinyjs::toggle("pev_container", anim = TRUE, animType = "slide")
  })
  
  observeEvent(input$toggle_pmad, {
    shinyjs::toggle("pmad_container", anim = TRUE, animType = "slide")
  })
  
  observeEvent(input$toggle_nrmse, {
    shinyjs::toggle("nrmse_container", anim = TRUE, animType = "slide")
  })
  
  # ============ DOWNLOAD HANDLERS ============
  
  output$download_vsn_Data <- downloadHandler(
    filename = function(){ paste("vsn-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`vsn_data`, file, row.names = FALSE) }
  )
  
  output$download_loess_Data <- downloadHandler(
    filename = function(){ paste("loess-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`loess_data`, file, row.names = FALSE) }
  )
  
  output$download_rlr_Data <- downloadHandler(
    filename = function(){ paste("rlr-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`rlr_data`, file, row.names = FALSE) }
  )
  
  output$download_vsn_knn_Data <- downloadHandler(
    filename = function(){ paste("vsn_knn-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`vsn_knn_data`, file, row.names = FALSE) }
  )
  
  output$download_vsn_lls_Data <- downloadHandler(
    filename = function(){ paste("vsn_lls-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`vsn_lls_data`, file, row.names = FALSE) }
  )
  
  output$download_vsn_svd_Data <- downloadHandler(
    filename = function(){ paste("vsn_svd-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`vsn_svd_data`, file, row.names = FALSE) }
  )
  
  output$download_loess_knn_Data <- downloadHandler(
    filename = function(){ paste("loess_knn-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`loess_knn_data`, file, row.names = FALSE) }
  )
  
  output$download_loess_lls_Data <- downloadHandler(
    filename = function(){ paste("loess_lls-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`loess_lls_data`, file, row.names = FALSE) }
  )
  
  output$download_loess_svd_Data <- downloadHandler(
    filename = function(){ paste("loess_svd-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`loess_svd_data`, file, row.names = FALSE) }
  )
  
  output$download_rlr_knn_Data <- downloadHandler(
    filename = function(){ paste("rlr_knn-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`rlr_knn_data`, file, row.names = FALSE) }
  )
  
  output$download_rlr_lls_Data <- downloadHandler(
    filename = function(){ paste("rlr_lls-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`rlr_lls_data`, file, row.names = FALSE) }
  )
  
  output$download_rlr_svd_Data <- downloadHandler(
    filename = function(){ paste("rlr_svd-", Sys.Date(), ".csv", sep="") },
    content = function(file){ write.csv(result()$`rlr_svd_data`, file, row.names = FALSE) }
  )
  
  output$download_pcv_result <- downloadHandler(
    filename = function() { paste("pcv_result-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(result()$`PCV Result`, file, row.names = FALSE) }
  )
  
  output$download_pev_result <- downloadHandler(
    filename = function() { paste("pev_result-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(result()$`PEV Result`, file, row.names = FALSE) }
  )
  
  output$download_pmad_result <- downloadHandler(
    filename = function() { paste("pmad_result-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(result()$`PMAD Result`, file, row.names = FALSE) }
  )
  
  output$download_nrmse_result <- downloadHandler(
    filename = function() { paste("nrmse_result-", Sys.Date(), ".csv", sep = "") },
    content = function(file) { write.csv(result()$`NRMSE Result`, file, row.names = FALSE) }
  )
  
  # Toggle visibility functions
  observeEvent(input$toggle_pcv_table, { shinyjs::toggle("pcv_container") })
  observeEvent(input$toggle_pev_table, { shinyjs::toggle("pev_container") })
  observeEvent(input$toggle_pmad_table, { shinyjs::toggle("pmad_container") })
  observeEvent(input$toggle_nrmse_table, { shinyjs::toggle("nrmse_container") })
  
  # ============ DE ANALYSIS TABLES WITH HORIZONTAL SCROLL ============
  
  # MA Plot ----------------------------------------------------------------
  output$gr_num_ma <- renderText({
    req(DataGroup())
    length(unique(DataGroup()$Groups))
  })
  
  comb_data_ma <- reactive({
    req(result(), DataGroup())
    switch(input$combination_ma,
           "vsn_knn" = top_table_fn(result()$vsn_knn_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "vsn_lls" = top_table_fn(result()$vsn_lls_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "vsn_svd" = top_table_fn(result()$vsn_svd_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "loess_knn" = top_table_fn(result()$loess_knn_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "loess_lls" = top_table_fn(result()$loess_lls_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "loess_svd" = top_table_fn(result()$loess_svd_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "rlr_knn" = top_table_fn(result()$rlr_knn_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "rlr_lls" = top_table_fn(result()$rlr_lls_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "rlr_svd" = top_table_fn(result()$rlr_svd_data, DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma),
           "Original_data" = top_table_fn(Data(), DataGroup(), input$ch_gr1_ma, input$ch_gr2_ma)
    )
  })
  
  observeEvent(input$btn_ma, {
    showNotification("Calculating differential expression for MA plot...", type = "message", duration = 2)
  })
  
  results_ma <- reactive({
    req(input$btn_ma, comb_data_ma())
    if (isolate(input$usevars_ma)) {
      MAplot_DE_fn(comb_data_ma(), x1 = input$x1_ma, x2 = input$x2_ma, p = input$p_ma)
    } else {
      MAplot_DE_fn(comb_data_ma())
    }
  })
  
  output$plot_ma <- plotly::renderPlotly({
    req(results_ma())
    results_ma()[["MA Plot"]]
  })
  
  output$result_ma <- renderDataTable({
    res <- req(results_ma())
    validate(need(isTRUE(input$usevars_ma), "'Results' not applicable without x1, x2, and p values"))
    datatable(res[["Result"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  output$upreg_ma <- renderDataTable({
    res <- req(results_ma())
    validate(need(isTRUE(input$usevars_ma), "'Up-Regulated' not applicable without x1, x2, and p values"))
    datatable(res[["Up-regulated"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  output$downreg_ma <- renderDataTable({
    res <- req(results_ma())
    validate(need(input$usevars_ma, "'Down-Regulated' not applicable without x1, x2, and p values"))
    datatable(res[["Down-regulated"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  output$nonsignif_ma <- renderDataTable({
    res <- req(results_ma())
    validate(need(input$usevars_ma, "'Non-significant' not applicable without x1, x2, and p values"))
    datatable(res[["Non-significant"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  # Volcano Plot ------------------------------------------------------------
  output$gr_num_volcano <- renderText({
    req(DataGroup())
    length(unique(DataGroup()$Groups))
  })
  
  comb_data_volcano <- reactive({
    req(result(), DataGroup())
    switch(input$combination_volcano,
           "vsn_knn" = top_table_fn(result()$vsn_knn_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "vsn_lls" = top_table_fn(result()$vsn_lls_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "vsn_svd" = top_table_fn(result()$vsn_svd_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "loess_knn" = top_table_fn(result()$loess_knn_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "loess_lls" = top_table_fn(result()$loess_lls_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "loess_svd" = top_table_fn(result()$loess_svd_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "rlr_knn" = top_table_fn(result()$rlr_knn_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "rlr_lls" = top_table_fn(result()$rlr_lls_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "rlr_svd" = top_table_fn(result()$rlr_svd_data, DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano),
           "Original_data" = top_table_fn(Data(), DataGroup(), input$ch_gr1_volcano, input$ch_gr2_volcano)
    )
  })
  
  observeEvent(input$btn_volcano, {
    showNotification("Calculating differential expression for Volcano plot...", type = "message", duration = 2)
  })
  
  results_volcano <- reactive({
    req(input$btn_volcano, comb_data_volcano())
    if (isolate(input$usevars_volcano)) {
      volcanoplot_DE_fn(comb_data_volcano(),
                        x1 = input$x1_volcano, x2 = input$x2_volcano, p = input$p_volcano)
    } else {
      volcanoplot_DE_fn(comb_data_volcano())
    }
  })
  
  output$plot_volcano <- plotly::renderPlotly({
    req(results_volcano())
    results_volcano()[["Volcano Plot"]]
  })
  
  output$result_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(need(isTRUE(input$usevars_volcano), "'Results' not applicable without x1, x2, and p values"))
    datatable(res[["Result"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  output$upreg_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(need(isTRUE(input$usevars_volcano), "'Up-Regulated' not applicable without x1, x2, and p values"))
    datatable(res[["Up-regulated"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  output$downreg_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(need(input$usevars_volcano, "'Down-Regulated' not applicable without x1, x2, and p values"))
    datatable(res[["Down-regulated"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  output$nonsignif_volcano <- renderDataTable({
    res <- req(results_volcano())
    validate(need(input$usevars_volcano, "'Non-significant' not applicable without x1, x2, and p values"))
    datatable(res[["Non-significant"]],
              options = get_dt_options(page_length = 15, scroll_y = "400px"),
              class = "display compact stripe hover", rownames = TRUE)
  })
  
  # Download handlers for Volcano plot tables
  output$download_result_volcano <- downloadHandler(
    filename = function() paste("result_volcano-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_volcano()
      write.csv(res[["Result"]], file, row.names = TRUE)
    }
  )
  
  output$download_upreg_volcano <- downloadHandler(
    filename = function() paste("upreg_volcano-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_volcano()
      write.csv(res[["Up-regulated"]], file, row.names = TRUE)
    }
  )
  
  output$download_downreg_volcano <- downloadHandler(
    filename = function() paste("downreg_volcano-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_volcano()
      write.csv(res[["Down-regulated"]], file, row.names = TRUE)
    }
  )
  
  output$download_nonsignif_volcano <- downloadHandler(
    filename = function() paste("nonsignif_volcano-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_volcano()
      write.csv(res[["Non-significant"]], file, row.names = TRUE)
    }
  )
  
  # Download handlers for MA plot tables (add if missing)
  output$download_result_ma <- downloadHandler(
    filename = function() paste("result_ma-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_ma()
      write.csv(res[["Result"]], file, row.names = TRUE)
    }
  )
  
  output$download_upreg_ma <- downloadHandler(
    filename = function() paste("upreg_ma-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_ma()
      write.csv(res[["Up-regulated"]], file, row.names = TRUE)
    }
  )
  
  output$download_downreg_ma <- downloadHandler(
    filename = function() paste("downreg_ma-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_ma()
      write.csv(res[["Down-regulated"]], file, row.names = TRUE)
    }
  )
  
  output$download_nonsignif_ma <- downloadHandler(
    filename = function() paste("nonsignif_ma-", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      res <- results_ma()
      write.csv(res[["Non-significant"]], file, row.names = TRUE)
    }
  )
  
  # Note: The download handler for selected data is already defined earlier in your server.
  # Remove any duplicate `output$download_selected_data` that may appear later.
  
  # ============ BOXPLOT OUTPUTS ============
  output$Boxplot_data_vsn_knn <- renderPlotly({
    req(result()$vsn_knn_data)
    showNotification("Generating Boxplot for VSN+KNN...", type = "message", duration = 1)
    Boxplot_data(result()$vsn_knn_data)
  })
  
  output$Boxplot_data_vsn_lls <- renderPlotly({
    req(result()$vsn_lls_data)
    showNotification("Generating Boxplot for VSN+LLS...", type = "message", duration = 1)
    Boxplot_data(result()$vsn_lls_data)
  })
  
  output$Boxplot_data_vsn_svd <- renderPlotly({
    req(result()$vsn_svd_data)
    showNotification("Generating Boxplot for VSN+SVD...", type = "message", duration = 1)
    Boxplot_data(result()$vsn_svd_data)
  })
  
  output$Boxplot_data_loess_knn <- renderPlotly({
    req(result()$loess_knn_data)
    showNotification("Generating Boxplot for LOESS+KNN...", type = "message", duration = 1)
    Boxplot_data(result()$loess_knn_data)
  })
  
  output$Boxplot_data_loess_lls <- renderPlotly({
    req(result()$loess_lls_data)
    showNotification("Generating Boxplot for LOESS+LLS...", type = "message", duration = 1)
    Boxplot_data(result()$loess_lls_data)
  })
  
  output$Boxplot_data_loess_svd <- renderPlotly({
    req(result()$loess_svd_data)
    showNotification("Generating Boxplot for LOESS+SVD...", type = "message", duration = 1)
    Boxplot_data(result()$loess_svd_data)
  })
  
  output$Boxplot_data_rlr_knn <- renderPlotly({
    req(result()$rlr_knn_data)
    showNotification("Generating Boxplot for RLR+KNN...", type = "message", duration = 1)
    Boxplot_data(result()$rlr_knn_data)
  })
  
  output$Boxplot_data_rlr_lls <- renderPlotly({
    req(result()$rlr_lls_data)
    showNotification("Generating Boxplot for RLR+LLS...", type = "message", duration = 1)
    Boxplot_data(result()$rlr_lls_data)
  })
  
  output$Boxplot_data_rlr_svd <- renderPlotly({
    req(result()$rlr_svd_data)
    showNotification("Generating Boxplot for RLR+SVD...", type = "message", duration = 1)
    Boxplot_data(result()$rlr_svd_data)
  })
  
  output$Boxplot_data_original <- renderPlotly({
    req(Data())
    showNotification("Generating Boxplot for Original Data...", type = "message", duration = 1)
    Boxplot_data(Data())
  })
  
  # ============ DENSITY PLOT OUTPUTS ============
  output$Densityplot_data_vsn_knn <- renderPlotly({
    req(result()$vsn_knn_data)
    showNotification("Generating Density Plot for VSN+KNN...", type = "message", duration = 1)
    Densityplot_data(result()$vsn_knn_data)
  })
  
  output$Densityplot_data_vsn_lls <- renderPlotly({
    req(result()$vsn_lls_data)
    showNotification("Generating Density Plot for VSN+LLS...", type = "message", duration = 1)
    Densityplot_data(result()$vsn_lls_data)
  })
  
  output$Densityplot_data_vsn_svd <- renderPlotly({
    req(result()$vsn_svd_data)
    showNotification("Generating Density Plot for VSN+SVD...", type = "message", duration = 1)
    Densityplot_data(result()$vsn_svd_data)
  })
  
  output$Densityplot_data_loess_knn <- renderPlotly({
    req(result()$loess_knn_data)
    showNotification("Generating Density Plot for LOESS+KNN...", type = "message", duration = 1)
    Densityplot_data(result()$loess_knn_data)
  })
  
  output$Densityplot_data_loess_lls <- renderPlotly({
    req(result()$loess_lls_data)
    showNotification("Generating Density Plot for LOESS+LLS...", type = "message", duration = 1)
    Densityplot_data(result()$loess_lls_data)
  })
  
  output$Densityplot_data_loess_svd <- renderPlotly({
    req(result()$loess_svd_data)
    showNotification("Generating Density Plot for LOESS+SVD...", type = "message", duration = 1)
    Densityplot_data(result()$loess_svd_data)
  })
  
  output$Densityplot_data_rlr_knn <- renderPlotly({
    req(result()$rlr_knn_data)
    showNotification("Generating Density Plot for RLR+KNN...", type = "message", duration = 1)
    Densityplot_data(result()$rlr_knn_data)
  })
  
  output$Densityplot_data_rlr_lls <- renderPlotly({
    req(result()$rlr_lls_data)
    showNotification("Generating Density Plot for RLR+LLS...", type = "message", duration = 1)
    Densityplot_data(result()$rlr_lls_data)
  })
  
  output$Densityplot_data_rlr_svd <- renderPlotly({
    req(result()$rlr_svd_data)
    showNotification("Generating Density Plot for RLR+SVD...", type = "message", duration = 1)
    Densityplot_data(result()$rlr_svd_data)
  })
  
  output$Densityplot_data_original <- renderPlotly({
    req(Data())
    showNotification("Generating Density Plot for Original Data...", type = "message", duration = 1)
    Densityplot_data(Data())
  })
  
  # ============ CORRELATION HEATMAP OUTPUTS ============
  output$Corrplot_data_vsn_knn <- renderPlotly({
    req(result()$vsn_knn_data)
    showNotification("Generating Correlation Heatmap for VSN+KNN...", type = "message", duration = 1)
    Corrplot_data(result()$vsn_knn_data)
  })
  
  output$Corrplot_data_vsn_lls <- renderPlotly({
    req(result()$vsn_lls_data)
    showNotification("Generating Correlation Heatmap for VSN+LLS...", type = "message", duration = 1)
    Corrplot_data(result()$vsn_lls_data)
  })
  
  output$Corrplot_data_vsn_svd <- renderPlotly({
    req(result()$vsn_svd_data)
    showNotification("Generating Correlation Heatmap for VSN+SVD...", type = "message", duration = 1)
    Corrplot_data(result()$vsn_svd_data)
  })
  
  output$Corrplot_data_loess_knn <- renderPlotly({
    req(result()$loess_knn_data)
    showNotification("Generating Correlation Heatmap for LOESS+KNN...", type = "message", duration = 1)
    Corrplot_data(result()$loess_knn_data)
  })
  
  output$Corrplot_data_loess_lls <- renderPlotly({
    req(result()$loess_lls_data)
    showNotification("Generating Correlation Heatmap for LOESS+LLS...", type = "message", duration = 1)
    Corrplot_data(result()$loess_lls_data)
  })
  
  output$Corrplot_data_loess_svd <- renderPlotly({
    req(result()$loess_svd_data)
    showNotification("Generating Correlation Heatmap for LOESS+SVD...", type = "message", duration = 1)
    Corrplot_data(result()$loess_svd_data)
  })
  
  output$Corrplot_data_rlr_knn <- renderPlotly({
    req(result()$rlr_knn_data)
    showNotification("Generating Correlation Heatmap for RLR+KNN...", type = "message", duration = 1)
    Corrplot_data(result()$rlr_knn_data)
  })
  
  output$Corrplot_data_rlr_lls <- renderPlotly({
    req(result()$rlr_lls_data)
    showNotification("Generating Correlation Heatmap for RLR+LLS...", type = "message", duration = 1)
    Corrplot_data(result()$rlr_lls_data)
  })
  
  output$Corrplot_data_rlr_svd <- renderPlotly({
    req(result()$rlr_svd_data)
    showNotification("Generating Correlation Heatmap for RLR+SVD...", type = "message", duration = 1)
    Corrplot_data(result()$rlr_svd_data)
  })
  
  output$Corrplot_data_original <- renderPlotly({
    req(Data())
    showNotification("Generating Correlation Heatmap for Original Data...", type = "message", duration = 1)
    Corrplot_data(Data())
  })
  
  # ============ QQ PLOT OUTPUTS ============
  output$QQplot_data_vsn_knn <- renderPlotly({
    req(result()$vsn_knn_data)
    showNotification("Generating Q-Q Plot for VSN+KNN...", type = "message", duration = 1)
    QQplot_data(result()$vsn_knn_data)
  })
  
  output$QQplot_data_vsn_lls <- renderPlotly({
    req(result()$vsn_lls_data)
    showNotification("Generating Q-Q Plot for VSN+LLS...", type = "message", duration = 1)
    QQplot_data(result()$vsn_lls_data)
  })
  
  output$QQplot_data_vsn_svd <- renderPlotly({
    req(result()$vsn_svd_data)
    showNotification("Generating Q-Q Plot for VSN+SVD...", type = "message", duration = 1)
    QQplot_data(result()$vsn_svd_data)
  })
  
  output$QQplot_data_loess_knn <- renderPlotly({
    req(result()$loess_knn_data)
    showNotification("Generating Q-Q Plot for LOESS+KNN...", type = "message", duration = 1)
    QQplot_data(result()$loess_knn_data)
  })
  
  output$QQplot_data_loess_lls <- renderPlotly({
    req(result()$loess_lls_data)
    showNotification("Generating Q-Q Plot for LOESS+LLS...", type = "message", duration = 1)
    QQplot_data(result()$loess_lls_data)
  })
  
  output$QQplot_data_loess_svd <- renderPlotly({
    req(result()$loess_svd_data)
    showNotification("Generating Q-Q Plot for LOESS+SVD...", type = "message", duration = 1)
    QQplot_data(result()$loess_svd_data)
  })
  
  output$QQplot_data_rlr_knn <- renderPlotly({
    req(result()$rlr_knn_data)
    showNotification("Generating Q-Q Plot for RLR+KNN...", type = "message", duration = 1)
    QQplot_data(result()$rlr_knn_data)
  })
  
  output$QQplot_data_rlr_lls <- renderPlotly({
    req(result()$rlr_lls_data)
    showNotification("Generating Q-Q Plot for RLR+LLS...", type = "message", duration = 1)
    QQplot_data(result()$rlr_lls_data)
  })
  
  output$QQplot_data_rlr_svd <- renderPlotly({
    req(result()$rlr_svd_data)
    showNotification("Generating Q-Q Plot for RLR+SVD...", type = "message", duration = 1)
    QQplot_data(result()$rlr_svd_data)
  })
  
  output$QQplot_data_original <- renderPlotly({
    req(Data())
    showNotification("Generating Q-Q Plot for Original Data...", type = "message", duration = 1)
    QQplot_data(Data())
  })
  
  # ============ MDS PLOT OUTPUTS ============
  output$MDSplot_data_vsn_knn <- renderPlotly({
    req(result()$vsn_knn_data)
    showNotification("Generating MDS Plot for VSN+KNN...", type = "message", duration = 1)
    MDSplot_data(result()$vsn_knn_data)
  })
  
  output$MDSplot_data_vsn_lls <- renderPlotly({
    req(result()$vsn_lls_data)
    showNotification("Generating MDS Plot for VSN+LLS...", type = "message", duration = 1)
    MDSplot_data(result()$vsn_lls_data)
  })
  
  output$MDSplot_data_vsn_svd <- renderPlotly({
    req(result()$vsn_svd_data)
    showNotification("Generating MDS Plot for VSN+SVD...", type = "message", duration = 1)
    MDSplot_data(result()$vsn_svd_data)
  })
  
  output$MDSplot_data_loess_knn <- renderPlotly({
    req(result()$loess_knn_data)
    showNotification("Generating MDS Plot for LOESS+KNN...", type = "message", duration = 1)
    MDSplot_data(result()$loess_knn_data)
  })
  
  output$MDSplot_data_loess_lls <- renderPlotly({
    req(result()$loess_lls_data)
    showNotification("Generating MDS Plot for LOESS+LLS...", type = "message", duration = 1)
    MDSplot_data(result()$loess_lls_data)
  })
  
  output$MDSplot_data_loess_svd <- renderPlotly({
    req(result()$loess_svd_data)
    showNotification("Generating MDS Plot for LOESS+SVD...", type = "message", duration = 1)
    MDSplot_data(result()$loess_svd_data)
  })
  
  output$MDSplot_data_rlr_knn <- renderPlotly({
    req(result()$rlr_knn_data)
    showNotification("Generating MDS Plot for RLR+KNN...", type = "message", duration = 1)
    MDSplot_data(result()$rlr_knn_data)
  })
  
  output$MDSplot_data_rlr_lls <- renderPlotly({
    req(result()$rlr_lls_data)
    showNotification("Generating MDS Plot for RLR+LLS...", type = "message", duration = 1)
    MDSplot_data(result()$rlr_lls_data)
  })
  
  output$MDSplot_data_rlr_svd <- renderPlotly({
    req(result()$rlr_svd_data)
    showNotification("Generating MDS Plot for RLR+SVD...", type = "message", duration = 1)
    MDSplot_data(result()$rlr_svd_data)
  })
  
  output$MDSplot_data_original <- renderPlotly({
    req(Data())
    showNotification("Generating MDS Plot for Original Data...", type = "message", duration = 1)
    MDSplot_data(Data())
  })
}

#Run the app

shinyApp(ui=ui, server=server)
 
