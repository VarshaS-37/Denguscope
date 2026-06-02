library(shiny)
library(shinyWidgets)
library(DT)
library(shinycssloaders)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(enrichplot)

##########################################################################
ui <- tagList(
  tags$head(
    tags$meta(name="viewport", content="width=1200"),
    tags$link(
      href = "https://fonts.googleapis.com/css2?family=Montserrat:wght@300;400;500;700&family=Pacifico&display=swap",
      rel = "stylesheet"
    ),
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        min-width: 1200px;
        font-family: 'Montserrat', sans-serif;
        background-color: #f3f5fd; /* fallback behind image */
        
        background-position: center center;
      }
      body, button, input, select, textarea {
        font-family: 'Montserrat', sans-serif;
      }
      .main-container, .plotly {
        font-family: 'Montserrat', sans-serif !important;
      }
      #main-title {
        text-align: center;
        font-size: 48px;
        margin-top: 20px;
        margin-bottom: 10px;
        color: #325175; /* title color */
        position: relative;
        z-index: 2;
      }
      /* Custom font for Dengoscope title */
      #main-title span.dengoscope {
        font-family: 'Pacifico', cursive;
      }
      /* Navbar styles */
      .navbar-default {
        background-color: #92b5d4 !important;  
      }
      .navbar-default .navbar-nav > li > a {
        color: white !important;               
      }
      .navbar-default .navbar-nav > .active > a {
        background-color: #325175 !important;
        color: white !important;
      }
      .navbar-default .navbar-nav > li:not(.active) > a:hover {
        background-color: #688fb3 !important;  
        color: white !important;
      }
      .nav-tabs > li > a {
        color: white;
        font-weight: 600;
        padding: 10px 18px;
        margin-right: 5px;
        background-color: #8dabce;
      }
      .nav-tabs > li > a:hover {
        background-color: #A5B9C9;
        color: #1d3f58;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background-color: #325175;
        color: white;
      }
      
      /* Main container for all content */
      .main-container {
        position: relative;
        z-index: 1;
        min-height: 100vh;
        display: flex;
        flex-direction: column;
        align-items: center;
      }
    .counts-tabs .nav-tabs {
      display: flex;
      width: 100%;
    }
    
    .counts-tabs .tab-content {
      overflow-x: auto;
    }
    .navbar-nav {
    float: none !important;
    display: flex !important;
    justify-content: center !important; 
     
    }
  
    .navbar-nav > li {
      float: none !important;
      display: inline-block !important;
    }
  
    .navbar-nav > li > a {
    text-align: center;
    }
    
      .home-bg {
      position: absolute;          /* stay fixed behind content */
      top: 0;
      left: 0;
      width: 100%;             /* full width of viewport */
      height: 100vh;            /* full height of viewport */
      background-image: url('bkg.png'); /* image in www folder */
      background-size: cover;   /* scale to fill screen */
      background-position: center; /* center the image */
      background-repeat: no-repeat;
    
      z-index: 0;              /* behind all content */
      display: flex;
      flex-direction: column;
      justify-content: center;  /* vertical centering */
      align-items: center;      /* horizontal centering */
      }
    @media (orientation: portrait) {

  .home-bg {
    background-size: contain;
    background-position: top center;
  }

}
    "))
  ),
  
 
  # Main content container
  div(class = "main-container",
      div(id = "main-title", span("Denguscope", class = "dengoscope")),
      
  div(style = "width: 100%;",
  ##########################################################################
  navbarPage(
    title = "", 
    tabPanel(
      "About",
      div(class = "home-bg",
      div(
        style="
      min-height:90vh;
      display:flex;
      flex-direction:column;
      justify-content:flex-start;
      align-items:center;
      text-align:center;
      padding-top:150px;
      color:#325175;
      font-size:18px;
      line-height:1.8;
    ",
        h2("Welcome to Denguscope",
           style="font-family:'Pacifico', cursive;"),
        br(),
        p(
          HTML("
      🧬 An interactive R Shiny App displaying differential expression
      analysis of <b>RNA-Seq data from 73 dengue patient samples</b> obtained from ENA
      <a href='https://www.ebi.ac.uk/ena/browser/view/PRJNA1279769' target='_blank' style='color:#2a6ebb;'>(link)</a>.
      ")
        ),
        p("
      🧬 Toggle between tabs to explore different plots, hover over points to view details, and modify parameters to dynamically update the visualizations.
    "),
        p("
      🧬 Blue hyperlinks will take you to external databases and relevant websites.
    "),
        p("Happy exploring 🔍 !",
          style="font-family:'Pacifico', cursive; font-size:22px;"),
     br(),
     br(),
        HTML("
      <span style='font-family:Pacifico; color:#325175; font-size:20px;'>Created by Varsha --> <a href='https://github.com/VarshaS-37/Dengoscope' target='_blank' style='color:#2a6ebb;'>GitHub</a>
      </span>
    "))
      
    ))
    ,
    ##########################################################################
    tabPanel("Metadata",
             fluidPage(
               h3("Metadata of the samples",style = "text-align: center; font-style: italic;"),
               withSpinner(div(style = "display: flex; justify-content: center;",
                   DT::DTOutput("csvTable", width = "90%", height = "400px")
               ),type = 6,
               color = "#325175")
             )
    ),
    ##########################################################################
    tabPanel("Data Overview",
             fluidPage(
               h3(
                 "Distribution of samples across the selected variable on the left",
                 style = "text-align: center; font-style: italic;"
               ),
               sidebarLayout(
                 sidebarPanel(
                   h4("Select the variable of interest:"),
                   radioButtons(
                     inputId = "vars_selected",
                     label = NULL,
                     choices = c(
                       "Age",
                       "Platelet_Count",
                       "Sample_Type",
                       "Gender"
                     ),
                     selected = "Sample_Type"
                   ),
                   width = 2
                 ),
                 mainPanel(
                   div(
                     style = "width: 600px; margin-left: auto; margin-right: auto;",
                     withSpinner(
                       plotlyOutput("piePlot", width = "500px", height = "500px"),
                       type = 6,
                       color = "#325175"
                     )
                   ),
                   uiOutput("analysis_ui")
                 )
               )
             )
    ) ,
    ##################################################################################
    tabPanel(
      "QC",
      fluidPage(
       
    div(class = "counts-tabs",
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Raw Read Counts",
            br(),
            withSpinner(
              plotlyOutput("raw_counts_plot", width="2000px", height="700px"),
              type = 6, color = "#325175"
            )
          ),
          tabPanel(
            "VST Normalized Counts",
            br(),
            withSpinner(
              plotlyOutput("vst_counts_plot", width="2000px", height="700px"),
              type = 6, color = "#325175"
            )
          ),
          tabPanel(
            "PCA Plot",
            br(),
            withSpinner(
              plotlyOutput("pca_plot", width="1500px", height="900px"),
              type = 6, color = "#325175"
            )
          ),
          tabPanel(
            "Sample Distance Heatmap",
            br(),
            withSpinner(
              plotlyOutput("sample_heatmap", width="1500px", height="900px"),
              type = 6, color = "#325175"
            )
          ),
          tabPanel(
            "Gene Type Distribution",
            br(),
            withSpinner(
              plotlyOutput("gene_type_plot", width="1500px", height="900px"),
              type = 6, color = "#325175"
            )
          )
        )
      ))
    ),
    ##########################################################################
    
    tabPanel("Gene Expression",
             fluidPage(
               div(
                 style = "overflow-x:auto; width:100%;",
                 h3(
                   "Expression of genes across samples or sample groups",
                   style = "text-align: center; font-style: italic;"
                 ),
                 br(),
               uiOutput("gene_boxplot_title"),
               sidebarLayout(
                 sidebarPanel(
                   h4("Select 1 or multiple genes:"),
                   pickerInput(
                     inputId = "selected_gene",
                     label = NULL,
                     choices = NULL,
                     multiple = TRUE,
                     selected = NULL,
                     options = list(
                       `actions-box` = FALSE,
                       `live-search` = TRUE
                     )
                   ),
                   br(),
                   radioButtons(
                     inputId = "plot_type",
                     label = "Select plot type:",
                     choices = c(
                       "Sample Groups" = "groups",
                       "Individual Samples" = "samples"
                     ),
                     selected = "groups"
                   ),
                   width = 2
                 ),
                 mainPanel(
                   fluidRow(
                     column(
                       width = 12,
                       withSpinner(
                         plotlyOutput("gene_boxplot", width = "150%", height = "600px"),
                         type = 6, color = "#325175"
                       )
                     )
                   )
                 )
               )
             ))
    ),
    #########################################################
    tabPanel("Gene Expression Correlation",
             fluidPage(
               h3(
                 "Heatmap representing the expression correlation among selected genes",
                 style = "text-align: center; font-style: italic;"
               ),
               br(),
               sidebarLayout(
                 sidebarPanel(
                   h4("Select 1 or multiple genes:"),
                   pickerInput(
                     inputId = "selected_gene_2",
                     label = NULL,
                     choices = NULL,
                     multiple = TRUE,
                     selected = NULL,
                     options = list(
                       `actions-box` = FALSE,
                       `live-search` = TRUE
                     )
                   ),
                   radioButtons(
                     inputId = "group_option",
                     label = "Select samples to include:",
                     choices = c(
                       "All samples" = "all",
                       "Severe" = "Severe",
                       "Non-Severe" = "Non-Severe",
                       "Negative" = "Negative"
                     ),
                     selected = "all"
                   ),
                   width = 2
                 ),
                 mainPanel(
                   fluidRow(
                     column(
                       width = 12,
                       align = "center",
                       withSpinner(
                         plotOutput("gene_corr_heatmap", width = "700px", height = "700px"),
                         type = 6,
                         color = "#325175"
                       )
                     )
                   )
                 )
               )
             )
    ),
    #############################################################################
    tabPanel(
      "MA Plot",
      fluidPage(
        h3(
          "Scatter plot highlighting differentially expressed genes",
          style = "text-align: center; font-style: italic; margin-bottom: 30px;"
        ),
        fluidRow(
          column(
            width = 12,
            withSpinner(
              plotlyOutput("ma_plot", height = "650px"),
              type = 6,
              color = "#325175"
            )
          )
        )
      )
    ),
    ##########################################################################
    tabPanel("DE Heatmap",
             fluidPage(
               h3("Heatmap of top differentially expressed genes across samples",style = "text-align: center; font-style: italic;"),
               br(),
               sidebarLayout(
                 sidebarPanel(
                   # Filter choice
                   radioButtons("filter_by", "Filter by:",
                                choices = c("Adjusted p-value" = "padj", 
                                            "Absolute log2FC" = "log2FC"),
                                selected = "padj"),
                   radioButtons(
                     "cluster_option",
                     "Clustering:",
                     choices = c(
                       "No clustering" = "none",
                       "Cluster genes" = "rows",
                       "Cluster samples" = "cols",
                       "Cluster both" = "both"
                     ),
                     selected = "none"
                   ),
                   # Dynamic numeric input for cutoff (typable)
                   uiOutput("cutoff_ui"),
                   # Number of top genes (typable)
                   numericInput("top_n", "Number of top genes:",
                                value = 20, min = 20, max = 100, step = 20)
                 ,width=2),
                 mainPanel(
                   uiOutput("heatmap_ui")
                   )
                 )
               )
             ),
    ###################################################################33
    tabPanel(
      "Dysregulated Genes",
      sidebarLayout(
        sidebarPanel(
          style = "margin-top: 45px;",
          h4("Significance thresholds"),
          numericInput(
            "volcano_padj",
            "Adjusted p-value cutoff:",
            value = 0.05,
            min = 0.001,
            max = 0.1,
            step = 0.001
          ),
          numericInput(
            "volcano_logfc",
            "Absolute log2FC cutoff:",
            value = 1,
            min = 0,
            max = 5,
            step = 0.1
          ),
          width = 2
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Volcano Plot",
              h3("Significantly upregulated and downregulated genes based on selected thresholds",style = "text-align: center; font-style: italic;"),
              withSpinner(
                plotlyOutput("volcano_plot", height = "600px", width="100%"),
                type = 6, color = "#325175"
              )
            ),
            tabPanel(
              "DEG Counts",
              h3("Total number of significantly dysregulated genes based on selcted thresholds",style = "text-align: center; font-style: italic; margin-bottom: 30px;"),
              withSpinner(
                plotlyOutput("deg_barplot", width = "500px", height = "500px"),
                type = 6, color = "#325175"
              )
            ),
            tabPanel(
              "DE Genes",
              br(),
              selectInput(
                "volcano_table_filter",
                "Show genes:",
                choices = c("Upregulated", "Downregulated"),
                selected = "Upregulated",
                width = "250px"
              ),
              br(),
              DT::dataTableOutput("volcano_gene_table")
            )
          )
        )
      )
    )
  ,
  ######################################################################
  tabPanel(
    "Gene Ontology",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          style = "margin-top: 45px;",
          pickerInput(
            inputId = "pathway_type",
            label = "Select Enrichment Type:",
            choices = c(
              "GO Biological Process (BP)" = "bp",
              "GO Molecular Function (MF)" = "mf",
              "GO Cellular Component (CC)" = "cc"
            ),
            selected = "bp",
            options = list(`live-search` = TRUE)
          ),
          numericInput(
            "top_terms",
            "Number of top terms:",
            value = 10,
            min = 10,
            max = 70,
            step = 10
          ),
          width = 2
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Dot Plot",
              h3("Enriched GO terms from DE genes (dot size = gene count, color = significance)",style = "text-align: center; font-style: italic; margin-bottom: 30px;"),
              withSpinner(
                plotlyOutput("pathway_plot", height = "1800px",width="100%"),
                type = 6,
                color = "#325175"
              )
            ),
            tabPanel(
              "GO Terms",
          
                   br(),
                  DT::dataTableOutput("pathway_table")
              )
            )
        )
    )
  )
),
##################################################################
tabPanel(
  "Genes Used in ML Model",
  fluidPage(
    h3(
      "Expression Heatmap of Feature-Reduced Genes Selected for Machine Learning",
      style = "text-align:center; font-style:italic;"
    ),
    br(),
    withSpinner(
      uiOutput(
        "ml_heatmap_ui")
    )
  )
)
)
)
))
########################################################################################
server <- function(input, output, session) {
  
  metadata_once <- readRDS("metadata_with_links.rds")
  DE_df_once <- readRDS("annotated_DE_results.rds")
  
  output$csvTable <- DT::renderDT({
    csvData <- metadata_once
    csvData <- csvData %>%
      rename(ID = Link) 
    colnames(csvData)[colnames(csvData)=="Platelet_Count"] <- "Platelet_Count (10^3/µL)"
    colnames(csvData)[colnames(csvData)=="Total_Leukocyte_Count"] <- "Leukocyte_Count (10^3/µL)"# ensure it's character
    DT::datatable(
      csvData,
      escape = FALSE,  # allow HTML in all columns
      options = list(
        pageLength = 15,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(visible = FALSE, targets = which(names(csvData) == "Sample_ID") - 1)
        )
      ),
      selection = "none",
      rownames = FALSE
    )
  })
  #############################################################################################3
  plotly_montserrat <- function(p, title_text = NULL) {
    if (!is.null(title_text)) {
      p <- p %>% layout(
        title = list(
          text = title_text,
          font = list(family = "Montserrat, sans-serif", size = 18),
          x = 0.5, xanchor = "center"
        )
      )
    }
    p %>% layout(
      font = list(
        family = "Montserrat, sans-serif",
        size = 14,
        color = "#000000"
      )
    )
  }
  #############################################################################################3
  
  output$raw_counts_plot <- renderPlotly({
    raw_plot_df <- readRDS("raw_counts_plot_data.rds")
    plot_ly(
      raw_plot_df,
      x = ~Sample_ID,
      y = ~Counts,
      type = "bar",
      color = ~Sample_Type,
      colors = c(
        "Negative" = "#fc9d9a",
        "Severe" = "#8B4C70",
        "Non-Severe" = "#A5CAD2"
      )
    ) %>%
      plotly_montserrat(title_text = "Raw sequencing counts per sample before normalization") %>%
      layout(
        margin = list(l = 50, r = 20, t = 50, b = 150),
        xaxis = list(title = "Sample", tickangle = -45),
        yaxis = list(title = "Total Reads"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = FALSE)
  })
  #############################################################################################3
 
  output$vst_counts_plot <- renderPlotly({
    vst_plot_df <- readRDS("vst_plot_data.rds")
    sample_colors <- c(
      "Negative" = "#fc9d9a",
      "Severe" = "#8B4C70",
      "Non-Severe" = "#A5CAD2"
    )
    plot_ly(
      vst_plot_df,
      x = ~Sample_ID,
      y = ~Expression,
      type = "box",
      color = ~Sample_Type,
      colors = sample_colors,
      boxpoints = FALSE
    ) %>%
      plotly_montserrat(title_text = "Variance-stabilized normalized counts per Sample") %>%
      layout(
        xaxis = list(title = "Sample", tickangle = -45),
        yaxis = list(title = "VST Expression"),
        margin = list(l = 50, r = 20, t = 50, b = 150),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd"
      ) %>%
      config(displayModeBar = FALSE)
    
  })
  #############################################################################################3
  
  output$pca_plot <- renderPlotly({
    pca_df <- readRDS("pca_plot_data.rds")
    plot_ly(
      pca_df,
      x = ~PC1,
      y = ~PC2,
      type = "scatter",
      mode = "markers",
      color = ~Sample_Type,
      symbol = ~Sample_Type,
      colors = c(
        "Negative" = "#fc9d9a",
        "Severe" = "#8B4C70",
        "Non-Severe" = "#A5CAD2"
      ),
      marker = list(size = 10),
      text = ~paste(
        "Sample:", Sample_ID,
        "<br>Group:", Sample_Type
      ),
      hoverinfo = "text"
    ) %>%
      plotly_montserrat(title_text = "Sample clustering based on gene expression patterns") %>%
      layout(
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2"),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd"
      ) %>%
      config(displayModeBar = FALSE)
  })
  #############################################################################################3
 

  
  output$sample_heatmap <- renderPlotly({
    df <- readRDS("sample_distance_matrix.rds")
    mat <- as.matrix(df)
    color_scale <- list(
      list(0, "#fc9d9a"),   # low
      list(0.5, "white"),   # middle
      list(1, "#8B4C70")    # high
    )
    plot_ly(
      x = colnames(mat),
      y = rownames(mat),
      z = mat,
      type = "heatmap",
      colorscale = color_scale
    ) %>%
      layout(
        title = "Heatmap representing similarity/distance between samples based on expression profiles",
        xaxis = list(title = "Samples"),
        yaxis = list(title = "Samples"),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd",
        font = list(family = "Montserrat")
      ) %>%
      config(displayModeBar = FALSE)
  })
  #############################################################################################3
 
  output$gene_type_plot <- renderPlotly({
    gene_type_summary <- readRDS("gene_type_summary.rds")
    df <- gene_type_summary
    df$Type <- factor(df$Type, levels = df$Type)
    plot_ly(
      df,
      x = ~Count,
      y = ~Type,
      type = "bar",
      orientation = "h",
      text = ~Count,
      textposition = "auto",
      marker = list(color = "#8B4C70")
    ) %>%
      layout(
        title = "Gene Type Distribution",
        margin = list(l = 20, r = 20, t = 50, b = 50),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd",
        showlegend = FALSE,
        font = list(family = "Montserrat", size = 14)
      ) %>%
      config(displayModeBar = FALSE)
  })
 ############################################################################################# 
  age_breaks <- seq(0,100,20)
  platelet_breaks <- seq(0,600,100)
  output$piePlot <- renderPlotly({
    csvData <- metadata_once
    var <- input$vars_selected 
    if (is.null(var) || var == "") {
      plotly_empty(type = "pie") %>%
        layout(
          annotations = list(
            x = 0.5, y = 0.5,
            text = "Select a variable to see pie chart",
            showarrow = FALSE,
            font = list(family="Montserrat, sans-serif", size=20)
          ),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent"
        )
    } else {
      df <- csvData
      if (var %in% c("Age","Platelet_Count")) {
        df[[var]] <- as.numeric(df[[var]])
        breaks <- if (var == "Age") age_breaks else platelet_breaks
        labels <- paste(head(breaks,-1), "-", tail(breaks,-1))
        df$Category <- cut(df[[var]],
                           breaks = breaks,
                           labels = labels,
                           include.lowest = TRUE,
                           right = FALSE)
      } else {
        df$Category <- df[[var]]
      }
      df_plot <- as.data.frame(table(df$Category))
      colnames(df_plot) <- c("Category","Count")
      plot_ly(
        df_plot,
        labels = ~Category,
        values = ~Count,
        type = "pie",
        textinfo = "label+percent",
        text = ~paste(Count,"Samples"),
        hoverinfo = "text",
        marker = list(colors = c("#fc9d9a","#8B4C70","#A5CAD2","#758EB7","#F9AD6A","#FFDCA2")[1:nrow(df_plot)])
      ) %>%
        plotly_montserrat(title_text = paste("Sample distribution by", var)) %>%
        layout(
          margin = list(l=20,r=20,t=50,b=100),
          paper_bgcolor = "transparent",
          plot_bgcolor = "transparent",
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE)
    }
  })
  ##########################################################################################
  norm_counts_top_DE <- readRDS("top100_norm_counts.rds")
  observe({
    req(norm_counts_top_DE)
    updatePickerInput(
      session,
      inputId = "selected_gene",
      choices = rownames(norm_counts_top_DE),
      selected = rownames(norm_counts_top_DE)[1]  # optional default
    )
    updatePickerInput(
      session,
      inputId = "selected_gene_2",
      choices = rownames(norm_counts_top_DE),
      selected = rownames(norm_counts_top_DE)[1:2]
    )
  })
  ##########################################################################################
  # Render boxplot
  output$gene_boxplot_title <- renderUI({
    req(input$selected_gene)
    div(
      style = "text-align:center",
      h4(paste("Expression of:", paste(input$selected_gene, collapse=", ")))
    )
  })
  ########################################################################################3###
  
  output$gene_boxplot <- renderPlotly({
    expr_long <- readRDS("top100_expression_long.rds")
    sample_order <- readRDS("sample_order.rds")$Sample
    req(input$selected_gene)
    df_plot <- expr_long %>%
      dplyr::filter(Gene %in% input$selected_gene)
    color_map <- c(
      "Negative" = "#fc9d9a",
      "Severe" = "#8B4C70",
      "Non-Severe" = "#A5CAD2"
    )
    if(input$plot_type == "groups"){
      
      p <- plot_ly(
        df_plot,
        x = ~Gene,
        y = ~Expression,
        color = ~Condition,
        colors = color_map,
        type = "box",
        boxpoints = "all",
        jitter = 0.3,
        pointpos = 0
      )
    }
   else if(input$plot_type == "samples"){
      df_plot$Sample <- factor(df_plot$Sample, levels = sample_order)
      p <- plot_ly(
        df_plot,
        x = ~Sample,
        y = ~Expression,
        color = ~Condition,
        colors = color_map,
        split = ~Gene,
        type = "scatter",
        mode = "markers",
        hoverinfo = "text",
        text = ~paste(
          "Sample:", Sample,
          "<br>Gene:", Gene,
          "<br>Condition:", Condition,
          "<br>Expression:", round(Expression,2)
        )
      )
    }
    p %>%
      layout(
        yaxis = list(title = "Normalized Expression"),
        xaxis = list(title = ""),
        boxmode = "group",
        margin = list(l = 50, r = 50, t = 50, b = 120),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd"
      ) %>%
      config(displayModeBar = FALSE)
  })
  ################################################################################
  
  output$gene_corr_heatmap <- renderPlot({
    expr_mat <- readRDS("top100_norm_counts.rds")
    metadata <- metadata_once
    rownames(metadata) <- metadata$Sample_ID
    req(input$selected_gene_2)
    genes_to_plot <- input$selected_gene_2
    if(length(genes_to_plot) < 2){
      plot.new()
      text(
        0.5, 0.5,
        "Please select at least two genes to compute correlation.",
        cex = 1.4
      )
      return()
    }
    samples <- colnames(expr_mat)
    if(input$group_option == "all"){
      samples <- intersect(colnames(expr_mat), rownames(metadata))
    } else {
      group_samples <- rownames(metadata)[metadata$Sample_Type == input$group_option]
      samples <- intersect(colnames(expr_mat), group_samples)
    }
    if(length(samples) == 0){
      plot.new()
      text(0.5, 0.5, "No samples available", cex = 1.5)
      return()
    }
    mat <- expr_mat[genes_to_plot, samples, drop = FALSE]
    gene_var <- apply(mat, 1, var, na.rm = TRUE)
    mat <- mat[gene_var > 0, , drop = FALSE]
    if(nrow(mat) == 0){
      plot.new()
      text(0.5, 0.5, "Selected genes have no variance", cex = 1.5)
      return()
    }
    # Compute correlation
    corr_mat <- cor(t(mat), use = "pairwise.complete.obs")
    n <- nrow(corr_mat)
    heat1 <- pheatmap::pheatmap(
      corr_mat,
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      display_numbers = ifelse(n <= 25, TRUE, FALSE),
      main = paste("Correlation between selected genes -", input$group_option),
      fontsize = ifelse(n <= 20, 16, 10),
      color = colorRampPalette(c("white","#8B4C70" , "#fc9d9a"))(50),
      silent = TRUE
    )
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f3f5fd", col = NA))
    grid::pushViewport(grid::viewport(width = 0.95, height = 0.95))
    grid::grid.draw(heat1$gtable)
  }, width = 900, height = 900)
  #########################################################################
  output$cutoff_ui <- renderUI({
    if(input$filter_by == "padj") {
      numericInput("cutoff_value",
                   "Adjusted p-value cutoff:",
                   value = 0.05,
                   min = 0.001,
                   max = 0.1,
                   step = 0.001)  # small step, fully typable
    } else {
      numericInput("cutoff_value",
                   "Absolute log2FC cutoff:",
                   value = 1,
                   min = 0,
                   max = 5,
                   step = 0.1)  # small step, fully typable
    }
  })
  #################################################################################
  output$heatmap_ui <- renderUI({
    n_genes <- input$top_n
    # Set ~20 pixels per gene row
    plot_height <- 30 * n_genes
    withSpinner(plotOutput("heatmap_plot", height = paste0(plot_height, "px"), width = "2000px"),type = 6, color = "#325175")
  })
  ################################################################################

  output$heatmap_plot <- renderPlot({
    norm_counts_top <- readRDS("annotated_normalized_counts.rds")
    colData <- metadata_once[, c("Sample_ID", "Sample_Type")]
    colData <- colData %>% dplyr::mutate(across(where(is.character), as.factor))
    rownames(colData) <- colData$Sample_ID
    rownames(norm_counts_top) <- norm_counts_top$X
    norm_counts_top$X <- NULL
    DE_df <- DE_df_once
    DE_df <- DE_df %>% dplyr::mutate(across(where(is.character), as.factor))
    if(input$filter_by == "padj"){
      DE_filtered <- DE_df[!is.na(DE_df$padj) & DE_df$padj <= input$cutoff_value, ]
    } else {
      DE_filtered <- DE_df[!is.na(DE_df$log2FoldChange) & abs(DE_df$log2FoldChange) >= input$cutoff_value, ]
    }
    DE_filtered <- DE_filtered[DE_filtered$Gene %in% rownames(norm_counts_top), ]
    top_genes <- head(DE_filtered$Gene, input$top_n)
    if(length(top_genes) == 0){
      plot.new()
      text(0.5, 0.5, "No genes pass the selected cutoff or exist in counts", cex = 1.5)
      return()
    }
    mat <- norm_counts_top[top_genes, , drop = FALSE]
    mat <- as.matrix(mat)
    mode(mat) <- "numeric"
    gene_var <- apply(mat, 1, var, na.rm = TRUE)
    mat <- mat[gene_var > 0, , drop = FALSE]
    if(nrow(mat) == 0){
      plot.new()
      text(0.5, 0.5, "No genes have variance to plot", cex = 1.5)
      return()
    }
    mat_scaled <- t(scale(t(mat)))
    mat_scaled[is.na(mat_scaled)] <- 0
    mat_scaled[is.infinite(mat_scaled)] <- 0
    group_info <- colData[colnames(mat_scaled), "Sample_Type"]
    mat_scaled <- mat_scaled[, order(group_info)]
    annotation_col <- data.frame(Sample_Type = group_info[order(group_info)])
    rownames(annotation_col) <- colnames(mat_scaled)
    ann_colors <- list(
      Sample_Type = c("Negative" = "#fc9d9a", 
                "Severe" = "#8B4C70",
                "Non-Severe" = "#A5CAD2")
    )
    cluster_rows <- FALSE
    cluster_cols <- FALSE
    if(input$cluster_option == "rows"){
      cluster_rows <- TRUE
    }
    if(input$cluster_option == "cols"){
      cluster_cols <- TRUE
    }
    if(input$cluster_option == "both"){
      cluster_rows <- TRUE
      cluster_cols <- TRUE
    }
    # Draw heatmap
    heat <-pheatmap::pheatmap(
      mat_scaled,
      cluster_rows = cluster_rows,
      cluster_cols = cluster_cols,       # disable clustering on columns to separate groups
      show_rownames = TRUE,
      show_colnames = TRUE,
      fontsize = 14,
      annotation_col = annotation_col,
      annotation_colors = ann_colors,
      
      border_color = "grey60"
    )
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "#f3f5fd",col=NA))  # set bg color
    grid::grid.draw(heat$gtable)
    
  })
  ################################################################
  DEdf <- DE_df_once
  DEdf <- DEdf %>% dplyr::mutate(across(where(is.character), as.factor))
  deg_counts <- reactive({
    df <- DEdf
    df <- df[!is.na(df$padj) & !is.na(df$log2FoldChange), ]
    df$Regulation <- "Not Significant"
    df$Regulation[df$padj < input$volcano_padj &
                    df$log2FoldChange > input$volcano_logfc] <- "Upregulated"
    df$Regulation[df$padj < input$volcano_padj &
                    df$log2FoldChange < -input$volcano_logfc] <- "Downregulated"
    df %>%
      dplyr::count(Regulation)
  })
  ###########################################################
  output$deg_barplot <- renderPlotly({
    df <- deg_counts()
    plot_ly(
      df,
      x = ~Regulation,
      y = ~n,
      type = "bar",
      color = ~Regulation,
      colors = c(
        "Upregulated" = "#8B4C70",
        "Downregulated" = "#fc9d9a",
        "Not Significant" = "#A5CAD2"
      ),
      text = ~n,
      textposition = "outside"
    ) %>%
      layout(
       
        xaxis = list(title = "Gene Category"),
        yaxis = list(title = "Gene Count"),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd",
        font = list(family = "Montserrat, sans-serif", size = 14)
      ) %>%
      config(displayModeBar = FALSE)
  })
  ###############################################################################################
  output$volcano_plot <- renderPlotly({
    req(input$volcano_padj, input$volcano_logfc)
    library(plotly)
    library(dplyr)
    df <- DE_df_once
    df <- df[!is.na(df$padj) & !is.na(df$log2FoldChange), ]
    df$neglog10padj <- -log10(df$padj)
    df$Regulation <- "Not Significant"
    df$Regulation[df$padj < input$volcano_padj & df$log2FoldChange > input$volcano_logfc] <- "Upregulated"
    df$Regulation[df$padj < input$volcano_padj & df$log2FoldChange < -input$volcano_logfc] <- "Downregulated"
    top_up <- df %>%
      filter(Regulation == "Upregulated") %>%
      arrange(padj, desc(log2FoldChange)) %>%  
      head(10)
    top_down <- df %>%
      filter(Regulation == "Downregulated") %>%
      arrange(padj, log2FoldChange) %>%   
      head(10)
    top_genes <- rbind(top_up, top_down)
    top_genes$Gene_Symbol <- sub(".*\\((.*)\\)", "\\1", top_genes$Gene)
    top_genes$Ensembl_ID <- sub(" .*", "", top_genes$Gene)
    top_genes$Gene_Symbol[top_genes$Gene_Symbol == "" | is.na(top_genes$Gene_Symbol)] <- top_genes$Ensembl_ID
    p <- plot_ly(
      df,
      x = ~log2FoldChange,
      y = ~neglog10padj,
      type = "scatter",
      mode = "markers",
      color = ~Regulation,
      colors = c(
        "Upregulated" = "#8B4C70",
        "Downregulated" = "#fc9d9a",
        "Not Significant" = "#A5CAD2"
      ),
      marker = list(size = 6, opacity = 0.7),
      text = ~paste(
        "Gene:", Gene,
        "<br>log2FC:", round(log2FoldChange, 2),
        "<br>padj:", signif(padj, 3)
      ),
      hoverinfo = "text"
    )
    p <- p %>%
      add_markers(
        data = top_up,
        x = ~log2FoldChange,
        y = ~neglog10padj,
        marker = list(symbol = "triangle-up", size = 12, color = "#325175"),
        text = ~paste(
          "Gene:", Gene,
          "<br>log2FC:", round(log2FoldChange, 2),
          "<br>padj:", signif(padj, 3)
        ),
        hoverinfo = "text",
        name = "Top 10 Upregulated",
        inherit = FALSE
      )
    p <- p %>%
      add_markers(
        data = top_down,
        x = ~log2FoldChange,
        y = ~neglog10padj,
        marker = list(symbol = "triangle-down", size = 12, color = "#325175"),
        text = ~paste(
          "Gene:", Gene,
          "<br>log2FC:", round(log2FoldChange, 2),
          "<br>padj:", signif(padj, 3)
        ),
        hoverinfo = "text",
        name = "Top 10 Downregulated",
        inherit = FALSE
      )
    p <- p %>%
      layout(
       
        xaxis = list(title = "Log2 Fold Change"),
        yaxis = list(title = "-Log10 Adjusted p-value"),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd",
        shapes = list(
          # logFC cutoff right
          list(
            type = "line",
            x0 = input$volcano_logfc,
            x1 = input$volcano_logfc,
            y0 = 0,
            y1 = max(df$neglog10padj, na.rm = TRUE),
            line = list(dash = "dash")
          ),
          # logFC cutoff left
          list(
            type = "line",
            x0 = -input$volcano_logfc,
            x1 = -input$volcano_logfc,
            y0 = 0,
            y1 = max(df$neglog10padj, na.rm = TRUE),
            line = list(dash = "dash")
          ),
          # padj cutoff
          list(
            type = "line",
            x0 = min(df$log2FoldChange, na.rm = TRUE),
            x1 = max(df$log2FoldChange, na.rm = TRUE),
            y0 = -log10(input$volcano_padj),
            y1 = -log10(input$volcano_padj),
            line = list(dash = "dash")
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    p
  })
  ################################################################################
  DE_df <- DE_df_once
  DE_df <- DE_df %>% dplyr::mutate(across(where(is.character), as.factor))
  volcano_filtered <- reactive({
    req(input$volcano_padj, input$volcano_logfc, input$volcano_table_filter)
    df <- DE_df
    df$Ensembl_ID <- sub(" .*", "", df$Gene)
    df$Gene_Symbol <- sub(".*\\((.*)\\)", "\\1", df$Gene)
    df <- df[!is.na(df$padj) & !is.na(df$log2FoldChange), ]
    df$Regulation <- "Not Significant"
    df$Regulation[df$padj < input$volcano_padj & df$log2FoldChange > input$volcano_logfc] <- "Upregulated"
    df$Regulation[df$padj < input$volcano_padj & df$log2FoldChange < -input$volcano_logfc] <- "Downregulated"
    if(input$volcano_table_filter == "Upregulated") {
      df <- df[df$Regulation == "Upregulated", ]
    } else if(input$volcano_table_filter == "Downregulated") {
      df <- df[df$Regulation == "Downregulated", ]
    }
    df$Gene_Link <- paste0(
      '<a href="https://www.ncbi.nlm.nih.gov/gene/?term=', 
      df$Gene_Symbol, 
      '" target="_blank">', 
      df$Gene_Symbol, 
      '</a>'
    )
    df_display <- df[, c("Gene_Link", "Ensembl_ID", "log2FoldChange", "padj", "Regulation")]
    colnames(df_display)[1] <- "Gene"
    df_display
  })
  ######################################################################################
  output$volcano_gene_table <- DT::renderDataTable({
    df <- volcano_filtered()
    
    DT::datatable(
      df,
      rownames = FALSE,
      escape = FALSE,  # allow HTML links
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      selection = "none"
    )
  })
  ################################################################
  output$ma_plot <- renderPlotly({
    DE_df <-DE_df_once
    DE_df <- DE_df %>% dplyr::mutate(across(where(is.character), as.factor))
    res_df <- DE_df
    res_df$gene <- rownames(res_df)
    res_df$regulation <- "Not Significant"
    res_df$regulation[res_df$padj < input$volcano_padj & res_df$log2FoldChange >= input$volcano_logfc] <- "Upregulated"
    res_df$regulation[res_df$padj < input$volcano_padj & res_df$log2FoldChange <= -input$volcano_logfc] <- "Downregulated"
    plot_ly(
      res_df,
      x = ~baseMean,
      y = ~log2FoldChange,
      type = "scatter",
      mode = "markers",
      color = ~regulation,
      colors = c(
        "Upregulated" = "#8B4C70",
        "Downregulated" = "#fc9d9a",
        "Not Significant" = "#A5CAD2"
      ),
      marker = list(size = 6),
      text = ~paste(
        "Gene:", gene,
        "<br>Log2FC:", round(log2FoldChange,2),
        "<br>Mean Expression:", round(baseMean,2),
        "<br>Adj p-value:", signif(padj,3)
      )
    ) %>%
      layout(
        xaxis = list(
          title = "Mean Expression (baseMean)",
          type = "log"
        ),
        yaxis = list(
          title = "Log2 Fold Change"
        ),
        margin = list(l = 50, r = 20, t = 50, b = 100),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd",
        font = list(family = "Montserrat, sans-serif", size = 14)
      ) %>%
      config(displayModeBar = FALSE)
  })
  ################################################################
  output$pathway_plot <- renderPlotly({
    if(input$pathway_type == "bp"){
      df <- readRDS("go_enrich_bp_filtered.rds")
    } else if(input$pathway_type == "mf"){
      df <- readRDS("go_enrich_mf_filtered.rds")
    } else if(input$pathway_type == "cc"){
      df <- readRDS("go_enrich_cc_filtered.rds")
    }
    df <- df %>% filter(!is.na(p.adjust))
    df <- df %>% arrange(p.adjust) %>% head(input$top_terms)
    p <- ggplot(
      df,
      aes(
        x = Count,
        y = reorder(str_wrap(Description, 40), Count),
        text = paste(
          "Term:", Description,
          "<br>Gene Count:", Count,
          "<br>Adj p-value:", signif(p.adjust, 3)
        )
      )
    ) +
      geom_point(aes(size = Count, color = -log10(p.adjust)), alpha = 0.9) +
      scale_size_continuous(name = "Gene Count", range = c(2, 10)) +
      scale_color_gradient(low = "#fc9d9a", high = "#8B4C70", name = "-log10(adj p-value)") +
      labs(x = "Gene Count", y = "GO / Pathway Term") +
      theme_minimal() +
      theme(
        text = element_text(family = "Montserrat"),
        axis.text.y = element_text(size = 11)
      )
    ggplotly(p, tooltip = "text") %>%
      
      layout(
        margin = list(l = 300, r = 180, t = 50, b = 80),
       
        font = list(family = "Montserrat, sans-serif", size = 14),
        paper_bgcolor = "#f3f5fd",
        plot_bgcolor = "#f3f5fd",
        xaxis = list(title = "Gene Count", automargin = TRUE),
        yaxis = list(
          title = list(text = "GO Term", standoff = 30),
          automargin = TRUE
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  ##############################################################
  deg <- DE_df_once
  pathway_data <- reactive({
    ego <- switch(
      input$pathway_type,
      "bp"   = readRDS("go_enrich_bp_filtered.rds"),
      "mf"   = readRDS("go_enrich_mf_filtered.rds"),
      "cc"   = readRDS("go_enrich_cc_filtered.rds")
    )

    df <- ego
  
    df <- df %>% filter(!is.na(p.adjust))
    df <- df %>% arrange(p.adjust)
    df <- head(df, input$top_terms)
    df
  })
  ####################################################
  output$pathway_table <- renderDT({
    df <- pathway_data()  
    deg_entrez <- DE_df_once
    
    # Mapping: ENS_ID -> SYMBOL
    ensembl_to_symbol <- setNames(
      sub(".*\\((.*)\\)", "\\1", deg_entrez$Gene),
      deg_entrez$ENS_ID
    )
    
    # Mapping: ENTREZID -> ENS_ID
    entrez_to_ensembl <- setNames(
      deg_entrez$ENS_ID,
      deg_entrez$ENTREZID
    )
    df$FoldEnrichment <- round(df$FoldEnrichment, 3)
    df$p.adjust <- signif(df$p.adjust, 3)
   
    
    # Handle empty result
    if(nrow(df) == 0) {
      df <- data.frame(
        ID = character(0),
        Description = character(0),
        Genes_in_dataset = character(0),
        GeneRatio = numeric(0),
        FoldEnrichment = numeric(0),
        p.adjust = numeric(0),
        
        stringsAsFactors = FALSE
      )
    } else {
      
      
      # Map ENTREZID to symbols with links
      df$Genes_in_dataset <- sapply(df$geneID, function(entrez_str){
        entrez_genes <- strsplit(entrez_str, "/")[[1]] %>% trimws()
        ens_genes <- entrez_to_ensembl[as.character(entrez_genes)]  # <- important
        symbols <- ensembl_to_symbol[ens_genes]
        symbols[is.na(symbols)] <- entrez_genes[is.na(symbols)]     # fallback to ENTREZ
        links <- paste0('<a href="https://www.ncbi.nlm.nih.gov/gene/?term=', 
                        symbols, '" target="_blank">', symbols, '</a>')
        paste(links, collapse = ", ")
      })
      
      # Keep desired column order
      df <- df[, c("ID", "Description", "Genes_in_dataset", "GeneRatio", 
                   "FoldEnrichment", "p.adjust")]
    }
    
    # Render datatable
    DT::datatable(
      df,
      rownames = FALSE,
      escape = FALSE,        # allow HTML links
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      selection = "none"
    )
  })
    ################################################################################
  output$ml_heatmap_ui <- renderUI({
    
    biomarker_df <- readRDS("ml_reduced_with_gene_names.rds")
    
    n_genes <- ncol(biomarker_df) - 2
    
    plot_height <- 30 * n_genes
    
    withSpinner(
      plotOutput(
        "ml_biomarker_heatmap",
        height = paste0(plot_height, "px"),
        width = "2000px"
      ),
      type = 6,
      color = "#325175"
    )
    
  })
  
  
  output$ml_biomarker_heatmap <- renderPlot({
    
    biomarker_df <- readRDS("ml_reduced_with_gene_names.rds")
    
    sample_ids <- biomarker_df$Sample_ID
    
    annotation_col <- data.frame(
      Class = biomarker_df$Class
    )
    
    rownames(annotation_col) <- sample_ids
    
    expr_mat <- biomarker_df %>%
      dplyr::select(-Sample_ID, -Class) %>%
      as.matrix()
    
    rownames(expr_mat) <- sample_ids
    
    mode(expr_mat) <- "numeric"
    
    expr_mat <- t(expr_mat)
    
    expr_mat <- t(scale(t(expr_mat)))
    
    expr_mat[is.na(expr_mat)] <- 0
    expr_mat[is.infinite(expr_mat)] <- 0
    
    ann_colors <- list(
      Class = c(
        "Negative" = "#fc9d9a",
        "Severe" = "#8B4C70",
        "Non_Severe" = "#A5CAD2"
      )
    )
    
    heat <- pheatmap::pheatmap(
      expr_mat,
      cluster_rows = FALSE,
      cluster_cols = FALSE,
      
      show_rownames = TRUE,
      show_colnames = TRUE,
      
      fontsize = 14,
      
      annotation_col = annotation_col,
      annotation_colors = ann_colors,
      
      border_color = "grey60",
      
      silent = TRUE
    )
    
    grid::grid.newpage()
    grid::grid.rect(
      gp = grid::gpar(
        fill = "#f3f5fd",
        col = NA
      )
    )
    
    grid::grid.draw(heat$gtable)
    
  })
}
shinyApp(ui = ui, server = server)