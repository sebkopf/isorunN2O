library(shiny)
library(shinyBS)
library(isorunN2O)
source("folder_browser.R")
source("variables.R")

# Define UI that plots the isotope label enrichment
ui <- shinyUI(
  navbarPage(
    title = "N2O Data Viewer",
    header = "",
    id = "menu", inverse = FALSE,


    # DATA MENU
    tabPanel(
      "Data", value="data",

      # Tabs
      tabsetPanel(
        id = "data_tabs", position = "above", type = "tabs", selected = "data_folder_tab", # selected = "data_overview"

        # data history - NOT currently implemented
        #       tabPanel(
        #         value = "data_history_tab", "History",
        #         br(),
        #         fluidRow(column(width = 5, offset = 1, htmlOutput("datahis_date_range_widget"))),
        #         plotOutput("data_history", height="600px", width = "900px")
        #       ),

        # File Details
        tabPanel(
          value = "data_folder_tab", "Data",
          br(),
          fluidRow(
            column(width = 3,
                   fileInput('upload', 'Upload additional data (zip archives)', accept=c('.zip')),
                   modalFolderSelectorInput("data_folder", size = "large",
                                            folders_label = "Folders",
                                            files_label = "MAT files",
                                            dialog_open_label = "Select data folder",
                                            dialog_close_label = "Open",
                                            dialog_close_id = "open"),
                   htmlOutput("data_loaded_masses")),
            column(width = 9, htmlOutput("data_loaded_files"))
          ),
          plotOutput("data_traces_plot", height="500px", width = "900px")
        ),

        # Overviews
        tabPanel(
          value = "data_overview", "Overview",
          sidebarLayout(

            # OVERVIEW OPTIONS PANEL

            sidebarPanel(
              fluidRow(
                column(width = 8, h4(textOutput("loaded_data_folder"))),
                column(width = 4, align="right", actionButton("data_refresh", "Fetch new", icon("refresh")))),
              htmlOutput("rt_selector_widget"),
              radioButtons("data_type_selector", "Data to show:", inline = FALSE, variables),
              radioButtons("data_drift_correction", "Drift correction:", inline = TRUE, c("none", "linear", "loess")),
              conditionalPanel(
                condition = "input.data_drift_correction == 'loess'",
                numericInput("data_drift_loess", "Local polynomial regression range (alpha):", 0.75, min = 0.1, step = 0.1)
                ),
              htmlOutput("group_selector_widgets")
            ),

            # MAIN DISPLAY PANEL

            mainPanel(
              bsCollapsePanel("Categorization", htmlOutput("category_info"), value = "info", style = "info"),


              tags$div(class = "pull-right",
                       downloadButton("summary_csv_download", "Download Summary", icon("save"))),
              tags$div(class = "pull-right",
                       downloadButton("data_csv_download", "Download Data", icon("save"))),
              tabsetPanel(
                tabPanel("Static Plot",
                         downloadButton("data_overview_download", "Download Plot", icon("plot")),
                         plotOutput("data_overview_plot", height="600px", width = "900px")),
                tabPanel("Interactive Plot", plotlyOutput("data_overview_iplot", height="600px", width = "900px")),
                tabPanel("Drift Correction Plot", value = "drift", plotOutput("data_drift_correct_plot", height="600px", width = "700px")),
                tabPanel("Summary Table", value = "summary", dataTableOutput("data_summary_table"))
              )
            )
          )
        )

      )
    ),


    # LINEATIY ====
    tabPanel(
      "Linearity", value="linearity",

      # Tabs
      tabsetPanel(
        id = "linearity_tabs", selected = "file_tab", position = "above", type = "tabs",

        # File Details
        tabPanel(
          value = "file_tab", "Linearity data",
          br(),
          fluidRow(
            column(width = 3,

                   modalFolderSelectorInput("linearity_folder", size = "large",
                                            folders_label = "Folders",
                                            files_label = "MAT files",
                                            dialog_open_label = "Select linearity & ON/OFF folder",
                                            dialog_close_label = "Open",
                                            dialog_close_id = "open"),

                   br(), br(),
                   htmlOutput("loaded_masses")),
            column(width = 9, htmlOutput("loaded_files"))
          ),
          plotOutput("linearity_traces_plot", height="500px", width = "900px")
          #showOutput("massPlot", "morris") # not using this morris plot at the moment because it's too slow
        ),

        # Linearity
        tabPanel(
          value = "linearity_tab", "Evaluation",
          br(),
          fluidRow(align="center",
                   column(width = 1, "Range:"),
                   column(width = 2, htmlOutput("slider_O_min")),
                   column(width = 2, htmlOutput("slider_O_max")),
                   column(width = 1, offset = 1, "Range:"),
                   column(width = 2, htmlOutput("slider_N_min")),
                   column(width = 2, htmlOutput("slider_N_max"))
          ),
          br(),
          fluidRow(align="center",
                   column(width = 6, textOutput("regression_O")),
                   column(width = 6, textOutput("regression_N"))
          ),
          fluidRow(align="center",
                   column(width = 6, plotOutput("linearity_plot_O", width="400px")),
                   column(width = 6, plotOutput("linearity_plot_N", width="400px"))
          ),
          br(),
          downloadButton("summarize", "Save record & generate summary", icon("save")),
          br(),
          ""
        ),

        # Linearity history
        tabPanel(
          value = "linearity_history_tab", "History",
          br(),
          fluidRow(column(width = 5, offset = 1, htmlOutput("linhis_date_range_widget"))),
          plotOutput("linearity_history", height="600px", width = "900px")
        )
      )
    ),


  # SETTINGS MENU ==========

  tabPanel(
    "Settings", value = "settings",
    h4("Please only edit these settings if you know what you're doing!", style = "color: #f50000;"),
    htmlOutput("settings"),
    br(),
    strong(htmlOutput("settings_msg"), style = "color: #f50000;"),
    br(),
    actionButton("save_settings", "Save settings", icon("save"))
    )
  )
)
