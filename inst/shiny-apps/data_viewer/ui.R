library(shiny)
library(shinyFiles)

# Define UI that plots the isotope label enrichment
ui <- shinyUI(
  navbarPage(
    title = "N2O Data Viewer",
    header = "",
    id = "menu", inverse = FALSE,

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
                   shinyDirButton('linearity_folder', 'Click to select data folder',
                                  'Please select the linearity & ON/OFF folder'),
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

    # DATA MENU
    tabPanel(
      "Data", value="data",

      # Tabs
      tabsetPanel(
        id = "data_tabs", selected = "data_folder_tab", position = "above", type = "tabs",

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
                   shinyDirButton('data_folder', 'Click to select data folder',
                                  'Please select the data folder'),
                   br(), br(),
                   htmlOutput("data_loaded_masses")),
            column(width = 9, htmlOutput("data_loaded_files"))
          ),
          plotOutput("data_traces_plot", height="500px", width = "900px")
        ),

        # Overviews
        tabPanel(
          value = "data_overview", "Overview",
          sidebarLayout(
            sidebarPanel(
              fluidRow(
                column(width = 8, h4(textOutput("loaded_data_folder"))),
                column(width = 4, align="right", actionButton("data_refresh", "Fetch new", icon("refresh")))),
              radioButtons("data_type_selector", "Data to show:", inline = TRUE,
                           c("d15N" = " 15N/14N",
                             "d18O" = " 18O/16O",
                             "Area All" = "Intensity All")),
              htmlOutput("rt_selector_widget"),
              htmlOutput("group_selector_widgets")
            ),
            mainPanel(
              plotOutput("data_overview_plot", height="600px", width = "900px"),
              downloadButton("data_overview_download", "Download Plot", icon("plot")),
              downloadButton("data_csv_download", "Download Grouped Data", icon("save"))
            )
          )
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
