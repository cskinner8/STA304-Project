# STA304H5F, Fall 2026
library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# Load our own cleaned survey once using a relative path for deployment.
raw <- read.csv("data/study_locations.csv", stringsAsFactors = FALSE,
                check.names = FALSE)

# Load shared helpers before any UI components that depend on them.
# R/_disable_autoload.R disables Shiny's automatic alphabetical sourcing.
source(file.path("R", "helper.R"), local = TRUE)
source(file.path("R", "controls.R"), local = TRUE)
source(file.path("R", "study-tabs.R"), local = TRUE)

# Main layout. Sidebar and tab contents are defined in R/.
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bg = "#F5F8F8", fg = "#17313B",
    primary = "#167D86", base_font = "system-ui"),
  # Keep the existing Bootstrap theme and separate stylesheet.
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  tags$div(class = "app-wrap",
    tags$div(class = "hero",
      tags$div(class = "hero-copy",
        tags$div(class = "eyebrow", "STA304H5F  /  FALL 2026  /  SURVEY EXPLORER"),
        tags$h1("Where we study", tags$span(class = "title-dot", ".")),
        tags$p("An interactive look at study spaces, everyday preferences and the barriers students face at UTM.")),
      tags$div(class = "hero-accent", tags$div(class = "accent-top", "55"),
        tags$div("STUDENT RESPONSES"), tags$div(class = "accent-rule"),
        tags$div("LUAI'S FAN CLUB"))
    ),
    fluidRow(
      column(width = 3,
        sidebar_controls
      ),
      column(width = 9,
        tags$div(class = "summary-strip",
          uiOutput("kpi_n"), uiOutput("kpi_home"),
          uiOutput("kpi_first_choice"), uiOutput("kpi_seating")),
        tags$div(class = "main-shell",
          tabsetPanel(id = "report_tab", type = "tabs",
            overview_tab,
            rq1_tab,
            rq2_tab,
            rq3_tab,
            rq4_tab,
            responses_tab
          )
        ),
        tags$div(class="footer-line", "WHERE WE STUDY", tags$span("STA304H5F · FALL 2026 · LUAI'S FAN CLUB"))
      )
    )
  )
)

# Source all reactive logic
server <- function(input, output, session) {
  source(file.path("server-analysis.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
