# WHERE WE STUDY | STA304H5F, Fall 2026
# Interactive Shiny website based on 55 de-identified survey responses.
# Packages: shiny, bslib, ggplot2, DT. No personal identifiers are loaded.

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

# Anna: for section 1, I would try to put it in different .R files so you don't
# have to scroll through all of the code in app.R.
# When you have functions, it's common to have a /R folder that contains all 
# of the functions to separate from the ui and server side.

# -----------------------------------------------------------------------------
# 1. Data and readable labels
# -----------------------------------------------------------------------------
raw <- read.csv("data/study_locations.csv", stringsAsFactors = FALSE,
                check.names = FALSE)
stopifnot(nrow(raw) == 55L)
stopifnot(!any(c("studentID", "usual_location_other_text", "barrier_other_text") %in% names(raw)))

importance <- c(
  imp_seating = "Available seating",
  imp_noise = "Noise level",
  imp_crowding = "Crowding",
  imp_facilities = "Study facilities",
  imp_food_transit = "Nearby food / transit",
  imp_class_proximity = "Proximity to classes"
)

usual_names <- c(
  "Home / residence",
  "UTM Library: basement / main",
  "UTM Library: upper floors",
  "Davis study / silent rooms",
  "Other UTM campus space",
  "Other: Robarts Library"
)
recent_names <- c(
  "Home / residence", "UTM Library: basement", "UTM Library: first floor",
  "UTM Library: upper floors", "Davis study / silent rooms",
  "Other UTM campus space", "Other off-campus location"
)
work_names <- c(
  "Individual studying / reading", "Individual assignments / projects",
  "Collaborative studying", "Collaborative assignments / projects"
)
barriers <- c(
  barrier_seating = "Not enough seating",
  barrier_crowded = "Space too crowded",
  barrier_noise = "Too noisy",
  barrier_travel = "Travel time or distance",
  barrier_hours = "Limited opening hours",
  barrier_transport = "Transportation difficulties",
  barrier_facilities = "Facilities unavailable",
  barrier_classes = "Need to stay near classes",
  barrier_other_code = "Temperature / air quality"
)

expected_columns <- c("usual_location", "recent_location", "recent_work_type",
                      "preferred_location_access", "usual_is_first_choice",
                      "change_location_frequency", names(importance), names(barriers))
stopifnot(all(expected_columns %in% names(raw)))
stopifnot(!anyNA(raw[expected_columns]))
stopifnot(all(raw$recent_work_type %in% 1:4), all(raw$usual_location %in% 1:6),
          all(raw$recent_location %in% 1:7))

raw$task_group <- ifelse(raw$recent_work_type <= 2, "Individual", "Collaborative")
raw$usual_group <- ifelse(raw$usual_location == 1, "Home",
                     ifelse(raw$usual_location %in% 2:3, "UTM Library",
                     ifelse(raw$usual_location %in% 4:5, "Other UTM spaces",
                            "Other off-campus")))
raw$recent_place_group <- ifelse(raw$recent_location == 1, "Home / residence",
                           ifelse(raw$recent_location %in% 2:4, "UTM Library",
                                  "Other locations"))

palette_sets <- list(
  "Coastal teal" = c("#167D86", "#83C4C2", "#D8ECE9"),
  "University blue" = c("#275D9A", "#8AADD9", "#D7E7F6"),
  "Warm coral" = c("#CE6B55", "#F1AE85", "#F7E4D8"),
  "Colour-blind friendly" = c("#0072B2", "#E69F00", "#56B4E9")
)

# Anna: It would be good if you could add actual documentation for your functions 
# as well, as it's not directly obvious to me what this is necessarily used for.
# Would be easier to go back and explain what it's for as well. For R, roxygen
# comments are often used for documentation.
pct_text <- function(k, n, digits = 1) {
  if (is.null(n) || n <= 0) return("—")
  sprintf(paste0("%.", digits, "f%%"), 100 * k / n)
}

# Anna: why are you calling this distribution free? This is a weird documentation
# comment.This just looks like a type of confidence interval.
# Distribution-free, score-based interval for a binary sample proportion.
wilson_interval <- function(k, n, conf = 0.95) {
  if (n < 1) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  100 * c(max(0, centre - half), min(1, centre + half))
}

# Bootstrap entire respondent rows rather than independent barrier answers.
bootstrap_barrier_intervals <- function(df, vars, reps = 20000L) {
  n <- nrow(df)
  if (n == 0) return(matrix(NA_real_, nrow = length(vars), ncol = 2))
  m <- as.matrix(df[, vars, drop = FALSE])
  # Anna: why are you setting this seed at all times? Perhaps you could have
  # someone set in their own seed so they can see how the bootstrap differs?
  set.seed(20261021)
  samples <- replicate(reps, {
    idx <- sample.int(n, size = n, replace = TRUE)
    colMeans(m[idx, , drop = FALSE])
  })
  t(apply(samples, 1, quantile, probs = c(.025, .975))) * 100
}

rating_long <- function(df, selected) {
  if (length(selected) == 0 || nrow(df) == 0) {
    return(data.frame(factor_name = character(), score = numeric()))
  }
  data.frame(
    factor_name = rep(unname(importance[selected]), each = nrow(df)),
    score = unlist(df[, selected, drop = FALSE], use.names = FALSE)
  )
}

base_plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold", size = 15, colour = "#17313B"),
          plot.subtitle = element_text(colour = "#536B75"),
          axis.title = element_text(colour = "#405760"),
          axis.text = element_text(colour = "#314B54"),
          plot.margin = margin(16, 18, 18, 12))
}

metric_card <- function(caption, value, foot) {
  tags$div(class = "metric-card",
           tags$div(class = "metric-label", caption),
           tags$div(class = "metric-value", value),
           tags$div(class = "metric-foot", foot))
}

section_intro <- function(kicker, heading, description) {
  tags$div(class = "section-intro",
           tags$div(class = "section-kicker", kicker),
           tags$h2(heading), tags$p(description))
}

# -----------------------------------------------------------------------------
# 2. User interface
# -----------------------------------------------------------------------------
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bg = "#F5F8F8", fg = "#17313B",
                          primary = "#167D86", base_font = "system-ui"),
  # Anna: TBH, I wondered if you could've just opted for an in-house R Shiny 
  # interface (something like: https://rstudio.github.io/shinythemes/)
  # While I do think your interface looks nice, because it requires certain usage of 
  # css, it's a bit beyond what I expect statistics students to be proficient in.
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
        tags$div(class = "sidebar-shell",
          tags$div(class = "sidebar-heading", "Explore your subgroup"),
          tags$p(class = "helper-text", "These controls filter every chart, table and statistic."),
          selectInput("task_filter", "Most recent academic task",
                      choices = c("All task types" = "all", "Individual work" = "Individual",
                                  "Collaborative work" = "Collaborative"), selected = "all"),
          selectInput("place_filter", "Usual study setting",
                      choices = c("All settings" = "all", "Home / residence" = "Home",
                                  "UTM Library" = "UTM Library",
                                  "Other UTM spaces" = "Other UTM spaces",
                                  "Other off-campus" = "Other off-campus"), selected = "all"),
          sliderInput("access_filter", "Preferred-location access score",
                      min = 1, max = 7, value = c(1,7), step = 1, sep = ""),
          actionButton("reset_filters", "Reset filters", class = "reset-button"),
          tags$hr(class = "sidebar-divider"),
          tags$div(class = "sidebar-heading", "Customize charts"),
          selectInput("palette", "Colour palette", choices = names(palette_sets),
                      selected = "Coastal teal"),
          radioButtons("measure", "Bar chart measure",
                       choices = c("Percent" = "percent", "Count" = "count"),
                       selected = "percent", inline = TRUE),
          conditionalPanel("input.measure == 'percent'",
                           sliderInput("percent_limit", "Percentage-axis maximum",
                                       min = 10, max = 100, value = 100, step = 10)),
          conditionalPanel("input.measure == 'count'",
                           sliderInput("count_limit", "Count-axis maximum",
                                       min = 5, max = 55, value = 55, step = 5)),
          tags$div(class = "privacy-note",
            tags$strong("About these filters"),
            tags$p("The survey did not ask for age, gender or other demographics. Filters use recorded study behaviour, not demographics."))
        )
      ),
      column(width = 9,
        tags$div(class = "summary-strip",
          uiOutput("kpi_n"), uiOutput("kpi_home"),
          uiOutput("kpi_first_choice"), uiOutput("kpi_seating")),
        tags$div(class = "main-shell",
          tabsetPanel(id = "report_tab", type = "tabs",
            tabPanel("Overview", value = "overview",
              section_intro("RESEARCH SNAPSHOT", "Students have different study priorities",
                            "Explore each question using the tabs above. All numbers update when you change the subgroup filters."),
              fluidRow(
                column(6, tags$div(class = "insight-card", tags$div(class = "insight-no", "01  /  STUDY ENVIRONMENT"),
                                    h4("Seating comes first"), textOutput("overview_factor"))),
                column(6, tags$div(class = "insight-card", tags$div(class = "insight-no", "02  /  EVERYDAY HABITS"),
                                    h4("Home is a common choice"), textOutput("overview_home")))
              ),
              fluidRow(
                column(6, tags$div(class = "insight-card", tags$div(class = "insight-no", "03  /  ACADEMIC TASKS"),
                                    h4("Small collaborative subgroup"), textOutput("overview_task"))),
                column(6, tags$div(class = "insight-card", tags$div(class = "insight-no", "04  /  ACCESS BARRIERS"),
                                    h4("Access is not always guaranteed"), textOutput("overview_access")))
              ),
              tags$div(class = "note-block",
                tags$strong("How to interpret this dashboard"),
                tags$p("These responses describe participating STA304 students, not a random sample of all UTM students. Summary rankings are descriptive; they are not proof of statistically significant differences.")),
              section_intro("ABOUT THE PROJECT", "Why we asked these questions",
                            "A student survey project for STA304H5F: Surveys, Sampling and Observational Data, Fall 2026."),
              tags$div(class="about-columns",
                tags$div(class="about-item", h4("Who participated?"),
                  p("55 respondents from the STA304 course at the University of Toronto Mississauga. Participation was voluntary via course-related recruitment channels; this was not a documented simple random sample.")),
                tags$div(class="about-item", h4("Four questions"),
                  tags$ol(tags$li("Which study-space features matter most?"),
                          tags$li("Where do students usually study, and is it their first choice?"),
                          tags$li("Does the location of a recent study session differ by task type?"),
                          tags$li("What barriers are reported by students with limited access?"))),
                tags$div(class="about-item", h4("How the numbers were produced"),
                  p("Importance and frequency ratings use ordered 1–7 scales. Recent task categories were grouped as individual or collaborative. RQ3 uses Fisher’s exact test because some Chi-square expected counts are too small. RQ4 reports observed proportions, percentile bootstrap and Wilson 95% confidence intervals.")),
                tags$div(class="about-item", h4("Important limitations"),
                  p("The survey did not collect demographic variables. Therefore the interactive filters use behaviours and study settings, not gender, age or student status. Subgroups can become very small; results are exploratory and do not establish causation or representativeness."))
              ),
              tags$div(class="note-block", tags$strong("Interpret results with care"),
                tags$p("The course feedback noted that the five-person bootstrap subgroup cannot support strong conclusions. Importance rankings for RQ1 and proportions for RQ2 are descriptive, not formal claims that one category is statistically different from another."))
            ),
            tabPanel("RQ1 · Study factors", value = "rq1",
              section_intro("RESEARCH QUESTION 01", "What matters in a study space?",
                            "Compare the distributions of importance ratings on a 1–7 scale. A higher score means greater importance."),
              tags$div(class = "control-band",
                checkboxGroupInput("selected_factors", "Factors to display",
                                   choices = setNames(names(importance), unname(importance)),
                                   selected = names(importance), inline = TRUE),
                radioButtons("factor_chart", "Display as",
                             choices = c("Boxplots"="box", "Rating heatmap"="heatmap"),
                             selected = "box", inline = TRUE),
                sliderInput("rating_limits", "Boxplot score-axis range",
                            min = 1, max = 7, value = c(1, 7), step = 1, sep = "")
              ),
              tags$div(class = "viz-card", plotOutput("factor_plot", height = "420px")),
              tags$div(class = "interpretation", tags$strong("Reading the chart"),
                       textOutput("factor_explainer")),
              tags$div(class = "table-card", h4("Importance summary"),
                       tableOutput("factor_table"))
            ),
            tabPanel("RQ2 · Study places", value = "rq2",
              section_intro("RESEARCH QUESTION 02", "Where do students usually study?",
                            "Compare usual study locations and how often students say that location is their first choice."),
              tags$div(class = "control-band",
                radioButtons("place_detail", "Location grouping",
                             choices=c("Individual locations"="detailed",
                                       "Grouped settings"="grouped"),
                             selected="detailed", inline=TRUE)),
              tags$div(class = "viz-card", h4("Usual study location"),
                       plotOutput("usual_plot", height = "350px")),
              tags$div(class = "interpretation", textOutput("usual_explainer")),
              tags$div(class = "viz-card", h4("Is the usual location also the first choice?"),
                       plotOutput("first_choice_plot", height = "265px")),
              tags$div(class = "interpretation", textOutput("first_choice_explainer"))
            ),
            tabPanel("RQ3 · Academic tasks", value = "rq3",
              section_intro("RESEARCH QUESTION 03", "Does task type change where students work?",
                            "Each student described one recent session. Compare the actual location of individual versus collaborative work."),
              tags$div(class = "viz-card", plotOutput("task_plot", height="400px")),
              tags$div(class = "interpretation", textOutput("task_explainer")),
              tags$div(class = "table-card", h4("Counts by task and location"),
                       tableOutput("task_table")),
              tags$div(class = "test-card", h4("Exploratory association test"),
                       htmlOutput("fisher_explainer"))
            ),
            tabPanel("RQ4 · Access barriers", value = "rq4",
              section_intro("RESEARCH QUESTION 04", "What gets in the way?",
                            "Explore reported barriers among students with limited access to their preferred study location. Respondents could select multiple barriers."),
              tags$div(class="control-band",
                selectInput("barrier_threshold", "Who counts as having limited access?",
                  choices=c("Never through sometimes (scores 1–4)"="4",
                            "Through often (scores 1–5)"="5",
                            "Anything less than always (scores 1–6)"="6"), selected="4"),
                selectInput("interval_method", "Show 95% interval on the plot",
                  choices=c("Wilson interval"="wilson",
                            "Percentile bootstrap"="bootstrap",
                            "No interval"="none"), selected="wilson")
              ),
              tags$div(class = "viz-card", plotOutput("barrier_plot", height="410px")),
              tags$div(class = "interpretation", textOutput("barrier_explainer")),
              tags$div(class="table-card", h4("Barrier estimates and uncertainty"),
                       tableOutput("barrier_table")),
              tags$div(class="note-block",
                tags$strong("A note on confidence intervals"),
                tags$p("The original five-person subgroup is very small. Percentile bootstrap intervals based on 20,000 whole-respondent resamples can be misleading, especially if zero people selected a barrier: the bootstrap interval then becomes 0–0%. Wilson intervals provide a useful additional perspective, but neither method fixes voluntary-response sampling bias."))
            ),
            tabPanel("Explore responses", value="data",
              section_intro("UNDER THE SURFACE", "Browse the responses",
                            "This searchable table contains only de-identified survey fields. Use the sidebar to choose the respondents included."),
              tags$div(class="table-card", DTOutput("response_table")),
              tags$p(class="helper-text", "Study-location and task categories are labelled in words. No course IDs, names, raffle details or open-text responses appear in this project."))
            )

          ),
        tags$div(class="footer-line", "WHERE WE STUDY", tags$span("STA304H5F · FALL 2026 · LUAI'S FAN CLUB"))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# 3. Reactive analysis
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "task_filter", selected = "all")
    updateSelectInput(session, "place_filter", selected = "all")
    updateSliderInput(session, "access_filter", value = c(1, 7))
  })

  # Anna: the one thing I think is uncanny is the lack of comments when it comes to
  # some of these reactive variables; I think I would've been explicit that here, 
  # filtered is some reactive variable that filters the data based on some sort of
  # X or Y...
  filtered <- reactive({
    x <- raw
    if (!is.null(input$task_filter) && input$task_filter != "all") {
      x <- x[x$task_group == input$task_filter, , drop=FALSE]
    }
    if (!is.null(input$place_filter) && input$place_filter != "all") {
      x <- x[x$usual_group == input$place_filter, , drop=FALSE]
    }
    if (!is.null(input$access_filter)) {
      x <- x[x$preferred_location_access >= input$access_filter[1] &
             x$preferred_location_access <= input$access_filter[2], , drop=FALSE]
    }
    x
  })
  colours <- reactive(palette_sets[[input$palette]])
  bar_cap <- reactive(if (identical(input$measure, "count")) input$count_limit else input$percent_limit)
  require_rows <- function(df) validate(need(nrow(df) > 0, "No responses match these filters. Try Reset filters."))

  output$kpi_n <- renderUI({
    metric_card("RESPONDENTS SHOWN", nrow(filtered()), "out of 55 total")
  })
  output$kpi_home <- renderUI({
    x <- filtered(); metric_card("USUALLY AT HOME",
      pct_text(sum(x$usual_location == 1), nrow(x)), "of filtered respondents")
  })
  output$kpi_first_choice <- renderUI({
    x <- filtered(); metric_card("FIRST CHOICE · OFTEN+",
      pct_text(sum(x$usual_is_first_choice >= 5), nrow(x)), "scores 5–7 of 7")
  })
  output$kpi_seating <- renderUI({
    x <- filtered(); metric_card("SEATING IMPORTANCE",
      if (nrow(x)) sprintf("%.2f / 7", mean(x$imp_seating)) else "—",
      "mean among filtered respondents")
  })

  output$overview_factor <- renderText({
    x <- filtered(); if (!nrow(x)) return("No responses match the current filters.")
    m <- vapply(names(importance), function(v) mean(x[[v]]), numeric(1))
    i <- which.max(m)
    sprintf("%s has the highest average rating in this selection (%.2f out of 7).",
            unname(importance[i]), m[i])
  })
  output$overview_home <- renderText({
    x <- filtered(); if (!nrow(x)) return("No responses match the current filters.")
    sprintf("%d of %d respondents (%s) usually study at home or in residence.",
      sum(x$usual_location == 1), nrow(x), pct_text(sum(x$usual_location == 1), nrow(x)))
  })
  output$overview_task <- renderText({
    x <- filtered(); if (!nrow(x)) return("No responses match the current filters.")
    sprintf("%d reported individual work; %d reported collaborative work in their most recent session.",
      sum(x$task_group == "Individual"), sum(x$task_group == "Collaborative"))
  })
  output$overview_access <- renderText({
    x <- filtered(); if (!nrow(x)) return("No responses match the current filters.")
    sprintf("%d of %d scored preferred-location access 4 or below (sometimes or less often).",
      sum(x$preferred_location_access <= 4), nrow(x))
  })

  selected_importance <- reactive({
    sel <- input$selected_factors
    if (is.null(sel)) character(0) else intersect(names(importance), sel)
  })
  factor_summary <- reactive({
    x <- filtered(); sel <- selected_importance()
    if (nrow(x) == 0 || length(sel) == 0) return(data.frame())
    do.call(rbind, lapply(sel, function(v) {
      y <- x[[v]]
      data.frame(Factor=unname(importance[[v]]), Mean=mean(y),
                 Median=median(y), IQR=IQR(y), N=length(y))
    }))
  })
  output$factor_plot <- renderPlot({
    x <- filtered(); require_rows(x)
    sel <- selected_importance()
    validate(need(length(sel) > 0, "Select at least one factor to display."))
    long <- rating_long(x, sel)
    long$factor_name <- factor(long$factor_name, levels=rev(unname(importance[sel])))
    if (identical(input$factor_chart, "heatmap")) {
      counts <- as.data.frame(table(Factor = long$factor_name,
                                    Rating = factor(long$score, levels=1:7)))
      ggplot(counts, aes(x=Rating, y=Factor, fill=Freq)) +
        geom_tile(colour="white", linewidth=1.0) +
        geom_text(aes(label=ifelse(Freq > 0, Freq, "")), size=3.3, colour="#18343B") +
        scale_fill_gradient(low="#EEF5F5", high=colours()[1], name="Students") +
        labs(x="Importance rating (1 = not at all; 7 = extremely)", y=NULL,
             title="How many students gave each rating?") + base_plot_theme() +
        theme(panel.grid = element_blank())
    } else {
      ggplot(long, aes(x=factor_name, y=score)) +
        geom_boxplot(fill=colours()[2], colour=colours()[1],
                     width=.57, outlier.colour="#C06549", outlier.size=2,
                     linewidth=.65, na.rm=TRUE) +
        scale_y_continuous(breaks=1:7) +
        coord_flip(ylim=input$rating_limits) +
        labs(x=NULL, y="Importance rating (1–7)",
             title="Importance of study-environment factors") + base_plot_theme()
    }
  }, res = 110)
  output$factor_table <- renderTable({
    t <- factor_summary(); validate(need(nrow(t)>0, "Choose a factor and a nonempty subgroup."))
    t$Mean <- sprintf("%.2f", t$Mean)
    t$Median <- sprintf("%.1f", t$Median)
    t$IQR <- sprintf("%.2f", t$IQR)
    t
  }, striped=TRUE, spacing="s", width="100%")
  output$factor_explainer <- renderText({
    t <- factor_summary()
    if (nrow(t)==0) return("Choose a factor and a nonempty subgroup.")
    i <- which.max(t$Mean)
    sprintf("Among the %d filtered respondents, %s has the highest mean importance score of the selected factors (%.2f/7; median %.1f). Each student's ratings are ordinal, so this is a descriptive ranking rather than proof of a statistically significant difference.",
      t$N[1], tolower(t$Factor[i]), t$Mean[i], t$Median[i])
  })

  usual_distribution <- reactive({
    x <- filtered()
    if (identical(input$place_detail,"grouped")) {
      cats <- c("Home", "UTM Library", "Other UTM spaces", "Other off-campus")
      counts <- table(factor(x$usual_group, levels=cats))
      labels <- c("Home / residence", "UTM Library", "Other UTM spaces", "Other off-campus")
    } else {
      counts <- table(factor(x$usual_location,levels=1:6))
      labels <- usual_names
    }
    data.frame(Location=labels, Count=as.integer(counts), stringsAsFactors=FALSE)
  })
  output$usual_plot <- renderPlot({
    x <- filtered(); require_rows(x)
    counts <- usual_distribution()
    counts$Percent <- 100 * counts$Count / nrow(x)
    counts$Value <- if (identical(input$measure,"count")) counts$Count else counts$Percent
    counts$Location <- factor(counts$Location, levels=rev(counts$Location))
    counts$Label <- if (identical(input$measure,"count")) as.character(counts$Count) else sprintf("%.1f%%", counts$Percent)
    ggplot(counts, aes(Location, Value)) +
      geom_col(fill=colours()[1], width=.65) +
      geom_text(aes(label=Label), hjust=-.14, size=3.4, colour="#17313B") +
      scale_y_continuous(expand=expansion(mult=c(0,.12))) +
      coord_flip(ylim=c(0, bar_cap()), clip="off") +
      labs(x=NULL, y=if (input$measure=="count") "Respondents" else "Share of respondents (%)") +
      base_plot_theme()
  }, res=110)
  output$usual_explainer <- renderText({
    x <- filtered(); if (!nrow(x)) return("No respondents match the filters.")
    k <- sum(x$usual_location==1)
    sprintf("Home or residence was the usual study location for %d of %d respondents (%s) in the selected group. The UTM Library accounts for %d students across its coded levels.",
      k, nrow(x), pct_text(k,nrow(x)), sum(x$usual_location %in% 2:3))
  })
  output$first_choice_plot <- renderPlot({
    x <- filtered(); require_rows(x)
    ratings <- 1:7
    counts <- as.integer(table(factor(x$usual_is_first_choice,levels=ratings)))
    scores <- data.frame(Rating=factor(ratings,levels=ratings), Count=counts,
                         Percent=100*counts/nrow(x))
    scores$Value <- if (input$measure=="count") scores$Count else scores$Percent
    ggplot(scores, aes(Rating, Value)) +
      geom_col(fill=colours()[1], width=.67) +
      geom_text(aes(label=ifelse(Count>0, as.character(Count), "")),
                vjust=-.35, size=3.5, colour="#2D4853") +
      coord_cartesian(ylim=c(0,bar_cap()), clip="off") +
      scale_y_continuous(expand=expansion(mult=c(0,.13))) +
      labs(x="How often is the usual location also the first choice? (1 = never; 7 = always)",
           y=if(input$measure=="count") "Respondents (numbers above bars)" else "Share (%) · counts above bars") +
      base_plot_theme()
  }, res=110)
  output$first_choice_explainer <- renderText({
    x <- filtered(); if(!nrow(x)) return("No respondents match the filters.")
    k <- sum(x$usual_is_first_choice>=5)
    a <- sum(x$usual_is_first_choice==7)
    sprintf("%d of %d respondents (%s) say their usual location is their first choice often, very often or always. Only %d selected always; the two descriptions are not interchangeable.",
            k, nrow(x),pct_text(k,nrow(x)),a)
  })

  task_contingency <- reactive({
    x <- filtered()
    table(Task = factor(x$task_group,levels=c("Individual","Collaborative")),
          Location = factor(x$recent_place_group,
                   levels=c("Home / residence","UTM Library","Other locations")))
  })
  output$task_plot <- renderPlot({
    x <- filtered(); require_rows(x)
    tab <- task_contingency(); long <- as.data.frame(tab)
    names(long) <- c("Task", "Location", "Count")
    group_total <- rowSums(tab)
    long$Percent <- ifelse(group_total[as.character(long$Task)] > 0,
                           100 * long$Count / pmax(1, group_total[as.character(long$Task)]), 0)
    long$Value <- if (input$measure=="count") long$Count else long$Percent
    long <- long[group_total[as.character(long$Task)] > 0, , drop=FALSE]
    ggplot(long, aes(x=Task, y=Value, fill=Location)) +
      geom_col(width=.55, colour="white", linewidth=.6) +
      scale_fill_manual(values=setNames(colours(), levels(long$Location)),drop=FALSE) +
      coord_cartesian(ylim=c(0,bar_cap())) +
      labs(x="Most recent academic task", y=if(input$measure=="count") "Respondents" else "Within-task share (%)",
           fill="Recent study location", title="Where the most recent session took place") +
      base_plot_theme() + theme(legend.position="bottom")
  }, res=110)
  output$task_table <- renderTable({
    ct <- task_contingency()
    dat <- data.frame(Task=rownames(ct), ct, check.names=FALSE)
    dat$Total <- rowSums(ct)
    names(dat) <- c("Task", "Home / residence", "UTM Library", "Other locations", "Total")
    dat
  }, striped=TRUE, spacing="s", width="100%")
  output$task_explainer <- renderText({
    ct <- task_contingency()
    n <- sum(ct); if (n==0) return("No respondents match the filters.")
    sprintf("Of %d selected responses, %d recorded individual work and %d recorded collaborative work. Percentages by task can be unstable when few collaborative sessions remain after filtering.",
      n, sum(ct["Individual",]), sum(ct["Collaborative",]))
  })
  output$fisher_explainer <- renderUI({
    ct <- task_contingency()
    a <- ct[rowSums(ct)>0, colSums(ct)>0, drop=FALSE]
    if (nrow(a)<2 || ncol(a)<2) return(tags$p("An association test needs at least two task types and two locations in the selected subgroup. Broaden the filters to compare both groups."))
    ex <- outer(rowSums(a),colSums(a)) / sum(a)
    result <- tryCatch(fisher.test(a, workspace=2e7), error=function(e) NULL)
    if(is.null(result)) return(tags$p("The exact test could not be calculated for this filtered table."))
    p <- result$p.value
    tags$div(
      tags$p(sprintf("Fisher's exact test: p = %.3f. %d of %d expected cells are below 5, so the usual Chi-square approximation is not reliable.",
                     p, sum(ex<5), length(ex))),
      tags$p(if(p < .05)
        "There is evidence of an association within this selected sample; the result remains exploratory because participation was voluntary."
        else "This selected sample does not provide clear evidence that recent location and task type are associated. This does not prove the groups behave identically.")
    )
  })

  limited_access <- reactive({
    x <- filtered()
    x[x$preferred_location_access <= as.integer(input$barrier_threshold),,drop=FALSE]
  })
  barrier_estimates <- reactive({
    x <- limited_access(); n <- nrow(x)
    if(n==0) return(data.frame())
    cnt <- vapply(names(barriers),function(v) sum(x[[v]]), numeric(1))
    out <- data.frame(Barrier=unname(barriers), Count=as.integer(cnt),
                      Percent=100*cnt/n, stringsAsFactors=FALSE)
    wil <- t(vapply(cnt,wilson_interval,numeric(2),n=n))
    out$Wilson_lower <- wil[,1]; out$Wilson_upper <- wil[,2]
    out
  })
  bootstrap_results <- reactive({
    x <- limited_access()
    if(nrow(x)==0) return(NULL)
    bootstrap_barrier_intervals(x, names(barriers), reps=20000L)
  })
  output$barrier_plot <- renderPlot({
    x <- limited_access(); require_rows(x)
    out <- barrier_estimates()
    out$Value <- if(input$measure == "count") out$Count else out$Percent
    if(input$interval_method == "bootstrap") {
      bounds <- bootstrap_results()
      out$low <- bounds[,1]; out$high <- bounds[,2]
    } else {
      out$low <- out$Wilson_lower; out$high <- out$Wilson_upper
    }
    if(input$measure == "count") {
      out$low <- out$low / 100 * nrow(x)
      out$high <- out$high / 100 * nrow(x)
    }
    out$Barrier <- factor(out$Barrier, levels=rev(out$Barrier[order(out$Percent, decreasing=TRUE)]))
    p <- ggplot(out, aes(Barrier, Value)) +
      geom_col(fill=colours()[1], width=.65, alpha=.92) +
      labs(x=NULL, y=if(input$measure=="count") "Respondents" else "Percentage of eligible respondents (%)",
           title=sprintf("Reported barriers · %d eligible respondents",nrow(x))) +
      scale_y_continuous(expand=expansion(mult=c(0,.09))) +
      coord_flip(ylim=c(0,bar_cap()), clip="off") + base_plot_theme()
    if(input$interval_method != "none") {
      p <- p + geom_errorbar(aes(ymin=low, ymax=high), width=.18,
                            colour="#2D4853", linewidth=.6)
    }
    p
  }, res=110)
  
  output$barrier_table <- renderTable({
    out <- barrier_estimates(); validate(need(nrow(out)>0,"No eligible respondents remain after filtering."))
    bounds <- bootstrap_results()
    out$`Observed share` <- sprintf("%d (%.1f%%)",out$Count,out$Percent)
    out$`Bootstrap 95% interval` <- sprintf("%.1f–%.1f%%",bounds[,1],bounds[,2])
    out$`Wilson 95% interval` <- sprintf("%.1f–%.1f%%",out$Wilson_lower,out$Wilson_upper)
    out[,c("Barrier","Observed share","Bootstrap 95% interval","Wilson 95% interval")]
  }, striped=TRUE, spacing="s", width="100%")
  
  output$barrier_explainer <- renderText({
    out <- barrier_estimates()
    if(!nrow(out)) return("No respondents meet the access threshold and the current filters.")
    n <- nrow(limited_access())
    if(max(out$Count)==0) return(sprintf("None of the %d eligible respondents selected a specific barrier.",n))
    which_top <- out$Barrier[out$Count==max(out$Count)]
    lead <- paste(tolower(which_top),collapse=" and ")
    sprintf("Among %d students with access scores at or below %s, %s %s most frequently selected (%d of %d; %.1f%%). Because respondents could select multiple options, shares do not sum to 100%%. With very small groups, avoid ranking barriers for the wider population.",
      n,input$barrier_threshold, lead, if(length(which_top)==1) "was" else "were",
      max(out$Count),n, max(out$Percent))
  })

  output$response_table <- DT::renderDT({
    x <- filtered(); require_rows(x)
    view <- data.frame(
      `Usual location` = usual_names[x$usual_location],
      `Recent task` = work_names[x$recent_work_type],
      `Recent location` = recent_names[x$recent_location],
      `First-choice frequency` = x$usual_is_first_choice,
      `Preferred-location access` = x$preferred_location_access,
      `Disruption frequency` = x$change_location_frequency,
      `Seating importance` = x$imp_seating,
      `Noise importance` = x$imp_noise,
      `Crowding importance` = x$imp_crowding,
      `Facilities importance` = x$imp_facilities,
      `Near food / transit` = x$imp_food_transit,
      `Near classes` = x$imp_class_proximity,
      check.names=FALSE
    )
    DT::datatable(view, rownames=FALSE, filter="top",
      options=list(pageLength=10, lengthMenu=c(10,25,50), scrollX=TRUE,
                   autoWidth=TRUE, dom="ftip"),
      class="stripe hover compact")
  }, server=FALSE)
}

shinyApp(ui = ui, server = server)