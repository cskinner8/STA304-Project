# study-tabs
#
# All six navigation tabs, project background and explanatory notes.
# The controls in controls.R and helper functions in helper.R are loaded first.
# Live outputs are defined in reactive-analysis.R.

# ---- Overview and project background ----
overview_tab <- tabPanel("Overview", value = "overview",
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
)

barrier_note <- tags$div(class="note-block",
  tags$strong("A note on confidence intervals"),
  tags$p("The original five-person subgroup is very small. Percentile bootstrap intervals based on 20,000 whole-respondent resamples can be misleading, especially if zero people selected a barrier: the bootstrap interval then becomes 0–0%. Wilson intervals provide a useful additional perspective, but neither method fixes voluntary-response sampling bias."))

# ---- RQ1: Study factors ----
rq1_tab <- tabPanel("RQ1 · Study factors", value = "rq1",
  section_intro("RESEARCH QUESTION 01", "What matters in a study space?",
    "Compare the distributions of importance ratings on a 1–7 scale. A higher score means greater importance."),
  rq1_controls,
  tags$div(class = "viz-card", plotOutput("factor_plot", height = "420px")),
  tags$div(class = "interpretation", tags$strong("Reading the chart"),
    textOutput("factor_explainer")),
  tags$div(class = "table-card", h4("Importance summary"),
    tableOutput("factor_table"))
)

# ---- RQ2: Study places ----
rq2_tab <- tabPanel("RQ2 · Study places", value = "rq2",
  section_intro("RESEARCH QUESTION 02", "Where do students usually study?",
    "Compare usual study locations and how often students say that location is their first choice."),
  rq2_controls,
  tags$div(class = "viz-card", h4("Usual study location"),
    plotOutput("usual_plot", height = "350px")),
  tags$div(class = "interpretation", textOutput("usual_explainer")),
  tags$div(class = "viz-card", h4("Is the usual location also the first choice?"),
    plotOutput("first_choice_plot", height = "265px")),
  tags$div(class = "interpretation", textOutput("first_choice_explainer"))
)

# ---- RQ3: Academic tasks ----
rq3_tab <- tabPanel("RQ3 · Academic tasks", value = "rq3",
  section_intro("RESEARCH QUESTION 03", "Does task type change where students work?",
    "Each student described one recent session. Compare the actual location of individual versus collaborative work."),
  tags$div(class = "viz-card", plotOutput("task_plot", height="400px")),
  tags$div(class = "interpretation", textOutput("task_explainer")),
  tags$div(class = "table-card", h4("Counts by task and location"),
    tableOutput("task_table")),
  tags$div(class = "test-card", h4("Exploratory association test"),
    htmlOutput("fisher_explainer"))
)

# ---- RQ4: Access barriers ----
rq4_tab <- tabPanel("RQ4 · Access barriers", value = "rq4",
  section_intro("RESEARCH QUESTION 04", "What gets in the way?",
    "Explore reported barriers among students with limited access to their preferred study location. Respondents could select multiple barriers."),
  rq4_controls,
  tags$div(class = "viz-card", plotOutput("barrier_plot", height="410px")),
  tags$div(class = "interpretation", textOutput("barrier_explainer")),
  tags$div(class="table-card", h4("Barrier estimates and uncertainty"),
    tableOutput("barrier_table")),
  barrier_note
)

# ---- Explore responses ----
responses_tab <- tabPanel("Explore responses", value="data",
  section_intro("UNDER THE SURFACE", "Browse the responses",
    "This searchable table contains only de-identified survey fields. Use the sidebar to choose the respondents included."),
  tags$div(class="table-card", DTOutput("response_table")),
  tags$p(class="helper-text", "Study-location and task categories are labelled in words. No course IDs, names, raffle details or open-text responses appear in this project."))
