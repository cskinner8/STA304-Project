# controls
#
# Sidebar controls shared by all tabs, followed by the controls specific to
# RQ1, RQ2 and RQ4. Input IDs match the reactive logic in reactive-analysis.R.
# No additional HTML wrappers are used, so existing CSS still applies.

# ---- Global filters and chart appearance --------------------------------
sidebar_controls <- tags$div(class = "sidebar-shell",
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

# ---- RQ1: select importance factors, chart type and rating range ---------
rq1_controls <- tags$div(class = "control-band",
  checkboxGroupInput("selected_factors", "Factors to display",
    choices = setNames(names(importance), unname(importance)),
    selected = names(importance), inline = TRUE),
  radioButtons("factor_chart", "Display as",
    choices = c("Boxplots"="box", "Rating heatmap"="heatmap"),
    selected = "box", inline = TRUE),
  sliderInput("rating_limits", "Boxplot score-axis range",
    min = 1, max = 7, value = c(1, 7), step = 1, sep = "")
)

# ---- RQ2: detailed or grouped locations ----------------------------------
rq2_controls <- tags$div(class = "control-band",
  radioButtons("place_detail", "Location grouping",
    choices=c("Individual locations"="detailed",
      "Grouped settings"="grouped"),
    selected="detailed", inline=TRUE))

# ---- RQ4: eligibility threshold, interval method and random seed ---------
rq4_controls <- tags$div(class="control-band",
  selectInput("barrier_threshold", "Who counts as having limited access?",
    choices=c("Never through sometimes (scores 1–4)"="4",
      "Through often (scores 1–5)"="5",
      "Anything less than always (scores 1–6)"="6"), selected="4"),
  selectInput("interval_method", "Show 95% interval on the plot",
    choices=c("Wilson interval"="wilson",
      "Percentile bootstrap"="bootstrap",
      "No interval"="none"), selected="wilson"),
  numericInput("bootstrap_seed", "Bootstrap random seed",
    value = 20261021, min = 0,
    max = 2147483647, step = 1),
  tags$p(class = "helper-text",
    "Change the seed to generate a new set of bootstrap resamples. The observed shares and Wilson intervals stay the same.")
)
