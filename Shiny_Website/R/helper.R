# helper
#
# Shared survey labels, grouped variables, statistical helpers and chart styling.
# app.R loads the survey first, then sources this file before the UI is built.

stopifnot(nrow(raw) == 55L)
stopifnot(!any(c("studentID", "usual_location_other_text", "barrier_other_text") %in% names(raw)))

# Readable names for the six importance items (codebook column = display label).
importance <- c(
  imp_seating = "Available seating",
  imp_noise = "Noise level",
  imp_crowding = "Crowding",
  imp_facilities = "Study facilities",
  imp_food_transit = "Nearby food / transit",
  imp_class_proximity = "Proximity to classes"
)

# Decode the original numeric location and task categories.
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
# One binary column per barrier, including the coded write-in.
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

# Confirm that the fields needed by the app are present and complete.
expected_columns <- c("usual_location", "recent_location", "recent_work_type",
                      "preferred_location_access", "usual_is_first_choice",
                      "change_location_frequency", names(importance), names(barriers))
stopifnot(all(expected_columns %in% names(raw)))
stopifnot(!anyNA(raw[expected_columns]))
stopifnot(all(raw$recent_work_type %in% 1:4), all(raw$usual_location %in% 1:6),
          all(raw$recent_location %in% 1:7))

# Group the original codes for subgroup filters and the RQ3 contingency table.
raw$task_group <- ifelse(raw$recent_work_type <= 2, "Individual", "Collaborative")
raw$usual_group <- ifelse(raw$usual_location == 1, "Home",
                          ifelse(raw$usual_location %in% 2:3, "UTM Library",
                                 ifelse(raw$usual_location %in% 4:5, "Other UTM spaces",
                                        "Other off-campus")))
raw$recent_place_group <- ifelse(raw$recent_location == 1, "Home / residence",
                                 ifelse(raw$recent_location %in% 2:4, "UTM Library",
                                        "Other locations"))

# Chart palettes shared by every research-question tab.
palette_sets <- list(
  "Coastal teal" = c("#167D86", "#83C4C2", "#D8ECE9"),
  "University blue" = c("#275D9A", "#8AADD9", "#D7E7F6"),
  "Warm coral" = c("#CE6B55", "#F1AE85", "#F7E4D8"),
  "Colour-blind friendly" = c("#0072B2", "#E69F00", "#56B4E9")
)

# Statistical and data-formatting helpers used by the server.

#' Format a sample proportion as a percentage
#' @description Convert a count and denominator to a readable percentage.
#' @param k Number of respondents with the characteristic.
#' @param n Total number of eligible respondents.
#' @param digits Number of decimal places to display.
#' @return A percentage string, or an em dash if there is no denominator.
pct_text <- function(k, n, digits = 1) {
  if (is.null(n) || n <= 0) return("—")
  sprintf(paste0("%.", digits, "f%%"), 100 * k / n)
}

#' Calculate a Wilson score confidence interval for a sample proportion
#' @description Compute the binomial Wilson score interval for a barrier.
#' @param k Number of respondents selecting a particular barrier.
#' @param n Number of eligible respondents.
#' @param conf Confidence level (default 0.95).
#' @return Numeric vector with lower and upper limits in percentage units.
#' @details Uses the binomial Wilson score interval, which can provide
#' informative limits even when the observed proportion is zero or one.
wilson_interval <- function(k, n, conf = 0.95) {
  if (n < 1) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2)
  p <- k / n
  den <- 1 + z^2 / n
  centre <- (p + z^2 / (2 * n)) / den
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / den
  100 * c(max(0, centre - half), min(1, centre + half))
}

#' Estimate percentile bootstrap intervals for barrier proportions
#' @description Resample respondents with replacement to estimate uncertainty.
#' @param df Data frame containing eligible respondent rows.
#' @param vars Character vector of binary barrier column names.
#' @param reps Number of bootstrap resamples (default 20000).
#' @param seed Integer seed selected by the visitor.
#' @return Matrix with one row per barrier and two columns giving the
#' 2.5th and 97.5th percentiles in percentage units.
#' @details Resamples entire respondent rows to preserve the dependence
#' between multiple barriers selected by the same respondent. A seed makes
#' each set of results reproducible; changing it generates new resamples.
bootstrap_barrier_intervals <- function(df, vars, reps = 20000L, seed) {
  n <- nrow(df)
  if (n == 0) return(matrix(NA_real_, nrow = length(vars), ncol = 2))
  m <- as.matrix(df[, vars, drop = FALSE])
  set.seed(seed)
  samples <- replicate(reps, {
    idx <- sample.int(n, size = n, replace = TRUE)
    colMeans(m[idx, , drop = FALSE])
  })
  t(apply(samples, 1, quantile, probs = c(.025, .975))) * 100
}

#' Convert selected importance ratings to long format
#' @description Arrange the ratings for ggplot2 boxplots and heatmaps.
#' @param df Filtered respondent data frame.
#' @param selected Character vector of importance variable names.
#' @return Data frame with one row per respondent-factor rating, containing
#' a readable factor name and its numerical score.
rating_long <- function(df, selected) {
  if (length(selected) == 0 || nrow(df) == 0) {
    return(data.frame(factor_name = character(), score = numeric()))
  }
  data.frame(
    factor_name = rep(unname(importance[selected]), each = nrow(df)),
    score = unlist(df[, selected, drop = FALSE], use.names = FALSE)
  )
}

# Shared plot styling and UI components.

#' Provide a consistent ggplot theme
#' @return A ggplot2 theme object used for all charts in the app.
base_plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold", size = 15, colour = "#17313B"),
          plot.subtitle = element_text(colour = "#536B75"),
          axis.title = element_text(colour = "#405760"),
          axis.text = element_text(colour = "#314B54"),
          plot.margin = margin(16, 18, 18, 12))
}

#' Build a dashboard summary card
#' @param caption Short metric label.
#' @param value Value to show in the card.
#' @param foot Supporting text describing the metric or denominator.
#' @return A Shiny HTML div containing the three text elements.
metric_card <- function(caption, value, foot) {
  tags$div(class = "metric-card",
           tags$div(class = "metric-label", caption),
           tags$div(class = "metric-value", value),
           tags$div(class = "metric-foot", foot))
}

#' Build the introduction to a research-question tab
#' @param kicker Small heading above the main heading.
#' @param heading Main section heading.
#' @param description Short explanation of what the visitor can explore.
#' @return A Shiny HTML div containing the introductory text.
section_intro <- function(kicker, heading, description) {
  tags$div(class = "section-intro",
           tags$div(class = "section-kicker", kicker),
           tags$h2(heading), tags$p(description))
}

#' Require a nonempty subgroup before drawing a plot or table
#' @param df Data frame after filtering.
#' @return A Shiny validation result; displays a message if no rows remain.
require_rows <- function(df) validate(need(nrow(df) > 0, "No responses match these filters. Try Reset filters."))
