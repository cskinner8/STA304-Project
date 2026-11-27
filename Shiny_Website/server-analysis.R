# reactive-analysis
#
# Reactive calculations, charts, tables and interpretations. app.R sources
# this file locally within server(), so input, output and session are available
# and each visitor gets a separate reactive state.

# ---- Global filters and shared reactive values ----

# Reset the three global subgroup filters to the full sample.
observeEvent(input$reset_filters, {
  updateSelectInput(session, "task_filter", selected = "all")
  updateSelectInput(session, "place_filter", selected = "all")
  updateSliderInput(session, "access_filter", value = c(1, 7))
})

# Reactive subset used by all tabs: apply the selected task, usual setting,
# and preferred-location access range. Recomputes when any filter changes.
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
# Reactive colour vector chosen in the sidebar; all charts share it.
colours <- reactive(palette_sets[[input$palette]])
# Reactive maximum for bar charts, based on the selected count/percent scale.
bar_cap <- reactive(if (identical(input$measure, "count")) input$count_limit else input$percent_limit)

# ---- Overview: summary cards and interpretations ----

# Dashboard cards: live sample size, home share, first choice and seating.
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

# Overview: describe the current subgroup using plain-language summaries.
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

# ---- RQ1: Study-environment factors ----

# RQ1: selected importance variables, kept in the original factor order.
selected_importance <- reactive({
  sel <- input$selected_factors
  if (is.null(sel)) character(0) else intersect(names(importance), sel)
})
# RQ1: mean, median, IQR and n for each selected factor in the filtered data.
# The table and its interpretation use the same summary.
factor_summary <- reactive({
  x <- filtered(); sel <- selected_importance()
  if (nrow(x) == 0 || length(sel) == 0) return(data.frame())
  do.call(rbind, lapply(sel, function(v) {
    y <- x[[v]]
    data.frame(Factor=unname(importance[[v]]), Mean=mean(y),
               Median=median(y), IQR=IQR(y), N=length(y))
  }))
})
# RQ1: draw either the boxplots or the rating-frequency heatmap.
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

# ---- RQ2: Usual study locations and first choice ----

# RQ2: counts of usual locations, using detailed or grouped categories.
# Recomputes when the global filters or location-grouping control change.
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
# RQ2: display the usual-location distribution as counts or percentages.
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
# RQ2: distribution of first-choice ratings (keep all seven scale levels).
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

# ---- RQ3: Recent academic task and location ----

# RQ3: 2-by-3 contingency table of task type and recent study location.
# The chart, count table and Fisher test all use this filtered table.
task_contingency <- reactive({
  x <- filtered()
  table(Task = factor(x$task_group,levels=c("Individual","Collaborative")),
        Location = factor(x$recent_place_group,
                          levels=c("Home / residence","UTM Library","Other locations")))
})
# RQ3: compare locations using within-task percentages or raw counts.
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
# RQ3: drop empty margins before running Fisher's exact test.
# Explain why a test cannot be run if only one group or location remains.
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

# ---- RQ4: Access barriers and confidence intervals ----

# RQ4: further restrict the filtered sample to the chosen access threshold.
# All barrier denominators come from this eligible subgroup.
limited_access <- reactive({
  x <- filtered()
  x[x$preferred_location_access <= as.integer(input$barrier_threshold),,drop=FALSE]
})
# RQ4: observed counts, percentages and Wilson confidence intervals.
# This calculation does not depend on the random seed.
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
# RQ4: 20,000 whole-row bootstrap resamples of the eligible subgroup.
# Changing the threshold, global filters or seed recomputes the intervals.
# Both the plot and the table reuse this cached reactive result.
bootstrap_results <- reactive({
  x <- limited_access()
  if(nrow(x)==0) return(NULL)
  seed <- input$bootstrap_seed
  validate(need(length(seed) == 1L && is.finite(seed) &&
                  seed >= 0 && seed <= 2147483647 && seed == floor(seed),
                "Enter a whole-number seed from 0 to 2147483647."))
  bootstrap_barrier_intervals(x, names(barriers), reps = 20000L,
                              seed = as.integer(seed))
})
# RQ4: show observed barrier shares with the selected interval type.
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

# RQ4: show observed shares and both interval methods side by side.
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

# ---- Explore responses: searchable data table ----

# Response browser: display only readable, de-identified survey fields.
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
