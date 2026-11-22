# Run from the folder containing app.R: Rscript checks/check_data.R
x <- read.csv("data/study_locations.csv", check.names = FALSE)
stopifnot(nrow(x) == 55L, ncol(x) == 23L)
stopifnot(!any(c("studentID", "usual_location_other_text", "barrier_other_text") %in% names(x)))
stopifnot(!anyNA(x))
stopifnot(sum(x$usual_location == 1L) == 35L)
stopifnot(sum(x$recent_work_type <= 2L) == 50L)
stopifnot(sum(x$recent_work_type > 2L) == 5L)
stopifnot(sum(x$usual_is_first_choice >= 5L) == 53L)
stopifnot(sum(x$preferred_location_access <= 4L) == 5L)
stopifnot(sum(x$barrier_seating[x$preferred_location_access <= 4L]) == 3L)
stopifnot(sum(x$barrier_hours[x$preferred_location_access <= 4L]) == 3L)
stopifnot(abs(mean(x$imp_seating) - 6.1272727) < .00001)
ct <- table(
  factor(ifelse(x$recent_work_type <= 2, "Individual", "Collaborative"),
         levels=c("Individual", "Collaborative")),
  factor(ifelse(x$recent_location == 1, "Home",
                ifelse(x$recent_location %in% 2:4, "Library", "Other")),
         levels=c("Home", "Library", "Other"))
)
stopifnot(all(as.matrix(ct) == matrix(c(32L,3L,10L,0L,8L,2L), nrow=2)))
p <- fisher.test(ct)$p.value
stopifnot(abs(p - 0.418) < .003)
cat("PASS: 55 complete, de-identified responses; summary statistics and Fisher test match the submitted report.\n")
cat(sprintf("Fisher exact test p = %.6f\n",p))
