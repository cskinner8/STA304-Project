# Where We Study · STA304H5F Fall 2026

An interactive **R Shiny** website about study-location choices among 55 respondents to the STA304H5F survey at UTM. Prepared by **Luai's Fan Club** for the **website checkpoint due November 27, 2026**.

**Public app URL:** https://01a20bc9-1da1-f8d7-76c2-a97b353e93a5.share.connect.posit.cloud/

## Files

| File | Purpose |
|---|---|
| `app.R` | All Shiny UI and reactive server logic |
| `data/study_locations.csv` | 55 anonymized survey responses (23 coded analysis columns; no names, IDs, email addresses, or open-text responses) |
| `www/styles.css` | Original styling |
| `checks/check_data.R` | Reproducibility checks against the EDA results |
| `.gitignore` | Prevents accidental inclusion of local R history and original/private source data |
| `manifest.json` | **You generate this locally before GitHub deployment** (not included in this package) |

## Running locally in RStudio

1. Download and unzip the project. Open **RStudio**.
2. In RStudio, choose **File > New Project > Existing Directory**, select the folder containing `app.R`, then create the project. Alternatively, use **File > Open File** to open `app.R` and set your working directory to its folder.
3. In the RStudio **Console**, install dependencies once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "DT", "rsconnect"))
   ```

4. In RStudio, run:

   ```r
   getwd()    # The current directory should contain app.R and data/
   source("checks/check_data.R")
   shiny::runApp(".", launch.browser = TRUE)
   ```

   Or click **Run App** after opening `app.R`.
5. Try **all seven tabs**, change the global subgroup filters, colour palettes and axis limits, switch boxplots to the heatmap, and vary the RQ4 access threshold/interval method. If a subgroup has no responses, the app should display a helpful message rather than a graph.

**Baseline outputs before filtering:** 55 respondents; 35 usually at home (63.6%); 53 say their usual location is their first choice often or more (96.4%); mean seating importance 6.13/7; 50 individual versus 5 collaborative sessions; Fisher's exact test for 2×3 task-by-location table p ≈ 0.418; and for RQ4, the 1–4 access subgroup has 5 respondents, with seating and hours each selected by 3/5.

## Publish through GitHub and Posit Connect Cloud

1. **Generate a dependency manifest** from the project folder in the RStudio Console (after installing the packages):

   ```r
   rsconnect::writeManifest()
   ```

   Verify a **`manifest.json`** file appears alongside `app.R`. Posit Connect Cloud requires this file for GitHub-based **R** deployments. If package dependencies change, regenerate the file.
2. Make a **public** GitHub repository for this project, or place these files in the assigned STA304 group repository (if using a subfolder, select that subfolder's app entry point when publishing). Upload or commit `app.R`, `www/styles.css`, `data/study_locations.csv`, `README.md`, `checks/check_data.R`, and the generated `manifest.json`. Preserve these relative paths and do **not** upload the original Excel file, course-specific IDs, raffle opt-ins, or unredacted survey exports.
3. Sign in to [Posit Connect Cloud](https://connect.posit.cloud/) and connect GitHub if asked. Click **Publish** → **Shiny**, select the GitHub repository and `main` branch, and choose **`app.R`** as the primary file. Follow prompts to publish.
4. Open the resulting public link in a **private/incognito** browser window. Confirm tabs, chart switches, filters, table, and styles load. If publication fails, inspect the Connect Cloud build logs. Most errors involve a missing manifest, package, or file-path mismatch. If needed, regenerate and commit `manifest.json`.
5. Paste the **public Posit Connect Cloud URL** at the top of this README, commit it, and submit the URL according to your course instructions.

See the [official Posit Connect Cloud Shiny R deployment guide](https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html).

## How the app differs from the instructor example

- Four study-specific research pages plus overview, an interactive response table, and a methods/about page.
- Global behavioural subgroup filters (recent task type, usual study setting, preferred-location access).
- Adjustable percentage/count scale and axis maximum, colour palettes, location grouping, and selectable factor boxplots versus a rating **heatmap**.
- Reactive Fisher's exact test where both task groups can be compared, and RQ4 Wilson/bootstrap interval switches with adjustable eligibility thresholds.
- Plain-language interpretations that change along with the filtered responses.

## Important sampling/marking note

**The questionnaire did not collect a demographic variable** such as age, gender, or domestic/international status. The rubric explicitly asks for a *demographic filter*; the filters in this app are genuine **behavioural** subgroups, not demographic ones. Do not invent demographic columns or label these filters as demographic. Ask your TA whether a study-behaviour subgroup filter is acceptable for this part of the checkpoint or whether approved additional demographic information is available. A demographic filtering mark cannot be guaranteed without it.

## Statistical notes

The submitted EDA received 2/2. Importance rankings and study-location proportions are descriptive, not confirmatory between-category tests. The 2×3 Fisher test evaluates **recent actual location**, not preferred location; the small collaborative subgroup makes comparisons uncertain. RQ4 uses the user-selected threshold; the primary threshold (1–4) includes only five students. Percentile bootstrap resamples entire rows (20,000 repetitions) to preserve the multiple-response structure; Wilson intervals are a separate complementary binomial interval. Neither corrects voluntary-response sampling bias. The app does not fit any regression model.
