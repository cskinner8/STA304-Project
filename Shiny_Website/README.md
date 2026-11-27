# Where We Study · STA304H5F Fall 2026

An interactive **R Shiny** website about the study-location choices among 55 respondents to the STA304H5F survey at UTM.

**Public app URL:** https://01a20bc9-1da1-f8d7-76c2-a97b353e93a5.share.connect.posit.cloud/

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
   shiny::runApp(".", launch.browser = TRUE)
   ```

   Or click **Run App** after opening `app.R`.
   
6. **Baseline outputs before filtering:** 55 respondents; 35 usually at home (63.6%); 53 say their usual location is their first choice often or more (96.4%); mean seating importance 6.13/7; 50 individual versus 5 collaborative sessions; Fisher's exact test for 2×3 task-by-location table p ≈ 0.418; and for RQ4, the 1–4 access subgroup has 5 respondents, with se

## Important sampling/marking note

**The questionnaire did not collect a demographic variable** such as age, gender, or domestic/international status. The rubric asked for a *demographic filter*; the filters we used in this app are genuine **behavioural** subgroups, not demographic ones as we did not get demographic ones. 
