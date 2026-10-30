CLINICAL DRUG EFFICACY DASHBOARD (R + Shiny + SQLite)
======================================================

Contents
- app.R: Shiny dashboard
- data/clinical_trial.db: SQLite database, table clinical_trial
- data/clinical_trial_data.csv: same dataset in CSV format
- README.txt: these instructions

IMPORTANT
This is a synthetic educational dataset (90 fictional records), not real patient data and not medical evidence. Do not use it for clinical decisions.

RUN ON WINDOWS
1. Extract the ZIP to a simple folder, for example C:\RM project 1.
2. Open R or RStudio.
3. Install the required packages ONCE:
   install.packages(c("shiny", "DBI", "RSQLite"), repos = "https://cloud.r-project.org")
4. If R asks "Would you like to use a personal library instead? (yes/No/cancel)",
   type: yes
   Then press Enter. Do not type a number.
5. Launch the app by running:
   shiny::runApp("C:/RM project 1")
   Replace the folder path with the location where you extracted the ZIP.
   Or set the working directory to the extracted folder and run:
   shiny::runApp()
6. A browser window should open with the dashboard.

If package installation says the Program Files library is not writable, use a user library:
   user_lib <- file.path(Sys.getenv("USERPROFILE"), "R", "library")
   dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
   .libPaths(c(user_lib, .libPaths()))
   install.packages(c("shiny", "DBI", "RSQLite"), lib = user_lib,
                    repos = "https://cloud.r-project.org")
Then run the app again in the same R session.

TESTS INCLUDED
- One-sample t-test on each participant's paired change (week 8 minus baseline)
- Wilcoxon signed-rank test on paired changes
- Welch two-sample t-test for independent treatment groups
- Wilcoxon rank-sum test for independent groups
- Descriptive statistics and skewness
- Synthetic adverse-event counts

INTERPRETATION NOTES
- A paired baseline/follow-up design uses paired tests.
- Independent groups use two-sample tests.
- Welch's t-test compares means and does not assume equal group variances.
- Wilcoxon tests are rank-based; do not automatically interpret them as median tests without considering distribution shape.
- Consider study design, assumptions, effect sizes, confidence intervals, and multiplicity before conclusions.
