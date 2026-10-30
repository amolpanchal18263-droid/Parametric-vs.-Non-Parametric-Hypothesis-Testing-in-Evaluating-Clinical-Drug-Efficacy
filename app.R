# Clinical Drug Efficacy Dashboard
# Synthetic educational data; not real patient data or medical evidence.

required <- c("shiny", "DBI", "RSQLite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    paste0("Missing package(s): ", paste(missing, collapse = ", "),
           "\nInstall them in the R Console with:\ninstall.packages(c(",
           paste(sprintf('"%s"', missing), collapse = ", "),
           "), repos='https://cloud.r-project.org')"),
    call. = FALSE
  )
}

library(shiny)
library(DBI)
library(RSQLite)

app_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
db_file <- file.path(app_dir, "data", "clinical_trial.db")
csv_file <- file.path(app_dir, "data", "clinical_trial_data.csv")

load_data <- function() {
  if (file.exists(db_file)) {
    con <- DBI::dbConnect(RSQLite::SQLite(), db_file)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    return(DBI::dbReadTable(con, "clinical_trial"))
  }
  if (file.exists(csv_file)) return(read.csv(csv_file, stringsAsFactors = FALSE))
  stop("Dataset not found. Keep the data folder next to app.R.")
}
trial <- load_data()
trial$treatment_group <- factor(trial$treatment_group, levels = c("Placebo", "Drug A", "Drug B"))
trial$sex <- factor(trial$sex)
trial$adverse_event <- factor(trial$adverse_event, levels = c("None", "Mild", "Moderate", "Severe"))

skewness <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || sd(x) == 0) return(NA_real_)
  mean((x - mean(x))^3) / (sd(x)^3)
}
safe_test <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}
fmt_p <- function(p) if (is.null(p) || length(p) == 0 || is.na(p)) "Not available" else if (p < .001) "< 0.001" else sprintf("%.3f", p)

ui <- fluidPage(
  titlePanel("Clinical Drug Efficacy: Parametric vs Non-Parametric Tests"),
  p("Educational demonstration using synthetic clinical-trial-style data. Not real patient data."),
  sidebarLayout(
    sidebarPanel(
      selectInput("group", "Treatment group", choices = levels(trial$treatment_group), selected = "Drug A"),
      selectInput("test_group", "Group comparison", choices = c("Drug A vs Placebo", "Drug B vs Placebo", "Drug A vs Drug B")),
      selectInput("outcome", "Outcome for between-group tests", choices = c("Change score" = "change_score", "Week 8 score" = "week8_score")),
      helpText("The app reads the SQLite database in data/clinical_trial.db. CSV is included as a fallback.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
          br(), fluidRow(
            column(4, wellPanel(h4("Participants"), textOutput("n_total"))),
            column(4, wellPanel(h4("Mean change"), textOutput("mean_change"))),
            column(4, wellPanel(h4("Median change"), textOutput("median_change")))
          ),
          h3("Outcome by treatment group"), plotOutput("group_plot", height = "320px"),
          h3("Dataset preview"), tableOutput("preview")
        ),
        tabPanel("Distribution & skewness",
          br(), h4("Change score distribution"), plotOutput("hist_plot", height = "320px"),
          h4("Descriptive statistics"), tableOutput("descriptives"),
          p("Skewness is the standardized third central moment. Values farther from zero suggest greater asymmetry.")
        ),
        tabPanel("Hypothesis tests",
          br(), h4("Paired baseline vs Week 8 within selected group"),
          p("H0: median/mean paired change is zero. The paired t-test evaluates mean change; Wilcoxon signed-rank is a rank-based paired alternative."),
          tableOutput("paired_results"),
          h4("Between-group comparison for selected outcome"),
          p("H0: the two groups have equal means (Welch t-test) or equal distributions/location (Wilcoxon rank-sum; interpretation depends on distribution shapes)."),
          tableOutput("between_results"),
          h4("Interpretation guide"),
          tags$ul(
            tags$li("Use paired tests for baseline and follow-up measurements from the same participant."),
            tags$li("Use independent two-sample tests to compare different treatment groups."),
            tags$li("Welch's t-test does not require equal variances but assumes independent observations and approximately normal sampling distributions."),
            tags$li("Wilcoxon tests are rank-based; they are not universally a test of medians unless additional distribution-shape assumptions hold."),
            tags$li("Check study design, missingness, outliers, assumptions, effect sizes, and confidence intervals before drawing conclusions.")
          )
        ),
        tabPanel("Adverse events", br(), plotOutput("ae_plot", height = "320px"), tableOutput("ae_table")),
        tabPanel("About data",
          br(), h4("Database and fields"),
          p("The SQLite database contains a table named clinical_trial. The same records are provided in CSV format."),
          tableOutput("schema"),
          p("All 90 records are synthetic and generated for teaching/demo use. Do not use this dashboard to make clinical decisions.")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  output$n_total <- renderText(nrow(trial))
  output$mean_change <- renderText(sprintf("%.2f points", mean(trial$change_score, na.rm = TRUE)))
  output$median_change <- renderText(sprintf("%.2f points", median(trial$change_score, na.rm = TRUE)))

  output$group_plot <- renderPlot({
    y <- if (input$outcome == "change_score") "Change score" else "Week 8 score"
    boxplot(trial[[input$outcome]] ~ trial$treatment_group,
            xlab = "Treatment group", ylab = y, main = paste(y, "by treatment group"))
    stripchart(trial[[input$outcome]] ~ trial$treatment_group, vertical = TRUE,
               method = "jitter", add = TRUE, pch = 16)
  })

  output$preview <- renderTable(head(trial, 10), striped = TRUE, bordered = TRUE)

  output$hist_plot <- renderPlot({
    x <- trial$change_score[trial$treatment_group == input$group]
    hist(x, breaks = "FD", col = "grey85", border = "white",
         main = paste("Change score:", input$group), xlab = "Week 8 minus baseline")
    abline(v = mean(x), lwd = 2, lty = 2)
    legend("topright", legend = "Mean", lty = 2, bty = "n")
  })

  output$descriptives <- renderTable({
    do.call(rbind, lapply(levels(trial$treatment_group), function(g) {
      x <- trial$change_score[trial$treatment_group == g]
      data.frame(Group = g, N = length(x), Mean = round(mean(x), 2),
                 SD = round(sd(x), 2), Median = round(median(x), 2),
                 IQR = round(IQR(x), 2), Skewness = round(skewness(x), 3))
    }))
  }, digits = 3, striped = TRUE, bordered = TRUE)

  output$paired_results <- renderTable({
    x <- trial$change_score[trial$treatment_group == input$group]
    tt <- safe_test(t.test(x, mu = 0))
    wt <- safe_test(wilcox.test(x, mu = 0, exact = FALSE, conf.int = TRUE))
    data.frame(
      Test = c("One-sample t-test on paired changes", "Wilcoxon signed-rank test on paired changes"),
      Statistic = c(if (is.null(tt)) NA else unname(tt$statistic),
                    if (is.null(wt)) NA else unname(wt$statistic)),
      `p-value` = c(if (is.null(tt)) NA else tt$p.value, if (is.null(wt)) NA else wt$p.value),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }, digits = 4, striped = TRUE, bordered = TRUE)

  output$between_results <- renderTable({
    pair <- switch(input$test_group,
      "Drug A vs Placebo" = c("Drug A", "Placebo"),
      "Drug B vs Placebo" = c("Drug B", "Placebo"),
      "Drug A vs Drug B" = c("Drug A", "Drug B"))
    x <- trial[trial$treatment_group == pair[1], input$outcome]
    y <- trial[trial$treatment_group == pair[2], input$outcome]
    tt <- safe_test(t.test(x, y))
    wt <- safe_test(wilcox.test(x, y, exact = FALSE))
    data.frame(
      Comparison = c(paste(pair, collapse = " vs ")),
      Outcome = input$outcome,
      Test = c("Welch two-sample t-test", "Wilcoxon rank-sum test"),
      Statistic = c(if (is.null(tt)) NA else unname(tt$statistic),
                    if (is.null(wt)) NA else unname(wt$statistic)),
      `p-value` = c(if (is.null(tt)) NA else tt$p.value, if (is.null(wt)) NA else wt$p.value),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }, digits = 4, striped = TRUE, bordered = TRUE)

  output$ae_plot <- renderPlot({
    tab <- table(trial$treatment_group, trial$adverse_event)
    barplot(t(tab), beside = TRUE, legend.text = TRUE,
            args.legend = list(x = "topright", cex = 0.8, bty = "n"),
            xlab = "Treatment group", ylab = "Number of participants",
            main = "Reported adverse events (synthetic data)")
  })
  output$ae_table <- renderTable(as.data.frame.matrix(table(trial$treatment_group, trial$adverse_event)),
                                rownames = TRUE, striped = TRUE, bordered = TRUE)
  output$schema <- renderTable(data.frame(
    Field = names(trial),
    Description = c("Synthetic participant identifier", "Placebo, Drug A, or Drug B",
      "Age in years", "Sex category", "Baseline efficacy score (0–100)",
      "Week 8 efficacy score (0–100)", "Week 8 score minus baseline score",
      "Medication adherence percentage", "Synthetic adverse-event severity")
  ), striped = TRUE, bordered = TRUE)
}

shinyApp(ui = ui, server = server)
