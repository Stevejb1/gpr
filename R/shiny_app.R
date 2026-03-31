#' Launch the gpr Shiny GUI
#'
#' Launches an interactive Shiny application for power analysis.
#' Opens in the RStudio viewer panel or default web browser.
#'
#' @param browser Logical. If TRUE opens in browser instead of RStudio viewer.
#'   Default FALSE.
#' @return None. Launches the Shiny app.
#' @export
#' @examples
#' # See vignette for examples
launch_gpr <- function(browser = FALSE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Please install shiny: install.packages('shiny')")
  }
  if (browser) options(shiny.launch.browser = TRUE)
  shiny::runApp(gpr_shiny_app(), display.mode = "normal")
}

#' Build the gpr Shiny app object
#' @return A Shiny app object
#' @keywords internal
gpr_shiny_app <- function() {

  # ── Auto-detect RStudio theme ─────────────────────────────────────────────
  dark_mode <- tryCatch({
    rstudioapi::getThemeInfo()$dark
  }, error = function(e) FALSE)

  bg       <- if (dark_mode) "#1e1e1e" else "#f8f9fa"
  bg2      <- if (dark_mode) "#252526" else "#ffffff"
  bg3      <- if (dark_mode) "#2d2d2d" else "#ffffff"
  bg4      <- if (dark_mode) "#1e1e1e" else "#f0f0f0"
  border   <- if (dark_mode) "#3c3c3c" else "#e9ecef"
  text     <- if (dark_mode) "#d4d4d4" else "#212529"
  muted    <- if (dark_mode) "#808080" else "#6c757d"
  accent   <- if (dark_mode) "#4ec9b0" else "#2C7BB6"
  btn_bg   <- if (dark_mode) "#0e639c" else "#2C7BB6"
  btn_hov  <- if (dark_mode) "#1177bb" else "#1a5a8a"
  input_bg <- if (dark_mode) "#3c3c3c" else "#ffffff"
  input_bd <- if (dark_mode) "#555555" else "#dee2e6"
  dd_bg    <- if (dark_mode) "#2d2d2d" else "#ffffff"

  css <- sprintf("
    body { font-family: 'Helvetica Neue', Arial, sans-serif;
           background-color: %s; color: %s; }
    .sidebar-panel { background-color: %s;
                     border-right: 1px solid %s;
                     padding: 20px; min-height: 100vh; }
    .main-panel { padding: 20px; }
    .result-box { background-color: %s; border: 1px solid %s;
                  border-radius: 8px; padding: 20px; margin-bottom: 20px;
                  box-shadow: 0 1px 3px rgba(0,0,0,0.1); }
    .result-title { font-size: 1.1rem; font-weight: 600; color: %s;
                    margin-bottom: 15px; border-bottom: 1px solid %s;
                    padding-bottom: 8px; }
    .result-value { font-size: 1.8rem; font-weight: 700; color: %s; }
    .result-label { font-size: 0.85rem; color: %s;
                    text-transform: uppercase; letter-spacing: 0.5px; }
    .section-header { font-size: 0.75rem; font-weight: 700;
                      text-transform: uppercase; letter-spacing: 1px;
                      color: %s; margin-top: 20px; margin-bottom: 8px; }
    .btn-run { background-color: %s; color: #ffffff; border: none;
               border-radius: 6px; padding: 10px 24px; font-weight: 600;
               width: 100%%; margin-top: 20px; font-size: 1rem; }
    .btn-run:hover { background-color: %s; color: #ffffff; }
    select, input[type='number'], input[type='text'] {
      background-color: %s !important; color: %s !important;
      border: 1px solid %s !important; border-radius: 4px !important; }
    .shiny-input-container { margin-bottom: 12px; }
    .shiny-input-container label { color: %s; }
    .selectize-input { background-color: %s !important;
                       color: %s !important;
                       border: 1px solid %s !important; }
    .selectize-dropdown { background-color: %s !important;
                          color: %s !important;
                          border: 1px solid %s !important; }
    .selectize-dropdown-content .option:hover {
      background-color: %s !important; }
  ",
                 bg, text,
                 bg2, border,
                 bg3, border,
                 accent, border,
                 text,
                 muted,
                 muted,
                 btn_bg, btn_hov,
                 input_bg, text, input_bd,
                 text,
                 input_bg, text, input_bd,
                 dd_bg, text, border,
                 btn_bg
  )

  # ── UI ────────────────────────────────────────────────────────────────────
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML(css))),

    shiny::fluidRow(
      shiny::column(12,
                    shiny::div(
                      style = sprintf(
                        "background:%s; border-bottom:2px solid %s;
             padding:16px 24px; margin-bottom:20px;
             box-shadow:0 2px 4px rgba(0,0,0,0.1);
             text-align:center;",
                        bg2, border),
                      shiny::h3("gpr: Power Analysis for R",
                                style = sprintf("margin:0; color:%s; font-weight:700;
                             font-size:1.6rem;", accent)),
                      shiny::p("By Steve Jean-Baptiste",
                               style = sprintf("margin:4px 0 0 0; color:%s;
                             font-size:0.9rem;", muted))
                    )
      )
    ),

    shiny::fluidRow(

      # ── Sidebar ─────────────────────────────────────────────────────────
      shiny::column(3,
                    shiny::div(class = "sidebar-panel",

                               shiny::div(class = "section-header", "Test Family"),
                               shiny::selectInput("family", NULL,
                                                  choices = c("t tests" = "t", "F tests" = "f",
                                                              "Chi-square tests" = "chisq", "z tests" = "z",
                                                              "Exact tests" = "exact")),

                               shiny::div(class = "section-header", "Statistical Test"),
                               shiny::uiOutput("test_selector"),

                               shiny::div(class = "section-header", "Type of Power Analysis"),
                               shiny::selectInput("analysis", NULL,
                                                  choices = c(
                                                    "A priori: Compute required sample size"    = "a_priori",
                                                    "Post hoc: Compute achieved power"          = "post_hoc",
                                                    "Sensitivity: Compute required effect size" = "sensitivity",
                                                    "Criterion: Compute required alpha"         = "criterion",
                                                    "Compromise: Compute implied alpha & power" = "compromise"
                                                  )),

                               shiny::div(class = "section-header", "Input Parameters"),
                               shiny::uiOutput("input_fields"),

                               shiny::actionButton("run", "Calculate",
                                                   class = "btn-run",
                                                   icon  = shiny::icon("calculator"))
                    )
      ),

      # ── Main panel ──────────────────────────────────────────────────────
      shiny::column(9,
                    shiny::div(class = "main-panel",

                               shiny::uiOutput("result_cards"),

                               shiny::fluidRow(
                                 shiny::column(6,
                                               shiny::div(class = "result-box",
                                                          shiny::div(class = "result-title",
                                                                     "Central and Noncentral Distributions"),
                                                          shiny::plotOutput("dist_plot", height = "300px")
                                               )
                                 ),
                                 shiny::column(6,
                                               shiny::div(class = "result-box",
                                                          shiny::div(class = "result-title", "Power Curve"),
                                                          shiny::plotOutput("power_plot", height = "300px")
                                               )
                                 )
                               ),

                               shiny::div(class = "result-box",
                                          shiny::div(class = "result-title", "Full Output"),
                                          shiny::uiOutput("result_table")
                               )
                    )
      )
    )
  )

  # ── Server ────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    # Dynamic test selector
    output$test_selector <- shiny::renderUI({
      choices <- switch(input$family,
                        "t" = c(
                          "Means: Two independent groups"                = "two.sample",
                          "Means: Difference from constant"              = "one.sample",
                          "Means: Two dependent means (paired)"          = "paired",
                          "Correlation: Point biserial"                  = "point.biserial",
                          "Wilcoxon signed-rank (matched pairs)"         = "wilcoxon.matched",
                          "Wilcoxon signed-rank (one sample)"            = "wilcoxon.onesample",
                          "Wilcoxon-Mann-Whitney (two groups)"           = "wilcoxon.twosample",
                          "Linear bivariate reg: one group, slope"       = "reg.slope.one",
                          "Linear bivariate reg: two groups, intercepts" = "reg.intercepts",
                          "Linear bivariate reg: two groups, slopes"     = "reg.slopes",
                          "Linear multiple reg: single coefficient"      = "reg.coefficient",
                          "Generic t test"                               = "generic"
                        ),
                        "f" = c(
                          "ANCOVA: Fixed effects"                 = "ancova",
                          "ANOVA: Fixed effects, one-way"         = "anova.one.way",
                          "ANOVA: Fixed effects, special"         = "anova.fixed.special",
                          "ANOVA: Repeated measures, between"     = "anova.rm.between",
                          "ANOVA: Repeated measures, within"      = "anova.rm.within",
                          "ANOVA: Repeated measures, interaction" = "anova.rm.interaction",
                          "Hotellings T2: One group"              = "hotelling.one",
                          "Hotellings T2: Two groups"             = "hotelling.two",
                          "MANOVA: Global effects"                = "manova.global",
                          "MANOVA: Special effects"               = "manova.special",
                          "MANOVA: RM between"                    = "manova.rm.between",
                          "MANOVA: RM within"                     = "manova.rm.within",
                          "MANOVA: RM interaction"                = "manova.rm.interaction",
                          "Linear multiple reg: R2 deviation"     = "reg.r2.deviation",
                          "Linear multiple reg: R2 increase"      = "reg.r2.increase",
                          "Variance: Test of equality"            = "variance.equality",
                          "Generic F test"                        = "generic"
                        ),
                        "chisq" = c(
                          "Goodness-of-fit: Contingency tables" = "gof",
                          "Variance: Difference from constant"  = "variance",
                          "Generic chi-square test"             = "generic"
                        ),
                        "z" = c(
                          "Correlation: Tetrachoric model"               = "correlation.tetrachoric",
                          "Correlations: Two dependent r's (common)"     = "correlation.two.dep.common",
                          "Correlations: Two dependent r's (no common)"  = "correlation.two.dep.none",
                          "Correlations: Two independent r's"            = "correlation.two.indep",
                          "Logistic regression"                          = "logistic.regression",
                          "Poisson regression"                           = "poisson.regression",
                          "Proportions: Two independent"                 = "proportions.two.indep",
                          "Generic z test"                               = "generic"
                        ),
                        "exact" = c(
                          "Correlation: Bivariate normal"       = "correlation.bivariate",
                          "Linear multiple reg: Random model"   = "regression.random",
                          "Proportion: One sample (binomial)"   = "proportion.one.sample",
                          "Proportions: McNemar"                = "proportion.mcnemar",
                          "Proportions: Fisher's exact"         = "proportion.fisher",
                          "Proportions: Unconditional"          = "proportion.unconditional",
                          "Proportions: Offset"                 = "proportion.offset",
                          "Proportion: Sign test"               = "proportion.sign",
                          "Generic binomial test"               = "generic.binomial"
                        )
      )
      shiny::selectInput("test", NULL, choices = choices)
    })

    # Dynamic input fields
    output$input_fields <- shiny::renderUI({
      req(input$analysis)
      fields <- list()

      if (input$analysis != "sensitivity") {
        if (!is.null(input$test) &&
            input$test == "proportions.two.indep") {
          fields <- c(fields, list(
            shiny::numericInput("p1", "Proportion p1", 0.50, 0, 1, 0.01),
            shiny::numericInput("p2", "Proportion p2", 0.70, 0, 1, 0.01)
          ))
        } else if (!is.null(input$test) &&
                   input$test == "proportion.one.sample") {
          fields <- c(fields, list(
            shiny::numericInput("p0", "P (H0) constant proportion",
                                0.50, 0, 1, 0.01),
            shiny::numericInput("p1", "P (H1) alternative proportion",
                                0.65, 0, 1, 0.01)
          ))
        } else {
          default_es <- switch(input$family,
                               "t" = 0.50, "f" = 0.25, "chisq" = 0.30,
                               "z" = 0.50, "exact" = 0.15, 0.50)
          fields <- c(fields, list(
            shiny::numericInput("effect_size",
                                "Effect size (default = medium)",
                                default_es, 0, 10, 0.01)
          ))
        }
      }

      if (input$analysis != "criterion") {
        fields <- c(fields, list(
          shiny::numericInput("alpha", "Alpha (default = 0.05)",
                              0.05, 0.001, 0.999, 0.001)
        ))
      }

      if (input$analysis %in% c("a_priori", "sensitivity", "criterion")) {
        fields <- c(fields, list(
          shiny::numericInput("power", "Power 1-beta (default = 0.80)",
                              0.80, 0.01, 0.999, 0.01)
        ))
      }

      if (input$analysis %in% c("post_hoc", "sensitivity",
                                "criterion", "compromise")) {
        fields <- c(fields, list(
          shiny::numericInput("n", "Sample size (n)", 100, 2, 1e6, 1)
        ))
      }

      if (input$analysis == "compromise") {
        fields <- c(fields, list(
          shiny::numericInput("beta_alpha_ratio",
                              "Beta/alpha ratio (default = 1)",
                              1, 0.01, 100, 0.1)
        ))
      }

      if (!is.null(input$family) && input$family == "f" &&
          !is.null(input$test) &&
          input$test %in% c("anova.one.way", "anova.fixed.special",
                            "anova.rm.between", "anova.rm.within",
                            "anova.rm.interaction", "ancova",
                            "manova.global", "manova.special",
                            "manova.rm.between", "manova.rm.within",
                            "manova.rm.interaction")) {
        fields <- c(fields, list(
          shiny::numericInput("groups", "Number of groups", 3, 2, 100, 1)
        ))
      }

      if (!is.null(input$family) && input$family == "f" &&
          !is.null(input$test) &&
          input$test %in% c("reg.r2.deviation", "reg.r2.increase",
                            "hotelling.one", "hotelling.two")) {
        fields <- c(fields, list(
          shiny::numericInput("predictors", "Number of predictors",
                              3, 1, 100, 1)
        ))
      }

      if (!is.null(input$family) && input$family == "chisq") {
        fields <- c(fields, list(
          shiny::numericInput("df", "Degrees of freedom", 3, 1, 100, 1)
        ))
      }

      if (!is.null(input$family) &&
          input$family %in% c("t", "z", "exact")) {
        fields <- c(fields, list(
          shiny::selectInput("tails", "Tails",
                             choices = c("Two-tailed" = 2, "One-tailed" = 1),
                             selected = 2)
        ))
      }

      if (!is.null(input$family) && input$family == "t") {
        fields <- c(fields, list(
          shiny::numericInput("allocation_ratio",
                              "Allocation ratio n2/n1 (default = 1)",
                              1, 0.01, 100, 0.01)
        ))
      }

      do.call(shiny::tagList, fields)
    })

    # ── Helper functions ────────────────────────────────────────────────────
    get_fn <- function() {
      switch(input$family,
             "t"     = gpr_ttest,
             "f"     = gpr_ftest,
             "chisq" = gpr_chisq,
             "z"     = gpr_ztest,
             "exact" = gpr_exact
      )
    }

    build_args <- function(test, analysis, res = NULL) {
      args <- list(test = test, analysis = analysis, plot = FALSE)
      if (!is.null(res)) {
        if (!is.null(res$effect_size)) args$effect_size <- res$effect_size
        if (!is.null(res$alpha))       args$alpha       <- res$alpha
        if (!is.null(res$total_n))     args$n           <- res$total_n
        else if (!is.null(res$n))      args$n           <- res$n
      } else {
        if (!is.null(input$effect_size))
          args$effect_size     <- input$effect_size
        if (!is.null(input$alpha))     args$alpha       <- input$alpha
        if (!is.null(input$power))     args$power       <- input$power
        if (!is.null(input$n))         args$n           <- input$n
        if (!is.null(input$beta_alpha_ratio))
          args$beta_alpha_ratio <- input$beta_alpha_ratio
      }
      if (!is.null(input$groups))     args$groups      <- input$groups
      if (!is.null(input$predictors)) args$predictors  <- input$predictors
      if (!is.null(input$df))         args$df          <- input$df
      if (!is.null(input$tails) &&
          input$family %in% c("t", "z", "exact"))
        args$tails <- as.integer(input$tails)
      if (!is.null(input$allocation_ratio) && input$family == "t")
        args$allocation_ratio <- input$allocation_ratio
      if (!is.null(input$p1)) args$p1 <- input$p1
      if (!is.null(input$p2)) args$p2 <- input$p2
      if (!is.null(input$p0)) args$p0 <- input$p0
      args
    }

    # ── Run analysis ────────────────────────────────────────────────────────
    result <- shiny::eventReactive(input$run, {
      req(input$test, input$analysis, input$family)
      tryCatch(
        do.call(get_fn(), build_args(input$test, input$analysis)),
        error = function(e) {
          shiny::showNotification(paste("Error:", e$message),
                                  type = "error", duration = 5)
          NULL
        }
      )
    })

    # ── Result cards ────────────────────────────────────────────────────────
    output$result_cards <- shiny::renderUI({
      res <- result()
      req(res)

      key_label <- switch(input$analysis,
                          "a_priori"    = "Total Sample Size",
                          "post_hoc"    = "Achieved Power",
                          "sensitivity" = "Effect Size",
                          "criterion"   = "Alpha",
                          "compromise"  = "Actual Power"
      )
      key_value <- switch(input$analysis,
                          "a_priori"    = if (!is.null(res$total_n)) res$total_n else res$n,
                          "post_hoc"    = round(res$power, 4),
                          "sensitivity" = round(res$effect_size, 4),
                          "criterion"   = round(res$alpha, 4),
                          "compromise"  = round(res$power, 4)
      )

      shiny::fluidRow(
        shiny::column(3,
                      shiny::div(class = "result-box", style = "text-align:center;",
                                 shiny::div(class = "result-label", key_label),
                                 shiny::div(class = "result-value",
                                            style = sprintf("color:%s;", accent), key_value)
                      )
        ),
        shiny::column(3,
                      shiny::div(class = "result-box", style = "text-align:center;",
                                 shiny::div(class = "result-label", "Power (1-beta)"),
                                 shiny::div(class = "result-value", round(res$power, 4))
                      )
        ),
        shiny::column(3,
                      shiny::div(class = "result-box", style = "text-align:center;",
                                 shiny::div(class = "result-label", "Alpha"),
                                 shiny::div(class = "result-value", round(res$alpha, 4))
                      )
        ),
        shiny::column(3,
                      shiny::div(class = "result-box", style = "text-align:center;",
                                 shiny::div(class = "result-label", "Beta"),
                                 shiny::div(class = "result-value", round(res$beta, 4))
                      )
        )
      )
    })

    # ── Full output grid ────────────────────────────────────────────────────
    output$result_table <- shiny::renderUI({
      res <- result()
      req(res)

      items <- res[!sapply(res, is.null)]
      items <- items[sapply(items, function(x) length(x) == 1)]

      clean_name <- function(x) {
        x <- gsub("_", " ", x)
        x <- gsub("\\.", " ", x)
        tools::toTitleCase(x)
      }

      format_val <- function(x) {
        if (is.numeric(x)) {
          if (x == round(x)) as.character(as.integer(x))
          else sprintf("%.4f", x)
        } else {
          x <- gsub("_", " ", x)
          x <- gsub("\\.", " ", x)
          tools::toTitleCase(x)
        }
      }

      params <- names(items)
      values <- sapply(items, format_val)
      labels <- sapply(params, clean_name)

      cards <- lapply(seq_along(params), function(i) {
        shiny::column(4,
                      shiny::div(
                        style = sprintf(
                          "background:%s; border-radius:6px;
               padding:10px 14px; margin-bottom:10px;", bg4),
                        shiny::div(
                          style = sprintf(
                            "font-size:0.75rem; color:%s;
                 text-transform:uppercase; letter-spacing:0.5px;", muted),
                          labels[i]),
                        shiny::div(
                          style = sprintf(
                            "font-size:1.1rem; font-weight:600; color:%s;", text),
                          values[i])
                      )
        )
      })

      rows <- list()
      n_items <- length(params)
      for (i in seq(1, n_items, by = 3)) {
        group <- cards[i:min(i + 2, n_items)]
        rows  <- c(rows, list(do.call(shiny::fluidRow, group)))
      }
      do.call(shiny::tagList, rows)
    })

    # ── Distribution plot ───────────────────────────────────────────────────
    output$dist_plot <- shiny::renderPlot({
      res <- result()
      req(res)

      es   <- res$effect_size
      alph <- res$alpha
      n    <- if (!is.null(res$total_n)) res$total_n else res$n

      plot_bg <- bg3

      make_dist_plot <- function(df_p, ar, br, al = NULL,
                                 crit, crit_label,
                                 x_label, h0, h1,
                                 col0, col1, tails = 2) {
        p <- ggplot2::ggplot(df_p,
                             ggplot2::aes(x = x, y = y, color = dist)) +
          ggplot2::geom_area(data = br,
                             ggplot2::aes(x = x, y = y),
                             fill = "#6BAED6", alpha = 0.5,
                             inherit.aes = FALSE) +
          ggplot2::geom_area(data = ar,
                             ggplot2::aes(x = x, y = y),
                             fill = "#FC8D59", alpha = 0.5,
                             inherit.aes = FALSE)
        if (tails == 2 && !is.null(al))
          p <- p + ggplot2::geom_area(data = al,
                                      ggplot2::aes(x = x, y = y),
                                      fill = "#FC8D59", alpha = 0.5,
                                      inherit.aes = FALSE)
        p +
          ggplot2::geom_line(linewidth = 1.1) +
          ggplot2::geom_vline(xintercept = crit, linetype = "dashed",
                              color = if (dark_mode) "#808080" else "gray30") +
          ggplot2::annotate("text", x = crit, y = max(h0) * 0.95,
                            label = crit_label, hjust = -0.05,
                            size = 3.5,
                            color = if (dark_mode) "#808080" else "gray30") +
          ggplot2::annotate("text",
                            x = crit - (crit - min(df_p$x)) * 0.3,
                            y = max(h1) * 0.25,
                            label = expression(beta),
                            size = 7, color = "#6BAED6") +
          ggplot2::annotate("text",
                            x = crit + (max(df_p$x) - crit) * 0.3,
                            y = max(h0) * 0.15,
                            label = if (tails == 2) expression(alpha/2)
                            else expression(alpha),
                            size = 7, color = "#FC8D59") +
          ggplot2::scale_color_manual(
            values = c(col0, col1)) +
          ggplot2::labs(x = x_label, y = "Density", color = NULL) +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(
            legend.position  = "top",
            panel.grid.minor = ggplot2::element_blank(),
            plot.background  = ggplot2::element_rect(fill = plot_bg,
                                                     color = NA),
            panel.background = ggplot2::element_rect(fill = plot_bg,
                                                     color = NA),
            text             = ggplot2::element_text(color = text),
            axis.text        = ggplot2::element_text(color = muted),
            legend.text      = ggplot2::element_text(color = text),
            legend.background = ggplot2::element_rect(fill = plot_bg,
                                                      color = NA)
          )
      }

      if (input$family == "t") {
        is_two  <- input$test %in% c("two.sample", "wilcoxon.twosample",
                                     "reg.intercepts", "reg.slopes")
        alloc   <- if (!is.null(input$allocation_ratio))
          input$allocation_ratio else 1
        tails   <- if (!is.null(input$tails)) as.integer(input$tails) else 2
        df_val  <- if (is_two) n * (1 + alloc) - 2 else n - 1
        ncp_val <- if (is_two) es * sqrt(n * alloc / (1 + alloc))
        else es * sqrt(n)
        crit    <- if (tails == 2) qt(1 - alph/2, df_val)
        else qt(1 - alph, df_val)
        x    <- seq(min(-4, ncp_val - 4), max(4, ncp_val + 4),
                    length.out = 500)
        h0   <- dt(x, df_val)
        h1   <- dt(x, df_val, ncp = ncp_val)
        df_p <- data.frame(
          x    = rep(x, 2), y = c(h0, h1),
          dist = rep(c("H0 (Central t)", "H1 (Noncentral t)"),
                     each = length(x)))
        make_dist_plot(
          df_p = df_p,
          ar   = data.frame(x = x[x >= crit],  y = h0[x >= crit]),
          br   = data.frame(x = x[x <= crit],  y = h1[x <= crit]),
          al   = data.frame(x = x[x <= -crit], y = h0[x <= -crit]),
          crit = crit,
          crit_label = sprintf("critical t = %.4f", crit),
          x_label = "t", h0 = h0, h1 = h1, tails = tails,
          col0 = c("H0 (Central t)" = "#D94701"),
          col1 = c("H1 (Noncentral t)" = "#2171B5")
        )

      } else if (input$family == "f") {
        num_df  <- if (!is.null(res$num_df)) res$num_df else 2
        den_df  <- if (!is.null(res$den_df)) res$den_df
        else max(n - num_df - 1, 1)
        ncp_val <- es^2 * n
        crit    <- qf(1 - alph, num_df, den_df)
        x_max   <- max(qf(0.999, num_df, den_df, ncp = ncp_val), crit * 2)
        x       <- seq(0, x_max, length.out = 500)
        h0      <- df(x, num_df, den_df)
        h1      <- df(x, num_df, den_df, ncp = ncp_val)
        df_p    <- data.frame(
          x    = rep(x, 2), y = c(h0, h1),
          dist = rep(c("H0 (Central F)", "H1 (Noncentral F)"),
                     each = length(x)))
        make_dist_plot(
          df_p = df_p,
          ar   = data.frame(x = x[x >= crit], y = h0[x >= crit]),
          br   = data.frame(x = x[x <= crit], y = h1[x <= crit]),
          crit = crit,
          crit_label = sprintf("critical F = %.4f", crit),
          x_label = "F", h0 = h0, h1 = h1, tails = 1,
          col0 = c("H0 (Central F)" = "#D94701"),
          col1 = c("H1 (Noncentral F)" = "#2171B5")
        )

      } else if (input$family == "chisq") {
        df_val  <- if (!is.null(input$df)) input$df else 3
        ncp_val <- es^2 * n
        crit    <- qchisq(1 - alph, df_val)
        x_max   <- max(qchisq(0.999, df_val, ncp = ncp_val), crit * 2)
        x       <- seq(0, x_max, length.out = 500)
        h0      <- dchisq(x, df_val)
        h1      <- dchisq(x, df_val, ncp = ncp_val)
        df_p    <- data.frame(
          x    = rep(x, 2), y = c(h0, h1),
          dist = rep(c("H0 (Central Chi-sq)", "H1 (Noncentral Chi-sq)"),
                     each = length(x)))
        make_dist_plot(
          df_p = df_p,
          ar   = data.frame(x = x[x >= crit], y = h0[x >= crit]),
          br   = data.frame(x = x[x <= crit], y = h1[x <= crit]),
          crit = crit,
          crit_label = sprintf("critical Chi-sq = %.4f", crit),
          x_label = "Chi-squared", h0 = h0, h1 = h1, tails = 1,
          col0 = c("H0 (Central Chi-sq)" = "#D94701"),
          col1 = c("H1 (Noncentral Chi-sq)" = "#2171B5")
        )

      } else if (input$family == "z") {
        tails   <- if (!is.null(input$tails)) as.integer(input$tails) else 2
        ncp_val <- es * sqrt(n)
        crit    <- if (tails == 2) qnorm(1 - alph/2)
        else qnorm(1 - alph)
        x    <- seq(min(-4, ncp_val - 4), max(4, ncp_val + 4),
                    length.out = 500)
        h0   <- dnorm(x, 0, 1)
        h1   <- dnorm(x, ncp_val, 1)
        df_p <- data.frame(
          x    = rep(x, 2), y = c(h0, h1),
          dist = rep(c("H0 (z | H0 true)", "H1 (z | H1 true)"),
                     each = length(x)))
        make_dist_plot(
          df_p = df_p,
          ar   = data.frame(x = x[x >= crit],  y = h0[x >= crit]),
          br   = data.frame(x = x[x <= crit],  y = h1[x <= crit]),
          al   = data.frame(x = x[x <= -crit], y = h0[x <= -crit]),
          crit = crit,
          crit_label = sprintf("critical z = %.4f", crit),
          x_label = "z", h0 = h0, h1 = h1, tails = tails,
          col0 = c("H0 (z | H0 true)" = "#D94701"),
          col1 = c("H1 (z | H1 true)" = "#2171B5")
        )

      } else {
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "Distribution plot\nnot available for exact tests",
                            size = 5, color = muted) +
          ggplot2::theme_void() +
          ggplot2::theme(
            plot.background = ggplot2::element_rect(fill = bg3, color = NA))
      }
    }, bg = bg3)

    # ── Power curve ─────────────────────────────────────────────────────────
    output$power_plot <- shiny::renderPlot({
      res <- result()
      req(res)

      es    <- res$effect_size
      alph  <- res$alpha
      n_res <- if (!is.null(res$total_n)) res$total_n else res$n
      n_max <- max(n_res * 2, 50)

      power_fn <- if (input$family == "t") {
        is_two <- input$test %in% c("two.sample", "wilcoxon.twosample",
                                    "reg.intercepts", "reg.slopes")
        alloc  <- if (!is.null(input$allocation_ratio))
          input$allocation_ratio else 1
        tails  <- if (!is.null(input$tails)) as.integer(input$tails) else 2
        function(n) {
          df_val  <- if (is_two) n * (1 + alloc) - 2 else n - 1
          ncp_val <- if (is_two) es * sqrt(n * alloc / (1 + alloc))
          else es * sqrt(n)
          crit    <- if (tails == 2) qt(1 - alph/2, df_val)
          else qt(1 - alph, df_val)
          if (tails == 2)
            pt(crit,  df_val, ncp = ncp_val, lower.tail = FALSE) +
            pt(-crit, df_val, ncp = ncp_val, lower.tail = TRUE)
          else
            pt(crit, df_val, ncp = ncp_val, lower.tail = FALSE)
        }
      } else if (input$family == "f") {
        num_df <- if (!is.null(res$num_df)) res$num_df else 2
        function(n) {
          den_df  <- n - num_df - 1
          if (den_df <= 0) return(NA)
          ncp_val <- es^2 * n
          crit    <- qf(1 - alph, num_df, den_df)
          pf(crit, num_df, den_df, ncp = ncp_val, lower.tail = FALSE)
        }
      } else if (input$family == "chisq") {
        df_val <- if (!is.null(input$df)) input$df else 3
        function(n) {
          ncp_val <- es^2 * n
          crit    <- qchisq(1 - alph, df_val)
          pchisq(crit, df_val, ncp = ncp_val, lower.tail = FALSE)
        }
      } else if (input$family == "z") {
        tails <- if (!is.null(input$tails)) as.integer(input$tails) else 2
        function(n) {
          ncp_val <- es * sqrt(n)
          crit    <- if (tails == 2) qnorm(1 - alph/2)
          else qnorm(1 - alph)
          if (tails == 2)
            pnorm(crit - ncp_val, lower.tail = FALSE) +
            pnorm(-crit - ncp_val, lower.tail = TRUE)
          else
            pnorm(crit - ncp_val, lower.tail = FALSE)
        }
      } else {
        function(n) NA
      }

      ggplot2::ggplot(data.frame(n = c(2, n_max)),
                      ggplot2::aes(x = n)) +
        ggplot2::stat_function(fun = power_fn, color = accent,
                               linewidth = 1.2, n = 200) +
        ggplot2::geom_hline(yintercept = res$power, linetype = "dashed",
                            color = "#FC8D59", linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = n_res, linetype = "dashed",
                            color = "#6BAED6", linewidth = 0.8) +
        ggplot2::annotate("point", x = n_res, y = res$power,
                          color = "#FC8D59", size = 4) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    labels = scales::percent_format()) +
        ggplot2::labs(x = "Sample Size (N)", y = "Power (1 - beta)") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          panel.grid.minor  = ggplot2::element_blank(),
          plot.background   = ggplot2::element_rect(fill = bg3, color = NA),
          panel.background  = ggplot2::element_rect(fill = bg3, color = NA),
          text              = ggplot2::element_text(color = text),
          axis.text         = ggplot2::element_text(color = muted),
          panel.grid.major  = ggplot2::element_line(
            color = if (dark_mode) "#3c3c3c" else "#e9ecef")
        )
    }, bg = bg3)

  }

  shiny::shinyApp(ui = ui, server = server)
}
