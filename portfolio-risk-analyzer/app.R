# ============================================================================
# PORTFOLIO RISK ANALYZER
# A Shiny-based portfolio risk analysis tool featuring VaR, Monte Carlo
# simulation, stress testing, and performance attribution with live data.
#
# Author: Drew Galvin
# GitHub: github.com/drewgal-ncsu/Drew-Portfolio
# ============================================================================

library(shiny)
library(shinydashboard)
library(quantmod)
library(PerformanceAnalytics)
library(MASS)
library(plotly)
library(DT)
library(scales)
library(zoo)

# Suppress NSE binding warnings
Weight <- Ticker <- Allocation <- Metric <- Value <- NULL
Scenario <- Drawdown <- CumReturn <- SimID <- Day <- Return <- NULL

# ============================================================================
# STRESS TEST SCENARIOS (real historical crash windows)
# ============================================================================
STRESS_SCENARIOS <- list(
    "COVID-19 Crash (Feb–Mar 2020)" = list(start = "2020-02-19", end = "2020-03-23"),
    "Global Financial Crisis (2008)" = list(start = "2008-09-15", end = "2009-03-09"),
    "2022 Rate Shock" = list(start = "2022-01-03", end = "2022-10-12"),
    "Dot-Com Bust (2000–2002)" = list(start = "2000-03-10", end = "2002-10-09"),
    "Flash Crash (Aug 2015)" = list(start = "2015-08-10", end = "2015-08-25"),
    "EU Debt Crisis (2011)" = list(start = "2011-07-22", end = "2011-10-03")
)

BENCHMARK_CHOICES <- c("SPY", "VTI", "QQQ")

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Safely fetch adjusted price series for a ticker
fetch_returns <- function(ticker, start_date) {
    tryCatch(
        {
            data <- getSymbols(ticker, src = "yahoo", from = start_date, auto.assign = FALSE)
            if (is.null(data) || nrow(data) == 0) {
                return(NULL)
            }
            # Use adjusted close
            adj_col <- grep("Adjusted", colnames(data), value = TRUE)
            if (length(adj_col) == 0) adj_col <- grep("Close", colnames(data), value = TRUE)[1]
            prices <- data[, adj_col]
            dailyReturn(prices, type = "log")
        },
        error = function(e) NULL
    )
}

# Format currency helper
fmt_currency <- function(x, prefix = "$") {
    ifelse(abs(x) >= 1e9, paste0(prefix, round(x / 1e9, 2), "B"),
        ifelse(abs(x) >= 1e6, paste0(prefix, round(x / 1e6, 2), "M"),
            paste0(prefix, comma(round(x, 2)))
        )
    )
}

# Format percentage
fmt_pct <- function(x, digits = 2) {
    paste0(round(x * 100, digits), "%")
}

# Parametric VaR (normal)
calc_parametric_var <- function(returns, confidence = 0.95) {
    mu <- mean(returns, na.rm = TRUE)
    sigma <- sd(returns, na.rm = TRUE)
    var_val <- -(mu + qnorm(1 - confidence) * sigma)
    var_val
}

# Historical VaR
calc_historical_var <- function(returns, confidence = 0.95) {
    -quantile(returns, probs = 1 - confidence, na.rm = TRUE)
}

# Conditional VaR (Expected Shortfall)
calc_cvar <- function(returns, confidence = 0.95) {
    var_threshold <- quantile(returns, probs = 1 - confidence, na.rm = TRUE)
    -mean(returns[returns <= var_threshold], na.rm = TRUE)
}

# Run Monte Carlo simulation with correlated returns
run_monte_carlo <- function(returns_matrix, weights, n_sims = 5000, horizon = 252) {
    mu <- colMeans(returns_matrix, na.rm = TRUE)
    sigma <- cov(returns_matrix, use = "pairwise.complete.obs")

    # Generate correlated multivariate normal returns
    sim_returns <- mvrnorm(n = n_sims * horizon, mu = mu, Sigma = sigma)

    # Reshape into simulation paths
    sim_matrix <- array(sim_returns, dim = c(horizon, n_sims, length(mu)))

    # Portfolio returns per day per simulation
    port_daily <- matrix(0, nrow = horizon, ncol = n_sims)
    for (s in 1:n_sims) {
        daily_asset_returns <- sim_matrix[, s, ]
        if (is.null(dim(daily_asset_returns))) {
            daily_asset_returns <- matrix(daily_asset_returns, ncol = 1)
        }
        port_daily[, s] <- daily_asset_returns %*% weights
    }

    # Cumulative returns
    port_cum <- apply(port_daily, 2, function(x) cumprod(1 + x) - 1)

    list(
        daily = port_daily,
        cumulative = port_cum,
        final_returns = port_cum[horizon, ],
        var_95 = -quantile(port_cum[horizon, ], 0.05),
        var_99 = -quantile(port_cum[horizon, ], 0.01),
        median_return = median(port_cum[horizon, ]),
        mean_return = mean(port_cum[horizon, ]),
        prob_loss = mean(port_cum[horizon, ] < 0)
    )
}

# Calculate full performance metrics
calc_performance_metrics <- function(port_returns, bench_returns, rf_rate = 0.045) {
    rf_daily <- rf_rate / 252


    ann_return <- mean(port_returns, na.rm = TRUE) * 252
    ann_vol <- sd(port_returns, na.rm = TRUE) * sqrt(252)
    sharpe <- (ann_return - rf_rate) / ann_vol

    downside_returns <- port_returns[port_returns < 0]
    downside_dev <- sd(downside_returns, na.rm = TRUE) * sqrt(252)
    sortino <- if (downside_dev > 0) (ann_return - rf_rate) / downside_dev else NA

    # Beta & Alpha vs benchmark
    merged <- merge(port_returns, bench_returns, join = "inner")
    if (nrow(merged) > 30) {
        reg <- lm(as.numeric(merged[, 1]) ~ as.numeric(merged[, 2]))
        beta_val <- coef(reg)[2]
        bench_ann <- mean(merged[, 2], na.rm = TRUE) * 252
        alpha_val <- ann_return - (rf_rate + beta_val * (bench_ann - rf_rate))
    } else {
        beta_val <- NA
        alpha_val <- NA
    }

    # Max Drawdown
    cum_returns <- cumprod(1 + port_returns)
    rolling_max <- cummax(cum_returns)
    drawdowns <- (cum_returns - rolling_max) / rolling_max
    max_dd <- min(drawdowns, na.rm = TRUE)

    # Calmar
    calmar <- if (abs(max_dd) > 0) ann_return / abs(max_dd) else NA

    list(
        ann_return = ann_return, ann_vol = ann_vol, sharpe = sharpe,
        sortino = sortino, beta = beta_val, alpha = alpha_val,
        max_drawdown = max_dd, calmar = calmar
    )
}

ui <- dashboardPage(
    skin = "black",
    dashboardHeader(
        title = span(icon("shield-halved"), " Portfolio Risk Analyzer"),
        titleWidth = 350
    ),
    dashboardSidebar(
        width = 300,
        div(
            style = "padding: 15px;",
            h4(icon("briefcase"), "Portfolio Construction", style = "color: #ecf0f1;"),

            # Asset 1
            fluidRow(
                column(7, textInput("ticker1", "Asset 1", value = "AAPL", placeholder = "Ticker")),
                column(5, numericInput("weight1", "Wt %", value = 25, min = 0, max = 100, step = 5))
            ),
            # Asset 2
            fluidRow(
                column(7, textInput("ticker2", "Asset 2", value = "MSFT", placeholder = "Ticker")),
                column(5, numericInput("weight2", "Wt %", value = 25, min = 0, max = 100, step = 5))
            ),
            # Asset 3
            fluidRow(
                column(7, textInput("ticker3", "Asset 3", value = "GOOG", placeholder = "Ticker")),
                column(5, numericInput("weight3", "Wt %", value = 20, min = 0, max = 100, step = 5))
            ),
            # Asset 4
            fluidRow(
                column(7, textInput("ticker4", "Asset 4", value = "AMZN", placeholder = "Ticker")),
                column(5, numericInput("weight4", "Wt %", value = 15, min = 0, max = 100, step = 5))
            ),
            # Asset 5
            fluidRow(
                column(7, textInput("ticker5", "Asset 5", value = "NVDA", placeholder = "Ticker")),
                column(5, numericInput("weight5", "Wt %", value = 15, min = 0, max = 100, step = 5))
            ),
            # Asset 6
            fluidRow(
                column(7, textInput("ticker6", "Asset 6", value = "", placeholder = "Optional")),
                column(5, numericInput("weight6", "Wt %", value = 0, min = 0, max = 100, step = 5))
            ),
            uiOutput("weight_validation"),
            hr(style = "border-color: #4a4a4a;"),
            h4(icon("chart-bar"), "Benchmark", style = "color: #ecf0f1;"),
            selectInput("benchmark", NULL, choices = BENCHMARK_CHOICES, selected = "SPY"),
            hr(style = "border-color: #4a4a4a;"),
            h4(icon("sliders"), "Parameters", style = "color: #ecf0f1;"),
            sliderInput("lookback_years", "Lookback Period (Years)",
                min = 1, max = 10, value = 3, step = 1
            ),
            sliderInput("confidence", "VaR Confidence Level",
                min = 0.90, max = 0.99, value = 0.95, step = 0.01
            ),
            numericInput("rf_rate", "Risk-Free Rate (%)",
                value = 4.5, min = 0, max = 15, step = 0.25
            ),
            numericInput("n_sims", "Monte Carlo Simulations",
                value = 5000, min = 1000, max = 20000, step = 1000
            ),
            hr(style = "border-color: #4a4a4a;"),
            actionButton("run_analysis", "Run Full Analysis",
                icon = icon("play"),
                class = "btn-primary btn-block",
                style = "font-size: 16px; padding: 12px; margin-top: 10px;"
            )
        )
    ),
    dashboardBody(
        tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;600;700&display=swap');
      body, .content-wrapper, .main-sidebar { font-family: 'Inter', sans-serif; }
      .skin-black .main-header .logo { background-color: #1a1a2e; font-weight: 700; }
      .skin-black .main-header .navbar { background-color: #16213e; }
      .content-wrapper { background-color: #0f0f23; }
      .box { background-color: #1a1a2e; border-top: 3px solid #00d2ff; color: #ecf0f1; }
      .box-header { color: #ecf0f1; }
      .box .box-header .box-title { color: #ecf0f1; }
      .info-box { background-color: #16213e; color: #ecf0f1; }
      .info-box .info-box-text { color: #a0a0b0; }
      .info-box .info-box-number { color: #ecf0f1; font-size: 22px; }
      .info-box-icon { background-color: #00d2ff !important; }
      .nav-tabs-custom>.tab-content { background: #1a1a2e; }
      .nav-tabs-custom>.nav-tabs>li.active>a { background-color: #1a1a2e; color: #ecf0f1; border-top-color: #00d2ff; }
      .nav-tabs-custom>.nav-tabs>li>a { color: #a0a0b0; }
      .nav-tabs-custom>.nav-tabs { background-color: #16213e; border-bottom-color: #4a4a4a; }
      .small-box { background-color: #16213e !important; }
      .small-box h3, .small-box p { color: #ecf0f1; }
      .table { color: #ecf0f1; }
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate {
        color: #ecf0f1 !important;
      }
      .metric-card {
        background: linear-gradient(135deg, #16213e, #1a1a2e);
        border-left: 4px solid #00d2ff; padding: 15px;
        margin-bottom: 10px; border-radius: 4px;
      }
      .metric-label { color: #a0a0b0; font-size: 12px; text-transform: uppercase; letter-spacing: 0.5px; }
      .metric-value { color: #ecf0f1; font-size: 24px; font-weight: 700; }
      .risk-high { border-left-color: #e74c3c !important; }
      .risk-medium { border-left-color: #f39c12 !important; }
      .risk-low { border-left-color: #27ae60 !important; }
      .summary-section {
        background-color: #16213e; padding: 20px; border-radius: 8px;
        color: #ecf0f1; margin-bottom: 15px; border-left: 4px solid #00d2ff;
      }
      .summary-section h4 { color: #00d2ff; margin-top: 0; }
      hr { border-color: #4a4a4a; }
      .weight-ok { color: #27ae60; font-weight: bold; }
      .weight-err { color: #e74c3c; font-weight: bold; }
    "))),

        # KPI Row
        fluidRow(
            valueBoxOutput("sharpe_box", width = 3),
            valueBoxOutput("var_box", width = 3),
            valueBoxOutput("max_dd_box", width = 3),
            valueBoxOutput("beta_box", width = 3)
        ),

        # Tabs
        fluidRow(
            column(
                12,
                tabBox(
                    width = 12, id = "main_tabs",

                    # Tab 1: Portfolio Overview
                    tabPanel(
                        title = span(icon("chart-pie"), " Portfolio Overview"),
                        fluidRow(
                            column(
                                5,
                                box(
                                    width = 12, title = "Allocation Breakdown",
                                    plotlyOutput("allocation_pie", height = "350px")
                                )
                            ),
                            column(
                                7,
                                box(
                                    width = 12, title = "Cumulative Performance vs Benchmark",
                                    plotlyOutput("perf_chart", height = "350px")
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                6,
                                box(
                                    width = 12, title = "Rolling 60-Day Correlation to Benchmark",
                                    plotlyOutput("rolling_corr_chart", height = "300px")
                                )
                            ),
                            column(
                                6,
                                box(
                                    width = 12, title = "Asset Correlation Matrix",
                                    plotlyOutput("corr_heatmap", height = "300px")
                                )
                            )
                        )
                    ),

                    # Tab 2: VaR & Risk Metrics
                    tabPanel(
                        title = span(icon("triangle-exclamation"), " VaR & Risk"),
                        fluidRow(
                            column(
                                4,
                                box(
                                    width = 12, title = "Value at Risk Summary",
                                    uiOutput("var_summary")
                                )
                            ),
                            column(
                                8,
                                box(
                                    width = 12, title = "Return Distribution with VaR Thresholds",
                                    plotlyOutput("var_histogram", height = "400px")
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                12,
                                box(
                                    width = 12, title = "Performance Attribution Metrics",
                                    DTOutput("metrics_table")
                                )
                            )
                        )
                    ),

                    # Tab 3: Monte Carlo Simulation
                    tabPanel(
                        title = span(icon("dice"), " Monte Carlo"),
                        fluidRow(
                            column(
                                8,
                                box(
                                    width = 12, title = "Simulated Portfolio Paths (1-Year Horizon)",
                                    plotlyOutput("mc_paths", height = "450px")
                                )
                            ),
                            column(
                                4,
                                box(
                                    width = 12, title = "Simulation Statistics",
                                    uiOutput("mc_stats")
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                12,
                                box(
                                    width = 12, title = "Distribution of Terminal Returns",
                                    plotlyOutput("mc_histogram", height = "300px")
                                )
                            )
                        )
                    ),

                    # Tab 4: Stress Testing
                    tabPanel(
                        title = span(icon("bolt"), " Stress Testing"),
                        fluidRow(
                            column(
                                12,
                                box(
                                    width = 12, title = "Portfolio Drawdown Under Historical Crises",
                                    plotlyOutput("stress_chart", height = "450px"),
                                    p("Simulates portfolio performance during 6 real market crashes using actual historical returns.",
                                        style = "color: #a0a0b0; font-style: italic; margin-top: 10px;"
                                    )
                                )
                            )
                        ),
                        fluidRow(
                            column(
                                12,
                                box(
                                    width = 12, title = "Stress Test Results Summary",
                                    DTOutput("stress_table")
                                )
                            )
                        )
                    ),

                    # Tab 5: Research Report
                    tabPanel(
                        title = span(icon("file-lines"), " Risk Report"),
                        fluidRow(
                            column(
                                12,
                                actionButton("generate_report", "Generate Risk Report",
                                    icon = icon("wand-magic-sparkles"),
                                    class = "btn-primary btn-lg",
                                    style = "margin-bottom: 20px;"
                                ),
                                uiOutput("risk_report")
                            )
                        )
                    )
                )
            )
        )
    )
)
server <- function(input, output, session) {
    # ---- WEIGHT VALIDATION ----
    portfolio_weights <- reactive({
        tickers <- c(
            input$ticker1, input$ticker2, input$ticker3,
            input$ticker4, input$ticker5, input$ticker6
        )
        weights <- c(
            input$weight1, input$weight2, input$weight3,
            input$weight4, input$weight5, input$weight6
        )
        valid <- nchar(trimws(tickers)) > 0 & weights > 0
        list(
            tickers = toupper(trimws(tickers[valid])),
            weights = weights[valid] / 100,
            raw_sum = sum(weights[valid])
        )
    })

    output$weight_validation <- renderUI({
        pw <- portfolio_weights()
        if (abs(pw$raw_sum - 100) < 0.01) {
            div(
                class = "weight-ok", icon("check-circle"),
                paste0("Weights sum to 100% (", length(pw$tickers), " assets)")
            )
        } else {
            div(
                class = "weight-err", icon("exclamation-triangle"),
                paste0("Weights sum to ", pw$raw_sum, "% — must equal 100%")
            )
        }
    })

    # ---- MAIN DATA FETCH ----
    analysis_data <- eventReactive(input$run_analysis, {
        pw <- portfolio_weights()
        validate(need(abs(pw$raw_sum - 100) < 0.01, "Weights must sum to 100%."))
        validate(need(length(pw$tickers) >= 2, "At least 2 assets required."))

        withProgress(message = "Running full analysis...", value = 0, {
            start_date <- Sys.Date() - (input$lookback_years * 365)
            n_assets <- length(pw$tickers)

            # Fetch individual asset returns
            incProgress(0.1, detail = "Fetching asset data...")
            asset_returns_list <- lapply(pw$tickers, function(t) fetch_returns(t, start_date))
            names(asset_returns_list) <- pw$tickers

            # Check for failures
            failed <- sapply(asset_returns_list, is.null)
            validate(need(!all(failed), "Could not fetch data for any assets."))
            if (any(failed)) {
                showNotification(paste("Failed to load:", paste(pw$tickers[failed], collapse = ", ")),
                    type = "warning"
                )
            }

            # Filter to valid assets
            valid_idx <- !failed
            valid_tickers <- pw$tickers[valid_idx]
            valid_weights_raw <- pw$weights[valid_idx]
            valid_weights <- valid_weights_raw / sum(valid_weights_raw) # renormalize
            valid_returns <- asset_returns_list[valid_idx]

            # Merge all returns into a matrix
            incProgress(0.15, detail = "Aligning time series...")
            merged <- do.call(merge, valid_returns)
            colnames(merged) <- valid_tickers
            merged <- na.omit(merged)
            returns_matrix <- as.matrix(coredata(merged))

            # Portfolio returns (weighted)
            port_returns <- xts(returns_matrix %*% valid_weights,
                order.by = index(merged)
            )
            colnames(port_returns) <- "Portfolio"

            # Benchmark returns
            incProgress(0.1, detail = "Loading benchmark...")
            bench_returns <- fetch_returns(input$benchmark, start_date)
            validate(need(!is.null(bench_returns), "Could not load benchmark data."))

            # Align portfolio and benchmark
            aligned <- merge(port_returns, bench_returns, join = "inner")
            aligned <- na.omit(aligned)
            colnames(aligned) <- c("Portfolio", "Benchmark")

            # Risk metrics
            incProgress(0.15, detail = "Computing risk metrics...")
            conf <- input$confidence
            rf <- input$rf_rate / 100
            port_vec <- as.numeric(aligned[, "Portfolio"])

            parametric_var <- calc_parametric_var(port_vec, conf)
            historical_var <- calc_historical_var(port_vec, conf)
            cvar <- calc_cvar(port_vec, conf)

            perf <- calc_performance_metrics(aligned[, "Portfolio"], aligned[, "Benchmark"], rf)

            # Monte Carlo
            incProgress(0.2, detail = paste0("Running ", input$n_sims, " Monte Carlo simulations..."))
            mc <- run_monte_carlo(returns_matrix, valid_weights, n_sims = input$n_sims, horizon = 252)

            # Stress Testing — fetch separate long history (back to 2000)
            incProgress(0.1, detail = "Fetching full history for stress tests...")
            stress_start <- as.Date("2000-01-01")
            stress_returns_list <- lapply(valid_tickers, function(t) {
                fetch_returns(t, stress_start)
            })
            names(stress_returns_list) <- valid_tickers

            # Filter to tickers that have long history
            stress_valid <- !sapply(stress_returns_list, is.null)
            stress_tickers <- valid_tickers[stress_valid]
            stress_wts_raw <- valid_weights[stress_valid]
            stress_wts <- stress_wts_raw / sum(stress_wts_raw)

            # Merge long history
            if (length(stress_tickers) >= 2) {
                stress_merged <- do.call(merge, stress_returns_list[stress_valid])
                colnames(stress_merged) <- stress_tickers
                stress_merged <- na.omit(stress_merged)
            } else {
                stress_merged <- NULL
            }

            incProgress(0.05, detail = "Simulating stress scenarios...")
            stress_results <- list()
            stress_paths_list <- list()

            if (!is.null(stress_merged) && nrow(stress_merged) > 0) {
                for (scenario_name in names(STRESS_SCENARIOS)) {
                    sc <- STRESS_SCENARIOS[[scenario_name]]
                    tryCatch(
                        {
                            stress_data <- stress_merged[paste0(sc$start, "/", sc$end)]
                            if (nrow(stress_data) >= 5) {
                                stress_port <- as.numeric(
                                    as.matrix(coredata(stress_data)) %*% stress_wts
                                )
                                cum_ret <- cumprod(1 + stress_port) - 1
                                max_dd <- min(cum_ret)
                                total_ret <- tail(cum_ret, 1)

                                stress_results[[scenario_name]] <- data.frame(
                                    Scenario = scenario_name,
                                    Days = nrow(stress_data),
                                    TotalReturn = total_ret,
                                    MaxDrawdown = max_dd,
                                    WorstDay = min(stress_port),
                                    BestDay = max(stress_port),
                                    stringsAsFactors = FALSE
                                )

                                stress_paths_list[[scenario_name]] <- data.frame(
                                    Day = seq_along(cum_ret),
                                    CumReturn = cum_ret,
                                    Scenario = scenario_name,
                                    stringsAsFactors = FALSE
                                )
                            }
                        },
                        error = function(e) NULL
                    )
                }
            }

            stress_df <- if (length(stress_results) > 0) {
                do.call(rbind, stress_results)
            } else {
                NULL
            }
            stress_paths_df <- if (length(stress_paths_list) > 0) {
                do.call(rbind, stress_paths_list)
            } else {
                NULL
            }

            incProgress(0.15, detail = "Done!")

            list(
                tickers = valid_tickers, weights = valid_weights,
                returns_matrix = returns_matrix, merged = merged,
                port_returns = aligned[, "Portfolio"],
                bench_returns = aligned[, "Benchmark"],
                aligned = aligned,
                parametric_var = parametric_var,
                historical_var = historical_var,
                cvar = cvar,
                perf = perf, mc = mc,
                stress_df = stress_df,
                stress_paths_df = stress_paths_df,
                confidence = conf, rf_rate = rf
            )
        })
    })

    # ---- VALUE BOXES ----
    output$sharpe_box <- renderValueBox({
        data <- tryCatch(analysis_data(), error = function(e) NULL)
        val <- if (!is.null(data)) round(data$perf$sharpe, 2) else "—"
        clr <- if (!is.null(data) && data$perf$sharpe > 1) "green" else if (!is.null(data) && data$perf$sharpe > 0.5) "yellow" else "red"
        valueBox(val, "Sharpe Ratio", icon = icon("chart-line"), color = clr)
    })

    output$var_box <- renderValueBox({
        data <- tryCatch(analysis_data(), error = function(e) NULL)
        val <- if (!is.null(data)) fmt_pct(data$parametric_var) else "—"
        valueBox(val, paste0("Daily VaR (", if (!is.null(data)) paste0(data$confidence * 100, "%") else "95%", ")"),
            icon = icon("triangle-exclamation"), color = "red"
        )
    })

    output$max_dd_box <- renderValueBox({
        data <- tryCatch(analysis_data(), error = function(e) NULL)
        val <- if (!is.null(data)) fmt_pct(abs(data$perf$max_drawdown)) else "—"
        valueBox(val, "Max Drawdown", icon = icon("arrow-trend-down"), color = "purple")
    })

    output$beta_box <- renderValueBox({
        data <- tryCatch(analysis_data(), error = function(e) NULL)
        val <- if (!is.null(data) && !is.na(data$perf$beta)) round(data$perf$beta, 2) else "—"
        clr <- if (!is.null(data) && !is.na(data$perf$beta) && data$perf$beta > 1) "red" else "blue"
        valueBox(val, paste0("Beta vs ", input$benchmark), icon = icon("scale-balanced"), color = clr)
    })

    # ---- TAB 1: PORTFOLIO OVERVIEW ----
    output$allocation_pie <- renderPlotly({
        data <- analysis_data()
        df <- data.frame(Ticker = data$tickers, Allocation = data$weights)
        colors <- c("#00d2ff", "#e94560", "#f39c12", "#27ae60", "#9b59b6", "#3498db")
        plot_ly(df,
            labels = ~Ticker, values = ~Allocation, type = "pie",
            marker = list(colors = colors[seq_along(data$tickers)]),
            textinfo = "label+percent", textfont = list(color = "#ecf0f1"),
            hoverinfo = "label+percent+value"
        ) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"), showlegend = TRUE,
                legend = list(font = list(color = "#ecf0f1"))
            )
    })

    output$perf_chart <- renderPlotly({
        data <- analysis_data()
        aligned <- data$aligned
        port_cum <- cumprod(1 + aligned[, "Portfolio"]) - 1
        bench_cum <- cumprod(1 + aligned[, "Benchmark"]) - 1
        df <- data.frame(
            Date = index(aligned),
            Portfolio = as.numeric(port_cum) * 100,
            Benchmark = as.numeric(bench_cum) * 100
        )
        plot_ly(df, x = ~Date) |>
            add_lines(y = ~Portfolio, name = "Portfolio", line = list(color = "#00d2ff", width = 2.5)) |>
            add_lines(y = ~Benchmark, name = input$benchmark, line = list(color = "#e94560", width = 2, dash = "dash")) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"),
                xaxis = list(gridcolor = "#2a2a3e", title = ""),
                yaxis = list(gridcolor = "#2a2a3e", title = "Cumulative Return (%)", ticksuffix = "%"),
                legend = list(orientation = "h", y = -0.15), hovermode = "x unified"
            )
    })

    output$rolling_corr_chart <- renderPlotly({
        data <- analysis_data()
        aligned <- data$aligned
        if (nrow(aligned) < 60) {
            return(plotly_empty())
        }
        roll_cor <- rollapply(aligned,
            width = 60, FUN = function(x) cor(x[, 1], x[, 2]),
            by.column = FALSE, align = "right"
        )
        df <- data.frame(Date = index(roll_cor), Correlation = as.numeric(coredata(roll_cor)))
        df <- df[!is.na(df$Correlation), ]
        plot_ly(df,
            x = ~Date, y = ~Correlation, type = "scatter", mode = "lines",
            line = list(color = "#f39c12", width = 2),
            fill = "tozeroy", fillcolor = "rgba(243,156,18,0.15)"
        ) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"),
                xaxis = list(gridcolor = "#2a2a3e", title = ""),
                yaxis = list(gridcolor = "#2a2a3e", title = "Correlation", range = c(-1, 1))
            )
    })

    output$corr_heatmap <- renderPlotly({
        data <- analysis_data()
        cor_mat <- cor(data$returns_matrix, use = "pairwise.complete.obs")
        plot_ly(
            x = data$tickers, y = data$tickers, z = round(cor_mat, 2),
            type = "heatmap",
            colorscale = list(c(0, "#e74c3c"), c(0.5, "#1a1a2e"), c(1, "#00d2ff")),
            zmin = -1, zmax = 1,
            text = round(cor_mat, 2), texttemplate = "%{text}",
            hovertemplate = "%{x} vs %{y}: %{z:.2f}<extra></extra>"
        ) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"),
                xaxis = list(title = ""), yaxis = list(title = "", autorange = "reversed")
            )
    })

    # ---- TAB 2: VaR & RISK ----
    output$var_summary <- renderUI({
        data <- analysis_data()
        risk_class <- function(var_val) {
            if (var_val > 0.03) "risk-high" else if (var_val > 0.015) "risk-medium" else "risk-low"
        }
        tags$div(
            div(
                class = paste("metric-card", risk_class(data$parametric_var)),
                div(class = "metric-label", "Parametric VaR (Daily)"),
                div(class = "metric-value", fmt_pct(data$parametric_var))
            ),
            div(
                class = paste("metric-card", risk_class(data$historical_var)),
                div(class = "metric-label", "Historical VaR (Daily)"),
                div(class = "metric-value", fmt_pct(data$historical_var))
            ),
            div(
                class = paste("metric-card", risk_class(data$cvar)),
                div(class = "metric-label", "CVaR / Expected Shortfall"),
                div(class = "metric-value", fmt_pct(data$cvar))
            ),
            hr(),
            div(
                class = "metric-card",
                div(class = "metric-label", "Annualized Return"),
                div(
                    class = "metric-value", style = paste0("color:", ifelse(data$perf$ann_return > 0, "#27ae60", "#e74c3c")),
                    fmt_pct(data$perf$ann_return)
                )
            ),
            div(
                class = "metric-card",
                div(class = "metric-label", "Annualized Volatility"),
                div(class = "metric-value", fmt_pct(data$perf$ann_vol))
            ),
            div(
                class = "metric-card",
                div(class = "metric-label", "Calmar Ratio"),
                div(class = "metric-value", if (!is.na(data$perf$calmar)) round(data$perf$calmar, 2) else "N/A")
            )
        )
    })

    output$var_histogram <- renderPlotly({
        data <- analysis_data()
        port_vec <- as.numeric(data$port_returns) * 100
        h <- hist(port_vec, breaks = 80, plot = FALSE)
        ymax <- max(h$counts) * 0.9
        bin_size <- diff(range(port_vec)) / 80
        plot_ly(
            x = port_vec, type = "histogram",
            xbins = list(size = bin_size),
            marker = list(color = "rgba(0,210,255,0.5)", line = list(color = "#00d2ff", width = 0.5)),
            name = "Daily Returns"
        ) |>
            add_segments(
                x = -data$parametric_var * 100, xend = -data$parametric_var * 100,
                y = 0, yend = ymax,
                line = list(color = "#e74c3c", width = 2, dash = "dash"), name = "Parametric VaR"
            ) |>
            add_segments(
                x = -data$historical_var * 100, xend = -data$historical_var * 100,
                y = 0, yend = ymax,
                line = list(color = "#f39c12", width = 2, dash = "dash"), name = "Historical VaR"
            ) |>
            add_segments(
                x = -data$cvar * 100, xend = -data$cvar * 100,
                y = 0, yend = ymax,
                line = list(color = "#9b59b6", width = 2, dash = "dash"), name = "CVaR (ES)"
            ) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"),
                xaxis = list(gridcolor = "#2a2a3e", title = "Daily Return (%)", ticksuffix = "%"),
                yaxis = list(gridcolor = "#2a2a3e", title = "Frequency"),
                legend = list(orientation = "h", y = -0.2), bargap = 0.02
            )
    })

    output$metrics_table <- renderDT({
        data <- analysis_data()
        p <- data$perf
        df <- data.frame(
            Metric = c(
                "Annualized Return", "Annualized Volatility", "Sharpe Ratio",
                "Sortino Ratio", paste0("Beta (vs ", input$benchmark, ")"),
                paste0("Alpha (vs ", input$benchmark, ")"), "Max Drawdown", "Calmar Ratio",
                paste0("Parametric VaR (", data$confidence * 100, "%)"),
                paste0("Historical VaR (", data$confidence * 100, "%)"),
                "CVaR / Expected Shortfall"
            ),
            Value = c(
                fmt_pct(p$ann_return), fmt_pct(p$ann_vol),
                round(p$sharpe, 3), if (!is.na(p$sortino)) round(p$sortino, 3) else "N/A",
                if (!is.na(p$beta)) round(p$beta, 3) else "N/A",
                if (!is.na(p$alpha)) fmt_pct(p$alpha) else "N/A",
                fmt_pct(p$max_drawdown),
                if (!is.na(p$calmar)) round(p$calmar, 3) else "N/A",
                fmt_pct(data$parametric_var), fmt_pct(data$historical_var), fmt_pct(data$cvar)
            ),
            stringsAsFactors = FALSE
        )
        datatable(df,
            options = list(dom = "t", pageLength = 15, ordering = FALSE),
            rownames = FALSE, class = "cell-border stripe"
        ) |>
            formatStyle(columns = c("Metric", "Value"), backgroundColor = "#1a1a2e", color = "#ecf0f1") |>
            formatStyle("Metric", fontWeight = "bold")
    })

    # ---- TAB 3: MONTE CARLO ----
    output$mc_paths <- renderPlotly({
        data <- analysis_data()
        mc <- data$mc
        n_show <- min(200, ncol(mc$cumulative))
        p <- plot_ly()
        for (i in seq_len(n_show)) {
            p <- p |> add_lines(
                x = 1:nrow(mc$cumulative), y = mc$cumulative[, i] * 100,
                line = list(color = "rgba(0,210,255,0.07)", width = 0.8),
                showlegend = FALSE, hoverinfo = "skip"
            )
        }
        # Median path
        median_path <- apply(mc$cumulative, 1, median) * 100
        p5_path <- apply(mc$cumulative, 1, quantile, 0.05) * 100
        p95_path <- apply(mc$cumulative, 1, quantile, 0.95) * 100
        p <- p |>
            add_lines(
                x = 1:nrow(mc$cumulative), y = median_path,
                line = list(color = "#f39c12", width = 3), name = "Median"
            ) |>
            add_lines(
                x = 1:nrow(mc$cumulative), y = p5_path,
                line = list(color = "#e74c3c", width = 2, dash = "dash"), name = "5th Percentile"
            ) |>
            add_lines(
                x = 1:nrow(mc$cumulative), y = p95_path,
                line = list(color = "#27ae60", width = 2, dash = "dash"), name = "95th Percentile"
            ) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"),
                xaxis = list(gridcolor = "#2a2a3e", title = "Trading Days"),
                yaxis = list(gridcolor = "#2a2a3e", title = "Cumulative Return (%)", ticksuffix = "%"),
                legend = list(orientation = "h", y = -0.15)
            )
        p
    })

    output$mc_stats <- renderUI({
        data <- analysis_data()
        mc <- data$mc
        tags$div(
            div(
                class = "metric-card",
                div(class = "metric-label", "Simulations Run"),
                div(class = "metric-value", comma(length(mc$final_returns)))
            ),
            div(
                class = "metric-card",
                div(class = "metric-label", "Median 1-Year Return"),
                div(
                    class = "metric-value", style = paste0("color:", ifelse(mc$median_return > 0, "#27ae60", "#e74c3c")),
                    fmt_pct(mc$median_return)
                )
            ),
            div(
                class = "metric-card",
                div(class = "metric-label", "Mean 1-Year Return"),
                div(class = "metric-value", fmt_pct(mc$mean_return))
            ),
            div(
                class = "metric-card risk-high",
                div(class = "metric-label", "VaR 95% (1-Year)"),
                div(class = "metric-value", fmt_pct(mc$var_95))
            ),
            div(
                class = "metric-card risk-high",
                div(class = "metric-label", "VaR 99% (1-Year)"),
                div(class = "metric-value", fmt_pct(mc$var_99))
            ),
            div(
                class = "metric-card",
                div(class = "metric-label", "Probability of Loss"),
                div(
                    class = "metric-value",
                    style = paste0("color:", ifelse(mc$prob_loss > 0.3, "#e74c3c", "#27ae60")),
                    fmt_pct(mc$prob_loss)
                )
            )
        )
    })

    output$mc_histogram <- renderPlotly({
        data <- analysis_data()
        mc <- data$mc
        finals <- mc$final_returns * 100
        h <- hist(finals, breaks = 100, plot = FALSE)
        ymax <- max(h$counts) * 0.9
        bin_size <- diff(range(finals)) / 100
        plot_ly(
            x = finals, type = "histogram",
            xbins = list(size = bin_size),
            marker = list(color = "rgba(0,210,255,0.5)", line = list(color = "#00d2ff", width = 0.5)),
            name = "Terminal Returns"
        ) |>
            add_segments(
                x = 0, xend = 0, y = 0, yend = ymax,
                line = list(color = "#ecf0f1", width = 2), name = "Break Even"
            ) |>
            add_segments(
                x = -mc$var_95 * 100, xend = -mc$var_95 * 100, y = 0, yend = ymax,
                line = list(color = "#e74c3c", width = 2, dash = "dash"), name = "VaR 95%"
            ) |>
            layout(
                paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
                font = list(color = "#ecf0f1"),
                xaxis = list(gridcolor = "#2a2a3e", title = "1-Year Return (%)", ticksuffix = "%"),
                yaxis = list(gridcolor = "#2a2a3e", title = "Frequency"),
                legend = list(orientation = "h", y = -0.2), bargap = 0.02
            )
    })

    # ---- TAB 4: STRESS TESTING ----
    output$stress_chart <- renderPlotly({
        data <- analysis_data()
        req(data$stress_paths_df)
        df <- data$stress_paths_df
        df$CumReturn <- df$CumReturn * 100
        colors <- c("#e74c3c", "#f39c12", "#9b59b6", "#3498db", "#27ae60", "#00d2ff")
        scenarios <- unique(df$Scenario)
        p <- plot_ly()
        for (i in seq_along(scenarios)) {
            sdf <- df[df$Scenario == scenarios[i], ]
            p <- p |> add_lines(
                data = sdf, x = ~Day, y = ~CumReturn,
                name = scenarios[i], line = list(color = colors[i], width = 2.5)
            )
        }
        p |> layout(
            paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
            font = list(color = "#ecf0f1"),
            xaxis = list(gridcolor = "#2a2a3e", title = "Trading Days Into Crisis"),
            yaxis = list(gridcolor = "#2a2a3e", title = "Cumulative Return (%)", ticksuffix = "%"),
            legend = list(font = list(size = 10, color = "#ecf0f1")),
            hovermode = "x unified"
        )
    })

    output$stress_table <- renderDT({
        data <- analysis_data()
        req(data$stress_df)
        df <- data$stress_df
        df$TotalReturn <- fmt_pct(df$TotalReturn)
        df$MaxDrawdown <- fmt_pct(df$MaxDrawdown)
        df$WorstDay <- fmt_pct(df$WorstDay)
        df$BestDay <- fmt_pct(df$BestDay)
        colnames(df) <- c("Scenario", "Duration (Days)", "Total Return", "Max Drawdown", "Worst Day", "Best Day")
        datatable(df,
            options = list(dom = "t", pageLength = 10, ordering = FALSE),
            rownames = FALSE, class = "cell-border stripe"
        ) |>
            formatStyle(columns = colnames(df), backgroundColor = "#1a1a2e", color = "#ecf0f1") |>
            formatStyle("Scenario", fontWeight = "bold")
    })

    # ---- TAB 5: RISK REPORT ----
    output$risk_report <- renderUI({
        req(input$generate_report)
        data <- analysis_data()
        p <- data$perf
        mc <- data$mc

        rec <- if (p$sharpe > 1 && abs(p$max_drawdown) < 0.25) {
            list(text = "LOW RISK", class = "background-color: #27ae60;")
        } else if (p$sharpe > 0.5 && abs(p$max_drawdown) < 0.40) {
            list(text = "MODERATE RISK", class = "background-color: #f39c12;")
        } else {
            list(text = "HIGH RISK", class = "background-color: #e74c3c;")
        }

        portfolio_str <- paste(paste0(data$tickers, " (", round(data$weights * 100), "%)"), collapse = ", ")

        tags$div(
            style = "max-width: 900px;",
            div(
                class = "summary-section",
                style = "text-align: center; border-left: none; border-top: 4px solid #00d2ff;",
                h2("Portfolio Risk Analysis Report", style = "color: #ecf0f1; margin: 0;"),
                p(paste("Generated:", format(Sys.Date(), "%B %d, %Y")),
                    style = "color: #a0a0b0; margin: 5px 0 0 0;"
                )
            ),

            # Risk Rating
            div(
                style = paste0(rec$class, " padding: 20px; border-radius: 8px; text-align: center; margin-bottom: 15px;"),
                h3(style = "color: white; margin: 0;", paste("Risk Assessment:", rec$text)),
                p(
                    style = "color: white; margin: 5px 0 0 0; font-size: 16px;",
                    paste0(
                        "Sharpe: ", round(p$sharpe, 2), " | Max DD: ", fmt_pct(p$max_drawdown),
                        " | Beta: ", if (!is.na(p$beta)) round(p$beta, 2) else "N/A"
                    )
                )
            ),

            # Portfolio constituents
            div(
                class = "summary-section",
                h4("Portfolio Composition"),
                p(paste0("This portfolio consists of ", length(data$tickers), " assets: ", portfolio_str, ".")),
                p(paste0("Benchmarked against ", input$benchmark, " over a ", input$lookback_years, "-year lookback period."))
            ),

            # Performance
            div(
                class = "summary-section",
                h4("Performance Summary"),
                tags$table(
                    style = "width: 100%; color: #ecf0f1;",
                    tags$tr(
                        tags$td(style = "padding: 5px;", "Annualized Return:"),
                        tags$td(style = "padding: 5px; text-align: right; font-weight: bold;", fmt_pct(p$ann_return)),
                        tags$td(style = "padding: 5px;", "Annualized Vol:"),
                        tags$td(style = "padding: 5px; text-align: right; font-weight: bold;", fmt_pct(p$ann_vol))
                    ),
                    tags$tr(
                        tags$td(style = "padding: 5px;", "Sharpe Ratio:"),
                        tags$td(style = "padding: 5px; text-align: right; font-weight: bold;", round(p$sharpe, 3)),
                        tags$td(style = "padding: 5px;", "Sortino Ratio:"),
                        tags$td(
                            style = "padding: 5px; text-align: right; font-weight: bold;",
                            if (!is.na(p$sortino)) round(p$sortino, 3) else "N/A"
                        )
                    ),
                    tags$tr(
                        tags$td(style = "padding: 5px;", "Max Drawdown:"),
                        tags$td(style = "padding: 5px; text-align: right; font-weight: bold;", fmt_pct(p$max_drawdown)),
                        tags$td(style = "padding: 5px;", "Calmar Ratio:"),
                        tags$td(
                            style = "padding: 5px; text-align: right; font-weight: bold;",
                            if (!is.na(p$calmar)) round(p$calmar, 3) else "N/A"
                        )
                    )
                )
            ),

            # VaR Section
            div(
                class = "summary-section",
                h4("Value at Risk Analysis"),
                p(paste0("At the ", data$confidence * 100, "% confidence level, the portfolio's daily Value at Risk measures are:")),
                tags$ul(
                    style = "color: #a0a0b0;",
                    tags$li(paste0("Parametric VaR (Normal): ", fmt_pct(data$parametric_var))),
                    tags$li(paste0("Historical VaR: ", fmt_pct(data$historical_var))),
                    tags$li(paste0("Conditional VaR (Expected Shortfall): ", fmt_pct(data$cvar)))
                )
            ),

            # MC Section
            div(
                class = "summary-section",
                h4("Monte Carlo Simulation Results"),
                p(paste0(
                    comma(length(mc$final_returns)), " correlated return paths were simulated using the multivariate normal distribution ",
                    "with the historical covariance structure of the portfolio assets."
                )),
                tags$ul(
                    style = "color: #a0a0b0;",
                    tags$li(paste0("Median 1-year return: ", fmt_pct(mc$median_return))),
                    tags$li(paste0("Probability of loss over 1 year: ", fmt_pct(mc$prob_loss))),
                    tags$li(paste0("1-year VaR (95%): ", fmt_pct(mc$var_95))),
                    tags$li(paste0("1-year VaR (99%): ", fmt_pct(mc$var_99)))
                )
            ),

            # Disclaimer
            div(
                style = "padding: 15px; color: #666; font-size: 11px; font-style: italic; text-align: center;",
                p("This report was generated for educational and analytical purposes only.
             It does not constitute investment advice. Past performance does not guarantee future results.")
            )
        )
    })
}

# ============================================================================
# RUN
# ============================================================================

shinyApp(ui = ui, server = server)
