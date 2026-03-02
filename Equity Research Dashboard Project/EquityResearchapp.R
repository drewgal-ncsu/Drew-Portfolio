# ============================================================================
# EQUITY RESEARCH DASHBOARD
# A Shiny-based equity analysis tool for DCF valuation, comparable company
# analysis, and automated research summary generation.
#
# Author: Drew [Last Name]
# GitHub: github.com/[username]/equity-research-dashboard
# ============================================================================
# List of required packages for a Finance Micro-SaaS
pkgs <- c(
  "shiny", "shinydashboard", "quantmod", "tidyverse",
  "plotly", "DT", "scales", "glue"
)

# Install any missing packages
install.packages(setdiff(pkgs, rownames(installed.packages())))
install.packages("shiny")
install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

# Verify by loading them all
lapply(pkgs, library, character.only = TRUE)
library(shiny)
library(shinydashboard)
library(quantmod)
library(tidyverse)
library(plotly)
library(DT)
library(scales)
library(glue)

# Prevent tidyverse NSE variable bindings from triggering IDE warnings
WACC <- Terminal_Growth <- Price <- NULL
`Market Cap` <- `P/E` <- Volume <- NULL
Year <- Revenue <- `Free Cash Flow` <- `Discount Factor` <- Ticker <- NULL
# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Safely fetch stock data with error handling
fetch_stock_data <- function(ticker) {
  tryCatch(
    {
      getQuote(ticker, what = yahooQF(c(
        "Name", "Last Trade (Price Only)", "Market Capitalization",
        "Earnings/Share", "P/E Ratio", "Dividend Yield",
        "Book Value", "EBITDA", "Revenue", "50-day Moving Average",
        "200-day Moving Average"
      )))
    },
    error = function(e) {
      NULL
    }
  )
}

# Fetch historical price data
fetch_price_history <- function(ticker, years = 3) {
  tryCatch(
    {
      start_date <- Sys.Date() - (years * 365)
      getSymbols(ticker, src = "yahoo", from = start_date, auto.assign = FALSE)
    },
    error = function(e) {
      NULL
    }
  )
}

# Fetch key financial stats
fetch_key_stats <- function(ticker) {
  tryCatch(
    {
      getQuote(ticker)
    },
    error = function(e) {
      NULL
    }
  )
}

# DCF Valuation Model
run_dcf <- function(current_revenue, revenue_growth, ebitda_margin,
                    tax_rate, capex_pct, nwc_change_pct,
                    wacc, terminal_growth, projection_years,
                    shares_outstanding, net_debt) {
  # Project free cash flows
  years <- 1:projection_years
  revenues <- current_revenue * (1 + revenue_growth)^years
  ebitda <- revenues * ebitda_margin
  taxes <- ebitda * tax_rate
  capex <- revenues * capex_pct
  nwc_changes <- revenues * nwc_change_pct

  # Unlevered Free Cash Flow
  fcf <- ebitda - taxes - capex - nwc_changes

  # Terminal Value (Gordon Growth Model)
  terminal_fcf <- fcf[projection_years] * (1 + terminal_growth)
  terminal_value <- terminal_fcf / (wacc - terminal_growth)

  # Discount factors
  discount_factors <- 1 / (1 + wacc)^years
  terminal_discount <- 1 / (1 + wacc)^projection_years

  # Present values
  pv_fcf <- fcf * discount_factors
  pv_terminal <- terminal_value * terminal_discount

  # Enterprise & Equity Value

  enterprise_value <- sum(pv_fcf) + pv_terminal
  equity_value <- enterprise_value - net_debt
  price_per_share <- equity_value / shares_outstanding

  # Build projection table
  projection_table <- tibble::tibble(
    Year = paste0("Year ", years),
    Revenue = revenues,
    EBITDA = ebitda,
    `Free Cash Flow` = fcf,
    `Discount Factor` = discount_factors,
    `PV of FCF` = pv_fcf
  )

  list(
    projection_table = projection_table,
    enterprise_value = enterprise_value,
    equity_value = equity_value,
    price_per_share = price_per_share,
    pv_terminal = pv_terminal,
    pv_fcf_total = sum(pv_fcf),
    terminal_value = terminal_value,
    fcf = fcf
  )
}

# Sensitivity analysis matrix
run_sensitivity <- function(current_revenue, revenue_growth, ebitda_margin,
                            tax_rate, capex_pct, nwc_change_pct,
                            wacc, terminal_growth, projection_years,
                            shares_outstanding, net_debt) {
  wacc_range <- seq(wacc - 0.02, wacc + 0.02, by = 0.005)
  growth_range <- seq(terminal_growth - 0.01, terminal_growth + 0.01, by = 0.005)

  # Ensure no invalid combos (wacc must be > growth)
  matrix_data <- expand.grid(WACC = wacc_range, Terminal_Growth = growth_range) |>
    dplyr::filter(.data$WACC > .data$Terminal_Growth + 0.005) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Price = run_dcf(
        current_revenue, revenue_growth, ebitda_margin,
        tax_rate, capex_pct, nwc_change_pct,
        WACC, Terminal_Growth, projection_years,
        shares_outstanding, net_debt
      )$price_per_share
    ) |>
    dplyr::ungroup()

  matrix_data
}

# Format currency
fmt_currency <- function(x, prefix = "$") {
  ifelse(abs(x) >= 1e9,
    paste0(prefix, round(x / 1e9, 2), "B"),
    ifelse(abs(x) >= 1e6,
      paste0(prefix, round(x / 1e6, 2), "M"),
      paste0(prefix, comma(round(x, 2)))
    )
  )
}

# ============================================================================
# UI
# ============================================================================

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = span(
      icon("chart-line"), " Equity Research Dashboard"
    ),
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 300,

    # Ticker Input
    div(
      style = "padding: 15px;",
      h4(icon("search"), "Company Selection", style = "color: #ecf0f1;"),
      textInput("ticker", "Ticker Symbol", value = "AAPL", placeholder = "e.g., AAPL, MSFT, GOOG"),
      actionButton("analyze_btn", "Analyze",
        icon = icon("magnifying-glass-chart"),
        class = "btn-primary btn-block",
        style = "margin-bottom: 20px;"
      ),
      hr(style = "border-color: #4a4a4a;"),

      # DCF Assumptions
      h4(icon("calculator"), "DCF Assumptions", style = "color: #ecf0f1;"),
      numericInput("current_revenue", "Current Revenue ($M)",
        value = 383000, min = 0, step = 1000
      ),
      sliderInput("revenue_growth", "Revenue Growth Rate",
        min = -0.10, max = 0.30, value = 0.05, step = 0.01,
        post = "%", pre = ""
      ),
      sliderInput("ebitda_margin", "EBITDA Margin",
        min = 0.05, max = 0.60, value = 0.33, step = 0.01
      ),
      sliderInput("tax_rate", "Effective Tax Rate",
        min = 0.10, max = 0.35, value = 0.21, step = 0.01
      ),
      sliderInput("capex_pct", "CapEx (% of Revenue)",
        min = 0.01, max = 0.20, value = 0.04, step = 0.01
      ),
      sliderInput("nwc_change", "Change in NWC (% of Revenue)",
        min = -0.05, max = 0.10, value = 0.01, step = 0.005
      ),
      hr(style = "border-color: #4a4a4a;"),
      h4(icon("sliders"), "Discount & Terminal", style = "color: #ecf0f1;"),
      sliderInput("wacc", "WACC",
        min = 0.06, max = 0.15, value = 0.10, step = 0.005
      ),
      sliderInput("terminal_growth", "Terminal Growth Rate",
        min = 0.01, max = 0.04, value = 0.025, step = 0.005
      ),
      numericInput("projection_years", "Projection Period (Years)",
        value = 5, min = 3, max = 10, step = 1
      ),
      hr(style = "border-color: #4a4a4a;"),
      h4(icon("building"), "Capital Structure", style = "color: #ecf0f1;"),
      numericInput("shares_out", "Shares Outstanding (M)",
        value = 15500, min = 1, step = 100
      ),
      numericInput("net_debt", "Net Debt ($M)",
        value = 49000, min = -100000, step = 1000
      )
    )
  ),
  dashboardBody(
    # Custom CSS
    tags$head(tags$style(HTML("
      .skin-black .main-header .logo { background-color: #1a1a2e; font-weight: bold; }
      .skin-black .main-header .navbar { background-color: #16213e; }
      .content-wrapper { background-color: #0f0f23; }
      .box { background-color: #1a1a2e; border-top: 3px solid #e94560; color: #ecf0f1; }
      .box-header { color: #ecf0f1; }
      .box .box-header .box-title { color: #ecf0f1; }
      .info-box { background-color: #16213e; color: #ecf0f1; }
      .info-box .info-box-text { color: #a0a0b0; }
      .info-box .info-box-number { color: #ecf0f1; font-size: 22px; }
      .info-box-icon { background-color: #e94560 !important; }
      .nav-tabs-custom>.tab-content { background: #1a1a2e; }
      .nav-tabs-custom>.nav-tabs>li.active>a { background-color: #1a1a2e; color: #ecf0f1; border-top-color: #e94560; }
      .nav-tabs-custom>.nav-tabs>li>a { color: #a0a0b0; }
      .nav-tabs-custom>.nav-tabs { background-color: #16213e; border-bottom-color: #4a4a4a; }
      .small-box { background-color: #16213e !important; }
      .small-box h3, .small-box p { color: #ecf0f1; }
      .table { color: #ecf0f1; }
      .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_paginate {
        color: #ecf0f1 !important;
      }
      .recommendation-box {
        padding: 20px; border-radius: 8px; text-align: center;
        font-size: 28px; font-weight: bold; margin: 10px 0;
      }
      .rec-buy { background-color: #27ae60; color: white; }
      .rec-sell { background-color: #e74c3c; color: white; }
      .rec-hold { background-color: #f39c12; color: white; }
      .metric-card {
        background: linear-gradient(135deg, #16213e, #1a1a2e);
        border-left: 4px solid #e94560; padding: 15px;
        margin-bottom: 10px; border-radius: 4px;
      }
      .metric-label { color: #a0a0b0; font-size: 12px; text-transform: uppercase; }
      .metric-value { color: #ecf0f1; font-size: 24px; font-weight: bold; }
      .summary-section {
        background-color: #16213e; padding: 20px; border-radius: 8px;
        color: #ecf0f1; margin-bottom: 15px; border-left: 4px solid #e94560;
      }
      .summary-section h4 { color: #e94560; margin-top: 0; }
      hr { border-color: #4a4a4a; }
    "))),
    fluidRow(
      # Top KPI Cards
      valueBoxOutput("price_box", width = 3),
      valueBoxOutput("mcap_box", width = 3),
      valueBoxOutput("pe_box", width = 3),
      valueBoxOutput("target_box", width = 3)
    ),

    # Main Content Tabs
    fluidRow(
      column(
        12,
        tabBox(
          width = 12,
          id = "main_tabs",

          # Tab 1: Price Chart & Overview
          tabPanel(
            title = span(icon("chart-area"), " Price & Overview"),
            fluidRow(
              column(
                8,
                box(
                  width = 12, title = "Price History",
                  plotlyOutput("price_chart", height = "400px"),
                  radioButtons("chart_period", NULL,
                    choices = c("6M" = 0.5, "1Y" = 1, "2Y" = 2, "3Y" = 3),
                    selected = 1, inline = TRUE
                  )
                )
              ),
              column(
                4,
                box(
                  width = 12, title = "Key Metrics",
                  uiOutput("key_metrics")
                ),
                box(
                  width = 12, title = "Moving Averages",
                  uiOutput("ma_analysis")
                )
              )
            )
          ),

          # Tab 2: DCF Valuation
          tabPanel(
            title = span(icon("calculator"), " DCF Valuation"),
            fluidRow(
              column(
                8,
                box(
                  width = 12, title = "Projected Free Cash Flows",
                  plotlyOutput("fcf_chart", height = "350px")
                ),
                box(
                  width = 12, title = "DCF Projection Table",
                  DTOutput("dcf_table")
                )
              ),
              column(
                4,
                box(
                  width = 12, title = "Valuation Summary",
                  uiOutput("dcf_summary")
                ),
                box(
                  width = 12, title = "Value Breakdown",
                  plotlyOutput("value_pie", height = "250px")
                )
              )
            )
          ),

          # Tab 3: Sensitivity Analysis
          tabPanel(
            title = span(icon("table"), " Sensitivity Analysis"),
            fluidRow(
              column(
                12,
                box(
                  width = 12, title = "Implied Share Price: WACC vs Terminal Growth",
                  plotlyOutput("sensitivity_heatmap", height = "500px"),
                  p("Each cell shows the implied share price under different WACC and
                                terminal growth assumptions.",
                    style = "color: #a0a0b0; font-style: italic;"
                  )
                )
              )
            )
          ),

          # Tab 4: Comparable Company Analysis
          tabPanel(
            title = span(icon("building"), " Comparable Companies"),
            fluidRow(
              column(
                12,
                box(
                  width = 12,
                  textInput("comps_tickers",
                    "Enter Comparable Tickers (comma-separated)",
                    value = "MSFT, GOOG, META, AMZN",
                    placeholder = "e.g., MSFT, GOOG, META"
                  ),
                  actionButton("run_comps", "Run Comps Analysis",
                    icon = icon("play"), class = "btn-primary"
                  ),
                  br(), br(),
                  DTOutput("comps_table")
                )
              )
            ),
            fluidRow(
              column(
                6,
                box(
                  width = 12, title = "P/E Comparison",
                  plotlyOutput("comps_pe_chart", height = "300px")
                )
              ),
              column(
                6,
                box(
                  width = 12, title = "Market Cap Comparison",
                  plotlyOutput("comps_mcap_chart", height = "300px")
                )
              )
            )
          ),

          # Tab 5: Research Summary
          tabPanel(
            title = span(icon("file-alt"), " Research Summary"),
            fluidRow(
              column(
                12,
                actionButton("generate_summary", "Generate Research Summary",
                  icon = icon("wand-magic-sparkles"),
                  class = "btn-primary btn-lg",
                  style = "margin-bottom: 20px;"
                ),
                uiOutput("research_summary")
              )
            )
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {
  # Reactive: Stock data
  stock_data <- eventReactive(input$analyze_btn,
    {
      withProgress(message = "Fetching market data...", {
        ticker <- toupper(trimws(input$ticker))

        # Get quote data
        quote <- tryCatch(
          {
            getQuote(ticker, what = yahooQF(c("Last Trade (Price Only)", "Open", "Days High", "Days Low", "Volume", "P/E Ratio", "Market Capitalization")))
          },
          error = function(e) NULL
        )

        # Get historical prices
        incProgress(0.3, detail = "Loading price history...")
        prices <- fetch_price_history(ticker, years = 3)

        # Get additional stats
        incProgress(0.3, detail = "Calculating metrics...")

        list(
          ticker = ticker,
          quote = quote,
          prices = prices
        )
      })
    },
    ignoreNULL = FALSE
  )

  # Reactive: DCF results
  dcf_results <- reactive({
    run_dcf(
      current_revenue = input$current_revenue,
      revenue_growth = input$revenue_growth,
      ebitda_margin = input$ebitda_margin,
      tax_rate = input$tax_rate,
      capex_pct = input$capex_pct,
      nwc_change_pct = input$nwc_change,
      wacc = input$wacc,
      terminal_growth = input$terminal_growth,
      projection_years = input$projection_years,
      shares_outstanding = input$shares_out,
      net_debt = input$net_debt
    )
  })

  # ---- VALUE BOXES ----

  output$price_box <- renderValueBox({
    data <- stock_data()
    price <- if (!is.null(data$quote)) round(data$quote$Last, 2) else "N/A"
    valueBox(
      paste0("$", price), "Current Price",
      icon = icon("dollar-sign"), color = "red"
    )
  })

  output$mcap_box <- renderValueBox({
    data <- stock_data()
    mcap <- if (!is.null(data$quote) && "Market Capitalization" %in% names(data$quote)) {
      fmt_currency(data$quote$`Market Capitalization`)
    } else {
      "N/A"
    }
    valueBox(mcap, "Market Cap", icon = icon("building"), color = "purple")
  })

  output$pe_box <- renderValueBox({
    data <- stock_data()
    pe <- if (!is.null(data$quote) && "P/E Ratio" %in% names(data$quote)) {
      round(data$quote$`P/E Ratio`, 1)
    } else {
      "N/A"
    }
    valueBox(paste0(pe, "x"), "P/E Ratio", icon = icon("chart-pie"), color = "blue")
  })

  output$target_box <- renderValueBox({
    dcf <- dcf_results()
    price <- dcf$price_per_share
    current <- if (!is.null(stock_data()$quote)) stock_data()$quote$Last else NA

    upside <- if (!is.na(current) && current > 0) {
      pct <- round((price - current) / current * 100, 1)
      paste0(ifelse(pct > 0, "+", ""), pct, "%")
    } else {
      ""
    }

    valueBox(
      paste0("$", round(price, 2)),
      paste("DCF Target", upside),
      icon = icon("bullseye"),
      color = if (!is.na(current) && price > current) "green" else "red"
    )
  })

  # ---- PRICE CHART ----

  output$price_chart <- renderPlotly({
    data <- stock_data()
    req(data$prices)

    period <- as.numeric(input$chart_period)
    start <- Sys.Date() - (period * 365)

    prices <- data$prices
    prices <- prices[zoo::index(prices) >= start]

    df <- data.frame(
      Date = zoo::index(prices),
      Close = as.numeric(Cl(prices)),
      Volume = as.numeric(Vo(prices))
    )

    # Add moving averages
    if (nrow(df) >= 50) df$MA50 <- zoo::rollmean(df$Close, 50, fill = NA, align = "right")
    if (nrow(df) >= 200) df$MA200 <- zoo::rollmean(df$Close, 200, fill = NA, align = "right")

    p <- plot_ly(df, x = ~Date) |>
      add_lines(
        y = ~Close, name = "Price",
        line = list(color = "#e94560", width = 2)
      ) |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor = "#1a1a2e",
        font = list(color = "#ecf0f1"),
        xaxis = list(gridcolor = "#2a2a3e", title = ""),
        yaxis = list(
          gridcolor = "#2a2a3e", title = "Price ($)",
          tickprefix = "$"
        ),
        legend = list(orientation = "h", y = -0.15),
        hovermode = "x unified"
      )

    if ("MA50" %in% names(df)) {
      p <- p |> add_lines(
        y = ~MA50, name = "50-Day MA",
        line = list(color = "#3498db", width = 1, dash = "dash")
      )
    }
    if ("MA200" %in% names(df)) {
      p <- p |> add_lines(
        y = ~MA200, name = "200-Day MA",
        line = list(color = "#f39c12", width = 1, dash = "dash")
      )
    }

    p
  })

  # ---- KEY METRICS ----

  output$key_metrics <- renderUI({
    data <- stock_data()
    q <- data$quote

    metrics <- list()
    if (!is.null(q)) {
      if ("Last" %in% names(q)) metrics[["Current Price"]] <- paste0("$", round(q$Last, 2))
      if ("Open" %in% names(q)) metrics[["Open"]] <- paste0("$", round(q$Open, 2))
      if ("High" %in% names(q)) metrics[["Day High"]] <- paste0("$", round(q$High, 2))
      if ("Low" %in% names(q)) metrics[["Day Low"]] <- paste0("$", round(q$Low, 2))
      if ("Volume" %in% names(q)) metrics[["Volume"]] <- comma(q$Volume)
      if ("P/E Ratio" %in% names(q)) metrics[["P/E Ratio"]] <- round(q$`P/E Ratio`, 2)
    }

    tags$div(
      lapply(names(metrics), function(name) {
        div(
          class = "metric-card",
          div(class = "metric-label", name),
          div(class = "metric-value", metrics[[name]])
        )
      })
    )
  })

  output$ma_analysis <- renderUI({
    data <- stock_data()
    req(data$prices)

    prices <- data$prices
    close_prices <- as.numeric(Cl(prices))
    current <- tail(close_prices, 1)

    ma50 <- if (length(close_prices) >= 50) mean(tail(close_prices, 50)) else NA
    ma200 <- if (length(close_prices) >= 200) mean(tail(close_prices, 200)) else NA

    tags$div(
      if (!is.na(ma50)) {
        div(
          class = "metric-card",
          div(class = "metric-label", "50-Day MA"),
          div(class = "metric-value", paste0("$", round(ma50, 2))),
          div(
            style = paste0("color:", ifelse(current > ma50, "#27ae60", "#e74c3c")),
            ifelse(current > ma50, "▲ Price Above", "▼ Price Below")
          )
        )
      },
      if (!is.na(ma200)) {
        div(
          class = "metric-card",
          div(class = "metric-label", "200-Day MA"),
          div(class = "metric-value", paste0("$", round(ma200, 2))),
          div(
            style = paste0("color:", ifelse(current > ma200, "#27ae60", "#e74c3c")),
            ifelse(current > ma200, "▲ Price Above", "▼ Price Below")
          )
        )
      },
      if (!is.na(ma50) && !is.na(ma200)) {
        div(
          class = "metric-card",
          div(class = "metric-label", "Signal"),
          div(
            class = "metric-value",
            style = paste0("color:", ifelse(ma50 > ma200, "#27ae60", "#e74c3c")),
            ifelse(ma50 > ma200, "Golden Cross ▲", "Death Cross ▼")
          )
        )
      }
    )
  })

  # ---- DCF VALUATION ----

  output$fcf_chart <- renderPlotly({
    dcf <- dcf_results()
    df <- dcf$projection_table

    plot_ly(df, x = ~Year) |>
      add_bars(
        y = ~`Free Cash Flow`, name = "FCF",
        marker = list(color = "#e94560")
      ) |>
      add_lines(
        y = ~Revenue, name = "Revenue", yaxis = "y2",
        line = list(color = "#3498db", width = 2)
      ) |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor = "#1a1a2e",
        font = list(color = "#ecf0f1"),
        xaxis = list(gridcolor = "#2a2a3e"),
        yaxis = list(gridcolor = "#2a2a3e", title = "FCF ($M)", tickprefix = "$"),
        yaxis2 = list(
          overlaying = "y", side = "right", title = "Revenue ($M)",
          gridcolor = "transparent", tickprefix = "$"
        ),
        legend = list(orientation = "h", y = -0.2),
        barmode = "group"
      )
  })

  output$dcf_table <- renderDT({
    dcf <- dcf_results()
    df <- dcf$projection_table |>
      dplyr::mutate(dplyr::across(
        tidyselect::where(is.numeric) & !tidyselect::contains("Discount"),
        ~ paste0("$", comma(round(.)))
      )) |>
      dplyr::mutate(`Discount Factor` = round(dcf$projection_table$`Discount Factor`, 4))

    datatable(df,
      options = list(
        dom = "t", pageLength = 10, ordering = FALSE,
        columnDefs = list(list(className = "dt-right", targets = 1:5))
      ),
      rownames = FALSE,
      class = "cell-border stripe"
    ) |>
      formatStyle(
        columns = names(df),
        backgroundColor = "#1a1a2e", color = "#ecf0f1"
      )
  })

  output$dcf_summary <- renderUI({
    dcf <- dcf_results()
    current_price <- if (!is.null(stock_data()$quote)) stock_data()$quote$Last else NA
    target <- dcf$price_per_share

    upside <- if (!is.na(current_price) && current_price > 0) {
      round((target - current_price) / current_price * 100, 1)
    } else {
      NA
    }

    rec <- if (!is.na(upside)) {
      if (upside > 15) {
        list(text = "BUY", class = "rec-buy")
      } else if (upside < -15) {
        list(text = "SELL", class = "rec-sell")
      } else {
        list(text = "HOLD", class = "rec-hold")
      }
    } else {
      list(text = "N/A", class = "rec-hold")
    }

    tags$div(
      div(class = paste("recommendation-box", rec$class), rec$text),
      br(),
      div(
        class = "metric-card",
        div(class = "metric-label", "Enterprise Value"),
        div(class = "metric-value", fmt_currency(dcf$enterprise_value))
      ),
      div(
        class = "metric-card",
        div(class = "metric-label", "Equity Value"),
        div(class = "metric-value", fmt_currency(dcf$equity_value))
      ),
      div(
        class = "metric-card",
        div(class = "metric-label", "Implied Price / Share"),
        div(class = "metric-value", paste0("$", round(target, 2)))
      ),
      if (!is.na(upside)) {
        div(
          class = "metric-card",
          div(class = "metric-label", "Upside / Downside"),
          div(
            class = "metric-value",
            style = paste0("color:", ifelse(upside > 0, "#27ae60", "#e74c3c")),
            paste0(ifelse(upside > 0, "+", ""), upside, "%")
          )
        )
      }
    )
  })

  output$value_pie <- renderPlotly({
    dcf <- dcf_results()

    plot_ly(
      labels = c("PV of Cash Flows", "PV of Terminal Value"),
      values = c(dcf$pv_fcf_total, dcf$pv_terminal),
      type = "pie",
      marker = list(colors = c("#e94560", "#3498db")),
      textinfo = "label+percent",
      textfont = list(color = "#ecf0f1")
    ) |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor = "#1a1a2e",
        font = list(color = "#ecf0f1"),
        showlegend = FALSE
      )
  })

  # ---- SENSITIVITY ANALYSIS ----

  output$sensitivity_heatmap <- renderPlotly({
    matrix_data <- run_sensitivity(
      input$current_revenue, input$revenue_growth, input$ebitda_margin,
      input$tax_rate, input$capex_pct, input$nwc_change,
      input$wacc, input$terminal_growth, input$projection_years,
      input$shares_out, input$net_debt
    )

    # Pivot for heatmap
    matrix_wide <- matrix_data |>
      dplyr::mutate(
        WACC = paste0(round(.data$WACC * 100, 1), "%"),
        Terminal_Growth = paste0(round(.data$Terminal_Growth * 100, 1), "%"),
        Price = round(.data$Price, 2)
      )

    plot_ly(
      data = matrix_wide,
      x = ~Terminal_Growth,
      y = ~WACC,
      z = ~Price,
      type = "heatmap",
      colorscale = list(c(0, "#e74c3c"), c(0.5, "#f39c12"), c(1, "#27ae60")),
      text = ~ paste0("$", Price),
      texttemplate = "$%{z:.2f}",
      hovertemplate = "WACC: %{y}<br>Terminal Growth: %{x}<br>Price: $%{z:.2f}<extra></extra>"
    ) |>
      layout(
        paper_bgcolor = "#1a1a2e",
        plot_bgcolor = "#1a1a2e",
        font = list(color = "#ecf0f1"),
        xaxis = list(title = "Terminal Growth Rate", gridcolor = "#2a2a3e"),
        yaxis = list(title = "WACC", gridcolor = "#2a2a3e", autorange = "reversed")
      )
  })

  # ---- COMPARABLE COMPANIES ----

  comps_data <- eventReactive(input$run_comps, {
    withProgress(message = "Fetching comparable data...", {
      tickers <- trimws(unlist(strsplit(input$comps_tickers, ",")))
      all_tickers <- c(toupper(trimws(input$ticker)), toupper(tickers))

      results <- lapply(all_tickers, function(t) {
        tryCatch(
          {
            q <- getQuote(t, what = yahooQF(c("Last Trade (Price Only)", "Volume", "P/E Ratio", "Market Capitalization")))
            tibble::tibble(
              Ticker = t,
              Price = round(q$Last, 2),
              `Market Cap` = if ("Market Capitalization" %in% names(q)) q$`Market Capitalization` else NA,
              `P/E` = if ("P/E Ratio" %in% names(q)) round(q$`P/E Ratio`, 1) else NA,
              Volume = if ("Volume" %in% names(q)) q$Volume else NA
            )
          },
          error = function(e) {
            tibble::tibble(Ticker = t, Price = NA, `Market Cap` = NA, `P/E` = NA, Volume = NA)
          }
        )
      })

      dplyr::bind_rows(results) |> dplyr::filter(!is.na(.data$Price))
    })
  })

  output$comps_table <- renderDT({
    df <- comps_data() |>
      dplyr::mutate(
        `Market Cap` = sapply(.data$`Market Cap`, fmt_currency),
        Price = paste0("$", .data$Price),
        `P/E` = paste0(.data$`P/E`, "x"),
        Volume = comma(.data$Volume)
      )

    datatable(df,
      options = list(dom = "t", pageLength = 20, ordering = TRUE),
      rownames = FALSE,
      class = "cell-border stripe"
    ) |>
      formatStyle(
        columns = names(df),
        backgroundColor = "#1a1a2e", color = "#ecf0f1"
      ) |>
      formatStyle("Ticker", fontWeight = "bold")
  })

  output$comps_pe_chart <- renderPlotly({
    df <- comps_data() |> dplyr::filter(!is.na(.data$`P/E`))

    colors <- ifelse(df$Ticker == toupper(input$ticker), "#e94560", "#3498db")

    plot_ly(df,
      x = ~Ticker, y = ~`P/E`, type = "bar",
      marker = list(color = colors)
    ) |>
      layout(
        paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
        font = list(color = "#ecf0f1"),
        xaxis = list(gridcolor = "#2a2a3e"),
        yaxis = list(gridcolor = "#2a2a3e", title = "P/E Ratio")
      )
  })

  output$comps_mcap_chart <- renderPlotly({
    df <- comps_data() |> dplyr::filter(!is.na(.data$`Market Cap`))

    colors <- ifelse(df$Ticker == toupper(input$ticker), "#e94560", "#3498db")

    plot_ly(df,
      x = ~Ticker, y = ~`Market Cap`, type = "bar",
      marker = list(color = colors)
    ) |>
      layout(
        paper_bgcolor = "#1a1a2e", plot_bgcolor = "#1a1a2e",
        font = list(color = "#ecf0f1"),
        xaxis = list(gridcolor = "#2a2a3e"),
        yaxis = list(
          gridcolor = "#2a2a3e", title = "Market Cap ($)",
          tickprefix = "$"
        )
      )
  })

  # ---- RESEARCH SUMMARY ----

  output$research_summary <- renderUI({
    req(input$generate_summary)

    data <- stock_data()
    dcf <- dcf_results()
    ticker <- toupper(input$ticker)
    current_price <- if (!is.null(data$quote)) round(data$quote$Last, 2) else "N/A"
    target_price <- round(dcf$price_per_share, 2)

    upside <- if (!is.null(data$quote) && data$quote$Last > 0) {
      round((dcf$price_per_share - data$quote$Last) / data$quote$Last * 100, 1)
    } else {
      NA
    }

    rec <- if (!is.na(upside)) {
      if (upside > 15) {
        "BUY"
      } else if (upside < -15) {
        "SELL"
      } else {
        "HOLD"
      }
    } else {
      "N/A"
    }

    rec_color <- switch(rec,
      "BUY" = "#27ae60",
      "SELL" = "#e74c3c",
      "HOLD" = "#f39c12",
      "#a0a0b0"
    )

    pe <- if (!is.null(data$quote) && "P/E Ratio" %in% names(data$quote)) {
      round(data$quote$`P/E Ratio`, 1)
    } else {
      "N/A"
    }

    tags$div(
      style = "max-width: 900px;",
      # Header
      div(
        class = "summary-section",
        style = "text-align: center; border-left: none; border-top: 4px solid #e94560;",
        h2(paste0(ticker, " — Equity Research Summary"),
          style = "color: #ecf0f1; margin: 0;"
        ),
        p(paste("Generated:", format(Sys.Date(), "%B %d, %Y")),
          style = "color: #a0a0b0; margin: 5px 0 0 0;"
        )
      ),

      # Recommendation Banner
      div(
        style = paste0(
          "background-color:", rec_color, "; padding: 20px; border-radius: 8px; ",
          "text-align: center; margin-bottom: 15px;"
        ),
        h3(style = "color: white; margin: 0;", paste("Recommendation:", rec)),
        p(
          style = "color: white; margin: 5px 0 0 0; font-size: 18px;",
          paste0(
            "Current: $", current_price, " | Target: $", target_price,
            if (!is.na(upside)) paste0(" | Upside: ", ifelse(upside > 0, "+", ""), upside, "%") else ""
          )
        )
      ),

      # Valuation Overview
      div(
        class = "summary-section",
        h4("Valuation Overview"),
        p(paste0(
          "Based on a ", input$projection_years, "-year discounted cash flow analysis with a ",
          round(input$wacc * 100, 1), "% WACC and ", round(input$terminal_growth * 100, 1),
          "% terminal growth rate, our model implies an enterprise value of ",
          fmt_currency(dcf$enterprise_value), " and an equity value of ",
          fmt_currency(dcf$equity_value), "."
        )),
        p(paste0(
          "The implied price per share of $", target_price,
          if (!is.na(upside)) {
            paste0(
              " represents a ", ifelse(upside > 0, "+", ""), upside,
              "% ", ifelse(upside > 0, "upside", "downside"),
              " from the current market price of $", current_price, "."
            )
          } else {
            "."
          }
        ))
      ),

      # Key Assumptions
      div(
        class = "summary-section",
        h4("Key Model Assumptions"),
        tags$table(
          style = "width: 100%; color: #ecf0f1;",
          tags$tr(
            tags$td(style = "padding: 5px;", "Revenue Growth:"),
            tags$td(
              style = "padding: 5px; text-align: right; font-weight: bold;",
              paste0(round(input$revenue_growth * 100, 1), "%")
            ),
            tags$td(style = "padding: 5px;", "EBITDA Margin:"),
            tags$td(
              style = "padding: 5px; text-align: right; font-weight: bold;",
              paste0(round(input$ebitda_margin * 100, 1), "%")
            )
          ),
          tags$tr(
            tags$td(style = "padding: 5px;", "WACC:"),
            tags$td(
              style = "padding: 5px; text-align: right; font-weight: bold;",
              paste0(round(input$wacc * 100, 1), "%")
            ),
            tags$td(style = "padding: 5px;", "Terminal Growth:"),
            tags$td(
              style = "padding: 5px; text-align: right; font-weight: bold;",
              paste0(round(input$terminal_growth * 100, 1), "%")
            )
          ),
          tags$tr(
            tags$td(style = "padding: 5px;", "Tax Rate:"),
            tags$td(
              style = "padding: 5px; text-align: right; font-weight: bold;",
              paste0(round(input$tax_rate * 100, 1), "%")
            ),
            tags$td(style = "padding: 5px;", "CapEx (% Rev):"),
            tags$td(
              style = "padding: 5px; text-align: right; font-weight: bold;",
              paste0(round(input$capex_pct * 100, 1), "%")
            )
          )
        )
      ),

      # Value Composition
      div(
        class = "summary-section",
        h4("Enterprise Value Composition"),
        p(paste0(
          "Terminal value accounts for ",
          round(dcf$pv_terminal / dcf$enterprise_value * 100, 1),
          "% of total enterprise value (", fmt_currency(dcf$pv_terminal),
          "), while the present value of projected cash flows contributes ",
          round(dcf$pv_fcf_total / dcf$enterprise_value * 100, 1),
          "% (", fmt_currency(dcf$pv_fcf_total), ")."
        )),
        p(paste0(
          "Net debt of ", fmt_currency(input$net_debt),
          " is subtracted to arrive at the equity value of ",
          fmt_currency(dcf$equity_value), " across ",
          comma(input$shares_out), "M diluted shares."
        ))
      ),

      # Risk Factors (static template)
      div(
        class = "summary-section",
        h4("Key Risks & Considerations"),
        p("Investors should consider the following risk factors when evaluating this analysis:"),
        tags$ul(
          style = "color: #a0a0b0;",
          tags$li("Model sensitivity to WACC and terminal growth assumptions (see Sensitivity tab)"),
          tags$li("Revenue growth assumptions may not materialize in a downturn"),
          tags$li("Margin compression from competitive pressures or input cost inflation"),
          tags$li("Regulatory and macroeconomic headwinds"),
          tags$li("This model uses simplified assumptions and should be supplemented with industry-specific analysis")
        )
      ),

      # Disclaimer
      div(
        style = "padding: 15px; color: #666; font-size: 11px; font-style: italic; text-align: center;",
        p("This report was generated for educational and analytical purposes only.
                   It does not constitute investment advice. The author is not a registered
                   investment advisor. Past performance does not guarantee future results.")
      )
    )
  })
}

# ============================================================================
# RUN
# ============================================================================

shinyApp(ui = ui, server = server)
