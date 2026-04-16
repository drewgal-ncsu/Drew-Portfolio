# ============================================================
# Presentation-Ready Graph Suite
# In-Sample (1970–2003) + Out-of-Sample (2004–2022)
# ============================================================
# Drew Galvin — Quantitative Investing Project 2026
#
# This script generates 6 polished graph types for each period.
# It uses the EXACT same 5-factor strategy pipeline from
# multifactor_strategy.R applied to both datasets.
# ============================================================

# ---- 0. Setup ----
library(tidyverse)
library(zoo)
library(PerformanceAnalytics)
library(gridExtra)
library(grid)

load("MainDataJKP.RData")
load("/Users/droob/Downloads/OOS Project Workspace.RData")

ff <- FF_Factors

# Consistent styling - "Vibrant Presentation" Palette
STRATEGY_COL <- "#2563eb"    # Vibrant Electric Blue
MARKET_COL   <- "#94a3b8"    # Clean Slate Gray
ACCENT_COL   <- "#ef4444"    # Vibrant Signal Red (Drawdowns)
ACCENT2_COL  <- "#f59e0b"    # Bright Amber (Highlights)
BG_COL       <- "white"

theme_presentation <- theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = BG_COL, color = NA),
    panel.background  = element_rect(fill = BG_COL, color = NA),
    plot.title        = element_text(face = "bold", size = 18, color = "#1e293b", margin = margin(b = 10)),
    plot.subtitle     = element_text(size = 13, color = "#475569", margin = margin(b = 15)),
    plot.caption      = element_text(size = 10, color = "#94a3b8", hjust = 0, margin = margin(t = 15)),
    legend.position   = "bottom",
    legend.text       = element_text(size = 12, face = "bold"),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(color = "#f1f5f9"),
    panel.grid.major.y = element_line(color = "#e2e8f0"),
    axis.text         = element_text(size = 12, color = "#1e293b", face = "bold"),
    axis.title        = element_text(size = 13, color = "#475569"),
    plot.margin       = margin(25, 25, 25, 25)  # More breathing room
  )

# ---- 1. Reusable Strategy Pipeline Function ----
run_strategy <- function(stock_data, ff_data, label) {
  
  factors <- c("ret_12_1", "ret_6_1", "be_me", "qmj_prof", "rvol_21d")
  
  df_clean <- stock_data %>%
    filter(!is.na(FXRet), !is.na(market_equity)) %>%
    filter(if_all(all_of(factors), ~ !is.na(.)))
  
  df_univ <- df_clean %>%
    group_by(ym) %>%
    mutate(mktcap_p20 = quantile(market_equity, 0.20)) %>%
    filter(market_equity >= mktcap_p20) %>%
    select(-mktcap_p20) %>% ungroup()
  
  df_scored <- df_univ %>%
    group_by(ym) %>%
    mutate(
      z_mom12  = percent_rank(ret_12_1),
      z_mom6   = percent_rank(ret_6_1),
      z_val    = percent_rank(be_me),
      z_qual   = percent_rank(qmj_prof),
      z_lowvol = percent_rank(-rvol_21d),
      composite = (z_mom12 + z_mom6 + z_val + z_qual + z_lowvol) / 5
    ) %>% ungroup()
  
  N_TARGET <- 100; N_BUFFER <- 140
  reb_months <- c(3, 6, 9, 12)
  all_ym <- sort(unique(df_scored$ym))
  current_holdings <- character(0)
  port_list <- list()
  
  for (i in seq_along(all_ym)) {
    this_ym <- all_ym[i]
    m_data <- df_scored %>% filter(ym == this_ym)
    m_num  <- m_data$month[1]
    
    if (m_num %in% reb_months || length(current_holdings) == 0) {
      m_ranked <- m_data %>% arrange(desc(composite))
      if (length(current_holdings) > 0) {
        top_buf  <- m_ranked %>% slice_head(n = N_BUFFER) %>% pull(stock_id) %>% as.character()
        retained <- intersect(current_holdings, top_buf)
        spots    <- max(0, N_TARGET - length(retained))
        if (spots > 0) {
          new_adds <- m_ranked %>% filter(!(stock_id %in% retained)) %>%
            slice_head(n = spots) %>% pull(stock_id) %>% as.character()
          current_holdings <- c(retained, new_adds)
        } else {
          current_holdings <- m_ranked %>% filter(stock_id %in% retained) %>%
            slice_head(n = N_TARGET) %>% pull(stock_id) %>% as.character()
        }
      } else {
        current_holdings <- m_ranked %>% slice_head(n = N_TARGET) %>% pull(stock_id) %>% as.character()
      }
    }
    port_list[[i]] <- m_data %>% filter(stock_id %in% current_holdings) %>% mutate(weight = 1/n())
  }
  portfolio_all <- bind_rows(port_list)
  
  port_ret <- portfolio_all %>%
    group_by(ym, year, month, dateff) %>% summarise(port_ret = mean(FXRet, na.rm=TRUE), .groups="drop")
  
  mkt_ret_df <- df_univ %>%
    group_by(ym, year, month, dateff) %>% summarise(mkt_ret = mean(FXRet, na.rm=TRUE), .groups="drop")
  
  res <- port_ret %>%
    left_join(mkt_ret_df, by=c("ym","year","month","dateff")) %>%
    mutate(
      date     = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
      port_dec = port_ret / 100,
      mkt_dec  = mkt_ret / 100,
      port_cum = cumprod(1 + port_dec),
      mkt_cum  = cumprod(1 + mkt_dec),
      port_dd  = (port_cum - cummax(port_cum)) / cummax(port_cum),
      mkt_dd   = (mkt_cum - cummax(mkt_cum)) / cummax(mkt_cum)
    ) %>%
    left_join(ff_data %>% select(-date), by="dateff")
  
  # Summary stats
  ann_ret  <- (1 + mean(res$port_dec))^12 - 1
  ann_vol  <- sd(res$port_dec) * sqrt(12)
  sharpe   <- ann_ret / ann_vol
  m_ret    <- (1 + mean(res$mkt_dec))^12 - 1
  m_vol    <- sd(res$mkt_dec) * sqrt(12)
  m_sharpe <- m_ret / m_vol
  
  # CAPM regression
  capm <- lm((port_ret - rf) ~ mktrf, data = res)
  capm_s <- summary(capm)
  
  # FF6 regression
  rd <- res %>% filter(!is.na(mktrf), !is.na(umd), !is.na(rmw), !is.na(cma))
  if (nrow(rd) > 0) {
    ff6 <- lm((port_ret - rf) ~ mktrf + smb + hml + rmw + cma + umd, data = rd)
    ff6_s <- summary(ff6)
  } else {
    ff6 <- NULL; ff6_s <- NULL
  }
  
  list(
    res = res, label = label,
    ann_ret = ann_ret, ann_vol = ann_vol, sharpe = sharpe,
    m_ret = m_ret, m_vol = m_vol, m_sharpe = m_sharpe,
    capm = capm, capm_s = capm_s,
    ff6 = ff6, ff6_s = ff6_s
  )
}

# ---- 2. Run Both Periods ----
cat("Running in-sample strategy (1970–2003)...\n")
is_result <- run_strategy(MainDataJKP %>% filter(year <= 2003), ff, "In-Sample (1970–2003)")

cat("Running out-of-sample strategy (2004–2022)...\n")
oos_result <- run_strategy(OOSDataJKP, ff, "Out-of-Sample (2004–2022)")

cat(sprintf("\n--- IN-SAMPLE ---\nStrategy: %.2f%% return, %.2f%% vol, %.2f Sharpe\nMarket:   %.2f%% return, %.2f%% vol, %.2f Sharpe\n",
    is_result$ann_ret*100, is_result$ann_vol*100, is_result$sharpe,
    is_result$m_ret*100, is_result$m_vol*100, is_result$m_sharpe))
cat(sprintf("\n--- OUT-OF-SAMPLE ---\nStrategy: %.2f%% return, %.2f%% vol, %.2f Sharpe\nMarket:   %.2f%% return, %.2f%% vol, %.2f Sharpe\n",
    oos_result$ann_ret*100, oos_result$ann_vol*100, oos_result$sharpe,
    oos_result$m_ret*100, oos_result$m_vol*100, oos_result$m_sharpe))

# ============================================================
# GRAPH GENERATION FUNCTIONS
# ============================================================

# ---- Graph 1: Growth of $1 Invested (Log Scale) ----
make_growth_plot <- function(r, period_tag) {
  df <- r$res
  end_port <- round(tail(df$port_cum, 1), 2)
  end_mkt  <- round(tail(df$mkt_cum, 1), 2)
  end_date <- tail(df$date, 1)
  
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = mkt_cum, color = "Market Baseline"), linewidth = 0.9, alpha = 0.85) +
    geom_line(aes(y = port_cum, color = "5-Factor Strategy"), linewidth = 1.3) +
    # Endpoint annotations
    annotate("text", x = end_date + 90, y = end_port, label = paste0("$", end_port),
             color = STRATEGY_COL, fontface = "bold", size = 4, hjust = 0) +
    annotate("text", x = end_date + 90, y = end_mkt, label = paste0("$", end_mkt),
             color = MARKET_COL, fontface = "bold", size = 4, hjust = 0) +
    scale_y_log10(labels = scales::dollar_format(accuracy = 1),
                  breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y",
                 expand = expansion(mult = c(0.02, 0.08))) +
    scale_color_manual(values = c("5-Factor Strategy" = STRATEGY_COL, "Market Baseline" = MARKET_COL)) +
    labs(
      title    = "Growth of $1 Invested Over Time",
      subtitle = paste0("Log Scale — ", r$label),
      x = NULL, y = "Cumulative Wealth ($)", color = NULL,
      caption  = "Source: MainDataJKP.RData | Equal-weighted universe benchmark"
    ) +
    theme_presentation +
    coord_cartesian(clip = "off")
}

# ---- Graph 2: Monthly Excess Return / Annualized Return Comparison ----
make_return_plot <- function(r, period_tag) {
  bar_data <- tibble(
    Portfolio = factor(c("5-Factor Strategy", "Market Baseline"),
                       levels = c("5-Factor Strategy", "Market Baseline")),
    `Annualized Return` = c(r$ann_ret * 100, r$m_ret * 100)
  )
  
  ggplot(bar_data, aes(x = Portfolio, y = `Annualized Return`, fill = Portfolio)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = sprintf("%.2f%%", `Annualized Return`)),
              vjust = -0.8, size = 6, fontface = "bold", color = "#0f172a") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + # Extra room for top labels
    scale_fill_manual(values = c("5-Factor Strategy" = STRATEGY_COL, "Market Baseline" = MARKET_COL)) +
    labs(
      title    = "Annualized Return Performance",
      subtitle = r$label,
      x = NULL, y = "Annualized Return (%)", fill = NULL,
      caption  = "Excess return = Strategy return minus Market return"
    ) +
    theme_presentation +
    theme(legend.position = "none",
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(size = 13, face = "bold")) +
    # Add excess return annotation
    annotate("segment", x = 1, xend = 2, y = r$ann_ret * 100, yend = r$ann_ret * 100,
             linetype = "dashed", color = ACCENT2_COL, linewidth = 0.6) +
    annotate("text", x = 1.5, y = max(r$ann_ret, r$m_ret) * 100 + 1.5,
             label = sprintf("+%.0f bps excess", (r$ann_ret - r$m_ret) * 10000),
             color = ACCENT2_COL, fontface = "bold", size = 4.5)
}

# ---- Graph 3: Sharpe Ratio Comparison ----
make_sharpe_plot <- function(r, period_tag) {
  bar_data <- tibble(
    Portfolio = factor(c("5-Factor Strategy", "Market Baseline"),
                       levels = c("5-Factor Strategy", "Market Baseline")),
    Sharpe = c(r$sharpe, r$m_sharpe),
    Vol = c(r$ann_vol * 100, r$m_vol * 100)
  )
  
  p1 <- ggplot(bar_data, aes(x = Portfolio, y = Sharpe, fill = Portfolio)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = sprintf("%.2f", Sharpe)), vjust = -0.8, size = 6, fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_fill_manual(values = c("5-Factor Strategy" = STRATEGY_COL, "Market Baseline" = MARKET_COL)) +
    labs(title = "Sharpe Ratio", subtitle = r$label, x = NULL, y = "Sharpe Ratio") +
    theme_presentation + theme(legend.position = "none", panel.grid.major.x = element_blank(),
                                axis.text.x = element_text(size = 12, face = "bold"))
  
  p2 <- ggplot(bar_data, aes(x = Portfolio, y = Vol, fill = Portfolio)) +
    geom_col(width = 0.55) +
    geom_text(aes(label = sprintf("%.1f%%", Vol)), vjust = -0.8, size = 5.5, fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    scale_fill_manual(values = c("5-Factor Strategy" = STRATEGY_COL, "Market Baseline" = MARKET_COL)) +
    labs(title = "Annualized Volatility", subtitle = r$label, x = NULL, y = "Volatility (%)") +
    theme_presentation + theme(legend.position = "none", panel.grid.major.x = element_blank(),
                                axis.text.x = element_text(size = 12, face = "bold"))
  
  gridExtra::grid.arrange(p1, p2, ncol = 2)
}

# ---- Graph 4: CAPM Summary ----
make_capm_plot <- function(r, period_tag) {
  cs <- r$capm_s
  coefs <- as.data.frame(cs$coefficients)
  coefs$Factor <- rownames(coefs)
  coefs$Factor[coefs$Factor == "(Intercept)"] <- "Alpha (Monthly)"
  coefs$Factor[coefs$Factor == "mktrf"] <- "Market Beta"
  
  coefs <- coefs %>%
    mutate(
      lower = Estimate - 1.96 * `Std. Error`,
      upper = Estimate + 1.96 * `Std. Error`,
      sig   = ifelse(`Pr(>|t|)` < 0.01, "***",
              ifelse(`Pr(>|t|)` < 0.05, "**",
              ifelse(`Pr(>|t|)` < 0.10, "*", "")))
    )
  
  ggplot(coefs, aes(x = Factor, y = Estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#cccccc") +
    geom_col(fill = STRATEGY_COL, width = 0.5) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.7, color = "#334155") +
    geom_text(aes(label = sprintf("%.3f%s", Estimate, sig)),
              vjust = ifelse(coefs$Estimate >= 0, -1.8, 2.5), size = 6, fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = 0.25)) + 
    labs(
      title    = "CAPM Regression Coefficients",
      subtitle = paste0(r$label, "  |  R² = ", sprintf("%.3f", cs$r.squared)),
      x = NULL, y = "Coefficient Estimate",
      caption  = "Error bars = 95% CI  |  *** p<0.01, ** p<0.05, * p<0.10"
    ) +
    theme_presentation +
    theme(panel.grid.major.x = element_blank())
}

# ---- Graph 5: FF6 Factor Loadings ----
make_ff6_plot <- function(r, period_tag) {
  if (is.null(r$ff6_s)) {
    plot.new()
    text(0.5, 0.5, "FF6 data not available for this period", cex = 1.5)
    return(invisible(NULL))
  }
  
  fs <- r$ff6_s
  coefs <- as.data.frame(fs$coefficients)
  coefs$Factor <- rownames(coefs)
  
  labels <- c("(Intercept)" = "Alpha", "mktrf" = "MktRF", "smb" = "SMB",
              "hml" = "HML", "rmw" = "RMW", "cma" = "CMA", "umd" = "UMD")
  coefs$Factor <- labels[coefs$Factor]
  coefs$Factor <- factor(coefs$Factor, levels = c("Alpha", "MktRF", "SMB", "HML", "RMW", "CMA", "UMD"))
  
  coefs <- coefs %>% mutate(
    lower = Estimate - 1.96 * `Std. Error`,
    upper = Estimate + 1.96 * `Std. Error`,
    sig   = ifelse(`Pr(>|t|)` < 0.01, "***",
            ifelse(`Pr(>|t|)` < 0.05, "**",
            ifelse(`Pr(>|t|)` < 0.10, "*", ""))),
    bar_fill = ifelse(Factor == "Alpha", ACCENT_COL, STRATEGY_COL)
  )
  
  ggplot(coefs, aes(x = Factor, y = Estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#cccccc") +
    geom_col(fill = coefs$bar_fill, width = 0.55) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.7, color = "#334155") +
    geom_text(aes(label = sprintf("%.3f%s", Estimate, sig)),
              vjust = ifelse(coefs$Estimate >= 0, -1.8, 2.5), size = 5.5, fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = 0.25)) +
    labs(
      title    = "Fama-French 6-Factor Regression",
      subtitle = paste0(r$label, "  |  Adj. R² = ", sprintf("%.3f", fs$adj.r.squared)),
      x = NULL, y = "Factor Loading",
      caption  = "Error bars = 95% CI  |  Alpha shown in red  |  *** p<0.01"
    ) +
    theme_presentation +
    theme(panel.grid.major.x = element_blank())
}

# ---- Graph 7: Drawdown Profile ----
make_drawdown_plot <- function(r, period_tag) {
  df <- r$res
  ggplot(df, aes(x = date)) +
    geom_area(aes(y = mkt_dd, fill = "Market Baseline"), alpha = 0.35) +
    geom_line(aes(y = port_dd, color = "5-Factor Strategy"), linewidth = 1.3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.3))) +
    scale_x_date(date_breaks = ifelse(period_tag == "IS", "5 years", "3 years"), date_labels = "%Y") +
    scale_fill_manual(values = c("Market Baseline" = MARKET_COL)) +
    scale_color_manual(values = c("5-Factor Strategy" = ACCENT_COL)) +
    labs(
      title    = "Peak-to-Trough Drawdown Analysis",
      subtitle = paste0("Strategy vs. Market Resilience — ", r$label),
      x = NULL, y = "Drawdown (%)", fill = NULL, color = NULL,
      caption  = "Visualizing downside protection: The red line consistently rides above the gray areas during crashes."
    ) +
    theme_presentation +
    theme(legend.position = "bottom")
}

# ---- Helper: significance stars ----
sig_stars <- function(p) {
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*",
  ifelse(p < 0.10,  ".", ""))))
}

# ---- Graph 6: Combined "Must-Haves" Summary Dashboard ----
make_summary_dashboard <- function(is_r, oos_r) {
  
  capm_is  <- is_r$capm_s$coefficients
  ff6_is   <- is_r$ff6_s$coefficients
  capm_oos <- oos_r$capm_s$coefficients
  ff6_oos  <- oos_r$ff6_s$coefficients
  
  # FF6 factor names for the regression rows
  ff6_rows <- c("(Intercept)", "mktrf", "smb", "hml", "rmw", "cma", "umd")
  ff6_labels <- c("FF6 Alpha (mo.)", "FF6 Market (MktRF)", "FF6 Size (SMB)",
                   "FF6 Value (HML)", "FF6 Profitability (RMW)", 
                   "FF6 Investment (CMA)", "FF6 Momentum (UMD)")
  
  # Build IS regression stats
  is_est <- sapply(ff6_rows, function(r) sprintf("%.3f%s", ff6_is[r, "Estimate"], sig_stars(ff6_is[r, "Pr(>|t|)"])))
  is_se  <- sapply(ff6_rows, function(r) sprintf("(%.3f)", ff6_is[r, "Std. Error"]))
  is_t   <- sapply(ff6_rows, function(r) sprintf("%.2f",   ff6_is[r, "t value"]))
  is_p   <- sapply(ff6_rows, function(r) {
    pv <- ff6_is[r, "Pr(>|t|)"]
    if (pv < 0.001) "<0.001" else sprintf("%.3f", pv)
  })
  
  # Build OOS regression stats
  oos_est <- sapply(ff6_rows, function(r) sprintf("%.3f%s", ff6_oos[r, "Estimate"], sig_stars(ff6_oos[r, "Pr(>|t|)"])))
  oos_se  <- sapply(ff6_rows, function(r) sprintf("(%.3f)", ff6_oos[r, "Std. Error"]))
  oos_t   <- sapply(ff6_rows, function(r) sprintf("%.2f",   ff6_oos[r, "t value"]))
  oos_p   <- sapply(ff6_rows, function(r) {
    pv <- ff6_oos[r, "Pr(>|t|)"]
    if (pv < 0.001) "<0.001" else sprintf("%.3f", pv)
  })
  
  # ------ TOP TABLE: Core Performance Metrics ------
  perf_metrics <- tibble(
    Metric = c("Ann. Return", "Ann. Volatility", "Sharpe Ratio",
               "CAPM Alpha (mo.)", "CAPM Beta"),
    `IS Strategy` = c(
      sprintf("%.2f%%", is_r$ann_ret * 100),
      sprintf("%.2f%%", is_r$ann_vol * 100),
      sprintf("%.2f",   is_r$sharpe),
      sprintf("%.3f",   capm_is["(Intercept)", "Estimate"]),
      sprintf("%.3f",   capm_is["mktrf", "Estimate"])
    ),
    `IS Market` = c(
      sprintf("%.2f%%", is_r$m_ret * 100),
      sprintf("%.2f%%", is_r$m_vol * 100),
      sprintf("%.2f",   is_r$m_sharpe), "—", "—"
    ),
    `OOS Strategy` = c(
      sprintf("%.2f%%", oos_r$ann_ret * 100),
      sprintf("%.2f%%", oos_r$ann_vol * 100),
      sprintf("%.2f",   oos_r$sharpe),
      sprintf("%.3f",   capm_oos["(Intercept)", "Estimate"]),
      sprintf("%.3f",   capm_oos["mktrf", "Estimate"])
    ),
    `OOS Market` = c(
      sprintf("%.2f%%", oos_r$m_ret * 100),
      sprintf("%.2f%%", oos_r$m_vol * 100),
      sprintf("%.2f",   oos_r$m_sharpe), "—", "—"
    )
  )
  
  # ------ BOTTOM TABLE: FF6 Regression with Significance ------
  reg_metrics <- tibble(
    Factor         = ff6_labels,
    `IS Estimate`  = as.character(is_est),
    `IS Std.Err`   = as.character(is_se),
    `IS t-stat`    = as.character(is_t),
    `IS p-value`   = as.character(is_p),
    `OOS Estimate` = as.character(oos_est),
    `OOS Std.Err`  = as.character(oos_se),
    `OOS t-stat`   = as.character(oos_t),
    `OOS p-value`  = as.character(oos_p)
  )
  
  # ------ STYLING ------
  tt_perf <- gridExtra::ttheme_minimal(
    core    = list(fg_params = list(fontsize = 13, fontface = "plain", col = "#1e293b"),
                   bg_params = list(fill = c("#f8fafc", "#ffffff"), col = "#e2e8f0", lwd = 1.5)),
    colhead = list(fg_params = list(fontsize = 13, fontface = "bold", col = "white"),
                   bg_params = list(fill = STRATEGY_COL, col = "#e2e8f0", lwd = 2))
  )
  
  tt_reg <- gridExtra::ttheme_minimal(
    core    = list(fg_params = list(fontsize = 12, fontface = "plain", col = "#1e293b"),
                   bg_params = list(fill = c("#f8fafc", "#ffffff"), col = "#e2e8f0", lwd = 1.5)),
    colhead = list(fg_params = list(fontsize = 12, fontface = "bold", col = "white"),
                   bg_params = list(fill = "#334155", col = "#e2e8f0", lwd = 2))
  )
  
  tbl_perf <- gridExtra::tableGrob(perf_metrics, rows = NULL, theme = tt_perf)
  tbl_reg  <- gridExtra::tableGrob(reg_metrics,  rows = NULL, theme = tt_reg)
  
  # ------ TITLE / SUBTITLE ------
  title_g <- textGrob("Strategy Performance Dashboard",
                       gp = gpar(fontsize = 22, fontface = "bold", col = "#0f172a"),
                       x = 0.05, hjust = 0, y = 0.2)
  sub_g   <- textGrob("5-Factor Vanguard-Style Composite  |  In-Sample vs. Out-of-Sample",
                       gp = gpar(fontsize = 14, col = "#64748b", fontface = "italic"),
                       x = 0.05, hjust = 0, y = 0.2)
  
  # Section label for the regression table
  reg_label <- textGrob("Fama-French 6-Factor Regression  |  *** p<0.001  ** p<0.01  * p<0.05  . p<0.10",
                         gp = gpar(fontsize = 12, fontface = "bold.italic", col = "#475569"),
                         x = 0.05, hjust = 0, y = 0.3)
  
  final <- gridExtra::arrangeGrob(
    title_g, sub_g, tbl_perf, reg_label, tbl_reg,
    heights = c(0.05, 0.03, 0.30, 0.04, 0.58)
  )
  final
}

# ============================================================
# GENERATE & SAVE DASHBOARD
# ============================================================
cat("\nGenerating Dashboard...\n")

# --- Graph 6: Combined Dashboard ---
dash <- make_summary_dashboard(is_result, oos_result)
png("graph6_dashboard.png", width = 2400, height = 1600, res = 180)
grid.draw(dash)
dev.off()

cat("\n✓ Strategy Performance Dashboard saved successfully as graph6_dashboard.png\n")

