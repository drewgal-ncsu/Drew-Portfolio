# ============================================================
# Vanguard-Style 5-Factor Composite Strategy (Master Script)
# Momentum + Value + Quality + Low Volatility
# ============================================================
# Drew Galvin — Quantitative Investing Project 2026
#
# This script executes the entire project pipeline:
#   1. Runs the Original 5-Factor Strategy on IS (1970-2003) & OOS (2004-2022)
#   2. Runs the Improved Strategy D on OOS (2004-2022)
#   3. Generates the full presentation-ready graph suite (Graphs 1-8)
# ============================================================

# ---- 0. Setup ----
library(tidyverse)
library(zoo)
library(scales)
library(PerformanceAnalytics)

# ---- 1. Load Data ----
load("MainDataJKP.RData")                              # IS: 1970–2003
load("/Users/droob/Downloads/OOS Project Workspace.RData") # OOS: 2004–2022

ff <- FF_Factors

# ---- 2. Reusable Strategy Pipeline ----
run_strategy <- function(data, factor_list, directions, label) {
  
  df_clean <- data %>%
    filter(!is.na(FXRet), !is.na(market_equity)) %>%
    filter(if_all(all_of(factor_list), ~ !is.na(.)))
  
  # Universe: top 80% by market cap each month
  df_univ <- df_clean %>%
    group_by(ym) %>%
    mutate(mktcap_p20 = quantile(market_equity, 0.20)) %>%
    filter(market_equity >= mktcap_p20) %>%
    select(-mktcap_p20) %>% ungroup()
  
  # Cross-sectional percent-rank scoring
  df_scored <- df_univ %>% group_by(ym)
  for (i in seq_along(factor_list)) {
    f <- factor_list[i]
    d <- directions[i]
    col_name <- paste0("z_", i)
    if (d == 1) {
      df_scored <- df_scored %>% mutate(!!col_name := percent_rank(.data[[f]]))
    } else {
      df_scored <- df_scored %>% mutate(!!col_name := percent_rank(-.data[[f]]))
    }
  }
  z_cols <- paste0("z_", seq_along(factor_list))
  df_scored <- df_scored %>%
    mutate(composite = rowMeans(across(all_of(z_cols)))) %>% ungroup()
  
  # Portfolio construction: top 100, quarterly rebalance
  N_TARGET <- 100; N_BUFFER <- 140; reb_months <- c(3, 6, 9, 12)
  all_ym <- sort(unique(df_scored$ym))
  current_holdings <- character(0); port_list <- list()
  
  for (i in seq_along(all_ym)) {
    this_ym <- all_ym[i]
    m_data <- df_scored %>% filter(ym == this_ym)
    m_num <- m_data$month[1]
    if (m_num %in% reb_months || length(current_holdings) == 0) {
      m_ranked <- m_data %>% arrange(desc(composite))
      if (length(current_holdings) > 0) {
        top_buf <- m_ranked %>% slice_head(n = N_BUFFER) %>% pull(stock_id) %>% as.character()
        retained <- intersect(current_holdings, top_buf)
        spots <- max(0, N_TARGET - length(retained))
        if (spots > 0) {
          new_adds <- m_ranked %>% filter(!(stock_id %in% retained)) %>%
            slice_head(n = spots) %>% pull(stock_id) %>% as.character()
          current_holdings <- c(retained, new_adds)
        } else {
          current_holdings <- m_ranked %>% filter(stock_id %in% retained) %>%
            slice_head(n = N_TARGET) %>% pull(stock_id) %>% as.character()
        }
      } else {
        current_holdings <- m_ranked %>% slice_head(n = N_TARGET) %>%
          pull(stock_id) %>% as.character()
      }
    }
    port_list[[i]] <- m_data %>%
      filter(stock_id %in% current_holdings) %>% mutate(weight = 1 / n())
  }
  portfolio_all <- bind_rows(port_list)
  
  # Monthly returns
  port_ret <- portfolio_all %>%
    group_by(ym, year, month, dateff) %>%
    summarise(port_ret = mean(FXRet, na.rm = TRUE), .groups = "drop")
  
  mkt_ret_df <- df_univ %>%
    group_by(ym, year, month, dateff) %>%
    summarise(mkt_ret = mean(FXRet, na.rm = TRUE), .groups = "drop")
  
  res <- port_ret %>%
    left_join(mkt_ret_df, by = c("ym", "year", "month", "dateff")) %>%
    mutate(
      date     = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")),
      port_dec = port_ret / 100,
      mkt_dec  = mkt_ret / 100,
      port_cum = cumprod(1 + port_dec),
      mkt_cum  = cumprod(1 + mkt_dec),
      port_dd  = (port_cum - cummax(port_cum)) / cummax(port_cum),
      mkt_dd   = (mkt_cum - cummax(mkt_cum)) / cummax(mkt_cum)
    ) %>%
    left_join(ff %>% select(-date), by="dateff")
  
  ann_ret <- (1 + mean(res$port_dec))^12 - 1
  ann_vol <- sd(res$port_dec) * sqrt(12)
  sharpe  <- ann_ret / ann_vol
  
  cat(sprintf("\n[%s]  Growth: $%.2f  Ann: %.2f%%  Sharpe: %.3f\n",
              label, tail(res$port_cum, 1), ann_ret * 100, sharpe))
  
  list(label = label, res = res, ann_ret = ann_ret, ann_vol = ann_vol, sharpe = sharpe)
}

# ---- 3. Execute Strategies ----
cat("\n========= EXECUTING STRATEGIES =========\n")

# A. ORIGINAL Strategy on IS Data
original_is <- run_strategy(
  MainDataJKP %>% filter(year <= 2003),
  c("ret_12_1", "ret_6_1", "be_me", "qmj_prof", "rvol_21d"), c(1, 1, 1, 1, -1),
  "Original 5-Factor (IS: 1970-2003)"
)

# B. ORIGINAL Strategy on OOS Data
original_oos <- run_strategy(
  OOSDataJKP,
  c("ret_12_1", "ret_6_1", "be_me", "qmj_prof", "rvol_21d"), c(1, 1, 1, 1, -1),
  "Original 5-Factor (OOS: 2004-2022)"
)

# C. IMPROVED Strategy on OOS Data
improved_oos <- run_strategy(
  OOSDataJKP,
  c("ret_12_1", "op_at", "cop_at", "qmj_prof", "ocf_at"), c(1, 1, 1, 1, 1),
  "Improved Quality-Heavy Strategy (OOS: 2004-2022)"
)

# ---- 4. Styling Constants ----
STRAT_COL  <- "#1e3a5f"    # Deep Navy (Final Strategy)
MKT_COL    <- "#9ca3af"    # Muted Slate Gray (Market)
IS_COL     <- "#2563eb"    # Electric Blue (In-Sample bars)
OOS_COL    <- "#0d9488"    # Teal (Out-of-Sample bars)
MKT_LINE   <- "#dc2626"    # Muted Red (Market overlay)
DD_COL     <- "#ef4444"    # Signal Red (Drawdowns)
BG_COL     <- "white"

theme_polished <- theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = BG_COL, color = NA),
    panel.background  = element_rect(fill = BG_COL, color = NA),
    plot.title        = element_text(face = "bold", size = 18, color = "#0f172a", margin = margin(b = 8)),
    plot.subtitle     = element_text(size = 13, color = "#475569", margin = margin(b = 12)),
    plot.caption      = element_text(size = 10, color = "#94a3b8", hjust = 0, margin = margin(t = 12)),
    legend.position   = "bottom",
    legend.text       = element_text(size = 12, face = "bold"),
    legend.key.width  = unit(1.5, "cm"),
    panel.grid.minor  = element_blank(),
    panel.grid.major.x = element_line(color = "#f1f5f9"),
    panel.grid.major.y = element_line(color = "#e2e8f0"),
    axis.text         = element_text(size = 12, color = "#1e293b"),
    axis.title        = element_text(size = 13, color = "#475569"),
    plot.margin       = margin(20, 25, 15, 20)
  )

# ---- 5. Graph Generation ----
cat("\n========= GENERATING GRAPHS =========\n")

# GRAPH 1: Improved Strategy OOS Growth
df1 <- improved_oos$res
eval_end_1 <- tail(df1$port_cum, 1); mkt_end_1 <- tail(df1$mkt_cum, 1); dt_1 <- tail(df1$date, 1)

p1 <- ggplot(df1, aes(x = date)) +
  geom_line(aes(y = mkt_cum, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum, color = "Final Strategy"), linewidth = 1.4) +
  annotate("text", x = dt_1 + 60, y = eval_end_1, label = sprintf("$%.2f", eval_end_1), color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = dt_1 + 60, y = mkt_end_1, label = sprintf("$%.2f", mkt_end_1), color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  scale_y_log10(labels = dollar_format(accuracy = 0.01), breaks = c(0.5, 1, 2, 3, 5, 7, 10)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = expansion(mult = c(0.02, 0.08))) +
  scale_color_manual(values = c("Final Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL)) +
  labs(title = "Graph 1: Growth of $1 Invested (Improved Strategy)", subtitle = "Quality-Heavy Strategy vs. Market Benchmark — Out-of-Sample (2004–2022)", x = NULL, y = "Growth of $1 (Log Scale)", color = NULL) +
  coord_cartesian(clip = "off") + theme_polished

ggsave("graph_growth_oos.png", p1, width = 11, height = 6.5, dpi = 200)

# GRAPH 2: Original Annual Returns IS+OOS
monthly_all <- bind_rows(
  original_is$res %>% mutate(Period = "In-Sample"),
  original_oos$res %>% mutate(Period = "Out-of-Sample")
) %>% arrange(date) %>% mutate(port_cum_full = cumprod(1 + port_dec), mkt_cum_full = cumprod(1 + mkt_dec))

annual_is  <- original_is$res %>% group_by(year) %>% summarise(port_annual = prod(1 + port_dec) - 1, mkt_annual  = prod(1 + mkt_dec) - 1, .groups = "drop") %>% mutate(Period = "In-Sample (1970–2003)")
annual_oos <- original_oos$res %>% group_by(year) %>% summarise(port_annual = prod(1 + port_dec) - 1, mkt_annual  = prod(1 + mkt_dec) - 1, .groups = "drop") %>% mutate(Period = "Out-of-Sample (2004–2022)")
annual_all <- bind_rows(annual_is, annual_oos) %>% mutate(Period = factor(Period, levels = c("In-Sample (1970–2003)", "Out-of-Sample (2004–2022)")))
boundary_year <- 2003.5

p2 <- ggplot(annual_all, aes(x = year)) +
  geom_vline(xintercept = boundary_year, linetype = "dashed", color = "#94a3b8", linewidth = 0.7) +
  annotate("text", x = boundary_year - 0.5, y = max(annual_all$port_annual) * 0.95, label = "In-Sample ←", color = "#475569", size = 3.8, fontface = "italic", hjust = 1) +
  annotate("text", x = boundary_year + 0.5, y = max(annual_all$port_annual) * 0.95, label = "→ Out-of-Sample", color = "#475569", size = 3.8, fontface = "italic", hjust = 0) +
  geom_col(aes(y = port_annual, fill = Period), width = 0.7, alpha = 0.9) +
  geom_line(aes(y = mkt_annual), color = MKT_LINE, linewidth = 0.8, alpha = 0.7) +
  geom_point(aes(y = mkt_annual), color = MKT_LINE, size = 1.2, alpha = 0.7) +
  geom_hline(yintercept = 0, color = "#cbd5e1", linewidth = 0.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0.05, 0.1))) +
  scale_x_continuous(breaks = seq(1970, 2022, by = 5), expand = expansion(mult = c(0.01, 0.01))) +
  scale_fill_manual(values = c("In-Sample (1970–2003)" = IS_COL, "Out-of-Sample (2004–2022)" = OOS_COL)) +
  labs(title = "Graph 2: Annual Portfolio Returns", subtitle = "Original 5-Factor Strategy (bars) vs. Market Benchmark (red line) — 1970 to 2022", x = "Year", y = "Annual Return (%)", fill = NULL) +
  theme_polished + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10), legend.position = "top", legend.justification = "left")

ggsave("graph_annual_returns.png", p2, width = 14, height = 7, dpi = 200)

# GRAPH 3: Original Full Growth 1970-2022
eval_end_3 <- tail(monthly_all$port_cum_full, 1); mkt_end_3 <- tail(monthly_all$mkt_cum_full, 1); dt_3 <- tail(monthly_all$date, 1)
boundary_date <- as.Date("2003-12-31")

p3 <- ggplot(monthly_all, aes(x = date)) +
  geom_vline(xintercept = boundary_date, linetype = "dashed", color = "#94a3b8", linewidth = 0.7) +
  geom_line(aes(y = mkt_cum_full, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum_full, color = "Original Strategy"), linewidth = 1.4) +
  annotate("text", x = boundary_date - 365, y = eval_end_3 * 0.7, label = "In-Sample ←", color = "#475569", fontface = "italic", hjust = 1) +
  annotate("text", x = boundary_date + 365, y = eval_end_3 * 0.7, label = "→ Out-of-Sample", color = "#475569", fontface = "italic", hjust = 0) +
  annotate("text", x = dt_3 + 180, y = eval_end_3, label = sprintf("$%.2f", eval_end_3), color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = dt_3 + 180, y = mkt_end_3, label = sprintf("$%.2f", mkt_end_3), color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  scale_y_log10(labels = dollar_format(accuracy = 1), breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.08))) +
  scale_color_manual(values = c("Original Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL)) +
  labs(title = "Graph 3: Growth of $1 Invested (Full History)", subtitle = "Original 5-Factor Strategy vs. Market Benchmark (1970–2022)", x = NULL, y = "Growth of $1 (Log Scale)", color = NULL) +
  coord_cartesian(clip = "off") + theme_polished

ggsave("graph_full_growth.png", p3, width = 12, height = 7, dpi = 200)

# GRAPH 4: Original IS Growth 1970-2003
df4 <- original_is$res
eval_end_4 <- tail(df4$port_cum, 1); mkt_end_4 <- tail(df4$mkt_cum, 1); dt_4 <- tail(df4$date, 1)

p4 <- ggplot(df4, aes(x = date)) +
  geom_line(aes(y = mkt_cum, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum, color = "Original Strategy"), linewidth = 1.4) +
  annotate("text", x = dt_4 + 120, y = eval_end_4, label = sprintf("$%.2f", eval_end_4), color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = dt_4 + 120, y = mkt_end_4, label = sprintf("$%.2f", mkt_end_4), color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  scale_y_log10(labels = dollar_format(accuracy = 0.1), breaks = c(1, 2, 5, 10, 15)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = expansion(mult = c(0.01, 0.08))) +
  scale_color_manual(values = c("Original Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL)) +
  labs(title = "Graph 4: Growth of $1 Invested (In-Sample)", subtitle = "Original 5-Factor Strategy vs. Market Benchmark (1970–2003)", x = NULL, y = "Growth of $1 (Log Scale)", color = NULL) +
  coord_cartesian(clip = "off") + theme_polished

ggsave("graph_original_is_growth.png", p4, width = 11, height = 6.5, dpi = 200)

# GRAPH 5: Original OOS Growth 2004-2022
df5 <- original_oos$res
eval_end_5 <- tail(df5$port_cum, 1); mkt_end_5 <- tail(df5$mkt_cum, 1); dt_5 <- tail(df5$date, 1)

p5 <- ggplot(df5, aes(x = date)) +
  geom_line(aes(y = mkt_cum, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum, color = "Original Strategy"), linewidth = 1.4) +
  annotate("text", x = dt_5 + 60, y = eval_end_5, label = sprintf("$%.2f", eval_end_5), color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = dt_5 + 60, y = mkt_end_5, label = sprintf("$%.2f", mkt_end_5), color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  scale_y_log10(labels = dollar_format(accuracy = 0.01), breaks = c(0.5, 1, 2, 3, 5, 7, 10)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y", expand = expansion(mult = c(0.02, 0.08))) +
  scale_color_manual(values = c("Original Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL)) +
  labs(title = "Graph 5: Growth of $1 Invested (Out-of-Sample)", subtitle = "Original 5-Factor Strategy vs. Market Benchmark (2004–2022)", x = NULL, y = "Growth of $1 (Log Scale)", color = NULL) +
  coord_cartesian(clip = "off") + theme_polished

ggsave("graph_original_oos_growth.png", p5, width = 11, height = 6.5, dpi = 200)

# GRAPH 6: IS Drawdown Profile (Original Strategy)
p6 <- ggplot(df4, aes(x = date)) +
  geom_area(aes(y = mkt_dd, fill = "Market Benchmark"), alpha = 0.4) +
  geom_line(aes(y = port_dd, color = "Original Strategy"), linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Market Benchmark" = "#d1d5db")) +
  scale_color_manual(values = c("Original Strategy" = DD_COL)) +
  labs(title = "Graph 6: Drawdown Profile (In-Sample)", subtitle = "Visualizing downside protection during market shocks (1970–2003)", x = NULL, y = "Drawdown", fill = "", color = "") +
  theme_polished + theme(legend.position = "bottom")

ggsave("graph_is_drawdown.png", p6, width = 11, height = 6.5, dpi = 200)

# GRAPH 7: OOS Drawdown Profile (Original Strategy)
p7 <- ggplot(df5, aes(x = date)) +
  geom_area(aes(y = mkt_dd, fill = "Market Benchmark"), alpha = 0.4) +
  geom_line(aes(y = port_dd, color = "Original Strategy"), linewidth = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Market Benchmark" = "#d1d5db")) +
  scale_color_manual(values = c("Original Strategy" = DD_COL)) +
  labs(title = "Graph 7: Drawdown Profile (Out-of-Sample)", subtitle = "Visualizing downside protection during market shocks (2004–2022)", x = NULL, y = "Drawdown", fill = "", color = "") +
  theme_polished + theme(legend.position = "bottom")

ggsave("graph_oos_drawdown.png", p7, width = 11, height = 6.5, dpi = 200)

# GRAPH 8: Regression Validation Photo
reg_data <- original_is$res %>% filter(!is.na(mktrf), !is.na(umd))
if(nrow(reg_data) > 0) {
  ff6_reg <- lm((port_ret - rf) ~ mktrf + smb + hml + rmw + cma + umd, data = reg_data)
} else {
  ff6_reg <- lm((port_ret - rf) ~ mktrf + smb + hml, data = original_is$res)
}
reg_summary <- capture.output(summary(ff6_reg))

png("graph_regression.png", width = 800, height = 500, bg = "white")
par(mar = c(1, 1, 1, 1))
plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
text(x = 0, y = 1, paste(reg_summary, collapse = "\n"), cex = 1.0, family = "mono", adj = c(0, 1))
dev.off()

cat("✓ All 8 core graphs successfully generated and saved.\n")
