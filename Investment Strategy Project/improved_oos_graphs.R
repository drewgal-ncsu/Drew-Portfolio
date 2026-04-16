# ============================================================
# Improved OOS Strategy + Polished Presentation Graphs
# ============================================================
# Drew Galvin — Quantitative Investing Project 2026
#
# This script produces two polished graphs:
#   Graph 1: Growth of $1 (2004–2022) — IMPROVED Strategy D
#   Graph 2: Annual Portfolio Returns (1970–2022) — ORIGINAL Strategy
#
# Strategy D (Quality-Heavy):
#   1. ret_12_1   (Momentum, Higher = Better)
#   2. op_at      (Operating Profitability, Higher = Better)
#   3. cop_at     (Cash-Based Profitability, Higher = Better)
#   4. qmj_prof   (Quality/Profitability, Higher = Better)
#   5. ocf_at     (Operating Cash Flow, Higher = Better)
# ============================================================

# ---- 0. Setup ----
library(tidyverse)
library(zoo)
library(scales)

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
      mkt_cum  = cumprod(1 + mkt_dec)
    )
  
  ann_ret <- (1 + mean(res$port_dec))^12 - 1
  ann_vol <- sd(res$port_dec) * sqrt(12)
  sharpe  <- ann_ret / ann_vol
  
  cat(sprintf("\n[%s]  Growth: $%.2f  Ann: %.2f%%  Sharpe: %.3f\n",
              label, tail(res$port_cum, 1), ann_ret * 100, sharpe))
  
  list(label = label, res = res, ann_ret = ann_ret, ann_vol = ann_vol, sharpe = sharpe)
}

# ---- 3. Styling Constants ----
STRAT_COL  <- "#1e3a5f"    # Deep Navy (Final Strategy)
MKT_COL    <- "#9ca3af"    # Muted Slate Gray (Market)
IS_COL     <- "#2563eb"    # Electric Blue (In-Sample bars)
OOS_COL    <- "#0d9488"    # Teal (Out-of-Sample bars)
MKT_LINE   <- "#dc2626"    # Muted Red (Market overlay)
BG_COL     <- "white"

theme_polished <- theme_minimal(base_size = 14) +
  theme(
    plot.background   = element_rect(fill = BG_COL, color = NA),
    panel.background  = element_rect(fill = BG_COL, color = NA),
    plot.title        = element_text(face = "bold", size = 18, color = "#0f172a",
                                     margin = margin(b = 8)),
    plot.subtitle     = element_text(size = 13, color = "#475569",
                                     margin = margin(b = 12)),
    plot.caption      = element_text(size = 10, color = "#94a3b8", hjust = 0,
                                     margin = margin(t = 12)),
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

# ============================================================
#  GRAPH 1: Growth of $1 — IMPROVED Strategy D (OOS Only)
# ============================================================
cat("\n========= GRAPH 1: Improved Strategy D on OOS =========\n")

improved <- run_strategy(
  OOSDataJKP,
  c("ret_12_1", "op_at", "cop_at", "qmj_prof", "ocf_at"),
  c(1, 1, 1, 1, 1),
  "Improved Quality-Heavy Strategy"
)

df1 <- improved$res

# Terminal values for end-point labels
strat_end <- tail(df1$port_cum, 1)
mkt_end   <- tail(df1$mkt_cum, 1)
end_date  <- tail(df1$date, 1)

p1 <- ggplot(df1, aes(x = date)) +
  geom_line(aes(y = mkt_cum, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum, color = "Final Strategy"), linewidth = 1.4) +
  
  # End-point dollar labels
  annotate("text", x = end_date + 60, y = strat_end,
           label = sprintf("$%.2f", strat_end),
           color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = end_date + 60, y = mkt_end,
           label = sprintf("$%.2f", mkt_end),
           color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  
  scale_y_log10(
    labels = dollar_format(accuracy = 0.01),
    breaks = c(0.5, 1, 2, 3, 5, 7, 10)
  ) +
  scale_x_date(
    date_breaks = "3 years", date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08))  # room for end labels
  ) +
  scale_color_manual(
    values = c("Final Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL),
    guide = guide_legend(override.aes = list(linewidth = 2))
  ) +
  labs(
    title    = "Growth of $1 Invested Over Time",
    subtitle = "Quality-Heavy Strategy vs. Market Benchmark — Out-of-Sample (2004–2022)",
    x = NULL, y = "Growth of $1 Invested (Log Scale)", color = NULL,
    caption  = paste0(
      "Strategy: ret_12_1 + op_at + cop_at + qmj_prof + ocf_at  |  ",
      sprintf("Ann. Return: %.1f%%  Sharpe: %.2f", improved$ann_ret * 100, improved$sharpe)
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_polished

ggsave("graph_growth_oos.png", p1, width = 11, height = 6.5, dpi = 200)
cat("✓ Graph 1 saved: graph_growth_oos.png\n")


# ============================================================
#  GRAPH 2: Annual Returns — ORIGINAL Strategy (IS + OOS)
# ============================================================
cat("\n========= GRAPH 2: Original Strategy Annual Returns =========\n")

# Run ORIGINAL strategy on IS data
original_is <- run_strategy(
  MainDataJKP %>% filter(year <= 2003),
  c("ret_12_1", "ret_6_1", "be_me", "qmj_prof", "rvol_21d"),
  c(1, 1, 1, 1, -1),
  "Original 5-Factor (IS: 1970-2003)"
)

# Run ORIGINAL strategy on OOS data
original_oos <- run_strategy(
  OOSDataJKP,
  c("ret_12_1", "ret_6_1", "be_me", "qmj_prof", "rvol_21d"),
  c(1, 1, 1, 1, -1),
  "Original 5-Factor (OOS: 2004-2022)"
)

# Compute annual returns from monthly data
annual_from_monthly <- function(res_df, period_label) {
  res_df %>%
    group_by(year) %>%
    summarise(
      port_annual = prod(1 + port_dec) - 1,
      mkt_annual  = prod(1 + mkt_dec) - 1,
      .groups = "drop"
    ) %>%
    mutate(Period = period_label)
}

annual_is  <- annual_from_monthly(original_is$res, "In-Sample (1970–2003)")
annual_oos <- annual_from_monthly(original_oos$res, "Out-of-Sample (2004–2022)")

annual_all <- bind_rows(annual_is, annual_oos) %>%
  arrange(year) %>%
  mutate(Period = factor(Period, levels = c("In-Sample (1970–2003)", "Out-of-Sample (2004–2022)")))

# Boundary year for the vertical divider
boundary_year <- 2003.5

p2 <- ggplot(annual_all, aes(x = year)) +
  # Vertical period divider
  geom_vline(xintercept = boundary_year, linetype = "dashed",
             color = "#94a3b8", linewidth = 0.7) +
  annotate("text", x = boundary_year - 0.5, y = max(annual_all$port_annual) * 0.95,
           label = "In-Sample ←", color = "#475569", size = 3.8,
           fontface = "italic", hjust = 1) +
  annotate("text", x = boundary_year + 0.5, y = max(annual_all$port_annual) * 0.95,
           label = "→ Out-of-Sample", color = "#475569", size = 3.8,
           fontface = "italic", hjust = 0) +
  
  # Strategy bars colored by period
  geom_col(aes(y = port_annual, fill = Period), width = 0.7, alpha = 0.9) +
  
  # Market return overlay line
  geom_line(aes(y = mkt_annual), color = MKT_LINE, linewidth = 0.8, alpha = 0.7) +
  geom_point(aes(y = mkt_annual), color = MKT_LINE, size = 1.2, alpha = 0.7) +
  
  # Zero reference line
  geom_hline(yintercept = 0, color = "#cbd5e1", linewidth = 0.5) +
  
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.1))
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2022, by = 5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_fill_manual(
    values = c("In-Sample (1970–2003)" = IS_COL, "Out-of-Sample (2004–2022)" = OOS_COL)
  ) +
  labs(
    title    = "Annual Portfolio Returns: In-Sample and Out-of-Sample",
    subtitle = "Original 5-Factor Strategy (bars) vs. Market Benchmark (red line) — 1970 to 2022",
    x = "Year", y = "Annual Return (%)", fill = NULL,
    caption  = "Original strategy: ret_12_1 + ret_6_1 + be_me + qmj_prof + rvol_21d  |  Bars = Strategy, Red line = Market"
  ) +
  theme_polished +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "top",
    legend.justification = "left"
  )

ggsave("graph_annual_returns.png", p2, width = 14, height = 7, dpi = 200)
cat("✓ Graph 2 saved: graph_annual_returns.png\n")


# ============================================================
#  GRAPH 3: Full Growth of $1 — ORIGINAL Strategy (1970–2022)
# ============================================================
cat("\n========= GRAPH 3: Original Strategy Full Growth 1970-2022 =========\n")

# Combine monthly results and re-calculate continuous cumulative return
monthly_all <- bind_rows(
  original_is$res %>% mutate(Period = "In-Sample"),
  original_oos$res %>% mutate(Period = "Out-of-Sample")
) %>%
  arrange(date) %>%
  mutate(
    port_cum_full = cumprod(1 + port_dec),
    mkt_cum_full  = cumprod(1 + mkt_dec)
  )

strat_end_full <- tail(monthly_all$port_cum_full, 1)
mkt_end_full   <- tail(monthly_all$mkt_cum_full, 1)
end_date_full  <- tail(monthly_all$date, 1)
boundary_date  <- as.Date("2003-12-31")

p3 <- ggplot(monthly_all, aes(x = date)) +
  # Vertical period divider
  geom_vline(xintercept = boundary_date, linetype = "dashed",
             color = "#94a3b8", linewidth = 0.7) +
  
  # Market and Strategy lines
  geom_line(aes(y = mkt_cum_full, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum_full, color = "Original Strategy"), linewidth = 1.4) +
  
  # IS/OOS Annotations (placed high up on the graph to not overlap lines)
  annotate("text", x = boundary_date - 365, y = strat_end_full * 0.7,
           label = "In-Sample ←", color = "#475569", size = 4.5,
           fontface = "italic", hjust = 1) +
  annotate("text", x = boundary_date + 365, y = strat_end_full * 0.7,
           label = "→ Out-of-Sample", color = "#475569", size = 4.5,
           fontface = "italic", hjust = 0) +
  
  # End-point dollar labels
  annotate("text", x = end_date_full + 180, y = strat_end_full,
           label = sprintf("$%.2f", strat_end_full),
           color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = end_date_full + 180, y = mkt_end_full,
           label = sprintf("$%.2f", mkt_end_full),
           color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  
  scale_y_log10(
    labels = dollar_format(accuracy = 1),
    breaks = c(1, 2, 5, 10, 20, 50, 100, 200, 500)
  ) +
  scale_x_date(
    date_breaks = "5 years", date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  scale_color_manual(
    values = c("Original Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL),
    guide = guide_legend(override.aes = list(linewidth = 2))
  ) +
  labs(
    title    = "Growth of $1 Invested Over Time: Full History (1970–2022)",
    subtitle = "Original 5-Factor Strategy vs. Market Benchmark — Spanning IS and OOS Periods",
    x = NULL, y = "Growth of $1 Invested (Log Scale)", color = NULL,
    caption  = "Original strategy: ret_12_1 + ret_6_1 + be_me + qmj_prof + rvol_21d  |  Dashed line separates In-Sample and Out-of-Sample"
  ) +
  coord_cartesian(clip = "off") +
  theme_polished

ggsave("graph_full_growth.png", p3, width = 12, height = 7, dpi = 200)
cat("✓ Graph 3 saved: graph_full_growth.png\n")


# ============================================================
#  GRAPH 4: Growth of $1 — ORIGINAL Strategy (IS: 1970–2003)
# ============================================================
cat("\n========= GRAPH 4: Original Strategy IS Growth (1970-2003) =========\n")

df4 <- original_is$res

strat_end_is <- tail(df4$port_cum, 1)
mkt_end_is   <- tail(df4$mkt_cum, 1)
end_date_is  <- tail(df4$date, 1)

p4 <- ggplot(df4, aes(x = date)) +
  geom_line(aes(y = mkt_cum, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum, color = "Original Strategy"), linewidth = 1.4) +
  
  annotate("text", x = end_date_is + 120, y = strat_end_is,
           label = sprintf("$%.2f", strat_end_is),
           color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = end_date_is + 120, y = mkt_end_is,
           label = sprintf("$%.2f", mkt_end_is),
           color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  
  scale_y_log10(
    labels = dollar_format(accuracy = 0.1),
    breaks = c(1, 2, 5, 10, 15)
  ) +
  scale_x_date(
    date_breaks = "5 years", date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  scale_color_manual(
    values = c("Original Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL),
    guide = guide_legend(override.aes = list(linewidth = 2))
  ) +
  labs(
    title    = "Growth of $1 Invested Over Time: In-Sample (1970–2003)",
    subtitle = "Original 5-Factor Strategy vs. Market Benchmark",
    x = NULL, y = "Growth of $1 Invested (Log Scale)", color = NULL,
    caption  = paste0(
      "Original strategy: ret_12_1 + ret_6_1 + be_me + qmj_prof + rvol_21d  |  ",
      sprintf("Ann. Return: %.1f%%  Sharpe: %.2f", original_is$ann_ret * 100, original_is$sharpe)
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_polished

ggsave("graph_original_is_growth.png", p4, width = 11, height = 6.5, dpi = 200)
cat("✓ Graph 4 saved: graph_original_is_growth.png\n")


# ============================================================
#  GRAPH 5: Growth of $1 — ORIGINAL Strategy (OOS: 2004–2022)
# ============================================================
cat("\n========= GRAPH 5: Original Strategy OOS Growth (2004-2022) =========\n")

df5 <- original_oos$res

strat_end_oos <- tail(df5$port_cum, 1)
mkt_end_oos   <- tail(df5$mkt_cum, 1)
end_date_oos  <- tail(df5$date, 1)

p5 <- ggplot(df5, aes(x = date)) +
  geom_line(aes(y = mkt_cum, color = "Market Benchmark"), linewidth = 0.9, alpha = 0.85) +
  geom_line(aes(y = port_cum, color = "Original Strategy"), linewidth = 1.4) +
  
  annotate("text", x = end_date_oos + 60, y = strat_end_oos,
           label = sprintf("$%.2f", strat_end_oos),
           color = STRAT_COL, fontface = "bold", size = 5.5, hjust = 0) +
  annotate("text", x = end_date_oos + 60, y = mkt_end_oos,
           label = sprintf("$%.2f", mkt_end_oos),
           color = MKT_COL, fontface = "bold", size = 5, hjust = 0) +
  
  scale_y_log10(
    labels = dollar_format(accuracy = 0.01),
    breaks = c(0.5, 1, 2, 3, 5, 7, 10)
  ) +
  scale_x_date(
    date_breaks = "3 years", date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_color_manual(
    values = c("Original Strategy" = STRAT_COL, "Market Benchmark" = MKT_COL),
    guide = guide_legend(override.aes = list(linewidth = 2))
  ) +
  labs(
    title    = "Growth of $1 Invested Over Time: Out-of-Sample (2004–2022)",
    subtitle = "Original 5-Factor Strategy vs. Market Benchmark",
    x = NULL, y = "Growth of $1 Invested (Log Scale)", color = NULL,
    caption  = paste0(
      "Original strategy: ret_12_1 + ret_6_1 + be_me + qmj_prof + rvol_21d  |  ",
      sprintf("Ann. Return: %.1f%%  Sharpe: %.2f", original_oos$ann_ret * 100, original_oos$sharpe)
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_polished

ggsave("graph_original_oos_growth.png", p5, width = 11, height = 6.5, dpi = 200)
cat("✓ Graph 5 saved: graph_original_oos_growth.png\n")


# ---- Summary Output ----
cat("\n============================================================\n")
cat("  GRAPH 1: IMPROVED Strategy D (OOS only)\n")
cat(sprintf("    OOS Growth: $%.2f | Ann Ret: %.2f%% | Sharpe: %.3f\n",
            strat_end, improved$ann_ret * 100, improved$sharpe))
cat("\n  GRAPHS 2-5: ORIGINAL Strategy\n")
cat(sprintf("    Full 1970-2022 Growth: $%.2f (Market: $%.2f)\n",
            strat_end_full, mkt_end_full))
cat(sprintf("    IS 1970-2003 Growth  : $%.2f (Market: $%.2f)\n",
            strat_end_is, mkt_end_is))
cat(sprintf("    OOS 2004-2022 Growth : $%.2f (Market: $%.2f)\n",
            strat_end_oos, mkt_end_oos))
cat("============================================================\n")
cat("\n✓ All 5 polished graphs saved successfully.\n")
