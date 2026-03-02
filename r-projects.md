# Finance & Analytics Portfolio

**A collection of interactive R Shiny applications demonstrating financial analysis, quantitative modeling, and data engineering skills — built for the job search.**

---

## Why This Exists

This repository is a living portfolio designed to do one thing: show hiring managers in capital markets, wealth management, investment research, and financial analysis that I can do the work — not just talk about it.

Every project here pulls live market data, performs real calculations, and produces professional-quality output. These aren't homework assignments or static scripts. They're deployable tools that demonstrate the analytical thinking, technical execution, and financial fluency that entry-level finance roles demand.

Each app is built in **R Shiny** and deployed on [shinyapps.io](https://www.shinyapps.io/) so you can interact with them live — no installation required.

---

## Live Projects

### 1. [Equity Research Dashboard](./equity-research-dashboard/)
**What it does:** Interactive DCF valuation engine with live market data, comparable company analysis, sensitivity modeling, and automated one-page equity research report generation.

**Skills demonstrated:** Financial modeling (DCF), equity valuation, comparable company analysis, sensitivity analysis, investment thesis construction

**Key features:**
- Real-time stock data via Yahoo Finance
- Adjustable DCF assumptions with instant recalculation
- WACC vs terminal growth sensitivity heatmap
- Peer benchmarking across P/E and market cap
- Auto-generated research summary with BUY / HOLD / SELL recommendation

🔗 **Live app:** *[shinyapps.io link here]*

---

### 2. [Portfolio Risk Analyzer](./portfolio-risk-analyzer/)
**What it does:** Multi-asset portfolio construction tool with Monte Carlo simulation, Value at Risk (parametric, historical, CVaR), correlation analysis, and stress testing against six historical market crashes.

**Skills demonstrated:** Portfolio theory, risk management, Monte Carlo methods, statistical analysis, stress testing, performance attribution

**Key features:**
- Build custom portfolios with up to 6 assets
- Three flavors of VaR plus Expected Shortfall
- 5,000+ correlated return simulations with percentile bands
- Stress test against COVID crash, GFC, 2022 rate shock, and more
- Automated risk assessment report with actionable insights

🔗 **Live app:** *[shinyapps.io link here]*

---

## Planned Projects

The following projects are in development or on the roadmap. Each one adds a new dimension to the portfolio, covering different analytical competencies that finance teams look for.

### 3. Earnings Surprise Tracker
Track historical earnings beats and misses, visualize stock price reactions in event windows around announcements, and identify patterns by sector and market cap. Demonstrates event-driven analysis, statistical significance testing, and the ability to work with messy real-world data.

### 4. Factor Screener & Backtester
Score a universe of stocks on value, momentum, and quality factors. Rank and filter based on composite scores, then backtest factor-weighted strategies against benchmarks. Demonstrates quantitative strategy development, factor investing knowledge, and rigorous backtesting methodology.

### 5. Macro Regime Dashboard
Track Fed funds rate, yield curve shape, inflation (CPI/PCE), GDP growth, and unemployment. Detect regime changes and visualize historical recession indicators with real-time data from FRED. Demonstrates macroeconomic literacy, time series analysis, and the ability to contextualize markets within the broader economy.

### 6. WACC Calculator
Full weighted average cost of capital model with CAPM-based cost of equity, credit spread estimation for cost of debt, and optimal capital structure analysis. A focused utility tool that demonstrates deep understanding of corporate finance fundamentals.

### 7. Options Pricing & Greeks Visualizer
Interactive Black-Scholes pricing model with real-time Greeks calculation and 3D surface plots showing how option value changes across strike, volatility, and time to expiration. Demonstrates derivatives knowledge and quantitative modeling.

### 8. Sector Rotation Analyzer
Analyze relative sector performance across market cycles, identify momentum shifts between sectors, and visualize rotation patterns using relative strength metrics. Demonstrates top-down investment analysis and the ability to identify macro-driven trading signals.

---

## Tech Stack

| Layer | Tools |
|-------|-------|
| **Language** | R |
| **Web Framework** | Shiny, shinydashboard |
| **Market Data** | quantmod (Yahoo Finance), FRED API |
| **Visualization** | plotly, ggplot2 |
| **Risk Analytics** | PerformanceAnalytics, MASS |
| **Data Wrangling** | tidyverse, zoo, lubridate |
| **Tables** | DT |
| **Deployment** | shinyapps.io |
| **Development** | Google Antigravity, RStudio |

---

## What Each Project Signals to a Hiring Manager

| Project | Interview Signal |
|---------|-----------------|
| Equity Research Dashboard | "I can analyze and value individual securities" |
| Portfolio Risk Analyzer | "I understand portfolio construction and risk management" |
| Earnings Surprise Tracker | "I can work with event-driven data and identify patterns" |
| Factor Screener | "I understand quantitative strategies and backtesting rigor" |
| Macro Dashboard | "I can contextualize markets within the broader economy" |
| WACC Calculator | "I have strong corporate finance fundamentals" |
| Options Visualizer | "I understand derivatives and quantitative modeling" |
| Sector Rotation | "I can identify macro-driven investment signals" |

---

## How to Run Locally

Each project is self-contained in its own folder with its own `README.md` and setup instructions.

```r
# 1. Clone the repo
# git clone https://github.com/[username]/finance-portfolio.git

# 2. Open any project folder in RStudio or Google Antigravity

# 3. Install dependencies (first time only — see each project's README)

# 4. Run the app
shiny::runApp()
```

---

## Project Structure

```
finance-portfolio/
├── README.md                          ← You are here
├── equity-research-dashboard/
│   ├── app.R
│   ├── README.md
│   └── .gitignore
├── portfolio-risk-analyzer/
│   ├── app.R
│   ├── README.md
│   └── .gitignore
├── earnings-surprise-tracker/         ← Coming soon
├── factor-screener/                   ← Coming soon
├── macro-regime-dashboard/            ← Coming soon
├── wacc-calculator/                   ← Coming soon
├── options-pricing-visualizer/        ← Coming soon
└── sector-rotation-analyzer/          ← Coming soon
```

---

## About

I'm an aspiring finance professional building these tools to bridge the gap between financial analysis and technology. The goal is simple: demonstrate that I don't just understand financial concepts in theory — I can build tools that apply them to real data and produce actionable insights.

If you're a recruiter or hiring manager, I'd love to walk you through any of these projects in detail. Each one was designed to spark the kind of technical conversation that belongs in a finance interview.

---

## License

MIT License — free to use, modify, and learn from.