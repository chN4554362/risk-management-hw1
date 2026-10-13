# =====================================================================
# Практичне заняття №2: VaR та Expected Shortfall в R
# Активи: Apple (AAPL) та індекс S&P 500 (^GSPC)
# =====================================================================

# 1. Завантаження пакетів -------------------------------------------------
for (pkg in c("quantmod", "PerformanceAnalytics", "ggplot2")) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

if (!dir.exists("data")) dir.create("data")

# 2. Завантаження даних (останні 3 роки) ---------------------------------
tickers   <- c("AAPL", "^GSPC")
from_date <- Sys.Date() - 3 * 365
to_date   <- Sys.Date()

prices_list <- lapply(tickers, function(tk) {
  x <- getSymbols(tk, src = "yahoo", from = from_date, to = to_date,
                  auto.assign = FALSE)
  Ad(x)
})
prices <- na.omit(do.call(merge, prices_list))
colnames(prices) <- c("AAPL", "SP500")

# Зберігаємо сирі дані
write.csv(data.frame(Date = index(prices), coredata(prices)),
          "data/prices.csv", row.names = FALSE)

# 3. Логарифмічні дохідності -----------------------------------------------
returns <- na.omit(CalculateReturns(prices, method = "log"))
cat("Спостережень:", nrow(returns), " | NA:", sum(is.na(returns)), "\n\n")

# 4. VaR та ES ---------------------------------------------------------------
# Примітка: PerformanceAnalytics повертає VaR/ES як від'ємні числа (збиток).
calc_metrics <- function(r, name) {
  r <- as.numeric(r)
  r_xts <- xts(r, order.by = Sys.Date() - rev(seq_along(r)))  # лише для сумісності
  data.frame(
    Asset          = name,
    VaR_Hist_95    = as.numeric(VaR(r_xts, p = 0.95, method = "historical")),
    VaR_Hist_99    = as.numeric(VaR(r_xts, p = 0.99, method = "historical")),
    VaR_Param_95   = as.numeric(VaR(r_xts, p = 0.95, method = "gaussian")),
    VaR_Param_99   = as.numeric(VaR(r_xts, p = 0.99, method = "gaussian")),
    ES_Hist_95     = as.numeric(ES(r_xts,  p = 0.95, method = "historical"))
  )
}

results <- do.call(rbind, lapply(colnames(returns),
                                 function(nm) calc_metrics(returns[, nm], nm)))

results_pct <- results
results_pct[, -1] <- round(results[, -1] * 100, 2)

cat("--- РЕЗУЛЬТАТИ АНАЛІЗУ РИЗИКУ (у % від вартості, від'ємне = збиток) ---\n")
print(results_pct, row.names = FALSE)

write.csv(results_pct, "data/var_results.csv", row.names = FALSE)

# Markdown-таблиця для README.md
md <- c(
  "| Актив | Hist VaR 95% | Hist VaR 99% | Param VaR 95% | Param VaR 99% | ES 95% |",
  "|---|---|---|---|---|---|",
  apply(results_pct, 1, function(x)
    paste0("| ", x[1], " | ", paste0(trimws(x[-1]), "%", collapse = " | "), " |"))
)
writeLines(md, "data/results_table.md")
cat("\nТаблицю для README збережено у data/results_table.md\n")

# 5. Візуалізація -----------------------------------------------------------
plot_df <- do.call(rbind, lapply(colnames(returns), function(nm) {
  data.frame(Asset = nm, Return = as.numeric(returns[, nm]))
}))

var_lines <- data.frame(
  Asset = rep(results$Asset, 2),
  Level = rep(c("VaR 95%", "VaR 99%"), each = nrow(results)),
  Value = c(results$VaR_Hist_95, results$VaR_Hist_99)
)

p <- ggplot(plot_df, aes(x = Return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "grey80", colour = "white") +
  geom_density(colour = "steelblue", linewidth = 0.8) +
  geom_vline(data = var_lines, aes(xintercept = Value, colour = Level),
             linewidth = 1, linetype = "dashed") +
  scale_colour_manual(values = c("VaR 95%" = "orange", "VaR 99%" = "red")) +
  facet_wrap(~ Asset, scales = "free", ncol = 1) +
  labs(title = "Розподіл логарифмічних дохідностей та історичний VaR",
       x = "Щоденна лог-дохідність", y = "Щільність", colour = NULL) +
  theme_minimal(base_size = 13)

ggsave("data/var_chart.png", p, width = 8, height = 8, dpi = 150)
cat("Графік збережено: data/var_chart.png\n")
