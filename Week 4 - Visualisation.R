
library(readxl)
credit <- read_excel("C:/Users/bop33746/Dropbox/11 - Teaching/WiSe 2026-2027/QMH (WiSe 26-27) - Quantitative methods for historians/Data/credit.xlsx")

summary(credit)


df <- credit
df$share_subsidy <- df$crediti_agevolati / df$credit_total

summary(df)




library(dplyr)
df <- credit |>
  mutate(share_subsidy = crediti_agevolati / credit_total)


summary(df)


boxplot(share_subsidy ~ year, data = df,
        xlab = "", ylab = "Share of subsidised credit",
        ylim = c(0, 1), col = "grey85")

library(ggplot2)


ggplot(df, aes(x = factor(year), y = share_subsidy, fill = factor(year))) +
  geom_boxplot(outlier.shape = 21) +
  #stat_summary(fun = mean, geom = "point", shape = 3, size = 2.5) +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = NULL,
       y = "Share of subsidised credit",
       title = "Share of subsidised credit by province, 1982–1988") +
  theme_classic()
