# prop_ci test ------------

library(tidyverse)
dt <- diamonds %>%
  sample_n(1000) %>%
  mutate(best = cut == "Ideal")
count(dt, best)
count_over(dt, cut, color)
dt[2, 4] <- NA
dt[3, 4] <- "(Missing)"
dt <- mutate(dt, clarity = fct_recode(clarity, "(Missing)" = "I1"))
count(dt, clarity)
prop_ci(dt, "best", "clarity", min_n = 0)
prop_ci(dt, "best", "clarity", min_n = 0, plot = F)
gg <- prop_ci(dt, "best", "clarity", min_n = 0, return_plot = T)
gg + ylab("Mean Proportion `best`")

prop_ci2(dt, "best", "clarity", min_n = 0)
prop_ci2(dt, "best", "clarity", min_n = 0, order_n = T)
expect_s3_class(gg, "ggplot")

dt
prop_ci(dt, "best", "clarity", min_n = 0)
prop_ci(dt, "best", "clarity", min_n = 60)
prop_ci(dt, "best", "clarity", min_n = 300)
prop_ci(dt, "best", "color", min_n = 0)


prop_ci2(dt, "best", "table", min_n = 10, order_n = F)
prop_ci2(dt, "best", "table", min_n = 10, order_n = T)
prop_ci2(dt, "best", "table", min_n = 10)


# plot missing--------
var_summary(attenu)
plot_missing2(attenu)
library(data.table)
