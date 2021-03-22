
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


# ranger  -----------

set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = ifelse(cut == "Ideal", 1, 0) %>% factor(levels = c(1, 0))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(100)

rf <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = T, num.trees = 1000)
rf2 <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = F, num.trees = 1000)

rang_oob_err(rf, dt)
rang_mtry(dt, top ~ ., 1:8, num.trees = 100)

dt[1, 2] <- NA
count_nas2(dt, all = T)
count_matches2(dt, c("F", "I1"), all = T, sort = T)

# plot missing
var_summary(attenu)
plot_missing2(attenu)
library(data.table)
