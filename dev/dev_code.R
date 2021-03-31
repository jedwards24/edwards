# Incomplete code on possible updates

# keys? ---------------
# Sub function to build functional dependencies?
# is_one2one() might be useful here
# Or use chk::check_key()
library(tidyverse)

only_uniques <- function(cols, data) {
  first_dupe <- anyDuplicated(data[, cols])
  first_dupe == 0
}

dt <- mtcars
combo_list <- lapply(
  X = seq_along(dt),
  FUN = combn,
  x = names(dt),
  simplify = FALSE
)

combos <- tibble(columns = unlist(combo_list, recursive = FALSE)) %>%
  mutate(valid = vapply(columns, only_uniques, logical(1), data = dt))

filter(combos, valid == T) %>% slice(1) %>% pull(columns)

anyDuplicated(dt[c("mpg", "wt")])

head(dt)
as_tibble(dt)
dt[1:11, ]
only_uniques(c("mpg", "wt"), dt)

?anyDuplicated

# benchmarking func ---------
# I want to automate comparisons between functions with a range of n arguments, then plot the results.
library(microbenchmark)
#https://stackoverflow.com/questions/48362852/microbenchmark-pass-a-variable-to-the-function
#https://github.com/r-lib/bench

n_vec <- c(1e3, 1e4, 1e5, 1e6)
set.seed(11)
for(i in seq_along(n_vec)){
  mb <- microbenchmark(...)
}

# glmnet tools -------------
library(tidyverse)
library(glmnet)
#library(glmnetUtils)
library(edwards)
#source("funcs/glmnet.R")
set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = (cut == "Ideal") %>% factor(levels = c(F, T))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(500) %>%
  mutate(clarity = factor(clarity, ordered = F)) #changed to test both types of factors
dt
dt$clarity %>% levels

xmat <- model.matrix(top ~ . * table, dt)[, -1]
set.seed(22)
fit <- cv.glmnet(x=xmat, y = dt$top, family="binomial", nfolds = 3)

plot(fit)
colnames(xmat)
glmnet_to_table(fit, names(dt), s = "lambda.min")
glmnet_to_table(fit, names(dt))
glmnet_to_table(fit)


glmnet_to_table(fit, names(dt), s = 10^-3)
glmnet_to_table(fit, s = 10^-3)

tt$name %>% str_extract_all(var_reg)
tt$name %>% str_remove_all(paste0("(?!((", var_reg, "))).+"))

# multiple interactions
xmat2 <- model.matrix(top ~ . * color * table, dt)[, -1]
set.seed(22)
fit2 <- cv.glmnet(x=xmat2, y = dt$top, family="binomial", nfolds = 3)
plot(fit2)
colnames(xmat2)
glmnet_to_table2(fit2, names(dt)) %>% prinf

glmnet_to_table2(fit2, names(dt), s = 10 ^ -2) %>% prinf
glmnet_to_table2(fit2, s = 10 ^ -2) %>% prinf
glmnet_to_table2(fit2)
coef(fit)

# explore glmnet fit object
names(fit)
fit$lambda %>% head
fit$call
fit$nzero
fit$name
fit$glmnet.fit %>% names
fit$glmnet.fit$lambda %>% length()
fit$glmnet.fit$classnames
fit$glmnet.fit$beta %>% head
fit$glmnet.fit$beta %>% dim # 20 rows (vars) x 100 cols (lambda)

fit$glmnet.fit$a0 %>% head
fit$glmnet.fit$call
fit$glmnet.fit$beta %>% str

coef(fit) %>% as.matrix() #coefs at s = "lanmbda.1se" (default s)
glmnet_to_table(fit, names(dtin), s = min(fit$lambda), min_coef = 0) %>% prinf # 2 intercepts??

fit2 <- glmnet::cv.glmnet(x=x_mat, y = dt$top, family="binomial")
names(fit2)

# all lambda models
xmat <- model.matrix(top ~ ., dt)[, -1]
set.seed(22)
fit <- cv.glmnet(x=xmat, y = dt$top, family="binomial")
plot(fit)

beta_mat <- as.matrix(fit$glmnet.fit$beta)
tb <- beta_mat %>% t() %>% as_tibble() %>%
  mutate(s = fit$lambda) %>%
  mutate(intercept = fit$glmnet.fit$a0) %>%
  mutate(nzero = fit$nzero) %>%
  mutate(dev = fit$cvm) %>%
  select(s, nzero, dev, intercept, everything())
tb
view(tb)
names(fit)
fit$cvm %>% head
fit$cvup %>% head
fit$cvsd %>% head
fit$glmnet.fit$dev.ratio
fit$glmnet.fit$nulldev
log(s_all) %>% head

plot(x = log(fit$lambda), y = fit$cvm, typ = 'l')

rownames(beta_mat)
inter <- fit$glmnet.fit$a0

tb %>%
  select(-nzero, -dev, -intercept) %>%
  pivot_longer(-s) %>%
  ggplot(aes(log(s), value, colour = name, group = name)) +
  geom_line()

# nzero vs dev
# only plot best dev for each nzero
tb %>%
  select(nzero, dev) %>%
  group_by(nzero) %>%
  filter(dev == min(dev)) %>%
  ggplot(aes(nzero, dev)) +
  geom_line()

# min s to be included
s_all <- sort(fit$lambda)
level_names <- rownames(as.matrix(coef(fit, s="lambda.1se")))

#ns <- length(s_all)
max_include <- numeric(length(level_names))
for (s in s_all){
  ce <- as.vector(coef(fit, s=s))
  max_include[abs(ce) > 0] <- s
}
ranks <- tibble(var = level_names, max_s_include = max_include, rank = row_number(desc(max_s_include)))
arrange(ranks, rank) %>% prinf

as.vector(coef(fit, s=s))

glmnet(x=xmat, y = dt$top, family="binomial") %>% plot(label = T)


#simplify_datetime() --------------
# Wrapper for following but print which cols are converted.
# mutate_if(dt, is_simple_datetime, ~as.Date(.))

# count_int() var_integer()? ---------
summarise_if(dt2, is.numeric, count_int) %>% pivot_longer(cols = everything())

# datetime_summary() -------
#Explore which are simple datetimes. Maybe also check which are 0 in Excel i.e.equal as.POSIXct("1899-12-31", tz = "UTC")
summarise_if(dt, is.POSIXct, is_simple_datetime) %>% pivot_longer(cols = everything())