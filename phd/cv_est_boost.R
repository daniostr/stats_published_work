df_a <-
  data.table(
    state = NULL,
    bur = NULL,
    bur_sd = NULL,
    ci_lwr = NULL,
    ci_upr = NULL
  )
df_rank <-
  data.table(state = NULL,
             HospFirstUnit = NULL,
             rank_ord = NULL)

model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * age_grpd + sex_male +
      year_hosp_date_f + season +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      sector_jew + cci_score +
      chapter_grp + offset(log_los),
    data = df,
    family = "quasipoisson"
  )
tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  by = c("age_grpd"),
  specs = "HospFirstUnit",
  type = "response",
  at = c(log_los = 9.21034)
) %>% as.data.table(.)


function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * age_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("age_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "[18,58]"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "[18,58]"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
less_58_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
less_58_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * age_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("age_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(58,71]"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(58,71]"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
low_mdm_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
low_mdm_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * age_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("age_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(71,81]"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(71,81]"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
high_mdm_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
high_mdm_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * age_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("age_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(81,111]"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(81,111]"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
more_81_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
more_81_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "[18,58]",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "[18,58]"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "[18,58]"]),
    ci_lwr = less_58_ci_lwr,
    ci_upr = less_58_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "(58,71]",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(58,71]"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(58,71]"]),
    ci_lwr = low_mdm_ci_lwr,
    ci_upr = low_mdm_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "(71,81]",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(71,81]"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(71,81]"]),
    ci_lwr = high_mdm_ci_lwr,
    ci_upr = high_mdm_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "(81,111]",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(81,111]"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$age_grpd == "(81,111]"]),
    ci_lwr = more_81_ci_lwr,
    ci_upr = more_81_ci_upr
  )
))
setorder(tbl_bld_count,-rate)
setorder(tbl_bld_count, age_grpd)
tbl_bld_count$rank <- rep(1:7, 4)
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "[18,58]",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$age_grpd == "[18,58]"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$age_grpd == "[18,58]"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "(58,71]",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$age_grpd == "(58,71]"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$age_grpd == "(58,71]"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "(71,81]",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$age_grpd == "(71,81]"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$age_grpd == "(71,81]"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "(81,111]",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$age_grpd == "(81,111]"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$age_grpd == "(81,111]"]
  )
))

# --------------

model_bld_count <- glm(
  total_count_bld ~ HospFirstUnit * sex_male +
    year_hosp_date_f + season +
    age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                           3) +
    sector_jew + cci_score +
    chapter_grp + offset(log_los),
  data = df,
  family = "quasipoisson"
)
tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  by = c("sex_male"),
  specs = "HospFirstUnit",
  type = "response",
  at = c(log_los = 9.21034)
) %>% as.data.table(.)

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <- glm(
    total_count_bld ~ HospFirstUnit * sex_male +
      year_hosp_date_f + season +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      sector_jew + cci_score +
      chapter_grp + offset(log_los),
    data = d2,
    family = "quasipoisson"
  )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("sex_male"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$sex_male == 0])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$sex_male == 0])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
female_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
female_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <- glm(
    total_count_bld ~ HospFirstUnit * sex_male +
      year_hosp_date_f + season +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      sector_jew + cci_score +
      chapter_grp + offset(log_los),
    data = d2,
    family = "quasipoisson"
  )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("sex_male"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$sex_male == 1])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$sex_male == 1])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
male_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
male_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]


df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "Female",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$sex_male == 0]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$sex_male == 0]),
    ci_lwr = female_ci_lwr,
    ci_upr = female_ci_upr
  )
))

df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "Male",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$sex_male == 1]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$sex_male == 1]),
    ci_lwr = male_ci_lwr,
    ci_upr = male_ci_upr
  )
))

setorder(tbl_bld_count,-rate)
setorder(tbl_bld_count, sex_male)
tbl_bld_count$rank <- rep(1:7, 2)
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "Female",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$sex_male == 0],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$sex_male == 0]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "Male",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$sex_male == 1],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$sex_male == 1]
  )
))

# --------------

model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * sector_jew + sex_male +
      year_hosp_date_f + season +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      cci_score +
      chapter_grp + offset(log_los),
    data = df,
    family = "quasipoisson"
  )
tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  by = c("sector_jew"),
  specs = "HospFirstUnit",
  type = "response",
  at = c(log_los = 9.21034)
) %>% as.data.table(.)

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * sector_jew + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("sector_jew"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$sector_jew == 0])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$sector_jew == 0])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
non_jew_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
non_jew_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * sector_jew + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("sector_jew"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$sector_jew == 1])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$sector_jew == 1])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
jew_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
jew_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]


df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "Non_Jew",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$sector_jew == 0]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$sector_jew == 0]),
    ci_lwr = non_jew_ci_lwr,
    ci_upr = non_jew_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "Jew",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$sector_jew == 1]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$sector_jew == 1]),
    ci_lwr = jew_ci_lwr,
    ci_upr = jew_ci_upr
  )
))
setorder(tbl_bld_count,-rate)
setorder(tbl_bld_count, sector_jew)
tbl_bld_count$rank <- rep(1:7, 2)

df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "Non_Jew",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$sector_jew == 0],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$sector_jew == 0]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "Jew",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$sector_jew == 1],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$sector_jew == 1]
  )
))
# --------------

model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * season + sector_jew + sex_male +
      year_hosp_date_f +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      cci_score +
      chapter_grp + offset(log_los),
    data = df,
    family = "quasipoisson"
  )
tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  by = c("season"),
  specs = "HospFirstUnit",
  type = "response",
  at = c(log_los = 9.21034)
) %>% as.data.table(.)
function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("season"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$season == "winter"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$season == "winter"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
winter_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
winter_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * season + sex_male + sector_jew +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("season"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$season == "spring"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$season == "spring"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
spring_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
spring_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * season + sex_male + sector_jew +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("season"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$season == "summer"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$season == "summer"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
summer_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
summer_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * season + sex_male + sector_jew +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("season"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$season == "fall"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$season == "fall"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
fall_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
fall_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]


df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "winter",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$season == "winter"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$season == "winter"]),
    ci_lwr = winter_ci_lwr,
    ci_upr = winter_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "spring",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$season == "spring"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$season == "spring"]),
    ci_lwr = spring_ci_lwr,
    ci_upr = spring_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "summer",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$season == "summer"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$season == "summer"]),
    ci_lwr = summer_ci_lwr,
    ci_upr = summer_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "fall",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$season == "fall"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$season == "fall"]),
    ci_lwr = fall_ci_lwr,
    ci_upr = fall_ci_upr
  )
))
setorder(tbl_bld_count,-rate)
setorder(tbl_bld_count, season)
tbl_bld_count$rank <- rep(1:7, 4)
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "winter",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$season == "winter"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$season == "winter"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "spring",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$season == "spring"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$season == "spring"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "summer",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$season == "summer"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$season == "summer"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "fall",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$season == "fall"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$season == "fall"]
  )
))
# --------------

model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * cci_score_grpd + sex_male +
      year_hosp_date_f + season +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      sector_jew + cci_score +
      chapter_grp + offset(log_los),
    data = df,
    family = "quasipoisson"
  )
tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  by = c("cci_score_grpd"),
  specs = "HospFirstUnit",
  type = "response",
  at = c(log_los = 9.21034)
) %>% as.data.table(.)

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * cci_score_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("cci_score_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "mild"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "mild"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
mild_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
mild_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * cci_score_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("cci_score_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "moderate"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "moderate"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
moderate_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
moderate_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * cci_score_grpd + sex_male +
        year_hosp_date_f + season +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        sector_jew + cci_score +
        chapter_grp + offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("cci_score_grpd"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "severe"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "severe"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
severe_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
severe_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]


df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "mild",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "mild"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "mild"]),
    ci_lwr = mild_ci_lwr,
    ci_upr = mild_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "moderate",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "moderate"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "moderate"]),
    ci_lwr = moderate_ci_lwr,
    ci_upr = moderate_ci_upr
  )
))
df_a <- rbindlist(list(
  df_a,
  data.table(
    state = "severe",
    bur = mean(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "severe"]),
    bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$cci_score_grpd == "severe"]),
    ci_lwr = severe_ci_lwr,
    ci_upr = severe_ci_upr
  )
))

setorder(tbl_bld_count,-rate)
setorder(tbl_bld_count, cci_score_grpd)
tbl_bld_count$rank <- rep(1:7, 3)
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "mild",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$cci_score_grpd == "mild"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$cci_score_grpd == "mild"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "moderate",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$cci_score_grpd == "moderate"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$cci_score_grpd == "moderate"]
  )
))
df_rank <- rbindlist(list(
  df_rank,
  data.table(
    state = "severe",
    HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$cci_score_grpd == "severe"],
    rank_ord = tbl_bld_count$rank[tbl_bld_count$cci_score_grpd == "severe"]
  )
))

# --------------
model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
      year_hosp_date_f +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      cci_score +
      offset(log_los),
    data = df,
    family = "quasipoisson"
  )
tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  by = c("chapter_grp"),
  specs = "HospFirstUnit",
  type = "response",
  at = c(log_los = 9.21034)
) %>% as.data.table(.)

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("chapter_grp"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Symptoms, Signs, And Ill-Defined Conditions"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Symptoms, Signs, And Ill-Defined Conditions"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
sss_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
sss_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("chapter_grp"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Circulatory System"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Circulatory System"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
circu_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
circu_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("chapter_grp"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Respiratory System"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Respiratory System"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
resp_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
resp_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("chapter_grp"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Genitourinary System"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Genitourinary System"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
genit_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
genit_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("chapter_grp"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Digestive System"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Digestive System"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
digest_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
digest_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

function_1 <- function(data, i) {
  d2 <- data[i, ]
  model_bld_count <-
    glm(
      total_count_bld ~ HospFirstUnit * chapter_grp + season + sector_jew + sex_male +
        year_hosp_date_f +
        age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                               3) +
        cci_score +
        offset(log_los),
      data = d2,
      family = "quasipoisson"
    )
  tbl_bld_count <- emmeans::emmeans(
    model_bld_count,
    by = c("chapter_grp"),
    specs = "HospFirstUnit",
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
  mean_est = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Infectious And Parasitic Diseases"])
  sd_est = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Infectious And Parasitic Diseases"])
  cv_est = sd_est / mean_est * 100
  return(cv_est)
}

bootstrap_correlation <- boot(df, function_1, R = 1000)
ifectious_ci_lwr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 4]
ifectious_ci_upr <-
  boot.ci(boot.out = bootstrap_correlation, type = c("perc"))$percent[, 5]

df_a <-
  rbindlist(list(
    df_a,
    data.table(
      state = "Symptoms, Signs, And Ill-Defined Conditions",
      bur = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Symptoms, Signs, And Ill-Defined Conditions"]),
      bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Symptoms, Signs, And Ill-Defined Conditions"]),
      ci_lwr = sss_ci_lwr,
      ci_upr = sss_ci_upr
    )
  ))
df_a <-
  rbindlist(list(
    df_a,
    data.table(
      state = "Diseases Of The Circulatory System",
      bur = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Circulatory System"]),
      bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Circulatory System"]),
      ci_lwr = circu_ci_lwr,
      ci_upr = circu_ci_upr
    )
  ))
df_a <-
  rbindlist(list(
    df_a,
    data.table(
      state = "Diseases Of The Respiratory System",
      bur = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Respiratory System"]),
      bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Respiratory System"]),
      ci_lwr = resp_ci_lwr,
      ci_upr = resp_ci_upr
    )
  ))
df_a <-
  rbindlist(list(
    df_a,
    data.table(
      state = "Diseases Of The Genitourinary System",
      bur = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Genitourinary System"]),
      bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Genitourinary System"]),
      ci_lwr = genit_ci_lwr,
      ci_upr = genit_ci_upr
    )
  ))
df_a <-
  rbindlist(list(
    df_a,
    data.table(
      state = "Diseases Of The Digestive System",
      bur = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Digestive System"]),
      bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Diseases Of The Digestive System"]),
      ci_lwr = digest_ci_lwr,
      ci_upr = digest_ci_upr
    )
  ))
df_a <-
  rbindlist(list(
    df_a,
    data.table(
      state = "Infectious And Parasitic Diseases",
      bur = mean(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Infectious And Parasitic Diseases"]),
      bur_sd = sd(tbl_bld_count$rate[tbl_bld_count$chapter_grp == "Infectious And Parasitic Diseases"]),
      ci_lwr = ifectious_ci_lwr,
      ci_upr = ifectious_ci_upr
    )
  ))

setorder(tbl_bld_count,-rate)
setorder(tbl_bld_count, chapter_grp)
tbl_bld_count$rank <- rep(1:7, 7)
df_rank <-
  rbindlist(list(
    df_rank,
    data.table(
      state = "Symptoms, Signs, And Ill-Defined Conditions",
      HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$chapter_grp == "Symptoms, Signs, And Ill-Defined Conditions"],
      rank_ord = tbl_bld_count$rank[tbl_bld_count$chapter_grp == "Symptoms, Signs, And Ill-Defined Conditions"]
    )
  ))
df_rank <-
  rbindlist(list(
    df_rank,
    data.table(
      state = "Diseases Of The Circulatory System",
      HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$chapter_grp == "Diseases Of The Circulatory System"],
      rank_ord = tbl_bld_count$rank[tbl_bld_count$chapter_grp == "Diseases Of The Circulatory System"]
    )
  ))
df_rank <-
  rbindlist(list(
    df_rank,
    data.table(
      state = "Diseases Of The Respiratory System",
      HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$chapter_grp == "Diseases Of The Respiratory System"],
      rank_ord = tbl_bld_count$rank[tbl_bld_count$chapter_grp == "Diseases Of The Respiratory System"]
    )
  ))
df_rank <-
  rbindlist(list(
    df_rank,
    data.table(
      state = "Diseases Of The Genitourinary System",
      HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$chapter_grp == "Diseases Of The Genitourinary System"],
      rank_ord = tbl_bld_count$rank[tbl_bld_count$chapter_grp == "Diseases Of The Genitourinary System"]
    )
  ))
df_rank <-
  rbindlist(list(
    df_rank,
    data.table(
      state = "Diseases Of The Digestive System",
      HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$chapter_grp == "Diseases Of The Digestive System"],
      rank_ord = tbl_bld_count$rank[tbl_bld_count$chapter_grp == "Diseases Of The Digestive System"]
    )
  ))
df_rank <-
  rbindlist(list(
    df_rank,
    data.table(
      state = "Infectious And Parasitic Diseases",
      HospFirstUnit = tbl_bld_count$HospFirstUnit[tbl_bld_count$chapter_grp == "Infectious And Parasitic Diseases"],
      rank_ord = tbl_bld_count$rank[tbl_bld_count$chapter_grp == "Infectious And Parasitic Diseases"]
    )
  ))
write.csv(df_a, "cvest.csv")
write.csv(df_rank, "rank_exposure.csv")

df_a <- fread("cvest.csv")
df_a[, `:=`(cv_est = round(100 * bur_sd / bur, 2))]
df_a$Index <- 1:21
df_a$type <-
  factor(c(rep("Demographic", 8), rep("Temporal", 4), rep("Clinical", 9)),
         levels = c("Demographic", "Temporal", "Clinical"))
df_a[, `:=`(bur_n = paste0(formatC(
  bur, format = "d", big.mark = ","
),
"(",
round(bur_sd), ")"))]



labels_plt <-
  c(
    "Age, <58",
    "Age, 58-71",
    "Age, 71-81",
    "Age, ≥81",
    "Sex, Female",
    "Sex, Male",
    "Sector, Arab",
    "Sector, Jewish",
    "Season, Winter",
    "Season, Spring",
    "Season, Summer",
    "Seaeson, Fall",
    "CCI, Mild",
    "CCI, Moderate",
    "CCI, Severe",
    "ICD-9 Cat.,  Symptoms, Signs, And Ill-Defined Conditions",
    "ICD-9 Cat.,  Diseases Of The Circulatory System",
    "ICD-9 Cat.,  Diseases Of The Respiratory System",
    "ICD-9 Cat.,  Diseases Of The Genitourinary System",
    "ICD-9 Cat.,  Diseases Of The Digestive System",
    "ICD-9 Cat.,  Infectious And Parasitic Diseases"
  )


plot1 <- ggplot(df_a, aes(y = Index, x = cv_est)) +
  geom_point(shape = 18, size = 5) +
  geom_errorbarh(aes(xmin = ci_lwr, xmax = ci_upr), height = 0.25) +
  scale_y_continuous(
    name = "",
    breaks = 1:21,
    labels = labels_plt,
    trans = "reverse"
  ) +
  xlab("CV, % (95% CI)") +
  ylab(" ") +
  theme_gray() +
  theme(
    plot.margin = unit(c(5.5, 12, 5.5, 5.5), "pt"),
    panel.spacing = unit(0.5, "lines"),
    panel.border = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.y = element_text(size = 22, colour = "black"),
    axis.text.x.bottom = element_text(size = 22, colour = "black"),
    axis.title.x = element_text(size = 22, colour = "black"),
    text = element_text(family = "serif", size = 22),
    strip.text = element_text(size = 22)
  ) + facet_grid(type ~ .,
                 scales = "free",
                 space = "free",
                 switch = "y") +
  geom_vline(
    xintercept = 6.85,
    color = "red",
    linetype = "dashed",
    cex = 1,
    alpha = 0.5
  )


table_base <- ggplot(df_a, aes(y = state)) +
  ylab(NULL) + xlab("  ") +
  theme(
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_text(hjust = 0.5, size = 20),
    axis.text.x = element_text(
      color = "white",
      hjust = -3,
      size = 20
    ),
    ## This is used to help with alignment
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank(),
    strip.background = element_rect(fill = NA, color = "grey60"),
    strip.text = element_blank(),
    text = element_text(family = "serif")
  )   + facet_grid(type ~ ., scales = "free", space = "free")

## OR point estimate table
tab1 <- table_base +
  geom_text(
    aes(
      y = rev(Index),
      x = 1,
      label = bur_n
    ),
    family = "serif",
    size = 8,
    vjust = 0.1
  ) +
  theme(text = element_text(family = "serif"))


gt = ggplot_gtable(ggplot_build(tab1))
gt$layout$clip = "off"

gt1 = ggplot_gtable(ggplot_build(plot1))
gt1$layout$clip = "off"

lay <-  matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2), nrow = 1)
p3 <- grid.arrange(plot1, gt, layout_matrix = lay, clip = "off")
ggsave("filename.jpg", p3, width = 15, height = 12)

library(lme4)

summ(lmer(rank_ord ~ (1 | HospFirstUnit), data = df_rank))
