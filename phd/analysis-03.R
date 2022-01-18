# Load libraries -----------------------

library(tableone)
library(visdat)
library(clipr)
library(icd.data)
library(jtools)
library(survival)
library(nnet)
library(cobalt)
library(survey)
library(ggpubr)
library(MASS)
library(emmeans)
library(stats)
library(ggridges)
library(viridis)
library(boot)
library(lme4)
library(spearmanCI)
library(gridExtra)
source("R/loading-data.R")

set.seed(1)
emm_options(rg.limit = 1000000000000000000)


# Copy tables -----------------------
df_list = list(
  df_chemistry_er_c = copy(df_chemistry_er),
  df_chemistry_er_raw_c = copy(df_chemistry_er_raw),
  df_hematology_er_c = copy(df_hematology_er),
  df_hematology_er_raw_c = copy(df_hematology_er_raw),
  df_measures_er_c = copy(df_measures_er),
  df_chemistry_hosp_raw_c = copy(df_chemistry_hosp_raw),
  df_hematology_hosp_raw_c = copy(df_hematology_hosp_raw),
  df_measures_hosp_raw_ofek_c = copy(df_measures_hosp_raw_ofek),
  df_measures_hosp_raw_chameleon_c = copy(df_measures_hosp_raw_chameleon),
  df_hosp_and_er_details_c = copy(df_hosp_and_er_details),
  df_index_c = copy(df_index),
  df_demographic_relative_c = copy(df_demographic_relative),
  df_demographic_static_c = copy(df_demographic_static),
  df_index_c = copy(df_index),
  df_cci_c = copy(df_cci)
)

# Loop over tables and define ID as string and dates as date ymd_hms
for (i in df_list) {
  for (j in colnames(i)) {
    if (j %in% c("PID", "EncounterID", "ErEncounterID", "HospEncounterID")) {
      i[, (j) := lapply(.SD, as.character), .SDcols = j]
    }
    if (j %like% c("Date") | j %like% c("date")) {
      i[, (j) := lapply(.SD, lubridate::ymd_hms), .SDcols = j]
    }
  }
}
remove(i)
remove(j)


df <- df_list$df_hosp_and_er_details_c
# Add Columns
df <-
  unique(df_list$df_demographic_static_c, by = "PID")[df, on = c("PID")]
# Recoding and mutating
df[, `:=`(
  year_hosp_date = lubridate::year(HospEncounterDate),
  month_hosp_date = lubridate::month(HospEncounterDate)
)]
df[, `:=`(date_quarter = lubridate::quarter(HospEncounterDate))]
df[, `:=`(
  sex_male = as.factor(fifelse(GenderID == 2, 0, 1)),
  age_at_hosp = round(time_length(BirthDate %--% HospEncounterDate, "year")),
  sector_jew = as.factor(fifelse(SectorID == 2, 0, 1))
)]
df[, `:=`(qrtr = as.factor(lubridate::quarter(HospEncounterDate)))]
df[, `:=`(mnth = as.factor(lubridate::month(HospEncounterDate)))]
df[, `:=`(year_hosp_date_f = factor(year_hosp_date))]
df[, `:=`(length_of_stay = round(time_length(
  HospEncounterDate %--% HospDischargeDate, "days"
)))]



# Filters:
## Years between 2013 and 2019
## At least two days of hospitalizations
df <- df[HospFirstUnit >= 10011 & HospFirstUnit <= 10018]
df <- df[year_hosp_date >= 2014 & year_hosp_date <= 2019]
df <- df[!is.na(ErEncounterID)]
df <- unique(df, by = c("PID", "HospEncounterID"))
df <-
  df[(time_length(HospEncounterDate %--% HospDischargeDate, "hour") > 48) &
       (time_length(HospEncounterDate %--% HospDischargeDate, "hour") <= 336)]


# Outcome mutations ------------
## Death within 30 days
df[, `:=`(death_30 = fifelse(DeathDate <= HospDischargeDate + days(30), 1, 0))]
df[is.na(death_30), `:=`(death_30 = 0)]

# Readmissions      ------------
setorder(df, PID, HospEncounterDate)
df[, `:=`(
  HospEncounterDate_next = c(df$HospEncounterDate[-1], NA),
  PID_next = c(df$PID[-1], NA)
)]
df[, `:=`(time_to_next_hosp = round(
  time_length(HospDischargeDate %--% HospEncounterDate_next, "days")
))]
df[, `:=`(re_hosp = fifelse(PID == PID_next &
                              time_to_next_hosp <= 30, 1, 0))]

# ICD 9 coding chapters
df[, `:=`(diagnosis_code = as.factor(substr(HospMainDiagnosisCode, 1 , 3)))]
icd9_chapter <-
  data.table(icd9cm_hierarchy[, c("three_digit", "chapter")]) %>%
  unique(., by = "three_digit")
df <- icd9_chapter[df, on = c(three_digit = "diagnosis_code")]
temp_1 = data.table(table(df$chapter))
setorder(temp_1,-N)
df[, `:=`(chapter = factor(chapter, levels = temp_1$V1))]

# cci score formula
df_cci <- df_list$df_cci_c
cci_calc <- function(x) {
  x[, `:=`(
    cci_score = MYOCARDIAL_INFARCTION * 1 +
      CONGESTIVE_HEART_FAILURE * 1 +
      PERIPHERAL_VASCULAR_DISEASE * 1 +
      CEREBROVASCULAR_DISEASE * 1 +
      DEMENTIA * 1 +
      CHRONIC_PULMONARY_DISEASE * 1 +
      REHUMATOLOGIC_DISEASE * 1 +
      PEPTIC_ULCER_DISEASE * 1 +
      MILD_LIVER_DISEASE * 1 +
      DIABETES * 1 +
      HEMIPLEGIA_OR_PARAPLEGIA * 2 +
      RENAL_DISEASE * 2 +
      DIABETES_WITH_CHRONIC_COMPLICATIONS * 2 +
      ANY_MALIGANCY_INCLUDING_LEUKAEMIA_AND_LYMPPHOMA * 2 +
      MODERATE_OR_SEVERE_LIVER_DISEASA * 3 +
      METASTATIC_SOLID_TUMOR * 6 +
      AIDS * 6
  )]
}
cci_calc(df_cci)
df_cci <- unique(df_cci, by = c("PID", "HospEncounterDate"))
df <- df_cci[, c("PID",
                 "HospEncounterDate",
                 "cci_score")][df, on = c("PID", "HospEncounterDate")]
df[, `:=`(cci_score_grpd = cut(
  cci_score,
  breaks = c(-Inf, 2, 4, Inf),
  labels = c("mild", "moderate", "severe")
))]

# More re-coding and mutations
df[, `:=`(HospFirstUnit = as.factor(HospFirstUnit))]
df[, `:=`(mortality_or_re_hosp = fifelse(re_hosp == 0 &
                                           death_30 == 0, 0, 1))]
grouping_chapter <- levels(df$chapter)[7:19]
df$chapter_other = factor("Other")
df[, `:=`(chapter_grp = fifelse(
  chapter %in% grouping_chapter,
  as.character(chapter_other),
  as.character(chapter)
))]
df[, `:=`(chapter_grp = factor(
  chapter_grp,
  levels = c(
    "Symptoms, Signs, And Ill-Defined Conditions",
    "Diseases Of The Circulatory System" ,
    "Diseases Of The Respiratory System",
    "Diseases Of The Genitourinary System",
    "Diseases Of The Digestive System",
    "Infectious And Parasitic Diseases",
    "Other"
  ),
  labels = c(
    "Symptoms, Signs, And Ill-Defined Conditions",
    "Diseases Of The Circulatory System" ,
    "Diseases Of The Respiratory System",
    "Diseases Of The Genitourinary System",
    "Diseases Of The Digestive System",
    "Infectious And Parasitic Diseases",
    "Other"
  )
))]
df[, `:=`(log_los = log(length_of_stay))]


# Seasonality
df[, `:=`(summer_ses = 0)]
df[, `:=`(fall_ses = 0)]
df[, `:=`(winter_ses = 0)]
df[, `:=`(spring_ses = 0)]

for (i in 2014:2019) {
  summer_lwr <- mdy(paste0("05-28-" , i))
  summer_upr <- mdy(paste0("09-19-" , i))
  df[, `:=`(summer_ses = fifelse(
    (
      HospEncounterDate >= summer_lwr & HospEncounterDate <= summer_upr
    ) |
      summer_ses == 1,
    1,
    0
  ))]
  
  fall_lwr <- mdy(paste0("09-20-" , i))
  fall_upr <- mdy(paste0("12-03-" , i))
  df[, `:=`(fall_ses = fifelse(
    (HospEncounterDate >= fall_lwr & HospEncounterDate <= fall_upr) |
      fall_ses == 1,
    1,
    0
  ))]
  
  winter_lwr <- mdy(paste0("12-04-" , i))
  winter_upr <- mdy(paste0("03-27-" , i))
  df[, `:=`(winter_ses = fifelse(
    (
      HospEncounterDate >= winter_lwr & HospEncounterDate <= winter_upr
    ) |
      winter_ses == 1,
    1,
    0
  ))]
  
  spring_lwr <- mdy(paste0("03-28-" , i))
  spring_upr <- mdy(paste0("05-27-" , i))
  df[, `:=`(spring_ses = fifelse(
    (
      HospEncounterDate >= spring_lwr & HospEncounterDate <= spring_upr
    ) |
      spring_ses == 1,
    1,
    0
  ))]
  
}
df$season <- "winter"
df$season <- ifelse(df$summer_ses == 1, "summer", df$season)
df$season <- ifelse(df$fall_ses == 1, "fall", df$season)
df$season <- ifelse(df$spring_ses == 1, "spring", df$season)

# Delete NA, 300~ cases ---------------
df <- df[!is.na(HospFirstUnit) &
           !is.na(sex_male) &
           !is.na(age_at_hosp) &
           !is.na(sector_jew) &
           !is.na(chapter),]
df[, `:=`(age_grpd = cut_number(age_at_hosp, 4))]

# Count blood tests

cbc_names = c(
  "HB",
  "HCT",
  "RBC",
  "MCH",
  "MCV",
  "MCHC",
  "WBC",
  "NEUT.abs",
  "LYMP.abs",
  "EOS.abs",
  "BASO abs",
  "PLT"
)
basic_chem_names = c(
  "GLUCOSE",
  "UREA",
  "CREATININE",
  "SODIUM",
  "POTASSIUM",
  "CHLORIDE")

df_chem_hosp = df_list$df_chemistry_hosp_raw_c
df_chem_hosp <- df_chem_hosp[lubridate::year(darr_datetime) >= 2014]
df_chem_hosp = df_chem_hosp[test %in% basic_chem_names, ]
df_chem_hosp <- df[, c("PID",
                       "HospEncounterID",
                       "ErDischargeDate")][df_chem_hosp, on = c("PID",
                                                                "HospEncounterID")]
df_chem_hosp <- df_chem_hosp[!is.na(ErDischargeDate)]
df_chem_hosp <-
  df_chem_hosp[date(ErDischargeDate) != date(darr_datetime)]
df_chem_hosp = df_chem_hosp[, .(count_basic_chemistry = .N), by = c("PID",
                                                                    "HospEncounterID",
                                                                    "test")]
setorder(df_chem_hosp, PID, HospEncounterID,-count_basic_chemistry)
df_chem_hosp <-
  unique(df_chem_hosp, by = c("PID", "HospEncounterID"))
df_chem_hosp <- df_chem_hosp[, -c("test")]

df_cbc_hosp = df_list$df_hematology_hosp_raw_c
df_cbc_hosp <- df_cbc_hosp[lubridate::year(darr_datetime) >= 2014]
df_cbc_hosp <- df[, c("PID",
                      "HospEncounterID",
                      "ErDischargeDate")][df_cbc_hosp, on = c("PID",
                                                              "HospEncounterID")]
df_cbc_hosp <- df_cbc_hosp[!is.na(ErDischargeDate)]
df_cbc_hosp <-
  df_cbc_hosp[date(ErDischargeDate) != date(darr_datetime)]
df_cbc_hosp = df_cbc_hosp[test %in% cbc_names, ]
df_cbc_hosp = df_cbc_hosp[, .(count_cbc = .N), by = c("PID",
                                                      "HospEncounterID",
                                                      "test")]
setorder(df_cbc_hosp, PID, HospEncounterID,-count_cbc)
df_cbc_hosp <- unique(df_cbc_hosp, by = c("PID",
                                          "HospEncounterID"))
df_cbc_hosp <- df_cbc_hosp[, -c("test")]

df <- df_chem_hosp[df, on = c("PID",
                              "HospEncounterID")]
df <- df_cbc_hosp[df, on = c("PID",
                             "HospEncounterID")]

df[, `:=`(
  count_cbc = fifelse(is.na(count_cbc), 0, count_cbc),
  count_basic_chemistry = fifelse(is.na(count_basic_chemistry), 0, count_basic_chemistry)
)]
df[, `:=`(total_count_bld = count_cbc + count_basic_chemistry)]


# --------Table one---------------
myVar <-
  c("age_at_hosp",
    "sex_male",
    "sector_jew",
    "cci_score",
    "chapter_grp")
strata_1 <- "HospFirstUnit"
tableone_1 <-
  CreateTableOne(
    vars = myVar,
    data = df,
    strata = strata_1,
    addOverall = FALSE
  )
print(tableone_1, quote = TRUE, noSpaces = TRUE)

tableone_1 <-
  CreateTableOne(vars = myVar,
                 data = df,
                 addOverall = FALSE)
print(tableone_1, quote = TRUE, noSpaces = TRUE)

# Adjusted blood utilization rates -------------------

model_bld <-
  glm(
    total_count_bld ~ HospFirstUnit * year_hosp_date_f * mnth +
      age_at_hosp + I(age_at_hosp ^ 2) +
      sex_male + sector_jew + cci_score_grpd + chapter_grp + offset(log_los),
    data = df,
    family = "quasipoisson"
  )
tbl_bld <-
  emmeans::emmeans(
    model_bld,
    specs = "HospFirstUnit",
    by = c("year_hosp_date_f", "mnth"),
    type = "response",
    at = c(log_los = 9.21034)
  ) %>% as.data.table(.)
tbl_bld$hosp_unit_letter <- rep(LETTERS[1:7], 6 * 12)
setorder(tbl_bld, HospFirstUnit, year_hosp_date_f, mnth)
tbl_bld$time <- c(rep(1:72, 7))
tbl_bld <- tbl_bld[!is.na(rate)]

tbl_bld$season <- "spring"
tbl_bld$season[tbl_bld$mnth == 6 |
                 tbl_bld$mnth == 7 |
                 tbl_bld$mnth == 8 | tbl_bld$mnth == 9] <- "summer"
tbl_bld$season[tbl_bld$mnth == 10 | tbl_bld$mnth == 11] <- "fall"
tbl_bld$season[tbl_bld$mnth == 12 |
                 tbl_bld$mnth == 1 |
                 tbl_bld$mnth == 2 | tbl_bld$mnth == 3] <- "winter"

mdl <- lm(rate ~ time + season, data = tbl_bld)
tbl_bld$rate_n <- mdl$residuals + mdl$coefficients[1]

res.aov <-
  anova_test(
    data = tbl_bld,
    dv = rate_n,
    wid = HospFirstUnit,
    within = time
  )
get_anova_table(res.aov)


ggplot(data = tbl_bld, aes(y = rate_n, x = hosp_unit_letter)) +
  geom_boxplot(outlier.shape = NA, size = 0.8) + theme_bw() +
  scale_color_brewer(palette = "Dark2") +
  geom_jitter(
    color = "black",
    size = 1,
    alpha = 0.9,
    width = 0.25
  ) +
  ylab("Adjusted Mean Blood Utilization Rates (monthly)") + xlab("Ward") + theme(text = element_text(size = 20)) +
  stat_boxplot(geom = "errorbar", width = 0.3)

write.csv(tbl_bld, "output/adjusted_mean_bld.csv")


# --------------------------------------------------------------

model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * year_hosp_date_f * mnth +
      age_at_hosp + I(age_at_hosp ^ 2) +
      sex_male +
      sector_jew +
      cci_score_grpd +
      chapter_grp + offset(log_los),
    data = df,
    family = "quasipoisson"
  )

tbl_bld_count <- emmeans::emmeans(
  model_bld_count,
  specs = "HospFirstUnit",
  by = c("year_hosp_date_f", "mnth"),
  type = "response",
  at = c(log_los = 0)
) %>% as.data.table(.)
tbl_bld_count$hosp_unit_letter <- rep(LETTERS[1:7], 6 * 12)
setorder(tbl_bld_count, HospFirstUnit, year_hosp_date_f, mnth)
tbl_bld_count$time <- c(rep(1:72, 7))
tbl_bld_count <- tbl_bld_count[!is.na(rate)]

tbl_bld_count$season <- "spring"
tbl_bld_count$season[tbl_bld_count$mnth == 6 |
                       tbl_bld_count$mnth == 7 |
                       tbl_bld_count$mnth == 8 |
                       tbl_bld_count$mnth == 9] <- "summer"
tbl_bld_count$season[tbl_bld_count$mnth == 10 |
                       tbl_bld_count$mnth == 11] <- "fall"
tbl_bld_count$season[tbl_bld_count$mnth == 12 |
                       tbl_bld_count$mnth == 1 |
                       tbl_bld_count$mnth == 2 |
                       tbl_bld_count$mnth == 3] <- "winter"

mdl <- lm(rate ~ time + season, data = tbl_bld_count)
tbl_bld_count$rate_n <- mdl$residuals + mdl$coefficients[1]
tbl_bld_count$asymp.LCL_n <-
  tbl_bld_count$asymp.LCL - (tbl_bld_count$rate - tbl_bld_count$rate_n)
tbl_bld_count$asymp.UCL_n <-
  tbl_bld_count$asymp.UCL - (tbl_bld_count$rate - tbl_bld_count$rate_n)

mortality_or_re_hosp_model <-
  glm(
    mortality_or_re_hosp ~ HospFirstUnit * year_hosp_date_f * mnth +
      age_at_hosp + I(age_at_hosp ^ 2) +
      sex_male +
      sector_jew +
      cci_score_grpd +
      chapter_grp,
    data = df,
    family = "quasipoisson"
  )
tbl_mortality_or_re_hosp <-
  emmeans::emmeans(
    mortality_or_re_hosp_model,
    by = c("year_hosp_date_f", "mnth"),
    specs = "HospFirstUnit",
    type = "response"
  ) %>% as.data.table(.)
tbl_mortality_or_re_hosp$hosp_unit_letter <-
  rep(LETTERS[1:7], 6 * 12)
setorder(tbl_mortality_or_re_hosp,
         HospFirstUnit,
         year_hosp_date_f,
         mnth)
tbl_mortality_or_re_hosp$time <- c(rep(1:72, 7))
tbl_mortality_or_re_hosp <- tbl_mortality_or_re_hosp[!is.na(rate)]

tbl_mortality_or_re_hosp$season <- "spring"
tbl_mortality_or_re_hosp$season[tbl_mortality_or_re_hosp$mnth == 6 |
                                  tbl_mortality_or_re_hosp$mnth == 7 |
                                  tbl_mortality_or_re_hosp$mnth == 8 |
                                  tbl_mortality_or_re_hosp$mnth == 9] <- "summer"
tbl_mortality_or_re_hosp$season[tbl_mortality_or_re_hosp$mnth == 10 |
                                  tbl_mortality_or_re_hosp$mnth == 11] <- "fall"
tbl_mortality_or_re_hosp$season[tbl_mortality_or_re_hosp$mnth == 12 |
                                  tbl_mortality_or_re_hosp$mnth == 1 |
                                  tbl_mortality_or_re_hosp$mnth == 2 |
                                  tbl_mortality_or_re_hosp$mnth == 3] <- "winter"

mdl <- lm(rate ~ time + season, data = tbl_mortality_or_re_hosp)
tbl_mortality_or_re_hosp$rate_n <-
  mdl$residuals + mdl$coefficients[1]
tbl_mortality_or_re_hosp$asymp.LCL_n <-
  tbl_mortality_or_re_hosp$asymp.LCL - (tbl_mortality_or_re_hosp$rate - tbl_mortality_or_re_hosp$rate_n)
tbl_mortality_or_re_hosp$asymp.UCL_n <-
  tbl_mortality_or_re_hosp$asymp.UCL - (tbl_mortality_or_re_hosp$rate - tbl_mortality_or_re_hosp$rate_n)

spearmanCI::spearmanCI(
  tbl_bld_count$rate_n,
  tbl_mortality_or_re_hosp$rate_n,
  level = 0.9,
  method = "empirical"
)

mean_bld <- mean(tbl_bld_count$rate_n)
sd_bld <- sd(tbl_bld_count$rate_n)
mean_re_mrtl <- mean(tbl_mortality_or_re_hosp$rate_n)
sd_re_mrtl <- sd(tbl_mortality_or_re_hosp$rate_n)

plt_table <-
  data.table(
    expected_bld = (tbl_bld_count$rate_n - mean_bld) / sd_bld,
    lwr_bld = (tbl_bld_count$asymp.LCL_n - mean_bld) /
      sd_bld,
    upr_bld = (tbl_bld_count$asymp.UCL_n - mean_bld) /
      sd_bld,
    expected_com_score = (tbl_mortality_or_re_hosp$rate_n - mean_re_mrtl) /
      sd_re_mrtl,
    lwr_mortality_or_re_hosp = (tbl_mortality_or_re_hosp$asymp.LCL_n - mean_re_mrtl) /
      sd_re_mrtl,
    upr_mortality_or_re_hosp = (tbl_mortality_or_re_hosp$asymp.UCL_n - mean_re_mrtl) /
      sd_re_mrtl,
    hospfirstunit = paste0(
      tbl_bld_count$hosp_unit_letter,
      " ",
      "(",
      tbl_bld_count$year_hosp_date_f,
      ", " ,
      tbl_bld_count$mnth,
      ")"
    )
  )

setorder(plt_table, expected_bld)
plt_table$axis_x <- 1:503
plt_table_1 <-
  data.table(
    type_est = c(
      rep("Blood utilization rate", 503),
      rep("Composite outcome (mortality or readmission)", 503)
    ),
    expected = c(plt_table$expected_bld, plt_table$expected_com_score),
    lwr = c(plt_table$lwr_bld, plt_table$lwr_mortality_or_re_hosp),
    upr = c(plt_table$upr_bld, plt_table$upr_mortality_or_re_hosp),
    axis_x = c(plt_table$axis_x, plt_table$axis_x)
  )



ggplot(data = plt_table_1, aes(x = axis_x, y = expected, color = type_est)) +
  geom_point(size = 0.8) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  ylab("Means (z-score)") +
  xlab("") + theme_bw()  +
  scale_color_brewer(palette = "Dark2") +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    legend.justification = 'left',
    legend.direction = "horizontal",
    text = element_text(size = 20, family = "serif"),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  scale_y_continuous(breaks = c(round(seq(-3, 3, 1), 1)),
                     limits = c(-3.5, 3.5)) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, NA)) + geom_smooth(se = FALSE,
                                                      linetype = "dashed",
                                                      size = 1.3)
# --------------------------------------------------------------
df_no_day <-
  rbindlist(list(
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(1, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(2, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(3, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(4, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(5, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(6, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(7, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(8, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(9, 69412)),
    cbind(df[, c(
      "PID",
      "HospFirstUnit" ,
      "HospEncounterID",
      "ErDischargeDate",
      "HospDischargeDate",
      "chapter_grp",
      "season",
      "sector_jew",
      "sex_male",
      "year_hosp_date_f",
      "age_at_hosp",
      "cci_score"
    )], day = rep(10, 69412))
  ))
df_no_day <-
  df_no_day[ErDischargeDate + days(day) <= HospDischargeDate]


df_chem_hosp = df_list$df_chemistry_hosp_raw_c
df_chem_hosp <- df_chem_hosp[lubridate::year(darr_datetime) >= 2014]
df_chem_hosp = df_chem_hosp[test %in% basic_chem_names, ]
df_chem_hosp <-
  df[, c("PID", "HospEncounterID", "ErDischargeDate")][df_chem_hosp, on = c("PID", "HospEncounterID")]
df_chem_hosp <- df_chem_hosp[!is.na(ErDischargeDate)]
df_chem_hosp <-
  df_chem_hosp[date(ErDischargeDate) != date(darr_datetime)]
df_chem_hosp[, `:=`(day = time_length(date(ErDischargeDate) %--% date(darr_datetime), "days"))]
df_chem_hosp = df_chem_hosp[, .(count_basic_chemistry = .N), by = c("PID", "HospEncounterID", "test", "day")]
setorder(df_chem_hosp, PID, HospEncounterID,-count_basic_chemistry)
df_chem_hosp <-
  unique(df_chem_hosp, by = c("PID", "HospEncounterID", "day"))
df_chem_hosp <- df_chem_hosp[, -c("test")]


df_cbc_hosp = df_list$df_hematology_hosp_raw_c
df_cbc_hosp <- df_cbc_hosp[lubridate::year(darr_datetime) >= 2014]
df_cbc_hosp = df_cbc_hosp[test %in% cbc_names, ]
df_cbc_hosp <-
  df[, c("PID", "HospEncounterID", "ErDischargeDate")][df_cbc_hosp, on = c("PID", "HospEncounterID")]
df_cbc_hosp <- df_cbc_hosp[!is.na(ErDischargeDate)]
df_cbc_hosp <-
  df_cbc_hosp[date(ErDischargeDate) != date(darr_datetime)]
df_cbc_hosp[, `:=`(day = time_length(date(ErDischargeDate) %--% date(darr_datetime), "days"))]
df_cbc_hosp = df_cbc_hosp[, .(count_cbc = .N), by = c("PID", "HospEncounterID", "test", "day")]
setorder(df_cbc_hosp, PID, HospEncounterID,-count_cbc)
df_cbc_hosp <-
  unique(df_cbc_hosp, by = c("PID", "HospEncounterID", "day"))
df_cbc_hosp <- df_cbc_hosp[, -c("test")]



df_no_day <-
  df_chem_hosp[df_no_day, on = c("PID", "HospEncounterID", "day")]
df_no_day <-
  df_cbc_hosp[df_no_day, on = c("PID", "HospEncounterID", "day")]

df_no_day[, `:=`(
  count_cbc = fifelse(is.na(count_cbc), 0, count_cbc),
  count_basic_chemistry = fifelse(is.na(count_basic_chemistry), 0, count_basic_chemistry)
)]
df_no_day[, `:=`(total_count_bld = count_cbc + count_basic_chemistry)]

df_no_day[, `:=`(day = as.factor(day))]
model_bld_count <-
  glm(
    total_count_bld ~ HospFirstUnit * day + chapter_grp + season + sector_jew + sex_male +
      year_hosp_date_f +
      age_at_hosp + I(age_at_hosp ^ 2) + I(age_at_hosp ^
                                             3) +
      cci_score ,
    data = df_no_day,
    family = "quasipoisson"
  )

tbl_bld <-
  emmeans::emmeans(
    model_bld_count,
    specs = "HospFirstUnit",
    by = c("day"),
    type = "response"
  ) %>% as.data.table(.)
df_gg <-
  tbl_bld[, .(est_sd = sqrt(varp(rate)), est_mean = mean(rate)), by = c("day")]
df_gg <-
  tbl_bld[, .(est_sd = sd(rate), est_mean = mean(rate)), by = c("day")]

df_gg[, `:=`(cv_est = est_sd / est_mean)]
tbl_bld[, `:=`(day = as.numeric(as.character(day)))]

ggplot(data = tbl_bld, aes(x = day, y = rate, colour = HospFirstUnit)) + geom_point() + geom_line() +
  
  write.csv(tbl_bld, "/Users/owner/Desktop/plot.csv")

df_gg[, `:=`(cv_est = round(cv_est * 100, 1))]
write.csv(df_gg, "/Users/owner/Desktop/cv.csv")


