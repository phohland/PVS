# Packages ----------------------------------------------------------------
pacman::p_load(styler, ggplot2,readxl,devtools,tidyverse,janitor,dplyr,lme4, magrittr,
               broom, conflicted, see, performance, car, parameters, performance, tools)

conflict_prefer("lmer", "lme4")
conflict_prefer("filter", "dplyr")

library(RELSA)

# Sourcing ----------------------------------------------------------------
source("R/hrelsa_analysis.R")
source("R/hrelsa_baselines.R")
source("R/hrelsa_days.R")
source("R/hrelsa_final.R")
source("R/hrelsa_format.R")
source("R/hrelsa_levels.R")
source("R/hrelsa_norm.R")
source("R/hrelsa_plot.R")
source("R/hrelsa.R")

# Wrapper -----------------------------------------------------------------

hRELSA <- relsa_wrapper(
    # input file
  data_path = "data/txdata.csv",
  data_seperation = ",",
  data_decimal = ".",
  data_sheet = NULL,
  
  # data formation
  id = "id",
  treatment = "tx",
  condition = "sex",
  day = NULL,
  form_to_day = "exam_date",
  day_format = "%d%b%Y",
  new_day_one = TRUE,
  vars = c("sds_w", "sds_h", "sds_bmi", "sds_waist", "sds_wh_r", "sbp_sds", "dbp_sds"),
  turnvars = NULL,
  zvars = c("sds_w", "sds_h", "sds_bmi", "sds_waist", "sds_wh_r", "sbp_sds", "dbp_sds"),
  dropvars = NULL,
  
  # normalization
  norm_ontime = 1,
  baseline_day = 1,
  
  # levels
  levels = TRUE,
  k = 4,
  plot_instead_of_scree = FALSE
  
)

# Data Setup ----------------------------------------------------------

rawli <- read.csv("data/litx.csv", sep = ";", header = TRUE, dec = ".")
rawli <- as_tibble(rawli)
rawli$tx <- "liver"
rawli <- rawli %>%
  clean_names %>%
  select("id","tx","sex", "exam_date",
        "sds_w", "sds_h", "sds_bmi", "sds_waist", "sds_wh_r", "sbp_sds", "dbp_sds")

rawlu <- read.csv("data/lutx.csv", sep = ";", header = TRUE, dec = ".")
rawlu <- as_tibble(rawlu)
rawlu$tx <- "lungs"
rawlu <- rawlu %>%
  clean_names %>%
  select("id","tx","sex", "exam_date",
         "sds_w", "sds_h", "sds_bmi", "sds_waist", "sds_wh_r", "sbp_sds", "dbp_sds")

raw <- rbind(rawli, rawlu)

raw <- raw %>%
  mutate(
    id = as.factor(id),
    sex = factor(sex, levels = c("1", "2"),
                 labels = c("male", "female")),
    exam_date = as.Date(exam_date,"%d%b%Y"),
    tx = as.factor(tx)
  )


# hrelsa_days function to add day column
raw <- hrelsa_days(raw, format = "day", formthis = "exam_date")

# hRELSA -------------------------------------------------------------------

vars <- c("sds_w", "sds_h", "sds_bmi", "sds_waist", "sds_wh_r", "sbp_sds", "dbp_sds")
turnvars <- NULL
zvars <- c("sds_w", "sds_h", "sds_bmi", "sds_waist", "sds_wh_r", "sbp_sds", "dbp_sds")
dropvars <- NULL

dat <- hrelsa_format(raw, id = "id", treatment = "tx", condition = "sex", day = "day", vars = vars)

pre <- hrelsa_norm(dat, normthese = vars, zvars = zvars, ontime = 1)
bsl <- hrelsa_baselines(pre, bslday = 1, vars = vars, zvars = zvars, turnvars = turnvars)
final <- hrelsa_final(pre, bsl, drop = dropvars, turnvars = turnvars, zvars = zvars)
analysis <- hrelsa_analysis(final)

levels <- hrelsa_levels(pre, bsl = bsl, drops = dropvars, turns = turnvars,
                        zvars = zvars,  k = 4, showScree = FALSE, showPlot = TRUE)

which_patient <- 43 # 1 to 185 # maxsev has 46 # maxsev per variable have 160, 167, 43, 173, 105, 46, 52
which_plotvar <- c("sbp_sds")
hRELSA <-   hrelsa(pre = pre, bsl, a = which_patient, drop = dropvars, turnvars = turnvars, zvars = zvars)
plot <- hrelsa_plot(dat, hRELSA, levels = levels, a = which_patient, plotvar = which_plotvar, plotRELSA = TRUE,
                        myylim = c(-4, 4),  myYlim = c(0, 1), mypch = 1, mycol = "red", myXlab="days")

# Plotting ----------------------------------------------------------------

# max hRELSA plotted over years and seperated in tx
set.seed(123)
final$day <- as.factor(final$day)

p3 <- final %>%
  group_by(id, treatment, condition, day) %>%
  summarise(max = max(as.numeric(unlist(rms)), na.rm = TRUE)) %>%
  ggplot(aes(x = day, y = max, color = condition)) +
  geom_hline(yintercept=1, linetype="dashed") +
  geom_boxplot() +
  facet_grid(.~treatment) +
  labs (x = "years", y = expression('hRELSA'[max]), title = "maximum hRELSA scores over years",
        subtitle = "separated in transplantation kind", color = "sex") +
  xlim("1","2","3","4","5") +
  theme_classic() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("steelblue","darkred"))
p3

# max hRELSA plotted over sex and seperated in tx
p4 <- final %>%
  group_by(id, treatment, condition) %>%
  summarise(max = max(as.numeric(unlist(rms)), na.rm = TRUE)) %>%
  ggplot(aes(x = condition, y = max, color = condition)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitterdodge(), size = 1.5) +
  geom_hline(yintercept=1, linetype="dashed") +
  facet_grid(.~treatment) +
  labs (x = "sex", y = expression('hRELSA'[max]),
        title = "maximum hRELSA score per patient",
        subtitle = "separated in transplantation kind") +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("steelblue","darkred"))
p4