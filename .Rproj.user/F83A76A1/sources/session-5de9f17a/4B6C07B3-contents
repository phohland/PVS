# Packages ----------------------------------------------------------------
pacman::p_load(ggplot2, readxl, devtools, tidyverse,janitor, dplyr, tools)

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

# whole hRELSA function

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
  plot_instead_of_scree = FALSE,
  
  #plot
  which_patient = 43,
  which_var_to_plot = c("sbp_sds")
  
)

plot <- hrelsa_plot(hRELSA$plot$dat, hRELSA$plot$hRELSA, levels = hRELSA$plot$levels, a = hRELSA$plot$a, plotvar = hRELSA$plot$plotvar, plothRELSA = TRUE,
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