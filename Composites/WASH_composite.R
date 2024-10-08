# install.packages("pak")
# pak::pak("gnoblet/impactR.utils")
library(impactR.utils)

# Loading humind loads functions
# pak::pak("impact-initiatives-hppu/humind")
library(humind)

# Loading example data
# pak::pak("impact-initiatives-hppu/humind.data")
library(humind.data)
data(dummy_raw_data, package = "humind.data")

# Needed tidyverse packages
library(dplyr)
library(stringr)
library(tidyr)
library(forcats)


# Prepare datasets --------------------------------------------------------

loop <- left_joints_dup(list(
  loop = dummy_raw_data$roster,
  edu_ind = dummy_raw_data$edu_ind,
  health_ind = dummy_raw_data$health_ind,
  nut_ind = dummy_raw_data$nut_ind),
  person_id,
  uuid)

main <- dummy_raw_data$main


# Add indicators ----------------------------------------------------------

#------ Add to loop
loop <- loop |>
  # Demographics
  add_age_cat("ind_age", breaks = c(-1, 17, 59, 120)) |>
  add_age_18_cat("ind_age") |>
  # Education
  add_loop_edu_ind_age_corrected(main = main, month = 7) |>
  add_loop_edu_access_d() |>
  add_loop_edu_barrier_protection_d() |>
  add_loop_edu_disrupted_d() |>
  # WGQ-SS
  add_loop_wgq_ss() |>
  # Health --- example if wgq_dis_3 exists
  add_loop_healthcare_needed_cat(wgq_dis = "wgq_dis_3")

#------ Add loop to main
main <- main |>
  # Education
  add_loop_edu_ind_schooling_age_d_to_main(loop = loop) |>
  add_loop_edu_access_d_to_main(loop = loop) |>
  add_loop_edu_barrier_protection_d_to_main(loop = loop) |>
  add_loop_edu_disrupted_d_to_main(loop = loop) |>
  # WGQ-SS
  add_loop_wgq_ss_to_main(loop = loop) |>
  # Health
  add_loop_healthcare_needed_cat_to_main(
    loop =loop,
    ind_healthcare_needed_no_wgq_dis = "health_ind_healthcare_needed_no_wgq_dis",
    ind_healthcare_needed_yes_unmet_wgq_dis = "health_ind_healthcare_needed_yes_unmet_wgq_dis",
    ind_healthcare_needed_yes_met_wgq_dis = "health_ind_healthcare_needed_yes_met_wgq_dis")

#------- Clean up food security indicators

# HHS calculation should not contain Don't know or Prefer not to answer
main <- mutate(
  main,
  across(
    c("fsl_hhs_nofoodhh", "fsl_hhs_sleephungry", "fsl_hhs_alldaynight"),
    \(x) case_when(
      x %in% c("dnk", "pnta") ~ NA_character_,
      .default = x
    )
  )
)

# Add to main
main <- main |>
  # Demographics
  add_hoh_final() |>
  add_age_cat(age_col = "resp_age", breaks = c(-1, 17, 59, 120)) |>
  add_age_18_cat(age_col = "resp_age") |>
  add_age_cat(age_col = "hoh_age", breaks = c(-1, 17, 59, 120)) |>
  add_age_18_cat(age_col = "hoh_age") |>
  # Protection
  add_child_sep_cat() |>
  # WASH
  # WASH - Sanitation facility
  add_sanitation_facility_cat() |>
  add_sharing_sanitation_facility_cat() |>
  add_sharing_sanitation_facility_n_ind() |>
  add_sanitation_facility_jmp_cat() |>
  # WASH - Water
  add_drinking_water_source_cat() |>
  add_drinking_water_time_cat() |>
  add_drinking_water_time_threshold_cat() |>
  add_drinking_water_quality_jmp_cat() |>
  # WASH - Hygiene
  add_handwashing_facility_cat() |>
  # SNFI
  add_shelter_type_cat() |>
  add_shelter_issue_cat() |>
  add_fds_cannot_cat() |>
  # HLP
  add_occupancy_cat() |>
  # Food security
  add_lcsi() |>
  add_hhs() |>
  add_fcs(cutoffs = "normal") |>
  add_rcsi() |>
  add_fcm_phase() |>
  add_fclcm_phase() |>
  # Cash & markets
  add_income_source_zero_to_sl() |>
  add_income_source_prop() |>
  add_income_source_rank() |>
  # Expenditure 
  add_expenditure_type_zero_freq(
    # Note that in the initial kobo template the utilities column was:
    # cm_expenditure_frequent_utilitues
    # Spelling mistake
    expenditure_freq_types = c("cm_expenditure_frequent_food",
      "cm_expenditure_frequent_rent",
      "cm_expenditure_frequent_water",
      "cm_expenditure_frequent_nfi",
      "cm_expenditure_frequent_utilities",
      "cm_expenditure_frequent_fuel",
      "cm_expenditure_frequent_transportation",
      "cm_expenditure_frequent_communication",
      "cm_expenditure_frequent_other")
  ) |> 
  add_expenditure_type_prop_freq(
    # Note that in the initial kobo template the utilities column was:
    # cm_expenditure_frequent_utilitues
    # Spelling mistake
    "cm_expenditure_frequent_food",
    "cm_expenditure_frequent_rent",
    "cm_expenditure_frequent_water",
    "cm_expenditure_frequent_nfi",
    "cm_expenditure_frequent_utilities",
    "cm_expenditure_frequent_fuel",
    "cm_expenditure_frequent_transportation",
    "cm_expenditure_frequent_communication",
    "cm_expenditure_frequent_other"
  ) |> 
  add_expenditure_type_freq_rank(
    # Note that in the initial kobo template the utilities column was:
    # cm_expenditure_frequent_utilitues
    # Spelling mistake
    expenditure_freq_types = c(
    "cm_expenditure_frequent_food",
      "cm_expenditure_frequent_rent",
      "cm_expenditure_frequent_water",
      "cm_expenditure_frequent_nfi",
      "cm_expenditure_frequent_utilities",
      "cm_expenditure_frequent_fuel",
      "cm_expenditure_frequent_transportation",
      "cm_expenditure_frequent_communication",
      "cm_expenditure_frequent_other")
  ) |> 
  add_expenditure_type_zero_infreq() |> 
  add_expenditure_type_prop_infreq() |> 
  add_expenditure_type_infreq_rank() |>
  # AAP
  add_received_assistance() |>
  add_access_to_phone_best() |>
  add_access_to_phone_coverage()


# Add composites ---------------------------------------------------------

# Add sectoral composites
main <- main |>
  add_comp_foodsec() |>
  add_comp_snfi() |>
  add_comp_prot() |>
  add_comp_health() |>
  add_comp_wash() |>
  add_comp_edu()

# Add MSNI score and the 4 metrics
main <- main |>
  add_msni()


# Analysis data ----------------------------------------------------------


# Libraries
library(srvyr)
# pak::pak("impact-initiatives-hppu/humind")
library(impactR.analysis)
# pak::pak("gnoblet/impactR.kobo")
library(impactR.kobo)

# Loads other needed data -- explicit call
data(loa, package = "humind.data")
data(survey_updated, package = "humind.data")
data(choices_updated, package = "humind.data")

# Analysis groups ---------------------------------------------------------

# List of grouping variables
group_vars <- list("admin1", "hoh_gender", "hoh_age_cat")
# Here you can add in group_vars the variable you want to disaggregate by
# Following your data disagregation plan, see MSNI guidance for more information

# Add this list of variables to loop (including weights, and stratum if relevant), joining by uuid
# and removing columns existing in both
loop <- df_diff(loop, main, uuid) |>
  left_join(
    main |> select(uuid, weight, !!!unlist(group_vars)),
    by = "uuid"
  )


# Prepare design and kobo -------------------------------------------------

# Design main - weighted
design_main <- main |>
  as_survey_design(weight = weight)

# Survey - one column must be named label
# and the type column must be split into type and list_name
survey <- survey_updated |>
  split_survey(type) |>
  rename(label = label_english)

# Choices - one column must be named label
choices <- choices_updated |>
  rename(label = label_english)

# Loa for main only
loa <- loa |>
  filter(
    dataset == "main",
    str_detect(var, "wash")
  )


# Run analysis ------------------------------------------------------------

# Main analysis - weighted
if (nrow(loa) > 0) {
  an_main <- impactR.analysis::kobo_analysis_from_dap_group(
    design_main,
    loa,
    survey,
    choices,
    l_group = group_vars,
    choices_sep = "/")
} else {
  an_main <- tibble()
}






# Plot --------------------------------------------------------------------

# Libraries
library(ggpubr)
library(viridis)
library(showtext)

# Prepare data
an_main_nogroup <- an_main |>
  filter(
    is.na(group_key),
    var %in% c("comp_wash_score")) |> 
  mutate(
    stat = round(stat * 100, 1),
    var = fct_relevel(var, c("comp_wash_score"))
  )

# Colors
# Let's use magma, which change
# one_to_five_col <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#756bb1", "#54278f")
# one_to_five_lev <- c("5", "4", "3", "2", "1")
dark_grey <- "#58585A"

# Fonts
font_add_google("Roboto Condensed", "Roboto Condensed")
font_add("segoeui", "segoeui.ttf")

# Y positions
pos.acuteneed <- max(an_main_nogroup$stat) + 5
pos.need <- pos.acuteneed + 5
width <- 0.6
xmin.inneed <- 3 - 0.5 * width
xmax.inneed <- 5 + 0.5 * width
xmin.inacuteneed <- 4 - 0.5 * width
xmax.inacuteneed <- 5 + 0.5 * width


# Plot
p1 <- ggplot(an_main_nogroup) +
  geom_col(
    aes(
      x = var_value,
      y = stat,
      fill = var_value),
    position = "dodge",
    width = width) +
  geom_bracket(
      xmin = xmin.inneed,
      xmax = xmax.inneed,
      y.position = pos.need,
      label = "In need",
      label.size = 5,
      size = 0.2,
      family = "segoeui",
      colour = dark_grey
  ) +
  geom_bracket(
      xmin = xmin.inacuteneed,
      xmax = xmax.inacuteneed,
      y.position = pos.acuteneed,
      label = "In acute need",
      label.size = 5,
      family = "segoeui",
      colour = dark_grey,
  ) +
  geom_text(
    aes(
      x = var_value,
      y = stat,
      label = paste0(stat, "%")),
    size = 5,
    position = position_dodge(width = 1),
    family = "segoeui",
    colour = dark_grey,
    vjust = - 0.5) +
  # coord_flip() +
  labs(
    fill = "Levels",
    y = "",
    x = "Score",
    title = "WASH composite score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    text = element_text(family = "segoeui", colour = dark_grey),
    title = element_text(size = 18, family = "Roboto Condensed"),
  ) +
  scale_fill_viridis_d(
    option = "plasma",
    begin = 0.1,
    end = 1,
    direction = -1
  ) +
  scale_y_continuous(
    expand = c(0, 0.5),
    limits = c(0, pos.need + 5)
  ) 

# Savee
ggsave("Composites/WASH_composite.svg", p1, width = 6, height = 6, dpi = 300)












