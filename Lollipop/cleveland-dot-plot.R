# List of installed packages
box::use(
  dplyr[tibble, mutate, arrange, desc, group_by],
  ggplot2[...],
  hrbrthemes[...],
  forcats[fct_reorder, fct_recode],
  ggtext[element_textbox_simple],
  glue[glue],
  sysfonts[font_add_google]
)

# Prepare dataset
dat <- tibble(
  hoh_gender = c("male", "female"),
  ukr_camps = c(1, 1),
  ukr = c(2 ,2),
  ner = c(20, 27),
  mmr = c(1, 2),
  mli = c(6, 11),
  lbn_plr = c(4, 9),
  lbn_lbn = c(5, 7),
  lbn_mig = c(12, 4),
  ken = c(35, 51),
  hti_pap = c(51, 62),
  hti_rest = c(50, 56),
  drc = c(36, 47),
  car = c(48, 51),
  bfa = c(8, 16),
  bgd_ref = c(4, 9),
  bgd_hosts = c(1, 3)
)

# Prepare named vector of country names
country_names <- c(
  ukr_camps = "Ukraine - Camps",
  ukr = "Ukraine",
  ner = "Niger",
  mmr = "Myanmar",
  mli = "Mali",
  lbn_plr = "Lebanon - Palestinian refugees",
  lbn_lbn = "Lebanon - Lebanese",
  lbn_mig = "Lebanon - Migrants",
  ken = "Kenya",
  hti_pap = "Haiti - Port-au-Prince",
  hti_rest = "Haiti - Rest of the country",
  drc = "DRC",
  car = "CAR",
  bfa = "Burkina Faso",
  bgd_ref = "Bangladesh - Refugees",
  bgd_hosts = "Bangladesh - Hosts"
)

# Revert names and countries
country_names <- setNames(names(country_names), country_names)

# Transpose data
dat_long <- dat |> 
  tidyr::pivot_longer(
    cols = -hoh_gender,
    names_to = "country",
    values_to = "value"
  )


# Replace country values by country names 
dat_long <- dat_long |>
  mutate(
    country = fct_recode(
      country,
      !!!country_names
    ))

# pivot_wider for gender
dat_long_pivot <- dat_long |>
  tidyr::pivot_wider(
    names_from = hoh_gender,
    values_from = value
  )

# Add female to male difference
dat_long_pivot <- dat_long_pivot |>
  mutate(
    diff = female - male
  )


# Reorder data by values for females hoh_gender using fct reorder
dat_long_pivot <- dat_long_pivot |>
  mutate(
    country = fct_reorder(
      country,
      female,
      .desc = FALSE
    )
  )

# Colors
male_col <- "#F69541"
female_col <-"#699DC6"
diff_col <- "#B3A2C7"
lg <- "#E6E6E6"
mg <- "#B3B3B3"
dg <- "#58585A"
vdg <- "#3A3A3C"
black <- "#000000"

# Fonts
font_add_google("Roboto Condensed", "rc")
font_add_google("Roboto", "roboto")

# Plot 1 
p1 <- ggplot(dat_long_pivot) + 
  geom_segment(
    aes(
      x = country,
      xend = country,
      y = female,
      yend = male
    ),
    color = lg,
    size = 3) +
  geom_point(
    data = dat_long,
    aes(
      x = country,
      y = value,
      color = hoh_gender
    ),
    size = 4) +
  # Add breaks every 10 percents
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  # Manual legend for both points, with legend title being "gender of the head of households
  scale_color_manual(
    values = c(female_col, male_col),
    labels = c("Female", "Male")
  ) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Proportion (%)",
    color = "Gender of the head of household",
    title = glue("Food security needs<br> <span style='color:{female_col};'>Female</span>-headed vs. <span style='color:{male_col};'>male</span>-headed households<br>")
  ) +
  theme_minimal(
    base_family = "roboto",
    base_size = 14
  ) +
  theme(
    # Overall text
    text = element_text(
      family = "roboto",
      colour = vdg
    ),
    # Title and subtitle
    plot.title = element_textbox_simple(size = 20),
    # Title positioned to plot
    plot.title.position = "plot",
    # Have a thiner line of grid
    panel.grid.minor = element_line(linewidth = 0.25),
    # Add legend at the bottom
    legend.position = "none",
    # Text of axis in black
    axis.text = element_text(size = 14, color = vdg),
    # Legend title siwe
    legend.title = element_text(size = 14, color = vdg),
    # x axis title siwe
    axis.title = element_text(size = 14, colour = vdg)
  )

# Export plot
ggsave("Lollipop/cleveland-dot-plot.svg", p1, width = 8, height = 6)




# Reorder data by values for females hoh_gender using fct reorder
dat_long_pivot <- dat_long_pivot |>
  mutate(
    country = fct_reorder(
      country,
      diff,
      .desc = FALSE
    )
  )

# Plot 2: lollipop plot showing the % difference, ref point = 0
p2 <- ggplot(dat_long_pivot) +
  geom_segment(
    aes(
      x = country,
      xend = country,
      y = diff,
      yend = 0
    ),
    color = lg,
    size = 3) +
  geom_point(
    aes(
      x = country,
      y = diff,
      size = 4
    ),
    color = diff_col) +
  scale_y_continuous(breaks = seq(0, 100, 5)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Difference in % points",
    color = "Gender of the head of household",
    title = glue("Food security needs<br> Difference between female and male-headed households<br>")
  ) +
  theme_minimal(
    base_family = "roboto",
    base_size = 14
  ) +
  theme(
    # Overall text
    text = element_text(
      family = "roboto",
      colour = vdg
    ),
    # Title and subtitle
    plot.title = element_textbox_simple(size = 20),
    # Title positioned to plot
    plot.title.position = "plot",
    # Have a thiner line of grid
    panel.grid.minor = element_line(linewidth = 0.25),
    # Add legend at the bottom
    legend.position = "none",
    # Text of axis in black
    axis.text = element_text(size = 14, color = vdg),
    # Legend title siwe
    legend.title = element_text(size = 14, color = vdg),
    # x axis title siwe
    axis.title = element_text(size = 14, colour = vdg)
  )

p2

# Export plot
ggsave("Lollipop/cleveland-dot-plot-diff.svg", p2, width = 8, height = 6)

