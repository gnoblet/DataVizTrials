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
  ukr = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  ner = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  mmr = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  mli = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  lbn = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  ken = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  hti = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  drc = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  car = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  bfa = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  bgd_ref = sample(seq(0, 100,by = 1), 2, replace = TRUE),
  bgd_hosts = sample(seq(0, 100,by = 1), 2, replace = TRUE),
)

# Prepare named vector of country names
country_names <- c(
  ukr = "Ukraine",
  ner = "Niger",
  mmr = "Myanmar",
  mli = "Mali",
  lbn = "Lebanon",
  ken = "Kenya",
  hti = "Haiti",
  drc = "DRC",
  car = "CAR",
  bfa = "Burkina Faso",
  bgd_ref = "Bangladesh"
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
    linewidth = 3) +
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
    linewidth = 3) +
  # line dark grey on 0
  geom_hline(yintercept = 0, color = dg, size = 0.5) +
  geom_point(
    aes(
      x = country,
      y = diff,
      size = 4
    ),
    color = diff_col) +
  scale_y_continuous(breaks = seq(-100, 100, 5)) +
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

