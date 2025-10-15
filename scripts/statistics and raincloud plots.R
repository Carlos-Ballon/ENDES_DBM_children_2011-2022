# Summary statistics and normality tests
group_stat_table_plot <- function(data_used, var, outcome) {
  # Check if "var" and "outcome" are strings or variable names
  if (is.character(var)) {
    var <- as.name(var)
  }
  if (is.character(outcome)) {
    outcome <- as.name(outcome)
  }
  
  loadfonts(device = "win")
  loadfonts(device = "pdf")
  loadfonts(device = "postscript")
  
  # Levene’s test
  # levene_result <- car::leveneTest(formula(paste(var, "~", outcome)), data = data_used)
  
  # Extract p-value
  # levene_p_value <- levene_result$`Pr(>F)`[1]
  
  # Bartlett’s test
  # barlett_result <- stats::bartlett.test(formula(paste(var, "~", outcome)), data = data_used)
  
  # Extract p-value
  # barlett_p_value <- barlett_result$p.value
  
  add_sample <- function(x) {
    return(c(y = max(x) + .025, label = length(x)))
  }
  
  stat_table <- data_used |>
    dplyr::group_by({{ outcome }}) |>
    dplyr::summarise(
      Mean = round(mean({{ var }}, na.rm = TRUE), 2),
      SD = round(sd({{ var }}, na.rm = TRUE), 2),
      Median = round(median({{ var }}, na.rm = TRUE), 2),
      Min = round(min({{ var }}, na.rm = TRUE), 2),
      IQR = paste(round(quantile({{ var }}, probs = c(0.25, 0.75), na.rm = TRUE), 2), collapse = "-"),
      Max = round(max({{ var }}, na.rm = TRUE), 2),
      # Shapiro_Wilk = round(stats::shapiro.test({{ var }})$p.value, 3)
      # Anderson_Darling = round(nortest::ad.test({{ var }})$p.value, 3),
      Kurtosis = round(moments::kurtosis({{ var }}, na.rm = TRUE), 2),
      Skewness = round(moments::skewness({{ var }}, na.rm = TRUE), 2)
    ) |>
    dplyr::rename(
      # "Anderson-Darling" = Anderson_Darling,
      "IQR*" = IQR,
      "Group" = {{ outcome }}
    ) |>
    
    # Draw a textual table
    ggpubr::ggtexttable(
      rows = NULL,
      theme = ttheme(
        base_style = "blank",
        colnames.style = colnames_style(
          size = 11,
          color = "#272822",
          fill = "white",
          linecolor = "#f0f0f0"
        ),
        tbody.style = tbody_style(
          size = 11,
          color = "#272822",
          fill = "white",
          linecolor = "#f0f0f0",
          
        )
      )
    )
  
  foot_note <- paste0(
    "IQR* = Interquartile range",
    # "Levene's test p-value = ",
    # round(levene_p_value, 3),
    # "; ",
    # "Bartlett’s test p-value = ",
    # round(barlett_p_value, 3),
    "\n",
    # "Normal distribution if: p-values of hypothesis tests are > 0.05, ",
    "Skewness between -0.5 to 0.5, and Kurtosis between 2.5 to 3.5"
  ) |>
    paste(collapse = "\n")
  
  # Customization
  custom_stat_table <- stat_table |>
    ggpubr::tab_add_hline(
      at.row = 1:2, 
      row.side = "top", 
      linewidth = 2) |> 
    ggpubr::tab_add_hline(
      at.row = 2:tab_nrow(stat_table),
      row.side = "bottom",
      linetype = 1
    ) |>
    ggpubr::table_cell_font(
      row = 2:tab_nrow(stat_table),
      column = 2,
      size = 11,
      color = "#272822"
    ) |>
    ggpubr::table_cell_bg(
      row = 2:tab_nrow(stat_table),
      column = 2,
      fill = "grey89",
      color = "#f0f0f0"
    ) |>
    ggpubr::table_cell_bg(
      row = 2:tab_nrow(stat_table),
      column = 4,
      fill = "grey89",
      color = "#f0f0f0"
    ) |>
    ggpubr::table_cell_font(
      row = 2:3,
      column = 1,
      size = 11,
      color = "#272822"
    ) |>
    ggpubr::table_cell_bg(
      row = 2,
      column = 1,
      fill = lighten("#3d6721", 0.5),
      color = "#f0f0f0",
      alpha = 0.5
    ) |>
    ggpubr::table_cell_bg(
      row = 3,
      column = 1,
      fill = lighten("#a86826", 0.5),
      color = "#f0f0f0",
      alpha = 0.5
    ) |> 
    ggpubr::tab_add_title(
      text = "Summary statistics and normality tests",
      padding = unit(1.5, "line"),
      size = 14,
      family = "Zilla Slab",
      hjust = -0.4
    ) |>
    ggpubr::tab_add_footnote(
      text = foot_note,
      padding = unit(0.5, "line"),
      size = 9,
      family = "Fira Mono"
    )
  
  # Raincloud plots by groups
  plot <- ggplot2::ggplot(
    data = data_used, 
    aes_string(y = var, x = outcome)) +
    ggdist::stat_halfeye(
      aes(
        color = {{ outcome }}, 
        fill = after_scale(colorspace::lighten(color, 0.5))),
      adjust = 0.5,
      width = 1,
      .width = 0,
      scale = 0.7,
      justification = -0.2,
      point_colour = NA,
      position = position_dodge()
    ) +
    ggplot2::geom_boxplot(
      aes(
        color = stage(
          {{ outcome }}, 
          after_scale = darken(color, 0.1, space = "HLS")),
        fill = after_scale(desaturate(lighten(color, 0.8), 0.4))),
      width = 0.12,
      outlier.color = NA
    ) +
    gghalves::geom_half_point(
      aes(
        color = stage(
          {{ outcome }}, 
          after_scale = colorspace::darken(color, 0.1, space = "HLS"))),
      side = "l",
      shape = 1,
      size = 0.5,
      alpha = 0.1,
      range_scale = 0.4,
      position = position_jitter(seed = 1, width = 0)
    ) +
    ggplot2::stat_summary(
      geom = "text",
      fun = "median",
      aes(
        label = round(after_stat(y), 2),
        color = stage(
          {{ outcome }},
          after_scale = darken(color, .1, space = "HLS"))),
      family = "Lato",
      fontface = "bold",
      color = "white",
      size = 4,
      vjust = -3.5
    ) +
    ggplot2::stat_summary(
      geom = "text",
      fun.data = add_sample,
      aes(
        label = paste("n =", after_stat(label)),
        color = stage(
          {{ outcome }}, 
          after_scale = darken(color, .1, space = "HLS"))),
      family = "Lato",
      fontface = "bold",
      size = 3.5,
      position = position_nudge(x = 0.05, y = -0.3)
    ) +
    ggplot2::scale_y_continuous(breaks = scales::breaks_pretty(n = 6)) +
    ggplot2::scale_color_manual(values = c("#a86826", "#3d6721")) +
    ggplot2::scale_fill_manual(values = c("#a86826", "#3d6721")) +
    ggplot2::theme_bw(base_family = "Zilla Slab", base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      axis.line = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey92", linetype = "solid", size = 0.4),
      panel.grid.minor = element_blank(),
      axis.ticks = element_line(color = "grey92", linewidth = 0.4), 
      axis.ticks.length = unit(0.5, "lines"),
      axis.title.y = element_text(
        color = "#272822",
        size = 14),
      axis.title.x = element_text(
        color = "#272822",
        size = 14),
      axis.text.x = element_text(
        family = "Fira Mono", 
        color = "#3D3E38",
        margin = margin(b = 10)), 
      axis.text.y = element_text(
        color = darken(c("#a86826", "#3d6721"), 0.1, space = "HLS"), 
        size = 14,
        angle = 90,
        margin = margin(l = 20),
        hjust = -0.1),
      plot.title = element_text(color = "#272822", size = 18, face = "bold"),
      plot.subtitle = element_text(
        size = 12, 
        color = "grey30", 
        margin = margin(0.5, 1, -10, 1, unit = "cm")),
      plot.caption = element_text(size = 9, margin = margin(t = 15))) +
    ggplot2::coord_flip(xlim = c(1.2, NA), clip = "off")
  
  ggpubr::ggarrange(
    plot,
    custom_stat_table,
    ncol = 1,
    heights = c(3, 1))
}

# Use "var" and "outcome" to perform calculations and create the table
# group_stat_table_plot(data, "edad", "a_f")



