# Function for creating faceted pie charts.

facetpie <- function(df, facetvar, fillvar, pie_title, pie_subtitle, pie_fill) {
  tempdf <- df %>%
    group_by(!!sym(facetvar), !!sym(fillvar)) %>%
    summarize(num_grp = n()) %>%
    ungroup() %>%
    group_by(!!sym(facetvar)) %>%
    mutate(num_facet = sum(num_grp)) %>%
    ungroup() %>%
    mutate(frac_grp = num_grp / num_facet,
           facetvar = fct_reorder(!!sym(facetvar), num_facet))

  tempdf %>%
    ggplot(aes(x = factor(1), y = frac_grp ,fill = !!sym(fillvar))) +
    geom_bar(width = 1, stat = "identity") + coord_polar(theta = "y", start = 0) +
    geom_text(aes(x = 1, label = glue::glue("{num_grp} \n ({percent(frac_grp, 2)})"),
                  fontface = "bold"),
              position = position_stack(vjust = 0.5), size = 3) +
    labs(
      title = pie_title,
      subtitle = pie_subtitle,
      fill = pie_fill,
      y = NULL, x = NULL
    ) +
    theme(
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_blank()
    ) +
    scale_fill_iu() +
    facet_wrap(formula(paste("~", facetvar)),
               labeller = label_wrap_gen(width = 16, multi_line = TRUE))
}
