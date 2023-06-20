plot_spread <- function(data, x_variable) {
    data <- data %>%
        mutate({{x_variable}} := forcats::fct_infreq({{x_variable}}))

    ggplot(data) +
        geom_bar(aes(x = {{x_variable}}), fill = palette2, alpha = 0.7) +
        coord_flip() +
        labs(x = "Count", y = as_label(rlang::enquo(x_variable)),
             title = paste0(as_label(rlang::enquo(x_variable)), " spread among employees"),
             caption = "Data from Kaggle") +
        theme_classic()
}