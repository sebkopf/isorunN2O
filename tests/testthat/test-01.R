context("Corrections")

test_that("Testing 17O correction", {

  # absolute ratio values of the international standards
  VSMOW_18R <- 0.0020052 # [Baertschi, 1976]
  VSMOW_17R <- 0.0003799 # [Li et al., 1988]
  N2_15R <- 0.0036782 # [De Bievre et al., 1996]

  # mass dependent scaling constant (lambda)
  MD_SCALING_17O <- 0.52 # [Werner and Brandt, 2001]

  # ratios of the reference
  REF_45R <- isorunN2O:::calculate_45R (N2_15R, VSMOW_17R)
  REF_46R <- isorunN2O:::calculate_46R (N2_15R, VSMOW_17R, VSMOW_18R )

  # generate testing data set and convert from d18 and d15 values to d45 and d46
  data <-
    expand.grid(d18 = seq(-30, 30, by = 10), d15 = seq(-30, 30, by = 10)) %>%
    mutate(
      d17 = isorunN2O:::md_scale_delta(d18, MD_SCALING_17O),
      R15 = isorunN2O:::delta_to_ratio(d15, N2_15R),
      R18 = isorunN2O:::delta_to_ratio(d18, VSMOW_18R),
      R17 = isorunN2O:::delta_to_ratio(d17, VSMOW_17R),
      R45 = isorunN2O:::calculate_45R (R15, R17),
      R46 = isorunN2O:::calculate_46R (R15, R17, R18),
      d45 = isorunN2O:::ratio_to_delta(R45, REF_45R),
      d46 = isorunN2O:::ratio_to_delta(R46, REF_46R)
    )

  my_data <- data %>%
    correct_N2O_for_17O(d45, d46, ref_17R = VSMOW_17R, ref_18R = VSMOW_18R,
                        ref_15R_avg = N2_15R, lambda = MD_SCALING_17O) %>%
    mutate(d.diff = d18 - d18.raw)

  # make sure that any error from the conversation and back is tiny
  expect_true(all(my_data$d.diff < 1e-10))

  # if output is desired
#   head(my_data)
#   my_data %>%
#     gather(key, value, -(d18:d46))  %>%
#     filter(key %in% c("d.diff", "d.diff_from_R")) %>%
#     ggplot() + aes(d18, value, color=factor(d15)) +
#     geom_line() + theme_bw() + labs(y = "error [permil]") +
#     facet_grid(key~., scales = "free_y")
})
