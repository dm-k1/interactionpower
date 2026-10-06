#' Simulate one dataset with an interaction effect
#'
#' @param n Sample size.
#' @param b_x Standardized effect of x.
#' @param b_z Standardized effect of z.
#' @param b_interaction Standardized effect of the x by z interaction.
#' @return A dataframe with the columns x, z and y.
#' @export
simulate_interaction_data = function(n, b_x, b_z, b_interaction) {
  x = rnorm(n)
  z = rnorm(n)
  noise = rnorm(n, sd = sqrt(1 - b_x^2 - b_z^2 - b_interaction^2))
  y = b_x * x + b_z * z + b_interaction * x * z + noise
  return(data.frame(x, z, y))
}

#' Power of x, z and their interaction at one sample size
#'
#' @inheritParams simulate_interaction_data
#' @param n_simulations Number of simulated datasets.
#' @return Three powers, in the order x, z, interaction.
#' @export
power_interaction = function(n, b_x, b_z, b_interaction, n_simulations = 1000) {
  times_significant = c(0, 0, 0)
  for (i in 1:n_simulations) {
    data = simulate_interaction_data(n, b_x, b_z, b_interaction)
    model = summary(lm(y ~ x * z, data = data))
    p_values = model$coefficients[2:4, 4]
    times_significant = times_significant + (p_values < 0.05)
  }
  return(times_significant / n_simulations)
}

#' Raise the sample size until all three effects reach the desired power
#'
#' @inheritParams power_interaction
#' @param desired_power Power that all three effects should reach.
#' @param max_n Largest sample size to try.
#' @return A dataframe with one row per sample size tried and the power of
#'   each effect.
#' @export
find_sample_size = function(b_x, b_z, b_interaction, desired_power = 0.80,
                            n_simulations = 1000, max_n = 1000) {
  results = data.frame()

  n = 20
  power = c(0, 0, 0)
  while (any(power < desired_power) && n <= max_n) {
    power = as.numeric(power_interaction(n, b_x, b_z, b_interaction, n_simulations))
    results = rbind(
      results,
      data.frame(
        sample_size = n,
        power_x = power[1],
        power_z = power[2],
        power_interaction = power[3]
      )
    )
    n = n + 20
  }

  return(results)
}

#' Plot the results of find_sample_size() as one line per effect
#'
#' @param results The dataframe returned by find_sample_size().
#' @param desired_power Power level to mark with a dashed line.
#' @return A ggplot.
#' @export
plot_power_curve = function(results, desired_power = 0.80) {
  ggplot2::ggplot(results, ggplot2::aes(x = sample_size)) +
    ggplot2::geom_line(ggplot2::aes(y = power_x, colour = "X")) +
    ggplot2::geom_line(ggplot2::aes(y = power_z, colour = "Z")) +
    ggplot2::geom_line(ggplot2::aes(y = power_interaction, colour = "X x Z")) +
    ggplot2::geom_hline(yintercept = desired_power, linetype = "dashed") +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "Sample size", y = "Power", colour = "Effect") +
    ggplot2::theme_bw()
}
