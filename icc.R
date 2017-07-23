#' @title
#' item characteristic curve
#' 
#' @descriptions
#' plots the item characteristic curve for a given item
#' 
#' @param   item          item indicator (an integer)
#' @param   data          posterior samples from an IRT model
#' @param   range         x axis limits
#' @param   random_draws  number of random parameter draws from the posterior    
#'
#' @export
#'
icc <- function(item, data, range = 5, random_draws = 0) {
  d <- data.table("x" = seq(-range, range, length.out = 200))
  d[, "y" := icf(alpha = mean(data$alpha[,item]), beta = mean(data$beta[,item]), x)]

  if (random_draws > 0) { 
    s <- sample(1:nrow(data$beta), random_draws, replace = FALSE)

    for (i in 1:random_draws) {
      d[, paste0("v", i) := icf(
        alpha = mean(data$alpha[s[i], item]), 
        beta = mean(data$beta[s[i], item]), 
        x)
      ]
    }
  }

  d <- melt(d, id.vars = x)
  d[, "ind" := ifelse(variable == "y", FALSE, TRUE)]


  # init plot
  ggplot(d, aes(x, value, group = variable)) + 
    geom_path(colour = "#B3E5FC") +
    geom_path(aes(colour = ind), size = 1) + 
    scale_colour_manual(values = c("#03A9F4", "transparent"), guide = "none") + 
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), expand = c(0, 0.05)) + 
    xlab(expression(theta)) + 
    ylab("P(Y = 1)")
}

#' @title
#' item characteristic function
#' 
#' @description 
#' calculates the value of the item characteristic function
#'
#' @param   alpha   item discrimination
#' @param   beta    item difficulty
#' @param   theta   person parameter
#'
#' @export
#'
icf <- function(alpha, beta, theta) {
  (1+exp(-alpha*(theta - beta)))^-1
}
