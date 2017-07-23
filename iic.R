#' @title
#' item information curve
#' 
#' @description
#' plots the item information for a given item
#' 
#' @param   item          item indicator (an integer)
#' @param   data          posterior samples from an IRT model
#' @param   range         x axis limits
#' @param   random_draws  number of random parameter draws from the posterior    
#'
#' @export
#'
iic <- function(item, data, range = 5, random_draws = 0, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  d <- data.table("x" = seq(-range, range, length.out = 200))
  d[, "y" := iif(alpha = mean(data$alpha[,item]), beta = mean(data$beta[,item]), x)]

  if (random_draws > 0) { 
    s <- sample(1:nrow(data$beta), random_draws, replace = FALSE)

    for (i in 1:random_draws) {
      d[, paste0("v", i) := iif(
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
    geom_path(colour = "#CE93D8") +
    geom_path(aes(colour = ind), size = 1) + 
    scale_colour_manual(values = c("#9C27B0", "transparent"), guide = "none") + 
    scale_x_continuous(limits = c(-range, range), breaks = c(-range, 0, range), expand = c(0, 0)) + 
    xlab(expression(theta)) + 
    ylab("information")
}

#' @title
#' item information function
#' 
#' @description 
#' calculates the value of the item information function
#'
#' @param   alpha   item discrimination
#' @param   beta    item difficulty
#' @param   theta   person parameter
#'
#' @export
#'
iif <- function(alpha, beta, theta) {
  alpha^2 * icf(alpha, beta, theta) * (1 - icf(alpha, beta, theta))
}
