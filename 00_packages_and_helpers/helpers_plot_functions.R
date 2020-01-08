# ggplot theme ------------------------------------------------------------

b1_theme <-
	function() {
		theme_bw() +
			theme(
				axis.ticks = element_blank(),
				axis.line = element_blank(),
				panel.border = element_blank(),
				panel.grid.major = element_line(color = '#eeeeee'),
				strip.background = element_blank(),
				legend.position = "bottom",
				text = element_text(family = "Palatino")
			)
	}


# Plotting function -------------------------------------------------------


plot_coefs <- function(plot_data){
	plot_data$outcome <- with(plot_data, factor(outcome, levels = outcome[order(estimate)]))
	ggplot(plot_data, aes(y = outcome, x = estimate)) +
		geom_point() +
		geom_vline(xintercept = 0, linetype = "dashed", size = .25) +
		geom_segment(aes(x = conf.low, xend = conf.high, y = outcome, yend = outcome), 
								 alpha = .3) + 
		b1_theme()
}


# RI distributions for plots ----------------------------------------------


get_threshold_given_null <- function(null, test, outcome, alpha = .05){
	if(test == "lower") {qs <- c(alpha, NA)
	} else if(test == "upper"){qs <- c(NA, 1 - alpha)
	} else if(test == "two") {qs <- c(alpha/2, 1 - alpha/2)
	} else if(test == "none"){qs <- c(NA, NA)
	} else {stop("Please provide test with 'upper', 'lower', or 'two'.")}
	
	thresholds <- quantile(null, qs)
	return_frame <- data.frame(lower_ri = thresholds[1], 
														 upper_ri = thresholds[2], 
														 outcome = outcome, 
														 null = null,
														 test = test,
														 row.names = NULL)
	return(return_frame)
}


get_null_plot_data <- function(fit, outcome, test = "none"){
	if (class(fit) == "ri_fit") {
		null_dist <- get("null_distribution", fit)
	} else {
		null_dist <- fit
	}
	
	return_frame <- 
		get_threshold_given_null(null = null_dist, 
														 test = test,
														 outcome = outcome)
	
	return(return_frame)
	
}

