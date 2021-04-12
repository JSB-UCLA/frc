np_order_stat <- function(size, alpha, delta) {
	violation_rates <- pbinom(q=0:(size-1), size=size, prob=1-alpha, lower.tail=F) 
	return( which(violation_rates <= delta)[1] )
}

#' Calculation of s-CC
#'
#' This function loads a feature x (numeric, containing feature values)
#' and a response vector (numeric or character, containing class labels). 
#'
#' @param x a numeric feature vector containing one feature's values
#' @param y a numeric or character response vector containing class labels; its length must be equal to that of x
#' @param B the number of random splits, with each split dividing the observations into two halves; default value: 11
#' @param seed an integer of the random seed value; default value: 1

#' @return sCC: a numeric value between 0 and 1, a smaller value indicating a more predictive feature
#' @export
sCC <- function(x, y, B=11, seed=1) {
	set.seed(seed)
	if (!is.numeric(x)) {
		stop("x must be numeric.")
	}
	if (!is.numeric(y) & !is.character(y)) {
		stop("y must be numeric or character.")
	}
	n <- length(x)
	s <- sd(x)
	if (length(y) != n) {
		stop("The lengths of x and y are not equal.")
	}
	labels <- unique(y) # class labels
	K <- length(labels) # the number of classes
	n_list <- sapply(labels, FUN=function(label) sum(y==label)) # the number of observations in each class
	if (min(n_list) < 2) {
		stop("The smallest class should have at least two observations. We do not recommend using this function if the smallest class has fewer than 20 observations.")
	}
	x_list <- lapply(labels, FUN=function(label) x[y==label])
	sCCs <- sapply(1:B, function(b) {
		idx_ts_list <- vector("list", K) # indices of training observations of class k
		x_lo <- NULL
		y_lo <- NULL
		for (k in 1:K) {
			idx_ts_list[[k]] <- sample(1:n_list[k], ceiling(n_list[k]/2)) 
			x_lo_tmp <- x_list[[k]][-idx_ts_list[[k]]]
			x_lo <- c(x_lo, x_lo_tmp)
			y_lo <- c(y_lo, rep(k, length(x_lo_tmp)))
		}
		pred_mat <- sapply(1:K, FUN=function(k) {
			x_ts_tmp <- x_list[[k]][idx_ts_list[[k]]]
			dk <- try(ks::kde(x=x_ts_tmp, eval.points=x_lo)$estimate, silent=TRUE)
			while (class(dk)=="try-error") { # if density estimation is unsuccessful
				x_ts_tmp <- x_ts_tmp + rnorm(length(x_ts_tmp), sd=s/10)
				dk <- try(ks::kde(x=x_ts_tmp, eval.points=x_lo)$estimate, silent=TRUE)
			}
			dk[is.na(dk) | dk<=0] <- 10^(-88)
			dk * n_list[k]/n
		})
		y_lo_hat <- apply(pred_mat, 1, which.max)
		# classification error
		sum((y_lo - y_lo_hat) != 0) / length(x_lo)
	})
	sCC <- mean(sCCs)
	return(sCC)
}

#' Calculation of s-NPC
#'
#' This function loads a feature x (numeric, containing feature values)
#' and a response vector (numeric or character, containing class labels). 
#'
#' @param x a numeric feature vector containing one feature's values
#' @param y a numeric or character response vector containing class labels; its length must be equal to that of x
#' @param imp_class a numeric or character value of the most important class; the value must be in y; the most important class would be treated as class 0, and the other classes would be combined into class 1; default value: the first value of y in numeric or alphabetical order
#' @param alpha a numeric value between 0 and 1, the upper bound on the population type I error (i.e., the probability that a class 0 observation is predicted as class 1); default value: 0.3
#' @param delta a numeric value between 0 and 1, the violation rate of the population type I error (i.e., the probability that the population type I error exceeds alpha); default value: 0.05
#' @param B the number of random splits, with each split dividing the observations into two halves; default value: 11
#' @param seed an integer of the random seed value; default value: 1

#' @return sNPC: a numeric value between 0 and 1, a smaller value indicating a more predictive feature
#' @export
sNPC <- function(x, y, imp_class=sort(y)[1], alpha=0.3, delta=0.05, B=11, seed=1) {
	set.seed(seed)
	if (!is.numeric(x)) {
		stop("x must be numeric.")
	}
	if (!is.numeric(y) & !is.character(y)) {
		stop("y must be numeric or character.")
	}
	if (alpha < 0 | alpha > 1) {
		stop("The alpha value must be between 0 and 1.")
	}
	if (delta < 0 | delta > 1) {
		stop("The delta value must be between 0 and 1.")
	}
	n <- length(x)
	s <- sd(x)
	if (length(y) != n) {
		stop("The lengths of x and y are not equal.")
	}
	labels <- unique(y) # class labels
	if (!(imp_class %in% labels)) {
		stop("The value of imp_class is not in y.")
	}
	y_old <- y
	y <- rep(1, n)
	y[y_old == imp_class] <- 0
	idx0 <- which(y == 0)
	idx1 <- which(y == 1)
	n0 <- length(idx0)
	n1 <- length(idx1)
	x0 <- x[idx0]
	x1 <- x[idx1]
	n0_min <- 2 * ceiling(log(delta)/log(1-alpha))
	if (n0 < n0_min) {
		alpha_min <- round(1 - exp(log(delta)/floor(n0/2)), digits=1)
		stop(paste0("The minimum sample size of the important class must be at least ", n0_min, ", while the current size is only ", n0, ". Please increase alpha to at least ", alpha_min, "."))
	}
	if (n1 < 2) {
		stop("There is only one observation not in the importance class. We do not recommend using this function if fewer than 20 observations are not in the important class.")
	}
	sNPCs <- sapply(1:B, function(b) {
		n0_ts <- ceiling(n0/2)
		n0_lo <- n0 - n0_ts
		idx0_ts <- sample(1:n0, n0_ts)
		x0_ts <- x0[idx0_ts]
		x0_lo <- x0[-idx0_ts]
		
		n1_ts <- ceiling(n1/2)
		n1_lo <- n1 - n1_ts
		idx1_ts <- sample(1:n1, n1_ts)
		x1_ts <- x1[idx1_ts]
		x1_lo <- x1[-idx1_ts]
	
		x_lo <- c(x0_lo, x1_lo)
		y_lo <- c(rep(0, n0_lo), rep(1, n1_lo))
		n_lo <- n0_lo + n1_lo
		
		d0 <- try(ks::kde(x=x0_ts, eval.points=x_lo)$estimate, silent=TRUE)
		while (class(d0)=="try-error") { # if density estimation is unsuccessful
			x0_ts <- x0_ts + rnorm(n0_ts, sd=s/10)
			d0 <- try(ks::kde(x=x0_ts, eval.points=x_lo)$estimate, silent=TRUE)
		}
		d0[is.na(d0) | d0<=0] <- 10^(-88)
		
		d1 <- try(ks::kde(x=x1_ts, eval.points=x_lo)$estimate, silent=TRUE)
		while (class(d1)=="try-error") { # if density estimation is unsuccessful
			x1_ts <- x1_ts + rnorm(n1_ts, sd=s/10)
			d1 <- try(ks::kde(x=x1_ts, eval.points=x_lo)$estimate, silent=TRUE)
		}
		d1[is.na(d1) | d1<=0] <- 10^(-88)
		
		# classification scores on the left-out data
		s <- d1 / d0
		s0 <- s[1:n0_lo]
		s1 <- s[(n0_lo+1):n_lo]
		
		# classification scores on the left-out class 0 sample
		t <- s0
		t <- sort(t)
		
		# NP threshold
		t_np <- t[np_order_stat(n0_lo, alpha, delta)]
		
		# type II error
		sum(s1 <= t_np) / n1_lo
	})
	sNPC <- mean(sNPCs)
	return(sNPC)
}