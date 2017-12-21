# Creates a dataframe with the probabilities for each span value
#
# ARGUMENTS:
# data : a dataframe
# langue : name of the language for which we want to compute to probabilities
#
# RETURN VALUE:
# 
# A dataframe with 2 columns
#
#  - span: the value of the spans (all spans in the sample space, including those not attested in the treebank of this language)
#  - proba : the probability of this span for the language "langue"
#
proba_spans <- function(data, langue) {
  possible_spans <- unique(data[["span"]])
  result <- data %>% dplyr::filter(language==langue) %>% dplyr::group_by(span) %>% dplyr::summarise(proba=n()/nrow(dplyr::filter(data, language==langue)))
  for (i in 1:length(possible_spans)) {
    if (!(possible_spans[i] %in% result$span)) {
      # a span which is not attested in this language but it attested in the other one
      new_entry <- data.frame(possible_spans[i], 0)
      names(new_entry) <- c("span", "proba")
      result <- rbind(result, new_entry)
      print(nrow(result))
    }
  }
  return(result)
}

# Assign randomly a value in one of the columns of a datafram
#
# ARGUMENTS:
# d : a dataframe
#
# RETURN VALUE:
# 
# The same dataframe but with a randomly assigned language
#
#
randomize_lang <- function(d) {
  result <- d
  n <- nrow(d)
  d[["language"]] <- sample(d[["language"]], n)
  return(d)
}


# Mesure Wasserstein (difference in probability distribution) sur des donnes permutees
#
# ARGUMENTS:
# n_samples : number of samples to draw each time
# data : our datafram
# lang1, lang2 : the names of our languages
# var : the variable we want to compare the probability distribution of
#
# RETURN VALUE:
# 
# A tible with
# - n_samples lines
# - 1 column WassersteinMetric with the measure for each draw
#
wasserstein_table <- function(n_samples, data, lang1, lang2, var) {
  N_SAMPLES <- n_samples
  statistics <- rep(0, N_SAMPLES)
  for (i in 1:N_SAMPLES) {
    fake_data <- randomize_lang(data)
    fake_data_en <- dplyr::filter(fake_data, language==lang1)
    fake_data_pcm <- dplyr::filter(fake_data, language==lang2)
    statistics[i] <- transport::wasserstein1d(fake_data_en[[var]],fake_data_pcm[[var]])
  }
  statistics_d <- tibble::tibble(WassersteinMetric=statistics)
  return(statistics_d)
}

# Mesure Wasserstein (difference in probability distribution) sur des donnes echantillonnees par bootrstrapping
#
# ARGUMENTS:
# N_SAMPLES : number of samples to draw each time
# data_1 : a vector of data for group1
# data_2 : a vector of data for group2
# lang1, lang2 : the names of our languages
#
# RETURN VALUE:
# 
# A vector with the wasserstein metric measures between the 2 group's probability distribution for each sample
wasserstein_bootstrapped <- function(N_SAMPLES, data_1, data_2) {
  w <- rep(0, N_SAMPLES)
  for (i in 1:N_SAMPLES) {
  data_1_sampled <- sample(data_1, length(data_1), replace=TRUE)
  data_2_sampled <- sample(data_2, length(data_2), replace=TRUE)
  w[i] <-transport::wasserstein1d(data_1_sampled, data_2_sampled)
  }
  return(w)
}


# Plots a histogram given a vector of measures. Add informations on 1st quantile, 3 quantile, median and median in the original data.
#
# ARGUMENTS:
# data : the vector of measures
# observed_med : the median in the original data
# the name we will give to the column containing the measure (since we use a tibble)
#
# RETURN VALUE:
# 
# A vector with the wasserstein metric measures between the 2 group's probability distribution for each sample
plot_metric_w_q <- function(data, observed_med, column_name) {
  first_q <- quantile(data, 0.025)
  median <- quantile(data, 0.5)
  third_q <- quantile(data, 0.975)
  ggplot2::ggplot(tibble::tibble(column_name=data), ggplot2::aes(x=data)) +
    ggplot2::geom_histogram(fill="#579ECF", colour="black", binwidth=0.01) +
    ggplot2::geom_vline(xintercept=first_q, color="red", lty="dashed") +
    ggplot2::geom_vline(xintercept=third_q, color="red", lwd=1, lty="dashed") +
    ggplot2::geom_vline(xintercept=observed_med, color="black", lty="dashed", lwd=1) +
    ggplot2::geom_vline(xintercept=median, color="red", lty="dashed")
}

# Plots a histogram of WassersteinMetric
#
# ARGUMENTS:
# data : the dataframe of measures
# observed_value : the value of the wasserstein metric in our original data
#
# RETURN VALUE:
# 
# A vector with the wasserstein metric measures between the 2 group's probability distribution for each sample
plot_permuted_wasserstein <- function(data, observed_value) {
  # I wrote the column name WassersteinMetric in the function which is not optimal
  # but passing the column name as it is or as a string causes problems
  # and for some reason I couldn't get the UQ function (which I have used in the past for similar situations) to work
  p <- ggplot2::ggplot(data, ggplot2::aes(x=WassersteinMetric)) +
    ggplot2::geom_histogram(fill="#FFD86D", colour="black", binwidth = 0.01)
  p <- p + ggplot2::geom_vline(xintercept=observed_value, lwd=1, lty="dashed")
  return(p)
}