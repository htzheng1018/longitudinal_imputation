# introduce missing calues
introduce_missingness = function(data, mechanism, X, prob) {
  if (mechanism == "MCAR") {
    data[sample(1:nrow(data), size = round(nrow(data) * (1 - prob))), X] = NA
  } else if (mechanism == "MAR") {
    
    # in MAR, observation is based on complete cases
    objective_function = function(a, data, prob) {
      prob_try = plogis(a - 1 / sd(data$y) * data$y)
      mean_prob = mean(prob_try, na.rm = TRUE)
      return(abs(mean_prob - prob))  # to make the mean prob close to the target prob
    }
    # find the a
    optimized_a = optimize(objective_function, interval = c(-5, 5), data = data, prob = prob)$minimum
    # calculate new prob with new a
    prob = plogis(optimized_a - 1 / sd(data$y) * data$y)
    
    data[runif(nrow(data)) > prob, X] = NA
  }
  return(data)
}