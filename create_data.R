# create simulation datasets
create_data = function(observation_num, start_year, time_type, time_params, beta_tv, id_type, id_params, beta_iv) {
  all_years = seq(start_year, 2014, by = 2)
  data_raw = data.frame()
  for (i in 1: observation_num) {
    num_years = sample((1: length(all_years)), 1)
    year = sample(all_years, num_years)
    
    data_tmp = data.frame(
      id = i,
      year = year
    )
    
    data_raw = rbind(data_raw, data_tmp)
  }
  
  data_raw = data_raw %>% arrange(id, year) # raw dataframe, only id and year
  n = nrow(data_raw)
  
  n_random_effects = 4
  mean_vector = rep(0, n_random_effects)
  wishart_matrix = rWishart(1, df = n_random_effects + 2, Sigma = diag(n_random_effects))[,,1]
  random_effects_cov = wishart_matrix
  rand_effect_matrix = mvrnorm(observation_num, mu = mean_vector, Sigma = random_effects_cov)
  
  # random effect
  b_0i = rand_effect_matrix[, 1]
  b_1i = rand_effect_matrix[, 2]
  b_2i = rand_effect_matrix[, 2]
  b_3i = rand_effect_matrix[, 3]
  
  
  # individual covariates
  if (id_type == "Exponential") {
    data_raw$z1 = exp(rnorm(observation_num, mean = id_params[1], sd = id_params[2]))[data_raw$id]
  } else if (id_type == "Poisson") {
    data_raw$z1 = rpois(observation_num, id_params[1])[data_raw$id]
  } else if (id_type == "Gamma") {
    data_raw$z1 = rgamma(observation_num, id_params[1], id_params[2])[data_raw$id]
  }
  
  # time-varying covariates
  beta_0 = 2
  beta_1 = beta_tv[1]
  if (time_type == "Normal") {
    data_raw$x1 = round(rnorm(n, mean = time_params[1], sd = time_params[2])
                        + data_raw$year - 2003
                        + data_raw$z1)
    data_raw$x2 = round(rnorm(n, mean = time_params[1] + 1, sd = time_params[2] + 1)
                        + data_raw$year - 2005
                        + data_raw$z1)
    data_raw$x3 = round(rnorm(n, mean = time_params[1] + 1, sd = time_params[2] + 1)
                        + data_raw$year - 2003
                        + data_raw$z1)
  } else if (time_type == "Poisson") {
    data_raw$x1 = round(mapply(function(year) {
      rpois(1, round((year - 1995) / 5))
    }, data_raw$year)
    + data_raw$z1)
    data_raw$x2 = round(mapply(function(year) {
      rpois(1, round((year - 1995) / 5 + 2))
    }, data_raw$year)
    + data_raw$z1)
    data_raw$x3 = round(mapply(function(year) {
      rpois(1, round((year - 1995) / 10))
    }, data_raw$year)
    + data_raw$z1)
  } else if (time_type == "Uniform") {
    data_raw$x1 = round(mapply(function(year) {
      runif(1, min = (year - 2000) / 8, max = (year - 1992) / 8)
    }, data_raw$year) + data_raw$z1)
    data_raw$x2 = round(mapply(function(year) {
      runif(1, min = (year - 2000) / 8 + 1, max = (year - 1992) / 8 + 1)
    }, data_raw$year) + data_raw$z1)
    data_raw$x3 = round(mapply(function(year) {
      runif(1, min = (year - 2000) / 4, max = (year - 1992) / 4)
    }, data_raw$year) + data_raw$z1)
  }
  
  # outcome
  data_raw$x_sum = data_raw$x1 + data_raw$x2 + data_raw$x3
  data_raw$y = beta_0 + beta_1 * data_raw$x_sum +
    b_0i[data_raw$id] + b_1i[data_raw$id] * data_raw$x_sum +
    beta_iv * data_raw$z1
  rsd = rnorm(nrow(data_raw), 0, 1)
  data_raw$y = data_raw$y + rsd
  
  # missing values
  data = introduce_missingness(data_raw, "MAR", "x1", 0.3)
  data = introduce_missingness(data, "MAR", "x2", 0.5)
  data = introduce_missingness(data, "MAR", "x3", 0.7)
  data = introduce_missingness(data, "MAR", "z1", 0.9)
  data = introduce_missingness(data, "MAR", "y", 0.8)
  
  # x_sum 
  data$x_sum = data$x1 + data$x2 + data$x3
  
  return(list(data = data, raw = data_raw))
}