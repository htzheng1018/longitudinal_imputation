{
  library(SimEngine)
  source("create_data.R", local = T)
  source("introduce_missingness.R", local = T)
}





mydata = create_data(100, 2000, "Normal", c(4, 5, 2, 1), 0.7, "Exponential", c(log(sqrt(3.2)), log(sqrt(5/4))), 3)$data
# mydata_raw = create_data(100, 2000, "Poisson", 8, 2, "Gamma", c(3, 2), 12)$raw
