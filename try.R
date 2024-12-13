{
  library(SimEngine)
  source("create_data.R", local = T)
  source("introduce_missingness.R", local = T)
}





mydata = create_data(100, 2000, "Normal", c(4, 5, 2, 1), 0.7, "Exponential", c(log(sqrt(3.2)), log(sqrt(5/4))), 3)$data
# mydata_raw = create_data(100, 2000, "Poisson", 8, 2, "Gamma", c(3, 2), 12)$raw


mean(mydata$x1, na.rm = T)
mean(mydata$x2, na.rm = T)
mean(mydata$x3, na.rm = T)

# imputation process
m = 5
imp.use2l = imp_2lpmm_sumscore(mydata, c("x1", "x2", "x3"), "z1", m)