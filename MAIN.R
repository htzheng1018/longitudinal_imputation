################
#### set up ####
################

# load SimEngine + functions
{
  library(SimEngine)
  source("create_data.R", local = T)
  source("introduce_missingness.R", local = T)
  source("imp_pmm.R", local = T)
  # source("imp_2lpmm.R", local = T)
  source("imp_2lpmm_sumscore.R", local = T)
}


##############
#### MAIN ####
##############

# It can be ran both locally and in a cluster.

# start time
start_time = Sys.time()

# set up multi-cores
run_on_cluster(
  # use SimEngine
  first = {
    sim = new_sim()
    
    sim %<>% set_levels(
      observation_num = 100,
      start_years = 2000,
      time_list = list(
        "Norm" = list(time_type = "Normal", time_params = c(4, 5, 2, 1), time_beta = c(0.7, 0.8, 0.9)),
        "Pois" = list(time_type = "Poisson", time_params = 8, time_beta = c(2, 2.2, 2.5)),
        "Uni" = list(time_type = "Uniform", time_params = c(1, 3), time_beta = c(11, 11.5, 12))
      ),
      id_list = list(
        "Exp" = list(id_type = "Exponential", id_params = c(log(sqrt(3.2)), log(sqrt(5/4))), id_beta = 3),
        "Pois" = list(id_type = "Poisson", id_params = 2, id_beta = 1.5),
        "Gam" = list(id_type = "Gamma", id_params = c(3, 2), id_beta = 12)
      )
    )
    
    sim %<>% set_config(num_sim = 100, n_cores = 4, seed = 1018,
                        packages = c("dplyr", "parallel", "MASS", "Matrix", "mice", "miceadds")
    )
    
    sim %<>% set_script(function() {
      # create datasets
      data = create_data(L$observation_num, L$start_year, L$time_list$time_type, L$time_list$time_params, L$time_list$time_beta, L$id_list$id_type, L$id_list$id_params, L$id_list$id_beta)$data
      data_raw = create_data(L$observation_num, L$start_year, L$time_list$time_type, L$time_list$time_params, L$time_list$time_beta, L$id_list$id_type, L$id_list$id_params, L$id_list$id_beta)$raw
      
      # imputation process
      m = 5
      imp.use2l = imp_2lpmm_sumscore(data, c("x1", "x2", "x3"), "z1", "x_sum", m)
      imp.use = imp_pmm(data, c("x1", "x2", "x3", "x_sum"), "z1", m)
      
      mean_xsum_2lpmm = c()
      mean_xsum_pmm = c()
      for (i in (1: m)) {
        mean_xsum_2lpmm[i] = mean(complete(imp.use2l, i)$x_sum)
        mean_xsum_pmm[i] = mean(complete(imp.use, i)$x_sum)
      }
      
      # result
      xsum_2lpmm = mean(mean_xsum_2lpmm)
      xsum_pmm = mean(mean_xsum_pmm)
      xsum_true = mean(data_raw$x_sum)
      
      return(list(
        "xsum_true" = xsum_true,
        "xsum_2lpmm" = xsum_2lpmm,
        "xsum_pmm" = xum_pmm,
        "xsum_2l_pctg" = (xsum_2lpmm - xsum_true) / xsum_true * 100,
        "xsum_1l_pctg" = (xsum_pmm - xsum_true) / xsum_true * 100,
        ".complex" = list(
          "data_raw" = data_raw,
          "data" = data
        )
      ))
    })
  },
  
  main = {
    sim %<>% run()
    print(sim$errors)
  },
  
  last = {
    # bias
    bias_xsum = sim %>% SimEngine::summarize(
      list(stat = "bias", estimate = "xsum_2lpmm", truth = "xsum_true", name = "bias_xsum_2l"),
      list(stat = "bias", estimate = "xsum_pmm", truth = "xsum_true", name = "bias_xsum_1l"),
      list(stat = "mse", estimate = "xsum_2lpmm", truth = "xsum_true", name = "mse_xsum_2l"),
      list(stat = "mse", estimate = "xsum_pmm", truth = "xsum_true", name = "mse_xsum_1l")
      # list(stat = "coverage", lower = "2l_low", upper = "2l_up", truth = "x1_true", name = "cov_x1_2l"),
      # list(stat = "coverage", lower = "1l_low", upper = "1l_up", truth = "x1_true", name = "cov_x1_1l")
    )
    # bias percentage
    bias_xsum_pct = sim %>% SimEngine::summarize(
      list(stat = "mean", x = "xsum_2l_pctg", name = "bias_xsum_2lpmm"),
      list(stat = "mean", x = "xsum_1l_pctg", name = "bias_xsum_pmm")
    )
  },
  
  cluster_config = list(js = "slurm")
)



# end time
end_time = Sys.time()
execution_time = end_time - start_time
print(execution_time)