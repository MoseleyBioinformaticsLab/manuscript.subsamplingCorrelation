## Load your packages, e.g. library(targets).
tar_source("./packages.R")

## Load your R files
tar_source(files = "R")

select_functions = rlang::syms(c("var_select", "pca_select"))
#reference_cor = readRDS(here::here("data/recount_adeno_cor.rds"))
fractions = (c(seq(0.01, 0.09, 0.01), seq(0.1, 0.9, 0.05)))

iteration_select = tidyr::expand_grid(selection_function = select_functions,
                                      fraction = fractions)

## tar_plan supports drake-style targets and also tar_target()
tmp = list(
  tar_plan(

  rep_10 = rep(0.1, 20),
  
  # loading adenocarcinoma data -----
  tar_target(adeno_file,
             "data/recount_adeno_counts.rds",
             format = "file"),
  transcript_data = readRDS(adeno_file),
  transcript_pca = prcomp(t(log1p(transcript_data)), center = TRUE, scale. = FALSE),
  
  transcript_na = zero_to_na(transcript_data),
  
  ref_cor = ici_kendalltau(t(transcript_na)),
  ref_completeness = pairwise_completeness(t(transcript_na)),
  
  
  tar_target(select_random_fraction,
             select_random(transcript_na, fractions),
             pattern = map(fractions)
  ),
  
  tar_target(run_random,
             run_fractional_correlation(select_random_fraction),
             pattern = map(select_random_fraction)
  ),
  
  tar_target(select_ss_small,
             select_samples(transcript_na, n_sample = n_sample),
             pattern = map(n_sample = small_samples)
  ),
  
  tar_target(select_ss_big,
             select_samples(transcript_na, n_sample = n_sample),
             pattern = map(n_sample = big_samples)
  ),
  
  tar_target(run_small,
             run_small_samples(select_ss_small),
             pattern = map(select_ss_small)
  ),
  
  tar_target(run_big,
             run_big_samples(select_ss_big),
             pattern = map(select_ss_big)
  ),
  
  tar_target(results_random,
             random_2_reference(run_random, ref_cor),
             pattern = map(run_random)
  ),
  

  tar_target(results_small,
             get_run_time(run_small),
             pattern = map(run_small)
  ),
  
  tar_target(results_big,
             get_run_time(run_big),
             pattern = map(run_big)
  ),
  
  tar_target(results_rand_multiple,
             random_2_reference(run_random_multiple, ref_cor),
             pattern = map(run_random_multiple),
             iteration = "list"
  ),
  
  reshaped_random = reshape_data(results_random),
  reshaped_nonrandom = reshape_data(combined_nonrandom),
  reshaped_multiple = reshape_data(results_rand_multiple)
  
  # ends tar_plan
  ),
  nonrandom_stuff = tar_map(unlist = FALSE,
                            values = iteration_select,
                            tar_target(select_nonrandom_fraction, selection_function(transcript_pca, transcript_na, fraction)),
                            tar_target(run_nonrandom, run_fractional_correlation(select_nonrandom_fraction)),
                            tar_target(results_nonrandom, random_2_reference(run_nonrandom, ref_cor))),
  
  tar_combine(combined_nonrandom,
              nonrandom_stuff[["results_nonrandom"]],
              command = dplyr::bind_rows(!!!.x))
)
