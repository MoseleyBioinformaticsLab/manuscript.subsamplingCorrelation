## Load your packages, e.g. library(targets).
tar_source("./packages.R")

## Load your R files
tar_source(files = "R")

## tar_plan supports drake-style targets and also tar_target()
tar_plan(

  select_functions = rlang::syms(c("var_select", "pca_select")),
  #reference_cor = readRDS(here::here("data/recount_adeno_cor.rds"))
  fractions = (c(seq(0.01, 0.09, 0.01), seq(0.1, 0.9, 0.05))),
  rep_10 = rep(0.1, 20),
  
  small_samples = seq(4, 40, 4),
  big_samples = seq(40, 264, 20),
  
  x = seq(1, 10),
  y = seq(1, 10),
  y2 = seq(10, 1),
  where_na = create_na_indices(20),
  
  all_kt = all_kendalltau(x, y, y2, where_na),
  
  positive_kt = compare_positive_kt(x, y, where_na),
  negative_kt = compare_negative_kt(x, y2, where_na),
  positive_pearson = compare_positive_pearson(x, y, where_na),
  negative_pearson = compare_negative_pearson(x, y2, where_na),
  
  positive_kendall = compare_positive_pearson(x, y, where_na, method = "kendall"),
  negative_kendall = compare_negative_pearson(x, y2, where_na, method = "kendall"),
  
  realistic_sample_1 = create_sample(n = 1000),
  realistic_sample_2 = create_sample(n = 1000),
  realistic_neg_sample = sort(realistic_sample_2, decreasing = TRUE),
  realistic_na = create_random_na(),
  realistic_positive_kt = compare_positive_kt(realistic_sample_1, realistic_sample_2, realistic_na),
  realistic_negative_kt = compare_negative_kt(realistic_sample_1, realistic_neg_sample, realistic_na),
  
  realistic_positive_pearson = compare_positive_pearson(realistic_sample_1, realistic_sample_2, realistic_na),
  realistic_negative_pearson = compare_negative_pearson(realistic_sample_1, realistic_neg_sample, realistic_na),
  
  realistic_positive_kendall = compare_positive_pearson(realistic_sample_1, realistic_sample_2, realistic_na, method = "kendall"),
  realistic_negative_kendall = compare_negative_pearson(realistic_sample_1, realistic_neg_sample, realistic_na, method = "kendall")
# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style

)
