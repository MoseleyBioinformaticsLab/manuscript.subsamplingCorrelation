## Load your packages, e.g. library(targets).
tar_source("./packages.R")

## Load your R files
tar_source(files = "R")

select_functions = rlang::syms(c("var_select", "pca_select",
                                 "random_select"))
#reference_cor = readRDS(here::here("data/recount_adeno_cor.rds"))
fractions = (c(seq(0.01, 0.09, 0.01), seq(0.1, 0.9, 0.05)))

adenocarcinoma_select = tidyr::expand_grid(selection_function = select_functions,
                                      pca = rlang::syms("adenocarcinoma_pca"),
                                      fraction = fractions)


## tar_plan supports drake-style targets and also tar_target()
adenocarcinoma_related = tar_plan(

  rep_10 = rep(0.1, 20),
  
  # loading adenocarcinoma data -----
  tar_target(adeno_file,
             "data/recount_adeno_counts.rds",
             format = "file"),
  adenocarcinoma_data = readRDS(adeno_file),
  adenocarcinoma_pca = prcomp(t(log1p(adenocarcinoma_data)), center = TRUE, scale. = FALSE),
  
  adenocarcinoma_na = zero_to_na(adenocarcinoma_data),
  
  ici_adenocarcinoma_cor = ici_kendalltau(t(adenocarcinoma_na)),
  ici_transcript_completeness = pairwise_completeness(t(adenocarcinoma_na)),
)

adenocarcinoma_run_map = tar_map(unlist = FALSE,
                                values = adenocarcinoma_select,
                              tar_target(adenocarcinoma_selections,
                                         selection_function(pca, adenocarcinoma_na, fraction)),
                              tar_target(adenocarcinoma_runs,
                                         run_fractional_correlation(adenocarcinoma_selections)),
                              tar_target(adenocarcinoma_results,
                                         random_2_reference(adenocarcinoma_runs, ici_adenocarcinoma_cor)))

adenocarcinoma_combine_map = tar_combine(adenocarcinoma_combined,
                                         adenocarcinoma_run_map[["adenocarcinoma_results"]],
                                         command = dplyr::bind_rows(!!!.x))
  
list(adenocarcinoma_related,
     adenocarcinoma_run_map,
     adenocarcinoma_combine_map)
