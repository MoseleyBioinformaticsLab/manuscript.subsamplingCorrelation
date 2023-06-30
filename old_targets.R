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
  
  # small theoretical example --------
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
  
  # bigger, more realistic example ---------
  realistic_sample_1 = create_sample(n = 1000),
  realistic_sample_2 = create_sample(n = 1000),
  realistic_neg_sample = sort(realistic_sample_2, decreasing = TRUE),
  realistic_na = create_random_na(),
  realistic_positive_kt = compare_positive_kt(realistic_sample_1, realistic_sample_2, realistic_na),
  realistic_negative_kt = compare_negative_kt(realistic_sample_1, realistic_neg_sample, realistic_na),
  
  realistic_positive_pearson = compare_positive_pearson(realistic_sample_1, realistic_sample_2, realistic_na),
  realistic_negative_pearson = compare_negative_pearson(realistic_sample_1, realistic_neg_sample, realistic_na),
  
  realistic_positive_kendall = compare_positive_pearson(realistic_sample_1, realistic_sample_2, realistic_na, method = "kendall"),
  realistic_negative_kendall = compare_negative_pearson(realistic_sample_1, realistic_neg_sample, realistic_na, method = "kendall"),
  
  transcript_data = readRDS(here::here("data/recount_adeno_counts.rds")),
  transcript_pca = prcomp(t(log1p(transcript_data)), center = TRUE, scale. = FALSE),
  
  transcript_na = zero_to_na(transcript_data),
  
  ref_cor = ici_kendalltau(t(transcript_na)),
  ref_completeness = pairwise_completeness(t(transcript_na)),
  
  tar_target(select_nonrandom_fraction,
    select_data(transcript_pca, transcript_na, eval(select_functions), fractions),
    pattern = cross(select_functions, fractions)
  ),
  
  tar_target(run_nonrandom,
    run_fractional_correlation(select_nonrandom_fraction),
    pattern = map(select_nonrandom_fraction)
  ),
  
  tar_target(select_random_fraction,
    select_random(transcript_na, fraction = frac_value),
    pattern = map(frac_value = fractions)
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
  
  # tar_target(combined_random = target(
  #   bind_rows(results_random),
  #   transform = combine(results_random)
  # ),
  
  tar_target(results_small,
    get_run_time(run_small),
    pattern = map(run_small)
  ),
  
  # combined_small = target(
  #   bind_rows(results_small),
  #   transform = combine(results_small)
  # ),
  # 
  tar_target(results_big,
    get_run_time(run_big),
    pattern = map(run_big)
  ),
  
  # combined_big = target(
  #   bind_rows(results_big),
  #   transform = combine(results_big)
  # ),
  
  tar_target(results_nonrandom,
    random_2_reference(run_nonrandom, ref_cor),
    pattern = map(run_nonrandom),
    iteration = "list"
  ),
  
  # combined_nonrandom = target(
  #   bind_rows(results_nonrandom),
  #   transform = combine(results_nonrandom)
  # ),
  
  tar_target(results_rand_multiple,
    random_2_reference(run_random_multiple, ref_cor),
    pattern = map(run_random_multiple),
    iteration = "list"
  ),
  
  # combined_rand_multiple = target(
  #   bind_rows(results_rand_multiple),
  #   transform = combine(results_rand_multiple)
  # ),
  
  reshaped_random = reshape_data(results_random),
  reshaped_nonrandom = reshape_data(results_nonrandom),
  reshaped_multiple = reshape_data(results_rand_multiple),
  
  # pearson and kendall results
  # setup_pearson = list(data = transcript_na,
  #                      type = "random",
  #                      frac = 1),
  # ref_pearson = run_fractional_pearson(setup_pearson),
  # ref_pearson_log = run_fractional_pearson(setup_pearson, log_transform = TRUE),
  # ref_pearson_0 = run_fractional_pearson(setup_pearson, replace_0 = TRUE),
  # ref_pearson_0_log = run_fractional_pearson(setup_pearson, log_transform = TRUE, replace_0 = TRUE),
  # ref_kendall = run_fractional_kendall(setup_pearson),
  # ref_kendall_0 = run_fractional_kendall(setup_pearson, replace_0 = TRUE),
  # 
  # run_random_pearson = target(
  #   run_fractional_pearson(select_random_fraction),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # run_random_pearson_log = target(
  #   run_fractional_pearson(select_random_fraction, log_transform = TRUE),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # run_random_pearson_0 = target(
  #   run_fractional_pearson(select_random_fraction, replace_0 = TRUE),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # run_random_pearson_0_log = target(
  #   run_fractional_pearson(select_random_fraction, log_transform = TRUE, replace_0 = TRUE),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # run_random_kendall = target(
  #   run_fractional_kendall(select_random_fraction),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # run_random_kendall_0 = target(
  #   run_fractional_kendall(select_random_fraction, replace_0 = TRUE),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # results_random_pearson = target(
  #   random_2_reference(run_random_pearson, ref_pearson, "cor"),
  #   transform = map(run_random_pearson)
  # ),
  # 
  # combined_random_pearson = target(
  #   bind_rows(results_random_pearson),
  #   transform = combine(results_random_pearson)
  # ),
  # 
  # results_random_pearson_log = target(
  #   random_2_reference(run_random_pearson_log, ref_pearson_log, "cor"),
  #   transform = map(run_random_pearson_log)
  # ),
  # 
  # combined_random_pearson_log = target(
  #   bind_rows(results_random_pearson_log),
  #   transform = combine(results_random_pearson_log)
  # ),
  # 
  # results_random_pearson_0 = target(
  #   random_2_reference(run_random_pearson_0, ref_pearson_0, "cor"),
  #   transform = map(run_random_pearson_0)
  # ),
  # 
  # combined_random_pearson_0 = target(
  #   bind_rows(results_random_pearson_0),
  #   transform = combine(results_random_pearson_0)
  # ),
  # 
  # results_random_pearson_0_log = target(
  #   random_2_reference(run_random_pearson_0_log, ref_pearson_0_log, "cor"),
  #   transform = map(run_random_pearson_0_log)
  # ),
  # 
  # combined_random_pearson_0_log = target(
  #   bind_rows(results_random_pearson_0_log),
  #   transform = combine(results_random_pearson_0_log)
  # ),
  # 
  # 
  # results_random_kendall = target(
  #   random_2_reference(run_random_kendall, ref_kendall, "cor"),
  #   transform = map(run_random_kendall)
  # ),
  # 
  # combined_random_kendall = target(
  #   bind_rows(results_random_kendall),
  #   transform = combine(results_random_kendall)
  # ),
  # 
  # results_random_kendall_0 = target(
  #   random_2_reference(run_random_kendall_0, ref_kendall_0, "cor"),
  #   transform = map(run_random_kendall_0)
  # ),
  # 
  # combined_random_kendall_0 = target(
  #   bind_rows(results_random_kendall_0),
  #   transform = combine(results_random_kendall_0)
  # ),
  # 
  # left_censored_samples = create_lc_samples(),
  # left_censored_cor = left_censor_correlate(left_censored_samples),
  # random_censored_cor = random_censor_correlate(left_censored_samples),
  # logtransform_censored_cor = lt_left_censor_correlate(left_censored_samples),
  # 
  # subsample = sample(1000, 50),
  # left_sample_cor = left_censor_correlate(left_censored_samples[subsample, ]),
  # random_sample_cor = random_censor_correlate(left_censored_samples[subsample, ], n_na = seq(0, 12, 2)),
  # logtransform_sample_cor = lt_left_censor_correlate(left_censored_samples[subsample, ]),
  # 
  # brainsonrnaseq_counts = readRDS(here::here("data", "brainson_rnaseq201901_counts.rds")),
  # brainsonrnaseq_info = readRDS(here::here("data", "brainson_rnaseq201901_info.rds")),
  # brainsonrnaseq_outliers_1 = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 1, "sample", "tumor"),
  # brainsonrnaseq_outliers_25 = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 0.25, "sample", "tumor"),
  # brainsonrnaseq_outliers_50 = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 0.5, "sample", "tumor"),
  # brainsonrnaseq_outliers_75 = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 0.75, "sample", "tumor"),
  # brainsonrnaseq_outliers_100 = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 0.99, "sample", "tumor"),
  # 
  # brainsonrnaseq_outliers_1_alt = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 1, "sample", c("type", "tumor")),
  # brainsonrnaseq_outliers_25_alt = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 0.25, "sample", c("type", "tumor")),
  # brainsonrnaseq_outliers_50_alt = filter_generate_outliers(brainsonrnaseq_counts, brainsonrnaseq_info, 0.5, "sample", c("type", "tumor")),
  # 
  # yeast_paper_outliers = c("WT.21", "WT.22", "WT.25", "WT.28", "WT.34", "WT.36",
  #                          "Snf2.06", "Snf2.13", "Snf2.25", "Snf2.35"),
  # yeast_counts_info = readRDS(here::here("data", "yeast_counts_info.rds")),
  # yeast_outliers_1 = filter_generate_outliers(yeast_counts_info$counts, yeast_counts_info$info, 1,
  #                                             "sample_rep", "sample"),
  # yeast_outliers_50 = filter_generate_outliers(yeast_counts_info$counts, yeast_counts_info$info, 0.5,
  #                                              "sample_rep", "sample"),
  # yeast_outliers_25 = filter_generate_outliers(yeast_counts_info$counts, yeast_counts_info$info, 0.25,
  #                                              "sample_rep", "sample"),
  # 
  # adeno_info = readRDS(here::here("data", "transcript_info.rds")),
  # adeno_data = transcript_data,
  # adeno_outliers_1 = filter_generate_outliers(adeno_data, adeno_info, 1,
  #                                             "sample_id2", "tissue_type"),
  # adeno_outliers_25 = filter_generate_outliers(adeno_data, adeno_info, 0.25,
  #                                              "sample_id2", "tissue_type"),
  # adeno_outliers_50 = filter_generate_outliers(adeno_data, adeno_info, 0.5,
  #                                              "sample_id2", "tissue_type"),
  # 
  # eval_random = target(
  #   evaluate_by_pca(select_random_fraction, transcript_pca),
  #   transform = map(select_random_fraction)
  # ),
  # 
  # combined_eval_random = target(
  #   bind_rows(eval_random),
  #   transform = combine(eval_random)
  # ),
  # 
  # eval_nonrandom = target(
  #   evaluate_by_pca(select_nonrandom_fraction, transcript_pca),
  #   transform = map(select_nonrandom_fraction)
  # ),
  # 
  # combined_eval_nonrandom = target(
  #   bind_rows(eval_nonrandom),
  #   transform = combine(eval_nonrandom)
  # ),
  # 
  # all_pca_eval = bind_rows(combined_eval_random,
  #                          combined_eval_nonrandom),
  # 
  # pca_eval_summary = summarize_by_pca(all_pca_eval),
  # 
  # single_core_perf = run_single_cor(),
  # complexity_figure = create_complexity_figure(single_core_perf),
  # 
  # 
  # nsclc_info = readRDS("data/nsclc_info"),
  # nsclc_medians = readRDS("data/nsclc_scancentric_medians"),
  # nsclc_peaks = readRDS("data/nsclc_ppm_matched_peaks.rds"),
  # 
  # tar_render(supp_materials,
  #             "doc/supplemental_materials.Rmd"),
  # 
  # tar_render(manuscript,
  #          "doc/ici_kt_manuscript.Rmd"),
  # 
  # supp_materials_file = file_in("doc/supplemental_materials.docx"),
  # 
  # supp_tables = target(
  #   command = {
  #     rmarkdown::render(knitr_in("doc/supplemental_tables.Rmd"))
  #     file_out("doc/supplemental_tables.docx")
  #   }
  # ),
  # 
  # supp_tables_file = file_in("doc/supplemental_tables.docx"),
  # supp_tables_rds = file_in("doc/supp_table_count.rds"),
  # supp_figures_rds = file_in("doc/supp_figure_count.rds"),
  # supp_stuff_rda = file_in("doc/supp_stuff.rda"),
  # 
  # 

# target = function_to_make(arg), ## drake style

# tar_target(target2, function_to_make2(arg)) ## targets style

)
