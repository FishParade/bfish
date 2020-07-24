# Function for modeling timecourses.
timecourse_model <- function(datadf, exptdf, 
                             assay_vol = 0.2, path_length = 0.53, 
                             r2_thresh = 0.94, slope_thresh = 0.002) {
  
  # Replicate measurements at the same timepoint are averaged.
  datadf_avg <- datadf %>% 
    group_by(expt_id, time_s, time_m) %>% 
    summarize(abs_mean = mean(abs),
              abs_sd = sd(abs)) %>% 
    ungroup()
    # left_join(abtsexpt_byid)
  
  # Dataframe of conditions for each expt id.
  # i.e. wavelength and assay temp
  datadf_cond <- datadf %>% 
    group_by(expt_id) %>% 
    summarize(assay_temp = mean(temp_c),
              wavelength = median(wavelength),
              assay_datetime = mean(assay_datetime))
  
  # Max absorbance for each assay.
  datadf_max <- datadf %>% 
    group_by(well_id) %>% 
    summarize(abs_max = max(abs)) %>% 
    ungroup()
  
  # Nested df with list cols of model data.
  datadf_nest <- datadf %>% 
    group_by(well_id) %>% 
    nest() %>% 
    mutate(model_1 =  map(data, initial_rate, 0),
           model_2 =  map(data, initial_rate, 1),
           model_3 =  map(data, initial_rate, 2),
           model_4 =  map(data, initial_rate, 3),
           model_5 =  map(data, initial_rate, 4),
           model_6 =  map(data, initial_rate, 5),
           model_7 =  map(data, initial_rate, 6),
           model_8 =  map(data, initial_rate, 7),
           model_9 =  map(data, initial_rate, 8),
           model_10 = map(data, initial_rate, 9)
           ) %>% 
    pivot_longer(cols = model_1:model_10) %>% 
    rename(model_name = name, model = value) %>%
    filter(!is.na(model)) %>% 
    mutate(resids = map2(data, model, add_residuals),
           glance = map(model, glance),
           tidy   = map(model, tidy))
  
  # Unnested df with data from broom::glance (R2, p-value, etc. for each model).
  # Each row a model.
  datadf_glance <- unnest(datadf_nest, glance) %>% 
    select(-data, -model, -resids, -tidy) %>% 
    clean_names()
  
  # # Unnested df with data from broom::add_residuals. 
  # # Each row a timepoint for a model.
  # abtsdata_resids <- unnest(abtsdata_nest, resids, .drop = TRUE) %>% 
  #   select(expt_id, time_m, resid)
  
  # Unnested df with data from broom::tidy (only the slope and y-intercept in this case).
  # Each row a model.
  # Also grabbing wavelength and temperature here.
  datadf_terms <- unnest(datadf_nest, tidy) %>% 
    select(well_id, model_name, term, estimate) %>% 
    pivot_wider(id_cols = c(well_id, model_name), names_from = term, values_from = estimate) %>% 
    clean_names() %>% 
    rename(y_intercept = intercept, slope = time_m)
  

  # Join together assay data. Each row an experiment, with specific activities.
  # Relative activity computed relative to soluble enzyme as well.
  # Specific activity less than zero is set to 0.
  datadf_allmodels <- exptdf %>% 
    left_join(datadf_terms) %>% 
    left_join(datadf_glance) %>%
    left_join(datadf_max) %>% 
    left_join(datadf_cond) %>% 
    filter(!is.na(model_name)) %>%
    left_join(readout_df) %>%
    mutate(
      sm_equivalent_conc = substrate_conc_m_m * equivalents_of_sm,
      enzyme_assay_conc    = enzyme_stock_conc_mg_m_l * assay_dilution,
      enzyme_assay_mg      = enzyme_assay_conc * assay_enzyme_volume_m_l,
      assay_enzstock_vol   = assay_enzyme_volume_m_l * assay_dilution,
      specific_activity_mg = slope / (readout_stoichiometry * ext_coeff * path_length * enzyme_assay_mg / assay_vol),
      specific_activity_mg = case_when(specific_activity_mg > 0 ~ specific_activity_mg,
                                       specific_activity_mg <= 0 ~ 0),
      specific_activity_ml = slope / (readout_stoichiometry * ext_coeff * path_length * assay_enzstock_vol / assay_vol),
      specific_activity_ml = case_when(specific_activity_ml > 0 ~ specific_activity_ml,
                                       specific_activity_ml <= 0 ~ 0),
      model_quality        = case_when(r_squared > r2_thresh & slope > slope_thresh ~ "good model",
                                       TRUE ~ "bad model"),
      # Assign specific activity to 0 for all bad models.
      adjusted_activity_ml = case_when(model_quality == "good model" ~ specific_activity_ml,
                                       TRUE ~ 0),
      adjusted_activity_mg = case_when(model_quality == "good model" ~ specific_activity_mg,
                                       TRUE ~ 0)
      ) %>% 
    # Create columns to rank models for each combo of conditions, 
    # EXCEPT FOR DILUTION, i.e. all replicates and dilutions grouped.
    group_by(expt_id_alldil) %>%
    mutate(activity_rank = rank(desc(specific_activity_ml), ties.method = "first"),
           adjusted_activity_rank = rank(desc(adjusted_activity_ml), ties.method = "first")) %>%
    ungroup() %>%

    # Create columns to rank models for each well.
    group_by(well_id) %>% 
    mutate(well_activity_rank = rank(desc(specific_activity_ml), ties.method = "first"),
           well_adjusted_activity_rank = rank(desc(adjusted_activity_ml), ties.method = "first")) %>%
    ungroup()

  
  # Dataframe of positive control reactions !! FOR EACH NOTEBOOK/SUBSTRATE COMBINATION !!
  # For computing RELATIVE activities of different enzymes to a positive control *on that plate*
  # e.g. for activity of mutants in a library relative to the parent enzyme
  datadf_parent_activity <- datadf_allmodels %>% 
    filter(expt_type == "positive", adjusted_activity_rank == 1) %>% 
    group_by(notebook, substrate) %>% 
    summarize(parent_activity_mass = mean(adjusted_activity_mg),
              parent_activity_vol  = mean(adjusted_activity_ml)) %>% 
    ungroup()
  
  # Dataframe of positive control reactions  !! FOR EACH DATE/ENZYME/SUBSTRATE COMBINATION !!
  # For computing COMPARATIVE activities of each enzymes *under differing conditions (temp, pH)
  datadf_positive_activity <- datadf_allmodels %>% 
    filter(expt_type == "positive", adjusted_activity_rank == 1) %>% 
    group_by(date, enzyme, substrate, substrate_conc_m_m) %>% 
    summarize(positive_activity_mass = mean(adjusted_activity_mg),
              positive_activity_vol  = mean(adjusted_activity_ml)) %>% 
    ungroup()
  
  # Dataframe of starting material reactions.
  # For computing selectivity factors, e.g. ratio of glyceraldehyde activity to glycerol activity.
  # Note that conditions like pH and preheat temp aren't considered here.
  # equivalents_of_sm for adjusting for substrates with multiple oxidation sites
  # e.g. glycolaldehyde dimer
  datadf_selectivity <- datadf_allmodels %>% 
    filter(compound_role == "starting material", adjusted_activity_rank == 1) %>% 
    group_by(date, enzyme, compound_series, substrate, sm_equivalent_conc) %>% 
    summarize(sm_activity_mass = mean(adjusted_activity_mg),
              sm_activity_vol = mean(adjusted_activity_ml)) %>% 
    ungroup() %>% 
    select(-substrate)
  
  # Add relative/comparative activities and selectivity.
  datadf_allmodels <- datadf_allmodels %>%
    left_join(datadf_parent_activity) %>%
    left_join(datadf_positive_activity) %>% 
    left_join(datadf_selectivity) %>% 
    mutate(relative_activity_mass =  adjusted_activity_mg  / parent_activity_mass,
           relative_activity_vol  =  adjusted_activity_ml  / parent_activity_vol,
           compare_activity_mass  =  adjusted_activity_mg  / positive_activity_mass,
           compare_activity_vol   =  adjusted_activity_ml  / positive_activity_vol,
           selectivity_mass       =  adjusted_activity_mg  / sm_activity_mass,
           selectivity_vol        =  adjusted_activity_ml  / sm_activity_vol) %>% 
    mutate(
      selectivity_mass = case_when(
        is.infinite(selectivity_mass) ~ 1000,
        TRUE ~ selectivity_mass),
      selectivity_vol = case_when(
        is.infinite(selectivity_vol) ~ 1000,
        TRUE ~ selectivity_vol))
  
  # Dataframe of averaged replicates.
  datadf_wellmodels <- datadf_allmodels %>% 
    filter(well_adjusted_activity_rank == 1) %>% 
    group_by(expt_id) %>% 
    mutate(n_replicates = n(),
           activity_ml_mean = mean(adjusted_activity_ml),
           activity_ml_sd = sd(adjusted_activity_ml),
           activity_mg_mean = mean(adjusted_activity_mg),
           activity_mg_sd = sd(adjusted_activity_mg))

  # Selects a single model for each notebook/enzyme/substrate combination.
  datadf_modelsummary <- datadf_allmodels %>% 
    filter(adjusted_activity_rank == 1) %>% 
    left_join(datadf_wellmodels)
  
  model_list <- list(
    "datadf_allmodels" = datadf_allmodels,
    "datadf_modelsummary" = datadf_modelsummary,
    "datadf_wellmodels" = datadf_wellmodels
  )
  
  return(model_list)

}
