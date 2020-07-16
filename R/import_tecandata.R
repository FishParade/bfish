# Function for importing Tecan plate reader .xlsx files matching a particular filename suffix, e.g. "_test-assay.xlsx"
import_tecandata <- function(tecan_path = "./rawdata/uv-vis/", tecan_pattern = ".xlsx") {
  
    file_list <- list.files(path = tecan_path, pattern = str_c("*", tecan_pattern))
    assay_rawdf <- list()
    assay_timelist <- list()
    assay_data <- list()
    
  for (i in seq_along(file_list)) {

    # Find the row at which tabular data beings before data import.
    assay_rawdf[[i]] <- read_xlsx(str_glue("{tecan_path}{file_list[[i]]}"), sheet = "Sheet2") %>% 
      clean_names()
    data_skip <- grep("Cycle Nr.", assay_rawdf[[i]]$application_tecan_i_control)
    
    # Find the wavelength of the dataset.
    assay_hv <- grep("Measurement Wavelength", assay_rawdf[[i]]$application_tecan_i_control)
    assay_hv <- assay_rawdf[[i]][[assay_hv, 5]] %>% as.numeric()
  
    # Find the time of the measurement.
    assay_time <- grep("Start Time:", assay_rawdf[[i]]$application_tecan_i_control)
    assay_time <- lubridate::mdy_hms(assay_rawdf[[i]][[assay_time, 2]])
    
    assay_rawdf[[i]] <- read_xlsx(str_glue("{tecan_path}{file_list[[i]]}"), sheet = "Sheet2", skip = data_skip, guess_max = 200) %>% 
      clean_names() %>%
      mutate_if(is.numeric, as.character) %>%
      drop_na()
    
    # Df of expt data (time, temp) with cleaned
    assay_timelist[[i]] <- assay_rawdf[[i]][1:2,] %>%
      rename(expt_var = cycle_nr) %>% 
      pivot_longer(-expt_var) %>% 
      mutate(value = as.numeric(value))
  
    # Data without time, temp
    assay_data[[i]] <- assay_rawdf[[i]] %>%
      slice(-1,-2) %>%
      pivot_longer(-cycle_nr) %>%
      rename(well = cycle_nr, abs = value) %>%
      left_join(assay_timelist[[i]]) %>%
      pivot_wider(names_from = expt_var) %>%
      select(-name) %>%
      clean_names() %>%
      mutate(notebook = str_replace(file_list[[i]], tecan_pattern, ""),
             abs = as.numeric(case_when(abs == "OVER" ~ "4",
                                        TRUE ~ abs)),
             time_s = round(as.numeric(time_s)),
             time_m = round(time_s / 60),
             temp_c = as.numeric(temp_c),
             wavelength = assay_hv,
             assay_datetime = assay_time)
  }
    
    # Bind rows of the data list
    assay_data %>% 
      bind_rows()
}
