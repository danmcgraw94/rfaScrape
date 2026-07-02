#' @importFrom rlang .data
NULL

#' Get Data Levels
#'
#' Identifies the top-level data category tables in an RFA sqlite project.
#'
#' @param con The database (sqlite) connection object.
#'
#' @returns A vector of the data levels from the RFA project. Most projects
#' will be `c("1.0_ProjectData", "2.0_InputData", "3.0_Analyses",
#' "4.0_ReservoirModels", "5.0_Simulation")`
#'
#' @examples
#' \dontrun{
#'   con <- DBI::dbConnect(RSQLite::SQLite(), "path/to/file.rfa.sqlite")
#'   data_levels <- get_data_levels(con)
#' }
get_data_levels <- function(con) {
  tabs = DBI::dbListTables(con)
  top = tabs[grepl("^\\d+\\.0_", tabs)]
  top
}

#' Get Project Name
#'
#' Reads the RFA project name from the project data table, used to name the
#' export directory.
#'
#' @param con The database (sqlite) connection object.
#' @param data_levels The data levels string returned from `get_data_levels()`
#'
#' @returns The RFA project name. This will be used to create the export directory
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), "path/to/file.rfa.sqlite")
#' project_name <- get_project_name(con)
#' }
get_project_name <- function(con, data_levels) {
  project_info <- DBI::dbReadTable(con, data_levels[1])
  project_info$Name
}

#' Make Project Folder
#'
#' Creates the top-level export directory for a given RFA project.
#'
#' @param project_name The project name from `get_project_name()`
#' @param base_dir The export directory defined by the user. Default is the
#' current working directory (from `getwd()`)
#'
#' @returns The top level directory for the export process
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSQLite::SQLite(), "path/to/file.rfa.sqlite")
#' project_name <- get_project_name(con)
#' project_folder <- make_project_folder(project_name)
#' }
make_project_folder <- function(project_name, base_dir = getwd()) {
  folder_path <- file.path(base_dir, project_name)
  dir.create(folder_path, showWarnings = FALSE, recursive = TRUE)
  folder_path
}

#' Export RFA input data
#'
#' Exports discharge gage (DG), inflow hydrograph (IH), stage gage (SG), and
#' VFC input data tables to CSV files, organized by data type.
#'
#' @param con The database (sqlite) connection object.
#' @param project_dir The top-level project folder from `make_project_folder()`
#'
#' @returns The directory storing RFA input data
#'
#' @examples
#'\dontrun{
#' export_2inputdata(con, project_folder)
#' }
export_2inputdata <- function(con, project_dir) {
  # Create input data directory inside project dir
  input_dir <- file.path(project_dir, "input_data")
  dir.create(input_dir, showWarnings = FALSE)

  # Select the input data - tables with 2.2
  tables <- DBI::dbListTables(con)
  input_data_tables <- tables[grepl("^2\\.2_", tables)]

  # Summary of Input Data
  input_summary <- DBI::dbReadTable(
    con,
    tables[grepl("^2\\.0", tables)]
  )
  utils::write.csv(
    input_summary,
    file.path(input_dir, "input_data_summary.csv"),
    row.names = FALSE
  )

  for (tbl in input_data_tables) {
    # pull type code (DG, IH, SG, VFC) and the rest as the file name
    type_code <- sub("^2\\.2_([A-Z]+)_.*", "\\1", tbl)
    file_name <- sub("^2\\.2_[A-Z]+_(.*)", "\\1", tbl)

    # Set up sub dirs based on type code
    dir_name <- if (type_code == "VFC") "rfa_vfc" else tolower(type_code)
    sub_dir <- file.path(input_dir, dir_name)
    dir.create(sub_dir, showWarnings = FALSE)

    df <- DBI::dbReadTable(con, tbl)

    utils::write.csv(
      df,
      file.path(sub_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }
  input_dir
}

#' Export a summary of RFA VFC parameters
#'
#' Compiles LP3 distribution parameters (mean, standard deviation, skew) and
#' equivalent record length across all VFCs into a single summary CSV.
#'
#' @param con The database (sqlite) connection object.
#' @param project_dir The top-level project folder from `make_project_folder()`
#'
#' @returns The file path of the RFA VFC parameter input summary
#'
#' @examples
#'\dontrun{
#' export_vfc_parameters(con, project_folder)
#' }
export_vfc_parameters <- function(con, project_dir) {
  # Create vfc directory inside project_dir/input_data
  # and/or ensure it's there
  vfc_dir <- file.path(project_dir, "input_data", "rfa_vfc")
  dir.create(vfc_dir, showWarnings = FALSE)

  # Select the vfc data - vfc tables with 2.1
  tables <- DBI::dbListTables(con)
  vfc_param_tables <- tables[grepl("^2\\.1_VFC", tables)]

  # Create List for LP3 parameters & ERL
  lp3_summary <- lapply(vfc_param_tables, function(tbl) {
    vfc_params <- DBI::dbReadTable(con, tbl)
    vfc_name = sub("^2\\.1_", "", tbl)

    # Parameter Summary
    data.frame(
      vfc = vfc_name,
      dist = vfc_params$Distribution,
      mean_log = vfc_params$Mean,
      sd_log = vfc_params$StDev,
      skew_log = vfc_params$Skew,
      erl = vfc_params$EYR,
      duration_days = vfc_params$Duration
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::arrange(.data$erl)
  utils::write.csv(
    lp3_summary,
    file.path(vfc_dir, "vfc_parameters.csv"),
    row.names = FALSE
  )
  vfc_dir
}

#' Export analysis summaries, data, and results
#'
#' Exports empirical frequency (EFC), flow seasonality (FS), and reservoir
#' starting stage (RSSD) analysis setups and results to CSV files.
#'
#' @param con The database (sqlite) connection object.
#' @param project_dir The top-level project folder from `make_project_folder()`
#'
#' @returns The file path of the RFA analysis export directories
#'
#' @examples
#'\dontrun{
#' export_3analyses(con, project_folder)
#' }
export_3analyses <- function(con, project_dir) {
  # Create analyses directory inside project dir
  analyses_dir <- file.path(project_dir, "analyses")
  dir.create(analyses_dir, showWarnings = FALSE)

  # Set up sub folders
  efc_dir <- file.path(analyses_dir, "efc")
  fs_dir <- file.path(analyses_dir, "fs")
  rssd_dir <- file.path(analyses_dir, "rssd")
  dir.create(efc_dir, showWarnings = FALSE)
  dir.create(fs_dir, showWarnings = FALSE)
  dir.create(rssd_dir, showWarnings = FALSE)

  # Grab all analyses tables
  tables <- DBI::dbListTables(con)
  analyses_tables <- tables[grepl("^3\\.", tables)]
  analyses_summary <- DBI::dbReadTable(
    con,
    analyses_tables[grepl("^3\\.0", analyses_tables)]
  )

  # Export summary
  utils::write.csv(
    analyses_summary,
    file.path(analyses_dir, "summary_of_analyses.csv"),
    row.names = FALSE
  )

  # Grab all EFC analyses (3.1 is summary, 3.2 are results)
  efc_3.1 <- analyses_tables[grepl("^3\\.1_EFC", analyses_tables)]
  efc_3.2 <- analyses_tables[grepl("^3\\.2_EFC", analyses_tables)]

  # EFC Summary
  efc_summary <- lapply(efc_3.1, function(tbl) {
    efc_setup <- DBI::dbReadTable(con, tbl)
    if (nrow(efc_setup) == 0) {
      return(NULL)
    }
    efc_name = sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)

    # Parameter Summary
    data.frame(
      efc = efc_name,
      input_data_type = efc_setup$InputDataType,
      input_data_name = efc_setup$InputDataName,
      duration_days = efc_setup$Duration,
      plot_posit_form = efc_setup$PlotPositForm,
      year_start = efc_setup$YearSpecification
    )
  }) |>
    dplyr::bind_rows()
  utils::write.csv(
    efc_summary,
    file.path(efc_dir, "0.empirical_frequency_summary.csv"),
    row.names = FALSE
  )

  # Each empirical freq results
  for (tbl in efc_3.2) {
    file_name <- sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }

    utils::write.csv(
      df,
      file.path(efc_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }

  # Grab all FS analyses (3.1 is summary, 3.2 are results, 3.3 are events)
  fs_3.1 <- analyses_tables[grepl("^3\\.1_FS", analyses_tables)]
  fs_3.2 <- analyses_tables[grepl("^3\\.2_FS", analyses_tables)]
  fs_3.3 <- analyses_tables[grepl("^3\\.3_FS", analyses_tables)]

  # FS Summary
  fs_summary <- lapply(fs_3.1, function(tbl) {
    fs_setup <- DBI::dbReadTable(con, tbl)
    if (nrow(fs_setup) == 0) {
      return(NULL)
    }
    fs_name = sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)

    # Parameter Summary
    data.frame(
      fs = fs_name,
      input_data_name = fs_setup$InputDataName,
      threshold_flow = as.numeric(fs_setup$ThresholdFlow),
      duration_days = as.numeric(fs_setup$Duration),
      max_events_per_year = as.numeric(fs_setup$MaxEventsPerYear),
      min_days_btw_events = as.numeric(fs_setup$MinDaysBtwEvents),
      manual_entry = fs_setup$ManualDataEntry
    )
  }) |>
    dplyr::bind_rows()
  utils::write.csv(
    fs_summary,
    file.path(fs_dir, "0.seasonality_analysis_summary.csv"),
    row.names = FALSE
  )

  # Each Seasonality Result
  for (tbl in fs_3.2) {
    file_name <- sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }

    utils::write.csv(
      df,
      file.path(fs_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }

  # Events from Seasonality Result
  for (tbl in fs_3.3) {
    file_name <- sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }

    utils::write.csv(
      df,
      file.path(fs_dir, paste0(file_name, "_events.csv")),
      row.names = FALSE
    )
  }

  # Grab all RSSD analyses (3.1 is summary, 3.2 is timeseries (dont use), 3.3 is results)
  rssd_3.1 <- analyses_tables[grepl("^3\\.1_RSSD", analyses_tables)]
  #rssd_3.2 <- analyses_tables[grepl("^3\\.2_RSSD", analyses_tables)]
  rssd_3.3 <- analyses_tables[grepl("^3\\.3_RSSD", analyses_tables)]

  # RSSD Summary
  rssd_summary <- lapply(rssd_3.1, function(tbl) {
    rssd_setup <- DBI::dbReadTable(con, tbl)
    if (nrow(rssd_setup) == 0) {
      return(NULL)
    }

    rssd_name = sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)

    # Parameter Summary
    data.frame(
      rssd = rssd_name,
      input_data_type = rssd_setup$InputDataName,
      pool_threshold = rssd_setup$PoolThreshold,
      pool_duration_days = rssd_setup$PoolDuration
    )
  }) |>
    dplyr::bind_rows()
  utils::write.csv(
    rssd_summary,
    file.path(rssd_dir, "0.res_starting_stage_summary.csv"),
    row.names = FALSE
  )

  # Each Starting Stage Result
  for (tbl in rssd_3.3) {
    file_name <- sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }

    utils::write.csv(
      df,
      file.path(rssd_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }
  c(efc_dir, fs_dir, rssd_dir)
}

#' Export Reservoir Models
#'
#' Exports reservoir model configuration tables and a summary of all
#' reservoir models to CSV files.
#'
#' @param con The database (sqlite) connection object.
#' @param project_dir The top-level project folder from `make_project_folder()`
#'
#' @returns The file path of the RFA reservoir model directories
#'
#' @examples
#'\dontrun{
#' export_4resmodel(con, project_folder)
#' }
export_4resmodel <- function(con, project_dir) {
  # Create input data directory inside project dir
  res_model_dir <- file.path(project_dir, "res_models")
  dir.create(res_model_dir, showWarnings = FALSE)

  # Select the input data - tables with 2.2
  tables <- DBI::dbListTables(con)
  res_models <- tables[grepl("^4\\.1_", tables)]

  # Summary of Res models
  resmodel_summary <- DBI::dbReadTable(
    con,
    tables[grepl("^4\\.0", tables)]
  )
  utils::write.csv(
    resmodel_summary,
    file.path(res_model_dir, "0.resmodel_summary.csv"),
    row.names = FALSE
  )

  for (tbl in res_models) {
    file_name <- sub("^\\d+\\.\\d+_[A-Z]+_(.*)", "\\1", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }

    utils::write.csv(
      df,
      file.path(res_model_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }
  res_model_dir
}

#' Export RFA Results
#'
#' Exports simulation setup, hydrograph weights, tabular results, parameter
#' sampling, and stage-frequency results to CSV (or FST for large tables).
#'
#' @param con The database (sqlite) connection object.
#' @param project_dir The top-level project folder from `make_project_folder()`
#'
#' @returns The file path of the RFA reservoir results directories
#'
#' @examples
#'\dontrun{
#' export_5results(con, project_folder)
#' }
export_5results <- function(con, project_dir) {
  # Create analyses directory inside project dir
  sims_dir <- file.path(project_dir, "simulations")
  dir.create(sims_dir, showWarnings = FALSE)

  # Set up sub folders
  sim_sets_sir <- file.path(sims_dir, "simulation_settings")
  ih_wts_dir <- file.path(sims_dir, "hydrograph_weights")
  tabular_results_dir <- file.path(sims_dir, "tabular_results")
  param_dir <- file.path(sims_dir, "parameter_sampling")
  stage_freq_dir <- file.path(sims_dir, "stage_frequency_results")
  peak_dis_dir <- file.path(sims_dir, "peak_discharge_frequency")
  dis_dur_dir <- file.path(sims_dir, "discharge_duration_frequency")

  dir.create(sim_sets_sir, showWarnings = FALSE)
  dir.create(ih_wts_dir, showWarnings = FALSE)
  dir.create(tabular_results_dir, showWarnings = FALSE)
  dir.create(param_dir, showWarnings = FALSE)
  dir.create(stage_freq_dir, showWarnings = FALSE)
  dir.create(peak_dis_dir, showWarnings = FALSE)
  dir.create(dis_dur_dir, showWarnings = FALSE)

  # Grab all results tables
  tables <- DBI::dbListTables(con)
  tables_51 <- tables[grepl("^5\\.1_", tables)]
  tables_52 <- tables[grepl("^5\\.2_", tables)]
  tables_53 <- tables[grepl("^5\\.3_", tables)]
  tables_54 <- tables[grepl("^5\\.4_", tables)]
  tables_55 <- tables[grepl("^5\\.5_", tables)]
  tables_56 <- tables[grepl("^5\\.6_", tables)]
  tables_57 <- tables[grepl("^5\\.7_", tables)]

  results_summary <- DBI::dbReadTable(
    con,
    tables[grepl("^5\\.0", tables)]
  ) |>
    dplyr::arrange(.data$Name)

  # Export summary
  utils::write.csv(
    results_summary,
    file.path(sims_dir, "0.summary_of_simulations.csv"),
    row.names = FALSE
  )

  # 5.1 - Simulation Setup & 5.2 - Hydrograph Weights
  sim_summary <- lapply(tables_51, function(tbl) {
    sim_setup <- DBI::dbReadTable(con, tbl)
    if (nrow(sim_setup) == 0) {
      return(NULL)
    }
    # name
    sim_name = sub("^\\d+\\.\\d+_", "", tbl)

    # Simulation Summary
    data.frame(
      sim = sim_name,
      vfc = sim_setup$VFC,
      fs = sim_setup$FS,
      rssd = sim_setup$RSSD,
      res_model = sim_setup$ReservoirModel,
      method = sim_setup$Method,
      time_window = as.numeric(sim_setup$TimeWindow),
      timestep = sim_setup$TimeStep,
      skip_flow = as.numeric(sim_setup$SkipFlow),
      realizations = as.numeric(sim_setup$Realizations),
      events = as.numeric(sim_setup$Events),
      seed_type = sim_setup$SeedType,
      seed = as.numeric(sim_setup$Seed),
      stage_freq = as.numeric(sim_setup$StageFreq),
      discharge_freq = as.numeric(sim_setup$DischargeFreq),
      discharge_duration_freq = as.numeric(sim_setup$DischargeDurationFreq),
      results_duration_day = as.numeric(sim_setup$Duration),
      uncertainty_limits = sim_setup$UncertaintyLimits
    )
  }) |>
    dplyr::bind_rows()

  # Hydrograph Weights Summary
  ih_weights_summary <- lapply(tables_52, function(tbl) {
    sim_info <- DBI::dbReadTable(con, tbl)
    if (nrow(sim_info) == 0) {
      return(NULL)
    }
    # name
    sim_name = sub("^\\d+\\.\\d+_", "", tbl)

    # Hydro Summary
    #cbind(c(sim_name, sim_name, sim_name, sim_name), sim_info)
    data.frame(
      sim = sim_name,
      hydrograph = paste0("ih_", sim_info$Hydrograph),
      ih_weight = (sim_info$Simulation) * (sim_info$Weight)
      # simulation = as.integer(sim_info$simulation),
      # weight = as.numeric(sim_info$Weight)
    )
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(.data$sim, .data$hydrograph, .data$ih_weight) |>
    tidyr::pivot_wider(names_from = "hydrograph", values_from = "ih_weight")

  full_setup_summary <- dplyr::left_join(
    sim_summary,
    ih_weights_summary,
    by = "sim"
  )

  utils::write.csv(
    full_setup_summary,
    file.path(sims_dir, "1.simulation_setup.csv"),
    row.names = FALSE
  )

  # Tabular Results - 5.3
  for (tbl in tables_53) {
    file_name = sub("^\\d+\\.\\d+_", "", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }

    if (nrow(df) > 100000) {
      fst::write_fst(
        df,
        file.path(tabular_results_dir, paste0(file_name, ".fst"))
      )
    } else {
      utils::write.csv(
        df,
        file.path(tabular_results_dir, paste0(file_name, ".csv")),
        row.names = FALSE
      )
    }
  }

  # VFC Parameter Sampling Results - 5.4
  for (tbl in tables_54) {
    file_name = sub("^\\d+\\.\\d+_", "", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }
    utils::write.csv(
      df,
      file.path(param_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }

  # Stage-Frequency Results - 5.5
  for (tbl in tables_55) {
    file_name = sub("^\\d+\\.\\d+_", "", tbl)
    df <- DBI::dbReadTable(con, tbl)
    if (nrow(df) == 0) {
      next
    }
    utils::write.csv(
      df,
      file.path(stage_freq_dir, paste0(file_name, ".csv")),
      row.names = FALSE
    )
  }
  c(sim_sets_sir, ih_wts_dir, tabular_results_dir, param_dir, stage_freq_dir)
}
