# Get Standard Population
esp2013 <- readr::read_csv(
here::here("analyses", "EpiStandard", "european_standard_population.csv"),
col_types = list(
AgeGroup = "character",
EuropeanStandardPopulation = "integer")) |>
dplyr::rename("age_group" = AgeGroup,
"pop" = EuropeanStandardPopulation) %>%
dplyr::mutate(age_group = stringr::str_remove(age_group, " years")) %>%
dplyr::mutate(age_group = stringr::str_replace(age_group, "-", " to ")) %>%
dplyr::mutate(age_group = stringr::str_replace(age_group, "plus", " to 150"))

# Merge Age Groups

mergeAgeGroups <- function(refdata,
                           newGroups,
                           event = NULL,
                           age = "age_group",
                           pop = "pop",
                           ageRange = c(0,150),
                           strata = NULL) {
  
  if(isFALSE(is.data.frame(refdata))){
    cli::cli_abort("'refdata' must be a dataframe")
  }
  
  if(isFALSE(is.vector(newGroups))){
    cli::cli_abort("'newGroups' must be a vector")
  }
  
  # Check input columns
  if (!age %in% names(refdata)) {
    cli::cli_abort("Input data must contain {.field age_group}")
  }
  
  if (!pop %in% names(refdata)) {
    cli::cli_abort("Input data must contain {.field pop}")
  }
  
  if(is.null(event) == FALSE){
    if(!all(event %in% names(refdata))){
      cli::cli_abort("Input data must contain {.field event}.")
    }
  }
  
  if(is.null(strata) == FALSE){
    if(!all(strata %in% names(refdata))) {
      cli::cli_abort("'strata' must be a column or columns in 'refdata'")
    }
  }
  
  newRefdata <- refdata |>
    dplyr::mutate(age_low = stringr::str_extract(.data[[age]], "\\d+"),
                  age_high = stringr::str_extract(.data[[age]], "\\d+$")) |>
    dplyr::mutate(
      age_low  = as.integer(.data$age_low),
      age_high = as.integer(.data$age_high)
    )
  
  # Validate parsed bounds
  if (anyNA(newRefdata$age_high) | anyNA(newRefdata$age_low)) {
    bad_vals <- newRefdata[[age]][is.na(newRefdata$age_low) | is.na(newRefdata$age_high)]
    cli::cli_abort(c(
      "Some {.field age_group} values could not be parsed.",
      "x" = "Invalid labels: {.val {bad_vals}}",
      "i" = "Use format that includes lower and upper bound of each age group,
      for example {.val '0-4'} or {.val '0 to 4'}."
    ))
  }
  
  newRefdata <- newRefdata |>
    dplyr::filter(.data$age_low >= ageRange[1],
                  .data$age_high <= ageRange[2])
  
  merged_list <- vector("list", length(newGroups))
  
  for (i in seq_along(newGroups)) {
    start <- as.integer(stringr::str_extract(newGroups[[i]], "\\d+"))
    end   <- as.integer(stringr::str_extract(newGroups[[i]], "\\d+$"))
    
    # Rows fully inside [start, end]
    in_range <- newRefdata[newRefdata$age_low >= start & newRefdata$age_high <= end, , drop = FALSE]
    
    # Check alignment with existing boundaries
    if (nrow(in_range) == 0L ||
        min(in_range$age_low) != start ||
        max(in_range$age_high) != end) {
      cli::cli_abort(c( + "Cannot create custom range {.val {newGroups[[i]]}}.",
                        "x" = "It does not align with existing age group boundaries."
      ))
    }
    
    if(is.null(strata) & is.null(event)){
      
      total_pop <- sum(in_range$pop, na.rm = TRUE)
      
      merged_list[[i]] <- tibble::tibble(
        !!rlang::sym(age) := newGroups[[i]],
        !!rlang::sym(pop) := total_pop,
        stringsAsFactors = FALSE
      )
    } else if(!is.null(strata) & !is.null(event)){
      
      total_pop <- in_range |>
        dplyr::group_by(!!!rlang::syms(strata)) |>
        dplyr::summarise(!!rlang::sym(pop) := sum(.data[[pop]]),
                         !!rlang::sym(event) := sum(.data[[event]]))
      
      merged_list[[i]] <- total_pop |>
        dplyr::mutate(!!rlang::sym(age) := newGroups[i])
      
    } else if(!is.null(strata) & is.null(event)){
      
      total_pop <- in_range |>
        dplyr::group_by(!!!rlang::syms(strata)) |>
        dplyr::summarise(!!rlang::sym(pop) := sum(.data[[pop]]))
      
      merged_list[[i]] <- total_pop |>
        dplyr::mutate(!!rlang::sym(age) := newGroups[i])
      
    }
  }
  
  # If you want a single data frame at the end:
  result <- dplyr::bind_rows(merged_list) |>
    dplyr::select(colnames(refdata))
  
  rownames(result) <- NULL
  
  return(result)
}

## Standardise Rates 

directlyStandardiseRates <- function(data,
                                     event,
                                     denominator,
                                     age = "age_group",
                                     pop = "pop",
                                     strata = NULL,
                                     addMissingGroups = TRUE,
                                     refdata  = standardPopulation("Europe")) {
  
  #validations
  
  if(isFALSE(is.data.frame(data))){
    cli::cli_abort("'data' must be a dataframe")
  }
  
  if(isFALSE(is.data.frame(refdata))){
    cli::cli_abort("'refdata' must be a dataframe")
  }
  
  if(!event %in% names(data)) {
    cli::cli_abort("'event' must be a column in 'data'")
  }
  
  if(!denominator %in% names(data)) {
    cli::cli_abort("'denominator' must be a column in 'data'")
  }
  
  if(!pop %in% names(refdata)) {
    cli::cli_abort("'pop' must be a column in 'refdata'")
  }
  
  if(!age %in% names(refdata) |!age %in% names(data)) {
    cli::cli_abort("'age' must be a column in 'refdata' and 'data'")
  }
  
  if(is.null(strata) == FALSE){
    if(!all(strata %in% names(data))) {
      cli::cli_abort("'strata' must be a column or columns in 'data'")
    }
  }
  
  dataAgeGroups <- unique(data |> dplyr::pull(.data[[age]]))
  refAgeGroups <-  unique(refdata |> dplyr::pull(.data[[age]]))
  notInRef <- dataAgeGroups[!dataAgeGroups %in% unique(refdata |> dplyr::pull(.data[[age]]))]
  notInData <- refAgeGroups[!refAgeGroups %in% unique(data |> dplyr::pull(.data[[age]]))]
  
  if(length(notInRef)/length(dataAgeGroups) == 1 | length(notInData)/length(refAgeGroups) == 1 ) {
    cli::cli_abort("Different age groups used in data and refdata.")
  }
  
  if(isTRUE(addMissingGroups)){
    
    if(is.null(strata)){
      
      if(length(notInRef) > 0){
        new_rows <- data.frame(
          age_group = notInRef,
          population = rep(0, length(notInRef))
        )
        
        new_rows <- new_rows |>
          dplyr::rename(
            !!age := "age_group",
            !!pop := "population"
          )
        
        refdata <- dplyr::rows_append(refdata, new_rows)
      }
      
      if(length(notInData) > 0){
        new_rows <- data.frame(
          age_group = notInData,
          count = rep(0,length(notInData)),
          denom = rep(0,length(notInData))
        )
        
        new_rows <- new_rows |>
          dplyr::rename(
            !!age := "age_group",
            !!event := "count",
            !!denominator := "denom"
          )
        
        data <- dplyr::rows_append(data, new_rows)
      }
    } else if(!is.null(strata)){
      
      strata_values <- data |>
        dplyr::select(!!!rlang::syms(strata))
      
      list_strata <- list()
      
      for (i in seq_len(ncol(strata_values))) {
        strata_vec <- unique(strata_values[[i]])
        
        list_strata[[colnames(strata_values)[i]]] <- strata_vec
      }
      
      strata_table <- tidyr::crossing(!!!list_strata)
      
      if(length(notInRef) > 0){
        new_rows <- data.frame(
          age_group = notInRef,
          population = rep(0, length(notInRef))
        )
        
        new_rows <- new_rows |>
          dplyr::rename(
            !!age := "age_group",
            !!pop := "population"
          )
        
        refdata <- dplyr::rows_append(refdata, new_rows)
        
      }
      
      if(length(notInData) > 0){
        new_rows <- data.frame(
          age_group = notInData,
          count = rep(0,length(notInData)),
          denom = rep(0,length(notInData))
        )
        
        strata_table_data <- strata_table |>
          dplyr::mutate(denom = 0,
                        count = 0)
        
        new_rows <- strata_table_data |>
          dplyr::left_join(new_rows, by = c("denom", "count"))
        
        new_rows <- new_rows |>
          dplyr::rename(
            !!age := "age_group",
            !!event := "count",
            !!denominator := "denom"
          )
        
        data <- dplyr::rows_append(data, new_rows)
      }
      
    }
  }
  
  if(isFALSE(addMissingGroups)){
    
    if(!is.null(strata)){
      cli::cli_warn("Removing age groups that don't appear in both data and refdata")
    }
    
    if(length(notInRef) > 0){
      
      data <- data |>
        dplyr::filter(!.data[[age]] %in% notInRef)
    }
    
    if(length(notInData) > 0){
      
      refdata <- refdata |>
        dplyr::filter(!.data[[age]] %in% notInData)
    }
  }
  
  
  
  ## validate counts
  if(is.null(strata)){
    sum_data <- data |>
      dplyr::summarise(n = sum(.data[[event]], na.rm = TRUE), .groups = "drop")
  } else if(!is.null(strata)){
    sum_data <- data |>
      dplyr::group_by(!!!rlang::syms(strata)) |>
      dplyr::summarise(n = sum(.data[[event]], na.rm = TRUE), .groups = "drop")
  }
  
  
  if(sum(data[event], na.rm = TRUE) < 10){
    cli::cli_warn("Outcome count less than 10 - Standardising not advised.")
  }
  
  if(!is.null(strata) & isFALSE(addMissingGroups)){
    for(i in 1:nrow(sum_data)){
      if(sum_data$n[i] < 10){
        
        excl_strata <- sum_data[strata][i,]
        
        strata_msg <- excl_strata |>
          dplyr::select(dplyr::all_of(strata)) |>
          tidyr::unite("pair", dplyr::all_of(strata), sep = " and ", remove = FALSE) |>
          dplyr::pull(.data$pair)
        
        cli::cli_warn(paste0("Outcome count less than 10 for ", strata_msg, ". Standardisation not advised."))
        
      }
    }
  }
  
  method <- "normal"
  multiplier <- 100000
  sig <- 0.95
  
  
  # function
  
  all_data_st <- data |>
    dplyr::left_join(refdata, by = dplyr::join_by(!!rlang::sym(age)))
  
  if (!is.null(strata)) {
    all_data_st <- all_data_st |>
      dplyr::group_by(!!!rlang::syms(strata))
  }
  
  all_data_st <- all_data_st |>
    dplyr::mutate(n = sum(!!rlang::sym(event)),
                  d = sum(!!rlang::sym(denominator))) |>
    dplyr::mutate(
      cr_rate = .data$n / .data$d,
      cr_var = .data$n / .data$d ^ 2,
      wts = !!rlang::sym(pop) / sum(!!rlang::sym(pop))) |>
    # REMOVE EMPTY AGE GROUPS. This will remove any age groups with outcome or denominator of 0 AFTER calculating the weights.
    # This is mainly to avoid errors after adding missing age groups.
    dplyr::filter(!!rlang::sym(event) != "0",
                  !!rlang::sym(denominator) != "0") |>
    dplyr::mutate(
      st_rate = sum(.data$wts * (!!rlang::sym(event) / !!rlang::sym(denominator))),
      st_var = sum(as.numeric((.data$wts ^ 2) * (
        !!rlang::sym(event) / (!!rlang::sym(denominator)) ^ 2
      )))
    ) |>
    dplyr::distinct(!!!strata, .keep_all = TRUE) |>
    dplyr::select(dplyr::all_of(c(strata, "n", "d", "cr_rate", "cr_var", "st_rate", "st_var")))
  
  if (!is.null(strata)) {
    all_data_st <- all_data_st |>
      dplyr::ungroup()
  }
  
  # Compute Confidence Intervals (CI) according to method. The default is 'normal'
  if (method == "gamma") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = multiplier * .data$cr_rate,
        c_lower = multiplier * stats::qgamma((1 - sig) / 2, shape = .data$cr_rate ^
                                               2 / (.data$cr_var)) / (.data$cr_rate / .data$cr_var),
        c_upper = multiplier * stats::qgamma(1 - ((1 - sig) / 2), shape = 1 + .data$cr_rate ^
                                               2 / (.data$cr_var)) / (.data$cr_rate / .data$cr_var),
        s_rate = multiplier * .data$st_rate,
        s_lower = multiplier * stats::qgamma((1 - sig) / 2, shape = .data$st_rate ^
                                               2 / .data$st_var) / (.data$st_rate / .data$st_var),
        s_upper = multiplier * stats::qgamma(1 - ((1 - sig) / 2), shape = 1 + (.data$st_rate ^
                                                                                 2 / .data$st_var)) / (.data$st_rate / .data$st_var)
      )
    
  } else if (method == "normal") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = multiplier * .data$cr_rate,
        c_lower = multiplier * (.data$cr_rate + stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var)),
        c_upper = multiplier * (.data$cr_rate - stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var)),
        s_rate = multiplier * .data$st_rate,
        s_lower = multiplier * (.data$st_rate + stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var)),
        s_upper = multiplier * (.data$st_rate - stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var))
      )
    
  } else if (method == "lognormal") {
    tmp1 <- all_data_st |>
      dplyr::mutate(
        c_rate = multiplier * .data$cr_rate,
        c_lower = multiplier * exp((
          log(.data$cr_rate) + stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var) /
            (.data$cr_rate)
        )),
        c_upper = multiplier * exp((
          log(.data$cr_rate) - stats::qnorm((1 - sig) / 2) * sqrt(.data$cr_var) /
            (.data$cr_rate)
        )),
        s_rate = multiplier * .data$st_rate,
        s_lower = multiplier * exp((
          log(.data$st_rate) + stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var) /
            (.data$st_rate)
        )),
        s_upper = multiplier * exp((
          log(.data$st_rate) - stats::qnorm((1 - sig) / 2) * sqrt(.data$st_var) /
            (.data$st_rate)
        ))
      )
  } else {
    cli::cli_abort("method must be set as 'normal', 'lognormal', or 'gamma'")
  }
  
  #Clean up and output
  tmp1 <- tmp1 |>
    dplyr::mutate(dplyr::across(c("c_rate", "c_lower", "c_upper", "s_rate", "s_lower", "s_upper"),
                                ~ round(.x, digits = 4)))
  
  c_rate_name <- 'crude_rate'
  c_lower_name <- paste0('crude_rate_', sig*100, 'CI_lower')
  c_upper_name <- paste0('crude_rate_', sig*100, 'CI_upper')
  s_rate_name <- 'standardised_rate'
  s_lower_name <- paste0('standardised_rate_', sig*100, 'CI_lower')
  s_upper_name <- paste0('standardised_rate_', sig*100, 'CI_upper')
  
  tmp1 <- tmp1 |>
    dplyr::select(
      tidyselect::all_of(strata),
      !!event := "n",
      !!denominator := "d",
      !!c_rate_name := "c_rate",
      !!c_lower_name := "c_lower",
      !!c_upper_name := "c_upper",
      !!s_rate_name := "s_rate",
      !!s_lower_name := "s_lower",
      !!s_upper_name := "s_upper") |>
    dplyr::distinct()
}