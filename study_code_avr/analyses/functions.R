addCombinations <- function(cohort, name){
  cols <- colnames(cohort)[which(grepl("indication_", colnames(cohort)))]
  
  grid <- expand.grid(replicate(length(vars), c(0, 1), simplify = FALSE))
  colnames(grid) <- vars
  combined_grid <- grid[rowSums(grid) >= 2, ]
  logic_strings <- apply(combined_grid, 1, function(row) {
    paste(names(row), "==", row, collapse = " & ")
  })
  
  new_names <- sapply(logic_strings, function(x) {
    parts <- unlist(strsplit(x, " & "))
    active_vars <- parts[grep("== 1", parts)]
    clean_names <- gsub(" == 1", "", active_vars)
    paste(clean_names, collapse = "_")
  })
  new_names <- unname(new_names)
  
  new_names <- paste0("indication_",gsub("indication_|aortic_","", new_names))
  
  for(i in seq_along(logic_strings)) {
    cohort <- cohort |>
      mutate(!!new_names[i] := if_else(!!parse_expr(logic_strings[[i]]), 1L, 0L)) |>
      compute(temporary = FALSE, name = name)
  }
  
  logic_strings <- paste(paste(cols, collapse = " == 0 & "), "== 0")
  new_names <- "indication_no_indication_identified"
  cohort <- cohort |>
    mutate(!!new_names := if_else(!!parse_expr(logic_strings), 1L, 0L)) |>
    compute(temporary = FALSE, name = name)
  
  return(cohort)
}
