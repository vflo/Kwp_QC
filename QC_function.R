check_spreadsheet <- function(file_path) {
  df <- read_excel(file_path, 
                   sheet = "data")
  qc_results <- list()
  
  for (col in names(expected_types)) {
    if (!col %in% names(df)) {
      qc_results[[col]] <- tibble(
        Column = col,
        TypeCheck = "Missing",
        RangeCheck = NA,
        UnitCheck = NA
      )
      next
    }
    
    actual_col <- df[[col]]
    expected_type <- expected_types[[col]]
    
    # Check type
    type_flag <- class(actual_col)[1]
    type_ok <- if (expected_type == "numeric") is.numeric(actual_col) else is.character(actual_col)
    
    # Check range (if numeric)
    range_flag <- NA
    if (expected_type == "numeric" && col %in% names(valid_ranges)) {
      r <- valid_ranges[[col]]
      out_of_range <- sum(actual_col < r[1] | actual_col > r[2], na.rm = TRUE)
      range_flag <- ifelse(out_of_range == 0, "OK", paste(out_of_range, "out of range"))
    }
    
    # Unit transformation check (only if relevant)
    unit_flag <- NA
    if (str_detect(col, "Variable1")) {
      orig_unit <- df[["Unit_Original"]][1]
      for (norm_col in normalized_cols) {
        if (norm_col %in% names(df)) {
          is_valid <- mapply(check_transformation,
                             original_value = df[[col]],
                             original_unit = orig_unit,
                             transformed_value = df[[norm_col]],
                             MoreArgs = list(target_unit = "g/m3"))
          unit_flag <- ifelse(all(is.na(is_valid) | is_valid), "OK", "Check transformation")
        }
      }
    }
    
    qc_results[[col]] <- tibble(
      Column = col,
      TypeCheck = ifelse(type_ok, "OK", paste("Expected", expected_type, "got", type_flag)),
      RangeCheck = range_flag,
      UnitCheck = unit_flag
    )
  }
  
  bind_rows(qc_results)
}
