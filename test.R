
# Load Required Libraries

library(readxl)
library(tibble)
library(dplyr)
library(formattable)
library(stringr)
library(htmltools)
library(purrr)
library(sf)
library(terra)
library(rnaturalearth)
library(geodata)


# Load Functions

source("setup_functions.R")


# Load Data


file_path <- "test_that/404_Do_2008.xlsx"
file_path <- "test_that/404_Do_2008_test_2.xlsx"
file_path <- "test_that/test_missing.xlsx"
file_path <- 'test_that/1527_Silva_2004_20250513.xlsx'
file_path <- 'test_that/25_Alarcón_2000.xlsx'
file_path <- 'test_that/146_Bleby_2012.xlsx'
file_path <- 'TO_DO/Victor/410_Dodd_1993_vf.xlsx'
df <- read_excel(file_path, 
                 sheet = "data", na = "NA")

# file_path <- params$file_path
# file_name <- tools::file_path_sans_ext(basename(file_path))
# df <- read_excel(file_path, sheet = "data", na = "NA")


# Contributor Info

contributors <- unique(na.omit(df$Contributor))
contributor_text <- if (length(contributors) == 0) {
  "Not specified"
} else {
  paste(contributors, collapse = ", ")
}

n_IDref <- length(unique(na.omit(df$IDref)))


# Type and Range Checks

qc_results <- list()

for (col in names(expected_types)) {
  if (!col %in% names(df)) {
    qc_results[[col]] <- tibble(Column = col, TypeCheck = "Missing", RangeCheck = NA)
    next
  }
  
  actual_col <- df[[col]]
  if (all(is.na(actual_col) | actual_col == "")) {
    qc_results[[col]] <- tibble(Column = col, TypeCheck = NA, RangeCheck = NA)
    next
  }
  
  expected_type <- expected_types[[col]]
  type_flag <- class(actual_col)[1]
  
  if (expected_type == "numeric") {
    if (is.numeric(actual_col)) {
      type_ok <- TRUE
    } else if (is.character(actual_col)) {
      converted <- suppressWarnings(as.numeric(actual_col))
      type_ok <- all(is.na(actual_col) | !any(is.na(converted[!is.na(actual_col)])))
      if (type_ok) type_flag <- "numeric (converted from character)"
    } else {
      type_ok <- FALSE
    }
  } else {
    type_ok <- is.character(actual_col)
  }
  
  range_flag <- NA
  if (expected_type == "numeric") {
    r <- valid_ranges[[col]]
    out_of_range <- sum(actual_col < r[1] | actual_col > r[2], na.rm = TRUE)
    range_flag <- ifelse(out_of_range == 0, "OK", paste(out_of_range, "out of range"))
  }
  
  qc_results[[col]] <- tibble(
    Column = col,
    TypeCheck = ifelse(type_ok, "OK", paste("Expected", expected_type, "got", type_flag)),
    RangeCheck = range_flag
  )
}

qc_summary <- bind_rows(qc_results)
# View(qc_summary)

# Date Format Check

if ("Date" %in% names(df)) {
  date_status_vec <- check_date_format(df$Date)
  if (all(date_status_vec == "OK")) {
    date_status <- "OK"
  } else if (all(date_status_vec != "OK")) {
    date_status <- "All invalid or missing date formats"
  } else {
    date_status <- "Some invalid or missing date formats"
  }
  
  if ("Date" %in% qc_summary$Column) {
    qc_summary <- qc_summary %>%
      mutate(
        TypeCheck = ifelse(Column == "Date", date_status, TypeCheck),
        RangeCheck = ifelse(Column == "Date", NA, RangeCheck)
      )
  } else {
    qc_summary <- bind_rows(
      qc_summary,
      tibble(Column = "Date", TypeCheck = date_status, RangeCheck = NA)
    )
  }
}


# Country Code Validation

result <- qc_check_country_codes(df)


# Unit Transformation Check

if ("Kwp" %in% names(df)) {
  transformation_results <- sapply(1:nrow(df), function(i) {
    row_df <- df[i, c("Kwp", "Level", "Kwp_original_units",
                      "Kwp_cor_Leaf", "Kwp_cor_sapwood",
                      "Kwp_cor_plant", "Kwp_cor_wood",
                      "Kwp_cor_ground")]
    check_transformation(row_df)
  })
  
  passed <- sum(transformation_results == TRUE)
  total <- length(transformation_results)
  transformation_check <- paste0(passed, "/", total, " passed")
  
  failed_rows <- which(transformation_results != TRUE)
} else {
  transformation_check <- "Kwp column not found."
}




# Available Transformations

valid_levels <- c("Leaf", "Sapwood", "Ground", "Plant", "Wood")
invalid_levels <- df %>%
  filter(
    is.na(Level) |
      !(Level %in% valid_levels) |
      (Level == "Plant" & !(Kwp_original_units %in% valid_units))
  ) %>%
  distinct(Level, Kwp_original_units)

if (nrow(invalid_levels) > 0) {
  htmltools::HTML("<h3 style='color: red;'>❌ Invalid values in 'Level' column</h3>") |> print()
  htmltools::HTML("<p>The following values are not recognized and must be corrected before transformation checks:</p>") |> print()
  formattable(invalid_levels)
} else {
  # If valid, proceed with transformation detection
  transformation <- as.data.frame(
    t(apply(df, 1, function(row) check_Kwp_transformations(as.list(row))))
  )
}
safe_check <- function(i) {
  tryCatch(
    check_Kwp_transformations_correctness(as.list(df[i, ])),
    error = function(e) {
      message(paste("Error in row", i, ":", e$message))
      setNames(as.list(rep(NA, 5)), paste0("Kwp_cor_", c("Leaf", "sapwood", "plant", "wood", "ground")))
    }
  )
}
results_table <- purrr::map_dfr(1:nrow(df), safe_check)


# Report Sections

htmltools::HTML(sprintf(
  '<div style="margin-bottom: 20px;">
     <p><strong>Data Quality controlled by:</strong> %s</p>
     <p><strong>Data contributor(s):</strong> %s</p>
     <p><strong>Unique IDref:</strong> %d</p>
   </div>',
  params$researcher,
  contributor_text,
  n_IDref
))

# Type and Range Check
htmltools::HTML("<h3>Data Type and Range Check</h3>")
htmltools::HTML("<h5>This table verifies that each column matches the expected data type and value range.</h5>")
color_formatter <- formatter("span", style = x ~ ifelse(is.na(x), "color: grey", ifelse(x != "OK", "color: red", "color: black")))
formattable(qc_summary, list(TypeCheck = color_formatter, RangeCheck = color_formatter))

# Country Code Validation
htmltools::HTML("<h3>🌍 Country Code Validation</h3>")
formattable(result$summary_table, list(
  is_valid_country = formatter("span", style = x ~ style(
    color = ifelse(x, "green", "red"),
    font.weight = "bold"
  ))
))

# Coordinate Consistency Check
htmltools::HTML("<h3>📍 Coordinate Consistency Check</h3>")
htmltools::HTML("<h5>Checks whether site coordinates fall within declared country boundaries and are on land.</h5>")
df_checked <- qc_check_coordinates_simple(df, maps_folder = "maps")
n_total <- nrow(df_checked)
n_wrong <- ifelse(all(is.na(df_checked$is_inside_country)), NA, sum(!df_checked$is_inside_country, na.rm = TRUE))
n_water <- ifelse(all(is.na(df_checked$is_on_land)), NA, sum(!df_checked$is_on_land, na.rm = TRUE))

htmltools::HTML(sprintf("<ul><li><strong>Total sites checked:</strong> %d</li><li><strong>❌ Wrong coordinates:</strong> %d</li><li><strong>🌊 Over water:</strong> %d</li></ul>", n_total, n_wrong, n_water))

bad_coords <- df_checked %>% mutate(row = row_number()) %>% filter(!is_inside_country | !is_on_land) %>% relocate(row, .before = si_country)
if(all(is.na(df_checked$si_lat)) || all(is.na(df_checked$si_long))) {
  bad_coords <- df_checked %>%
    mutate(row = row_number(), si_country = NA, IDref = NA, si_lat = NA, si_long = NA, is_inside_country = NA, is_on_land = NA) %>%
    relocate(row, .before = si_country)
}

if (nrow(bad_coords) > 0) {
  htmltools::HTML(sprintf("<h4>❌ %d site(s) have invalid coordinates</h4>", nrow(bad_coords)))
  formattable(
    bad_coords %>% dplyr::select(row, si_country, IDref, si_lat, si_long, is_inside_country, is_on_land),
    list(
      is_inside_country = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", ifelse(x, "green", "red")), font.weight = "bold")),
      is_on_land = formatter("span", style = x ~ style(color = ifelse(is.na(x), "grey", ifelse(x, "green", "red")), font.weight = "bold"))
    )
  )
} else {
  htmltools::HTML("<p>✅ All site coordinates are valid and located on land.</p>")
}


# Transformation Summary

summary_html <- ""

summary_html <- paste0(summary_html, "<h3>Original Level Units Transformation Check Summary</h3>")
summary_html <- paste0(summary_html, "<h5>Checks whether original measurements were correctly transformed into standard units.</h5>")

if (grepl("\\d+/\\d+", transformation_check)) {
  ratio <- regmatches(transformation_check, regexpr("\\d+/\\d+", transformation_check))
  parts <- strsplit(ratio, "/")[[1]]
  passed <- as.numeric(parts[1])
  total <- as.numeric(parts[2])
  
  if (total == 0) {
    bg_color <- "#e2e3e5"
    display_text <- "⚠️ No data available to evaluate transformations"
    details_html <- ""
  } else {
    percent_passed <- passed / total
    bg_color <- if (percent_passed == 1) {
      "#d4edda"
    } else if (percent_passed > 0.8) {
      "#fff3cd"
    } else {
      "#f8d7da"
    }
    
    display_text <- paste0("✅ Transformation Check: ", transformation_check)
    
    error_messages <- transformation_results[transformation_results != TRUE]
    if (length(error_messages) > 0) {
      error_table <- as.data.frame(table(as.character(error_messages)))
      colnames(error_table) <- c("Message", "Count")
      
      error_lines <- apply(error_table, 1, function(row) {
        msg <- row["Message"]
        count <- as.numeric(row["Count"])
        if (msg == "original units are wrong") {
          if (count == 1) {
            return("<li>There is <b>1 row</b> with <i>original units that are wrong</i>.</li>")
          } else {
            return(sprintf("<li>There are <b>%d rows</b> with <i>original units that are wrong</i>.</li>", count))
          }
        } else if (msg == "Kwp is not numeric or is NA") {
          if (count == 1) {
            return("<li>There is <b>1 row</b> where <i>Kwp is not numeric or is NA</i>.</li>")
          } else {
            return(sprintf("<li>There are <b>%d rows</b> where <i>Kwp is not numeric or is NA</i>.</li>", count))
          }
        } else if (msg == "Level is not one of the expected levels") {
          if (count == 1) {
            return("<li>There is <b>1 row</b> with <i>unexpected Level value</i>.</li>")
          } else {
            return(sprintf("<li>There are <b>%d rows</b> with <i>unexpected Level values</i>.</li>", count))
          }
        } else {
          if (count == 1) {
            return(sprintf("<li>There is <b>1 row</b> with message: <i>%s</i>.</li>", msg))
          } else {
            return(sprintf("<li>There are <b>%d rows</b> with message: <i>%s</i>.</li>", count, msg))
          }
        }
      })
      
      details_html <- paste0("<ul style='margin-left:1em;'>", paste(error_lines, collapse = ""), "</ul>")
    } else {
      details_html <- ""
    }
  }
  
  summary_html <- paste0(summary_html, sprintf(
    '<h3 style="background-color:%s; padding:8px 12px; border-radius:6px; font-size:1.17em; font-weight:bold; margin-top:1em;">%s</h3>%s',
    bg_color, display_text, details_html
  ))
  
} else {
  summary_html <- paste0(summary_html, sprintf(
    '<h3 style="background-color:#e2e3e5; padding:8px 12px; border-radius:6px; font-size:1.17em; font-weight:bold; margin-top:1em;">⚠️ %s</h3>',
    transformation_check
  ))
}

cat(summary_html)


#  Kwp Consistency Check (Kwp = WaterFlux / deltaWP)

htmltools::HTML("<h3>Kwp Consistency Check</h3>")
htmltools::HTML("<h5>Checks whether the reported Kwp equals WaterFlux divided by deltaWP.</h5>")

# Check that all required columns exist
required_cols <- c("Kwp", "WaterFlux", "deltaWP")
missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  message_text <- sprintf("⚠️ Missing column(s): %s. Cannot perform the consistency check.", 
                          paste(missing_cols, collapse = ", "))
  bg_color <- "#e2e3e5"
} else {
  # All required columns present
  df_check <- df %>%
    mutate(
      Kwp_calc = WaterFlux / deltaWP,
      Kwp_check = ifelse(is.na(Kwp) | is.na(Kwp_calc), NA, abs(Kwp - Kwp_calc) < 1e-6)
    )
  
  n_valid <- sum(!is.na(df_check$Kwp_check))
  n_equal <- sum(df_check$Kwp_check == TRUE, na.rm = TRUE)
  
  if (n_valid == 0) {
    message_text <- "⚠️ Could not calculate consistency for any row due to NA values in WaterFlux or deltaWP."
    bg_color <- "#e2e3e5"
  } else if (n_equal == n_valid) {
    message_text <- sprintf("✅ All %d rows with valid values passed the check: Kwp = WaterFlux / deltaWP.", n_valid)
    bg_color <- "#d4edda"
  } else {
    n_failed <- n_valid - n_equal
    message_text <- sprintf("❌ %d of %d rows failed the check: Kwp ≠ WaterFlux / deltaWP.", n_failed, n_valid)
    bg_color <- "#f8d7da"
  }
}




# Available Transformations Table
htmltools::HTML("<h3>Available Transformations</h3>")
htmltools::HTML("<h5>Shows which transformations were possible based on available data.</h5>")

text_color_formatter <- formatter("span", style = x ~ ifelse(is.na(x) | x == "ORIGINAL", "color: grey", ifelse(x != "OK", "color: red", "color: green")))

if (ncol(transformation) > 0) {
  formattable(transformation,
              lapply(transformation, function(x) text_color_formatter))
} else {
  htmltools::HTML("<p><i>No transformation results available.</i></p>")
}

# Correctness of Transformations
htmltools::HTML("<h3>Correct Transformations</h3>")
htmltools::HTML("<h5>Validates correctness of derived transformations vs. original values and scaling.</h5>")

color_formatter2 <- formatter("span", style = x ~ ifelse(is.na(x) | x == "ORIGINAL", "color: grey", ifelse(x != "TRUE", "color: red", "color: green")))
formattable(results_table, lapply(results_table, function(x) color_formatter2))

