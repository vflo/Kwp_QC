# ------------------------------------------------------------------------------
# Automated Data Quality Report Runner
# Iterates over all .xlsx files in TO_DO/, generating one HTML report per
# dataset in /reports. Skips files that already have a report. Stops on the
# first failure and prints the offending filename so it can be fixed and
# the process resumed without re-running successful files.
# ------------------------------------------------------------------------------

library(rmarkdown)
library(readxl)
library(openxlsx)
library(dplyr)

source("setup_functions.R")

# ------------------------------------------------------------------------------
# STEP 1: Configuration
# ------------------------------------------------------------------------------
input_dir  <- "TO_DO"
output_dir <- "reports"

if (!dir.exists(output_dir)) dir.create(output_dir)

# ------------------------------------------------------------------------------
# STEP 2: Discover input files and filter out those already processed
# ------------------------------------------------------------------------------
all_files <- list.files(
  input_dir,
  pattern    = "\\.xlsx$",
  full.names = TRUE
) |>
  (\(x) x[!grepl("^~\\$", basename(x))])()  # drop Excel lock/temp files

pending_files <- all_files |>
  (\(x) {
    base       <- tools::file_path_sans_ext(basename(x))
    out_path   <- file.path(output_dir, paste0(base, "_report.html"))
    x[!file.exists(out_path)]
  })()

message(sprintf(
  "Found %d xlsx file(s); %d pending, %d already reported.",
  length(all_files), length(pending_files), length(all_files) - length(pending_files)
))

# ------------------------------------------------------------------------------
# STEP 3: Process each pending file. Stop on first failure.
# ------------------------------------------------------------------------------
for (file_path in pending_files) {
  
  file_name   <- tools::file_path_sans_ext(basename(file_path))
  output_file <- file.path(output_dir, paste0(file_name, "_report.html"))
  
  message(sprintf("→ Processing: %s", file_name))
  
  tryCatch({
    
    df_test          <- read_excel(file_path, sheet = "data", na = "NA")
    included_col     <- "Included" %in% names(df_test)
    included_values  <- if (included_col) tolower(trimws(na.omit(df_test$Included))) else character(0)
    should_include   <- !(included_col && length(included_values) > 0 && all(included_values == "no"))
    
    if (!should_include) {
      render(
        input       = "report_exclusion_message.Rmd",
        output_file = output_file,
        params      = list(file_name = file_name, report_date = format(Sys.time(), "%Y-%m-%d")),
        envir       = new.env(),
        quiet       = TRUE
      )
    } else {
      render(
        input       = "Data_Quality_Report.Rmd",
        output_file = output_file,
        params      = list(
          file_path   = file_path,
          file_name   = file_name,
          report_date = format(Sys.time(), "%Y-%m-%d"),
          researcher  = "Victor Flo"
        ),
        envir = new.env(),
        quiet = TRUE
      )
    }
    
  }, error = function(e) {
    message("\n========================================")
    message(sprintf("FAILED on file: %s", basename(file_path)))
    message(sprintf("Error: %s", conditionMessage(e)))
    message("========================================")
    stop(sprintf("Stopped at '%s'. Fix it and re-run; processed files will be skipped.",
                 basename(file_path)),
         call. = FALSE)
  })
}

message("All pending datasets processed successfully.")