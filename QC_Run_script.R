# ------------------------------------------------------------------------------
# Data Quality Report Runner
# This script generates a detailed quality control report for a dataset
# formatted according to a predefined structure. It outputs an HTML file
# in the /reports folder.
# ------------------------------------------------------------------------------
# install.packages("rmarkdown")
# install.packages("sass")
# install.packages("writexl")
# install.packages("geodata")
library(rmarkdown)
library(readxl)
library(openxlsx)
source("setup_functions.R")

# ------------------------------------------------------------------------------
# STEP 1: Define input Excel file to be tested and adjust columns names
# Only ONE should be uncommented or used at a time
# ------------------------------------------------------------------------------

file_path <- 'TO_DO/3000_SAPFLUXNET.xlsx'




#### RUN update excel file to change to the new template and overwrite it ####
# update_excel_file("Kwp_database_template_14_05_2025.xlsx",
# file_path, file_path, desired_cols = desired_cols)

# ------------------------------------------------------------------------------
# STEP 2: Define output location and file naming
# The output filename will include the original file name and timestamp
# ------------------------------------------------------------------------------

output_dir <- "reports"
file_name <- tools::file_path_sans_ext(basename(file_path))  # Extract base name
timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M")              # Add time

output_file <- file.path(output_dir, paste0(file_name, "_report_", timestamp, ".html"))

# ------------------------------------------------------------------------------
# STEP 3: Create reports directory if it doesn't exist
# ------------------------------------------------------------------------------

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# ------------------------------------------------------------------------------
# STEP 4: Check whether report should be generated
# ------------------------------------------------------------------------------

df_test <- read_excel(file_path, sheet = "data", na = "NA")
included_col <- "Included" %in% names(df_test)
included_values <- if (included_col) tolower(trimws(na.omit(df_test$Included))) else character(0)
should_include <- !(included_col && length(included_values) > 0 && all(included_values == "no"))


# ------------------------------------------------------------------------------
# 🧪 STEP 5: Render the appropriate report
# ------------------------------------------------------------------------------

if (!should_include) {
  message("🚫 Dataset marked as 'not included'. A message-only report will be generated.")
  
  render(
    input = "report_exclusion_message.Rmd",
    output_file = output_file,
    params = list(
      file_name = file_name,
      report_date = timestamp
    ),
    envir = new.env()
  )
} else {
  render(
    input = "Data_Quality_Report.Rmd",
    output_file = output_file,
    params = list(
      file_path = file_path,
      file_name = file_name,
      report_date = timestamp,
      researcher = "Victor Flo"
    ),
    envir = new.env()
  )
}

# ------------------------------------------------------------------------------
#  STEP 6: Automatically open the report in the browser
# ------------------------------------------------------------------------------

browseURL(output_file)


