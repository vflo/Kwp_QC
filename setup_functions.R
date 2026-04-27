library(readxl)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)
library(writexl)

# ----------------------------------------------------------------------------
# Define expected data types for the dataset
# ----------------------------------------------------------------------------
expected_types <- list(
  'IDref' = "numeric",
  'Included' = "character",
  'si_country' = "character",
  'si_lat' = "numeric",
  'si_long' = "numeric",
  'si_altitude' = "numeric",
  'si_site' = "character",  
  'si_treatment' = "character",
  'pl_name' = "character",
  'pl_species' = "character",
  'pl_growth_form' = "character",
  'pl_age' = "numeric",
  'pl_height' = "numeric",
  'pl_basal_area' = "numeric",
  'pl_DBH' = "numeric",
  'pl_LAI' = "numeric",
  'LAI_method' = "character",
  'pl_LA' = "numeric",
  'pl_SA' = "numeric",
  'SA_method' = "character",
  'pl_huber_value' = "numeric",
  'pl_devstage' = "character",
  'pl_experimental_conditions' = "character",
  'st_basal_area' = "numeric",
  'st_density' = "numeric",
  'soil_sand_perc' = "numeric",
  'soil_silt_perc' = "numeric",
  'soil_clay_perc' = "numeric",
  'soil_om_perc' = "numeric",
  'soil_bulk_density' = "numeric",
  'soil_type' = "character",
  'Date' = "character",
  'wp_leaf_midday' = "numeric",
  'wp_leaf_predawn' = "numeric",
  'wp_soil' = "numeric",
  'wp_soil_depth' = "numeric",
  'Standard_error_wp_leaf_midday' = "numeric",
  'Standard_error_wp_leaf_predawn' = "numeric",
  'Standard_error_wp_soil' = "numeric",
  'deltaWP' = "numeric",
  'WaterFlux' = "numeric",
  'Standard_error_WaterFlux' = "numeric",
  'Flux_original_units' = "character",
  'Level' = "character",
  'Aggregation' = "character",
  'N' = "numeric",
  'Kwp' = "numeric",
  'Standard_error_Kwp' = "numeric",
  'Kwp_original_units' = "character",
  'Kwp_cor_Leaf' = "numeric",
  'Kwp_cor_sapwood' = "numeric",
  'Kwp_cor_plant' = "numeric",
  'Kwp_cor_wood' = "numeric",
  'Kwp_cor_ground' = "numeric",
  'Kwp_method' = "character",
  'Flux_method' = "character",
  'SF_method' = "character",
  'gs' = "numeric",
  'E' = "numeric",
  'VPD' = "numeric",
  'CO2' = "numeric",
  'P50' = "character",
  'Pmin' = "character",
  'TLP' = "character",
  'Gsmax' = "character",
  'Ks' = "character",
  'Contributor' = "character",
  'Reference_title' = "character",
  'PaperDOI' = "character",
  'Observations' = "character"
)

desired_cols <- names(expected_types)

# ----------------------------------------------------------------------------
# Define valid ranges for numeric variables
# ----------------------------------------------------------------------------
valid_ranges <- list(
  'IDref' = c(0, Inf),
  'si_lat' = c(-90, 90),
  'si_long' = c(-180, 180),
  'si_altitude' = c(-400, 9000),
  'pl_age' = c(0, 5000),
  'pl_height' = c(0, 120),
  'pl_basal_area' = c(0, 100),
  'pl_DBH' = c(0, 1000),
  'pl_LAI' = c(0, 15),
  'pl_LA' = c(0, 1000),
  'pl_SA' = c(0, 500),
  'pl_huber_value' = c(0, 0.5),
  'st_basal_area' = c(0, 100),
  'st_density' = c(1, 20000),
  'soil_sand_perc' = c(0, 100),
  'soil_silt_perc' = c(0, 100),
  'soil_clay_perc' = c(0, 100),
  'soil_om_perc' = c(0, 100),
  'soil_bulk_density' = c(0.5, 2.5),
  'wp_leaf_midday' = c(-20, 0),
  'wp_leaf_predawn' = c(-7, 0),
  'wp_soil' = c(-10, 0),
  'wp_soil_depth' = c(0, 50),
  'Standard_error_wp_leaf_midday' = c(0, 5),
  'Standard_error_wp_leaf_predawn' = c(0, 5),
  'Standard_error_wp_soil' = c(0, 5),
  'deltaWP' = c(-10, 10),
  'WaterFlux' = c(0, 1000),
  'Standard_error_WaterFlux' = c(0, 100),
  'N' = c(0, 1000),
  'Kwp' = c(0, 10),
  'Standard_error_Kwp' = c(0, 5),
  'Kwp_cor_Leaf' = c(0, 10),
  'Kwp_cor_sapwood' = c(0, 10),
  'Kwp_cor_plant' = c(0, 10),
  'Kwp_cor_wood' = c(0, 10),
  'Kwp_cor_ground' = c(0, 10),
  'gs' = c(0, 2),
  'E' = c(0, 50),
  'VPD' = c(0, 10),
  'CO2' = c(0, 2000)
)

# ----------------------------------------------------------------------------
# Function to check if dates follow the expected formats
# ----------------------------------------------------------------------------
check_date_format <- function(date_vector) {
  pattern_single <- "^\\d{8}$"              # Format YYYYMMDD
  pattern_range  <- "^\\d{8}-\\d{8}$"        # Format YYYYMMDD-YYYYMMDD
  
  sapply(date_vector, function(date_str) {
    if (is.na(date_str) || date_str == "" || date_str == "NA") {
      return("Missing")
    } else if (grepl(pattern_single, date_str)) {
      return("OK")
    } else if (grepl(pattern_range, date_str)) {
      return("OK")
    } else {
      return("Invalid")
    }
  })
}

# ----------------------------------------------------------------------------
# Function to validate ISO3 country codes in 'si_country' column
# ----------------------------------------------------------------------------
qc_check_country_codes <- function(data) {
  if (!"si_country" %in% names(data)) {
    stop("The dataset must contain a 'si_country' column.")
  }
  
  valid_codes <- unique(rnaturalearth::countries110$iso_a3)
  
  summary_table <- tibble(
    si_country = unique(data$si_country),
    is_valid_country = unique(data$si_country) %in% valid_codes
  )
  
  list(summary_table = summary_table)
}

# ----------------------------------------------------------------------------
# Function to check if transformation is valid
# ----------------------------------------------------------------------------
# Lista completa de unidades válidas
valid_units <- c(
  "mmol/m²/s/MPa", "mmol/m2/s/MPa", "mmol m-2 s-1 MPa-1",
  "mol/m²/s/MPa", "mol/m2/s/MPa", "mol m-2 s-1 MPa-1",
  "kg/m²/s/MPa", "kg/m2/s/MPa", "kg m-2 s-1 MPa-1", 
  "l/m²/s/MPa", "l/m2/s/MPa", "l m-2 s-1 MPa-1",
  "g/m²/s/MPa", "g/m2/s/MPa", "g m-2 s-1 MPa-1",
  "mmol/m²/h/MPa", "mmol/m2/h/MPa", "mmol m-2 h-1 MPa-1",
  "mol/m²/h/MPa", "mol/m2/h/MPa", "mol m-2 h-1 MPa-1",
  "kg/m²/h/MPa", "kg/m2/h/MPa", "kg m-2 h-1 MPa-1",
  "l/m²/h/MPa", "l/m2/h/MPa", "l m-2 h-1 MPa-1",
  "g/m²/h/MPa", "g/m2/h/MPa", "g m-2 h-1 MPa-1",
  "mmol/dm²/s/MPa", "mmol/dm2/s/MPa", "mmol dm-2 s-1 MPa-1",
  "mol/dm²/s/MPa", "mol/dm2/s/MPa", "mol dm-2 s-1 MPa-1",
  "kg/dm²/s/MPa", "kg/dm2/s/MPa", "kg dm-2 s-1 MPa-1",
  "l/dm²/s/MPa", "l/dm2/s/MPa", "l dm-2 s-1 MPa-1",
  "g/dm²/s/MPa", "g/dm2/s/MPa", "g dm-2 s-1 MPa-1",
  "mmol/dm²/h/MPa", "mmol/dm2/h/MPa", "mmol dm-2 h-1 MPa-1",
  "mol/dm²/h/MPa", "mol/dm2/h/MPa", "mol dm-2 h-1 MPa-1",
  "kg/dm²/h/MPa", "kg/dm2/h/MPa", "kg dm-2 h-1 MPa-1",
  "l/dm²/h/MPa", "l/dm2/h/MPa", "l dm-2 h-1 MPa-1",
  "g/dm²/h/MPa", "g/dm2/h/MPa", "g dm-2 h-1 MPa-1",
  "mmol/cm²/s/MPa", "mmol/cm2/s/MPa", "mmol cm-2 s-1 MPa-1",
  "mol/cm²/s/MPa", "mol/cm2/s/MPa", "mol cm-2 s-1 MPa-1",
  "kg/cm²/s/MPa", "kg/cm2/s/MPa", "kg cm-2 s-1 MPa-1",
  "l/cm²/s/MPa", "l/cm2/s/MPa", "l cm-2 s-1 MPa-1",
  "g/cm²/s/MPa", "g/cm2/s/MPa", "g cm-2 s-1 MPa-1",
  "mmol/cm²/h/MPa", "mmol/cm2/h/MPa", "mmol cm-2 h-1 MPa-1",
  "mol/cm²/h/MPa", "mol/cm2/h/MPa", "mol cm-2 h-1 MPa-1",
  "kg/cm²/h/MPa", "kg/cm2/h/MPa", "kg cm-2 h-1 MPa-1",
  "l/cm²/h/MPa", "l/cm2/h/MPa", "l cm-2 h-1 MPa-1",
  "g/cm²/h/MPa", "g/cm2/h/MPa", "g cm-2 h-1 MPa-1",
  "mmol/s/MPa", "mmol s-1 MPa-1",
  "mol/s/MPa", "mol s-1 MPa-1",
  "kg/s/MPa", "kg s-1 MPa-1",
  "l/s/MPa", "l s-1 MPa-1",
  "g/s/MPa", "g s-1 MPa-1",
  "mmol/h/MPa", "mmol h-1 MPa-1",
  "mol/h/MPa", "mol h-1 MPa-1",
  "kg/h/MPa", "kg h-1 MPa-1",
  "l/h/MPa", "l h-1 MPa-1",
  "g/h/MPa", "g h-1 MPa-1",
  "m³/m²/s/MPa", "m3/m2/s/MPa", "m3 m-2 s-1 MPa-1",
  "m³/s/MPa", "m3/s/MPa", "m3 s-1 MPa-1",
  "m³/m²/h/MPa", "m3/m2/h/MPa", "m3 m-2 h-1 MPa-1",
  "m³/h/MPa", "m3/h/MPa", "m3 h-1 MPa-1"
)

check_transformation <- function(df){
  Kwp <- df$Kwp
  Level <- df$Level
  Kwp_original_units <- df$Kwp_original_units
  Kwp_cor_Leaf <- df$Kwp_cor_Leaf
  Kwp_cor_sapwood <- df$Kwp_cor_sapwood
  Kwp_cor_plant <- df$Kwp_cor_plant
  Kwp_cor_wood <- df$Kwp_cor_wood
  Kwp_cor_ground <- df$Kwp_cor_ground
  
  # Check if Kwp is numeric and not NA
  if (!is.numeric(Kwp) || is.na(Kwp)) {
    return("Kwp is not numeric or is NA")
  }
  
  # Check if Kwp_original_units is one of the expected units
  if (!Kwp_original_units %in% valid_units) {
    return("original units are wrong")
  }
  
  # Check if Level is one of the expected levels
  if (!Level %in% c("Leaf", "Sapwood", "Plant", "Wood", "Ground")) {
    return("Level is not one of the expected levels")
  }
  
  # Check each level transformation from the original units to Kg m-2 s-1 MPa-1
  # - 1 mol H2O = 0.018 kg
  # - 1 mmol H2O = 1.8e-5 kg
  # - 1 g = 1e-3 kg
  # - 1 dm² = 1e-2 m²
  # - 1 cm² = 1e-4 m²
  # - 1 h = 3600 s
  
  if (Level == "Leaf") {
    if (Kwp_original_units %in% c("mmol/m²/s/MPa", "mmol/m2/s/MPa", "mmol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 1.8e-5 - Kwp_cor_Leaf ) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/s/MPa", "mol/m2/s/MPa", "mol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 0.018 - Kwp_cor_Leaf ) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/s/MPa", "kg/m2/s/MPa", "kg m-2 s-1 MPa-1",
                                         "l/m²/s/MPa", "l/m2/s/MPa", "l m-2 s-1 MPa-1")) {
      return(abs(Kwp - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/s/MPa", "g/m2/s/MPa", "g m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/s/MPa", "m3/m2/s/MPa", "m3 m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e3 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/m²/h/MPa", "mmol/m2/h/MPa", "mmol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/h/MPa", "mol/m2/h/MPa", "mol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/h/MPa", "kg/m2/h/MPa", "kg m-2 h-1 MPa-1",
                                         "l/m²/h/MPa", "l/m2/h/MPa", "l m-2 h-1 MPa-1")) {
      return(abs(Kwp / 3600 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/h/MPa", "g/m2/h/MPa", "g m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/h/MPa", "m3/m2/h/MPa", "m3 m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e3/3600 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/s/MPa", "mmol/dm2/s/MPa", "mmol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/s/MPa", "mol/dm2/s/MPa", "mol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/s/MPa", "kg/dm2/s/MPa", "kg dm-2 s-1 MPa-1",
                                         "l/dm²/s/MPa", "l/dm2/s/MPa", "l dm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/s/MPa", "g/dm2/s/MPa", "g dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/h/MPa", "mmol/dm2/h/MPa", "mmol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/h/MPa", "mol/dm2/h/MPa", "mol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/h/MPa", "kg/dm2/h/MPa", "kg dm-2 h-1 MPa-1",
                                         "l/dm²/h/MPa", "l/dm2/h/MPa", "l dm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-2 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/h/MPa", "g/dm2/h/MPa", "g dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-2 - Kwp_cor_Leaf) < 1e-3)
    }else if (Kwp_original_units %in% c("mmol/cm²/s/MPa", "mmol/cm2/s/MPa", "mmol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-4 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/s/MPa", "mol/cm2/s/MPa", "mol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-4 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/s/MPa", "kg/cm2/s/MPa", "kg cm-2 s-1 MPa-1",
                                         "l/cm²/s/MPa", "l/cm2/s/MPa", "l cm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-4 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/s/MPa", "g/cm2/s/MPa", "g cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-4 - Kwp_cor_Leaf ) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/cm²/h/MPa", "mmol/cm2/h/MPa", "mmol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-4 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/h/MPa", "mol/cm2/h/MPa", "mol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-4 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/h/MPa", "kg/cm2/h/MPa", "kg cm-2 h-1 MPa-1",
                                         "l/cm²/h/MPa", "l/cm2/h/MPa", "l cm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-4 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/h/MPa", "g/cm2/h/MPa", "g cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-4 - Kwp_cor_Leaf) < 1e-3)
    }
  } else if (Level == "Sapwood") {
    if (Kwp_original_units %in% c("mmol/m²/s/MPa", "mmol/m2/s/MPa", "mmol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 1.8e-5 - Kwp_cor_sapwood ) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/s/MPa", "mol/m2/s/MPa", "mol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 0.018 - Kwp_cor_sapwood ) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/s/MPa", "kg/m2/s/MPa", "kg m-2 s-1 MPa-1",
                                         "l/m²/s/MPa", "l/m2/s/MPa", "l m-2 s-1 MPa-1")) {
      return(abs(Kwp - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/s/MPa", "g/m2/s/MPa", "g m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/s/MPa", "m3/m2/s/MPa", "m3 m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e3 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/m²/h/MPa", "mmol/m2/h/MPa", "mmol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/h/MPa", "mol/m2/h/MPa", "mol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/h/MPa", "kg/m2/h/MPa", "kg m-2 h-1 MPa-1",
                                         "l/m²/h/MPa", "l/m2/h/MPa", "l m-2 h-1 MPa-1")) {
      return(abs(Kwp / 3600 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/h/MPa", "g/m2/h/MPa", "g m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/h/MPa", "m3/m2/h/MPa", "m3 m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e3/3600 - Kwp_cor_Leaf) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/s/MPa", "mmol/dm2/s/MPa", "mmol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/s/MPa", "mol/dm2/s/MPa", "mol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/s/MPa", "kg/dm2/s/MPa", "kg dm-2 s-1 MPa-1",
                                         "l/dm²/s/MPa", "l/dm2/s/MPa", "l dm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/s/MPa", "g/dm2/s/MPa", "g dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/h/MPa", "mmol/dm2/h/MPa", "mmol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/h/MPa", "mol/dm2/h/MPa", "mol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/h/MPa", "kg/dm2/h/MPa", "kg dm-2 h-1 MPa-1",
                                         "l/dm²/h/MPa", "l/dm2/h/MPa", "l dm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-2 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/h/MPa", "g/dm2/h/MPa", "g dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-2 - Kwp_cor_sapwood) < 1e-3)
    }else if (Kwp_original_units %in% c("mmol/cm²/s/MPa", "mmol/cm2/s/MPa", "mmol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-4 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/s/MPa", "mol/cm2/s/MPa", "mol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-4 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/s/MPa", "kg/cm2/s/MPa", "kg cm-2 s-1 MPa-1",
                                         "l/cm²/s/MPa", "l/cm2/s/MPa", "l cm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-4 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/s/MPa", "g/cm2/s/MPa", "g cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-4 - Kwp_cor_sapwood ) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/cm²/h/MPa", "mmol/cm2/h/MPa", "mmol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-4 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/h/MPa", "mol/cm2/h/MPa", "mol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-4 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/h/MPa", "kg/cm2/h/MPa", "kg cm-2 h-1 MPa-1",
                                         "l/cm²/h/MPa", "l/cm2/h/MPa", "l cm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-4 - Kwp_cor_sapwood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/h/MPa", "g/cm2/h/MPa", "g cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-4 - Kwp_cor_sapwood) < 1e-3)
    }
  } else if (Level == "Plant") {
    if (Kwp_original_units %in% c("mmol/s/MPa", "mmol/s/MPa", "mmol s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/s/MPa", "mol/s/MPa", "mol s-1 MPa-1")) {
      return(abs(Kwp * 0.018 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/s/MPa", "kg/s/MPa", "kg s-1 MPa-1",
                                         "l/s/MPa", "l/s/MPa", "l s-1 MPa-1")) {
      return(abs(Kwp - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("g/s/MPa", "g/s/MPa", "g s-1 MPa-1")) {
      return(abs(Kwp * 1e-3 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/s/MPa", "m3/s/MPa", "m3 s-1 MPa-1")) {
      return(abs(Kwp * 1e3 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/h/MPa", "mmol/h/MPa", "mmol h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/h/MPa", "mol/h/MPa", "mol h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/h/MPa", "kg/h/MPa", "kg h-1 MPa-1",
                                         "l/h/MPa", "l/h/MPa", "l h-1 MPa-1")) {
      return(abs(Kwp / 3600 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("g/h/MPa", "g/h/MPa", "g h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600 - Kwp_cor_plant) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/h/MPa", "m3/h/MPa", "m3 h-1 MPa-1")) {
      return(abs(Kwp * 1e3/3600 - Kwp_cor_plant) < 1e-3)
    }
  }else if (Level == "Ground") {
    if (Kwp_original_units %in% c("mmol/m²/s/MPa", "mmol/m2/s/MPa", "mmol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 1.8e-5 - Kwp_cor_ground ) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/s/MPa", "mol/m2/s/MPa", "mol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 0.018 - Kwp_cor_ground ) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/s/MPa", "kg/m2/s/MPa", "kg m-2 s-1 MPa-1",
                                         "l/m²/s/MPa", "l/m2/s/MPa", "l m-2 s-1 MPa-1")) {
      return(abs(Kwp - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/s/MPa", "g/m2/s/MPa", "g m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/s/MPa", "m3/m2/s/MPa", "m3 m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e3 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/m²/h/MPa", "mmol/m2/h/MPa", "mmol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/h/MPa", "mol/m2/h/MPa", "mol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/h/MPa", "kg/m2/h/MPa", "kg m-2 h-1 MPa-1",
                                         "l/m²/h/MPa", "l/m2/h/MPa", "l m-2 h-1 MPa-1")) {
      return(abs(Kwp / 3600 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/h/MPa", "g/m2/h/MPa", "g m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/h/MPa", "m3/m2/h/MPa", "m3 m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e3/3600 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/s/MPa", "mmol/dm2/s/MPa", "mmol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/s/MPa", "mol/dm2/s/MPa", "mol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/s/MPa", "kg/dm2/s/MPa", "kg dm-2 s-1 MPa-1",
                                         "l/dm²/s/MPa", "l/dm2/s/MPa", "l dm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/s/MPa", "g/dm2/s/MPa", "g dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/h/MPa", "mmol/dm2/h/MPa", "mmol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/h/MPa", "mol/dm2/h/MPa", "mol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/h/MPa", "kg/dm2/h/MPa", "kg dm-2 h-1 MPa-1",
                                         "l/dm²/h/MPa", "l/dm2/h/MPa", "l dm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-2 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/h/MPa", "g/dm2/h/MPa", "g dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-2 - Kwp_cor_ground) < 1e-3)
    }else if (Kwp_original_units %in% c("mmol/cm²/s/MPa", "mmol/cm2/s/MPa", "mmol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-4 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/s/MPa", "mol/cm2/s/MPa", "mol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-4 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/s/MPa", "kg/cm2/s/MPa", "kg cm-2 s-1 MPa-1",
                                         "l/cm²/s/MPa", "l/cm2/s/MPa", "l cm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-4 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/s/MPa", "g/cm2/s/MPa", "g cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-4 - Kwp_cor_ground ) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/cm²/h/MPa", "mmol/cm2/h/MPa", "mmol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-4 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/h/MPa", "mol/cm2/h/MPa", "mol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-4 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/h/MPa", "kg/cm2/h/MPa", "kg cm-2 h-1 MPa-1",
                                         "l/cm²/h/MPa", "l/cm2/h/MPa", "l cm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-4 - Kwp_cor_ground) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/h/MPa", "g/cm2/h/MPa", "g cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-4 - Kwp_cor_ground) < 1e-3)
    }
  }else if (Level == "Wood") {
    if (Kwp_original_units %in% c("mmol/m²/s/MPa", "mmol/m2/s/MPa", "mmol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 1.8e-5 - Kwp_cor_wood ) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/s/MPa", "mol/m2/s/MPa", "mol m-2 s-1 MPa-1")) {
      return(abs(Kwp* 0.018 - Kwp_cor_wood ) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/s/MPa", "kg/m2/s/MPa", "kg m-2 s-1 MPa-1",
                                         "l/m²/s/MPa", "l/m2/s/MPa", "l m-2 s-1 MPa-1")) {
      return(abs(Kwp - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/s/MPa", "g/m2/s/MPa", "g m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/s/MPa", "m3/m2/s/MPa", "m3 m-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e3 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/m²/h/MPa", "mmol/m2/h/MPa", "mmol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/m²/h/MPa", "mol/m2/h/MPa", "mol m-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/m²/h/MPa", "kg/m2/h/MPa", "kg m-2 h-1 MPa-1",
                                         "l/m²/h/MPa", "l/m2/h/MPa", "l m-2 h-1 MPa-1")) {
      return(abs(Kwp / 3600 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/m²/h/MPa", "g/m2/h/MPa", "g m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("m³/m²/h/MPa", "m3/m2/h/MPa", "m3 m-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e3/3600 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/s/MPa", "mmol/dm2/s/MPa", "mmol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/s/MPa", "mol/dm2/s/MPa", "mol dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/s/MPa", "kg/dm2/s/MPa", "kg dm-2 s-1 MPa-1",
                                         "l/dm²/s/MPa", "l/dm2/s/MPa", "l dm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/s/MPa", "g/dm2/s/MPa", "g dm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/dm²/h/MPa", "mmol/dm2/h/MPa", "mmol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/dm²/h/MPa", "mol/dm2/h/MPa", "mol dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/dm²/h/MPa", "kg/dm2/h/MPa", "kg dm-2 h-1 MPa-1",
                                         "l/dm²/h/MPa", "l/dm2/h/MPa", "l dm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-2 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/dm²/h/MPa", "g/dm2/h/MPa", "g dm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-2 - Kwp_cor_wood) < 1e-3)
    }else if (Kwp_original_units %in% c("mmol/cm²/s/MPa", "mmol/cm2/s/MPa", "mmol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/1e-4 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/s/MPa", "mol/cm2/s/MPa", "mol cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 0.018/1e-4 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/s/MPa", "kg/cm2/s/MPa", "kg cm-2 s-1 MPa-1",
                                         "l/cm²/s/MPa", "l/cm2/s/MPa", "l cm-2 s-1 MPa-1")) {
      return(abs(Kwp / 1e-4 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/s/MPa", "g/cm2/s/MPa", "g cm-2 s-1 MPa-1")) {
      return(abs(Kwp * 1e-3/1e-4 - Kwp_cor_wood ) < 1e-3)
    } else if (Kwp_original_units %in% c("mmol/cm²/h/MPa", "mmol/cm2/h/MPa", "mmol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1.8e-5/3600/1e-4 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("mol/cm²/h/MPa", "mol/cm2/h/MPa", "mol cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 0.018/3600/1e-4 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("kg/cm²/h/MPa", "kg/cm2/h/MPa", "kg cm-2 h-1 MPa-1",
                                         "l/cm²/h/MPa", "l/cm2/h/MPa", "l cm-2 h-1 MPa-1")) {
      return(abs(Kwp /3600/1e-4 - Kwp_cor_wood) < 1e-3)
    } else if (Kwp_original_units %in% c("g/cm²/h/MPa", "g/cm2/h/MPa", "g cm-2 h-1 MPa-1")) {
      return(abs(Kwp * 1e-3/3600/1e-4 - Kwp_cor_wood) < 1e-3)
    }
  }
  
  
  # If no conditions matched, return message indicating transformation is not valid
  return(FALSE)
  
}

# ----------------------------------------------------------------------------
# Function to check Kwp transformations
# ----------------------------------------------------------------------------
check_Kwp_transformations <- function(row) {
  has <- function(...) all(!is.na(c(...)))
  get <- function(x) row[[x]]
  
  if (!"pl_LA" %in% names(row)) {
    row$pl_LA <- NA_character_
  }
  
  level <- get("Level")
  original_value <- get("Kwp")
  
  # Start by assuming no transformation
  result <- list(
    Kwp_cor_Leaf = NA_character_,
    Kwp_cor_sapwood = NA_character_,
    Kwp_cor_plant = NA_character_,
    Kwp_cor_wood = NA_character_,
    Kwp_cor_ground = NA_character_
  )
  
  if(level != "Leaf"){original_level <- tolower(level)
  }else{original_level <- "Leaf"}
  
  # Mark the original level
  result[[paste0("Kwp_cor_", original_level)]] <- "ORIGINAL"
  
  # Set of levels that are already calculated and available for chaining
  computed_levels <- c(level)
  
  # Iteratively check for possible transformations until no new levels are found
  previous_length <- 0
  while (length(computed_levels) > previous_length) {
    previous_length <- length(computed_levels)
    
    # LEAF ↔ SAPWOOD (pl_huber_value)
    if ("Leaf" %in% computed_levels && has(get("pl_huber_value")) && !"Sapwood" %in% computed_levels) {
      result$Kwp_cor_sapwood <- if (!is.na(get("Kwp_cor_sapwood"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Sapwood")
    }
    if ("Sapwood" %in% computed_levels && has(get("pl_huber_value")) && !"Leaf" %in% computed_levels) {
      result$Kwp_cor_Leaf <- if (!is.na(get("Kwp_cor_Leaf"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Leaf")
    }
    
    # PLANT  → SAPWOOD (pl_SA )
    if ("Plant" %in% computed_levels && has(get("pl_SA")) && !"Sapwood" %in% computed_levels) {
      result$Kwp_cor_sapwood <- if (!is.na(get("Kwp_cor_sapwood"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Sapwood")
    }
    # PLANT  → LEAF (pl_LA)
    if ("Plant" %in% computed_levels &&  has(get("pl_LA")) && !"Leaf" %in% computed_levels) {
      result$Kwp_cor_Leaf <- if (!is.na(get("Kwp_cor_Leaf"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Leaf")
    }
    
    # LEAF → PLANT (pl_LA)
    if ("Leaf" %in% computed_levels && has(get("pl_LA")) && !"Plant" %in% computed_levels) {
      result$Kwp_cor_plant <- if (!is.na(get("Kwp_cor_plant"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Plant")
    }
    
    # SAPWOOD → PLANT (pl_SA)
    if ("Sapwood" %in% computed_levels && has(get("pl_SA")) && !"Plant" %in% computed_levels) {
      result$Kwp_cor_plant <- if (!is.na(get("Kwp_cor_plant"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Plant")
    }
    
    # PLANT → WOOD (stand density or basal area, or DBH)
    if ("Plant" %in% computed_levels && 
        ((has(get("st_density")) && has(get("pl_basal_area"))) || has(get("pl_DBH"))) &&
        !"Wood" %in% computed_levels) {
      result$Kwp_cor_wood <- if (!is.na(get("Kwp_cor_wood"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Wood")
    }
    
    # WOOD or Leaf or Plant → GROUND (via LAI, st_density, pl_basal_area, etc.)
    if ((("Wood" %in% computed_levels && has(get("st_density")) && has(get("st_basal_area"))) ||
         ("Leaf" %in% computed_levels && has(get("pl_LAI"))) ||
         ("Plant" %in% computed_levels && has(get("st_density")))) &&
        !"Ground" %in% computed_levels) {
      result$Kwp_cor_ground <- if (!is.na(get("Kwp_cor_ground"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Ground")
    }
    
    # GROUND → PLANT (st_density)
    if ("Ground" %in% computed_levels && has(get("st_density")) && !"Plant" %in% computed_levels) {
      result$Kwp_cor_plant <- if (!is.na(get("Kwp_cor_plant"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Plant")
    }
    
    # GROUND → LEAF (pl_LAI)
    if ("Ground" %in% computed_levels && has(get("pl_LAI")) && !"Leaf" %in% computed_levels) {
      result$Kwp_cor_Leaf <- if (!is.na(get("Kwp_cor_Leaf"))) "OK" else "Missing"
      computed_levels <- c(computed_levels, "Leaf")
    }
  }
  
  return(unlist(result))
}

# ----------------------------------------------------------------------------
# Function to check if Kwp transformations are correct
# ----------------------------------------------------------------------------
check_Kwp_transformations_correctness <- function(row, tolerance = 0.01) {
  required_fields <- c("Level", "Kwp_cor_Leaf", "Kwp_cor_sapwood", "Kwp_cor_plant", "Kwp_cor_wood", "Kwp_cor_ground")
  if (!all(required_fields %in% names(row))) {
    return(setNames(as.list(rep(NA, 5)), paste0("Kwp_cor_", c("Leaf", "sapwood", "plant", "wood", "ground"))))
  }
  
  get_num <- function(name) {
    if (!name %in% names(row)) return(NA_real_)
    # Handle potential column duplication by using the last occurrence
    if (sum(names(row) == name) > 1) {
      indices <- which(names(row) == name)
      val <- suppressWarnings(as.numeric(row[[max(indices)]]))
    } else {
      val <- suppressWarnings(as.numeric(row[[name]]))
    }
    if (length(val) == 0 || is.na(val)) return(NA_real_)
    val
  }
  
  get_chr <- function(x) {
    if (!x %in% names(row)) return(NA_character_)
    as.character(row[[x]])
  }
  
  close_enough <- function(a, b) {
    if (any(is.na(c(a, b)))) return(FALSE)
    abs(a - b) < tolerance * abs(b)
  }
  
  level <- get_chr("Level")
  original_level <- level
  if(level != "Leaf"){original_level <- tolower(original_level)}
  source_col <- paste0("Kwp_cor_", original_level)
  if (!source_col %in% names(row)) {
    return(setNames(as.list(rep(NA, 5)), paste0("Kwp_cor_", c("Leaf", "sapwood", "plant", "wood", "ground"))))
  }
  source_val <- get_num(source_col)
  if (is.na(source_val)) {
    return(setNames(as.list(rep(NA, 5)), paste0("Kwp_cor_", c("Leaf", "sapwood", "plant", "wood", "ground"))))
  }
  
  result <- list(
    Kwp_cor_Leaf = NA,
    Kwp_cor_sapwood = NA,
    Kwp_cor_plant = NA,
    Kwp_cor_wood = NA,
    Kwp_cor_ground = NA
  )
  
  result[[paste0("Kwp_cor_", original_level)]] <- "ORIGINAL"
  
  # Keep track of validated levels and their values
  validated_levels <- list()
  validated_levels[[level]] <- source_val
  
  # Iteratively check for possible validations until no new levels are found
  previous_length <- 0
  while (length(validated_levels) > previous_length) {
    previous_length <- length(validated_levels)
    
    # Helper function to try validation and update if successful
    try_validate <- function(from_level, to_level, calculated_value, target_value) {
      if (from_level %in% names(validated_levels) && !to_level %in% names(validated_levels) && !is.na(calculated_value) && !is.na(target_value)) {
        # Use consistent naming: "Leaf" stays capitalized, others lowercase
        result_key <- if (to_level == "Leaf") "Kwp_cor_Leaf" else paste0("Kwp_cor_", tolower(to_level))
        
        if (close_enough(calculated_value, target_value)) {
          validated_levels[[to_level]] <<- target_value
          result[[result_key]] <<- TRUE
          return(TRUE)
        } else {
          result[[result_key]] <<- FALSE
          return(FALSE)
        }
      }
      return(FALSE)
    }
    
    # LEAF ↔ SAPWOOD (pl_huber_value)
    huber <- get_num("pl_huber_value")
    if ("Leaf" %in% names(validated_levels) && !is.na(huber)) {
      calculated_sapwood <- validated_levels[["Leaf"]] / huber
      try_validate("Leaf", "Sapwood", calculated_sapwood, get_num("Kwp_cor_sapwood"))
    }
    if ("Sapwood" %in% names(validated_levels) && !is.na(huber)) {
      calculated_leaf <- validated_levels[["Sapwood"]] * huber
      try_validate("Sapwood", "Leaf", calculated_leaf, get_num("Kwp_cor_Leaf"))
    }
    
    # PLANT → SAPWOOD (pl_SA)
    SA <- get_num("pl_SA")
    if ("Plant" %in% names(validated_levels) && !is.na(SA)) {
      calculated_sapwood <- validated_levels[["Plant"]] / SA
      try_validate("Plant", "Sapwood", calculated_sapwood, get_num("Kwp_cor_sapwood"))
    }
    
    # PLANT → LEAF (pl_LA)
    LA <- get_num("pl_LA")
    if ("Plant" %in% names(validated_levels) && !is.na(LA)) {
      calculated_leaf <- validated_levels[["Plant"]] / LA
      try_validate("Plant", "Leaf", calculated_leaf, get_num("Kwp_cor_Leaf"))
    }
    
    # LEAF/SAPWOOD → PLANT 
    LA <- get_num("pl_LA")
    SA <- get_num("pl_SA")
    plant_target <- get_num("Kwp_cor_plant")
    
    # LEAF → PLANT (pl_LA)
    if ("Leaf" %in% names(validated_levels) && !is.na(LA)) {
      calculated_plant <- validated_levels[["Leaf"]] * LA
      try_validate("Leaf", "Plant", calculated_plant, plant_target)
    }
    
    # SAPWOOD → PLANT (pl_SA)
    if ("Sapwood" %in% names(validated_levels) && !is.na(SA)) {
      calculated_plant <- validated_levels[["Sapwood"]] * SA
      try_validate("Sapwood", "Plant", calculated_plant, plant_target)
    }
    
    # PLANT → WOOD
    density <- get_num("st_density")
    ba <- get_num("pl_basal_area")
    dbh <- get_num("pl_DBH")
    st_basal_area <- get_num("st_basal_area")
    
    if ("Plant" %in% names(validated_levels)) {
      plant_val <- validated_levels[["Plant"]]
      wood_target <- get_num("Kwp_cor_wood")
      
      if (!is.na(ba) && ba > 0) {
        calculated_wood <- plant_val / ba
        try_validate("Plant", "Wood", calculated_wood, wood_target)
      } else if (!is.na(dbh) && dbh > 0) {
        ba_per_tree <- pi * ((dbh/100)^2)
        calculated_wood <- plant_val / ba_per_tree
        try_validate("Plant", "Wood", calculated_wood, wood_target)
      } else if (!is.na(st_basal_area) && st_basal_area != 0 && !is.na(density)) {
        calculated_wood <- plant_val * (density / st_basal_area)
        try_validate("Plant", "Wood", calculated_wood, wood_target)
      }
    }
    
    # Multiple paths to GROUND
    lai <- get_num("pl_LAI")
    ground_target <- get_num("Kwp_cor_ground")
    
    # LEAF → GROUND (via LAI)
    if ("Leaf" %in% names(validated_levels) && !is.na(lai)) {
      calculated_ground <- validated_levels[["Leaf"]] * lai
      try_validate("Leaf", "Ground", calculated_ground, ground_target)
    }
    
    # PLANT → GROUND (via density)
    if ("Plant" %in% names(validated_levels) && !is.na(density)) {
      calculated_ground <- validated_levels[["Plant"]] * density * 1e-4
      try_validate("Plant", "Ground", calculated_ground, ground_target)
    }
    
    # WOOD → GROUND (via basal area)
    if ("Wood" %in% names(validated_levels) && !is.na(ba)) {
      calculated_ground <- validated_levels[["Wood"]] * ba
      try_validate("Wood", "Ground", calculated_ground, ground_target)
    }
    
    # GROUND → PLANT (st_density)
    if ("Ground" %in% names(validated_levels) && !is.na(density) && density != 0) {
      calculated_plant <- validated_levels[["Ground"]] / (density * 1e-4)
      try_validate("Ground", "Plant", calculated_plant, get_num("Kwp_cor_plant"))
    }
    
    # GROUND → LEAF (pl_LAI)
    if ("Ground" %in% names(validated_levels) && !is.na(lai) && lai != 0) {
      calculated_leaf <- validated_levels[["Ground"]] / lai
      try_validate("Ground", "Leaf", calculated_leaf, get_num("Kwp_cor_Leaf"))
    }
  }
  
  return(lapply(result, as.character))
}

# ----------------------------------------------------------------------------
# List of valid ISO 3166-1 alpha-3 country codes
# ----------------------------------------------------------------------------
valid_country_codes <- c("AFG", "ALA", "ALB", "DZA", "ASM", "AND", "AGO", "AIA", "ATA", "ATG",
                         "ARG", "ARM", "ABW", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB",
                         "BLR", "BEL", "BLZ", "BEN", "BMU", "BTN", "BOL", "BES", "BIH", "BWA",
                         "BVT", "BRA", "IOT", "BRN", "BGR", "BFA", "BDI", "CPV", "KHM", "CMR",
                         "CAN", "CYM", "CAF", "TCD", "CHL", "CHN", "CXR", "CCK", "COL", "COM",
                         "COG", "COD", "COK", "CRI", "CIV", "HRV", "CUB", "CUW", "CYP", "CZE",
                         "DNK", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST",
                         "ETH", "FLK", "FRO", "FJI", "FIN", "FRA", "GUF", "PYF", "ATF", "GAB",
                         "GMB", "GEO", "DEU", "GHA", "GIB", "GRC", "GRL", "GRD", "GLP", "GUM",
                         "GTM", "GGY", "GIN", "GNB", "GUY", "HTI", "HMD", "VAT", "HND", "HKG",
                         "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL", "IMN", "ISR", "ITA",
                         "JAM", "JPN", "JEY", "JOR", "KAZ", "KEN", "KIR", "PRK", "KOR", "KWT",
                         "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", "LUX",
                         "MAC", "MKD", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MTQ",
                         "MRT", "MUS", "MYT", "MEX", "FSM", "MDA", "MCO", "MNG", "MNE", "MSR",
                         "MAR", "MOZ", "MMR", "NAM", "NRU", "NPL", "NLD", "NCL", "NZL", "NIC",
                         "NER", "NGA", "NIU", "NFK", "MNP", "NOR", "OMN", "PAK", "PLW", "PSE",
                         "PAN", "PNG", "PRY", "PER", "PHL", "PCN", "POL", "PRT", "PRI", "QAT",
                         "REU", "ROU", "RUS", "RWA", "BLM", "SHN", "KNA", "LCA", "MAF", "SPM",
                         "VCT", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP",
                         "SXM", "SVK", "SVN", "SLB", "SOM", "ZAF", "SGS", "SSD", "ESP", "LKA",
                         "SDN", "SUR", "SJM", "SWZ", "SWE", "CHE", "SYR", "TWN", "TJK", "TZA",
                         "THA", "TLS", "TGO", "TKL", "TON", "TTO", "TUN", "TUR", "TKM", "TCA",
                         "TUV", "UGA", "UKR", "ARE", "GBR", "USA", "UMI", "URY", "UZB", "VUT",
                         "VEN", "VNM", "VGB", "VIR", "WLF", "ESH", "YEM", "ZMB", "ZWE")

# ----------------------------------------------------------------------------
# Function to check country codes in a dataset
# ----------------------------------------------------------------------------
qc_check_country_codes <- function(data, country_column = "si_country") {
  if (!country_column %in% names(data)) {
    stop("The dataset does not contain the column: ", country_column)
  }
  
  codes <- unique(na.omit(data[[country_column]]))
  invalid_codes <- codes[!codes %in% valid_country_codes]
  
  list(
    total_countries = length(codes),
    valid = codes[codes %in% valid_country_codes],
    invalid = invalid_codes,
    summary_table = data.frame(
      country = codes,
      is_valid_country = codes %in% valid_country_codes
    )
  )
}

# ----------------------------------------------------------------------------
# Function to check coordinates and country maps
# ----------------------------------------------------------------------------
qc_check_coordinates_simple <- function(data, maps_folder = getwd()) {
  # Requisitos básicos
  required_cols <- c("si_lat", "si_long", "si_country")
  if (!all(required_cols %in% names(data))) {
    stop("Dataset must contain columns: si_lat, si_long, si_country")
  }
  
  # Verify column type
  if (!is.numeric(data$si_lat)) {
    message("⚠️ 'si_lat' is not numeric. Attempting to coerce to numeric.")
    data$si_lat <- suppressWarnings(as.numeric(data$si_lat))
  }
  if (!is.numeric(data$si_long)) {
    message("⚠️ 'si_long' is not numeric. Attempting to coerce to numeric.")
    data$si_long <- suppressWarnings(as.numeric(data$si_long))
  }
  
  # Descargar mapas si es necesario
  qc_download_maps_simple(data, folder = maps_folder)
  
  # Cargar polígonos de tierra globales
  land <- suppressMessages(suppressWarnings({
    tmp <- capture.output(
      obj <- rnaturalearth::ne_download(
        scale = "large", type = "land", category = "physical", returnclass = "sf"
      )
    )
    obj
  }))
  # Inicializar vectores de resultado
  inside_country <- logical(nrow(data))
  on_land <- logical(nrow(data))
  
  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    code <- row$si_country
    
    # Saltar si falta alguna info
    if (is.na(row$si_lat) || is.na(row$si_long) || is.na(code)) {
      inside_country[i] <- NA
      on_land[i] <- NA
      next
    }
    
    # Leer mapa del país
    map_file <- file.path(maps_folder, "gadm", paste0("gadm41_", code, "_0_pk.rds"))
    if (!file.exists(map_file)) {
      inside_country[i] <- NA
      on_land[i] <- NA
      next
    }
    
    country_map <- terra::unwrap(readRDS(map_file))
    
    # Crear punto como SpatVector
    point <- terra::vect(row[, c("si_long", "si_lat")],
                         geom = c("si_long", "si_lat"),
                         crs = "epsg:4326")
    
    # Está dentro del país
    inside_country[i] <- terra::is.related(country_map, point, relation = "contains")
    
    # Está sobre tierra
    point_sf <- sf::st_as_sf(row[, c("si_long", "si_lat")],
                             coords = c("si_long", "si_lat"), crs = 4326)
    on_land[i] <- !is.na(sf::st_join(point_sf, land, join = sf::st_within)$scalerank)
  }
  
  # Agregar columnas al dataframe original
  data$is_inside_country <- inside_country
  data$is_on_land <- on_land
  
  return(data)
}

# ----------------------------------------------------------------------------
# Function to download maps for countries in a dataset
# ----------------------------------------------------------------------------
qc_download_maps_simple <- function(data, folder = getwd(), verbose = TRUE) {
  # Comprobaciones básicas
  if (!"si_country" %in% names(data)) {
    stop("The dataset must contain a column named 'si_country'.")
  }
  
  # Crear carpeta gadm si no existe
  gadm_dir <- file.path(folder, "gadm")
  if (!dir.exists(gadm_dir)) {
    dir.create(gadm_dir, recursive = TRUE)
  }
  
  # Filtrar códigos únicos válidos
  country_codes <- unique(na.omit(data$si_country))
  
  # Inicializar contadores y registros
  downloaded <- 0
  skipped <- 0
  failed <- 0
  
  skipped_codes <- c()
  downloaded_codes <- c()
  failed_codes <- c()
  
  for (code in country_codes) {
    file_name <- paste0("gadm41_", code, "_0_pk.rds")
    file_path <- file.path(gadm_dir, file_name)
    
    if (file.exists(file_path)) {
      skipped <- skipped + 1
      skipped_codes <- c(skipped_codes, code)
      if (verbose) message(sprintf("✅ Map for %s already exists. Skipping...", code))
      next
    }
    
    # Intentar descargar el mapa
    tryCatch({
      if (verbose) message(sprintf("⬇️ Downloading map for %s...", code))
      geodata::gadm(country = code, level = 0, path = folder, resolution = 1)
      downloaded <- downloaded + 1
      downloaded_codes <- c(downloaded_codes, code)
    }, error = function(e) {
      message(sprintf("❌ Failed to download map for %s", code))
      failed <- failed + 1
      failed_codes <- c(failed_codes, code)
    })
  }
  
  # Mensaje resumen
  message(sprintf("\n📦 Summary of map downloads:"))
  message(sprintf("✔️  %d maps already existed: %s", skipped, paste(skipped_codes, collapse = ", ")))
  message(sprintf("⬇️  %d new maps downloaded: %s", downloaded, paste(downloaded_codes, collapse = ", ")))
  message(sprintf("⚠️  %d failed downloads: %s", failed, paste(failed_codes, collapse = ", ")))
}


# ----------------------------------------------------------------------------
# Function to update old templates
# ----------------------------------------------------------------------------
update_excel_file <- function(template_path, old_data_path, output_path, desired_cols) {
  library(openxlsx)
  
  # Load the new formatted template
  wb_template <- loadWorkbook(template_path)
  
  # Load the old data workbook
  df_old <- read.xlsx(old_data_path, sheet = "data")
  
  # Rename 'Original_units' to 'Kwp_original_units' if present
  if ("Original_units" %in% names(df_old)) {
    names(df_old)[names(df_old) == "Original_units"] <- "Kwp_original_units"
  }
  
  # Handle empty or partially defined data frames
  if (nrow(df_old) == 0) {
    # Create an empty data frame with desired column names
    df_old <- as.data.frame(matrix(ncol = length(desired_cols), nrow = 0))
    names(df_old) <- desired_cols
  } else {
    # Add missing columns with NA
    for (col in desired_cols) {
      if (!(col %in% names(df_old))) {
        df_old[[col]] <- NA
      }
    }
    # Reorder columns
    df_old <- df_old[, desired_cols]
  }
  
  # Overwrite just the values in the "data" sheet of the template (starting below headers)
  writeData(wb_template, sheet = "data", x = df_old, startCol = 1, startRow = 2, colNames = FALSE, withFilter = FALSE)
  
  # Save the updated workbook
  saveWorkbook(wb_template, file = output_path, overwrite = TRUE)
}
