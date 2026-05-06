# Helper functions for Excel operations and data transformations

# Convert Excel column letters to numbers
excel_col_to_num <- function(col_letters) {
  letters_vec <- strsplit(toupper(col_letters), "")[[1]]
  nums <- match(letters_vec, LETTERS)
  Reduce(function(a, b) a * 26 + b, nums, init = 0)
}

# Convert Excel cell address (e.g., "C4") to row and column numbers
cell_to_rowcol <- function(cell) {
  cell <- toupper(cell)
  col_letters <- gsub("[0-9]", "", cell)
  row_numbers <- as.integer(gsub("[A-Z]", "", cell))
  list(row = row_numbers, col = excel_col_to_num(col_letters))
}

# Transform 1/0 to ja/nee
to_ja_nee <- function(x) {
  if (is.na(x) || x == "") return(NA)
  if (isTRUE(x == 1) || identical(as.character(x), "1")) return("ja")
  if (isTRUE(x == 0) || identical(as.character(x), "0")) return("nee")
  x
}

# Transform 1/0 to vrouw/man
to_vrouw_man <- function(x) {
  if (is.na(x) || x == "") return(NA)
  if (isTRUE(x == 1) || identical(as.character(x), "1")) return("vrouw")
  if (isTRUE(x == 0) || identical(as.character(x), "0")) return("man")
  x
}

# Remove decimals from BMI
to_no_decimal <- function(x) {
  if (is.na(x) || x == "") return(NA)
  as.integer(round(as.numeric(x)))
}

# Pre-compute text mappings for efficiency
DIAGNOSIS01_MAPPINGS <- list(
  "gmh_diagnosis01#ME/CVS" = "ME/CVS",
  "gmh_diagnosis01#Multiple Sclerose" = "Multiple Sclerose",
  "gmh_diagnosis01#Q-koorts of Q-koortsvermoeidheidssyndroom" = "Q-koorts of Q-koorts",
  "gmh_diagnosis01#Ziekte van Lyme of post-behandelingssyndroom van de zi" = "Lyme",
  "gmh_diagnosis01#Post/Long COVID" = "Post-COVID",
  "gmh_diagnosis01#Ik doe mee als gezonde deelnemer" = "Gezonde deelnemer"
)

# Get text to add based on variable name and operation
get_text_to_add <- function(var_name, value) {
  # Fast check for empty/zero values
  if (is.na(value) || value == "" || value == 0) return(NULL)
  
  # Check if value is 1 (should add)
  if (!(isTRUE(value == 1) || identical(as.character(value), "1"))) {
    return(NULL)
  }
  
  # Check special mappings first (faster than grepl)
  if (var_name %in% names(DIAGNOSIS01_MAPPINGS)) {
    return(DIAGNOSIS01_MAPPINGS[[var_name]])
  }
  
  # Extract text after # for other variables
  if (grepl("#", var_name, fixed = TRUE)) {
    parts <- strsplit(var_name, "#", fixed = TRUE)[[1]]
    if (length(parts) > 1) {
      return(parts[2])
    }
  }
  
  return(NULL)
}
