##------------------------------------------------------------------------------
## Data table loading (CSV files)
##------------------------------------------------------------------------------

data.path <- file.path("..", "..", "data")

catch.data.file <- "catch.csv"
harvest.activity.file <- "harvesting-activities.csv"
management.file <- "management-decisions.csv"
gear.names.file <- "gear-names.csv"
maturity.file <- "maturity-vectors.csv"
variance.file <- "variance-parameter-results.csv"
maturity.sens.file <- "maturity-sensitivity-results-table.csv"

cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.csv(data.path,
                    catch.data.file)
harvest.activity <- load.csv(data.path,
                             harvest.activity.file,
                             header = FALSE)
management.activity <- load.csv(data.path,
                                management.file,
                                header = FALSE)
gear.names <- load.csv(data.path,
                       gear.names.file,
                       header = FALSE)
maturity.vec <- load.csv(data.path,
                         maturity.file,
                         header = TRUE)
variance.results <- load.csv(data.path,
                             variance.file,
                             header = TRUE)
maturity.sens <- load.csv(data.path,
                          maturity.sens.file,
                          header = TRUE)
