# This script composes a table for determining the indextypology of a sample
# Last changes added manually, check later what could be taken from DWH

library(RODBC)
library(tidyverse)
library(readr)

connection_vis <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;" # nolint: line_length_linter
  )

query_indextypology <-
  "SELECT hz.Breedte AS width, hz.Helling AS slope, hz.Zonatie AS indextypology
  FROM DimVisindexHuetZonatie hz
  WHERE hz.Versienummer = 1;"

data_indextypology <-
  sqlQuery(connection_vis, query_indextypology, stringsAsFactors = FALSE)

odbcClose(connection_vis)

data_indextypology <- data_indextypology %>%
  mutate(
    tidal = FALSE
  ) %>%
  bind_rows(
    data.frame(
      indextypology = c(rep("estuarien_Schelde", 3), "estuarien_IJzer"),
      tidal = TRUE,
      basin =
        c("Benedenscheldebekken", "Netebekken", "Dijlebekken", "IJzerbekken"),
      stringsAsFactors = FALSE
    )
  )

write_csv2(data_indextypology, "inst/extdata/data_indextypology.csv")
