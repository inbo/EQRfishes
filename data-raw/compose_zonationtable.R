# This script composes a table for determining the zonation of a sample

library(RODBC)
library(tidyverse)
library(readr)

connection_vis <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;"
  )

query_zonation <-
  "SELECT hz.Breedte AS width, hz.Helling AS slope, hz.Zonatie AS zonation
  FROM DimVisindexHuetZonatie hz
  WHERE hz.Versienummer = 1;"

data_zonation <-
  sqlQuery(connection_vis, query_zonation, stringsAsFactors = FALSE)

odbcClose(connection_vis)

data_zonation %<>%
  mutate(
    tidal = FALSE
  ) %>%
  bind_rows(
    data.frame(
      zonation = c(rep("estuarien_Schelde", 3),"estuarien_IJzer"),
      tidal = TRUE,
      basin =
        c("Benedenscheldebekken", "Netebekken", "Dijlebekken", "IJzerbekken"),
      stringsAsFactors = FALSE
    )
  )

write_csv2(data_zonation, "inst/extdata/data_zonation.csv")
