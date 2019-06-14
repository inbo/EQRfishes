# This script composes a table for determining the guild of a sample

library(RODBC)
library(tidyverse)
library(readr)

connection_vis <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;"
  )

query_guild <-
  "SELECT hz.Breedte AS width, hz.Helling AS slope, hz.Zonatie AS guild
  FROM DimVisindexHuetZonatie hz
  WHERE hz.Versienummer = 1;"

data_guild <-
  sqlQuery(connection_vis, query_guild, stringsAsFactors = FALSE)

odbcClose(connection_vis)

data_guild %<>%
  mutate(
    tidal = FALSE
  ) %>%
  bind_rows(
    data.frame(
      guild = c(rep("estuarien_Schelde", 3),"estuarien_IJzer"),
      tidal = TRUE,
      basin =
        c("Benedenscheldebekken", "Netebekken", "Dijlebekken", "IJzerbekken"),
      stringsAsFactors = FALSE
    )
  )

write_csv2(data_guild, "inst/extdata/data_guild.csv")
