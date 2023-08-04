# This script composes a table with reference species for determining metrics

library(RODBC)
library(tidyverse)
library(magrittr)
library(readr)

connection_vis <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;"
  )

query_taxonmetrics <-
  "SELECT *
  FROM visindex.data_taxonmetrics;"

data_taxonmetrics <-
  sqlQuery(connection_vis, query_taxonmetrics, stringsAsFactors = FALSE) %>%
  select(-data_taxonmetricsKey, -TaxonKey, -Soort) %>%
  rename(taxonname = WetenschappelijkeNaam)

query_brabeel <-
  "SELECT *
  FROM visindex.brabeel_metrieken;"

data_brabeel <-
  sqlQuery(connection_vis, query_brabeel, stringsAsFactors = FALSE) %>%
  select(-brabeel_metriekenKey, -TaxonKey, -Soortenlijst_brabeel) %>%
  rename(taxonname = Wetenschappelijke_naam_brabeel, taxoncode = TaxonCode)

query_brongebied <-
  "SELECT *
  FROM visindex.brongebied_metrieken;"

data_brongebied <-
  sqlQuery(connection_vis, query_brongebied, stringsAsFactors = FALSE) %>%
  select(-brongebied_metriekenKey, -TaxonKey, -Soortenlijst_brongebied) %>%
  rename(taxonname = Wetenschappelijke_naam_brongebied, taxoncode = TaxonCode)

query_getijde_zijrivieren_zoet <-
  "SELECT *
  FROM visindex.getijde_zijrivieren_zoet;"

data_getijde_zijrivieren_zoet <-
  sqlQuery(connection_vis, query_getijde_zijrivieren_zoet, stringsAsFactors = FALSE) %>%
  select(-getijde_zijrivieren_zoetKey, -TaxonKey, -Soortenlijst_Getijde_rivieren_zoet) %>%
  rename(taxonname = Wetenschappelijke_naam_Getijde_zijrivieren_zoet, taxoncode = TaxonCode)

query_ijzer <-
  "SELECT *
  FROM visindex.ijzerestuarium_metrieken;"

data_ijzer <-
  sqlQuery(connection_vis, query_ijzer, stringsAsFactors = FALSE) %>%
  select(-ijzerestuarium_metriekenKey, -TaxonKey)

query_mesohalien <-
  "SELECT *
  FROM visindex.zeeschelde_mesohalien_metrieken;"

data_mesohalien <-
  sqlQuery(connection_vis, query_mesohalien, stringsAsFactors = FALSE) %>%
  select(-zeeschelde_mesohalien_metriekenKey, -TaxonKey, -Soort)

query_oligohalien <-
  "SELECT *
  FROM visindex.zeeschelde_oligohalien_metrieken;"

data_oligohalien <-
  sqlQuery(connection_vis, query_oligohalien, stringsAsFactors = FALSE) %>%
  select(-zeeschelde_oligohalien_metriekenKey, -TaxonKey)

query_zeeschelde_zoet <-
  "SELECT *
  FROM visindex.zeeschelde_zoet_metrieken;"

data_zeeschelde_zoet <-
  sqlQuery(connection_vis, query_zeeschelde_zoet, stringsAsFactors = FALSE) %>%
  select(-zeeschelde_zoet_metriekenKey, -TaxonKey, -Nederlandse_naam) %>%
  rename(taxonname = Wetenschappelijke_Naam_Zeeschelde_zoet, taxoncode = TaxonCode)

odbcClose(connection_vis)

data_taxonmetrics %<>%
  full_join(
    data_brabeel %>%
      select(-"taxonname"),
    by = c("taxoncode")
  ) %>%
  full_join(
    data_brongebied %>%
      select(-"taxonname"),
    by = c("taxoncode")
  ) %>%
  full_join(
    data_getijde_zijrivieren_zoet %>%
      select(-"taxonname"),
    by = c("taxoncode")
  ) %>%
  full_join(data_ijzer, by = c("taxoncode")) %>%
  full_join(data_mesohalien, by = c("taxoncode")) %>%
  full_join(data_oligohalien, by = c("taxoncode")) %>%
  full_join(
    data_zeeschelde_zoet %>%
      select(-"taxonname"),
    by = c("taxoncode")
  ) %>%
  mutate_if(is.numeric, coalesce, 0) %>%
  filter(.data$taxoncode != "HYB.HYB.")

write_csv2(data_taxonmetrics, "inst/extdata/data_taxonmetrics.csv")
