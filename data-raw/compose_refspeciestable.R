# This script composes a table with reference species for determining metrics

library(RODBC)
library(tidyverse)
library(magrittr)
library(readr)

connection_vis <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;" # nolint: line_length_linter
  )

query_species <-
  "SELECT TaxonKey, Taxoncode, Soort, WetenschappelijkeNaam
  FROM dbo.DimTaxon"

list_species <-
  sqlQuery(connection_vis, query_species, stringsAsFactors = FALSE) %>%
  rename(
    taxonkey = TaxonKey, taxoncode = Taxoncode,
    taxonname = WetenschappelijkeNaam, dutch_name = Soort
  )

query_taxonmetrics <-
  "SELECT *
  FROM visindex.data_taxonmetrics;"

data_taxonmetrics <-
  sqlQuery(connection_vis, query_taxonmetrics, stringsAsFactors = FALSE) %>%
  select(
    -data_taxonmetricsKey, -wetenschappelijke_naam, -soort, taxonkey = taxonKey,
    -opmerking
  )

data_taxonmetrics %>%
  count(taxoncode) %>%
  filter(n > 1)

query_brabeel <-
  "SELECT *
  FROM visindex.brabeel_metrieken;"

data_brabeel <-
  sqlQuery(connection_vis, query_brabeel, stringsAsFactors = FALSE) %>%
  select(
    -brabeel_metriekenKey, -Wetenschappelijke_naam_brabeel,
    -Soortenlijst_brabeel
  ) %>%
  rename(taxoncode = TaxonCode, taxonkey = TaxonKey)

data_brabeel %>%
  count(taxoncode) %>%
  filter(n > 1)

query_brongebied <-
  "SELECT *
  FROM visindex.brongebied_metrieken;"

data_brongebied <-
  sqlQuery(connection_vis, query_brongebied, stringsAsFactors = FALSE) %>%
  select(
    -brongebied_metriekenKey, -Wetenschappelijke_naam_brongebied,
    -Soortenlijst_brongebied
  ) %>%
  rename(taxonkey = TaxonKey, taxoncode = TaxonCode)

data_brongebied %>%
  count(taxoncode) %>%
  filter(n > 1)

query_getijde_zijrivieren_zoet <-
  "SELECT *
  FROM visindex.getijde_zijrivieren_zoet;"

data_getijde_zijrivieren_zoet <- sqlQuery(
  connection_vis, query_getijde_zijrivieren_zoet, stringsAsFactors = FALSE
) %>%
  select(
    -getijde_zijrivieren_zoetKey,
    -Wetenschappelijke_naam_Getijde_zijrivieren_zoet,
    -Soortenlijst_Getijde_rivieren_zoet
  ) %>%
  rename(
    taxonkey = TaxonKey,
    taxoncode = TaxonCode
  )

data_getijde_zijrivieren_zoet %>%
  count(taxoncode) %>%
  filter(n > 1)

query_ijzer <-
  "SELECT *
  FROM visindex.ijzerestuarium_metrieken;"

data_ijzer <-
  sqlQuery(connection_vis, query_ijzer, stringsAsFactors = FALSE) %>%
  select(
    -ijzerestuarium_metriekenKey, -WetenschappelijkeNaam, taxonkey = TaxonKey,
    -NederLandseNaam, -opmerking
  )

data_ijzer %>%
  count(taxoncode) %>%
  filter(n > 1)

query_mesohalien <-
  "SELECT *
  FROM visindex.zeeschelde_mesohalien_metrieken;"

data_mesohalien <-
  sqlQuery(connection_vis, query_mesohalien, stringsAsFactors = FALSE) %>%
  select(
    -zeeschelde_mesohalien_metriekenKey, taxonkey = TaxonKey, -NederlandseNaam,
    -WetenschappelijkeNaam, -opmerkingen
  )

data_mesohalien %>%
  count(taxoncode) %>%
  filter(n > 1)

query_oligohalien <-
  "SELECT *
  FROM visindex.zeeschelde_oligohalien_metrieken;"

data_oligohalien <-
  sqlQuery(connection_vis, query_oligohalien, stringsAsFactors = FALSE) %>%
  select(
    -zeeschelde_oligohalien_metriekenKey, taxonkey = TaxonKey, -NederlandseNaam,
    -WetenschappelijkeNaam
  )

data_oligohalien %>%
  count(taxoncode) %>%
  filter(n > 1)

query_zeeschelde_zoet <-
  "SELECT *
  FROM visindex.zeeschelde_zoet_metrieken;"

data_zeeschelde_zoet <-
  sqlQuery(connection_vis, query_zeeschelde_zoet, stringsAsFactors = FALSE) %>%
  select(
    -zeeschelde_zoet_metriekenKey, -Wetenschappelijke_Naam_Zeeschelde_zoet,
    -Nederlandse_naam, -opmerking
  ) %>%
  rename(
    taxonkey = TaxonKey, taxoncode = TaxonCode
  )

data_zeeschelde_zoet %>%
  count(taxoncode) %>%
  filter(n > 1)

odbcClose(connection_vis)

data_taxonmetrics <- data_taxonmetrics %>%
  full_join(
    data_brabeel,
    by = c("taxonkey", "taxoncode")
  ) %>%
  full_join(
    data_brongebied,
    by = c("taxonkey", "taxoncode")
  ) %>%
  full_join(
    data_getijde_zijrivieren_zoet,
    by = c("taxonkey", "taxoncode")
  ) %>%
  full_join(data_ijzer, by = c("taxonkey", "taxoncode")) %>%
  full_join(data_mesohalien, by = c("taxonkey", "taxoncode")) %>%
  full_join(data_oligohalien, by = c("taxonkey", "taxoncode")) %>%
  full_join(
    data_zeeschelde_zoet,
    by = c("taxonkey", "taxoncode")
  ) %>%
  mutate_if(is.numeric, coalesce, 0) %>%
  filter(.data$taxoncode != "HYB.HYB.") %>%
  arrange(taxoncode) %>%
  left_join(list_species, by = c("taxonkey", "taxoncode"))

write_csv2(data_taxonmetrics, "inst/extdata/data_taxonmetrics.csv")
