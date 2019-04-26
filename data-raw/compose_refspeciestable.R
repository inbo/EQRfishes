# This script composes a table with reference species for determining metrics

library(RODBC)
library(tidyverse)
library(readr)

connection_vis <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;"
  )

query_taxonmetrics <-
  "SELECT vit.Taxoncode, vit.Totaal, vit.Exoot, vit.WF_Tolerantie,
    vit.WF_Type_Brasem, vit.WF_Type_Barbeel, vit.WF_Type_Upstream,
    vit.Grootte_Klasse_zoetwater, vit.Grootte_Klasse_upstream,
    vit.Shannon_Weaner, vit.Migratie, vit.Spec_Paaier, vit.Bentisch,
    vit.Invertivoor, vit.Omnivoor, vit.Piscivoor, vit.Piscivoor_Opm,
    vit.Invertivoor_Opm, vit.Invertivoor_upstream, vit.Water_Anadroom,
    vit.Water_Brak, vit.Water_Zoet, vit.Nat_Recrutering, vit.Recr_Grensw1,
    vit.Recr_Grensw2, vit.Recr_Grensw3, vit.Recr_Grensw4, vit.Recr_Grensw5,
    vit.Gkla_Grenswa1, Gkla_Grenswa3, Gkla_Grenswa5
  FROM DimVisindexTaxon vit;"

data_taxonmetrics <-
  sqlQuery(connection_vis, query_taxonmetrics, stringsAsFactors = FALSE)

odbcClose(connection_vis)

write_csv2(data_taxonmetrics, "inst/extdata/data_taxonmetrics.csv")
