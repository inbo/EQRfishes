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
  "SELECT vit.Taxoncode AS taxoncode, vit.Totaal, vit.Exoot, vit.WF_Tolerantie,
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

# data_taxonmetrics %<>%
#   mutate(
#     Totaal = ifelse(.data$Totaal == 0, NA, .data$Totaal),
#     Exoot = ifelse(.data$Exoot == 0, NA, .data$Exoot),
#     Grootte_Klasse_zoetwater =
#       ifelse(
#         .data$Grootte_Klasse_zoetwater == 0, NA, .data$Grootte_Klasse_zoetwater
#       ),
#     Grootte_Klasse_upstream =
#       ifelse(
#         .data$Grootte_Klasse_upstream == 0, NA, .data$Grootte_Klasse_upstream
#       ),
#     Spec_Paaier = ifelse(.data$Spec_Paaier == 0, NA, .data$Spec_Paaier),
#     Invertivoor = ifelse(.data$Invertivoor == 0, NA, .data$Invertivoor),
#     Omnivoor = ifelse(.data$Omnivoor == 0, NA, .data$Omnivoor),
#     Piscivoor = ifelse(.data$Piscivoor == 0, NA, .data$Piscivoor),
#     Invertivoor_upstream =
#       ifelse(.data$Invertivoor_upstream == 0, NA, .data$Invertivoor_upstream),
#     Water_Anadroom =
#       ifelse(.data$Water_Anadroom == 0, NA, .data$Water_Anadroom),
#     Water_Brak = ifelse(.data$Water_Brak == 0, NA, .data$Water_Brak),
#     Water_Zoet = ifelse(.data$Water_Zoet == 0, NA, .data$Water_Zoet),
#     Nat_Recrutering =
#       ifelse(.data$Nat_Recrutering == 0, NA, .data$Nat_Recrutering)
#   ) %>%
#   gather(key = listname, value = value, -taxoncode, na.rm = TRUE) %>%
#   bind_rows(
#     data.frame(
#       taxoncode = c("LEU.CEP.", "BAR.BUS.", "GAS.ACU."),
#       listname = c("LeuCep", "BarBus", "GasAcu"),
#       value = 1,
#       stringsAsFactors = FALSE
#     )
#   )

data_taxonmetrics %<>%
  mutate(
    LeuCep = ifelse(.data$taxoncode == "LEU.CEP.", 1, 0),
    BarBus = ifelse(.data$taxoncode == "BAR.BUS.", 1, 0),
    GasAcu = ifelse(.data$taxoncode == "GAS.ACU", 1, 0),
    Canals =
      ifelse(
        .data$taxoncode %in%
          c("ABR.BRA.", "ALB.ALB.", "ANG.ANG.", "BLI.BJO.", "CAR.CAR.",
            "CAR.AUG.", "CYP.CAR.", "ESO.LUC.", "GAS.ACU.", "GOB.GOB.",
            "GYM.CER.", "LEU.DEL.", "LEU.IDE.", "PER.FLU.", "PLA.FLE.",
            "PUN.PUN.", "RHO.SER.", "RUT.RUT.", "STI.LUC.", "SCA.ERY.",
            "SIL.GLA.", "LEU.CEP.", "TIN.TIN."),
        1, 0
      ),
    Lakes =
      ifelse(
        .data$taxoncode %in%
          c("ABR.BRA.", "ANG.ANG.", "BLI.BJO.", "CAR.CAR.", "CAR.AUG.",
            "CYP.CAR.", "ESO.LUC.", "GAS.ACU.", "GOB.GOB.", "GYM.CER.",
            "LEU.DEL.", "LEU.IDE.", "LOT.LOT.", "PER.FLU.", "PUN.PUN.",
            "RHO.SER.", "RUT.RUT.", "STI.LUC.", "SCA.ERY.", "SIL.GLA.",
            "TIN.TIN."),
        1, 0
      ),
    BenWei =
      ifelse(
        .data$taxoncode %in%
          c("ABR.BRA.", "BLI.BJO.", "CYP.CAR.", "GYM.CER.", "TIN.TIN."),
        1, 0
      ),
    Obligatory_species =
      ifelse(
        .data$taxoncode %in%
          c("ABR.BRA.", "BLI.BJO.", "ESO.LUC.", "GYM.CER.", "PER.FLU.",
            "RUT.RUT.", "SCA.ERY."),
        1, 0
      )
  )

write_csv2(data_taxonmetrics, "inst/extdata/data_taxonmetrics.csv")
