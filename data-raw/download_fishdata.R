# This script downloads measurement data on fishes that serve as test data

library(RODBC)
library(tidyverse)
connection_VIS <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;"
  )

query_sample <-
  "SELECT dw.Begindatum, dw.Einddatum, dw.WaarnemingKey as sample_key,
    dw.BreedteTransect AS width_transect, dw.LengteTraject AS length_trajectory,
    dw.BreedteRivier AS width_river, dG.Helling AS slope,
    dW.AantalFuiken AS n_fyke_nets, dW.AantalDagen, dM.Methodecode AS method,
    dG.Stilstaand, dG.Getijdewater AS tidal, dG.Bekken, dG.Brak
  FROM DimWaarneming dW
    LEFT JOIN DimGebied dG
      ON dW.GebiedKey = dG.GebiedKey
    LEFT JOIN DimMethode dM
      ON dW.MethodeKey = dM.MethodeKey
  WHERE dw.IsCurrent = 1 AND dG.IsCurrent = 1;"

data_sample <-
  sqlQuery(connection_VIS, query_sample, stringsAsFactors = FALSE)

query_fish <-
  "SELECT FM.WaarnemingKey, FM.MetingID, dV.Variabelecode, FM.Waarde, dT.Taxoncode
  FROM FactMeting FM
    INNER JOIN DimVariabele dV
      ON FM.VariabeleKey = dV.VariabeleKey
    LEFT JOIN DimTaxon dT
      ON FM.TaxonKey = dT.TaxonKey
  WHERE FM.IsCurrent = 1 AND dV.IsCurrent = 1
    AND dV.Variabelegroep LIKE 'Hugo%';"

data_fish <-
  sqlQuery(connection_VIS, query_fish, stringsAsFactors = FALSE)
data_fish <- data_fish %>%
  spread(key = Variabelecode, value = Waarde) %>%
  transmute(
    sample_key = WaarnemingKey, record_id = MetingID,
    taxoncode = Taxoncode,
    number = TAXONAANTAL,
    length = TAXONLEN,
    weight = ifelse(!is.na(TAXONGEW), TAXONGEW, TAXONTOTGEW)
  )

odbcClose(connection_VIS)

save(data_sample, data_fish, file = "inst/extrafiles/visdata.Rdata")
