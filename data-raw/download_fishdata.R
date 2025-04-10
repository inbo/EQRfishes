# This script downloads measurement data on fishes that serve as test data

library(RODBC)
library(tidyverse)
library(lubridate)
connection_VIS <-
  odbcDriverConnect(
    "Driver=SQL Server;Server=INBO-SQL08-PRD.inbo.be;Database=W0001_00_Vis;Trusted_Connection=Yes;"
  )

query_sample <-
  "SELECT dw.Begindatum, dw.Einddatum, dw.WaarnemingKey as sample_key,
    dw.GebiedID AS LocationID,
    dw.BreedteTransect AS width_transect, dw.LengteTraject AS length_trajectory,
    dG.Breedte AS width_river, dG.Helling AS slope,
    dW.AantalFuiken AS n_fyke_nets, dW.AantalDagen AS n_days,
    dM.Methodecode AS method, dG.Stilstaand, dG.Getijdewater AS tidal,
    dG.Bekken, dG.Brak, dW.IndexTypeCode, dW.Projectcode
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
    AND dV.Variabelegroep LIKE 'Hugo%'
    AND (FM.AfvisBeurtNrKey <= 1 OR FM.AfVisBeurtNrKey IS NULL);"

data_fish <-
  sqlQuery(connection_VIS, query_fish, stringsAsFactors = FALSE)

odbcClose(connection_VIS)

data_sample <- data_sample %>%
  mutate(
    year_begin = year(.data$Begindatum),
    year_end = year(.data$Einddatum),
    season_begin =
      ifelse(
        month(.data$Begindatum) %in% 3:5,
        "spring",
        ifelse(
          month(.data$Begindatum) %in% 6:8,
          "summer",
          ifelse(
            month(.data$Begindatum) %in% 9:11,
            "autumn",
            NA
          )
        )
      ),
    season_end =
      ifelse(
        month(.data$Einddatum) %in% 3:5,
        "spring",
        ifelse(
          month(.data$Einddatum) %in% 6:8,
          "summer",
          ifelse(
            month(.data$Einddatum) %in% 9:11,
            "autumn",
            NA
          )
        )
      )
  ) %>%
  filter( #data from winter are not used
    !is.na(.data$season_begin), !is.na(.data$season_end),
    .data$year_begin == .data$year_end,
    .data$season_begin == .data$season_end
  ) %>%
  mutate(
    year = .data$year_begin,
    season = .data$season_begin,
    sample_key_new =
      paste(
        .data$season, .data$year, "loc", .data$LocationID, "method",
        .data$method, sep = "_"
      )
  ) %>%
  select(
    -"year_begin", -"year_end", -"season_begin", -"season_end",
    -"Begindatum", -"Einddatum"
  )

data_sample_new <- data_sample %>%
  group_by(
    .data$sample_key_new, .data$LocationID, .data$width_river, .data$slope,
    .data$method, .data$Stilstaand, .data$tidal, .data$Bekken, .data$Brak,
    .data$IndexTypeCode
  ) %>%
  summarise(
    surface = sum(.data$width_transect * .data$length_trajectory),
    length_trajectory = sum(.data$length_trajectory),
    n_fyke_nets = sum(.data$n_fyke_nets),
    n_days = sum(.data$n_days)
  ) %>%
  ungroup() %>%
  mutate(
    width_transect = .data$surface / .data$length_trajectory,
    surface = NULL,
    sample_key = .data$sample_key_new,
    sample_key_new = NULL
  )

key_translation <- data_sample %>%
  select("sample_key", "sample_key_new") %>%
  distinct()

data_fish <- data_fish %>%
  spread(key = .data$Variabelecode, value = .data$Waarde) %>%
  transmute(
    sample_key = .data$WaarnemingKey, record_id = .data$MetingID,
    taxoncode = .data$Taxoncode,
    number = .data$TAXONAANTAL,
    length = .data$TAXONLEN,
    weight = ifelse(!is.na(.data$TAXONGEW), .data$TAXONGEW, .data$TAXONTOTGEW)
  ) %>%
  left_join(key_translation, by = "sample_key")
save(data_sample, data_fish, data_sample_new,
     file = "inst/extrafiles/visdata.Rdata")

data_sample_freshwater <- data_sample %>%
  filter(
    .data$sample_key %in%
      c(
        2258, 4550, 8434, 8507, 8681, 11611, 11652, 13282, 13384, 13512, 13534,
        13561
      )
  )
write.csv2(
  data_sample_freshwater, file = "inst/testdata/freshwater_sample.csv",
  row.names = FALSE
)
data_fish_freshwater <- data_fish %>%
  filter(
    .data$sample_key %in%
      c(
        2258, 4550, 8434, 8507, 8681, 11611, 11652, 13282, 13384, 13512, 13534,
        13561
      )
  )
write.csv2(
  data_fish_freshwater, file = "inst/testdata/freshwater_fish_data.csv",
  row.names = FALSE
)

data_sample_estuaries <- data_sample %>%
  filter(
    .data$sample_key %in%
      c(
        9877, 9878, 9900:9903, 9906, 9907, 10260:10266, 10268, 10269, 10276,
        10277, 10347:10356, 10392, 10393, 10398:10401, 11274
      )
  )
write.csv2(
  data_sample_estuaries, file = "inst/testdata/estuaries_sample.csv",
  row.names = FALSE
)
data_fish_estuaries <- data_fish %>%
  filter(
    .data$sample_key %in%
      c(
        9877, 9878, 9900:9903, 9906, 9907, 10260:10266, 10268, 10269, 10276,
        10277, 10347:10356, 10392, 10393, 10398:10401, 11274
      )
  )
write.csv2(
  data_fish_estuaries, file = "inst/testdata/estuaries_fish_data.csv",
  row.names = FALSE
)

data_sample_canals <- data_sample %>%
  filter(
    .data$sample_key %in%
      c(13207:13217, 13279, 13280, 13285:13292, 13294:13296, 13387)
  )
write.csv2(
  data_sample_canals, file = "inst/testdata/canals_sample.csv",
  row.names = FALSE
)
data_fish_canals <- data_fish %>%
  filter(
    .data$sample_key %in%
      c(13207:13217, 13279, 13280, 13285:13292, 13294:13296, 13387)
  )
write.csv2(
  data_fish_canals, file = "inst/testdata/canals_fish_data.csv",
  row.names = FALSE
)
