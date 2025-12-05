## code to prepare `rais_mun` dataset goes here
# available in: https://basedosdados.org/dataset/3e7c4d58-96ba-448e-b053-d385a829ef00?table=dabe5ea8-3bb5-4a3e-9d5a-3c7003cd4a60
# access date: 11/24/2025
library(basedosdados)
library(dplyr)

set_billing_id(
  Sys.getenv("billing_id")
)

rais_input <- bdplyr("br_me_rais.microdados_vinculos")

rais_query <- rais_input |>
  filter(
    natureza_juridica == "1031" &
      ano >= 2003
  ) |>
  mutate(
    new_hire = if_else(tipo_admissao == "1", 1, 0),
    dismissal = if_else(motivo_desligamento == "10" | motivo_desligamento == "11", 1, 0)
  ) |>
  group_by(
    id_municipio,
    ano
  ) |>
  summarise(
    total_headcount = n(),
    total_new_hire = sum(new_hire, na.rm = TRUE),
    total_dismissed = sum(dismissal, na.rm = TRUE)
  ) |>
  mutate(
    share_new_hire = total_new_hire/total_headcount,
    share_dismissed = total_dismissed/total_headcount
  ) |>
  ungroup()

rais_mun <- rais_query |>
  bd_collect()

usethis::use_data(rais_mun, overwrite = TRUE)
