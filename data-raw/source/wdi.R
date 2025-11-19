## code to prepare `economy_wide` variables for wage diagnostics
library(purrr)

idvar_list <- list(
    "gdp_lcu" = "WB_WDI_NY_GDP_MKTP_CN",
    "pexpenditure_lcu" = "WB_WDI_GC_XPN_TOTL_CN",
    "prevenue_lcu" = "WB_WDI_GC_REV_XGRT_CN",
    "taxrevenue_lcu" = "WB_WDI_GC_TAX_TOTL_CN",
    "emp_pop_rate" = "WB_WDI_SL_EMP_TOTL_SP_NE_ZS",
    "labor_force_total" = "WB_WDI_SL_TLF_TOTL_IN",
    "labor_force_advanced_edu" = "WB_WDI_SL_TLF_ADVN_ZS",
    "tot_pop" = "WB_WDI_SP_POP_TOTL",
    "salaried_rate" = "WB_WDI_SL_EMP_WORK_ZS",
    "government_expenditure_gdp" = "WB_WDI_NE_CON_GOVT_ZS",
    "cpi" = "WB_WDI_FP_CPI_TOTL",
    "ppp" = "WB_WDI_PA_NUS_PRVT_PP"
)

wdi <- lapply(
    X = idvar_list,
    FUN = get_data360_api,
    dataset_id = "WB_WDI"
) |>
    reduce(left_join) |>
    as_tibble() |>
    setNames(c("country_code", "year", names(idvar_list)))

fiscal_balance <- get_data360_api(
    dataset_id = "WB_MPO",
    indicator_id = "WB_MPO_GGBALOVRLCD_",
    pivot = FALSE
) |>
    # extract latest vintage
    filter(
        COMP_BREAKDOWN_1 == "WB_MPO_VINTAGE_SM_2025"
    ) |>
    pivot_data360() |>
    rename(
        fiscal_balance = wb_mpo_ggbalovrlcd
    )

wdi <-
    wdi |>
    left_join(
        fiscal_balance,
        by = c("country_code", "year")
    ) |>
    mutate(
        across(-c(country_code), as.numeric)
    )

wdi <-
    wdi |>
    mutate(emp_pop = emp_pop_rate * tot_pop)

wdi <-
    wdi |>
    mutate(salaried_pop = salaried_rate * emp_pop)

usethis::use_data(wdi, overwrite = TRUE)
