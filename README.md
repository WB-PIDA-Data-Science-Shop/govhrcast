
<!-- README.md is generated from README.Rmd. Please edit that file -->

# govhrcast

<!-- badges: start -->

<!-- badges: end -->

Designing a new retirement policy? Planning a hiring freeze? Considering
a civil service pay reform? govhrcast is an R package that lets you
simulate the workforce and wage bill implications of public sector
policy decisions from HRMIS microdata at the contract and personnel
level. See \[govhr\]
(<https://github.com/WB-PIDA-Data-Science-Shop/govhr>) package for the
pre-requisite data requirements.

The package provides a modular simulation engine built around a single
projection function, simulate_horizon(), which chains four workforce
modules — retirement, non-retirement exits, hiring, and
promotions/transfers — over successive time periods. Each module is
independently configurable, so you can hold some processes at their
historical baseline while reforming others.

## Installation

You can install the development version of govhrcast from
[GitHub](https://github.com/WB-PIDA-Data-Science-Shop/govhrcast) with:

``` r

# Install the development version from GitHub
remotes::install_github("WB-PIDA-Data-Science-Shop/govhrcast")
```

## Quick start

The package ships with harmonized example data from hypothetical
Brazilian state government HRMIS. A minimal status quo projection
requires three inputs: a contract module, a personnel module as well as
a supplementary allowance module.

``` r
library(govhrcast)
library(data.table)

# Contract module — one row per contract per snapshot
head(bra_hrmis_contract[ref_date == as.Date("2017-09-01"),
                         .(personnel_id, contract_id, est_id, paygrade,
                           contract_type, gross_salary_lcu)])
#>    personnel_id contract_id                             est_id paygrade
#>          <char>      <char>                             <char>   <char>
#> 1:  10128158468      133603                ALAGOAS PREVIDENCIA     <NA>
#> 2:  10139748407      168900         POLICIA MILITAR DE ALAGOAS     <NA>
#> 3:  10142013498      157463   SECRETARIA DE ESTADO DA EDUCACAO     <NA>
#> 4:   1021134406      142348         POLICIA MILITAR DE ALAGOAS     <NA>
#> 5:   1029262470      108796      SECRETARIA DE ESTADO DA SAUDE        A
#> 6:    103891480      100370 POLICIA CIVIL DO ESTADO DE ALAGOAS        C
#>    contract_type gross_salary_lcu
#>           <char>            <num>
#> 1:          <NA>          1578.04
#> 2:     permanent          3633.85
#> 3:    short-term           600.00
#> 4:     permanent          4060.24
#> 5:     permanent          2136.14
#> 6:     permanent          8046.56

# Personnel module — one row per person per snapshot
head(bra_hrmis_personnel[ref_date == as.Date("2017-09-01"),
                          .(personnel_id, age, personnel_tenure,
                            employment_status)])
#>    personnel_id      age personnel_tenure employment_status
#>          <char>    <num>            <num>            <char>
#> 1:  10128158468 60.82683         7.397673         pensioner
#> 2:  10139748407 46.31075         1.314168            active
#> 3:  10142013498 68.40520         2.261465            active
#> 4:   1021134406 34.96509         6.924025            active
#> 5:   1029262470 36.33949        11.452430            active
#> 6:    103891480 42.46407        13.785079            active

# Allowance module - one row per person per contract per allowance type per snapshot
head(bra_hrmis_allowance[ref_date == as.Date("2017-09-01"),
                         .(personnel_id, contract_id, allowance_type, allowance_lcu)])
#>    personnel_id contract_id                    allowance_type allowance_lcu
#>          <char>      <char>                            <char>        <char>
#> 1:  21060525453       34448           ADICIONAL_TEMPO_SERVICO             0
#> 2:  21060525453       34448                          COMISSAO             0
#> 3:  21060525453       34448                 ABONO_PERMANENCIA             0
#> 4:  21060525453       34448     DEMAIS_GRATIFICACOES_CARREIRA             0
#> 5:  21060525453       34448 DEMAIS_GRATIFICACOES_TRANSITORIAS             0
#> 6:  78750962434      144215           ADICIONAL_TEMPO_SERVICO             0
```

## A Status Quo Projection

The status quo run holds every process at its historically observed
rate. Retirement follows an age threshold, exits use establishment-level
attrition rates estimated from the panel, hiring replicates historical
intake rates, and promotions/transfers replicate the observed transition
matrix.

``` r

# Salary scale: mean salary per establishment x paygrade at baseline
salary_scale_dt <- bra_hrmis_contract[
  ref_date == as.Date("2017-09-01"),
  .(gross_salary_lcu = mean(gross_salary_lcu, na.rm = TRUE)),
  by = .(est_id, paygrade)
]

# Historical exit rates — one row per establishment
exit_dt <- estimate_historical_exit_rates(
  panel_contract_dt  = bra_hrmis_contract,
  panel_personnel_dt = bra_hrmis_personnel,
  group_cols         = "est_id"
)

# Historical movement baseline
movement_dt <- govhrcast:::estimate_movement_baseline(
  contract_dt = bra_hrmis_contract,
  group_cols  = c("est_id", "paygrade")
)

statusquo_sim <- simulate_horizon(
  contract_dt     = bra_hrmis_contract,
  personnel_dt    = bra_hrmis_personnel,
  salary_scale_dt = salary_scale_dt,

  retirement_policy = list(
    group_cols   = NULL,
    policy_table = NULL,
    defaults     = list(
      eligibility_type = "age_only",
      min_age          = 60,
      pension_type     = "rate",
      pension_rate     = 0.15,
      ref_wage_col     = "gross_salary_lcu",
      active_types     = c("permanent", "short-term", "fixed-term")
    )
  ),

  exit_policy = list(
    group_cols   = "est_id",
    policy_table = exit_dt,
    defaults     = list(
      exit_rate    = mean(exit_dt$exit_rate, na.rm = TRUE),
      active_types = c("permanent", "short-term", "fixed-term")
    )
  ),

  movement_policy = list(
    group_cols   = c("est_id", "paygrade"),
    policy_table = movement_dt,
    defaults     = list(
      movement_rate      = 0,
      movement_strategy  = "tenure",
      active_types       = "permanent",
      salary_update_rule = "scale"
    )
  ),

  hiring_policy = list(
    mode         = "status_quo",
    group_cols   = c("est_id", "paygrade"),
    salary_scale = salary_scale_dt,
    rate_mult    = 1
  ),

  salary_growth_rate = 0.04,
  ref_date           = as.Date("2017-09-01"),
  age_col            = "age",
  tenure_col         = "personnel_tenure",
  hire_date_col      = "first_employment_date",
  n_periods          = 5,
  salary_col         = "gross_salary_lcu",
  scenario_name      = "status_quo"
)
#> Scrubbed 8 transition(s) where to_group not found in salary_scale_dt.
#> Scrubbed 8 transition(s) where to_group not found in salary_scale_dt.
#> Scrubbed 7 transition(s) where to_group not found in salary_scale_dt.
#> Scrubbed 7 transition(s) where to_group not found in salary_scale_dt.
#> Scrubbed 6 transition(s) where to_group not found in salary_scale_dt.
```

### Projection output

simulate_horizon() returns a horizon object. The \$summary_dt element
contains one row per simulation period with key workforce and wage bill
statistics.

``` r

statusquo_sim$summary_dt
```

### How it works

simulate_horizon() chains four options modules (set any to NULL if you
prefer) over each projection period in a fixed sequence:

``` mermaid
flowchart TD
    A["Contract & Personnel data"] --> B["simulate_retirement()"]
    B --> C["simulate_exits()"]
    C --> D["simulate_hiring()"]
    D --> E["simulate_promotions_transfers()"]
    E --> F{Next period?}
    F -- Yes --> B
    F -- No --> G["horizon object"]
```

Each module is independently configurable through a policy list and
leaves the others unchanged, so you can reform one dimension of the
workforce system while holding everything else at its baseline.

| Module | What it models |
|----|----|
| simulate_retirement() | Age- and/or tenure-based retirement eligibility; defined benefit, defined contribution, rate, flat, and hybrid pension formulas |
| simulate_exits() | Non-retirement attrition at scalar or group-varying rates; random or rank-based exit selection |
| simulate_hiring() | Demand-driven hiring under four modes: status quo, flow (retirement replacement), stock (headcount target), and combined |
| simulate_promotions_transfers() | Vertical promotions and lateral transfers via empirical transition matrices or user-specified rates; four mover selection strategies |

## What can you model:

govhrcast is designed around the kinds of questions that arise in public
sector fiscal and workforce reform work:

\*\* Fiscal consolidation \*\* — what happens to the wage bill if we
freeze hiring for three years and rely on natural attrition to reduce
headcount? \*\* Retirement waves \*\* — how many workers will become
eligible for retirement over the next decade, and what is the pension
liability? \*\* Replacement policy \*\* — if we replace only 80% of
retirees, how long until headcount stabilises, and at what wage bill
level? \*\* Pay reform \*\* — how does a salary scale restructuring
interact with planned hiring and expected attrition over five years?
\*\* Promotion acceleration \*\* — what is the wage bill cost of
doubling promotion rates into senior grades as part of a civil service
reform? Scenarios are compared by running simulate_horizon() multiple
times with different policy lists and inspecting the \$comparison
element of the returned horizon object.

## Policy specification

Every module follows the same three-slot policy format:

``` r

list(
  group_cols   = "est_id",          # grouping variables — NULL for uniform policy
  policy_table = exit_dt,           # group-specific overrides — NULL for defaults only
  defaults     = list(              # fallback values for all groups
    exit_rate    = 0.05,
    active_types = c("permanent", "short-term", "fixed-term")
  )
)
#> $group_cols
#> [1] "est_id"
#> 
#> $policy_table
#>                                                           est_id   exit_rate
#>                                                           <char>       <num>
#>  1:                                SECRETARIA DE ESTADO DA SAUDE 0.039566450
#>  2:                           POLICIA CIVIL DO ESTADO DE ALAGOAS 0.019717482
#>  3:                        CORPO DE BOMBEIROS MILITAR DE ALAGOAS 0.020431065
#>  4:                             SECRETARIA DE ESTADO DA EDUCACAO 0.059119190
#>  5:    SEC DE ESTADO DA AGRICULTURA PECUARIA PESCA E AQUICULTURA 0.076620370
#>  6:        COMPANHIA ALAGOANA DE RECURSOS HUMANOS E PATRIMONIAIS 0.189764556
#>  7:                              SECRETARIA DE ESTADO DA FAZENDA 0.051882620
#>  8:                        SERVICOS DE ENGENHARIA DE ALAGOAS S/A 0.111111111
#>  9:                          DEPARTAMENTO DE ESTRADAS DE RODAGEM 0.042460317
#> 10: INSTITUTO DE ASSISTENCIA A SAUDE DOS SERVIDORES DO ESTADO DE 0.296296296
#> 11:        UNIVERSIDADE ESTADUAL DE CIENCIAS DA SAUDE DE ALAGOAS 0.061099092
#> 12:      INSTITUTO DE TECNOLOGIA EM INFORMATICA E INF DE ALAGOAS 0.118055556
#> 13:                    SECRETARIA DE ESTADO DA SEGURANCA PUBLICA 0.059817239
#> 14: SECRETARIA DE ESTADO DA ASSISTENCIA E DESENVOLVIMENTO SOCIAL 0.380952381
#> 15:     SECRETARIA DE ESTADO DO PLANEJAMENTO GESTAO E PATRIMONIO 0.161748877
#> 16:                                   POLICIA MILITAR DE ALAGOAS 0.043631685
#> 17:                                          ALAGOAS PREVIDENCIA 0.204365079
#> 18:    SECRETARIA DE ESTADO DA MULHER CIDADANIA DIREITOS HUMANOS 0.191666667
#> 19:                          SECRETARIA DE ESTADO DA COMUNICACAO 0.125000000
#> 20:                 DEPARTAMENTO ESTADUAL DE TRANSITO DE ALAGOAS 0.026251526
#> 21:                   SECRETARIA DE ESTADO DO TRABALHO E EMPREGO 0.083333333
#> 22:                                CONTROLADORIA GERAL DO ESTADO 0.285714286
#> 23:                                 INSTITUTO ZUMBI DOS PALMARES 0.044444444
#> 24:              INSTITUTO DO MEIO AMBIENTE DO ESTADO DE ALAGOAS 0.083333333
#> 25:                                  GABINETE DO VICE GOVERNADOR 0.090909091
#> 26:  SECRETARIA DE ESTADO DA CIENCIA DA TECNOLOGIA E DA INOVACAO 0.027777778
#> 27:                                 PROCURADORIA GERAL DO ESTADO 0.027777778
#> 28:                             UNIVERSIDADE ESTADUAL DE ALAGOAS 0.123313492
#> 29:                     FUNDACAO DE AMPARO A PESQUISA DE ALAGOAS 0.075757576
#> 30:   SECRETARIA DE ESTADO DO PLANEJAMENTO E DESENVOLV ECONOMICO 0.106250000
#> 31:    SECRETARIA DE ESTADO DO MEIO AMBIENTE E RECURSOS HIDRICOS 0.058333333
#> 32:            SECRETARIA DE ESTADO DO DESENVOLVIMENTO ECONOMICO 0.083333333
#> 33:                      SECRETARIA DE ESTADO DA INFRA ESTRUTURA 0.110119048
#> 34:  SECRETARIA DE ESTADO DO DESENVOLVIMENTO ECONOMICO E TURISMO 0.272727273
#> 35:                                  GABINETE MILITAR DO GOVERNO 0.000000000
#> 36:                DEFENSORIA PUBLICA GERAL DO ESTADO DE ALAGOAS 0.366666667
#> 37: AGENCIA REGULADORA DE SERVICOS PUBLICOS DO ESTADO DE ALAGOAS 0.666666667
#> 38:                                               GABINETE CIVIL 0.281250000
#> 39:               INSTITUTO DE METROLOGIA E QUALIDADE DE ALAGOAS 0.250000000
#> 40:  AGENCIA DE DEFESA E INSPECAO AGROPECUARIA ESTADO DE ALAGOAS 0.018181818
#> 41:                                AGENCIA DE FOMENTO DE ALAGOAS 0.100000000
#> 42:                              SECRETARIA DE ESTADO DA CULTURA 0.500000000
#> 43:                    DIRETORIA DE TEATROS DO ESTADO DE ALAGOAS 0.166666667
#> 44:                         PERICIA OFICIAL DO ESTADO DE ALAGOAS 0.031250000
#> 45:                                       GABINETE DO GOVERNADOR 0.000000000
#> 46:        SUPERINTENDENCIA GERAL DE ADMINISTRACAO PENITENCIARIA 0.030303030
#> 47:                     PROCURADORIA JUNTO AO TRIBUNAL DE CONTAS 0.500000000
#> 48:                SECRETARIA DE ESTADO DE PREVENCAO A VIOLENCIA 0.222222222
#> 49:               AGENCIA DE MODERNIZACAO DA GESTAO DE PROCESSOS 0.071428571
#> 50:    SECRETARIA DE ESTADO DE RESSOCIALIZACAO E INCLUSAO SOCIAL 0.009090909
#> 51:            SECRETARIA DE ESTADO DO ESPORTE LAZER E JUVENTUDE 0.000000000
#> 52:        INSTITUTO DE INOVACAO PARA O DESENV RURAL SUSTENTAVEL 0.500000000
#> 53:  SECRETARIA DE ESTADO DE TRANSPORTE E DESENVOLVIMENTO URBANO 0.000000000
#>                                                           est_id   exit_rate
#>                                                           <char>       <num>
#> 
#> $defaults
#> $defaults$exit_rate
#> [1] 0.05
#> 
#> $defaults$active_types
#> [1] "permanent"  "short-term" "fixed-term"
```

Groups absent from policy_table automatically inherit the corresponding
value from defaults. This means you can calibrate policy for the
establishments you have data for and fall back gracefully for the rest.

## Documentation

Full function-level documentation is available via ?simulate_horizon,
?simulate_retirement, ?simulate_exits, ?simulate_hiring, and
?simulate_promotions_transfers.

Vignettes covering status quo projections, reform scenario comparisons,
and pension liability estimation are in development.

## License

MIT © The Authors
