library(shiny)
library(tidyverse)
library(mstate)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(plotly)
library(gtsummary)
library(gt)
library(labelled)
library(glue)
library(stringr)

########### Publicar app ############
# Las credenciales NUNCA deben ir en el código fuente, ni siquiera comentadas.
# Configura la cuenta UNA VEZ desde la consola de RStudio (no queda guardado
# en este script), leyendo los valores desde tu .Renviron local (fuera del repo):
#
#   rsconnect::setAccountInfo(name   = Sys.getenv("SHINYAPPS_NAME"),
#                              token  = Sys.getenv("SHINYAPPS_TOKEN"),
#                              secret = Sys.getenv("SHINYAPPS_SECRET"))
#   rsconnect::deployApp(appName = "STRIVE")
#---###---###---###---###---###

# ── Load ──────────────────────────────────────────────────────────────────────
load("shiny_data.RData", envir = globalenv())

# ── Newdata builder for PH model ──────────────────────────────────────────────
make_newdata_ph <- function(resp_patho_val, age_val, gender_val,
                             country_val, copd_val, cardio_val, renal_val) {
  base_df <- tibble::tibble(
    resp_patho  = factor(resp_patho_val, levels = resp_patho_levels),
    age         = as.numeric(age_val),
    gender      = factor(gender_val,    levels = gender_levels),
    country_cat = factor(country_val,   levels = country_cat_levels),
    copd        = factor(copd_val,      levels = c("No", "Yes")),
    cardio      = factor(cardio_val,    levels = c("No", "Yes")),
    renal       = factor(renal_val,     levels = c("No", "Yes")),
    trans       = 1:11
  )
  attr(base_df, "trans") <- tmat
  class(base_df) <- c("msdata", "data.frame")

  df_exp <- mstate::expand.covs(base_df, add_covs, longnames = FALSE) |>
    as.data.frame()

  for (cov in covs_with_strat_factor) {
    fac_levels <- msm_prep_levels[[cov]]
    new_var    <- paste0(cov, "_strat")
    trans_stratified <- strat_map_shiny |>
      dplyr::filter(cov_strat == cov) |> dplyr::pull(trans)
    strat_vec           <- rep(fac_levels[1], nrow(df_exp))
    in_strat            <- df_exp$trans %in% trans_stratified
    strat_vec[in_strat] <- as.character(df_exp[[cov]][in_strat])
    df_exp[[new_var]]   <- factor(strat_vec, levels = fac_levels)
    cols_zero <- names(df_exp) |>
      stringr::str_subset(paste0("^", cov, "\\.")) |>
      purrr::keep(~ as.integer(stringr::str_extract(.x, "\\d+$")) %in% trans_stratified)
    for (col in cols_zero) df_exp[[col]] <- 0
  }

  if (length(age_strat_transitions) > 0) {
    strat_vec_age <- rep(age_strat_q_levels[1], nrow(df_exp))
    in_strat_age  <- df_exp$trans %in% age_strat_transitions
    strat_vec_age[in_strat_age] <- as.character(
      bin_age_strat_shiny(df_exp$age[in_strat_age]))
    df_exp$age_strat_q_strat <- factor(strat_vec_age, levels = age_strat_q_levels)
    age_zero_cols <- intersect(paste0("age.", age_strat_transitions), names(df_exp))
    for (col in age_zero_cols) df_exp[[col]] <- 0
  }

  for (col in intersect(paste0("renal.", c(4L, 11L)), names(df_exp))) df_exp[[col]] <- 0

  join_df <- data.frame(trans = as.integer(df_exp$trans))
  for (sc in strat_cols_lookup) {
    if (sc %in% names(df_exp)) join_df[[sc]] <- as.character(df_exp[[sc]])
  }
  joined <- dplyr::left_join(join_df, strata_lookup, by = c("trans", strat_cols_lookup))
  if (any(is.na(joined$strata_int))) {
    stop("strata lookup failed:\n",
         paste(utils::capture.output(print(joined[is.na(joined$strata_int),])),
               collapse = "\n"))
  }
  df_exp$strata <- joined$strata_int
  df_exp$trans  <- as.integer(df_exp$trans)
  df_exp        <- as.data.frame(df_exp)
  attr(df_exp, "trans") <- tmat
  class(df_exp) <- c("msdata", "data.frame")
  df_exp
}

# ── State metadata ─────────────────────────────────────────────────────────────
state_labels <- c(
  "1" = "Hospital Admission",
  "2" = "NIV/HFNC",
  "3" = "MV/ECMO",
  "4" = "Adv. Support Discharge",
  "5" = "Hospital Discharge",
  "6" = "Death"
)

state_colours <- c(
  "Hospital Admission"         = "#4d799f",
  "Hospital Discharge"         = "#27ae60",
  "Adv. Support Discharge"  = "#2e86c1",
  "NIV/HFNC"                   = "#f1c40f",
  "MV/ECMO"                    = "#e67e22",
  "Death"                      = "#e74c3c"
)

starting_state_choices <- c(
  "Hospital Admission"      = 1,
  "NIV/HFNC"               = 2,
  "MV/ECMO"                = 3,
  "Adv. Support Discharge"  = 4
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  theme = shinytheme("spacelab"),
  title = div(
    style = "display:flex; align-items:center; gap:12px;",
    img(src = "virus2.png", height = 40,
        style = "border-radius:50%; box-shadow:0 1px 4px rgba(0,0,0,0.2);"),
    span("Clinical State Progression",
         style = "font-size:1.5em; font-weight:700;
                  color:#2c3e50; letter-spacing:-0.3px;")
  ),
  header = tags$head(
    tags$link(
      rel  = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
    ),
    tags$style(HTML("

      /* ── Base ──────────────────────────────────────────────── */
      body {
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        background-color: #f0f3f7;
        color: #2c3e50;
        font-size: 16px;
      }
      .tab-content { background: #f0f3f7; }

      /* ── Navbar ─────────────────────────────────────────────── */
      .navbar {
        background-color: #ffffff !important;
        border-bottom: 1px solid #dce3ec !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.07) !important;
        position: relative;
      }
      /* Title centered absolutely */
      .navbar-brand {
        position: absolute !important;
        left: 50% !important;
        transform: translateX(-50%) !important;
        float: none !important;
        padding: 8px 0 !important;
      }
      /* Tabs pinned left */
      .navbar-nav {
        float: left !important;
      }
      .navbar-nav > li > a {
        font-size: 17px !important;
        font-weight: 600 !important;
        color: #5d7083 !important;
        padding: 18px 22px !important;
        transition: color 0.15s;
        display: flex !important;
        align-items: center !important;
        gap: 8px !important;
      }
      .navbar-nav > li > a:hover { color: #0d2d4f !important; }
      .navbar-nav > li.active > a {
        color: #1a5f8a !important;
        font-weight: 700 !important;
        background: transparent !important;
        border-bottom: 3px solid #1a5f8a !important;
      }

      /* ── Cards with darker gradient header ──────────────────── */
      .g-card {
        background: #ffffff;
        border-radius: 8px;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0,0,0,0.09);
        margin-bottom: 18px;
      }
      .g-card-header {
        background: linear-gradient(90deg, #0d2d4f 0%, #1a5f8a 100%);
        color: #ffffff;
        padding: 13px 18px;
        font-size: 17px;
        font-weight: 700;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .g-card-header i { font-size: 17px; opacity: 0.9; }
      .g-card-body { padding: 10px 14px; }

      /* ── Dataset overview list ──────────────────────────────── */
      .ov-list {
        list-style: none;
        padding: 4px 0;
        margin: 0;
      }
      .ov-list li {
        display: flex;
        align-items: center;
        gap: 13px;
        padding: 6px 2px;
        font-size: 16px;
        color: #445566;
        border-bottom: 1px solid #f0f3f7;
      }
      .ov-list li:last-child { border-bottom: none; }
      .ov-list .ov-icon {
        color: #1a5f8a;
        width: 22px;
        text-align: center;
        font-size: 17px;
        flex-shrink: 0;
      }

      /* ── Sidebar ────────────────────────────────────────────── */
      .well {
        background: #ffffff !important;
        border: none !important;
        border-radius: 8px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.09) !important;
        padding: 0 !important;
        overflow: hidden;
        width: 85% !important;
      }
      .sidebar-header {
        background: linear-gradient(90deg, #0d2d4f 0%, #1a5f8a 100%);
        color: white;
        padding: 14px 18px;
        font-size: 17px;
        font-weight: 700;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .sidebar-header i { font-size: 17px; opacity: 0.9; }
      .sidebar-body { padding: 6px 10px 10px; }

      .sidebar-section {
        display: flex;
        align-items: center;
        gap: 8px;
        font-size: 15px;
        font-weight: 700;
        color: #2c3e50;
        margin: 18px 0 8px;
        padding-left: 10px;
        border-left: 3px solid #1a5f8a;
      }
      .sidebar-section i { color: #1a5f8a; font-size: 15px; }
      .sidebar-section:first-child { margin-top: 10px; }

      /* ── Form controls ──────────────────────────────────────── */
      .shiny-input-container label,
      .control-label {
        color: #445566 !important;
        font-size: 15px !important;
        font-weight: 500 !important;
      }
      .radio label, .checkbox label {
        color: #445566 !important;
        font-size: 15px !important;
      }
      input[type=radio]    { accent-color: #1a5f8a; }
      input[type=checkbox] { accent-color: #1a5f8a; }

      .irs--shiny .irs-bar   { background: #1a5f8a !important;
                                border-color: #1a5f8a !important; }
      .irs--shiny .irs-handle {
        background: #1a5f8a !important;
        border-color: #0d2d4f !important;
        width: 16px !important;
        height: 16px !important;
        top: 22px !important;
      }
      .irs--shiny .irs-line  { background: #dce3ec !important; }
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        color: #94a3b8 !important;
        font-size: 12px !important;
        background: transparent !important;
      }
      .irs--shiny .irs-single {
        background: white !important;
        color: #0d2d4f !important;
        font-size: 13px !important;
        font-weight: 700 !important;
        border: 2px solid #1a5f8a !important;
        border-radius: 4px !important;
      }
      .irs--shiny .irs-single::before {
        border-top-color: #1a5f8a !important;
      }
      input[type=number] {
        background: #f8fafc !important;
        border: 1px solid #dce3ec !important;
        border-radius: 4px !important;
        color: #2c3e50 !important;
        font-size: 15px !important;
        text-align: center;
      }
      .material-switch > input:checked + label::before {
        background-color: #0d2d4f !important;
      }
      .material-switch > input:checked + label::after {
        background-color: #1a5f8a !important;
      }

      /* ── Buttons ────────────────────────────────────────────── */
      .btn-add {
        width: 100%;
        background: #0d2d4f !important;
        color: white !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 9px !important;
        font-weight: 700 !important;
        font-size: 15px !important;
        margin-bottom: 6px !important;
        transition: background 0.15s, transform 0.1s !important;
      }
      .btn-add:hover {
        background: #1a5f8a !important;
        transform: translateY(-1px) !important;
      }
      .btn-clear {
        width: 100%;
        background: #e74c3c !important;
        color: white !important;
        border: none !important;
        border-radius: 6px !important;
        padding: 9px !important;
        font-weight: 700 !important;
        font-size: 15px !important;
        transition: background 0.15s, transform 0.1s !important;
      }
      .btn-clear:hover {
        background: #c0392b !important;
        transform: translateY(-1px) !important;
      }

      /* ── Warning banner ─────────────────────────────────────── */
      .warn-banner {
        background: #fffde7;
        border: 1px solid #f9e04b;
        border-left: 4px solid #f0ad00;
        border-radius: 6px;
        padding: 12px 18px;
        color: #7a5800;
        font-size: 15px;
        margin-bottom: 16px;
        display: flex;
        align-items: center;
        gap: 10px;
      }
      .warn-banner strong { font-weight: 700; }

      /* ── Legend ─────────────────────────────────────────────── */
      .legend-wrap {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 8px;
        padding: 12px 16px;
        background: white;
        border-radius: 6px;
        border: 1px solid #dce3ec;
        margin-bottom: 18px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.05);
      }
      .legend-item {
        display: flex;
        align-items: center;
        gap: 8px;
        font-size: 15px;
        color: #445566;
        padding: 4px 10px;
      }
      .legend-dot {
        width: 14px;
        height: 14px;
        border-radius: 50%;
        flex-shrink: 0;
      }

      /* ── Plot box ───────────────────────────────────────────── */
      .plot-box {
        background: white;
        border-radius: 8px;
        border: 1px solid #dce3ec;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0,0,0,0.07);
        height: 100%;
      }

      /* ── Sidebar input polish ───────────────────────────────── */

      /* Radio group buttons (pill toggles) */
      .btn-group-container-sw { width: 100% !important; }
      .btn-group-container-sw .btn-group { width: 100% !important; display: flex !important; }
      .btn-group-container-sw .btn {
        flex: 1 !important;
        font-size: 14px !important;
        font-weight: 500 !important;
        padding: 8px 6px !important;
        border: 1px solid #c5d5e8 !important;
        background: #f8fafc !important;
        color: #5d7083 !important;
        transition: all 0.15s !important;
        white-space: normal !important;
        line-height: 1.3 !important;
      }
      .btn-group-container-sw .btn:first-child {
        border-radius: 6px 0 0 6px !important;
      }
      .btn-group-container-sw .btn:last-child {
        border-radius: 0 6px 6px 0 !important;
      }
      .btn-group-container-sw .btn.active,
      .btn-group-container-sw .btn:active {
        background: #0d2d4f !important;
        border-color: #0d2d4f !important;
        color: white !important;
        font-weight: 700 !important;
        box-shadow: 0 2px 6px rgba(13,45,79,0.3) !important;
        z-index: 1 !important;
      }
      .btn-group-container-sw .btn:hover:not(.active) {
        background: #e8f0f8 !important;
        color: #0d2d4f !important;
      }

      /* Age display box */
      .age-display {
        background: #0d2d4f;
        color: white;
        border-radius: 6px;
        padding: 6px 10px;
        font-size: 18px;
        font-weight: 700;
        text-align: center;
        min-width: 58px;
        line-height: 1.2;
      }
      .age-display small {
        display: block;
        font-size: 10px;
        font-weight: 400;
        opacity: 0.7;
        letter-spacing: 0.5px;
        text-transform: uppercase;
      }

      /* Comorbidity toggle cards */
      .comorb-item {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 9px 12px;
        border: 1px solid #dce3ec;
        border-radius: 6px;
        margin-bottom: 7px;
        background: #f8fafc;
        cursor: pointer;
        transition: all 0.15s;
        font-size: 15px;
        color: #445566;
        font-weight: 500;
      }
      .comorb-item:hover {
        border-color: #1a5f8a;
        background: #eef4fa;
        color: #0d2d4f;
      }
      .comorb-item.active-comorb {
        border-color: #1a5f8a;
        background: linear-gradient(90deg, #eef4fa 0%, #ddeeff 100%);
        color: #0d2d4f;
        font-weight: 700;
      }
      .comorb-item .comorb-icon {
        font-size: 14px;
        color: #1a5f8a;
        margin-right: 8px;
      }
      .comorb-item .comorb-check {
        width: 20px;
        height: 20px;
        border: 2px solid #c5d5e8;
        border-radius: 4px;
        display: flex;
        align-items: center;
        justify-content: center;
        flex-shrink: 0;
        font-size: 12px;
        transition: all 0.15s;
      }
      .comorb-item.active-comorb .comorb-check {
        background: #1a5f8a;
        border-color: #1a5f8a;
        color: white;
      }

      /* ── Sidebar input redesign ─────────────────────────────── */

      /* Section mini-cards */
      .input-card {
        background: #f8fafc;
        border: 1px solid #e2ecf5;
        border-radius: 7px;
        padding: 8px 10px;
        margin-bottom: 7px;
      }
      .input-card-label {
        font-size: 11px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 1px;
        color: #1a5f8a;
        margin-bottom: 7px;
        display: flex;
        align-items: center;
        gap: 6px;
      }

      /* Custom select-style radio rows */
      .radio-row {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
      .radio-opt {
        display: flex;
        align-items: center;
        gap: 8px;
        padding: 5px 10px;
        border: 1.5px solid #dce3ec;
        border-radius: 5px;
        background: white;
        cursor: pointer;
        transition: all 0.15s;
        font-size: 13px;
        color: #445566;
        font-weight: 500;
        user-select: none;
      }
      .radio-opt:hover {
        border-color: #1a5f8a;
        background: #f0f7ff;
        color: #0d2d4f;
      }
      .radio-opt.selected {
        border-color: #1a5f8a;
        background: #0d2d4f;
        color: white;
        font-weight: 600;
      }
      .radio-opt .opt-dot {
        width: 13px;
        height: 13px;
        border-radius: 50%;
        border: 2px solid currentColor;
        flex-shrink: 0;
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .radio-opt.selected .opt-dot::after {
        content: '';
        width: 5px;
        height: 5px;
        border-radius: 50%;
        background: white;
      }
      .radio-opt .opt-icon {
        font-size: 12px;
        width: 14px;
        text-align: center;
        flex-shrink: 0;
      }

      /* Two-column radio layout for 2-option groups */
      .radio-row-2col {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 4px;
      }
      .radio-row-2col .radio-opt {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
        font-size: 13px;
        padding: 5px 8px;
        gap: 6px;
      }

      /* Age input group */
      .age-group {
        display: flex;
        align-items: center;
        gap: 8px;
        margin-top: 2px;
      }
      .age-num-box {
        width: 62px;
        flex-shrink: 0;
      }
      .age-num-box input {
        text-align: center !important;
        font-size: 14px !important;
        font-weight: 700 !important;
        color: #0d2d4f !important;
        border: 2px solid #1a5f8a !important;
        border-radius: 5px !important;
        padding: 4px 2px !important;
        background: white !important;
      }

      /* Comorbidity toggles */
      .comorb-row {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }
      .comorb-item {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding: 6px 10px;
        border: 1.5px solid #dce3ec;
        border-radius: 5px;
        background: white;
        cursor: pointer;
        transition: all 0.15s;
        font-size: 13px;
        color: #445566;
        font-weight: 500;
        user-select: none;
      }
      .comorb-item:hover {
        border-color: #1a5f8a;
        background: #f0f7ff;
        color: #0d2d4f;
      }
      .comorb-item.on {
        border-color: #1a5f8a;
        background: #0d2d4f;
        color: white;
        font-weight: 600;
      }
      .comorb-left {
        display: flex;
        align-items: center;
        gap: 9px;
      }
      .comorb-left i { font-size: 14px; opacity: 0.85; }
      .comorb-toggle {
        width: 36px;
        height: 20px;
        border-radius: 10px;
        background: #c5d5e8;
        position: relative;
        flex-shrink: 0;
        transition: background 0.2s;
      }
      .comorb-toggle::after {
        content: '';
        position: absolute;
        width: 14px;
        height: 14px;
        border-radius: 50%;
        background: white;
        top: 3px;
        left: 3px;
        transition: left 0.2s;
        box-shadow: 0 1px 3px rgba(0,0,0,0.2);
      }
      .comorb-item.on .comorb-toggle {
        background: #3aaa7a;
      }
      .comorb-item.on .comorb-toggle::after {
        left: 19px;
      }

      /* Hidden real inputs */
      .hidden-input { display: none !important; }

      /* ── gt table ───────────────────────────────────────────── */
      .gt_table { margin-left: 0 !important; }
      .gt_table td, .gt_table th { font-size: 15px !important; }
    "))
  ),

  # ── Study Overview ──────────────────────────────────────────────────────────
  tabPanel(
    title = HTML('<i class="fas fa-chart-line" style="color:#1a5f8a;"></i> Study Overview'),
    div(style = "margin: 18px;",
      fluidRow(
        column(4,
          div(class = "g-card",
            div(class = "g-card-header",
                tags$i(class = "fas fa-database"), "Dataset Overview"),
            div(class = "g-card-body",
              tags$ul(class = "ov-list",
                tags$li(
                  tags$span(class = "ov-icon", tags$i(class = "fas fa-users")),
                  "4,968 adult participants"
                ),
                tags$li(
                  tags$span(class = "ov-icon", tags$i(class = "fas fa-virus")),
                  "Influenza/COVID-19 confirmed"
                ),
                tags$li(
                  tags$span(class = "ov-icon", tags$i(class = "fas fa-hospital")),
                  "STRIVE network hospitals"
                ),
                tags$li(
                  tags$span(class = "ov-icon", tags$i(class = "fas fa-clock")),
                  "Longitudinal follow-up"
                )
              )
            )
          ),
          div(class = "g-card",
            div(class = "g-card-header",
                tags$i(class = "fas fa-user-group"), "Patient Profile"),
            div(class = "g-card-body", style = "padding: 14px 10px;",
              uiOutput("patient_profile_tbl")
            )
          )
        ),
        column(8,
          div(class = "g-card", style = "height: calc(100% - 18px);",
            div(class = "g-card-header",
                tags$i(class = "fas fa-circle-nodes"), "Multistate Model"),
            div(class = "g-card-body",
              style = "display:flex; justify-content:center;
                       align-items:center; min-height:480px;",
              img(src = "mstate.png", style = "max-width:100%; height:auto;")
            )
          )
        )
      )
    )
  ),

  # ── Risk Profiles ───────────────────────────────────────────────────────────
  tabPanel(
    title = HTML('<i class="fas fa-user-check" style="color:#1a5f8a;"></i> Risk Profiles'),
    sidebarLayout(
      sidebarPanel(width = 3,
        div(class = "sidebar-header",
            tags$i(class = "fas fa-sliders"), "Profile Builder"),
        div(class = "sidebar-body",

          # ── Starting State ────────────────────────────────────
          div(class = "input-card",
            div(class = "input-card-label",
                tags$i(class = "fas fa-map-pin"), "Starting State"),
            div(class = "radio-row", id = "ss_group",
              lapply(seq_along(starting_state_choices), function(i) {
                val <- starting_state_choices[i]
                nm  <- names(starting_state_choices)[i]
                is_sel <- if (i == 1) "selected" else ""
                tags$div(
                  class = paste("radio-opt", is_sel),
                  id    = paste0("ss_", val),
                  onclick = paste0(
                    "document.querySelectorAll('#ss_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                    "this.classList.add('selected');",
                    "Shiny.setInputValue('starting_state','", val, "',{priority:'event'});"
                  ),
                  tags$span(class = "opt-dot"),
                  nm
                )
              })
            ),
            div(class = "hidden-input",
                radioButtons("starting_state", NULL,
                             choices = starting_state_choices, selected = 1))
          ),

          # ── Infection ─────────────────────────────────────────
          div(class = "input-card",
            div(class = "input-card-label",
                tags$i(class = "fas fa-biohazard"), "Infection"),
            div(class = "radio-row-2col", id = "inf_group",
              tags$div(class = "radio-opt selected", id = "inf_flu",
                onclick = paste0(
                  "document.querySelectorAll('#inf_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                  "this.classList.add('selected');",
                  "Shiny.setInputValue('resp_patho','Influenza',{priority:'event'});"
                ),
                tags$span(class = "opt-dot"),
                tags$i(class = "fas fa-snowflake opt-icon"),
                "Influenza"
              ),
              tags$div(class = "radio-opt", id = "inf_cov",
                onclick = paste0(
                  "document.querySelectorAll('#inf_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                  "this.classList.add('selected');",
                  "Shiny.setInputValue('resp_patho','COVID-19',{priority:'event'});"
                ),
                tags$span(class = "opt-dot"),
                tags$i(class = "fas fa-virus opt-icon"),
                "COVID-19"
              )
            ),
            div(class = "hidden-input",
                radioButtons("resp_patho", NULL,
                             choices = c("Influenza","COVID-19"),
                             selected = "Influenza"))
          ),

          # ── Demographics ──────────────────────────────────────
          div(class = "input-card",
            div(class = "input-card-label",
                tags$i(class = "fas fa-person"), "Demographics"),
            # Sex
            div(class = "radio-row-2col", id = "sex_group",
              style = "margin-bottom:6px;",
              tags$div(class = "radio-opt selected", id = "sex_m",
                onclick = paste0(
                  "document.querySelectorAll('#sex_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                  "this.classList.add('selected');",
                  "Shiny.setInputValue('gender','Male',{priority:'event'});"
                ),
                tags$span(class = "opt-dot"),
                HTML("&#9794;"), "Male"
              ),
              tags$div(class = "radio-opt", id = "sex_f",
                onclick = paste0(
                  "document.querySelectorAll('#sex_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                  "this.classList.add('selected');",
                  "Shiny.setInputValue('gender','Female',{priority:'event'});"
                ),
                tags$span(class = "opt-dot"),
                HTML("&#9792;"), "Female"
              )
            ),
            div(class = "hidden-input",
                radioButtons("gender", NULL,
                             choices = c("Male","Female"), selected = "Male")),
            # Age
            div(class = "input-card-label", style = "margin-top:2px; margin-bottom:4px;",
                tags$i(class = "fas fa-calendar"), "Age (years)"),
            div(class = "age-group",
              div(style = "flex:1;",
                  sliderInput("age", NULL, min = 18, max = 100,
                              value = 60, ticks = FALSE, width = "100%")),
              div(class = "age-num-box",
                  numericInput("age_num", NULL, value = 60,
                               min = 18, max = 100, width = "100%"))
            )
          ),

          # ── Country Income ────────────────────────────────────
          div(class = "input-card",
            div(class = "input-card-label",
                tags$i(class = "fas fa-earth-americas"), "Country Income"),
            div(class = "radio-row-2col", id = "ctry_group",
              tags$div(class = "radio-opt selected", id = "ctry_hic",
                onclick = paste0(
                  "document.querySelectorAll('#ctry_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                  "this.classList.add('selected');",
                  "Shiny.setInputValue('country_cat','HIC',{priority:'event'});"
                ),
                tags$span(class = "opt-dot"),
                tags$i(class = "fas fa-building-columns opt-icon"),
                "HIC"
              ),
              tags$div(class = "radio-opt", id = "ctry_lmic",
                onclick = paste0(
                  "document.querySelectorAll('#ctry_group .radio-opt').forEach(el=>el.classList.remove('selected'));",
                  "this.classList.add('selected');",
                  "Shiny.setInputValue('country_cat','LMIC',{priority:'event'});"
                ),
                tags$span(class = "opt-dot"),
                tags$i(class = "fas fa-seedling opt-icon"),
                "LMIC"
              )
            ),
            div(class = "hidden-input",
                radioButtons("country_cat", NULL,
                             choices = c("High income"="HIC","Low income"="LMIC"),
                             selected = "HIC"))
          ),

          # ── Comorbidities ─────────────────────────────────────
          div(class = "input-card",
            div(class = "input-card-label",
                tags$i(class = "fas fa-heart-pulse"), "Comorbidities"),
            div(class = "comorb-row",
              tags$div(class = "comorb-item", id = "copd_card",
                onclick = paste0(
                  "this.classList.toggle('on');",
                  "Shiny.setInputValue('copd', this.classList.contains('on'), {priority:'event'});"
                ),
                div(class = "comorb-left",
                    tags$i(class = "fas fa-lungs"), "COPD"),
                div(class = "comorb-toggle")
              ),
              tags$div(class = "comorb-item", id = "cardio_card",
                onclick = paste0(
                  "this.classList.toggle('on');",
                  "Shiny.setInputValue('cardio', this.classList.contains('on'), {priority:'event'});"
                ),
                div(class = "comorb-left",
                    tags$i(class = "fas fa-heart"), "Cardiovascular"),
                div(class = "comorb-toggle")
              ),
              tags$div(class = "comorb-item", id = "renal_card",
                onclick = paste0(
                  "this.classList.toggle('on');",
                  "Shiny.setInputValue('renal', this.classList.contains('on'), {priority:'event'});"
                ),
                div(class = "comorb-left",
                    tags$i(class = "fas fa-droplet"), "Kidney Disease"),
                div(class = "comorb-toggle")
              )
            )
          ),

          tags$hr(style = "border:none; border-top:1px solid #dce3ec;
                           margin: 8px 0 8px;"),
          actionButton("add",
            label = HTML('<i class="fas fa-plus"></i>&nbsp; Add Profile'),
            class = "btn-add"),
          actionButton("reset",
            label = HTML('<i class="fas fa-eraser"></i>&nbsp; Clear All'),
            class = "btn-clear")
        )
      ),

      mainPanel(width = 9,
        div(style = "padding: 18px 8px 0;",
          div(class = "warn-banner",
              tags$i(class = "fas fa-triangle-exclamation"),
              HTML("<strong>Warning:</strong> Estimates are based on a model pending external validation.")),
          uiOutput("legendUI"),
          uiOutput("plotsContainer") |>
            withSpinner(type = 8, color = "#1a5f8a", hide.ui = FALSE)
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  profiles     <- reactiveVal(list())
  max_profiles <- 4

  observeEvent(input$age,     {
    updateNumericInput(session, "age_num", value = input$age)
  })
  observeEvent(input$age_num, {
    updateSliderInput(session, "age", value = input$age_num)
  })

  observeEvent(input$add, {
    if (length(profiles()) >= max_profiles) {
      showNotification(paste("Maximum of", max_profiles, "profiles reached"),
                       type = "warning", duration = 3)
      return()
    }
    profiles(c(profiles(), list(list(
      id             = length(profiles()) + 1,
      starting_state = as.integer(input$starting_state),
      resp_patho     = input$resp_patho,
      gender         = input$gender,
      age            = input$age_num,
      country_cat    = input$country_cat,
      copd           = ifelse(isTRUE(input$copd),   "Yes", "No"),
      cardio         = ifelse(isTRUE(input$cardio), "Yes", "No"),
      renal          = ifelse(isTRUE(input$renal),  "Yes", "No")
    ))))
    showNotification("Profile added!", type = "message", duration = 2)
  })

  observeEvent(input$reset, {
    profiles(list())
    showNotification("All profiles cleared", type = "message", duration = 2)
  })

  get_profile_data <- function(profile) {
    nd  <- make_newdata_ph(
      resp_patho_val = profile$resp_patho,
      age_val        = profile$age,
      gender_val     = profile$gender,
      country_val    = profile$country_cat,
      copd_val       = profile$copd,
      cardio_val     = profile$cardio,
      renal_val      = profile$renal
    )
    fit <- mstate::msfit(msm_model_ph, newdata = nd, trans = tmat)
    mstate::probtrans(fit, predt = 0, variance = FALSE)
  }

  reachable_states <- function(tmat, from) {
    n        <- nrow(tmat)
    visited  <- rep(FALSE, n)
    frontier <- from
    while (length(frontier) > 0) {
      visited[frontier] <- TRUE
      next_states <- unlist(lapply(frontier, function(s) which(!is.na(tmat[s, ]))))
      frontier    <- setdiff(next_states, which(visited))
    }
    setdiff(which(visited), from)  # exclude the starting state itself
  }

  create_profile_plot <- function(profile, pt) {
    from   <- profile$starting_state
    to_idx <- reachable_states(tmat, from)
    pt_df  <- pt[[from]]

    comorbidities <- c(
      if (profile$copd   == "Yes") "COPD",
      if (profile$cardio == "Yes") "CVD",
      if (profile$renal  == "Yes") "CKD"
    )
    comorb_text <- if (length(comorbidities) == 0) "No comorbidities" else
      paste(comorbidities, collapse = ", ")

    country_label <- if (profile$country_cat == "HIC") "HIC" else "LMIC"

    main_title <- glue(
      "Profile {profile$id} starting at {state_labels[as.character(from)]}"
    )
    subtitle <- glue(
      "{profile$resp_patho} | {country_label} | ",
      "{profile$gender} | {profile$age} yo | {comorb_text}"
    )

    pstate_cols <- intersect(paste0("pstate", to_idx), names(pt_df))

    plot_df <- pt_df |>
      dplyr::select(time, dplyr::all_of(pstate_cols)) |>
      dplyr::filter(time <= 30) |>
      tidyr::pivot_longer(-time, names_to = "pstate", values_to = "prob") |>
      dplyr::mutate(
        state_num = as.integer(str_remove(pstate, "pstate")),
        state     = state_labels[as.character(state_num)],
        text      = glue(
          "Time: {round(time,1)} days<br>",
          "Prob: {scales::percent(prob, accuracy=0.1)}<br>",
          "State: {state}"
        )
      )

    gg <- ggplot(plot_df,
                 aes(x = time, y = prob, colour = state,
                     group = state, text = text)) +
      geom_line(linewidth = 1.3, alpha = 0.92) +
      scale_colour_manual(values = state_colours, drop = FALSE) +
      scale_y_continuous(limits = c(0, 1),
                         labels = scales::percent_format(),
                         expand = c(0, 0)) +
      scale_x_continuous(limits = c(0, 30),
                         breaks = seq(0, 30, 5),
                         expand = c(0, 0)) +
      labs(
        x      = glue("Time since {state_labels[as.character(from)]} (days)"),
        y      = "Probability (%)",
        colour = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.background  = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        axis.title       = element_text(size = 12, color = "#445566"),
        axis.text        = element_text(size = 12, color = "#7f8c8d"),
        legend.position  = "none"
      )

    ggplotly(gg, tooltip = "text") |>
      layout(
        paper_bgcolor = "white",
        plot_bgcolor  = "white",
        title = list(
          text = paste0(
            "<span style='font-size:18px; font-weight:700; color:#2c3e50;'>",
            main_title, "</span><br>",
            "<span style='font-size:14px; color:#95a5a6;'>",
            subtitle, "</span>"
          ),
          x       = 0.03,
          xanchor = "left",
          y       = 0.97,
          yanchor = "top",
          pad     = list(t = 5, b = 5)
        ),
        margin = list(t = 85, b = 40, l = 55, r = 15),
        xaxis = list(
          tickfont  = list(size = 12, color = "#7f8c8d"),
          titlefont = list(size = 12, color = "#445566"),
          gridcolor = "#ecf0f1",
          linecolor = "#dce3ec",
          zerolinecolor = "#dce3ec"
        ),
        yaxis = list(
          tickfont  = list(size = 12, color = "#7f8c8d"),
          titlefont = list(size = 12, color = "#445566"),
          gridcolor = "#ecf0f1",
          linecolor = "#dce3ec",
          zerolinecolor = "#dce3ec"
        )
      ) |>
      config(displayModeBar = FALSE)
  }

  output$plotsContainer <- renderUI({
    req(length(profiles()) > 0)
    plot_ui <- lapply(seq_along(profiles()), function(i) {
      column(
        width  = 6,
        offset = ifelse(length(profiles()) == 1, 3, 0),
        style  = "margin-bottom: 16px; height: 390px;",
        div(class = "plot-box",
            plotlyOutput(paste0("profilePlot", i), height = "385px"))
      )
    })
    tagList(fluidRow(plot_ui))
  })

  observe({
    profiles_list <- profiles()
    lapply(seq_along(profiles_list), function(i) {
      local({
        my_profile <- profiles_list[[i]]
        pt         <- get_profile_data(my_profile)
        output[[paste0("profilePlot", i)]] <- renderPlotly({
          create_profile_plot(my_profile, pt)
        })
      })
    })
  })

  output$legendUI <- renderUI({
    req(length(profiles()) > 0)
    items <- purrr::imap(state_colours[-1], function(col, nm) {
      div(class = "legend-item",
          div(class = "legend-dot", style = paste0("background:", col)),
          span(nm))
    })
    div(class = "legend-wrap", tagList(items))
  })

  output$patient_profile_tbl <- renderUI({
    tbl <- shiny_data |>
      dplyr::select(resp_patho, age, gender, country_cat, copd, cardio, renal) |>
      labelled::set_variable_labels(
        country_cat = "Country income level",
        cardio      = "Cardiovascular disease",
        renal       = "Chronic kidney disease"
      ) |>
      gtsummary::tbl_summary(
        by    = resp_patho,
        label = list(country_cat ~ "Country income level")
      ) |>
      gtsummary::modify_footnote_body(
        footnote = "Other than hypertension",
        columns  = label,
        rows     = variable == "cardio"
      ) |>
      gtsummary::add_overall() |>
      gtsummary::modify_header(label = "") |>
      gtsummary::bold_labels() |>
      gtsummary::as_gt() |>
      gt::tab_options(table.width = "100%", table.font.size = "15px")

    HTML(gt::as_raw_html(tbl))
  })
}

shinyApp(ui, server)
