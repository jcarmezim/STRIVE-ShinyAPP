# STRIVE — Clinical State Progression

Shiny app visualizing clinical state progression in patients hospitalized with influenza or COVID-19 (admission → respiratory support → discharge/death),
based on a multistate Cox model (fitted with the [`mstate`](https://cran.r-project.org/package=mstate)package) built on data from the STRIVE study.

**🔗 Live app: [brui.shinyapps.io/STRIVE](https://brui.shinyapps.io/STRIVE/)**

> ⚠️ Estimates are based on a model pending external validation.

## What it does

- **Study Overview** — dataset summary (participants, pathogens, STRIVE network hospitals) and a diagram of the multistate model.
- **Risk Profiles** — build up to 4 patient profiles (pathogen, age, sex, country income level, comorbidities) and, for each one, view the estimated probability of being in each clinical state over 30 days from the chosen starting state.

## Data

This repository **does not contain real patient data**. The included `.RData` (`shiny_data_demo.RData`) has the same structure as the real dataset but with synthetic values, generated via independent marginal resampling — no row corresponds to a real patient. The fitted model that is included (multistate Cox coefficients) is population-level, aggregated information, not individual patient data.

The app published at the link above runs on the study's real data, in a separate deployment that does not go through this repository.
