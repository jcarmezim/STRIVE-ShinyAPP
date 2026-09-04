# STRIVE — Clinical State Progression

Shiny app visualizing clinical state progression in patients hospitalized with influenza or COVID-19 based on a multistate Cox model built on data from the study.

**🔗 Live app: [brui.shinyapps.io/STRIVE](https://brui.shinyapps.io/STRIVE/)**

> ⚠️ Estimates are based on a model pending external validation.

## Data

This repository **does not contain real patient data**. The included `.RData` (`shiny_data_demo.RData`) has the same structure as the real dataset but with synthetic values, generated via independent marginal resampling — no row corresponds to a real patient. The fitted model that is included (multistate Cox coefficients) is population-level, aggregated information, not individual patient data.

The app published at the link above runs on the study's real data, in a separate deployment that does not go through this repository.
