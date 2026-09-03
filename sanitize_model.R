# sanitize_model.R
# -----------------------------------------------------------------------------
# Uso OPCIONAL. Solo lo necesitas si algún día quieres compartir/distribuir
# los objetos de modelo (msm_model_ph, msm_model_full) por separado del resto
# de shiny_data.RData -- por ejemplo, para reproducibilidad de un paper, sin
# entregar el dataset de pacientes.
#
# Elimina de los objetos coxph los vectores que se calculan por sujeto
# (linear.predictors, residuals, y) para no llevar de paseo, aunque sea de
# forma no identificable, ningún valor derivado por paciente. Esto NO afecta
# a msfit()/probtrans(), que solo necesitan coefficients, var, means y la
# información de estratos.

load("shiny_data.RData")   # ajusta la ruta si hace falta

strip_subject_vectors <- function(model) {
  model$residuals         <- NULL
  model$linear.predictors <- NULL
  model$y                 <- NULL
  model
}

msm_model_full <- strip_subject_vectors(msm_model_full)
msm_model_ph   <- strip_subject_vectors(msm_model_ph)

# ── Comprobación rápida: msfit() debe seguir funcionando ────────────────────
# (usa aquí un newdata de ejemplo con make_newdata_ph(), como en app.R, y
#  confirma que mstate::msfit(msm_model_ph, newdata = nd, trans = tmat)
#  no da error antes de sobrescribir nada)

# ── Guarda un bundle "solo modelo", SIN el dataframe de pacientes ───────────
objetos_a_excluir <- c("shiny_data")
save(list = setdiff(ls(), objetos_a_excluir),
     file = "model_only.RData")

message("Listo: model_only.RData no contiene el dataframe shiny_data ",
        "ni los vectores por sujeto de los modelos coxph.")
