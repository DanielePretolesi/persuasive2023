# Functions for reordering factors ----------------------------------------

reorder_likert <- function(item) {
  factor(
    item,
    levels = c(
      "Strongly disagree",
      "Rather disagree",
      "Neutral",
      "Rather agree",
      "Strongly agree"
    )
  )
}


reorder_imp <- function(item) {
  factor(
    item,
    levels = c(
      "Very unimportant",
      "Unimportant",
      "Neutral",
      "Important",
      "Very important")
  )
}


reorder_age <- function(item) {
  factor(
    item,
    levels = c(
      "> 60",
      "25 - 60",
      "< 25")
  )
}


reorder_ati <- function(item) {
  factor(
    item,
    levels = c(
      "high",
      "middle",
      "low")
  )
}


reorder_freq <- function(item) {
  factor(item, levels = c(
 "Less than one per week",
 "One per week",
 "One per day",
 "2-5 per day",
 "More than 5 per day"))
}


# Functions to compute inferential statistics -----------------------------

compare_otions <- function(var_string) {
  
  model_data <-  data_clean[, .SD, .SDcols = patterns(var_string)
  ][, melt(.SD, id.vars = "id")
  ][, value := reorder_likert(value)][] 
  
  model <- clmm(value ~ variable + (1|id), data = model_data)
  em_model <- emmeans(model, ~ variable)
  contrasts <- pairs(em_model, adjust = "holm")
  
  list(
    model = model,
    emmeans = em_model,
    contrasts = contrasts,
    coef = round(exp(model$beta), 3)
  )
}


compare_demographics_clm <- function(var, data = data_clean, reorder_fun = reorder_likert) {
# TODO: Refactor to improve cohension and coupling  
  
  model_data <- data[g1q00001 %in% c("Male", "Female"),
             ][, .SD, .SDcols = c("id", var, "g1q00001", "age_group", "ati_group")
               ][, melt(.SD, id.vars = c("id", "g1q00001", "age_group", "ati_group"))
                 ][, value := reorder_fun(value)]
  
  model <- clm(value ~ g1q00001 + age_group + ati_group, data = model_data)
  anova <- setDT(anova(model, type="II"), keep.rownames = TRUE)[, p_adjusted := p.adjust(`Pr(>Chisq)`, method = "holm")
  ][, sig := p_adjusted <= 0.05][]
  
  var_desc <- var_dict[var_name == var, ]$var_description_short_en
  
  list(
    "Cumulative link model",
    var,
    var_desc,
    model,
    anova
  )
}

compare_demographics_chisq <- function(var, factor = "g2q00005") {
  tab <- data_clean[g2q00005 %in% c("Yes", "No") & g1q00001 %in% c("Male", "Female"),
                    .N, by = c(var, factor)
  ][, setnames(.SD, c(var, factor), c("var_rename", "factor_name"))
  ][, dcast(.SD, factor_name ~ var_rename)] |>
    column_to_rownames("factor_name")
  
  var_desc <- var_dict[var_name == factor, ]$var_description_short_en
  
  list(
    var_desc,
    tab,
    chisq.test(tab)
  )
}


# Colors for plots --------------------------------------------------------

likert_colors = rev(c("#5ab4ac", "#91cdc7", "lightgrey", "#8c8f8f", "#585959"))
