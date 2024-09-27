create_case_when <- function(expl_subset, var_name) {
  case_when_conditions <- paste0(var_name, " == '", expl_subset$Code, "' ~ '", expl_subset$Var_expl, "'")
  case_when_str <- paste(case_when_conditions, collapse = ", ")
  return(paste0("case_when(", case_when_str, ")"))
}

