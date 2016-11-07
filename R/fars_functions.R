#function perc_cis
#to determine confidence intervals for proportions
# input x (number for drivers testing positive for a drug) and 
# n (number of non-missing observations) and output a character vector
# of the percentage of drivers testing positive and the 95% confidence interal
perc_cis <- function(x, n) {
  p_hat <- x / n
  se <- sqrt((p_hat * (1 - p_hat)) / n)
  upper_ci <- round((p_hat + 1.96*se)*100, digits = 1)
  lower_ci <- round((p_hat - 1.96*se)*100, digits = 1)
  p_hat <- round(p_hat*100, digits = 1)
  ci <- paste0(p_hat, "% (", lower_ci, "%, ", upper_ci, "%)")
  return(ci)
}

# function prev_test is just a convenient wrapper for perc_cis
prev_test <- function(drug, input_data) {
  to_test <- filter(input_data, drug_type == drug)
  n_test <- nrow(to_test)
  to_test <- filter(to_test, positive_for_drug == TRUE)
  x_test <- nrow(to_test)
  out_vec <- perc_cis(x_test, n_test)
  return(out_vec)
}

#function test_trend_ca
#counts multi-drug hits more than once

test_trend_ca <- function(drug, input_data = clean_fars) {
  ifelse(drug == "Nonalcohol", 
         to_test <- filter(input_data, drug_type != "Alcohol"),
         to_test <- filter(input_data, drug_type == drug))
  to_test <- to_test %>%
    group_by(year) %>%
    summarize(positive = sum(positive_for_drug, na.rm = TRUE),
              trials = sum(!is.na(positive_for_drug)))
  ca_test <- prop.trend.test(x = to_test$positive,
                             n = to_test$trials)
  z <- round(sqrt(ca_test$statistic), digits = 1)
  output <- tibble(Z = z, p.value = round(ca_test$p.value, digits = 3))
  return(output)
}

#function test_trend_log_reg
# counts multi-drug hits more than once
test_trend_log_reg <- function(drug, input_data = clean_fars) {
  ifelse(drug == "Nonalcohol", 
         to_test <- filter(input_data, drug_type != "Alcohol"),
         to_test <- filter(input_data, drug_type == drug))
  log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
  temp_out <- summary(log_reg)$coefficients
  output <- tibble(Z = round(temp_out[2,3], digits = 1), 
                   p.value = round(temp_out[2,4], digits = 3))
  return(output)
}