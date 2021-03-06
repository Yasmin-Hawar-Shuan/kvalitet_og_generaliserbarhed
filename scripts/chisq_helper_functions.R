library(chisq.posthoc.test)
library(kableExtra)
library(tidyverse)
library(Hmisc)

# Adding total 
add_total <- function(x) {
  x <- x %>% 
    rbind("Total" = apply(x, 2, sum)) %>% 
    cbind("Total" = c(apply(x, 1, sum), sum(x)))
}

# chisq.test as kbl table
xsq.test <- function(tab, cap) {
  x <- chisq.test(tab)
  x$obs.prop <- x$observed %>% 
    add_total() %>% 
    prop.table(2) * 2 * 100

  x$exp.prop <- x$expected %>% 
    add_total() %>% 
    prop.table(2) * 2 * 100
    
  x$observed <- add_total(x$observed)
  x$expected <- add_total(x$expected)
  
  list(p.value = x$p.value,
       statistic = x$statistic,
       residuals = x$residuals,
       stdres = x$stdres,
       
    observed = x$observed %>%
    kbl(caption = paste0("Observeret frekvens: ", cap),
        booktabs = T,
        label = paste0("obs-", str_replace_all(cap, " ", "-")),
        format.args = list(big.mark = ".")) %>% 
    row_spec(nrow(x$observed)-1, hline_after = T) %>% 
    kable_styling(full_width = F,
                  html_font = "Cambria",
                  latex_options = "HOLD_position"),
    
    obs.prop = x$obs.prop %>%
      round(digits = 2) %>% 
      kbl(caption = paste0("Observeret frekvens (\\%): ", cap),
          booktabs = T,
          label = paste0("obs-per-", str_replace_all(cap, " ", "-")),
          format.args = list(big.mark = ".")) %>% 
      row_spec(nrow(x$observed)-1, hline_after = T) %>% 
      kable_styling(full_width = F,
                    html_font = "Cambria",
                    latex_options = "HOLD_position"),
    
  expected = x$expected %>% 
    round(digits = 2) %>% 
    kbl(caption = paste0("Forventet frekvens: ", cap),
        booktabs = T,
        label = paste0("exp-", str_replace_all(cap, " ", "-")),
        format.args = list(big.mark = ".")) %>% 
    row_spec(nrow(x$expected)-1, hline_after = T) %>% 
    kable_styling(full_width = F, 
                  html_font = "Cambria", 
                  latex_options = "HOLD_position") %>% 
    footnote(general = ifelse(x$parameter > 1, "Pearsons chi-i-anden-test", "Pearsons chi-i-anden test med Yates' kontinuiteteskorrektion"),
             number = c(paste0("$\\\\chi^2$ (", as.numeric(x$parameter), ", N=", format(last(x$expected), big.mark = "."),
                               ") ", format(as.numeric(x$statistic), big.mark = ".")),
                        paste0("p-værdi: ", format(x$p.value, digits = 3), 
                               ifelse(x$p.value <= 0.001, " ***", 
                                      ifelse(x$p.value <= 0.01, " **", 
                                             ifelse(x$p.value <= 0.05, " *", "")))),
                        "* p < 0.05, ** p < 0.01, *** p < 0.001"), escape = F),
  
  exp.prop = x$exp.prop %>% 
    round(digits = 2) %>% 
    kbl(caption = paste0("Forventet frekvens (\\%): ", cap),
        booktabs = T,
        label = paste0("exp-per-", str_replace_all(cap, " ", "-")),
        format.args = list(big.mark = ".")) %>% 
    row_spec(nrow(x$expected)-1, hline_after = T) %>% 
    kable_styling(full_width = F, 
                  html_font = "Cambria", 
                  latex_options = "HOLD_position") %>% 
    footnote(general = ifelse(x$parameter > 1, "Pearsons chi-i-anden-test", "Pearsons chi-i-anden test med Yates' kontinuiteteskorrektion"),
             number = c(paste0("$\\\\chi^2$ (", as.numeric(x$parameter), ", N=", format(last(x$expected), big.mark = "."),
                               ") ", format(as.numeric(x$statistic), big.mark = ".")),
                        paste0("p-værdi: ", format(x$p.value, digits = 3), 
                               ifelse(x$p.value <= 0.001, " ***", 
                                      ifelse(x$p.value <= 0.01, " **", 
                                             ifelse(x$p.value <= 0.05, " *", "")))),
                        "* p < 0.05, ** p < 0.01, *** p < 0.001"), escape = F))
}

post_hoc_chi <- function(tab, cap, method) {
  test <- chisq.posthoc.test(tab, method = method)
  
  test$Value[test$Value == "Residuals"] <- "Residual"
  test$Value[test$Value == "p values"] <- "p-værdi"
  test$Dimension[seq.int(2, nrow(test), by = 2)] <- ""
  
  names(test)[1:2] <- c("Svarmulighed", "Værdi")
  
  test %>% 
    kbl(caption = paste0("Post-hoc: ", cap),
        booktabs = T) %>% 
    row_spec(seq.int(2, nrow(test), by = 2), hline_after = T) %>% 
    kable_styling(full_width = F,
                  html_font = "Cambria",
                  latex_options = c("HOLD_position", "striped")) %>% 
    footnote(general = paste0("Standardiserede chi-i-anden residualer og p-værdier med ", capitalize(method), " korrektion"),
             number = "Signifikans: p < 0.05, Stærk signifikans: p < 0.01, Meget stærk signifikans: p < 0.001",
             threeparttable = T)
}