library(EpiStats)
library(tidyverse)
library(glue)

set.seed(123)

# functions

sim_dat <- function(obs = 300, p_ex = 0.8, p_by = 0.2) {
  
  tibble(
    exposure = sample(c(0,1), size = obs, replace = TRUE, prob = c(1 - p_ex, p_ex)),
    by_var   = sample(c(0,1), size = obs, replace = TRUE, prob = c(1 - p_by, p_by))
  ) %>%
    mutate(
      outcome = case_when(
        runif(obs, 0, 1) < (0.10 + (0.20 * exposure) + (0.20 * by_var) + (0.40 * exposure * by_var)) ~ 1,
        T ~ 0
      ),
      by_var = as.factor(by_var)
    )
  
}


ctab <- function(dat, by_var = TRUE, correct = TRUE, correction = 0.5) {
  
  a <- dat %>%
    filter(exposure == 1 & outcome == 1) %>%
    nrow()
  b <- dat %>%
    filter(exposure == 1 & outcome == 0) %>%
    nrow()
  c <- dat %>%
    filter(exposure == 0 & outcome == 1) %>%
    nrow()
  d <- dat %>%
    filter(exposure == 0 & outcome == 0) %>%
    nrow()
  
  if (0 %in% c(a, b, c, d)) {
    a <- a + correction
    b <- b + correction
    c <- c + correction
    d <- d + correction
  }
  
  rr <- (a / (a + b)) / (c / (c + d))
  
  out <- list(
    "crude" = c("a" = a, "b" = b, "c" = c, "d" = d),
    "crude_rr" = rr
  )
  
  if (by_var == TRUE) {
    
    a1 <- dat %>% filter(exposure == 1 & outcome == 1 & by_var == 1) %>% nrow()
    b1 <- dat %>% filter(exposure == 1 & outcome == 0 & by_var == 1) %>% nrow()
    c1 <- dat %>% filter(exposure == 0 & outcome == 1 & by_var == 1) %>% nrow()
    d1 <- dat %>% filter(exposure == 0 & outcome == 0 & by_var == 1) %>% nrow()
    
    if ((0 %in% c(a1, b1, c1, d1)) & correct == TRUE) {
      a1 <- a1 + correction
      b1 <- b1 + correction
      c1 <- c1 + correction
      d1 <- d1 + correction
    }

    a0 <- dat %>% filter(exposure == 1 & outcome == 1 & by_var == 0) %>% nrow()
    b0 <- dat %>% filter(exposure == 1 & outcome == 0 & by_var == 0) %>% nrow()
    c0 <- dat %>% filter(exposure == 0 & outcome == 1 & by_var == 0) %>% nrow()
    d0 <- dat %>% filter(exposure == 0 & outcome == 0 & by_var == 0) %>% nrow()
    
    if ((0 %in% c(a0, b0, c0, d0)) & correct == TRUE) {
      a0 <- a0 + correction
      b0 <- b0 + correction
      c0 <- c0 + correction
      d0 <- d0 + correction
    }
    
    adj_rr_num <- ((a1 * (c1 + d1))/(a1 + b1 + c1 + d1)) + ((a0 * (c0 + d0))/(a0 + b0 + c0 + d0)) 
    adj_rr_den <- ((c1 * (a1 + b1))/(a1 + b1 + c1 + d1)) + ((c0 * (a0 + b0))/(a0 + b0 + c0 + d0))
    
    out$st1    <- c("a" = a1, "b" = b1, "c" = c1, "d" = d1)
    out$st0    <- c("a" = a0, "b" = b0, "c" = c0, "d" = d0)
    out$adj_rr <- adj_rr_num / adj_rr_den
    
  }
  
  return(tibble(
    "crude_rr" = out$crude_rr,
    "adj_rr"   = out$adj_rr
  ))
  
}

run_iter <- function(iter = 1000, correct = FALSE) {
  
  for (i in 1:iter) {
    
    if (i == 1) {
      
      tmp_out <- ctab(sim_dat(), correct = correct) %>% mutate(iter = i)
      
    } else {
      tmp     <- ctab(sim_dat(), correct = correct) %>% mutate(iter = i)
      tmp_out <- bind_rows(tmp_out, tmp)
    }
    
  }
  
  tmp_out <- tmp_out %>% drop_na()
  
  message(paste0(iter - dim(tmp_out)[1], " observations were dropped due to incalculable MH RR"))
  
  return(tmp_out)
  
}

# results ----------
results <- run_iter()

results %>%
  pivot_longer(
    names_to = "stat",
    values_to = "value",
    -iter
  ) %>%
  ggplot(aes(x = stat, y = value, group = stat)) +
  geom_boxplot() +
  labs(
    title    = "Distribution of crude and adjusted risk ratios",
    subtitle = glue::glue("from {max(results$iter)} simulations"),
    x        = "Statistic",
    y        = "Odds Ratio"
  ) +
  theme_minimal() +
  theme(
    text          = element_text(family = "Lato"),
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.title    = element_text(face = "italic"),
    panel.grid    = element_blank(),
    axis.line     = element_line(color = "black", size = 0.5)
  )

summary(results$crude_rr)
summary(results$adj_rr)

get_sse <- function(x) {
  
  tmp <- x %>%
    mutate(
      sqerr_adj   = (log(3) - log(adj_rr))^2,
      sqerr_crude = (log(3) - log(crude_rr))^2
    )
  
  return(
    list(
      "SSE_crude" = sum(tmp$sqerr_crude),
      "SSE_adj"   = sum(tmp$sqerr_adj)
    )
  )
  
}

get_sse(results)
