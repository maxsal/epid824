library(EpiStats)
library(tidyverse)

set.seed(123)

U <- runif(1000, -1, 1)
A = ifelse(U > 0.5, 1, 0)
U
A

data <- tibble(
  U = U,
  A = A
) %>%
  mutate(
    Y = if_else(
      A == 1 & (runif(1000, 0, 1) < 0.75),
      1,
      if_else(
        A == 0 & (runif(1000, 0, 1) < 0.25),
        1,
        0
      )
    )
  ) %>%
  as.data.frame()


cs(data, "Y", "A")
cc(data, "Y", "A")
