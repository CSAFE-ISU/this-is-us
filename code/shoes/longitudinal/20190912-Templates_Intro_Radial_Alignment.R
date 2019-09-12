library(tidyverse)
library(EBImage)
library(ShoeScrubR) # SHA: 40c68b42

img_output_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"

templates <- tibble(file = list.files("inst/templates", full.names = T)) %>%
  mutate(fname = basename(file) %>% str_remove("\\.png$")) %>%
  tidyr::extract(fname, into = c("Shoe", "Model", "Size", "Foot"),
                 regex = "(Adidas|Nike)_(.*)_([\\d\\.]{1,})[MW]_([LR])",
                 remove = T) %>%
  mutate(img = purrr::map(file, ~readImage(.) %>% channel("luminance")))

par(mfrow = c(2, 8))
templates %>%
  arrange(Shoe, Size, Foot) %>%
  magrittr::extract2("img") %>%
  purrr::map(auto_resize_img, final_dims = c(1600, 2600)) %>%
  purrr::map(plot)
