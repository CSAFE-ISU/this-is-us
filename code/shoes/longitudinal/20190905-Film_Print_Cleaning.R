library(tidyverse)
library(EBImage)
library(furrr)
plan(multicore)
# ---- Convenience functions ----

prune <- function(imgx, prop_limit = 0.02) {
  counts <- table(imgx)
  categories <- sort(unique(as.numeric(imgx)))
  prop <- counts/sum(counts)
  cat_replace <- categories[prop < prop_limit]
  imgx[imgx %in% cat_replace] <- 0
  imgx
} # Get rid of really small areas - assumes large areas have merged...


get_expanded_mask <- function(img, rad1 = 5, rad2 = 91, 
                              prop = 1.5*pi*rad2^2/length(img), 
                              expand_rad = 50) {
  
  f1 <- makeBrush(rad1, shape = "disc")
  f2 <- makeBrush(rad2, shape = "disc")
  mask_init <- img %>%
    erode(f1) %>%
    dilate(f2) %>% 
    bwlabel() %>% 
    prune(prop_limit = prop)
  
  background <- table(mask_init[0:10,0:10]) %>% sort() %>% magrittr::extract(1) %>%
    names() %>% parse_number()
  useful <- mask_init != background
  
  if (sum(useful) > 0) {
    useful_df <- as_tibble(useful, rownames = NA) %>%
      mutate(row = 1:n()) %>%
      tidyr::gather(key = col, value = value, -row) %>%
      mutate(col = col %>% str_remove("V") %>% parse_integer()) %>%
      filter(value > background)
    
    test_mat <- 0*useful
    
    useful_cols <- useful_df %>%
      group_by(row) %>%
      summarize(mincol = min(col), maxcol = max(col)) %>%
      mutate(col = purrr::map2(
        mincol, maxcol, 
        ~tibble(col = pmax(0, .x - expand_rad):pmin(ncol(img), .y + expand_rad)))) %>%
      select(-mincol, -maxcol) %>%
      unnest(col) %>%
      select(row, col)
    
    useful_rows <- useful_df %>%
      group_by(col) %>%
      summarize(minrow = min(row), maxrow = max(row)) %>%
      mutate(row = purrr::map2(
        minrow, maxrow, 
        ~tibble(row = pmax(0, .x - expand_rad):pmin(nrow(img), .y + expand_rad)))) %>%
      select(-minrow, -maxrow) %>%
      unnest(row) %>%
      select(row, col)
    
    bind_rows(useful_rows, useful_cols) %>%
      unique() %>%
      mutate(value = 1) %>%
      complete(row = 1:nrow(img), col = 1:ncol(img), fill = list(value = 0)) %>%
      spread(key = col, value = value, fill = NA) %>%
      arrange(row) %>%
      select(-row) %>%
      as.matrix() %>%
      as.Image()
  }
  
  
} # Shorthand for erode, dilate, bwlabel, then "convex hull" calculation

# ------------------------------------------------------------------------------

imglist <- list.files("/lss/research/csafe-shoeprints/ShoeImagingPermanent/", 
                      pattern = "00\\d{4}[RL]_\\d{8}_5_1_1", full.names = T)

d5 <- makeBrush(5, shape = 'disc', step = T)

imgs <- tibble(
  file = imglist,
  shoe_id = str_extract(file, "\\d{6}[RL]"),
  date = str_extract(file, "\\d{8}") %>% lubridate::ymd()
) %>%
  group_by(shoe_id, date) %>%
  filter(row_number() == 1) %>%
  mutate(orig = purrr::map(file, readImage, all = F)) 


img_steps <- imgs %>%
  filter(str_detect(shoe_id, "L")) %>%
  group_by(shoe_id) %>%
  arrange(date) %>%
  filter(row_number() == 1)

png("Film_Demo_Orig.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$orig, plot)
dev.off()

img_steps <- img_steps %>%
  mutate(
    inverse = map(orig, ~1 - .),
    filter_init = map(inverse, filter2, filter = d5),
    thresh_init = map(filter_init, thresh, w = 10, h = 10, offset = 0.90)
  )

png("Film_Demo_Inv.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$inverse, plot)
dev.off()

png("Film_Demo_Filter_Init.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$filter_init, plot)
dev.off()

png("Film_Demo_Thresh_init.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$thresh_init, plot)
dev.off()

img_steps <- img_steps %>%
  mutate(
    expand_mask = map(thresh_init, get_expanded_mask)
  )


png("Film_Demo_Mask.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$expand_mask, plot)
dev.off()

img_steps <- img_steps %>%
  mutate(cleaned = purrr::map2(orig, expand_mask, function(x, y) as.Image(x*y + (1 - y)*median(x))))%>%
  mutate(cleaned_thresh = purrr::map(cleaned, ~1 - thresh(1 - ., w = 150, h = 150, offset = .05)))


png("Film_Demo_Cleaned.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$cleaned, plot)
dev.off()
png("Film_Demo_Cleaned_Thresh.png", width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map(img_steps$cleaned_thresh, plot)
dev.off()


png(file.path(img_dir, "Film_Demo_Cleaned_Balance_Compare.png"), width = 300*9, height = 300*2, units = "px")
par(mfrow = c(1, 9)) 
purrr::map2(img_steps$clean_balance, img_steps$cleaned_thresh, function(x, y) {
  print(sum(normalize(x) != normalize(y)))
  plot(rgbImage(x, 1 + 0*x, y))
})
dev.off()
