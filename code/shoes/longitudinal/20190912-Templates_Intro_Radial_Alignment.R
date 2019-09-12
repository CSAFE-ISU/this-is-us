library(tidyverse)
library(EBImage)
library(ShoeScrubR) # SHA: fe17d486

img_output_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"
lss_dir <- "/lss/research/csafe-shoeprints/ShoeImagingPermanent"

templates <- tibble(file = list.files("inst/templates", full.names = T)) %>%
  mutate(fname = basename(file) %>% str_remove("\\.png$")) %>%
  tidyr::extract(fname, into = c("Shoe", "Model", "Size", "Foot"),
                 regex = "(Adidas|Nike)_(.*)_([\\d\\.]{1,})[MW]_([LR])",
                 remove = T) %>%
  mutate(img = purrr::map(file, ~readImage(.) %>% channel("luminance")))

png(filename = file.path(img_output_dir, "Templates_Equal_Size.png"), width = 300*8, height = 300*4, units = "px")
par(mfrow = c(2, 8))
templates %>%
  arrange(Shoe, Size, Foot) %>%
  magrittr::extract2("img") %>%
  purrr::map(ShoeScrubR:::auto_resize_img, final_dims = c(1600, 2600)) %>%
  purrr::map(plot)
dev.off()


# Generate explanation of how this would work

# Setup and initial cleaning
orig_img <- EBImage::readImage(file.path(lss_dir, "040639L_20180307_5_1_1_boekhoff_pashek_jekruse.tif"))
img <- orig_img %>% channel("luminance")

inv_img <- img %>%
  filter2(makeBrush(25, "gaussian")) %>%
  normalize() %>%
  magrittr::subtract(1, .)
thresh_img <- inv_img > .15

orig_mask <- shoe_mask("Nike", 8, "R", ppi = 300) %>% ShoeScrubR:::auto_resize_img(final_dims = dim(img))

png(filename = file.path(img_output_dir, "Templates_Setup.png"),
    width = 300*4, height = 300*2, units = "px")
par(mfrow = c(1, 4))
plot(img)
plot(thresh_img)
plot(orig_mask)
plot(rgbImage(green = 1 - thresh_img, blue = 1 - orig_mask, red = 1 - orig_mask))
dev.off()

# Compute center of each image (by # white pixels)
exaggerated_img <- thresh_img %>%
  opening(makeBrush(5, "disc")) %>%
  closing(makeBrush(301, "disc"))

img_center <- exaggerated_img %>%
  image_to_df() %>%
  summarize(row = round(mean(row)), col = round(mean(col))) %>%
  unlist()

mask_center <- image_to_df(orig_mask)  %>%
  summarize(row = round(mean(row)), col = round(mean(col))) %>%
  unlist()

t_dist <- mask_center - img_center
centered_mask <- translate(orig_mask, -t_dist, bg.col = 0)


png(filename = file.path(img_output_dir, "Templates_Naive_Centering.png"),
    width = 300*4, height = 300*2, units = "px")
par(mfrow = c(1, 4))
plot(rgbImage(green = exaggerated_img, blue = thresh_img, red = thresh_img))
points(-img_center[["row"]], img_center[["col"]], col = "blue", cex = 1.5, type = "p", pch = 16)
plot(orig_mask)
points(-mask_center[["row"]], mask_center[["col"]], col = "red", cex = 1.5, type = "p", pch = 16)
plot(rgbImage(green = 1 - orig_mask, blue = 1 - centered_mask, red = 1 - centered_mask))
plot(rgbImage(green = 1 - thresh_img, blue = 1 - centered_mask, red = 1 - centered_mask))
dev.off()

padded_img <- ShoeScrubR:::pad_to_center(thresh_img, img_center)
padded_mask <- ShoeScrubR:::pad_to_center(centered_mask, img_center)

# Angle alignment

radial_mask_init <- ShoeScrubR:::radial_mask(dim(padded_img), slopes = seq(-90, 90, 10), px = 1) %>%
  dilate(makeBrush(31, "disc"))

radial_masks <- purrr::map(
  c(-6, -2, 0, 2, 6), 
  ~ShoeScrubR:::radial_mask(dim(padded_img), 
                            slopes = seq(-90, 90, 10) + ., 
                            px = 1) %>%
    dilate(makeBrush(31, "disc")))


png(filename = file.path(img_output_dir, "Templates_Radial_Mask_Single.png"),
    width = 300*1, height = 300*2, units = "px")
purrr::map(radial_masks[3], ~plot(rgbImage(red = ., green = padded_img, blue = padded_mask)))
dev.off()


png(filename = file.path(img_output_dir, "Templates_Radial_Mask_Multiple.png"),
    width = 300*5, height = 300*2, units = "px")
par(mfrow = c(1, 5))
purrr::map(radial_masks, ~plot(rgbImage(red = ., green = padded_img, blue = padded_mask)))
dev.off()

