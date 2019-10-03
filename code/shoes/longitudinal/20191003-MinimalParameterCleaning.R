library(EBImage)
library(ShoeScrubR)
library(tidyverse)

set.seed(3142095)

img_output_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"
lss_dir <- "/lss/research/csafe-shoeprints/ShoeImagingPermanent"

# For a bunch of images...
full_imglist <- list.files("/lss/research/csafe-shoeprints/ShoeImagingPermanent/",
                           pattern = "0[01]\\d{4}[RL]_\\d{8}_5_1_1_.*_.*_.*", full.names = T)
dir <- "/tmp/film-prints"
if (!dir.exists(dir)) dir.create(dir)

file.copy(full_imglist, file.path(dir, basename(full_imglist)))
imglist <- file.path(dir, basename(full_imglist))

shoe_info <- read_csv("~/Projects/CSAFE/2018_Longitudinal_Shoe_Project/Clean_Data/shoe-info.csv") %>%
  filter(ShoeID %in% str_sub(basename(imglist), 1, 3)) %>%
  select(ShoeID, Brand, Size) %>%
  mutate(Size = str_remove(Size, "[ MW]") %>% parse_number()) %>%
  crossing(tibble(Mask_foot = c("R", "L"), Shoe_foot = c("L", "R"), ppi = c(200, 300))) %>%
  mutate(mask = purrr::pmap(list(Brand, Size, Mask_foot, ppi = ppi), shoe_mask))

scan_info <- tibble(
  file = imglist,
  ShoeID = str_extract(basename(file), "^\\d{3}"),
  Shoe_foot = str_extract(basename(file), "\\d{6}[RL]") %>% str_remove_all("\\d"),
  date = str_extract(basename(file), "\\d{8}") %>% parse_date(format = "%Y%m%d")
) %>%
  left_join(select(shoe_info, ShoeID, Brand, Size, Shoe_foot)) %>%
  group_by(Shoe_foot, Brand) %>%
  sample_n(2) %>%
  ungroup() %>%
  group_by(ShoeID, Shoe_foot) %>%
  # arrange(desc(date)) %>%
  # filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(
    img = purrr::map(file, EBImage::readImage, all = F),
    img = purrr::map(img, EBImage::channel, "luminance"),
    im_dim = purrr::map(img, dim)
  )


scan_info <- scan_info %>%
  mutate(em = purrr::map(img, ShoeScrubR:::em_thresh, scale_factor = 10)) %>%
  mutate(em_bin = purrr::map(em, ~(.$img_ratios[[1]] > 10))) %>%
  mutate(quick_clean =  purrr::map(em_bin, ShoeScrubR:::img_auto_clean)) %>%
  mutate(label = purrr::map(quick_clean, bwlabel)) %>%
  mutate(corner_clean = purrr::map(label, ShoeScrubR:::clean_img_corners))


cols <- 2
plot_dims <- c(pmax(ceiling(nrow(scan_info)/cols), 1), pmin(nrow(scan_info), cols))
png(filename = file.path(img_output_dir, "EM_pixsets.png"), width = 300*3*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$em, ~plot(Image(abind::abind(.$img_ratios, along = 3)), all = T, nx = 3))
dev.off()

cols <- 8
plot_dims <- c(pmax(ceiling(nrow(scan_info)/cols), 1), pmin(nrow(scan_info), cols))
png(filename = file.path(img_output_dir, "EM_LR10_pixset.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$em_bin, plot)
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_clean.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$quick_clean, plot)
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_label.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$label, ~plot(colorLabels(.)))
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_corner_clean.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$corner_clean, ~plot(colorLabels(.)))
dev.off()

scan_info <- scan_info %>%
  mutate(clean_img = purrr::map2(img, corner_clean, ~.x * (.y > 0) + (.y == 0))) %>%
  mutate(blur = purrr::map(clean_img, ~gblur(., sigma = sqrt(length(.))/50, boundary = "replicate"))) %>%
  mutate(exag_mask = purrr::map(clean_img, ShoeScrubR:::img_clean_blur))

png(filename = file.path(img_output_dir, "EM_LR10_clean_img.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$clean_img, plot)
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_blur_img.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$blur, plot)
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_blur_thresh.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$blur, ~plot(. < median(.)))
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_blur_label.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$blur, ~plot(colorLabels(bwlabel(. < median(.)))))
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_exag_mask.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$exag_mask, plot)
dev.off()

scan_info <- scan_info %>%
  mutate(clean_mask =  purrr::map(exag_mask, ShoeScrubR:::img_mask_clean))

png(filename = file.path(img_output_dir, "EM_LR10_mask_final.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$clean_mask, plot)
dev.off()

png(filename = file.path(img_output_dir, "EM_LR10_image_mask_final.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk2(scan_info$img, scan_info$clean_mask,  ~plot(.x * (.y > 0) + (.y == 0)))
dev.off()

scan_info <- scan_info %>%
  mutate(ppi = purrr::map_dbl(im_dim, ~round(mean(./c(7, 13))/100)*100)) %>%
  left_join(select(shoe_info, ShoeID, Brand, Size, Shoe_foot, mask, ppi))

scan_info <- scan_info %>%
  mutate(align_mask = purrr::map2(img, mask, rough_align))


plot_align <- function(df) {
  thresh_intersect <- 1 - thresh((1 - df$img)*df$mask, w = 5, h = 5, offset = 0.02)
  rgbImage(1 - df$mask, df$img, thresh_intersect) %>% plot()
}

png(filename = file.path(img_output_dir, "EM_LR10_alignment.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$align_mask, plot_align)
dev.off()