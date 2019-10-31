library(EBImage)
library(ShoeScrubR)
library(tidyverse)

set.seed(3142095)

img_output_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"
lss_dir <- "/lss/research/csafe-shoeprints/ShoeImagingPermanent"

# For a bunch of images...
full_imglist <- list.files("/lss/research/csafe-shoeprints/ShoeImagingPermanent/",
                           pattern = "00[4-9]\\d{3}[L]_\\d{8}_5_._1_.*_.*_.*", full.names = T)
dir <- "/tmp/film-prints"
if (!dir.exists(dir)) dir.create(dir)

file.copy(full_imglist, file.path(dir, basename(full_imglist)))
imglist <- file.path(dir, basename(full_imglist))

shoe_info <- read_csv("~/Projects/CSAFE/2018_Longitudinal_Shoe_Project/Clean_Data/shoe-info.csv") %>%
  filter(ShoeID %in% str_sub(basename(imglist), 1, 3)) %>%
  select(ShoeID, Brand, Size) %>%
  mutate(Size = str_remove(Size, "[ MW]") %>% parse_number()) %>%
  crossing(tibble(Mask_foot = c("R", "L"), Shoe_foot = c("L", "R")), ppi = c(200, 300)) %>%
  mutate(mask = purrr::pmap(list(Brand, Size, Mask_foot, ppi = ppi), shoe_mask))


scan_info <- tibble(
  file = imglist,
  ShoeID = str_extract(basename(file), "^\\d{3}"),
  Shoe_foot = str_extract(basename(file), "\\d{6}[RL]") %>% str_remove_all("\\d"),
  date = str_extract(basename(file), "\\d{8}") %>% parse_date(format = "%Y%m%d"),
  rep = str_extract(basename(file), "5_[12]_1") %>% str_remove("5_|_1")
) %>%
  left_join(unique(select(shoe_info, ShoeID, Brand, Size, Shoe_foot))) %>%
  mutate(
    img = purrr::map(file, EBImage::readImage, all = F),
    img = purrr::map(img, EBImage::channel, "luminance"),
    im_dim = purrr::map(img, dim)
  )

cols <- 12
plot_dims <- c(pmax(ceiling(nrow(scan_info)/cols), 1), pmin(nrow(scan_info), cols))
# png(filename = file.path(img_output_dir, "Alignment_Orig_files.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$img, plot)
# dev.off()


align_scan_data <- select(scan_info, ShoeID, Shoe_foot, date, rep, Brand, Size, img) %>%
  mutate(rep = paste0("img", str_sub(rep, 1, 1))) %>%
  tidyr::spread(key = rep, value = img) %>%
  mutate(aligned = purrr::map2(img1, img2, fft_align))

cols <- 6
plot_dims <- c(pmax(ceiling(nrow(align_scan_data)/cols), 1), pmin(nrow(align_scan_data), cols))
png(filename = file.path(img_output_dir, "FFT_Alignment_Orig_20191031.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(align_scan_data$aligned, ~ShoeScrubR:::plot_imlist(.[[3]]))
dev.off()
png(filename = file.path(img_output_dir, "FFT_Alignment_Angle_Only_20191031.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(align_scan_data$aligned, ~ShoeScrubR:::plot_imlist(.[[2]]))
dev.off()

png(filename = file.path(img_output_dir, "FFT_Alignment_20191031.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(align_scan_data$aligned, ~ShoeScrubR:::plot_imlist(.[[1]]))
dev.off()