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

scan_info <- scan_info %>%
  mutate(ppi = purrr::map_dbl(im_dim, est_ppi_film)) %>%
  left_join(shoe_info) %>%
  mutate(align = purrr::map2(img, mask, rough_align))


plot_align <- function(df) {
  thresh_intersect <- 1 - thresh((1 - df$img)*df$mask, w = 5, h = 5, offset = 0.02)
  rgbImage(1 - df$mask, df$img, thresh_intersect) %>% plot()
}

# png(filename = file.path(img_output_dir, "Alignment_Mask_Img_Align.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(scan_info$align, plot_align)
# dev.off()

# Need to fix center-of-mass based alignment (or just search over a wider range of shifts)
# May want to find the minimum width once angle alignment is complete and use that as "center"?
# How to use that approach with Nikes?


scan_info <- scan_info %>%
  mutate(aligned_img = purrr::map(align, "img"),
         # aligned_mask = purrr::map(align, "mask")
         aligned_img_thresh = purrr::map(aligned_img, ~thresh(., w = 250, h = 250)),
         aligned_mask = purrr::map2(align, aligned_img_thresh,
                                    ~(1 - .y) * ((round(.x$mask + .x$exag_img) >= 1))),
         clean_img = purrr::map2(aligned_img, aligned_mask,
                                 ~(normalize(.x)*(.y != 0) + (.y==0)) %>%
                                   normalize()),
         clean_img = purrr::map2(clean_img, align, ~{
           tmp <- .y$mask %>% dilate(makeBrush(51, "disc"))
           (tmp ==1 )*.x + (tmp != 1)
         })
  ) %>%
  mutate(clean_dim = purrr::map_int(aligned_img, ~max(dim(.))))

library(RNiftyReg)

align_scaled_matrix <- function(im1, im2, scale, im1mask = NULL, im2mask = NULL, ...) {
  
  res <- niftyreg(im1, im2, scope = "rigid", init = diag(c(1, 1, 1, 1)),
                  sourceMask = im1mask, targetMask = im2mask, estimateOnly = T, ...)
  
  scale_mat <- matrix(1, nrow = 4, ncol = 4)
  scale_mat[1:2,4] <- scale
  
  res$forwardTransforms[[1]] <- res$forwardTransforms[[1]]*scale_mat
  res$reverseTransforms[[1]] <- res$reverseTransforms[[1]]*scale_mat
  
  im1transmat <- matrix(0, nrow = 3, ncol = 2)
  im1transmat[1:2,] <- t(res$reverseTransforms[[1]][1:2, 1:2])
  im1transmat[3, 1:2] <- res$reverseTransforms[[1]][1:2, 4]
  
  list(matrix = im1transmat, alignment_result = res)
}

align_images_matrix <- function(im1, im2, affine_mat) {
  
  im1trans <- EBImage::affine(im1, m = affine_mat, output.dim = dim(im1), bg.col = img_mode(im1))
  
  end_dim <- pmax(dim(im1trans), dim(im2))
  
  pad1dim <- end_dim - dim(im1trans)
  pad2dim <- end_dim - dim(im2)
  
  im1trans <- im1trans %>% img_pad(right = pad1dim[1], bottom = pad1dim[2], value = img_mode(.))
  im2trans <- im2 %>% img_pad(right = pad2dim[1], bottom = pad2dim[2], value = img_mode(.))
  
  list(im1 = im1trans, im2 = im2trans)
}

align_scan_data <- select(scan_info, ShoeID, Shoe_foot, date, rep, Brand, Size,
                          clean_img) %>%
  mutate(rep = paste0("img", str_sub(rep, 1, 1))) %>%
  tidyr::spread(key = rep, value = clean_img) %>%
  left_join(
    select(scan_info, ShoeID, Shoe_foot, date, rep, Brand, Size,
           aligned_mask) %>%
      mutate(rep = paste0("mask", str_sub(rep, 1, 1))) %>%
      tidyr::spread(key = rep, value = aligned_mask)
  ) %>%
  left_join(
    select(scan_info, ShoeID, Shoe_foot, date, rep, Brand, Size,
           aligned_img) %>%
      mutate(rep = paste0("orig_img", str_sub(rep, 1, 1))) %>%
      tidyr::spread(key = rep, value = aligned_img)
  )

align_scan_data <- align_scan_data %>%
  mutate(scale = purrr::map2_dbl(img1, img2, ~ceiling(max(pmax(dim(.x), dim(.y)))/2048))) %>%
  mutate(img1_scaled = purrr::map2(img1, scale,
                                   ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                               h = round(dim(.x)[2]/.y))),
         img2_scaled = purrr::map2(img2, scale,
                                   ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                               h = round(dim(.x)[2]/.y))),
         mask1_scaled = purrr::map2(mask1, scale,
                                    ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                                h = round(dim(.x)[2]/.y))),
         mask2_scaled = purrr::map2(mask2, scale,
                                    ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                                h = round(dim(.x)[2]/.y))),
         warp_res = purrr::pmap(list(img1_scaled, img2_scaled, scale),
                                align_scaled_matrix))

align_scan_data <- align_scan_data %>%
  mutate(warp_matrix = purrr::map(warp_res, "matrix")) %>%
  mutate(align_bin_images = purrr::pmap(list(img1, img2, warp_matrix), align_images_matrix))

align_scan_data <- align_scan_data %>%
  mutate(align_images = purrr::pmap(list(orig_img1, orig_img2, warp_matrix), align_images_matrix))

cols <- 6
plot_dims <- c(pmax(ceiling(nrow(align_scan_data)/cols), 1), pmin(nrow(align_scan_data), cols))
# png(filename = file.path(img_output_dir, "Alignment_Result.png"), width = 300*plot_dims[2], height = 300*2*plot_dims[1], units = "px")
par(mfrow = plot_dims)
purrr::walk(align_scan_data$align_images, ~plot(rgbImage(.[[1]], .[[2]], pmax(.[[1]], .[[2]]))))
# dev.off()

## Testing whether image scaling affects time or results...
res <- select(align_scan_data, 1:5, warp_res)


align_scan_data <- align_scan_data %>%
  mutate(scale = purrr::map2_dbl(img1, img2, ~ceiling(max(pmax(dim(.x), dim(.y)))/512))) %>%
  mutate(img1_scaled = purrr::map2(img1, scale,
                                   ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                               h = round(dim(.x)[2]/.y))),
         img2_scaled = purrr::map2(img2, scale,
                                   ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                               h = round(dim(.x)[2]/.y))),
         mask1_scaled = purrr::map2(mask1, scale,
                                    ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                                h = round(dim(.x)[2]/.y))),
         mask2_scaled = purrr::map2(mask2, scale,
                                    ~img_resize(.x, w = round(dim(.x)[1]/.y),
                                                h = round(dim(.x)[2]/.y))),
         warp_res = purrr::pmap(list(img1_scaled, img2_scaled, scale),
                                align_scaled_matrix))

align_scan_data <- align_scan_data %>%
  mutate(warp_matrix = purrr::map(warp_res, "matrix")) %>%
  mutate(align_bin_images = purrr::pmap(list(img1, img2, warp_matrix), align_images_matrix)) %>%
  mutate(align_images = purrr::pmap(list(orig_img1, orig_img2, warp_matrix), align_images_matrix))

par(mfrow = plot_dims)
purrr::walk(align_scan_data$align_images, ~plot(rgbImage(.[[1]], .[[2]], pmax(.[[1]], .[[2]]))))
