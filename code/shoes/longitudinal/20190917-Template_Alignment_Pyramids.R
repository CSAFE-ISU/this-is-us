library(tidyverse)
library(EBImage)
library(ShoeScrubR) # SHA: fe17d486

img_output_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"
lss_dir <- "/lss/research/csafe-shoeprints/ShoeImagingPermanent"

# Setup and initial cleaning
orig_img <- EBImage::readImage(file.path(lss_dir, "040639L_20180307_5_1_1_boekhoff_pashek_jekruse.tif"))
img <- orig_img %>% channel("luminance")

inv_img <- img %>%
  filter2(makeBrush(25, "gaussian")) %>%
  normalize() %>%
  magrittr::subtract(1, .)
thresh_img <- inv_img > .15

orig_mask <- shoe_mask("Nike", 8, "R", ppi = 300) %>% ShoeScrubR:::auto_resize_img(final_dims = dim(img))

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

padded_exag_img <- ShoeScrubR:::pad_to_center(exaggerated_img, img_center)
padded_img <- ShoeScrubR:::pad_to_center(thresh_img, img_center)
padded_mask <- ShoeScrubR:::pad_to_center(centered_mask, img_center)

im_list <- list(orig = img, inv = inv_img, thresh = thresh_img, exaggerated = exaggerated_img, orig_mask = orig_mask, centered_mask = centered_mask)
im_px_mode <- c(1, 0, 0, 0, 0, 0)
im_list_pad <- purrr::map2(im_list, im_px_mode, ~pad_to_center(img = .x, value = .y, center = img_center))

resize_list <- function(imlist, ...) {
  purrr::map(imlist, EBImage::resize, ...)
}

pad_dim <- dim(im_list_pad[[1]])
pyramid <- tibble(scale_factor = c(1, seq(2, 20, 2))) %>%
  mutate(dims = purrr::map(scale_factor, ~floor(pad_dim/.))) %>%
  mutate(ims = purrr::map(dims, ~resize_list(im_list_pad, w = .[1], h = .[2])),
         type = purrr::map(ims, names)) %>%
  unnest(ims, type, .drop = F) %>%
  mutate(df = purrr::map(ims, image_to_df))

# png(filename = file.path(img_output_dir, "ImagePyramids.png"), width = 300*6, height = 300*2*2, units = "px")
# par(mfrow = c(2, 6))
# purrr::map2(pyramid$ims, pyramid$scale_factor, ~{
#   dd <- paste(dim(.x$thresh), collapse = ",")
#   plot(.x$thresh)
#   text(0, 0, labels = paste0("Scale: ", .y, "(", dd, ")"),
#        col = "white", cex = 2, adj = c(-0.5, 1.2))
# })
# dev.off()
#
#
# png(filename = file.path(img_output_dir, "MaskPyramids.png"), width = 300*6, height = 300*2*2, units = "px")
# par(mfrow = c(2, 6))
# purrr::map2(pyramid$ims, pyramid$scale_factor, ~{
#   dd <- paste(dim(.x$centered_mask), collapse = ",")
#   plot(.x$centered_mask)
#   text(0, 0, labels = paste0("Scale: ", .y, "(", dd, ")"),
#        col = "white", cex = 2, adj = c(-0.5, 1.2))
# })
# dev.off()

# ggplot(aes(x = row, y = col), data = im_df %>% mutate(km = kmeans(df, df[sample(1:nrow(df), 40, replace = F),])$cluster)) + ggvoronoi::geom_voronoi(aes(fill = factor(km)), alpha = .2) + geom_point(aes(color = factor(km)))

align_prcomp <- function(im_df) {
  df <- im_df[,c("row", "col")] %>% as.matrix()
  pca <- prcomp(df)
  angle <- pca$rotation[2,1] %>% acos() %>% `*`(180/pi)
  angle
}

pyramid <- pyramid %>%
  mutate(rot_angle_prcomp = purrr::map_dbl(df, align_prcomp)) %>%
  unnest(rot_angle_prcomp)

mask_imgs <- pyramid %>%
  filter(type %in% c("centered_mask", "thresh")) %>%
  mutate(rot_img = purrr::map2(ims, rot_angle_prcomp, ~EBImage::rotate(.x, angle = .y, output.dim = dim(.x)))) %>%
  mutate(rot_center = purrr::map(rot_img, . %>%
                                   image_to_df() %>%
                                   summarize(row = round(mean(row)), col = round(mean(col))) %>%
                                   unlist()))

masking_img <- mask_imgs %>%
  select(scale_factor, type, rot_img) %>%
  tidyr::spread(key = type, value = rot_img) %>%
  left_join(
    select(mask_imgs, scale_factor, type, rot_center) %>%
      mutate(type = paste0(type, "_rot_center")) %>%
      tidyr::spread(key = type, value = rot_center) %>%
      mutate(mask_shift = purrr::map2(centered_mask_rot_center, thresh_rot_center, ~.x - .y)) %>%
      select(-matches("rot_center"))
  ) %>%
  mutate(recentered_mask = purrr::map2(centered_mask, mask_shift, ~translate(.x, -.y, bg.col = 0)))


masking_img_2 <- masking_img %>%
  select(scale_factor, mask = recentered_mask, img = thresh) %>%
  mutate(by = round(40/(scale_factor)))




get_offsets <- function(dims, by = 2, range = list(width = c(.3, .7), height = c(.3, .7))) {
  width_px_range <- round(dims[1]*range$width)
  height_px_range <- round(dims[2]*range$height)
  
  width_px <- seq(width_px_range[1], width_px_range[2], by = by) - floor(dims[1]/2)
  height_px <- seq(height_px_range[1], height_px_range[2], by = by) - floor(dims[2]/2)
  
  tidyr::crossing(row = height_px, col = width_px) %>%
    mutate(id = 1:n()) %>%
    nest(row:col, .key = "offset")
}

tmp <- masking_img_2 %>%
  filter(scale_factor > 5) %>%
  mutate(offsets = map2(img, by, ~get_offsets(dim(.x), by = .y))) %>%
  unnest(offsets, .drop = F) %>%
  mutate(t_mask = map2(mask, offset, ~ translate(.x, -as.numeric(unlist(.y)), bg.col = 0))) %>%
  mutate(overlap = purrr::map2_dbl(img,t_mask, ~(sum(.x*.y)/sum(.x)))) %>%
  group_by(scale_factor) %>%
  arrange(desc(overlap)) %>%
  unnest(offset) %>%
  filter(row_number() <= 1) %>%
  arrange(desc(scale_factor))

tmp <- tmp %>%
  arrange(desc(scale_factor)) %>%
  mutate(color_img = purrr::map2(img, t_mask, ~rgbImage(red = .x*.y, green = .x, blue = .y))) %>%
  mutate(label = purrr::pmap_chr(list(overlap, row, col, scale_factor), ~sprintf("Scale:1/%d\n%.1f%% overlap,\nOS = (%d, %d)",..4,  ..1*100, round(..2)*..4, round(..3)*..4)))

png(filename = file.path(img_output_dir, "ShiftPyramids.png"), width = 300*nrow(tmp), height = 300*2*1, units = "px")
par(mfrow = c(1, nrow(tmp)))
purrr::map2(tmp$color_img, tmp$label, ~{
  plot(.x)
  text(0, 0, labels = .y, col = "white", adj = c(-.1, 1.2))
})
dev.off()


