library(tidyverse)
library(ShoeScrubR)
devtools::load_all("~/Projects/CSAFE/ShoeScrubR")

img_output_dir <- "~/Projects/CSAFE/2019-this_is_us/images/shoes/longitudinal/"

img1 <- matrix(1, nrow = 1500, ncol = 1500) %>% EBImage::as.Image()
img1[400:1200, 700:900] <- 0
img1[300:350, 400:1200] <- 0
img1[830:850, 900:1050] <- .5
img2 <- img1 %>%
          img_translate(v = c(50, 50), output.dim = c(1500, 1500), bg.col = 1) %>%
          img_rotate(15, output.origin = c(750,750), output.dim = c(1550, 1500), bg.col = 1)

noise1 <- sample(c(0, 1), length(img1), prob = c(.9, .1), replace = T) * rnorm(length(img1), sd = .15)
noise2 <- sample(c(0, 1), length(img1), prob = c(.9, .1), replace = T) * rnorm(length(img1), sd = .15)
img1 <- pmin(pmax(img1 + noise1, 0), 1)
img2 <- pmin(pmax(img2 + noise2, 0), 1)

png(filename = file.path(img_output_dir, "FFT_Demo_Orig.png"), width = 300*2, height = 300, units = "px")
par(mfrow = c(1, 2))
plot(img1)
plot(img2)
dev.off()

# Ensure same size, origin
ims <- pad_img_match(1 - img1, 1 - img2, value = .5)

png(filename = file.path(img_output_dir, "FFT_Demo_Pad.png"), width = 300, height = 300, units = "px")
plot_imlist(ims)
dev.off()

ims <- pad_img_match(1 - img1, 1 - img2, value = 1)

img_size <- dim(ims[[1]])
img_origin <- round(img_size/2)

# Keep bkgd?
bg_col <- .5

# Apply Hanning window to reduce edge discontinuities
ims_han <- hanning_list(ims, invert = F, widen_root = c(.5, .5))

png(filename = file.path(img_output_dir, "FFT_Demo_Hann.png"), width = 300, height = 300, units = "px")
plot_imlist(ims_han)
dev.off()



# FFT image
ffts <- fft_list(ims_han)


png(filename = file.path(img_output_dir, "FFT_Demo_FFT1.png"), width = 300*2, height = 300, units = "px")
par(mfrow = c(1, 2))
plot(normalize(log((Re(ffts[[1]]*Conj(ffts[[1]]))))))
plot(normalize(log((Re(ffts[[2]]*Conj(ffts[[2]]))))))
dev.off()

nt <- 720
# Use the FT magnitude as a new image to get rotation
im_fft_mag <- lapply(ffts, function(x) Re(sqrt(x * Conj(x))))
# Convert to polar coords
im_polar <- lapply(im_fft_mag, function(x) img_to_polar(x, ntheta = nt))

png(filename = file.path(img_output_dir, "FFT_Demo_FFT2_Polar.png"), width = 300*3, height = 300, units = "px")
par(mfrow = c(1, 3))
plot(normalize(log(im_polar[[1]])))
plot(normalize(log(im_polar[[2]])))
plot_imlist(list(normalize(log(im_polar[[1]])), normalize(log(im_polar[[2]]))))
dev.off()

angle.lim <- NULL
# Recover angle via polar transform of fft magnitude
nt <- 1000
thetas <- seq(0, 2*pi, length.out = nt)
angle_reg <- angle_recover(ffts, nt = nt)
angle_reg2 <- angle_reg
if (!is.null(angle.lim)) {
  # Enforce angle constraint
  invalid_thetas <- cos(thetas) <= cos(angle.lim)
  angle_reg2[invalid_thetas,] <- -Inf
}
theta <- thetas[get_value_idxs(angle_reg2)[1]]*180/pi

# Update with rotated image 2
ims_fix <- ims
ims_fix[[2]] <- img_rotate(ims[[2]], angle = theta, bg.col = bg_col,
                           output.dim = img_size,
                           output.origin = img_origin)


ims_han_fix <- hanning_list(ims_fix, invert = F)
png(filename = file.path(img_output_dir, "FFT_Demo_FFT3_Rotate.png"), width = 300*2, height = 300, units = "px")
par(mfrow = c(1, 2))
plot_imlist(ims_fix)
plot_imlist(ims_han_fix)
dev.off()


# Recover shift
ifft <- do.call(fft_register, ims_han_fix) # 1 on 2
trans.lim <- NULL
if (!is.null(trans.lim)) {
  # Enforce translation constraint
  idx_mat <- ifft * 0 - Inf
  valid_shifts_x <- 1:trans.lim[1]
  valid_shifts_y <- 1:trans.lim[2]
  idx_mat[valid_shifts_x, valid_shifts_y] <- 0
  idx_mat[img_size[1] - valid_shifts_x + 1, valid_shifts_y] <- 0
  idx_mat[valid_shifts_x, valid_shifts_y] <- 0
  idx_mat[img_size[1] - valid_shifts_x + 1, img_size[2] - valid_shifts_y + 1] <- 0
} else {
  idx_mat <- ifft * 0
}

png(filename = file.path(img_output_dir, "FFT_Demo_FFT4_Shift.png"), width = 300, height = 300, units = "px")
plot(normalize(log(pmax(1000 + 0*ifft, ifft))))
dev.off()

transform_2_by <- mapply(fix_coord, get_value_idxs(ifft + idx_mat), img_size)

ims_fix_trans <- ims_fix
ims_fix_trans[[1]] <- img_translate(ims_fix[[1]], v = -pmin(0, transform_2_by),
                                    bg.col = bg_col, output.dim = img_size)
ims_fix_trans[[2]] <- img_translate(ims_fix[[2]], v = pmax(0, transform_2_by),
                                    bg.col = bg_col, output.dim = img_size)

png(filename = file.path(img_output_dir, "FFT_Demo_FFT5_Shift.png"), width = 300, height = 300, units = "px")
plot_imlist(ims_fix_trans)
dev.off()