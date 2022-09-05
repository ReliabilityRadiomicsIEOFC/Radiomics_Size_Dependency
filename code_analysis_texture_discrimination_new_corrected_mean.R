set.seed(5)

sep <- "," # choose separator

## Load doMC libraries
sysinf <- Sys.info()
if (!is.null(sysinf)){
  os <- sysinf['sysname']
  if (os == 'Darwin') {
    library(doMC)
    registerDoMC(cores=4)
  }
}

options(show.error.locations = TRUE) # debug
options(error=traceback)

dir_save_plots <- '/Users/MyUser/Documents/Papers/texture_discrimination/Figures/'

## import libraries
library(caret)
library(irr)
library(scales)

####
# Discard of features with normalized counterparts
####
voxel_volume_A <- 0.625 * 0.625 * 5.5000104904174805
voxel_volume_B <- 0.64393937587738 * 0.64393937587738 * 5.0
voxel_volume_C <- 0.6071428656578064 * 0.6071428656578064 * 4.999999046325684

filename_A <- '/Users/MyUser/Documents/Papers/texture_discrimination/repeatable_features_A.csv'
filename_B <- '/Users/MyUser/Documents/Papers/texture_discrimination/repeatable_features_B.csv'
filename_C <- '/Users/MyUser/Documents/Papers/texture_discrimination/repeatable_features_C.csv'

dataset_A <- read.csv(filename_A, header = TRUE, sep = sep)
dataset_B <- read.csv(filename_B, header = TRUE, sep = sep)
dataset_C <- read.csv(filename_C, header = TRUE, sep = sep)

n_voxels_A <- dataset_A[, "original_shape_VoxelVolume"]/voxel_volume_A
n_voxels_B <- dataset_B[, "original_shape_VoxelVolume"]/voxel_volume_B
n_voxels_C <- dataset_C[, "original_shape_VoxelVolume"]/voxel_volume_C

##############
# Read original feature files
##############
filename_A1 <- '/Users/MyUser/Documents/Papers/texture_discrimination/output_pyradiomics_A_test.csv'
filename_A2 <- '/Users/MyUser/Documents/Papers/texture_discrimination/output_pyradiomics_A_retest.csv'
filename_B1 <- '/Users/MyUser/Documents/Papers/texture_discrimination/output_pyradiomics_B_test.csv'
filename_B2 <- '/Users/MyUser/Documents/Papers/texture_discrimination/output_pyradiomics_B_retest.csv'
filename_C1 <- '/Users/MyUser/Documents/Papers/texture_discrimination/output_pyradiomics_C_test.csv'
filename_C2 <- '/Users/MyUser/Documents/Papers/texture_discrimination/output_pyradiomics_C_retest.csv'

dataset_A1 <- read.csv(filename_A1, header = TRUE, sep = sep)
dataset_A2 <- read.csv(filename_A2, header = TRUE, sep = sep)
dataset_B1 <- read.csv(filename_B1, header = TRUE, sep = sep)
dataset_B2 <- read.csv(filename_B2, header = TRUE, sep = sep)
dataset_C1 <- read.csv(filename_C1, header = TRUE, sep = sep)
dataset_C2 <- read.csv(filename_C2, header = TRUE, sep = sep)

### Dataset A
list_of_non_normlzd_and_normlzd_feats_A <- lapply(c('glrlm_GrayLevelNonUniformity', 'glrlm_RunLengthNonUniformity', 'glszm_GrayLevelNonUniformity', 'gldm_DependenceNonUniformity'), function(r) grep(r, colnames(dataset_A)))
list_of_corrspdng_normlzd_feats_A <- lapply(c('glrlm_GrayLevelNonUniformityNormalized', 'glrlm_RunLengthNonUniformityNormalized', 'glszm_GrayLevelNonUniformityNormalized', 'gldm_DependenceNonUniformityNormalized'), function(r) grep(r, colnames(dataset_A)))
list_of_non_normlzd_feats_2_rmv_A <- list()
for(i in seq_len(length(list_of_non_normlzd_and_normlzd_feats_A))) {
  list_of_non_normlzd_feats_2_rmv_A[[i]] <- list_of_non_normlzd_and_normlzd_feats_A[[i]][-which(list_of_non_normlzd_and_normlzd_feats_A[[i]] %in% list_of_corrspdng_normlzd_feats_A[[i]])]
}

features_2_rmv_A <- unlist(list_of_non_normlzd_feats_2_rmv_A)
colnames(dataset_A)[unlist(list_of_non_normlzd_feats_2_rmv_A)]
clean_dataset_A <- dataset_A[, -c(features_2_rmv_A)]
clean_dataset_A <- clean_dataset_A[, !grepl("original_shape", colnames(clean_dataset_A))]

### Dataset B
list_of_non_normlzd_and_normlzd_feats_B <- lapply(c('glrlm_GrayLevelNonUniformity', 'glrlm_RunLengthNonUniformity', 'glszm_GrayLevelNonUniformity', 'gldm_DependenceNonUniformity'), function(r) grep(r, colnames(dataset_B)))
list_of_corrspdng_normlzd_feats_B <- lapply(c('glrlm_GrayLevelNonUniformityNormalized', 'glrlm_RunLengthNonUniformityNormalized', 'glszm_GrayLevelNonUniformityNormalized', 'gldm_DependenceNonUniformityNormalized'), function(r) grep(r, colnames(dataset_B)))
list_of_non_normlzd_feats_2_rmv_B <- list()
for(i in seq_len(length(list_of_non_normlzd_and_normlzd_feats_B))) {
  list_of_non_normlzd_feats_2_rmv_B[[i]] <- list_of_non_normlzd_and_normlzd_feats_B[[i]][-which(list_of_non_normlzd_and_normlzd_feats_B[[i]] %in% list_of_corrspdng_normlzd_feats_B[[i]])]
}

features_2_rmv_B <- unlist(list_of_non_normlzd_feats_2_rmv_B)
colnames(dataset_B)[unlist(list_of_non_normlzd_feats_2_rmv_B)]
clean_dataset_B <- dataset_B[, -c(features_2_rmv_B)]
clean_dataset_B <- clean_dataset_B[, !grepl("original_shape", colnames(clean_dataset_B))]

### Dataset C
list_of_non_normlzd_and_normlzd_feats_C <- lapply(c('glrlm_GrayLevelNonUniformity', 'glrlm_RunLengthNonUniformity', 'glszm_GrayLevelNonUniformity', 'gldm_DependenceNonUniformity'), function(r) grep(r, colnames(dataset_C)))
list_of_corrspdng_normlzd_feats_C <- lapply(c('glrlm_GrayLevelNonUniformityNormalized', 'glrlm_RunLengthNonUniformityNormalized', 'glszm_GrayLevelNonUniformityNormalized', 'gldm_DependenceNonUniformityNormalized'), function(r) grep(r, colnames(dataset_C)))
list_of_non_normlzd_feats_2_rmv_C <- list()
for(i in seq_len(length(list_of_non_normlzd_and_normlzd_feats_C))) {
  list_of_non_normlzd_feats_2_rmv_C[[i]] <- list_of_non_normlzd_and_normlzd_feats_C[[i]][-which(list_of_non_normlzd_and_normlzd_feats_C[[i]] %in% list_of_corrspdng_normlzd_feats_C[[i]])]
}

features_2_rmv_C <- unlist(list_of_non_normlzd_feats_2_rmv_C)
colnames(dataset_C)[unlist(list_of_non_normlzd_feats_2_rmv_C)]
clean_dataset_C <- dataset_C[, -c(features_2_rmv_C)]
clean_dataset_C <- clean_dataset_C[, !grepl("original_shape", colnames(clean_dataset_C))]


dataset_A1_filt <- dataset_A1[, which(colnames(dataset_A1) %in% colnames(clean_dataset_A))]
dataset_A2_filt <- dataset_A2[, which(colnames(dataset_A2) %in% colnames(clean_dataset_A))]
dataset_B1_filt <- dataset_B1[, which(colnames(dataset_B1) %in% colnames(clean_dataset_B))]
dataset_B2_filt <- dataset_B2[, which(colnames(dataset_B2) %in% colnames(clean_dataset_B))]
dataset_C1_filt <- dataset_C1[, which(colnames(dataset_C1) %in% colnames(clean_dataset_C))]
dataset_C2_filt <- dataset_C2[, which(colnames(dataset_C2) %in% colnames(clean_dataset_C))]

# corrected_clean_dataset_A <- clean_dataset_A
# corrected_clean_dataset_B <- clean_dataset_B
# corrected_clean_dataset_C <- clean_dataset_C

corrected_clean_dataset_A1 <- dataset_A1_filt
corrected_clean_dataset_A2 <- dataset_A2_filt
corrected_clean_dataset_B1 <- dataset_B1_filt
corrected_clean_dataset_B2 <- dataset_B2_filt
corrected_clean_dataset_C1 <- dataset_C1_filt
corrected_clean_dataset_C2 <- dataset_C2_filt
for (feature_name in c('firstorder_Energy', 'firstorder_TotalEnergy', 'gldm_GrayLevelNonUniformity', 'ngtdm_Coarseness')) {
  if (feature_name %in% c('firstorder_Energy', 'firstorder_TotalEnergy', 'gldm_GrayLevelNonUniformity')) {
    # corrected_clean_dataset_A[ , grep(feature_name, colnames(clean_dataset_A))] <- corrected_clean_dataset_A[, grep(feature_name, colnames(clean_dataset_A))]/rep(n_voxels_A,3)
    # corrected_clean_dataset_B[ , grep(feature_name, colnames(clean_dataset_B))] <- corrected_clean_dataset_B[, grep(feature_name, colnames(clean_dataset_B))]/rep(n_voxels_B,3)
    # corrected_clean_dataset_C[ , grep(feature_name, colnames(clean_dataset_C))] <- corrected_clean_dataset_C[, grep(feature_name, colnames(clean_dataset_C))]/rep(n_voxels_C,3)

    corrected_clean_dataset_A1[ , grep(feature_name, colnames(dataset_A1_filt))] <- corrected_clean_dataset_A1[, grep(feature_name, colnames(dataset_A1_filt))]/rep(n_voxels_A,3)
    corrected_clean_dataset_A2[ , grep(feature_name, colnames(dataset_A2_filt))] <- corrected_clean_dataset_A2[, grep(feature_name, colnames(dataset_A2_filt))]/rep(n_voxels_A,3)
    corrected_clean_dataset_B1[ , grep(feature_name, colnames(dataset_B1_filt))] <- corrected_clean_dataset_B1[, grep(feature_name, colnames(dataset_B1_filt))]/rep(n_voxels_B,3)
    corrected_clean_dataset_B2[ , grep(feature_name, colnames(dataset_B2_filt))] <- corrected_clean_dataset_B2[, grep(feature_name, colnames(dataset_B2_filt))]/rep(n_voxels_B,3)
    corrected_clean_dataset_C1[ , grep(feature_name, colnames(dataset_C1_filt))] <- corrected_clean_dataset_C1[, grep(feature_name, colnames(dataset_C1_filt))]/rep(n_voxels_C,3)
    corrected_clean_dataset_C2[ , grep(feature_name, colnames(dataset_C2_filt))] <- corrected_clean_dataset_C2[, grep(feature_name, colnames(dataset_C2_filt))]/rep(n_voxels_C,3)
  } else { # case of ngtdm_Coarseness
    # corrected_clean_dataset_A[ , grep(feature_name, colnames(clean_dataset_A))] <- corrected_clean_dataset_A[, grep(feature_name, colnames(clean_dataset_A))]*rep(n_voxels_A,3)
    # corrected_clean_dataset_B[ , grep(feature_name, colnames(clean_dataset_B))] <- corrected_clean_dataset_B[, grep(feature_name, colnames(clean_dataset_B))]*rep(n_voxels_B,3)
    # corrected_clean_dataset_C[ , grep(feature_name, colnames(clean_dataset_C))] <- corrected_clean_dataset_C[, grep(feature_name, colnames(clean_dataset_C))]*rep(n_voxels_C,3)

    corrected_clean_dataset_A1[ , grep(feature_name, colnames(dataset_A1_filt))] <- corrected_clean_dataset_A1[, grep(feature_name, colnames(dataset_A1_filt))]*rep(n_voxels_A,3)
    corrected_clean_dataset_A2[ , grep(feature_name, colnames(dataset_A2_filt))] <- corrected_clean_dataset_A2[, grep(feature_name, colnames(dataset_A2_filt))]*rep(n_voxels_A,3)
    corrected_clean_dataset_B1[ , grep(feature_name, colnames(dataset_B1_filt))] <- corrected_clean_dataset_B1[, grep(feature_name, colnames(dataset_B1_filt))]*rep(n_voxels_B,3)
    corrected_clean_dataset_B2[ , grep(feature_name, colnames(dataset_B2_filt))] <- corrected_clean_dataset_B2[, grep(feature_name, colnames(dataset_B2_filt))]*rep(n_voxels_B,3)
    corrected_clean_dataset_C1[ , grep(feature_name, colnames(dataset_C1_filt))] <- corrected_clean_dataset_C1[, grep(feature_name, colnames(dataset_C1_filt))]*rep(n_voxels_C,3)
    corrected_clean_dataset_C2[ , grep(feature_name, colnames(dataset_C2_filt))] <- corrected_clean_dataset_C2[, grep(feature_name, colnames(dataset_C2_filt))]*rep(n_voxels_C,3)
  }
}

diff_A <- 1.96*abs(corrected_clean_dataset_A1 - corrected_clean_dataset_A2)
diff_B <- 1.96*abs(corrected_clean_dataset_B1 - corrected_clean_dataset_B2)
diff_C <- 1.96*abs(corrected_clean_dataset_C1 - corrected_clean_dataset_C2)

mean_A <- (corrected_clean_dataset_A1 + corrected_clean_dataset_A2)/2
mean_B <- (corrected_clean_dataset_B1 + corrected_clean_dataset_B2)/2
mean_C <- (corrected_clean_dataset_C1 + corrected_clean_dataset_C2)/2

## Prepare texture df
#previous usage of test
# texture1_datasetA1 <- corrected_clean_dataset_A1[seq(6),]
# texture2_datasetA1 <- corrected_clean_dataset_A1[seq(7,7+5),]
# texture3_datasetA1 <- corrected_clean_dataset_A1[seq(13,13+5),]
#
# texture1_datasetB1 <- corrected_clean_dataset_B1[seq(6),]
# texture2_datasetB1 <- corrected_clean_dataset_B1[seq(7,7+5),]
# texture3_datasetB1 <- corrected_clean_dataset_B1[seq(13,13+5),]
#
# texture1_datasetC1 <- corrected_clean_dataset_C1[seq(6),]
# texture2_datasetC1 <- corrected_clean_dataset_C1[seq(7,7+5),]
# texture3_datasetC1 <- corrected_clean_dataset_C1[seq(13,13+5),]

# usage of mean of test and retest
texture1_datasetA1 <- mean_A[seq(6),]
texture2_datasetA1 <- mean_A[seq(7,7+5),]
texture3_datasetA1 <- mean_A[seq(13,13+5),]

texture1_datasetB1 <- mean_B[seq(6),]
texture2_datasetB1 <- mean_B[seq(7,7+5),]
texture3_datasetB1 <- mean_B[seq(13,13+5),]

texture1_datasetC1 <- mean_C[seq(6),]
texture2_datasetC1 <- mean_C[seq(7,7+5),]
texture3_datasetC1 <- mean_C[seq(13,13+5),]
###

texture1_diff_A <- diff_A[seq(6),]
texture2_diff_A <- diff_A[seq(7,7+5),]
texture3_diff_A <- diff_A[seq(13,13+5),]

texture1_diff_B <- diff_B[seq(6),]
texture2_diff_B <- diff_B[seq(7,7+5),]
texture3_diff_B <- diff_B[seq(13,13+5),]

texture1_diff_C <- diff_C[seq(6),]
texture2_diff_C <- diff_C[seq(7,7+5),]
texture3_diff_C <- diff_C[seq(13,13+5),]

text1_vs_text2_A <- matrix(nrow = nrow(texture1_diff_A), ncol = ncol(texture1_diff_A))
text1_vs_text3_A <- matrix(nrow = nrow(texture1_diff_A), ncol = ncol(texture1_diff_A))
text3_vs_text2_A <- matrix(nrow = nrow(texture1_diff_A), ncol = ncol(texture1_diff_A))
for (ind_r in seq(nrow(texture1_diff_A))) {
  for (ind_c in seq(ncol(texture1_diff_A))) {
    max_T1_A <- texture1_datasetA1[ind_r, ind_c] + texture1_diff_A[ind_r, ind_c]
    max_T2_A <- texture2_datasetA1[ind_r, ind_c] + texture2_diff_A[ind_r, ind_c]
    max_T3_A <- texture3_datasetA1[ind_r, ind_c] + texture3_diff_A[ind_r, ind_c]

    min_T1_A <- texture1_datasetA1[ind_r, ind_c] - texture1_diff_A[ind_r, ind_c]
    min_T2_A <- texture2_datasetA1[ind_r, ind_c] - texture2_diff_A[ind_r, ind_c]
    min_T3_A <- texture3_datasetA1[ind_r, ind_c] - texture3_diff_A[ind_r, ind_c]

    if (max_T1_A < min_T2_A || max_T2_A < min_T1_A) {
      text1_vs_text2_A[ind_r, ind_c] <- 1
    } else {
      text1_vs_text2_A[ind_r, ind_c] <- 0
    }

    if (max_T1_A < min_T3_A || max_T3_A < min_T1_A) {
      text1_vs_text3_A[ind_r, ind_c] <- 1
    } else {
      text1_vs_text3_A[ind_r, ind_c] <- 0
    }

    if (max_T3_A < min_T2_A || max_T2_A < min_T3_A) {
      text3_vs_text2_A[ind_r, ind_c] <- 1
    } else {
      text3_vs_text2_A[ind_r, ind_c] <- 0
    }
  }
}
colnames(text1_vs_text2_A) <- colnames(texture1_datasetA1)
colnames(text1_vs_text3_A) <- colnames(texture1_datasetA1)
colnames(text3_vs_text2_A) <- colnames(texture1_datasetA1)

# Center B
text1_vs_text2_B <- matrix(nrow = nrow(texture1_diff_B), ncol = ncol(texture1_diff_B))
text1_vs_text3_B <- matrix(nrow = nrow(texture1_diff_B), ncol = ncol(texture1_diff_B))
text3_vs_text2_B <- matrix(nrow = nrow(texture1_diff_B), ncol = ncol(texture1_diff_B))
for (ind_r in seq(nrow(texture1_diff_B))) {
  for (ind_c in seq(ncol(texture1_diff_B))) {
    max_T1_B <- texture1_datasetB1[ind_r, ind_c] + texture1_diff_B[ind_r, ind_c]
    max_T2_B <- texture2_datasetB1[ind_r, ind_c] + texture2_diff_B[ind_r, ind_c]
    max_T3_B <- texture3_datasetB1[ind_r, ind_c] + texture3_diff_B[ind_r, ind_c]

    min_T1_B <- texture1_datasetB1[ind_r, ind_c] - texture1_diff_B[ind_r, ind_c]
    min_T2_B <- texture2_datasetB1[ind_r, ind_c] - texture2_diff_B[ind_r, ind_c]
    min_T3_B <- texture3_datasetB1[ind_r, ind_c] - texture3_diff_B[ind_r, ind_c]

    if (max_T1_B < min_T2_B || max_T2_B < min_T1_B) {
      text1_vs_text2_B[ind_r, ind_c] <- 1
    } else {
      text1_vs_text2_B[ind_r, ind_c] <- 0
    }

    if (max_T1_B < min_T3_B || max_T3_B < min_T1_B) {
      text1_vs_text3_B[ind_r, ind_c] <- 1
    } else {
      text1_vs_text3_B[ind_r, ind_c] <- 0
    }

    if (max_T3_B < min_T2_B || max_T2_B < min_T3_B) {
      text3_vs_text2_B[ind_r, ind_c] <- 1
    } else {
      text3_vs_text2_B[ind_r, ind_c] <- 0
    }
  }
}
colnames(text1_vs_text2_B) <- colnames(texture1_datasetB1)
colnames(text1_vs_text3_B) <- colnames(texture1_datasetB1)
colnames(text3_vs_text2_B) <- colnames(texture1_datasetB1)

# Center C
text1_vs_text2_C <- matrix(nrow = nrow(texture1_diff_C), ncol = ncol(texture1_diff_C))
text1_vs_text3_C <- matrix(nrow = nrow(texture1_diff_C), ncol = ncol(texture1_diff_C))
text3_vs_text2_C <- matrix(nrow = nrow(texture1_diff_C), ncol = ncol(texture1_diff_C))
for (ind_r in seq(nrow(texture1_diff_C))) {
  for (ind_c in seq(ncol(texture1_diff_C))) {
    max_T1_C <- texture1_datasetC1[ind_r, ind_c] + texture1_diff_C[ind_r, ind_c]
    max_T2_C <- texture2_datasetC1[ind_r, ind_c] + texture2_diff_C[ind_r, ind_c]
    max_T3_C <- texture3_datasetC1[ind_r, ind_c] + texture3_diff_C[ind_r, ind_c]

    min_T1_C <- texture1_datasetC1[ind_r, ind_c] - texture1_diff_C[ind_r, ind_c]
    min_T2_C <- texture2_datasetC1[ind_r, ind_c] - texture2_diff_C[ind_r, ind_c]
    min_T3_C <- texture3_datasetC1[ind_r, ind_c] - texture3_diff_C[ind_r, ind_c]

    if (max_T1_C < min_T2_C || max_T2_C < min_T1_C) {
      text1_vs_text2_C[ind_r, ind_c] <- 1
    } else {
      text1_vs_text2_C[ind_r, ind_c] <- 0
    }

    if (max_T1_C < min_T3_C || max_T3_C < min_T1_C) {
      text1_vs_text3_C[ind_r, ind_c] <- 1
    } else {
      text1_vs_text3_C[ind_r, ind_c] <- 0
    }

    if (max_T3_C < min_T2_C || max_T2_C < min_T3_C) {
      text3_vs_text2_C[ind_r, ind_c] <- 1
    } else {
      text3_vs_text2_C[ind_r, ind_c] <- 0
    }
  }
}
colnames(text1_vs_text2_C) <- colnames(texture1_datasetC1)
colnames(text1_vs_text3_C) <- colnames(texture1_datasetC1)
colnames(text3_vs_text2_C) <- colnames(texture1_datasetC1)

#library('plot.matrix')

#original_text1_vs_text2_A <- text1_vs_text2_A[, grep('original', colnames(text1_vs_text2_A))]
#trasp_original_text1_vs_text2_A <- t(original_text1_vs_text2_A)
volumes <- c('29.8','20.7','13.3','7.4','3.3','0.8')
#plot(trasp_original_text1_vs_text2_A, breaks=c(0,0.5,1), col=c('red', 'green'), key=NULL, las=1, xlab=parse(text='Volume (cm^3)'), ylab="Radiomic feature", main="scanner A, texture 1 vs texture 2, original", cex.lab = 1)
#axis(1, at=1:6, ann=FALSE)
#axis(1, at=1:6, labels=volumes)

#par(mar=c(5.1, 4.1, 4.1, 4.1))
#dev.new(width=10,height=10,noRStudioGD = TRUE)
#asp=TRUE (to get squared boxes)

#plot(text1_vs_text2_A[, grep('original', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('log.sigma', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('wavelet.LL', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('wavelet.LH', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('wavelet.HL', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('wavelet.HH', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('square_', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('squareroot', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('exponential', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))
#plot(text1_vs_text2_A[, grep('logarithm', colnames(text1_vs_text2_A))], breaks=c(0,0.5,1), col=c('red', 'green'))

## features A
filterType_repeatable_feats_A <- list()
for (filterType in c("original_", "log.sigma.6.mm.3D_", "wavelet.LH_", "wavelet.HL_",
                    "wavelet.HH_", "wavelet.LL_", "square_", "squareroot_", "logarithm_",
                    "exponential_")) {
  filterType_repeatable_feats_A[[filterType]] <- gsub(filterType, "", colnames(dataset_A[, !grepl('original_shape', colnames(dataset_A))])[ grep(filterType, colnames(dataset_A[, !grepl('original_shape', colnames(dataset_A))]))])
}

highly_repeatable_features_by_filter_A <- unique(unlist(filterType_repeatable_feats_A))
# table(unlist(filterType_repeatable_feats_A))
counts_filters_A <- table(unlist(filterType_repeatable_feats_A))

ylabels_plot_max_A <- vector()
for (i in seq(length(counts_filters_A))) {
  ylabels_plot_max_A <- c(ylabels_plot_max_A, paste0(names(counts_filters_A[i]), ' (', counts_filters_A[i], ')'))
}


## features B
filterType_repeatable_feats_B <- list()
for (filterType in c("original_", "log.sigma.6.mm.3D_", "wavelet.LH_", "wavelet.HL_",
                     "wavelet.HH_", "wavelet.LL_", "square_", "squareroot_", "logarithm_",
                     "exponential_")) {
  filterType_repeatable_feats_B[[filterType]] <- gsub(filterType, "", colnames(dataset_B[, !grepl('original_shape', colnames(dataset_B))])[ grep(filterType, colnames(dataset_B[, !grepl('original_shape', colnames(dataset_B))]))])
}

highly_repeatable_features_by_filter_B <- unique(unlist(filterType_repeatable_feats_B))
# table(unlist(filterType_repeatable_feats_A))
counts_filters_B <- table(unlist(filterType_repeatable_feats_B))

ylabels_plot_max_B <- vector()
for (i in seq(length(counts_filters_B))) {
  ylabels_plot_max_B <- c(ylabels_plot_max_B, paste0(names(counts_filters_B[i]), ' (', counts_filters_B[i], ')'))
}


## features C
filterType_repeatable_feats_C <- list()
for (filterType in c("original_", "log.sigma.6.mm.3D_", "wavelet.LH_", "wavelet.HL_",
                     "wavelet.HH_", "wavelet.LL_", "square_", "squareroot_", "logarithm_",
                     "exponential_")) {
  filterType_repeatable_feats_C[[filterType]] <- gsub(filterType, "", colnames(dataset_C[, !grepl('original_shape', colnames(dataset_C))])[ grep(filterType, colnames(dataset_C[, !grepl('original_shape', colnames(dataset_C))]))])
}

highly_repeatable_features_by_filter_C <- unique(unlist(filterType_repeatable_feats_C))
# table(unlist(filterType_repeatable_feats_A))
counts_filters_C <- table(unlist(filterType_repeatable_feats_C))

ylabels_plot_max_C <- vector()
for (i in seq(length(counts_filters_C))) {
  ylabels_plot_max_C <- c(ylabels_plot_max_C, paste0(names(counts_filters_C[i]), ' (', counts_filters_C[i], ')'))
}

###################
#scanner A - texture 1 vs 2
###################

count_per_feature_A_1vs2 <- matrix(data = NA, nrow = length(counts_filters_A), ncol = nrow(text1_vs_text2_A))
rownames(count_per_feature_A_1vs2) <- names(counts_filters_A)
for (i in names(counts_filters_A)) {
  for (j in seq(nrow(text1_vs_text2_A))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_A_1vs2[i, j] <- sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))])#/counts_filters_A[i]*100
  }
}

count_per_feature_A_1vs2['glszm_SizeZoneNonUniformity',] <- count_per_feature_A_1vs2['glszm_SizeZoneNonUniformity',] - count_per_feature_A_1vs2['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_A_1vs2['glcm_Idm',] <- count_per_feature_A_1vs2['glcm_Idm',] - count_per_feature_A_1vs2['glcm_Idmn',]
count_per_feature_A_1vs2['glcm_Id',] <- count_per_feature_A_1vs2['glcm_Id',] - count_per_feature_A_1vs2['glcm_Idm',] - count_per_feature_A_1vs2['glcm_Idmn',] - count_per_feature_A_1vs2['glcm_Idn',]
count_per_feature_A_1vs2['firstorder_Mean',] <- count_per_feature_A_1vs2['firstorder_Mean',] - count_per_feature_A_1vs2['firstorder_MeanAbsoluteDeviation',]

count_per_feature_A_1vs2_total <- count_per_feature_A_1vs2
for (i in names(counts_filters_A)) {
  count_per_feature_A_1vs2[i, ] <- count_per_feature_A_1vs2[i, ]/counts_filters_A[i]*100
}

rownames(count_per_feature_A_1vs2) <- ylabels_plot_max_A
colnames(count_per_feature_A_1vs2) <- volumes
rownames(count_per_feature_A_1vs2_total) <- ylabels_plot_max_A
colnames(count_per_feature_A_1vs2_total) <- volumes
radius <-sqrt(c(29.8, 20.7, 13.3, 7.4, 3.3, 0.8)/pi)

df_A <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_1vs2))#, 
df_A$Radius <- sqrt(as.numeric(df_A$Volume)/pi) 
df_A$Volume <- factor(df_A$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))

gg <- ggplot(df_A, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A$Feature))))) +
  ggtitle("scanner A - texture 1 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"scanner A - texture 1 vs 2.pdf",sep = ''), width = 10, height = 20, units = "in")

#df_A_firstorder <- df_A[grepl('firstorder', df_A$Feature),]
#df_A_firstorder$Volume <- factor(df_A_firstorder$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
#gg <- ggplot(df_A_firstorder, aes(x=Count, y=Feature)) +
 # geom_point(aes(col=Volume, shape=Volume)) +
#  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))   +
  #scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A_firstorder$Feature))))) +
  #ggtitle("scanner A - texture 1 vs 2 - first order features")

#plot(gg)


#df_A_glcm <- df_A[grepl('glcm', df_A$Feature),]
#df_A_glcm$Volume <- factor(df_A_glcm$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
#gg <- ggplot(df_A_glcm, aes(x=Count, y=Feature)) +
 # geom_point(aes(col=Volume, shape=Volume)) +
#  scale_x_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10))   +
#  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A_glcm$Feature))))) +
 # ggtitle("scanner A - texture 1 vs 2 - GLCM features")

#plot(gg)


###################
#scanner A - texture 1 vs 3
###################

count_per_feature_A_1vs3 <- matrix(data = NA, nrow = length(counts_filters_A), ncol = nrow(text1_vs_text3_A))
rownames(count_per_feature_A_1vs3) <- names(counts_filters_A)
for (i in names(counts_filters_A)) {
  for (j in seq(nrow(text1_vs_text3_A))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_A_1vs3[i, j] <- sum(text1_vs_text3_A[j, grepl(i, colnames(text1_vs_text3_A))])
  }
}

count_per_feature_A_1vs3['glszm_SizeZoneNonUniformity',] <- count_per_feature_A_1vs3['glszm_SizeZoneNonUniformity',] - count_per_feature_A_1vs3['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_A_1vs3['glcm_Idm',] <- count_per_feature_A_1vs3['glcm_Idm',] - count_per_feature_A_1vs3['glcm_Idmn',]
count_per_feature_A_1vs3['glcm_Id',] <- count_per_feature_A_1vs3['glcm_Id',] - count_per_feature_A_1vs3['glcm_Idm',] - count_per_feature_A_1vs3['glcm_Idmn',] - count_per_feature_A_1vs3['glcm_Idn',]
count_per_feature_A_1vs3['firstorder_Mean',] <- count_per_feature_A_1vs3['firstorder_Mean',] - count_per_feature_A_1vs3['firstorder_MeanAbsoluteDeviation',]

count_per_feature_A_1vs3_total <- count_per_feature_A_1vs3
for (i in names(counts_filters_A)) {
  count_per_feature_A_1vs3[i, ] <- count_per_feature_A_1vs3[i, ]/counts_filters_A[i]*100
}

rownames(count_per_feature_A_1vs3) <- ylabels_plot_max_A
colnames(count_per_feature_A_1vs3) <- volumes
rownames(count_per_feature_A_1vs3_total) <- ylabels_plot_max_A
colnames(count_per_feature_A_1vs3_total) <- volumes

df_A <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_1vs3))
df_A$Volume <- factor(df_A$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_A, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A$Feature))))) +
  ggtitle("scanner A - texture 1 vs 3")

plot(gg)
ggsave(paste(dir_save_plots,"scanner A - texture 1 vs 3.pdf",sep = ''), width = 10, height = 20, units = "in")


###################
#scanner A - texture 3 vs 2
###################

count_per_feature_A_3vs2 <- matrix(data = NA, nrow = length(counts_filters_A), ncol = nrow(text3_vs_text2_A))
rownames(count_per_feature_A_3vs2) <- names(counts_filters_A)
for (i in names(counts_filters_A)) {
  for (j in seq(nrow(text3_vs_text2_A))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_A_3vs2[i, j] <- sum(text3_vs_text2_A[j, grepl(i, colnames(text3_vs_text2_A))])
  }
}

count_per_feature_A_3vs2['glszm_SizeZoneNonUniformity',] <- count_per_feature_A_3vs2['glszm_SizeZoneNonUniformity',] - count_per_feature_A_3vs2['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_A_3vs2['glcm_Idm',] <- count_per_feature_A_3vs2['glcm_Idm',] - count_per_feature_A_3vs2['glcm_Idmn',]
count_per_feature_A_3vs2['glcm_Id',] <- count_per_feature_A_3vs2['glcm_Id',] - count_per_feature_A_3vs2['glcm_Idm',] - count_per_feature_A_3vs2['glcm_Idmn',] - count_per_feature_A_3vs2['glcm_Idn',]
count_per_feature_A_3vs2['firstorder_Mean',] <- count_per_feature_A_3vs2['firstorder_Mean',] - count_per_feature_A_3vs2['firstorder_MeanAbsoluteDeviation',]

count_per_feature_A_3vs2_total <- count_per_feature_A_3vs2
for (i in names(counts_filters_A)) {
  count_per_feature_A_3vs2[i, ] <- count_per_feature_A_3vs2[i, ]/counts_filters_A[i]*100
}

rownames(count_per_feature_A_3vs2) <- ylabels_plot_max_A
colnames(count_per_feature_A_3vs2) <- volumes
rownames(count_per_feature_A_3vs2_total) <- ylabels_plot_max_A
colnames(count_per_feature_A_3vs2_total) <- volumes

df_A <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_3vs2))
df_A$Volume <- factor(df_A$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_A, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A$Feature))))) +
  ggtitle("scanner A - texture 3 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"scanner A - texture 3 vs 2.pdf",sep = ''), width = 10, height = 20, units = "in")

###################
#scanner B - texture 1 vs 2
###################

count_per_feature_B_1vs2 <- matrix(data = NA, nrow = length(counts_filters_B), ncol = nrow(text1_vs_text2_B))
rownames(count_per_feature_B_1vs2) <- names(counts_filters_B)
for (i in names(counts_filters_B)) {
  for (j in seq(nrow(text1_vs_text2_B))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_B_1vs2[i, j] <- sum(text1_vs_text2_B[j, grepl(i, colnames(text1_vs_text2_B))])
  }
}

count_per_feature_B_1vs2['glszm_SizeZoneNonUniformity',] <- count_per_feature_B_1vs2['glszm_SizeZoneNonUniformity',] - count_per_feature_B_1vs2['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_B_1vs2['glcm_Idm',] <- count_per_feature_B_1vs2['glcm_Idm',] - count_per_feature_B_1vs2['glcm_Idmn',]
count_per_feature_B_1vs2['glcm_Id',] <- count_per_feature_B_1vs2['glcm_Id',] - count_per_feature_B_1vs2['glcm_Idm',] - count_per_feature_B_1vs2['glcm_Idmn',] - count_per_feature_B_1vs2['glcm_Idn',]
count_per_feature_B_1vs2['firstorder_Mean',] <- count_per_feature_B_1vs2['firstorder_Mean',] - count_per_feature_B_1vs2['firstorder_MeanAbsoluteDeviation',]

count_per_feature_B_1vs2_total <- count_per_feature_B_1vs2
for (i in names(counts_filters_B)) {
  count_per_feature_B_1vs2[i, ] <- count_per_feature_B_1vs2[i, ]/counts_filters_B[i]*100
}

rownames(count_per_feature_B_1vs2) <- ylabels_plot_max_B
colnames(count_per_feature_B_1vs2) <- volumes
rownames(count_per_feature_B_1vs2_total) <- ylabels_plot_max_B
colnames(count_per_feature_B_1vs2_total) <- volumes

df_B <- data.frame(Feature=rep(ylabels_plot_max_B,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_B)), rep(volumes[2],length(ylabels_plot_max_B)), rep(volumes[3],length(ylabels_plot_max_B)), rep(volumes[4],length(ylabels_plot_max_B)), rep(volumes[5],length(ylabels_plot_max_B)), rep(volumes[6],length(ylabels_plot_max_B))), Percentage=as.vector(count_per_feature_B_1vs2))
df_B$Volume <- factor(df_B$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_B, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_B$Feature))))) +
  ggtitle("scanner B - texture 1 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"scanner B - texture 1 vs 2.pdf",sep = ''), width = 10, height = 20, units = "in")


###################
#scanner B - texture 1 vs 3
###################

count_per_feature_B_1vs3 <- matrix(data = NA, nrow = length(counts_filters_B), ncol = nrow(text1_vs_text3_B))
rownames(count_per_feature_B_1vs3) <- names(counts_filters_B)
for (i in names(counts_filters_B)) {
  for (j in seq(nrow(text1_vs_text3_B))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_B_1vs3[i, j] <- sum(text1_vs_text3_B[j, grepl(i, colnames(text1_vs_text3_B))])
  }
}

count_per_feature_B_1vs3['glszm_SizeZoneNonUniformity',] <- count_per_feature_B_1vs3['glszm_SizeZoneNonUniformity',] - count_per_feature_B_1vs3['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_B_1vs3['glcm_Idm',] <- count_per_feature_B_1vs3['glcm_Idm',] - count_per_feature_B_1vs3['glcm_Idmn',]
count_per_feature_B_1vs3['glcm_Id',] <- count_per_feature_B_1vs3['glcm_Id',] - count_per_feature_B_1vs3['glcm_Idm',] - count_per_feature_B_1vs3['glcm_Idmn',] - count_per_feature_B_1vs3['glcm_Idn',]
count_per_feature_B_1vs3['firstorder_Mean',] <- count_per_feature_B_1vs3['firstorder_Mean',] - count_per_feature_B_1vs3['firstorder_MeanAbsoluteDeviation',]

count_per_feature_B_1vs3_total <- count_per_feature_B_1vs3
for (i in names(counts_filters_B)) {
  count_per_feature_B_1vs3[i, ] <- count_per_feature_B_1vs3[i, ]/counts_filters_B[i]*100
}

rownames(count_per_feature_B_1vs3) <- ylabels_plot_max_B
colnames(count_per_feature_B_1vs3) <- volumes
rownames(count_per_feature_B_1vs3_total) <- ylabels_plot_max_B
colnames(count_per_feature_B_1vs3_total) <- volumes

df_B <- data.frame(Feature=rep(ylabels_plot_max_B,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_B)), rep(volumes[2],length(ylabels_plot_max_B)), rep(volumes[3],length(ylabels_plot_max_B)), rep(volumes[4],length(ylabels_plot_max_B)), rep(volumes[5],length(ylabels_plot_max_B)), rep(volumes[6],length(ylabels_plot_max_B))), Percentage=as.vector(count_per_feature_B_1vs3))
df_B$Volume <- factor(df_B$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_B, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_B$Feature))))) +
  ggtitle("scanner B - texture 1 vs 3")

plot(gg)
ggsave(paste(dir_save_plots,"scanner B - texture 1 vs 3.pdf",sep = ''), width = 10, height = 20, units = "in")

###################
#scanner B - texture 3 vs 2
###################

count_per_feature_B_3vs2 <- matrix(data = NA, nrow = length(counts_filters_B), ncol = nrow(text3_vs_text2_B))
rownames(count_per_feature_B_3vs2) <- names(counts_filters_B)
for (i in names(counts_filters_B)) {
  for (j in seq(nrow(text3_vs_text2_B))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_B_3vs2[i, j] <- sum(text3_vs_text2_B[j, grepl(i, colnames(text3_vs_text2_B))])
  }
}

count_per_feature_B_3vs2['glszm_SizeZoneNonUniformity',] <- count_per_feature_B_3vs2['glszm_SizeZoneNonUniformity',] - count_per_feature_B_3vs2['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_B_3vs2['glcm_Idm',] <- count_per_feature_B_3vs2['glcm_Idm',] - count_per_feature_B_3vs2['glcm_Idmn',]
count_per_feature_B_3vs2['glcm_Id',] <- count_per_feature_B_3vs2['glcm_Id',] - count_per_feature_B_3vs2['glcm_Idm',] - count_per_feature_B_3vs2['glcm_Idmn',] - count_per_feature_B_3vs2['glcm_Idn',]
count_per_feature_B_3vs2['firstorder_Mean',] <- count_per_feature_B_3vs2['firstorder_Mean',] - count_per_feature_B_3vs2['firstorder_MeanAbsoluteDeviation',]

count_per_feature_B_3vs2_total <- count_per_feature_B_3vs2
for (i in names(counts_filters_B)) {
  count_per_feature_B_3vs2[i, ] <- count_per_feature_B_3vs2[i, ]/counts_filters_B[i]*100
}

rownames(count_per_feature_B_3vs2) <- ylabels_plot_max_B
colnames(count_per_feature_B_3vs2) <- volumes
rownames(count_per_feature_B_3vs2_total) <- ylabels_plot_max_B
colnames(count_per_feature_B_3vs2_total) <- volumes

df_B <- data.frame(Feature=rep(ylabels_plot_max_B,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_B)), rep(volumes[2],length(ylabels_plot_max_B)), rep(volumes[3],length(ylabels_plot_max_B)), rep(volumes[4],length(ylabels_plot_max_B)), rep(volumes[5],length(ylabels_plot_max_B)), rep(volumes[6],length(ylabels_plot_max_B))), Percentage=as.vector(count_per_feature_B_3vs2))
df_B$Volume <- factor(df_B$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_B, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_B$Feature))))) +
  ggtitle("scanner B - texture 3 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"scanner B - texture 3 vs 2.pdf",sep = ''), width = 10, height = 20, units = "in")

###################
#scanner C - texture 1 vs 2
###################

count_per_feature_C_1vs2 <- matrix(data = NA, nrow = length(counts_filters_C), ncol = nrow(text1_vs_text2_C))
rownames(count_per_feature_C_1vs2) <- names(counts_filters_C)
for (i in names(counts_filters_C)) {
  for (j in seq(nrow(text1_vs_text2_C))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_C_1vs2[i, j] <- sum(text1_vs_text2_C[j, grepl(i, colnames(text1_vs_text2_C))])
  }
}

count_per_feature_C_1vs2['glszm_SizeZoneNonUniformity',] <- count_per_feature_C_1vs2['glszm_SizeZoneNonUniformity',] - count_per_feature_C_1vs2['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_C_1vs2['glcm_Idm',] <- count_per_feature_C_1vs2['glcm_Idm',] - count_per_feature_C_1vs2['glcm_Idmn',]
count_per_feature_C_1vs2['glcm_Id',] <- count_per_feature_C_1vs2['glcm_Id',] - count_per_feature_C_1vs2['glcm_Idm',] - count_per_feature_C_1vs2['glcm_Idmn',] - count_per_feature_C_1vs2['glcm_Idn',]
count_per_feature_C_1vs2['firstorder_Mean',] <- count_per_feature_C_1vs2['firstorder_Mean',] - count_per_feature_C_1vs2['firstorder_MeanAbsoluteDeviation',]

count_per_feature_C_1vs2_total <- count_per_feature_C_1vs2
for (i in names(counts_filters_C)) {
  count_per_feature_C_1vs2[i, ] <- count_per_feature_C_1vs2[i, ]/counts_filters_C[i]*100
}

rownames(count_per_feature_C_1vs2) <- ylabels_plot_max_C
colnames(count_per_feature_C_1vs2) <- volumes
rownames(count_per_feature_C_1vs2_total) <- ylabels_plot_max_C
colnames(count_per_feature_C_1vs2_total) <- volumes

df_C <- data.frame(Feature=rep(ylabels_plot_max_C,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_C)), rep(volumes[2],length(ylabels_plot_max_C)), rep(volumes[3],length(ylabels_plot_max_C)), rep(volumes[4],length(ylabels_plot_max_C)), rep(volumes[5],length(ylabels_plot_max_C)), rep(volumes[6],length(ylabels_plot_max_C))), Percentage=as.vector(count_per_feature_C_1vs2))
df_C$Volume <- factor(df_C$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_C, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_C$Feature))))) +
  ggtitle("scanner C - texture 1 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"scanner C - texture 1 vs 2.pdf",sep = ''), width = 10, height = 20, units = "in")

###################
#scanner C - texture 1 vs 3
###################

count_per_feature_C_1vs3 <- matrix(data = NA, nrow = length(counts_filters_C), ncol = nrow(text1_vs_text3_C))
rownames(count_per_feature_C_1vs3) <- names(counts_filters_C)
for (i in names(counts_filters_C)) {
  for (j in seq(nrow(text1_vs_text3_C))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_C_1vs3[i, j] <- sum(text1_vs_text3_C[j, grepl(i, colnames(text1_vs_text3_C))])
  }
}

count_per_feature_C_1vs3['glszm_SizeZoneNonUniformity',] <- count_per_feature_C_1vs3['glszm_SizeZoneNonUniformity',] - count_per_feature_C_1vs3['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_C_1vs3['glcm_Idm',] <- count_per_feature_C_1vs3['glcm_Idm',] - count_per_feature_C_1vs3['glcm_Idmn',]
count_per_feature_C_1vs3['glcm_Id',] <- count_per_feature_C_1vs3['glcm_Id',] - count_per_feature_C_1vs3['glcm_Idm',] - count_per_feature_C_1vs3['glcm_Idmn',] - count_per_feature_C_1vs3['glcm_Idn',]
count_per_feature_C_1vs3['firstorder_Mean',] <- count_per_feature_C_1vs3['firstorder_Mean',] - count_per_feature_C_1vs3['firstorder_MeanAbsoluteDeviation',]

count_per_feature_C_1vs3_total <- count_per_feature_C_1vs3
for (i in names(counts_filters_C)) {
  count_per_feature_C_1vs3[i, ] <- count_per_feature_C_1vs3[i, ]/counts_filters_C[i]*100
}

rownames(count_per_feature_C_1vs3) <- ylabels_plot_max_C
colnames(count_per_feature_C_1vs3) <- volumes
rownames(count_per_feature_C_1vs3_total) <- ylabels_plot_max_C
colnames(count_per_feature_C_1vs3_total) <- volumes

df_C <- data.frame(Feature=rep(ylabels_plot_max_C,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_C)), rep(volumes[2],length(ylabels_plot_max_C)), rep(volumes[3],length(ylabels_plot_max_C)), rep(volumes[4],length(ylabels_plot_max_C)), rep(volumes[5],length(ylabels_plot_max_C)), rep(volumes[6],length(ylabels_plot_max_C))), Percentage=as.vector(count_per_feature_C_1vs3))
df_C$Volume <- factor(df_C$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_C, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_C$Feature))))) +
  ggtitle("scanner C - texture 1 vs 3")

plot(gg)
ggsave(paste(dir_save_plots,"scanner C - texture 1 vs 3.pdf",sep = ''), width = 10, height = 20, units = "in")

###################
#scanner C - texture 3 vs 2
###################

count_per_feature_C_3vs2 <- matrix(data = NA, nrow = length(counts_filters_C), ncol = nrow(text3_vs_text2_C))
rownames(count_per_feature_C_3vs2) <- names(counts_filters_C)
for (i in names(counts_filters_C)) {
  for (j in seq(nrow(text3_vs_text2_C))) {
    # print(sum(text1_vs_text2_A[j, grepl(i, colnames(text1_vs_text2_A))]))
    count_per_feature_C_3vs2[i, j] <- sum(text3_vs_text2_C[j, grepl(i, colnames(text3_vs_text2_C))])
  }
}

count_per_feature_C_3vs2['glszm_SizeZoneNonUniformity',] <- count_per_feature_C_3vs2['glszm_SizeZoneNonUniformity',] - count_per_feature_C_3vs2['glszm_SizeZoneNonUniformityNormalized',]
count_per_feature_C_3vs2['glcm_Idm',] <- count_per_feature_C_3vs2['glcm_Idm',] - count_per_feature_C_3vs2['glcm_Idmn',]
count_per_feature_C_3vs2['glcm_Id',] <- count_per_feature_C_3vs2['glcm_Id',] - count_per_feature_C_3vs2['glcm_Idm',] - count_per_feature_C_3vs2['glcm_Idmn',] - count_per_feature_C_3vs2['glcm_Idn',]
count_per_feature_C_3vs2['firstorder_Mean',] <- count_per_feature_C_3vs2['firstorder_Mean',] - count_per_feature_C_3vs2['firstorder_MeanAbsoluteDeviation',]

count_per_feature_C_3vs2_total <- count_per_feature_C_3vs2
for (i in names(counts_filters_C)) {
  count_per_feature_C_3vs2[i, ] <- count_per_feature_C_3vs2[i, ]/counts_filters_C[i]*100
}

rownames(count_per_feature_C_3vs2) <- ylabels_plot_max_C
colnames(count_per_feature_C_3vs2) <- volumes
rownames(count_per_feature_C_3vs2_total) <- ylabels_plot_max_C
colnames(count_per_feature_C_3vs2_total) <- volumes

df_C <- data.frame(Feature=rep(ylabels_plot_max_C,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_C)), rep(volumes[2],length(ylabels_plot_max_C)), rep(volumes[3],length(ylabels_plot_max_C)), rep(volumes[4],length(ylabels_plot_max_C)), rep(volumes[5],length(ylabels_plot_max_C)), rep(volumes[6],length(ylabels_plot_max_C))), Percentage=as.vector(count_per_feature_C_3vs2))
df_C$Volume <- factor(df_C$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_C, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_C$Feature))))) +
  ggtitle("scanner C - texture 3 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"scanner C - texture 3 vs 2.pdf",sep = ''), width = 10, height = 20, units = "in")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

n = 1
cols = gg_color_hue(n)

# volume size vs discriminability
volume_sizes <- volumes
discriminability <- text1_vs_text2_A[, 'original_firstorder_Skewness']
discriminability[discriminability == 0] <- 'NO'
discriminability[discriminability == 1] <- 'YES'
discriminability_vs_size_df <- data.frame(Discriminative = discriminability, Volume = volume_sizes)
discriminability_vs_size_df$Volume <- factor(discriminability_vs_size_df$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(discriminability_vs_size_df, aes(x=Discriminative, y=Volume)) +
  geom_point(color=hue_pal()(1)) +
  ggtitle("scanner A - original_firstorder_Skewness")
plot(gg)
ggsave(paste(dir_save_plots,"disc_vs_size_scanner A - original_firstorder_Skewness.pdf",sep = ''), width = 9, height = 2.5, units = "in")

# discrimination is features dependendent
df_A <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_1vs2))
df_A$Volume <- factor(df_A$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(df_A[df_A$Feature %in% c("glcm_DifferenceVariance (10)","glcm_MCC (8)"),], aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A[df_A$Feature %in% c("glcm_DifferenceVariance (10)","glcm_MCC (8)"),]$Feature))))) +
  ggtitle("scanner A - texture 1 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"disc_vs_features_scanners A - texture 1 vs 2.pdf",sep = ''), width = 9, height = 2.5, units = "in")


# discrimination is field strenght dependendent
df_A <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_1vs2))
df_A$Volume <- factor(df_A$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
df_C <- data.frame(Feature=rep(ylabels_plot_max_C,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_C)), rep(volumes[2],length(ylabels_plot_max_C)), rep(volumes[3],length(ylabels_plot_max_C)), rep(volumes[4],length(ylabels_plot_max_C)), rep(volumes[5],length(ylabels_plot_max_C)), rep(volumes[6],length(ylabels_plot_max_C))), Percentage=as.vector(count_per_feature_C_1vs2))
df_C$Volume <- factor(df_C$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
selected_df_A <- df_A[df_A$Feature %in% c("ngtdm_Coarseness (10)"),]
selected_df_A$Feature <- paste("Scanner_A_", selected_df_A$Feature, sep='')
selected_df_C <- df_C[df_C$Feature %in% c("ngtdm_Coarseness (9)"),]
selected_df_C$Feature <- paste("Scanner_C_", selected_df_C$Feature, sep='')
df_A_C <- rbind(selected_df_A, selected_df_C)
# df_A_C$Feature[seq(1,6)] <- paste("Scanner_A_", df_A_C$Feature[seq(1,6)])
# df_A_C$Feature[seq(7,12)] <- paste("Scanner_C_", df_A_C$Feature[seq(7,12)])
gg <- ggplot(df_A_C, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A_C$Feature))))) +
  ggtitle("texture 1 vs 2 - ngtdm_Coarseness")

plot(gg)
ggsave(paste(dir_save_plots,"disc_vs_field_scanners A vs C - texture 1 vs 2 - ngtdm_Coarseness.pdf",sep = ''), width = 9, height = 2.5, units = "in")

# discrimination is texture dependendent
df_A_1_2 <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_1vs2))
df_A_1_2$Volume <- factor(df_A_1_2$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
df_A_1_3 <- data.frame(Feature=rep(ylabels_plot_max_A,6), Volume=c(rep(volumes[1],length(ylabels_plot_max_A)), rep(volumes[2],length(ylabels_plot_max_A)), rep(volumes[3],length(ylabels_plot_max_A)), rep(volumes[4],length(ylabels_plot_max_A)), rep(volumes[5],length(ylabels_plot_max_A)), rep(volumes[6],length(ylabels_plot_max_A))), Percentage=as.vector(count_per_feature_A_1vs3))
df_A_1_3$Volume <- factor(df_A_1_3$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
selected_df_A_1_2 <- df_A_1_2[df_A_1_2$Feature %in% c("glcm_DifferenceVariance (10)","glrlm_LongRunHighGrayLevelEmphasis (10)"),]
selected_df_A_1_2$Feature <- paste("Texture_1_vs_2_", selected_df_A_1_2$Feature, sep='')
selected_df_A_1_3 <- df_A_1_3[df_A_1_3$Feature %in% c("glcm_DifferenceVariance (10)","glrlm_LongRunHighGrayLevelEmphasis (10)"),]
selected_df_A_1_3$Feature <- paste("Texture_1_vs_3_", selected_df_A_1_3$Feature, sep='')
df_A_textures <- rbind(selected_df_A_1_2, selected_df_A_1_3)
# df_A_C$Feature[seq(1,6)] <- paste("Scanner_A_", df_A_C$Feature[seq(1,6)])
# df_A_C$Feature[seq(7,12)] <- paste("Scanner_C_", df_A_C$Feature[seq(7,12)])
gg <- ggplot(df_A_textures, aes(x=Percentage, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(df_A_textures$Feature))))) +
  ggtitle("texture 1 vs 2 - texture 1 vs 3")

plot(gg)
ggsave(paste(dir_save_plots,"disc_vs_texture_scanner A - texture 1 vs 2 - texture 1 vs 3.pdf",sep = ''), width = 9, height = 3, units = "in")


# discriminability as function filter
volume_sizes <- volumes
discriminability <- text1_vs_text2_A[, grepl('firstorder_Skewness', colnames(text1_vs_text2_A))]
discriminability[discriminability == 0] <- 'NO'
discriminability[discriminability == 1] <- 'YES'
discriminability_vec <- as.vector(discriminability)
feature_names <-c()
for (feature_indx in seq(1,length(colnames(discriminability)))){
  feature_names <-c(feature_names, rep(colnames(discriminability)[feature_indx],6))
}
# feature_names <- c(rep(colnames(discriminability)[1],6), rep(colnames(discriminability)[2],6), rep(colnames(discriminability)[3],6),
#                    rep(colnames(discriminability)[4],6), rep(colnames(discriminability)[5],6), rep(colnames(discriminability)[6],6),
#                    rep(colnames(discriminability)[7],6), rep(colnames(discriminability)[8],6), rep(colnames(discriminability)[9],6),
#                    rep(colnames(discriminability)[10],6))
discriminability_vs_filter_df <- data.frame(Discriminative = discriminability_vec, Feature=feature_names, Volume = rep(volume_sizes, length(colnames(discriminability))))
discriminability_vs_filter_df$Volume <- factor(discriminability_vs_size_df$Volume, levels = rev(c(volumes[1], volumes[2] , volumes[3] , volumes[4], volumes[5], volumes[6])))
gg <- ggplot(discriminability_vs_filter_df, aes(x=Discriminative, y=Feature)) +
  geom_point(shape=1, aes(col=Volume, size=Volume)) +
  # scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))   +
  scale_y_discrete(limits = rev(levels(as.factor(as.character(discriminability_vs_filter_df$Feature))))) +
  ggtitle("scanner C - texture 3 vs 2")

plot(gg)
ggsave(paste(dir_save_plots,"disc_vs_texture_scanner C - texture 2 vs 3.pdf",sep = ''), width = 9, height = 3, units = "in")

# dataset_A1[, 'original_firstorder_skewness']


######################################################################
#### New Plots
######################################################################
# ## features A
vector_feature_class_max_A <- vector(,6)
names(vector_feature_class_max_A) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")
for (features_class_index in seq(length(counts_filters_A))) {
  if (grepl( "firstorder", names(counts_filters_A)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_A["firstorder"] <- vector_feature_class_max_A["firstorder"] + counts_filters_A[features_class_index]
  } else if (grepl( "glcm", names(counts_filters_A)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_A["glcm"] <- vector_feature_class_max_A["glcm"] + counts_filters_A[features_class_index]
  } else if (grepl( "gldm", names(counts_filters_A)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_A["gldm"] <- vector_feature_class_max_A["gldm"] + counts_filters_A[features_class_index]
  } else if (grepl( "glrlm", names(counts_filters_A)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_A["glrlm"] <- vector_feature_class_max_A["glrlm"] + counts_filters_A[features_class_index]
  } else if (grepl( "glszm", names(counts_filters_A)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_A["glszm"] <- vector_feature_class_max_A["glszm"] + counts_filters_A[features_class_index]
  } else if (grepl( "ngtdm", names(counts_filters_A)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_A["ngtdm"] <- vector_feature_class_max_A["ngtdm"] + counts_filters_A[features_class_index]
  }
}


features_A <- colnames(dataset_A[, !grepl('original_shape', colnames(dataset_A))])[-c(1)]
vector_filter_class_max_A <- vector(,10)
names(vector_filter_class_max_A) <- c("original", "exponential", "logarithm", "square", "squareroot", "wavelet.LL", "wavelet.HH", "wavelet.HL", "wavelet.LH", "log.sigma.")
for (features_class_index in seq(length(features_A))) {
  if (grepl( "original", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["original"] <- vector_filter_class_max_A["original"] + 1
  } else if (grepl( "exponential", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["exponential"] <- vector_filter_class_max_A["exponential"] + 1
  } else if (grepl( "logarithm", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["logarithm"] <- vector_filter_class_max_A["logarithm"] + 1
  } else if (grepl( "square_", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["square"] <- vector_filter_class_max_A["square"] + 1
  } else if (grepl( "squareroot", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["squareroot"] <- vector_filter_class_max_A["squareroot"] + 1
  } else if (grepl( "wavelet.LL", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["wavelet.LL"] <- vector_filter_class_max_A["wavelet.LL"] + 1
  } else if (grepl( "wavelet.HH", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["wavelet.HH"] <- vector_filter_class_max_A["wavelet.HH"] + 1
  } else if (grepl( "wavelet.HL", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["wavelet.HL"] <- vector_filter_class_max_A["wavelet.HL"] + 1
  } else if (grepl( "wavelet.LH", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["wavelet.LH"] <- vector_filter_class_max_A["wavelet.LH"] + 1
  } else if (grepl( "log.sigma.", features_A[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_A["log.sigma."] <- vector_filter_class_max_A["log.sigma."] + 1
  }
}
vector_filter_class_max_A <- 3 * vector_filter_class_max_A

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_filter_class_count_A <- matrix(0,6,10)
colnames(vector_filter_class_count_A) <- c("original", "exponential", "logarithm", "square", "squareroot", "wavelet.LL", "wavelet.HH", "wavelet.HL", "wavelet.LH", "log.sigma.")
rownames(vector_filter_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "original", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,1] <- vector_filter_class_count_A[,1] + text_disc_A[,features_class_index]
  } else if (grepl( "exponential", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,2] <- vector_filter_class_count_A[,2] + text_disc_A[,features_class_index]
  } else if (grepl( "logarithm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,3] <- vector_filter_class_count_A[,3] + text_disc_A[,features_class_index]
  } else if (grepl( "square_", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,4] <- vector_filter_class_count_A[,4] + text_disc_A[,features_class_index]
  } else if (grepl( "squareroot", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,5] <- vector_filter_class_count_A[,5] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LL", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,6] <- vector_filter_class_count_A[,6] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HH", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,7] <- vector_filter_class_count_A[,7] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HL", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,8] <- vector_filter_class_count_A[,8] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LH", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,9] <- vector_filter_class_count_A[,9] + text_disc_A[,features_class_index]
  } else if (grepl( "log.sigma.", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_A[,10] <- vector_filter_class_count_A[,10] + text_disc_A[,features_class_index]
  }
}

perc_A_filter <- sweep(vector_filter_class_count_A, 2, vector_filter_class_max_A, FUN = '/') * 100


ylabels_plot_max_A <- vector()
for (i in seq(length(vector_feature_class_max_A))) {
  ylabels_plot_max_A <- c(ylabels_plot_max_A, paste0(names(vector_feature_class_max_A[i]), ' (', vector_feature_class_max_A[i], ')'))
}


# ## features B
vector_feature_class_max_B <- vector(,6)
names(vector_feature_class_max_B) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")
for (features_class_index in seq(length(counts_filters_B))) {
  if (grepl( "firstorder", names(counts_filters_B)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_B["firstorder"] <- vector_feature_class_max_B["firstorder"] + counts_filters_B[features_class_index]
  } else if (grepl( "glcm", names(counts_filters_B)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_B["glcm"] <- vector_feature_class_max_B["glcm"] + counts_filters_B[features_class_index]
  } else if (grepl( "gldm", names(counts_filters_B)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_B["gldm"] <- vector_feature_class_max_B["gldm"] + counts_filters_B[features_class_index]
  } else if (grepl( "glrlm", names(counts_filters_B)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_B["glrlm"] <- vector_feature_class_max_B["glrlm"] + counts_filters_B[features_class_index]
  } else if (grepl( "glszm", names(counts_filters_B)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_B["glszm"] <- vector_feature_class_max_B["glszm"] + counts_filters_B[features_class_index]
  } else if (grepl( "ngtdm", names(counts_filters_B)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_B["ngtdm"] <- vector_feature_class_max_B["ngtdm"] + counts_filters_B[features_class_index]
  }
}


features_B <- colnames(dataset_B[, !grepl('original_shape', colnames(dataset_B))])[-c(1)]
vector_filter_class_max_B <- vector(,10)
names(vector_filter_class_max_B) <- c("original", "exponential", "logarithm", "square", "squareroot", "wavelet.LL", "wavelet.HH", "wavelet.HL", "wavelet.LH", "log.sigma.")
for (features_class_index in seq(length(features_B))) {
  if (grepl( "original", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["original"] <- vector_filter_class_max_B["original"] + 1
  } else if (grepl( "exponential", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["exponential"] <- vector_filter_class_max_B["exponential"] + 1
  } else if (grepl( "logarithm", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["logarithm"] <- vector_filter_class_max_B["logarithm"] + 1
  } else if (grepl( "square_", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["square"] <- vector_filter_class_max_B["square"] + 1
  } else if (grepl( "squareroot", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["squareroot"] <- vector_filter_class_max_B["squareroot"] + 1
  } else if (grepl( "wavelet.LL", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["wavelet.LL"] <- vector_filter_class_max_B["wavelet.LL"] + 1
  } else if (grepl( "wavelet.HH", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["wavelet.HH"] <- vector_filter_class_max_B["wavelet.HH"] + 1
  } else if (grepl( "wavelet.HL", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["wavelet.HL"] <- vector_filter_class_max_B["wavelet.HL"] + 1
  } else if (grepl( "wavelet.LH", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["wavelet.LH"] <- vector_filter_class_max_B["wavelet.LH"] + 1
  } else if (grepl( "log.sigma.", features_B[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_B["log.sigma."] <- vector_filter_class_max_B["log.sigma."] + 1
  }
}
vector_filter_class_max_B <- 3 * vector_filter_class_max_B

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_filter_class_count_B <- matrix(0,6,10)
colnames(vector_filter_class_count_B) <- c("original", "exponential", "logarithm", "square", "squareroot", "wavelet.LL", "wavelet.HH", "wavelet.HL", "wavelet.LH", "log.sigma.")
rownames(vector_filter_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "original", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,1] <- vector_filter_class_count_B[,1] + text_disc_B[,features_class_index]
  } else if (grepl( "exponential", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,2] <- vector_filter_class_count_B[,2] + text_disc_B[,features_class_index]
  } else if (grepl( "logarithm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,3] <- vector_filter_class_count_B[,3] + text_disc_B[,features_class_index]
  } else if (grepl( "square_", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,4] <- vector_filter_class_count_B[,4] + text_disc_B[,features_class_index]
  } else if (grepl( "squareroot", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,5] <- vector_filter_class_count_B[,5] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LL", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,6] <- vector_filter_class_count_B[,6] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HH", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,7] <- vector_filter_class_count_B[,7] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HL", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,8] <- vector_filter_class_count_B[,8] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LH", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,9] <- vector_filter_class_count_B[,9] + text_disc_B[,features_class_index]
  } else if (grepl( "log.sigma.", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_B[,10] <- vector_filter_class_count_B[,10] + text_disc_B[,features_class_index]
  }
}

perc_B_filter <- sweep(vector_filter_class_count_B, 2, vector_filter_class_max_B, FUN = '/') * 100

ylabels_plot_max_B <- vector()
for (i in seq(length(vector_feature_class_max_B))) {
  ylabels_plot_max_B <- c(ylabels_plot_max_B, paste0(names(vector_feature_class_max_B[i]), ' (', vector_feature_class_max_B[i], ')'))
}


# ## features C
vector_feature_class_max_C <- vector(,6)
names(vector_feature_class_max_C) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")
for (features_class_index in seq(length(counts_filters_C))) {
  if (grepl( "firstorder", names(counts_filters_C)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_C["firstorder"] <- vector_feature_class_max_C["firstorder"] + counts_filters_C[features_class_index]
  } else if (grepl( "glcm", names(counts_filters_C)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_C["glcm"] <- vector_feature_class_max_C["glcm"] + counts_filters_C[features_class_index]
  } else if (grepl( "gldm", names(counts_filters_C)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_C["gldm"] <- vector_feature_class_max_C["gldm"] + counts_filters_C[features_class_index]
  } else if (grepl( "glrlm", names(counts_filters_C)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_C["glrlm"] <- vector_feature_class_max_C["glrlm"] + counts_filters_C[features_class_index]
  } else if (grepl( "glszm", names(counts_filters_C)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_C["glszm"] <- vector_feature_class_max_C["glszm"] + counts_filters_C[features_class_index]
  } else if (grepl( "ngtdm", names(counts_filters_C)[features_class_index], fixed = TRUE)) {
    vector_feature_class_max_C["ngtdm"] <- vector_feature_class_max_C["ngtdm"] + counts_filters_C[features_class_index]
  }
}


features_C <- colnames(dataset_C[, !grepl('original_shape', colnames(dataset_C))])[-c(1)]
vector_filter_class_max_C <- vector(,10)
names(vector_filter_class_max_C) <- c("original", "exponential", "logarithm", "square", "squareroot", "wavelet.LL", "wavelet.HH", "wavelet.HL", "wavelet.LH", "log.sigma.")
for (features_class_index in seq(length(features_C))) {
  if (grepl( "original", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["original"] <- vector_filter_class_max_C["original"] + 1
  } else if (grepl( "exponential", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["exponential"] <- vector_filter_class_max_C["exponential"] + 1
  } else if (grepl( "logarithm", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["logarithm"] <- vector_filter_class_max_C["logarithm"] + 1
  } else if (grepl( "square_", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["square"] <- vector_filter_class_max_C["square"] + 1
  } else if (grepl( "squareroot", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["squareroot"] <- vector_filter_class_max_C["squareroot"] + 1
  } else if (grepl( "wavelet.LL", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["wavelet.LL"] <- vector_filter_class_max_C["wavelet.LL"] + 1
  } else if (grepl( "wavelet.HH", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["wavelet.HH"] <- vector_filter_class_max_C["wavelet.HH"] + 1
  } else if (grepl( "wavelet.HL", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["wavelet.HL"] <- vector_filter_class_max_C["wavelet.HL"] + 1
  } else if (grepl( "wavelet.LH", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["wavelet.LH"] <- vector_filter_class_max_C["wavelet.LH"] + 1
  } else if (grepl( "log.sigma.", features_C[features_class_index], fixed = TRUE)) {
    vector_filter_class_max_C["log.sigma."] <- vector_filter_class_max_C["log.sigma."] + 1
  }
}
vector_filter_class_max_C <- 3 * vector_filter_class_max_C

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_filter_class_count_C <- matrix(0,6,10)
colnames(vector_filter_class_count_C) <- c("original", "exponential", "logarithm", "square", "squareroot", "wavelet.LL", "wavelet.HH", "wavelet.HL", "wavelet.LH", "log.sigma.")
rownames(vector_filter_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "original", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,1] <- vector_filter_class_count_C[,1] + text_disc_C[,features_class_index]
  } else if (grepl( "exponential", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,2] <- vector_filter_class_count_C[,2] + text_disc_C[,features_class_index]
  } else if (grepl( "logarithm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,3] <- vector_filter_class_count_C[,3] + text_disc_C[,features_class_index]
  } else if (grepl( "square_", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,4] <- vector_filter_class_count_C[,4] + text_disc_C[,features_class_index]
  } else if (grepl( "squareroot", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,5] <- vector_filter_class_count_C[,5] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LL", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,6] <- vector_filter_class_count_C[,6] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HH", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,7] <- vector_filter_class_count_C[,7] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HL", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,8] <- vector_filter_class_count_C[,8] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LH", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,9] <- vector_filter_class_count_C[,9] + text_disc_C[,features_class_index]
  } else if (grepl( "log.sigma.", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_filter_class_count_C[,10] <- vector_filter_class_count_C[,10] + text_disc_C[,features_class_index]
  }
}

perc_C_filter <- sweep(vector_filter_class_count_C, 2, vector_filter_class_max_C, FUN = '/') * 100

ylabels_plot_max_C <- vector()
for (i in seq(length(vector_feature_class_max_C))) {
  ylabels_plot_max_C <- c(ylabels_plot_max_C, paste0(names(vector_feature_class_max_C[i]), ' (', vector_feature_class_max_C[i], ')'))
}

vector_feature_class_sum_1vs2_A <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_1vs3_A <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_3vs2_A <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_1vs2_B <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_1vs3_B <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_3vs2_B <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_1vs2_C <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_1vs3_C <- matrix(0,nrow=6,ncol=6)
vector_feature_class_sum_3vs2_C <- matrix(0,nrow=6,ncol=6)
for (row_index in seq(nrow(count_per_feature_A_1vs2_total))) {
  if (grepl( "firstorder", rownames(count_per_feature_A_1vs2_total)[row_index], fixed = TRUE)) {
    vector_feature_class_sum_1vs2_A[1,] <- vector_feature_class_sum_1vs2_A[1,] + count_per_feature_A_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_A[1,] <- vector_feature_class_sum_1vs3_A[1,] + count_per_feature_A_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_A[1,] <- vector_feature_class_sum_3vs2_A[1,] + count_per_feature_A_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_B[1,] <- vector_feature_class_sum_1vs2_B[1,] + count_per_feature_B_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_B[1,] <- vector_feature_class_sum_1vs3_B[1,] + count_per_feature_B_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_B[1,] <- vector_feature_class_sum_3vs2_B[1,] + count_per_feature_B_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_C[1,] <- vector_feature_class_sum_1vs2_C[1,] + count_per_feature_C_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_C[1,] <- vector_feature_class_sum_1vs3_C[1,] + count_per_feature_C_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_C[1,] <- vector_feature_class_sum_3vs2_C[1,] + count_per_feature_C_3vs2_total[row_index,]
    
  } else if (grepl( "glcm", rownames(count_per_feature_A_1vs2_total)[row_index], fixed = TRUE)) {
    vector_feature_class_sum_1vs2_A[2,] <- vector_feature_class_sum_1vs2_A[2,] + count_per_feature_A_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_A[2,] <- vector_feature_class_sum_1vs3_A[2,] + count_per_feature_A_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_A[2,] <- vector_feature_class_sum_3vs2_A[2,] + count_per_feature_A_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_B[2,] <- vector_feature_class_sum_1vs2_B[2,] + count_per_feature_B_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_B[2,] <- vector_feature_class_sum_1vs3_B[2,] + count_per_feature_B_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_B[2,] <- vector_feature_class_sum_3vs2_B[2,] + count_per_feature_B_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_C[2,] <- vector_feature_class_sum_1vs2_C[2,] + count_per_feature_C_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_C[2,] <- vector_feature_class_sum_1vs3_C[2,] + count_per_feature_C_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_C[2,] <- vector_feature_class_sum_3vs2_C[2,] + count_per_feature_C_3vs2_total[row_index,]
    
  } else if (grepl( "gldm", rownames(count_per_feature_A_1vs2_total)[row_index], fixed = TRUE)) {
    vector_feature_class_sum_1vs2_A[3,] <- vector_feature_class_sum_1vs2_A[3,] + count_per_feature_A_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_A[3,] <- vector_feature_class_sum_1vs3_A[3,] + count_per_feature_A_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_A[3,] <- vector_feature_class_sum_3vs2_A[3,] + count_per_feature_A_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_B[3,] <- vector_feature_class_sum_1vs2_B[3,] + count_per_feature_B_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_B[3,] <- vector_feature_class_sum_1vs3_B[3,] + count_per_feature_B_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_B[3,] <- vector_feature_class_sum_3vs2_B[3,] + count_per_feature_B_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_C[3,] <- vector_feature_class_sum_1vs2_C[3,] + count_per_feature_C_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_C[3,] <- vector_feature_class_sum_1vs3_C[3,] + count_per_feature_C_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_C[3,] <- vector_feature_class_sum_3vs2_C[3,] + count_per_feature_C_3vs2_total[row_index,]
    
  } else if (grepl( "glrlm", rownames(count_per_feature_A_1vs2_total)[row_index], fixed = TRUE)) {
    vector_feature_class_sum_1vs2_A[4,] <- vector_feature_class_sum_1vs2_A[4,] + count_per_feature_A_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_A[4,] <- vector_feature_class_sum_1vs3_A[4,] + count_per_feature_A_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_A[4,] <- vector_feature_class_sum_3vs2_A[4,] + count_per_feature_A_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_B[4,] <- vector_feature_class_sum_1vs2_B[4,] + count_per_feature_B_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_B[4,] <- vector_feature_class_sum_1vs3_B[4,] + count_per_feature_B_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_B[4,] <- vector_feature_class_sum_3vs2_B[4,] + count_per_feature_B_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_C[4,] <- vector_feature_class_sum_1vs2_C[4,] + count_per_feature_C_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_C[4,] <- vector_feature_class_sum_1vs3_C[4,] + count_per_feature_C_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_C[4,] <- vector_feature_class_sum_3vs2_C[4,] + count_per_feature_C_3vs2_total[row_index,]
    
  } else if (grepl( "glszm", rownames(count_per_feature_A_1vs2_total)[row_index], fixed = TRUE)) {
    vector_feature_class_sum_1vs2_A[5,] <- vector_feature_class_sum_1vs2_A[5,] + count_per_feature_A_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_A[5,] <- vector_feature_class_sum_1vs3_A[5,] + count_per_feature_A_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_A[5,] <- vector_feature_class_sum_3vs2_A[5,] + count_per_feature_A_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_B[5,] <- vector_feature_class_sum_1vs2_B[5,] + count_per_feature_B_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_B[5,] <- vector_feature_class_sum_1vs3_B[5,] + count_per_feature_B_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_B[5,] <- vector_feature_class_sum_3vs2_B[5,] + count_per_feature_B_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_C[5,] <- vector_feature_class_sum_1vs2_C[5,] + count_per_feature_C_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_C[5,] <- vector_feature_class_sum_1vs3_C[5,] + count_per_feature_C_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_C[5,] <- vector_feature_class_sum_3vs2_C[5,] + count_per_feature_C_3vs2_total[row_index,]
    
  } else if (grepl( "ngtdm", rownames(count_per_feature_A_1vs2_total)[row_index], fixed = TRUE)) {
    vector_feature_class_sum_1vs2_A[6,] <- vector_feature_class_sum_1vs2_A[6,] + count_per_feature_A_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_A[6,] <- vector_feature_class_sum_1vs3_A[6,] + count_per_feature_A_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_A[6,] <- vector_feature_class_sum_3vs2_A[6,] + count_per_feature_A_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_B[6,] <- vector_feature_class_sum_1vs2_B[6,] + count_per_feature_B_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_B[6,] <- vector_feature_class_sum_1vs3_B[6,] + count_per_feature_B_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_B[6,] <- vector_feature_class_sum_3vs2_B[6,] + count_per_feature_B_3vs2_total[row_index,]
    vector_feature_class_sum_1vs2_C[6,] <- vector_feature_class_sum_1vs2_C[6,] + count_per_feature_C_1vs2_total[row_index,]
    vector_feature_class_sum_1vs3_C[6,] <- vector_feature_class_sum_1vs3_C[6,] + count_per_feature_C_1vs3_total[row_index,]
    vector_feature_class_sum_3vs2_C[6,] <- vector_feature_class_sum_3vs2_C[6,] + count_per_feature_C_3vs2_total[row_index,]
    
  }
}

perc_A <- (vector_feature_class_sum_1vs2_A + vector_feature_class_sum_1vs3_A + vector_feature_class_sum_3vs2_A)/(3*vector_feature_class_max_A) * 100
perc_B <- (vector_feature_class_sum_1vs2_B + vector_feature_class_sum_1vs3_B + vector_feature_class_sum_3vs2_B)/(3*vector_feature_class_max_B) * 100
perc_C <- (vector_feature_class_sum_1vs2_C + vector_feature_class_sum_1vs3_C + vector_feature_class_sum_3vs2_C)/(3*vector_feature_class_max_C) * 100

rownames(perc_A) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")
rownames(perc_B) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")
rownames(perc_C) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")
colnames(perc_A) <- volumes
colnames(perc_B) <- volumes
colnames(perc_C) <- volumes

library(corrplot)
library(RColorBrewer)
M <-perc_A
# pdf(file = paste(dir_save_plots,"perc_A_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          #col.lim = c(min(M), max(M)), #c(55, 100),#
#          col.lim = c(55, 100),#
#          full_col=FALSE,
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()

df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")


M <-perc_B
colnames(M) <- as.factor(colnames(M))
# pdf(file = paste(dir_save_plots,"perc_B_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

M <-perc_C
# pdf(file = paste(dir_save_plots,"perc_C_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_1vs2_A <- vector_feature_class_sum_1vs2_A/vector_feature_class_max_A * 100
colnames(perc_1vs2_A) <- colnames(perc_A)
rownames(perc_1vs2_A) <- rownames(perc_A)
M <-perc_1vs2_A
# pdf(file = paste(dir_save_plots,"perc_A_1vs2_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_1vs2_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_1vs3_A <- vector_feature_class_sum_1vs3_A/vector_feature_class_max_A * 100
colnames(perc_1vs3_A) <- colnames(perc_A)
rownames(perc_1vs3_A) <- rownames(perc_A)
M <-perc_1vs3_A
# pdf(file = paste(dir_save_plots,"perc_A_1vs3_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_1vs3_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_3vs2_A <- vector_feature_class_sum_3vs2_A/vector_feature_class_max_A * 100
colnames(perc_3vs2_A) <- colnames(perc_A)
rownames(perc_3vs2_A) <- rownames(perc_A)
M <-perc_3vs2_A
# pdf(file = paste(dir_save_plots,"perc_A_3vs2_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_3vs2_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_1vs2_B <- vector_feature_class_sum_1vs2_B/vector_feature_class_max_B * 100
colnames(perc_1vs2_B) <- colnames(perc_B)
rownames(perc_1vs2_B) <- rownames(perc_B)
M <-perc_1vs2_B
# pdf(file = paste(dir_save_plots,"perc_B_1vs2_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_1vs2_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_1vs3_B <- vector_feature_class_sum_1vs3_B/vector_feature_class_max_B * 100
colnames(perc_1vs3_B) <- colnames(perc_B)
rownames(perc_1vs3_B) <- rownames(perc_B)
M <-perc_1vs3_B
# pdf(file = paste(dir_save_plots,"perc_B_1vs3_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_1vs3_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_3vs2_B <- vector_feature_class_sum_3vs2_B/vector_feature_class_max_B * 100
colnames(perc_3vs2_B) <- colnames(perc_B)
rownames(perc_3vs2_B) <- rownames(perc_B)
M <-perc_3vs2_B
# pdf(file = paste(dir_save_plots,"perc_B_3vs2_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_3vs2_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_1vs2_C <- vector_feature_class_sum_1vs2_C/vector_feature_class_max_C * 100
colnames(perc_1vs2_C) <- colnames(perc_C)
rownames(perc_1vs2_C) <- rownames(perc_C)
M <-perc_1vs2_C
# pdf(file = paste(dir_save_plots,"perc_C_1vs2_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_1vs2_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_1vs3_C <- vector_feature_class_sum_1vs3_C/vector_feature_class_max_C * 100
colnames(perc_1vs3_C) <- colnames(perc_C)
rownames(perc_1vs3_C) <- rownames(perc_C)
M <-perc_1vs3_C
# pdf(file = paste(dir_save_plots,"perc_C_1vs3_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()

df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_1vs3_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_3vs2_C <- vector_feature_class_sum_3vs2_C/vector_feature_class_max_C * 100
colnames(perc_3vs2_C) <- colnames(perc_C)
rownames(perc_3vs2_C) <- rownames(perc_C)
M <-perc_3vs2_C
# pdf(file = paste(dir_save_plots,"perc_C_3vs2_featureClass_vs_Volume.pdf",sep = ''), width = 8, height = 8)
# corrplot(M, 
#          addCoef.col ='black', number.cex = 1.1,
#          method = 'color',
#          tl.cex = 1.25, tl.col = 'black', cl.cex = 1.2, cl.ratio = 0.2,
#          is.corr=FALSE,
#          col.lim = c(min(M), max(M)), #c(55, 100),#
#          col=brewer.pal(n=8, name="RdYlBu")
# )
# dev.off()
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_3vs2_featureClass_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

perc_agg_A <- colSums(vector_feature_class_sum_1vs2_A + vector_feature_class_sum_1vs3_A + vector_feature_class_sum_3vs2_A)/(3*sum(vector_feature_class_max_A)) * 100
perc_agg_B <- colSums(vector_feature_class_sum_1vs2_B + vector_feature_class_sum_1vs3_B + vector_feature_class_sum_3vs2_B)/(3*sum(vector_feature_class_max_B)) * 100
perc_agg_C <- colSums(vector_feature_class_sum_1vs2_C + vector_feature_class_sum_1vs3_C + vector_feature_class_sum_3vs2_C)/(3*sum(vector_feature_class_max_C)) * 100

perc_agg_scanners <- rbind(perc_agg_A, perc_agg_B, perc_agg_C)
colnames(perc_agg_scanners) <- volume_sizes
rownames(perc_agg_scanners) <- c("Scanner A", "Scanner B", "Scanner C")
perc_agg_scanners
M <-perc_agg_scanners
df_ggplot <- reshape2::melt(M)
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_agg_scanners_vs_Volume.pdf",sep = ''), width = 9, height = 4, units = "in")


M <- perc_A_filter
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_filter_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

M <- perc_B_filter
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_filter_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

M <- perc_C_filter
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_filter_vs_Volume.pdf",sep = ''), width = 9, height = 8, units = "in")

# correlation of filters vs volumes
p_values_A_filter <- vector(,10)
p_values_B_filter <- vector(,10)
p_values_C_filter <- vector(,10)
rho_values_A_filter <- vector(,10)
rho_values_B_filter <- vector(,10)
rho_values_C_filter <- vector(,10)
for (indx in seq(10)) {
  rho_p_A <- cor.test(perc_A_filter[,indx], as.numeric(volume_sizes), method = 'spearman')
  rho_p_B <- cor.test(perc_B_filter[,indx], as.numeric(volume_sizes), method = 'spearman')
  rho_p_C <- cor.test(perc_C_filter[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_filter[indx] <- rho_p_A$p.value
  p_values_B_filter[indx] <- rho_p_B$p.value
  p_values_C_filter[indx] <- rho_p_C$p.value
  rho_values_A_filter[indx] <- rho_p_A$estimate
  rho_values_B_filter[indx] <- rho_p_B$estimate
  rho_values_C_filter[indx] <- rho_p_C$estimate
}

rsqr_values_A_filter <- rho_values_A_filter**2
rsqr_values_B_filter <- rho_values_B_filter**2
rsqr_values_C_filter <- rho_values_C_filter**2

names(p_values_A_filter) <- colnames(perc_A_filter)
names(p_values_B_filter) <- colnames(perc_B_filter)
names(p_values_C_filter) <- colnames(perc_C_filter)

names(rsqr_values_A_filter) <- colnames(perc_A_filter)
names(rsqr_values_B_filter) <- colnames(perc_B_filter)
names(rsqr_values_C_filter) <- colnames(perc_C_filter)
  
p_values_filters_scanners <- rbind(p_values_A_filter, p_values_B_filter, p_values_C_filter)
r_squared_filters_scanners <- rbind(rsqr_values_A_filter, rsqr_values_B_filter, rsqr_values_C_filter)
rownames(r_squared_filters_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squared_filters_scanners) <- c('Original', 'Exponential', 'Logarithm', 'Square', 'Square Root', 'Wavelet LL', 'Wavelet HH', 'Wavelet HL', 'Wavelet LH', 'LoG')


pdf(file = paste(dir_save_plots,"r_squared_filters_scanners.pdf",sep = ''), width = 5.38, height = 8.68)
corrplot(t(r_squared_filters_scanners), p.mat = t(p_values_filters_scanners), sig.level = c(.001, .01, .05), 
         insig='label_sig', pch.cex = 1.6, pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()

# correlation of feature classes vs volumes
p_values_A_feature <- vector(,6)
p_values_B_feature <- vector(,6)
p_values_C_feature <- vector(,6)
rho_values_A_feature <- vector(,6)
rho_values_B_feature <- vector(,6)
rho_values_C_feature <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A)[,indx], as.numeric(volume_sizes), method = 'spearman')
  rho_p_B <- cor.test(t(perc_B)[,indx], as.numeric(volume_sizes), method = 'spearman')
  rho_p_C <- cor.test(t(perc_C)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_feature[indx] <- rho_p_A$p.value
  p_values_B_feature[indx] <- rho_p_B$p.value
  p_values_C_feature[indx] <- rho_p_C$p.value
  rho_values_A_feature[indx] <- rho_p_A$estimate
  rho_values_B_feature[indx] <- rho_p_B$estimate
  rho_values_C_feature[indx] <- rho_p_C$estimate
}

rsqr_values_A_feature <- rho_values_A_feature**2
rsqr_values_B_feature <- rho_values_B_feature**2
rsqr_values_C_feature <- rho_values_C_feature**2

names(p_values_A_feature) <- colnames(perc_A)
names(p_values_B_feature) <- colnames(perc_B)
names(p_values_C_feature) <- colnames(perc_C)

names(rsqr_values_A_feature) <- colnames(perc_A)
names(rsqr_values_B_feature) <- colnames(perc_B)
names(rsqr_values_C_feature) <- colnames(perc_C)

p_values_features_scanners <- rbind(p_values_A_feature, p_values_B_feature, p_values_C_feature)
r_squared_features_scanners <- rbind(rsqr_values_A_feature, rsqr_values_B_feature, rsqr_values_C_feature)
rownames(r_squared_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squared_features_scanners) <- c("firstorder", "glcm", "gldm", "glrlm", "glszm", "ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_features_scanners.pdf",sep = ''), width = 5.38, height = 8.68)
corrplot(t(r_squared_features_scanners), p.mat = t(p_values_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=0)
dev.off()

# correlation of feature classes vs volumes
p_values_agg <- matrix(,1,3)
rho_values_agg <- matrix(,1,3)
for (indx in seq(3)) {
  rho_p <- cor.test(t(perc_agg_scanners)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_agg[indx] <- rho_p$p.value
  rho_values_agg[indx] <- rho_p$estimate
}

rho_values_agg <- rho_values_agg**2

colnames(p_values_agg) <- rownames(perc_agg_scanners)

colnames(rho_values_agg) <- rownames(perc_agg_scanners)
rownames(rho_values_agg) <- c('R-Squared')

library(ggplot2)
library(gtable)  # 
library(grid)
library(scales)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
df_ggplot <- reshape2::melt(rho_values_agg)
df_ggplot$value <- round(df_ggplot$value,digits=2 )
plot = ggplot(df_ggplot, aes(x = Var1, y = Var2, fill = value))+
  geom_tile() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(),text = element_text(size=20), 
        axis.ticks = element_blank(), axis.text.x = element_text(color="black"), 
        axis.text.y = element_text(color="black"), panel.background = element_rect(fill = NA), 
        axis.title.x=element_blank(), axis.title.y=element_blank()) + 
  scale_fill_gradientn("", colours=brewer.pal(200,"RdBu"), limits=c(0,1), breaks=c(0,0.5,1),labels=c(0,0.5,1)) + 
  geom_text(data=df_ggplot,aes(x=Var1,label=value),  size=10)

plot

panel_height = unit(1,"npc") - 3*sum(ggplotGrob(plot)[["heights"]][-3]) - unit(2,"line")
plot + guides(fill= guide_colorbar(barheight=panel_height))
ggsave(paste(dir_save_plots,"rsquare_scanners.pdf",sep = ''), width = 8.13, height = 3.66, units = "in")


#####################################################################
# ## features A
vector_original_feature_class_max_A <- vector(,6)
names(vector_original_feature_class_max_A) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")
vector_original_feature_class_max_A["original_firstorder"] <- sum(grepl("original_firstorder", features_A))
vector_original_feature_class_max_A["original_glcm"] <- sum(grepl("original_glcm", features_A))
vector_original_feature_class_max_A["original_gldm"] <- sum(grepl("original_glrlm", features_A))
vector_original_feature_class_max_A["original_glrlm"] <- sum(grepl("original_glrlm", features_A))
vector_original_feature_class_max_A["original_glszm"] <- sum(grepl("original_glszm", features_A))
vector_original_feature_class_max_A["original_ngtdm"] <- sum(grepl("original_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_original_feature_class_count_A <- matrix(0,6,6)
colnames(vector_original_feature_class_count_A) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")
rownames(vector_original_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "original_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_A[,"original_firstorder"] <- vector_original_feature_class_count_A[,"original_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "original_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_A[,"original_glcm"] <- vector_original_feature_class_count_A[,"original_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "original_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_A[,"original_gldm"] <- vector_original_feature_class_count_A[,"original_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "original_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_A[,"original_glrlm"] <- vector_original_feature_class_count_A[,"original_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "original_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_A[,"original_glszm"] <- vector_original_feature_class_count_A[,"original_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "original_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_A[,"original_ngtdm"] <- vector_original_feature_class_count_A[,"original_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_original_feature_class <- sweep(vector_original_feature_class_count_A, 2, 3*vector_original_feature_class_max_A, FUN = '/') * 100

M <- perc_A_original_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_original_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_original_feature_class <- vector(,6)
rho_values_A_original_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_original_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_original_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_original_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_original_feature_class <- rho_values_A_original_feature_class**2

names(p_values_A_original_feature_class) <- colnames(perc_A)

names(rsqr_values_A_original_feature_class) <- colnames(perc_A)



# ## features B
vector_original_feature_class_max_B <- vector(,6)
names(vector_original_feature_class_max_B) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")
vector_original_feature_class_max_B["original_firstorder"] <- sum(grepl("original_firstorder", features_B))
vector_original_feature_class_max_B["original_glcm"] <- sum(grepl("original_glcm", features_B))
vector_original_feature_class_max_B["original_gldm"] <- sum(grepl("original_glrlm", features_B))
vector_original_feature_class_max_B["original_glrlm"] <- sum(grepl("original_glrlm", features_B))
vector_original_feature_class_max_B["original_glszm"] <- sum(grepl("original_glszm", features_B))
vector_original_feature_class_max_B["original_ngtdm"] <- sum(grepl("original_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_original_feature_class_count_B <- matrix(0,6,6)
colnames(vector_original_feature_class_count_B) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")
rownames(vector_original_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "original_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_B[,"original_firstorder"] <- vector_original_feature_class_count_B[,"original_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "original_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_B[,"original_glcm"] <- vector_original_feature_class_count_B[,"original_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "original_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_B[,"original_gldm"] <- vector_original_feature_class_count_B[,"original_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "original_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_B[,"original_glrlm"] <- vector_original_feature_class_count_B[,"original_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "original_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_B[,"original_glszm"] <- vector_original_feature_class_count_B[,"original_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "original_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_B[,"original_ngtdm"] <- vector_original_feature_class_count_B[,"original_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_original_feature_class <- sweep(vector_original_feature_class_count_B, 2, 3*vector_original_feature_class_max_B, FUN = '/') * 100

M <- perc_B_original_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_original_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_original_feature_class <- vector(,6)
rho_values_B_original_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_original_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_original_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_original_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_original_feature_class <- rho_values_B_original_feature_class**2

names(p_values_B_original_feature_class) <- colnames(perc_B)

names(rsqr_values_B_original_feature_class) <- colnames(perc_B)

# ## features C
vector_original_feature_class_max_C <- vector(,6)
names(vector_original_feature_class_max_C) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")
vector_original_feature_class_max_C["original_firstorder"] <- sum(grepl("original_firstorder", features_C))
vector_original_feature_class_max_C["original_glcm"] <- sum(grepl("original_glcm", features_C))
vector_original_feature_class_max_C["original_gldm"] <- sum(grepl("original_glrlm", features_C))
vector_original_feature_class_max_C["original_glrlm"] <- sum(grepl("original_glrlm", features_C))
vector_original_feature_class_max_C["original_glszm"] <- sum(grepl("original_glszm", features_C))
vector_original_feature_class_max_C["original_ngtdm"] <- sum(grepl("original_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_original_feature_class_count_C <- matrix(0,6,6)
colnames(vector_original_feature_class_count_C) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")
rownames(vector_original_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "original_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_C[,"original_firstorder"] <- vector_original_feature_class_count_C[,"original_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "original_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_C[,"original_glcm"] <- vector_original_feature_class_count_C[,"original_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "original_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_C[,"original_gldm"] <- vector_original_feature_class_count_C[,"original_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "original_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_C[,"original_glrlm"] <- vector_original_feature_class_count_C[,"original_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "original_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_C[,"original_glszm"] <- vector_original_feature_class_count_C[,"original_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "original_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_original_feature_class_count_C[,"original_ngtdm"] <- vector_original_feature_class_count_C[,"original_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_original_feature_class <- sweep(vector_original_feature_class_count_C, 2, 3*vector_original_feature_class_max_C, FUN = '/') * 100

M <- perc_C_original_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_original_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_original_feature_class <- vector(,6)
rho_values_C_original_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_original_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_original_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_original_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_original_feature_class <- rho_values_C_original_feature_class**2

names(p_values_C_original_feature_class) <- colnames(perc_C)

names(rsqr_values_C_original_feature_class) <- colnames(perc_C)

p_values_original_features_scanners <- rbind(p_values_A_original_feature_class, p_values_B_original_feature_class, p_values_C_original_feature_class)
r_squared_original_features_scanners <- rbind(rsqr_values_A_original_feature_class, rsqr_values_B_original_feature_class, rsqr_values_C_original_feature_class)
rownames(r_squared_original_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squared_original_features_scanners) <- c("original_firstorder", "original_glcm", "original_gldm", "original_glrlm", "original_glszm", "original_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_original_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_squared_original_features_scanners), p.mat = t(p_values_original_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_exponential_feature_class_max_A <- vector(,6)
names(vector_exponential_feature_class_max_A) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")
vector_exponential_feature_class_max_A["exponential_firstorder"] <- sum(grepl("exponential_firstorder", features_A))
vector_exponential_feature_class_max_A["exponential_glcm"] <- sum(grepl("exponential_glcm", features_A))
vector_exponential_feature_class_max_A["exponential_gldm"] <- sum(grepl("exponential_glrlm", features_A))
vector_exponential_feature_class_max_A["exponential_glrlm"] <- sum(grepl("exponential_glrlm", features_A))
vector_exponential_feature_class_max_A["exponential_glszm"] <- sum(grepl("exponential_glszm", features_A))
vector_exponential_feature_class_max_A["exponential_ngtdm"] <- sum(grepl("exponential_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_exponential_feature_class_count_A <- matrix(0,6,6)
colnames(vector_exponential_feature_class_count_A) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")
rownames(vector_exponential_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "exponential_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_A[,"exponential_firstorder"] <- vector_exponential_feature_class_count_A[,"exponential_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "exponential_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_A[,"exponential_glcm"] <- vector_exponential_feature_class_count_A[,"exponential_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "exponential_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_A[,"exponential_gldm"] <- vector_exponential_feature_class_count_A[,"exponential_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "exponential_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_A[,"exponential_glrlm"] <- vector_exponential_feature_class_count_A[,"exponential_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "exponential_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_A[,"exponential_glszm"] <- vector_exponential_feature_class_count_A[,"exponential_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "exponential_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_A[,"exponential_ngtdm"] <- vector_exponential_feature_class_count_A[,"exponential_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_exponential_feature_class <- sweep(vector_exponential_feature_class_count_A, 2, 3*vector_exponential_feature_class_max_A, FUN = '/') * 100

M <- perc_A_exponential_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_exponential_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_exponential_feature_class <- vector(,6)
rho_values_A_exponential_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_exponential_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_exponential_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_exponential_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_exponential_feature_class <- rho_values_A_exponential_feature_class**2

names(p_values_A_exponential_feature_class) <- colnames(perc_A)

names(rsqr_values_A_exponential_feature_class) <- colnames(perc_A)



# ## features B
vector_exponential_feature_class_max_B <- vector(,6)
names(vector_exponential_feature_class_max_B) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")
vector_exponential_feature_class_max_B["exponential_firstorder"] <- sum(grepl("exponential_firstorder", features_B))
vector_exponential_feature_class_max_B["exponential_glcm"] <- sum(grepl("exponential_glcm", features_B))
vector_exponential_feature_class_max_B["exponential_gldm"] <- sum(grepl("exponential_glrlm", features_B))
vector_exponential_feature_class_max_B["exponential_glrlm"] <- sum(grepl("exponential_glrlm", features_B))
vector_exponential_feature_class_max_B["exponential_glszm"] <- sum(grepl("exponential_glszm", features_B))
vector_exponential_feature_class_max_B["exponential_ngtdm"] <- sum(grepl("exponential_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_exponential_feature_class_count_B <- matrix(0,6,6)
colnames(vector_exponential_feature_class_count_B) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")
rownames(vector_exponential_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "exponential_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_B[,"exponential_firstorder"] <- vector_exponential_feature_class_count_B[,"exponential_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "exponential_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_B[,"exponential_glcm"] <- vector_exponential_feature_class_count_B[,"exponential_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "exponential_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_B[,"exponential_gldm"] <- vector_exponential_feature_class_count_B[,"exponential_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "exponential_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_B[,"exponential_glrlm"] <- vector_exponential_feature_class_count_B[,"exponential_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "exponential_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_B[,"exponential_glszm"] <- vector_exponential_feature_class_count_B[,"exponential_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "exponential_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_B[,"exponential_ngtdm"] <- vector_exponential_feature_class_count_B[,"exponential_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_exponential_feature_class <- sweep(vector_exponential_feature_class_count_B, 2, 3*vector_exponential_feature_class_max_B, FUN = '/') * 100

M <- perc_B_exponential_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_exponential_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_exponential_feature_class <- vector(,6)
rho_values_B_exponential_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_exponential_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_exponential_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_exponential_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_exponential_feature_class <- rho_values_B_exponential_feature_class**2

names(p_values_B_exponential_feature_class) <- colnames(perc_B)

names(rsqr_values_B_exponential_feature_class) <- colnames(perc_B)



# ## features C
vector_exponential_feature_class_max_C <- vector(,6)
names(vector_exponential_feature_class_max_C) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")
vector_exponential_feature_class_max_C["exponential_firstorder"] <- sum(grepl("exponential_firstorder", features_C))
vector_exponential_feature_class_max_C["exponential_glcm"] <- sum(grepl("exponential_glcm", features_C))
vector_exponential_feature_class_max_C["exponential_gldm"] <- sum(grepl("exponential_glrlm", features_C))
vector_exponential_feature_class_max_C["exponential_glrlm"] <- sum(grepl("exponential_glrlm", features_C))
vector_exponential_feature_class_max_C["exponential_glszm"] <- sum(grepl("exponential_glszm", features_C))
vector_exponential_feature_class_max_C["exponential_ngtdm"] <- sum(grepl("exponential_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_exponential_feature_class_count_C <- matrix(0,6,6)
colnames(vector_exponential_feature_class_count_C) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")
rownames(vector_exponential_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "exponential_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_C[,"exponential_firstorder"] <- vector_exponential_feature_class_count_C[,"exponential_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "exponential_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_C[,"exponential_glcm"] <- vector_exponential_feature_class_count_C[,"exponential_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "exponential_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_C[,"exponential_gldm"] <- vector_exponential_feature_class_count_C[,"exponential_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "exponential_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_C[,"exponential_glrlm"] <- vector_exponential_feature_class_count_C[,"exponential_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "exponential_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_C[,"exponential_glszm"] <- vector_exponential_feature_class_count_C[,"exponential_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "exponential_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_exponential_feature_class_count_C[,"exponential_ngtdm"] <- vector_exponential_feature_class_count_C[,"exponential_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_exponential_feature_class <- sweep(vector_exponential_feature_class_count_C, 2, 3*vector_exponential_feature_class_max_C, FUN = '/') * 100

M <- perc_C_exponential_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_exponential_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_exponential_feature_class <- vector(,6)
rho_values_C_exponential_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_exponential_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_exponential_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_exponential_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_exponential_feature_class <- rho_values_C_exponential_feature_class**2

names(p_values_C_exponential_feature_class) <- colnames(perc_C)

names(rsqr_values_C_exponential_feature_class) <- colnames(perc_C)

p_values_exponential_features_scanners <- rbind(p_values_A_exponential_feature_class, p_values_B_exponential_feature_class, p_values_C_exponential_feature_class)
r_squared_exponential_features_scanners <- rbind(rsqr_values_A_exponential_feature_class, rsqr_values_B_exponential_feature_class, rsqr_values_C_exponential_feature_class)
rownames(r_squared_exponential_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squared_exponential_features_scanners) <- c("exponential_firstorder", "exponential_glcm", "exponential_gldm", "exponential_glrlm", "exponential_glszm", "exponential_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_exponential_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_squared_exponential_features_scanners), p.mat = t(p_values_exponential_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_logarithm_feature_class_max_A <- vector(,6)
names(vector_logarithm_feature_class_max_A) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")
vector_logarithm_feature_class_max_A["logarithm_firstorder"] <- sum(grepl("logarithm_firstorder", features_A))
vector_logarithm_feature_class_max_A["logarithm_glcm"] <- sum(grepl("logarithm_glcm", features_A))
vector_logarithm_feature_class_max_A["logarithm_gldm"] <- sum(grepl("logarithm_glrlm", features_A))
vector_logarithm_feature_class_max_A["logarithm_glrlm"] <- sum(grepl("logarithm_glrlm", features_A))
vector_logarithm_feature_class_max_A["logarithm_glszm"] <- sum(grepl("logarithm_glszm", features_A))
vector_logarithm_feature_class_max_A["logarithm_ngtdm"] <- sum(grepl("logarithm_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_logarithm_feature_class_count_A <- matrix(0,6,6)
colnames(vector_logarithm_feature_class_count_A) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")
rownames(vector_logarithm_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "logarithm_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_A[,"logarithm_firstorder"] <- vector_logarithm_feature_class_count_A[,"logarithm_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "logarithm_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_A[,"logarithm_glcm"] <- vector_logarithm_feature_class_count_A[,"logarithm_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "logarithm_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_A[,"logarithm_gldm"] <- vector_logarithm_feature_class_count_A[,"logarithm_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "logarithm_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_A[,"logarithm_glrlm"] <- vector_logarithm_feature_class_count_A[,"logarithm_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "logarithm_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_A[,"logarithm_glszm"] <- vector_logarithm_feature_class_count_A[,"logarithm_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "logarithm_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_A[,"logarithm_ngtdm"] <- vector_logarithm_feature_class_count_A[,"logarithm_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_logarithm_feature_class <- sweep(vector_logarithm_feature_class_count_A, 2, 3*vector_logarithm_feature_class_max_A, FUN = '/') * 100
#perc_A_logarithm_feature_class <- vector_logarithm_feature_class_count_A / (3 * vector_logarithm_feature_class_max_A) * 100

M <- perc_A_logarithm_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_logarithm_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_logarithm_feature_class <- vector(,6)
rho_values_A_logarithm_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_logarithm_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_logarithm_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_logarithm_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_logarithm_feature_class <- rho_values_A_logarithm_feature_class**2

names(p_values_A_logarithm_feature_class) <- colnames(perc_A)

names(rsqr_values_A_logarithm_feature_class) <- colnames(perc_A)



# ## features B
vector_logarithm_feature_class_max_B <- vector(,6)
names(vector_logarithm_feature_class_max_B) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")
vector_logarithm_feature_class_max_B["logarithm_firstorder"] <- sum(grepl("logarithm_firstorder", features_B))
vector_logarithm_feature_class_max_B["logarithm_glcm"] <- sum(grepl("logarithm_glcm", features_B))
vector_logarithm_feature_class_max_B["logarithm_gldm"] <- sum(grepl("logarithm_glrlm", features_B))
vector_logarithm_feature_class_max_B["logarithm_glrlm"] <- sum(grepl("logarithm_glrlm", features_B))
vector_logarithm_feature_class_max_B["logarithm_glszm"] <- sum(grepl("logarithm_glszm", features_B))
vector_logarithm_feature_class_max_B["logarithm_ngtdm"] <- sum(grepl("logarithm_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_logarithm_feature_class_count_B <- matrix(0,6,6)
colnames(vector_logarithm_feature_class_count_B) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")
rownames(vector_logarithm_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "logarithm_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_B[,"logarithm_firstorder"] <- vector_logarithm_feature_class_count_B[,"logarithm_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "logarithm_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_B[,"logarithm_glcm"] <- vector_logarithm_feature_class_count_B[,"logarithm_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "logarithm_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_B[,"logarithm_gldm"] <- vector_logarithm_feature_class_count_B[,"logarithm_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "logarithm_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_B[,"logarithm_glrlm"] <- vector_logarithm_feature_class_count_B[,"logarithm_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "logarithm_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_B[,"logarithm_glszm"] <- vector_logarithm_feature_class_count_B[,"logarithm_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "logarithm_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_B[,"logarithm_ngtdm"] <- vector_logarithm_feature_class_count_B[,"logarithm_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_logarithm_feature_class <- sweep(vector_logarithm_feature_class_count_B, 2, 3*vector_logarithm_feature_class_max_B, FUN = '/') * 100
#perc_B_logarithm_feature_class <- vector_logarithm_feature_class_count_B / (3 * vector_logarithm_feature_class_max_B) * 100

M <- perc_B_logarithm_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_logarithm_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_logarithm_feature_class <- vector(,6)
rho_values_B_logarithm_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_logarithm_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_logarithm_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_logarithm_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_logarithm_feature_class <- rho_values_B_logarithm_feature_class**2

names(p_values_B_logarithm_feature_class) <- colnames(perc_B)

names(rsqr_values_B_logarithm_feature_class) <- colnames(perc_B)



# ## features C
vector_logarithm_feature_class_max_C <- vector(,6)
names(vector_logarithm_feature_class_max_C) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")
vector_logarithm_feature_class_max_C["logarithm_firstorder"] <- sum(grepl("logarithm_firstorder", features_C))
vector_logarithm_feature_class_max_C["logarithm_glcm"] <- sum(grepl("logarithm_glcm", features_C))
vector_logarithm_feature_class_max_C["logarithm_gldm"] <- sum(grepl("logarithm_glrlm", features_C))
vector_logarithm_feature_class_max_C["logarithm_glrlm"] <- sum(grepl("logarithm_glrlm", features_C))
vector_logarithm_feature_class_max_C["logarithm_glszm"] <- sum(grepl("logarithm_glszm", features_C))
vector_logarithm_feature_class_max_C["logarithm_ngtdm"] <- sum(grepl("logarithm_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_logarithm_feature_class_count_C <- matrix(0,6,6)
colnames(vector_logarithm_feature_class_count_C) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")
rownames(vector_logarithm_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "logarithm_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_C[,"logarithm_firstorder"] <- vector_logarithm_feature_class_count_C[,"logarithm_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "logarithm_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_C[,"logarithm_glcm"] <- vector_logarithm_feature_class_count_C[,"logarithm_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "logarithm_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_C[,"logarithm_gldm"] <- vector_logarithm_feature_class_count_C[,"logarithm_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "logarithm_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_C[,"logarithm_glrlm"] <- vector_logarithm_feature_class_count_C[,"logarithm_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "logarithm_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_C[,"logarithm_glszm"] <- vector_logarithm_feature_class_count_C[,"logarithm_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "logarithm_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_logarithm_feature_class_count_C[,"logarithm_ngtdm"] <- vector_logarithm_feature_class_count_C[,"logarithm_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_logarithm_feature_class <- sweep(vector_logarithm_feature_class_count_C, 2, 3*vector_logarithm_feature_class_max_C, FUN = '/') * 100
#perc_C_logarithm_feature_class <- vector_logarithm_feature_class_count_C / (3 * vector_logarithm_feature_class_max_C) * 100

M <- perc_C_logarithm_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_logarithm_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_logarithm_feature_class <- vector(,6)
rho_values_C_logarithm_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_logarithm_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_logarithm_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_logarithm_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_logarithm_feature_class <- rho_values_C_logarithm_feature_class**2

names(p_values_C_logarithm_feature_class) <- colnames(perc_C)

names(rsqr_values_C_logarithm_feature_class) <- colnames(perc_C)

p_values_logarithm_features_scanners <- rbind(p_values_A_logarithm_feature_class, p_values_B_logarithm_feature_class, p_values_C_logarithm_feature_class)
r_squared_logarithm_features_scanners <- rbind(rsqr_values_A_logarithm_feature_class, rsqr_values_B_logarithm_feature_class, rsqr_values_C_logarithm_feature_class)
rownames(r_squared_logarithm_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squared_logarithm_features_scanners) <- c("logarithm_firstorder", "logarithm_glcm", "logarithm_gldm", "logarithm_glrlm", "logarithm_glszm", "logarithm_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_logarithm_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_squared_logarithm_features_scanners), p.mat = t(p_values_logarithm_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_square_feature_class_max_A <- vector(,6)
names(vector_square_feature_class_max_A) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")
vector_square_feature_class_max_A["square_firstorder"] <- sum(grepl("square_firstorder", features_A))
vector_square_feature_class_max_A["square_glcm"] <- sum(grepl("square_glcm", features_A))
vector_square_feature_class_max_A["square_gldm"] <- sum(grepl("square_glrlm", features_A))
vector_square_feature_class_max_A["square_glrlm"] <- sum(grepl("square_glrlm", features_A))
vector_square_feature_class_max_A["square_glszm"] <- sum(grepl("square_glszm", features_A))
vector_square_feature_class_max_A["square_ngtdm"] <- sum(grepl("square_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_square_feature_class_count_A <- matrix(0,6,6)
colnames(vector_square_feature_class_count_A) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")
rownames(vector_square_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "square_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_A[,"square_firstorder"] <- vector_square_feature_class_count_A[,"square_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "square_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_A[,"square_glcm"] <- vector_square_feature_class_count_A[,"square_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "square_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_A[,"square_gldm"] <- vector_square_feature_class_count_A[,"square_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "square_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_A[,"square_glrlm"] <- vector_square_feature_class_count_A[,"square_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "square_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_A[,"square_glszm"] <- vector_square_feature_class_count_A[,"square_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "square_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_A[,"square_ngtdm"] <- vector_square_feature_class_count_A[,"square_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_square_feature_class <- sweep(vector_square_feature_class_count_A, 2, 3*vector_square_feature_class_max_A, FUN = '/') * 100
#perc_A_square_feature_class <- vector_square_feature_class_count_A / (3 * vector_square_feature_class_max_A) * 100

M <- perc_A_square_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_square_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_square_feature_class <- vector(,6)
rho_values_A_square_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_square_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_square_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_square_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_square_feature_class <- rho_values_A_square_feature_class**2

names(p_values_A_square_feature_class) <- colnames(perc_A)

names(rsqr_values_A_square_feature_class) <- colnames(perc_A)



# ## features B
vector_square_feature_class_max_B <- vector(,6)
names(vector_square_feature_class_max_B) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")
vector_square_feature_class_max_B["square_firstorder"] <- sum(grepl("square_firstorder", features_B))
vector_square_feature_class_max_B["square_glcm"] <- sum(grepl("square_glcm", features_B))
vector_square_feature_class_max_B["square_gldm"] <- sum(grepl("square_glrlm", features_B))
vector_square_feature_class_max_B["square_glrlm"] <- sum(grepl("square_glrlm", features_B))
vector_square_feature_class_max_B["square_glszm"] <- sum(grepl("square_glszm", features_B))
vector_square_feature_class_max_B["square_ngtdm"] <- sum(grepl("square_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_square_feature_class_count_B <- matrix(0,6,6)
colnames(vector_square_feature_class_count_B) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")
rownames(vector_square_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "square_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_B[,"square_firstorder"] <- vector_square_feature_class_count_B[,"square_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "square_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_B[,"square_glcm"] <- vector_square_feature_class_count_B[,"square_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "square_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_B[,"square_gldm"] <- vector_square_feature_class_count_B[,"square_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "square_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_B[,"square_glrlm"] <- vector_square_feature_class_count_B[,"square_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "square_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_B[,"square_glszm"] <- vector_square_feature_class_count_B[,"square_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "square_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_B[,"square_ngtdm"] <- vector_square_feature_class_count_B[,"square_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_square_feature_class <- sweep(vector_square_feature_class_count_B, 2, 3*vector_square_feature_class_max_B, FUN = '/') * 100
#perc_B_square_feature_class <- vector_square_feature_class_count_B / (3 * vector_square_feature_class_max_B) * 100

M <- perc_B_square_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_square_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_square_feature_class <- vector(,6)
rho_values_B_square_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_square_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_square_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_square_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_square_feature_class <- rho_values_B_square_feature_class**2

names(p_values_B_square_feature_class) <- colnames(perc_B)

names(rsqr_values_B_square_feature_class) <- colnames(perc_B)



# ## features C
vector_square_feature_class_max_C <- vector(,6)
names(vector_square_feature_class_max_C) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")
vector_square_feature_class_max_C["square_firstorder"] <- sum(grepl("square_firstorder", features_C))
vector_square_feature_class_max_C["square_glcm"] <- sum(grepl("square_glcm", features_C))
vector_square_feature_class_max_C["square_gldm"] <- sum(grepl("square_glrlm", features_C))
vector_square_feature_class_max_C["square_glrlm"] <- sum(grepl("square_glrlm", features_C))
vector_square_feature_class_max_C["square_glszm"] <- sum(grepl("square_glszm", features_C))
vector_square_feature_class_max_C["square_ngtdm"] <- sum(grepl("square_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_square_feature_class_count_C <- matrix(0,6,6)
colnames(vector_square_feature_class_count_C) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")
rownames(vector_square_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "square_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_C[,"square_firstorder"] <- vector_square_feature_class_count_C[,"square_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "square_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_C[,"square_glcm"] <- vector_square_feature_class_count_C[,"square_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "square_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_C[,"square_gldm"] <- vector_square_feature_class_count_C[,"square_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "square_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_C[,"square_glrlm"] <- vector_square_feature_class_count_C[,"square_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "square_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_C[,"square_glszm"] <- vector_square_feature_class_count_C[,"square_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "square_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_square_feature_class_count_C[,"square_ngtdm"] <- vector_square_feature_class_count_C[,"square_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_square_feature_class <- sweep(vector_square_feature_class_count_C, 2, 3*vector_square_feature_class_max_C, FUN = '/') * 100
#perc_C_square_feature_class <- vector_square_feature_class_count_C / (3 * vector_square_feature_class_max_C) * 100

M <- perc_C_square_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_square_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_square_feature_class <- vector(,6)
rho_values_C_square_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_square_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_square_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_square_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_square_feature_class <- rho_values_C_square_feature_class**2

names(p_values_C_square_feature_class) <- colnames(perc_C)

names(rsqr_values_C_square_feature_class) <- colnames(perc_C)

p_values_square_features_scanners <- rbind(p_values_A_square_feature_class, p_values_B_square_feature_class, p_values_C_square_feature_class)
r_squared_square_features_scanners <- rbind(rsqr_values_A_square_feature_class, rsqr_values_B_square_feature_class, rsqr_values_C_square_feature_class)
rownames(r_squared_square_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squared_square_features_scanners) <- c("square_firstorder", "square_glcm", "square_gldm", "square_glrlm", "square_glszm", "square_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_square_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_squared_square_features_scanners), p.mat = t(p_values_square_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_squareroot_feature_class_max_A <- vector(,6)
names(vector_squareroot_feature_class_max_A) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")
vector_squareroot_feature_class_max_A["squareroot_firstorder"] <- sum(grepl("squareroot_firstorder", features_A))
vector_squareroot_feature_class_max_A["squareroot_glcm"] <- sum(grepl("squareroot_glcm", features_A))
vector_squareroot_feature_class_max_A["squareroot_gldm"] <- sum(grepl("squareroot_glrlm", features_A))
vector_squareroot_feature_class_max_A["squareroot_glrlm"] <- sum(grepl("squareroot_glrlm", features_A))
vector_squareroot_feature_class_max_A["squareroot_glszm"] <- sum(grepl("squareroot_glszm", features_A))
vector_squareroot_feature_class_max_A["squareroot_ngtdm"] <- sum(grepl("squareroot_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_squareroot_feature_class_count_A <- matrix(0,6,6)
colnames(vector_squareroot_feature_class_count_A) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")
rownames(vector_squareroot_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "squareroot_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_A[,"squareroot_firstorder"] <- vector_squareroot_feature_class_count_A[,"squareroot_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "squareroot_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_A[,"squareroot_glcm"] <- vector_squareroot_feature_class_count_A[,"squareroot_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "squareroot_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_A[,"squareroot_gldm"] <- vector_squareroot_feature_class_count_A[,"squareroot_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "squareroot_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_A[,"squareroot_glrlm"] <- vector_squareroot_feature_class_count_A[,"squareroot_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "squareroot_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_A[,"squareroot_glszm"] <- vector_squareroot_feature_class_count_A[,"squareroot_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "squareroot_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_A[,"squareroot_ngtdm"] <- vector_squareroot_feature_class_count_A[,"squareroot_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_squareroot_feature_class <- sweep(vector_squareroot_feature_class_count_A, 2, 3*vector_squareroot_feature_class_max_A, FUN = '/') * 100
#perc_A_squareroot_feature_class <- vector_squareroot_feature_class_count_A / (3 * vector_squareroot_feature_class_max_A) * 100

M <- perc_A_squareroot_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_squareroot_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_squareroot_feature_class <- vector(,6)
rho_values_A_squareroot_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_squareroot_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_squareroot_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_squareroot_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_squareroot_feature_class <- rho_values_A_squareroot_feature_class**2

names(p_values_A_squareroot_feature_class) <- colnames(perc_A)

names(rsqr_values_A_squareroot_feature_class) <- colnames(perc_A)



# ## features B
vector_squareroot_feature_class_max_B <- vector(,6)
names(vector_squareroot_feature_class_max_B) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")
vector_squareroot_feature_class_max_B["squareroot_firstorder"] <- sum(grepl("squareroot_firstorder", features_B))
vector_squareroot_feature_class_max_B["squareroot_glcm"] <- sum(grepl("squareroot_glcm", features_B))
vector_squareroot_feature_class_max_B["squareroot_gldm"] <- sum(grepl("squareroot_glrlm", features_B))
vector_squareroot_feature_class_max_B["squareroot_glrlm"] <- sum(grepl("squareroot_glrlm", features_B))
vector_squareroot_feature_class_max_B["squareroot_glszm"] <- sum(grepl("squareroot_glszm", features_B))
vector_squareroot_feature_class_max_B["squareroot_ngtdm"] <- sum(grepl("squareroot_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_squareroot_feature_class_count_B <- matrix(0,6,6)
colnames(vector_squareroot_feature_class_count_B) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")
rownames(vector_squareroot_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "squareroot_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_B[,"squareroot_firstorder"] <- vector_squareroot_feature_class_count_B[,"squareroot_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "squareroot_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_B[,"squareroot_glcm"] <- vector_squareroot_feature_class_count_B[,"squareroot_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "squareroot_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_B[,"squareroot_gldm"] <- vector_squareroot_feature_class_count_B[,"squareroot_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "squareroot_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_B[,"squareroot_glrlm"] <- vector_squareroot_feature_class_count_B[,"squareroot_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "squareroot_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_B[,"squareroot_glszm"] <- vector_squareroot_feature_class_count_B[,"squareroot_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "squareroot_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_B[,"squareroot_ngtdm"] <- vector_squareroot_feature_class_count_B[,"squareroot_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_squareroot_feature_class <- sweep(vector_squareroot_feature_class_count_B, 2, 3*vector_squareroot_feature_class_max_B, FUN = '/') * 100
#perc_B_squareroot_feature_class <- vector_squareroot_feature_class_count_B / (3 * vector_squareroot_feature_class_max_B) * 100

M <- perc_B_squareroot_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_squareroot_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_squareroot_feature_class <- vector(,6)
rho_values_B_squareroot_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_squareroot_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_squareroot_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_squareroot_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_squareroot_feature_class <- rho_values_B_squareroot_feature_class**2

names(p_values_B_squareroot_feature_class) <- colnames(perc_B)

names(rsqr_values_B_squareroot_feature_class) <- colnames(perc_B)



# ## features C
vector_squareroot_feature_class_max_C <- vector(,6)
names(vector_squareroot_feature_class_max_C) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")
vector_squareroot_feature_class_max_C["squareroot_firstorder"] <- sum(grepl("squareroot_firstorder", features_C))
vector_squareroot_feature_class_max_C["squareroot_glcm"] <- sum(grepl("squareroot_glcm", features_C))
vector_squareroot_feature_class_max_C["squareroot_gldm"] <- sum(grepl("squareroot_glrlm", features_C))
vector_squareroot_feature_class_max_C["squareroot_glrlm"] <- sum(grepl("squareroot_glrlm", features_C))
vector_squareroot_feature_class_max_C["squareroot_glszm"] <- sum(grepl("squareroot_glszm", features_C))
vector_squareroot_feature_class_max_C["squareroot_ngtdm"] <- sum(grepl("squareroot_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_squareroot_feature_class_count_C <- matrix(0,6,6)
colnames(vector_squareroot_feature_class_count_C) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")
rownames(vector_squareroot_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "squareroot_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_C[,"squareroot_firstorder"] <- vector_squareroot_feature_class_count_C[,"squareroot_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "squareroot_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_C[,"squareroot_glcm"] <- vector_squareroot_feature_class_count_C[,"squareroot_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "squareroot_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_C[,"squareroot_gldm"] <- vector_squareroot_feature_class_count_C[,"squareroot_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "squareroot_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_C[,"squareroot_glrlm"] <- vector_squareroot_feature_class_count_C[,"squareroot_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "squareroot_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_C[,"squareroot_glszm"] <- vector_squareroot_feature_class_count_C[,"squareroot_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "squareroot_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_squareroot_feature_class_count_C[,"squareroot_ngtdm"] <- vector_squareroot_feature_class_count_C[,"squareroot_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_squareroot_feature_class <- sweep(vector_squareroot_feature_class_count_C, 2, 3*vector_squareroot_feature_class_max_C, FUN = '/') * 100
#perc_C_squareroot_feature_class <- vector_squareroot_feature_class_count_C / (3 * vector_squareroot_feature_class_max_C) * 100

M <- perc_C_squareroot_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_squareroot_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_squareroot_feature_class <- vector(,6)
rho_values_C_squareroot_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_squareroot_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_squareroot_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_squareroot_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_squareroot_feature_class <- rho_values_C_squareroot_feature_class**2

names(p_values_C_squareroot_feature_class) <- colnames(perc_C)

names(rsqr_values_C_squareroot_feature_class) <- colnames(perc_C)

p_values_squareroot_features_scanners <- rbind(p_values_A_squareroot_feature_class, p_values_B_squareroot_feature_class, p_values_C_squareroot_feature_class)
r_squarerootd_squareroot_features_scanners <- rbind(rsqr_values_A_squareroot_feature_class, rsqr_values_B_squareroot_feature_class, rsqr_values_C_squareroot_feature_class)
rownames(r_squarerootd_squareroot_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_squarerootd_squareroot_features_scanners) <- c("squareroot_firstorder", "squareroot_glcm", "squareroot_gldm", "squareroot_glrlm", "squareroot_glszm", "squareroot_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_squareroot_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_squarerootd_squareroot_features_scanners), p.mat = t(p_values_squareroot_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_wavelet.LL_feature_class_max_A <- vector(,6)
names(vector_wavelet.LL_feature_class_max_A) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")
vector_wavelet.LL_feature_class_max_A["wavelet.LL_firstorder"] <- sum(grepl("wavelet.LL_firstorder", features_A))
vector_wavelet.LL_feature_class_max_A["wavelet.LL_glcm"] <- sum(grepl("wavelet.LL_glcm", features_A))
vector_wavelet.LL_feature_class_max_A["wavelet.LL_gldm"] <- sum(grepl("wavelet.LL_glrlm", features_A))
vector_wavelet.LL_feature_class_max_A["wavelet.LL_glrlm"] <- sum(grepl("wavelet.LL_glrlm", features_A))
vector_wavelet.LL_feature_class_max_A["wavelet.LL_glszm"] <- sum(grepl("wavelet.LL_glszm", features_A))
vector_wavelet.LL_feature_class_max_A["wavelet.LL_ngtdm"] <- sum(grepl("wavelet.LL_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_wavelet.LL_feature_class_count_A <- matrix(0,6,6)
colnames(vector_wavelet.LL_feature_class_count_A) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")
rownames(vector_wavelet.LL_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "wavelet.LL_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_firstorder"] <- vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LL_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_glcm"] <- vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LL_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_gldm"] <- vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LL_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_glrlm"] <- vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LL_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_glszm"] <- vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LL_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_ngtdm"] <- vector_wavelet.LL_feature_class_count_A[,"wavelet.LL_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_wavelet.LL_feature_class <- sweep(vector_wavelet.LL_feature_class_count_A, 2, 3*vector_wavelet.LL_feature_class_max_A, FUN = '/') * 100
#perc_A_wavelet.LL_feature_class <- vector_wavelet.LL_feature_class_count_A / (3 * vector_wavelet.LL_feature_class_max_A) * 100

M <- perc_A_wavelet.LL_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_wavelet.LL_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_wavelet.LL_feature_class <- vector(,6)
rho_values_A_wavelet.LL_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_wavelet.LL_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_wavelet.LL_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_wavelet.LL_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_wavelet.LL_feature_class <- rho_values_A_wavelet.LL_feature_class**2

names(p_values_A_wavelet.LL_feature_class) <- colnames(perc_A)

names(rsqr_values_A_wavelet.LL_feature_class) <- colnames(perc_A)



# ## features B
vector_wavelet.LL_feature_class_max_B <- vector(,6)
names(vector_wavelet.LL_feature_class_max_B) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")
vector_wavelet.LL_feature_class_max_B["wavelet.LL_firstorder"] <- sum(grepl("wavelet.LL_firstorder", features_B))
vector_wavelet.LL_feature_class_max_B["wavelet.LL_glcm"] <- sum(grepl("wavelet.LL_glcm", features_B))
vector_wavelet.LL_feature_class_max_B["wavelet.LL_gldm"] <- sum(grepl("wavelet.LL_glrlm", features_B))
vector_wavelet.LL_feature_class_max_B["wavelet.LL_glrlm"] <- sum(grepl("wavelet.LL_glrlm", features_B))
vector_wavelet.LL_feature_class_max_B["wavelet.LL_glszm"] <- sum(grepl("wavelet.LL_glszm", features_B))
vector_wavelet.LL_feature_class_max_B["wavelet.LL_ngtdm"] <- sum(grepl("wavelet.LL_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_wavelet.LL_feature_class_count_B <- matrix(0,6,6)
colnames(vector_wavelet.LL_feature_class_count_B) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")
rownames(vector_wavelet.LL_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "wavelet.LL_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_firstorder"] <- vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LL_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_glcm"] <- vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LL_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_gldm"] <- vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LL_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_glrlm"] <- vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LL_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_glszm"] <- vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LL_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_ngtdm"] <- vector_wavelet.LL_feature_class_count_B[,"wavelet.LL_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_wavelet.LL_feature_class <- sweep(vector_wavelet.LL_feature_class_count_B, 2, 3*vector_wavelet.LL_feature_class_max_B, FUN = '/') * 100
#perc_B_wavelet.LL_feature_class <- vector_wavelet.LL_feature_class_count_B / (3 * vector_wavelet.LL_feature_class_max_B) * 100

M <- perc_B_wavelet.LL_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_wavelet.LL_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_wavelet.LL_feature_class <- vector(,6)
rho_values_B_wavelet.LL_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_wavelet.LL_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_wavelet.LL_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_wavelet.LL_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_wavelet.LL_feature_class <- rho_values_B_wavelet.LL_feature_class**2

names(p_values_B_wavelet.LL_feature_class) <- colnames(perc_B)

names(rsqr_values_B_wavelet.LL_feature_class) <- colnames(perc_B)



# ## features C
vector_wavelet.LL_feature_class_max_C <- vector(,6)
names(vector_wavelet.LL_feature_class_max_C) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")
vector_wavelet.LL_feature_class_max_C["wavelet.LL_firstorder"] <- sum(grepl("wavelet.LL_firstorder", features_C))
vector_wavelet.LL_feature_class_max_C["wavelet.LL_glcm"] <- sum(grepl("wavelet.LL_glcm", features_C))
vector_wavelet.LL_feature_class_max_C["wavelet.LL_gldm"] <- sum(grepl("wavelet.LL_glrlm", features_C))
vector_wavelet.LL_feature_class_max_C["wavelet.LL_glrlm"] <- sum(grepl("wavelet.LL_glrlm", features_C))
vector_wavelet.LL_feature_class_max_C["wavelet.LL_glszm"] <- sum(grepl("wavelet.LL_glszm", features_C))
vector_wavelet.LL_feature_class_max_C["wavelet.LL_ngtdm"] <- sum(grepl("wavelet.LL_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_wavelet.LL_feature_class_count_C <- matrix(0,6,6)
colnames(vector_wavelet.LL_feature_class_count_C) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")
rownames(vector_wavelet.LL_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "wavelet.LL_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_firstorder"] <- vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LL_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_glcm"] <- vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LL_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_gldm"] <- vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LL_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_glrlm"] <- vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LL_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_glszm"] <- vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LL_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_ngtdm"] <- vector_wavelet.LL_feature_class_count_C[,"wavelet.LL_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_wavelet.LL_feature_class <- sweep(vector_wavelet.LL_feature_class_count_C, 2, 3*vector_wavelet.LL_feature_class_max_C, FUN = '/') * 100
#perc_C_wavelet.LL_feature_class <- vector_wavelet.LL_feature_class_count_C / (3 * vector_wavelet.LL_feature_class_max_C) * 100

M <- perc_C_wavelet.LL_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_wavelet.LL_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_wavelet.LL_feature_class <- vector(,6)
rho_values_C_wavelet.LL_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_wavelet.LL_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_wavelet.LL_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_wavelet.LL_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_wavelet.LL_feature_class <- rho_values_C_wavelet.LL_feature_class**2

names(p_values_C_wavelet.LL_feature_class) <- colnames(perc_C)

names(rsqr_values_C_wavelet.LL_feature_class) <- colnames(perc_C)

p_values_wavelet.LL_features_scanners <- rbind(p_values_A_wavelet.LL_feature_class, p_values_B_wavelet.LL_feature_class, p_values_C_wavelet.LL_feature_class)
r_wavelet.LLd_wavelet.LL_features_scanners <- rbind(rsqr_values_A_wavelet.LL_feature_class, rsqr_values_B_wavelet.LL_feature_class, rsqr_values_C_wavelet.LL_feature_class)
rownames(r_wavelet.LLd_wavelet.LL_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_wavelet.LLd_wavelet.LL_features_scanners) <- c("wavelet.LL_firstorder", "wavelet.LL_glcm", "wavelet.LL_gldm", "wavelet.LL_glrlm", "wavelet.LL_glszm", "wavelet.LL_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_wavelet.LL_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_wavelet.LLd_wavelet.LL_features_scanners), p.mat = t(p_values_wavelet.LL_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_wavelet.LH_feature_class_max_A <- vector(,6)
names(vector_wavelet.LH_feature_class_max_A) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")
vector_wavelet.LH_feature_class_max_A["wavelet.LH_firstorder"] <- sum(grepl("wavelet.LH_firstorder", features_A))
vector_wavelet.LH_feature_class_max_A["wavelet.LH_glcm"] <- sum(grepl("wavelet.LH_glcm", features_A))
vector_wavelet.LH_feature_class_max_A["wavelet.LH_gldm"] <- sum(grepl("wavelet.LH_glrlm", features_A))
vector_wavelet.LH_feature_class_max_A["wavelet.LH_glrlm"] <- sum(grepl("wavelet.LH_glrlm", features_A))
vector_wavelet.LH_feature_class_max_A["wavelet.LH_glszm"] <- sum(grepl("wavelet.LH_glszm", features_A))
vector_wavelet.LH_feature_class_max_A["wavelet.LH_ngtdm"] <- sum(grepl("wavelet.LH_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_wavelet.LH_feature_class_count_A <- matrix(0,6,6)
colnames(vector_wavelet.LH_feature_class_count_A) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")
rownames(vector_wavelet.LH_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "wavelet.LH_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_firstorder"] <- vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LH_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_glcm"] <- vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LH_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_gldm"] <- vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LH_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_glrlm"] <- vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LH_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_glszm"] <- vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.LH_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_ngtdm"] <- vector_wavelet.LH_feature_class_count_A[,"wavelet.LH_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_wavelet.LH_feature_class <- sweep(vector_wavelet.LH_feature_class_count_A, 2, 3*vector_wavelet.LH_feature_class_max_A, FUN = '/') * 100
#perc_A_wavelet.LH_feature_class <- vector_wavelet.LH_feature_class_count_A / (3 * vector_wavelet.LH_feature_class_max_A) * 100

M <- perc_A_wavelet.LH_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_wavelet.LH_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_wavelet.LH_feature_class <- vector(,6)
rho_values_A_wavelet.LH_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_wavelet.LH_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_wavelet.LH_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_wavelet.LH_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_wavelet.LH_feature_class <- rho_values_A_wavelet.LH_feature_class**2

names(p_values_A_wavelet.LH_feature_class) <- colnames(perc_A)

names(rsqr_values_A_wavelet.LH_feature_class) <- colnames(perc_A)



# ## features B
vector_wavelet.LH_feature_class_max_B <- vector(,6)
names(vector_wavelet.LH_feature_class_max_B) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")
vector_wavelet.LH_feature_class_max_B["wavelet.LH_firstorder"] <- sum(grepl("wavelet.LH_firstorder", features_B))
vector_wavelet.LH_feature_class_max_B["wavelet.LH_glcm"] <- sum(grepl("wavelet.LH_glcm", features_B))
vector_wavelet.LH_feature_class_max_B["wavelet.LH_gldm"] <- sum(grepl("wavelet.LH_glrlm", features_B))
vector_wavelet.LH_feature_class_max_B["wavelet.LH_glrlm"] <- sum(grepl("wavelet.LH_glrlm", features_B))
vector_wavelet.LH_feature_class_max_B["wavelet.LH_glszm"] <- sum(grepl("wavelet.LH_glszm", features_B))
vector_wavelet.LH_feature_class_max_B["wavelet.LH_ngtdm"] <- sum(grepl("wavelet.LH_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_wavelet.LH_feature_class_count_B <- matrix(0,6,6)
colnames(vector_wavelet.LH_feature_class_count_B) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")
rownames(vector_wavelet.LH_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "wavelet.LH_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_firstorder"] <- vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LH_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_glcm"] <- vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LH_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_gldm"] <- vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LH_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_glrlm"] <- vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LH_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_glszm"] <- vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.LH_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_ngtdm"] <- vector_wavelet.LH_feature_class_count_B[,"wavelet.LH_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_wavelet.LH_feature_class <- sweep(vector_wavelet.LH_feature_class_count_B, 2, 3*vector_wavelet.LH_feature_class_max_B, FUN = '/') * 100
#perc_B_wavelet.LH_feature_class <- vector_wavelet.LH_feature_class_count_B / (3 * vector_wavelet.LH_feature_class_max_B) * 100

M <- perc_B_wavelet.LH_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_wavelet.LH_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_wavelet.LH_feature_class <- vector(,6)
rho_values_B_wavelet.LH_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_wavelet.LH_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_wavelet.LH_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_wavelet.LH_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_wavelet.LH_feature_class <- rho_values_B_wavelet.LH_feature_class**2

names(p_values_B_wavelet.LH_feature_class) <- colnames(perc_B)

names(rsqr_values_B_wavelet.LH_feature_class) <- colnames(perc_B)



# ## features C
vector_wavelet.LH_feature_class_max_C <- vector(,6)
names(vector_wavelet.LH_feature_class_max_C) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")
vector_wavelet.LH_feature_class_max_C["wavelet.LH_firstorder"] <- sum(grepl("wavelet.LH_firstorder", features_C))
vector_wavelet.LH_feature_class_max_C["wavelet.LH_glcm"] <- sum(grepl("wavelet.LH_glcm", features_C))
vector_wavelet.LH_feature_class_max_C["wavelet.LH_gldm"] <- sum(grepl("wavelet.LH_glrlm", features_C))
vector_wavelet.LH_feature_class_max_C["wavelet.LH_glrlm"] <- sum(grepl("wavelet.LH_glrlm", features_C))
vector_wavelet.LH_feature_class_max_C["wavelet.LH_glszm"] <- sum(grepl("wavelet.LH_glszm", features_C))
vector_wavelet.LH_feature_class_max_C["wavelet.LH_ngtdm"] <- sum(grepl("wavelet.LH_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_wavelet.LH_feature_class_count_C <- matrix(0,6,6)
colnames(vector_wavelet.LH_feature_class_count_C) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")
rownames(vector_wavelet.LH_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "wavelet.LH_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_firstorder"] <- vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LH_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_glcm"] <- vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LH_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_gldm"] <- vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LH_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_glrlm"] <- vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LH_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_glszm"] <- vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.LH_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_ngtdm"] <- vector_wavelet.LH_feature_class_count_C[,"wavelet.LH_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_wavelet.LH_feature_class <- sweep(vector_wavelet.LH_feature_class_count_C, 2, 3*vector_wavelet.LH_feature_class_max_C, FUN = '/') * 100
#perc_C_wavelet.LH_feature_class <- vector_wavelet.LH_feature_class_count_C / (3 * vector_wavelet.LH_feature_class_max_C) * 100

M <- perc_C_wavelet.LH_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_wavelet.LH_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_wavelet.LH_feature_class <- vector(,6)
rho_values_C_wavelet.LH_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_wavelet.LH_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_wavelet.LH_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_wavelet.LH_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_wavelet.LH_feature_class <- rho_values_C_wavelet.LH_feature_class**2

names(p_values_C_wavelet.LH_feature_class) <- colnames(perc_C)

names(rsqr_values_C_wavelet.LH_feature_class) <- colnames(perc_C)

p_values_wavelet.LH_features_scanners <- rbind(p_values_A_wavelet.LH_feature_class, p_values_B_wavelet.LH_feature_class, p_values_C_wavelet.LH_feature_class)
r_wavelet.LHd_wavelet.LH_features_scanners <- rbind(rsqr_values_A_wavelet.LH_feature_class, rsqr_values_B_wavelet.LH_feature_class, rsqr_values_C_wavelet.LH_feature_class)
rownames(r_wavelet.LHd_wavelet.LH_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_wavelet.LHd_wavelet.LH_features_scanners) <- c("wavelet.LH_firstorder", "wavelet.LH_glcm", "wavelet.LH_gldm", "wavelet.LH_glrlm", "wavelet.LH_glszm", "wavelet.LH_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_wavelet.LH_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_wavelet.LHd_wavelet.LH_features_scanners), p.mat = t(p_values_wavelet.LH_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_wavelet.HL_feature_class_max_A <- vector(,6)
names(vector_wavelet.HL_feature_class_max_A) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")
vector_wavelet.HL_feature_class_max_A["wavelet.HL_firstorder"] <- sum(grepl("wavelet.HL_firstorder", features_A))
vector_wavelet.HL_feature_class_max_A["wavelet.HL_glcm"] <- sum(grepl("wavelet.HL_glcm", features_A))
vector_wavelet.HL_feature_class_max_A["wavelet.HL_gldm"] <- sum(grepl("wavelet.HL_glrlm", features_A))
vector_wavelet.HL_feature_class_max_A["wavelet.HL_glrlm"] <- sum(grepl("wavelet.HL_glrlm", features_A))
vector_wavelet.HL_feature_class_max_A["wavelet.HL_glszm"] <- sum(grepl("wavelet.HL_glszm", features_A))
vector_wavelet.HL_feature_class_max_A["wavelet.HL_ngtdm"] <- sum(grepl("wavelet.HL_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_wavelet.HL_feature_class_count_A <- matrix(0,6,6)
colnames(vector_wavelet.HL_feature_class_count_A) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")
rownames(vector_wavelet.HL_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "wavelet.HL_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_firstorder"] <- vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HL_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_glcm"] <- vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HL_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_gldm"] <- vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HL_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_glrlm"] <- vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HL_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_glszm"] <- vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HL_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_ngtdm"] <- vector_wavelet.HL_feature_class_count_A[,"wavelet.HL_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_wavelet.HL_feature_class <- sweep(vector_wavelet.HL_feature_class_count_A, 2, 3*vector_wavelet.HL_feature_class_max_A, FUN = '/') * 100
#perc_A_wavelet.HL_feature_class <- vector_wavelet.HL_feature_class_count_A / (3 * vector_wavelet.HL_feature_class_max_A) * 100

M <- perc_A_wavelet.HL_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_wavelet.HL_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_wavelet.HL_feature_class <- vector(,6)
rho_values_A_wavelet.HL_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_wavelet.HL_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_wavelet.HL_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_wavelet.HL_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_wavelet.HL_feature_class <- rho_values_A_wavelet.HL_feature_class**2

names(p_values_A_wavelet.HL_feature_class) <- colnames(perc_A)

names(rsqr_values_A_wavelet.HL_feature_class) <- colnames(perc_A)



# ## features B
vector_wavelet.HL_feature_class_max_B <- vector(,6)
names(vector_wavelet.HL_feature_class_max_B) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")
vector_wavelet.HL_feature_class_max_B["wavelet.HL_firstorder"] <- sum(grepl("wavelet.HL_firstorder", features_B))
vector_wavelet.HL_feature_class_max_B["wavelet.HL_glcm"] <- sum(grepl("wavelet.HL_glcm", features_B))
vector_wavelet.HL_feature_class_max_B["wavelet.HL_gldm"] <- sum(grepl("wavelet.HL_glrlm", features_B))
vector_wavelet.HL_feature_class_max_B["wavelet.HL_glrlm"] <- sum(grepl("wavelet.HL_glrlm", features_B))
vector_wavelet.HL_feature_class_max_B["wavelet.HL_glszm"] <- sum(grepl("wavelet.HL_glszm", features_B))
vector_wavelet.HL_feature_class_max_B["wavelet.HL_ngtdm"] <- sum(grepl("wavelet.HL_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_wavelet.HL_feature_class_count_B <- matrix(0,6,6)
colnames(vector_wavelet.HL_feature_class_count_B) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")
rownames(vector_wavelet.HL_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "wavelet.HL_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_firstorder"] <- vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HL_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_glcm"] <- vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HL_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_gldm"] <- vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HL_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_glrlm"] <- vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HL_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_glszm"] <- vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HL_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_ngtdm"] <- vector_wavelet.HL_feature_class_count_B[,"wavelet.HL_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_wavelet.HL_feature_class <- sweep(vector_wavelet.HL_feature_class_count_B, 2, 3*vector_wavelet.HL_feature_class_max_B, FUN = '/') * 100
#perc_B_wavelet.HL_feature_class <- vector_wavelet.HL_feature_class_count_B / (3 * vector_wavelet.HL_feature_class_max_B) * 100

M <- perc_B_wavelet.HL_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_wavelet.HL_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_wavelet.HL_feature_class <- vector(,6)
rho_values_B_wavelet.HL_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_wavelet.HL_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_wavelet.HL_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_wavelet.HL_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_wavelet.HL_feature_class <- rho_values_B_wavelet.HL_feature_class**2

names(p_values_B_wavelet.HL_feature_class) <- colnames(perc_B)

names(rsqr_values_B_wavelet.HL_feature_class) <- colnames(perc_B)



# ## features C
vector_wavelet.HL_feature_class_max_C <- vector(,6)
names(vector_wavelet.HL_feature_class_max_C) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")
vector_wavelet.HL_feature_class_max_C["wavelet.HL_firstorder"] <- sum(grepl("wavelet.HL_firstorder", features_C))
vector_wavelet.HL_feature_class_max_C["wavelet.HL_glcm"] <- sum(grepl("wavelet.HL_glcm", features_C))
vector_wavelet.HL_feature_class_max_C["wavelet.HL_gldm"] <- sum(grepl("wavelet.HL_glrlm", features_C))
vector_wavelet.HL_feature_class_max_C["wavelet.HL_glrlm"] <- sum(grepl("wavelet.HL_glrlm", features_C))
vector_wavelet.HL_feature_class_max_C["wavelet.HL_glszm"] <- sum(grepl("wavelet.HL_glszm", features_C))
vector_wavelet.HL_feature_class_max_C["wavelet.HL_ngtdm"] <- sum(grepl("wavelet.HL_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_wavelet.HL_feature_class_count_C <- matrix(0,6,6)
colnames(vector_wavelet.HL_feature_class_count_C) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")
rownames(vector_wavelet.HL_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "wavelet.HL_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_firstorder"] <- vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HL_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_glcm"] <- vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HL_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_gldm"] <- vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HL_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_glrlm"] <- vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HL_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_glszm"] <- vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HL_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_ngtdm"] <- vector_wavelet.HL_feature_class_count_C[,"wavelet.HL_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_wavelet.HL_feature_class <- sweep(vector_wavelet.HL_feature_class_count_C, 2, 3*vector_wavelet.HL_feature_class_max_C, FUN = '/') * 100
#perc_C_wavelet.HL_feature_class <- vector_wavelet.HL_feature_class_count_C / (3 * vector_wavelet.HL_feature_class_max_C) * 100

M <- perc_C_wavelet.HL_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_wavelet.HL_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_wavelet.HL_feature_class <- vector(,6)
rho_values_C_wavelet.HL_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_wavelet.HL_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_wavelet.HL_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_wavelet.HL_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_wavelet.HL_feature_class <- rho_values_C_wavelet.HL_feature_class**2

names(p_values_C_wavelet.HL_feature_class) <- colnames(perc_C)

names(rsqr_values_C_wavelet.HL_feature_class) <- colnames(perc_C)

p_values_wavelet.HL_features_scanners <- rbind(p_values_A_wavelet.HL_feature_class, p_values_B_wavelet.HL_feature_class, p_values_C_wavelet.HL_feature_class)
r_wavelet.HLd_wavelet.HL_features_scanners <- rbind(rsqr_values_A_wavelet.HL_feature_class, rsqr_values_B_wavelet.HL_feature_class, rsqr_values_C_wavelet.HL_feature_class)
rownames(r_wavelet.HLd_wavelet.HL_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_wavelet.HLd_wavelet.HL_features_scanners) <- c("wavelet.HL_firstorder", "wavelet.HL_glcm", "wavelet.HL_gldm", "wavelet.HL_glrlm", "wavelet.HL_glszm", "wavelet.HL_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_wavelet.HL_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_wavelet.HLd_wavelet.HL_features_scanners), p.mat = t(p_values_wavelet.HL_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_wavelet.HH_feature_class_max_A <- vector(,6)
names(vector_wavelet.HH_feature_class_max_A) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")
vector_wavelet.HH_feature_class_max_A["wavelet.HH_firstorder"] <- sum(grepl("wavelet.HH_firstorder", features_A))
vector_wavelet.HH_feature_class_max_A["wavelet.HH_glcm"] <- sum(grepl("wavelet.HH_glcm", features_A))
vector_wavelet.HH_feature_class_max_A["wavelet.HH_gldm"] <- sum(grepl("wavelet.HH_glrlm", features_A))
vector_wavelet.HH_feature_class_max_A["wavelet.HH_glrlm"] <- sum(grepl("wavelet.HH_glrlm", features_A))
vector_wavelet.HH_feature_class_max_A["wavelet.HH_glszm"] <- sum(grepl("wavelet.HH_glszm", features_A))
vector_wavelet.HH_feature_class_max_A["wavelet.HH_ngtdm"] <- sum(grepl("wavelet.HH_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_wavelet.HH_feature_class_count_A <- matrix(0,6,6)
colnames(vector_wavelet.HH_feature_class_count_A) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")
rownames(vector_wavelet.HH_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "wavelet.HH_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_firstorder"] <- vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HH_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_glcm"] <- vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HH_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_gldm"] <- vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HH_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_glrlm"] <- vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HH_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_glszm"] <- vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "wavelet.HH_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_ngtdm"] <- vector_wavelet.HH_feature_class_count_A[,"wavelet.HH_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_wavelet.HH_feature_class <- sweep(vector_wavelet.HH_feature_class_count_A, 2, 3*vector_wavelet.HH_feature_class_max_A, FUN = '/') * 100
#perc_A_wavelet.HH_feature_class <- vector_wavelet.HH_feature_class_count_A / (3 * vector_wavelet.HH_feature_class_max_A) * 100

M <- perc_A_wavelet.HH_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_wavelet.HH_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_wavelet.HH_feature_class <- vector(,6)
rho_values_A_wavelet.HH_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_wavelet.HH_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_wavelet.HH_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_wavelet.HH_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_wavelet.HH_feature_class <- rho_values_A_wavelet.HH_feature_class**2

names(p_values_A_wavelet.HH_feature_class) <- colnames(perc_A)

names(rsqr_values_A_wavelet.HH_feature_class) <- colnames(perc_A)



# ## features B
vector_wavelet.HH_feature_class_max_B <- vector(,6)
names(vector_wavelet.HH_feature_class_max_B) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")
vector_wavelet.HH_feature_class_max_B["wavelet.HH_firstorder"] <- sum(grepl("wavelet.HH_firstorder", features_B))
vector_wavelet.HH_feature_class_max_B["wavelet.HH_glcm"] <- sum(grepl("wavelet.HH_glcm", features_B))
vector_wavelet.HH_feature_class_max_B["wavelet.HH_gldm"] <- sum(grepl("wavelet.HH_glrlm", features_B))
vector_wavelet.HH_feature_class_max_B["wavelet.HH_glrlm"] <- sum(grepl("wavelet.HH_glrlm", features_B))
vector_wavelet.HH_feature_class_max_B["wavelet.HH_glszm"] <- sum(grepl("wavelet.HH_glszm", features_B))
vector_wavelet.HH_feature_class_max_B["wavelet.HH_ngtdm"] <- sum(grepl("wavelet.HH_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_wavelet.HH_feature_class_count_B <- matrix(0,6,6)
colnames(vector_wavelet.HH_feature_class_count_B) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")
rownames(vector_wavelet.HH_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "wavelet.HH_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_firstorder"] <- vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HH_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_glcm"] <- vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HH_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_gldm"] <- vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HH_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_glrlm"] <- vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HH_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_glszm"] <- vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "wavelet.HH_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_ngtdm"] <- vector_wavelet.HH_feature_class_count_B[,"wavelet.HH_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_wavelet.HH_feature_class <- sweep(vector_wavelet.HH_feature_class_count_B, 2, 3*vector_wavelet.HH_feature_class_max_B, FUN = '/') * 100
#perc_B_wavelet.HH_feature_class <- vector_wavelet.HH_feature_class_count_B / (3 * vector_wavelet.HH_feature_class_max_B) * 100

M <- perc_B_wavelet.HH_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_wavelet.HH_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_wavelet.HH_feature_class <- vector(,6)
rho_values_B_wavelet.HH_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_wavelet.HH_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_wavelet.HH_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_wavelet.HH_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_wavelet.HH_feature_class <- rho_values_B_wavelet.HH_feature_class**2

names(p_values_B_wavelet.HH_feature_class) <- colnames(perc_B)

names(rsqr_values_B_wavelet.HH_feature_class) <- colnames(perc_B)



# ## features C
vector_wavelet.HH_feature_class_max_C <- vector(,6)
names(vector_wavelet.HH_feature_class_max_C) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")
vector_wavelet.HH_feature_class_max_C["wavelet.HH_firstorder"] <- sum(grepl("wavelet.HH_firstorder", features_C))
vector_wavelet.HH_feature_class_max_C["wavelet.HH_glcm"] <- sum(grepl("wavelet.HH_glcm", features_C))
vector_wavelet.HH_feature_class_max_C["wavelet.HH_gldm"] <- sum(grepl("wavelet.HH_glrlm", features_C))
vector_wavelet.HH_feature_class_max_C["wavelet.HH_glrlm"] <- sum(grepl("wavelet.HH_glrlm", features_C))
vector_wavelet.HH_feature_class_max_C["wavelet.HH_glszm"] <- sum(grepl("wavelet.HH_glszm", features_C))
vector_wavelet.HH_feature_class_max_C["wavelet.HH_ngtdm"] <- sum(grepl("wavelet.HH_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_wavelet.HH_feature_class_count_C <- matrix(0,6,6)
colnames(vector_wavelet.HH_feature_class_count_C) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")
rownames(vector_wavelet.HH_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "wavelet.HH_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_firstorder"] <- vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HH_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_glcm"] <- vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HH_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_gldm"] <- vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HH_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_glrlm"] <- vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HH_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_glszm"] <- vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "wavelet.HH_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_ngtdm"] <- vector_wavelet.HH_feature_class_count_C[,"wavelet.HH_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_wavelet.HH_feature_class <- sweep(vector_wavelet.HH_feature_class_count_C, 2, 3*vector_wavelet.HH_feature_class_max_C, FUN = '/') * 100
#perc_C_wavelet.HH_feature_class <- vector_wavelet.HH_feature_class_count_C / (3 * vector_wavelet.HH_feature_class_max_C) * 100

M <- perc_C_wavelet.HH_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_wavelet.HH_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_wavelet.HH_feature_class <- vector(,6)
rho_values_C_wavelet.HH_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_wavelet.HH_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_wavelet.HH_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_wavelet.HH_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_wavelet.HH_feature_class <- rho_values_C_wavelet.HH_feature_class**2

names(p_values_C_wavelet.HH_feature_class) <- colnames(perc_C)

names(rsqr_values_C_wavelet.HH_feature_class) <- colnames(perc_C)

p_values_wavelet.HH_features_scanners <- rbind(p_values_A_wavelet.HH_feature_class, p_values_B_wavelet.HH_feature_class, p_values_C_wavelet.HH_feature_class)
r_wavelet.HHd_wavelet.HH_features_scanners <- rbind(rsqr_values_A_wavelet.HH_feature_class, rsqr_values_B_wavelet.HH_feature_class, rsqr_values_C_wavelet.HH_feature_class)
rownames(r_wavelet.HHd_wavelet.HH_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_wavelet.HHd_wavelet.HH_features_scanners) <- c("wavelet.HH_firstorder", "wavelet.HH_glcm", "wavelet.HH_gldm", "wavelet.HH_glrlm", "wavelet.HH_glszm", "wavelet.HH_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_wavelet.HH_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_wavelet.HHd_wavelet.HH_features_scanners), p.mat = t(p_values_wavelet.HH_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()


#####################################################################
# ## features A
vector_log.sigma.6.mm.3D_feature_class_max_A <- vector(,6)
names(vector_log.sigma.6.mm.3D_feature_class_max_A) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")
vector_log.sigma.6.mm.3D_feature_class_max_A["log.sigma.6.mm.3D_firstorder"] <- sum(grepl("log.sigma.6.mm.3D_firstorder", features_A))
vector_log.sigma.6.mm.3D_feature_class_max_A["log.sigma.6.mm.3D_glcm"] <- sum(grepl("log.sigma.6.mm.3D_glcm", features_A))
vector_log.sigma.6.mm.3D_feature_class_max_A["log.sigma.6.mm.3D_gldm"] <- sum(grepl("log.sigma.6.mm.3D_glrlm", features_A))
vector_log.sigma.6.mm.3D_feature_class_max_A["log.sigma.6.mm.3D_glrlm"] <- sum(grepl("log.sigma.6.mm.3D_glrlm", features_A))
vector_log.sigma.6.mm.3D_feature_class_max_A["log.sigma.6.mm.3D_glszm"] <- sum(grepl("log.sigma.6.mm.3D_glszm", features_A))
vector_log.sigma.6.mm.3D_feature_class_max_A["log.sigma.6.mm.3D_ngtdm"] <- sum(grepl("log.sigma.6.mm.3D_ngtdm", features_A))

text_disc_A <- text1_vs_text2_A + text1_vs_text3_A + text3_vs_text2_A
vector_log.sigma.6.mm.3D_feature_class_count_A <- matrix(0,6,6)
colnames(vector_log.sigma.6.mm.3D_feature_class_count_A) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")
rownames(vector_log.sigma.6.mm.3D_feature_class_count_A) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_A)))) {
  if (grepl( "log.sigma.6.mm.3D_firstorder", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_firstorder"] <- vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_firstorder"] + text_disc_A[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glcm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_glcm"] <- vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_glcm"] + text_disc_A[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_gldm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_gldm"] <- vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_gldm"] + text_disc_A[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glrlm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_glrlm"] <- vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_glrlm"] + text_disc_A[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glszm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_glszm"] <- vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_glszm"] + text_disc_A[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_ngtdm", colnames(text_disc_A)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_ngtdm"] <- vector_log.sigma.6.mm.3D_feature_class_count_A[,"log.sigma.6.mm.3D_ngtdm"] + text_disc_A[,features_class_index]
  }
}

perc_A_log.sigma.6.mm.3D_feature_class <- sweep(vector_log.sigma.6.mm.3D_feature_class_count_A, 2, 3*vector_log.sigma.6.mm.3D_feature_class_max_A, FUN = '/') * 100
#perc_A_log.sigma.6.mm.3D_feature_class <- vector_log.sigma.6.mm.3D_feature_class_count_A / (3 * vector_log.sigma.6.mm.3D_feature_class_max_A) * 100

M <- perc_A_log.sigma.6.mm.3D_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_A_log.sigma.6.mm.3D_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_A_log.sigma.6.mm.3D_feature_class <- vector(,6)
rho_values_A_log.sigma.6.mm.3D_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_A <- cor.test(t(perc_A_log.sigma.6.mm.3D_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_A_log.sigma.6.mm.3D_feature_class[indx] <- rho_p_A$p.value
  rho_values_A_log.sigma.6.mm.3D_feature_class[indx] <- rho_p_A$estimate
}

rsqr_values_A_log.sigma.6.mm.3D_feature_class <- rho_values_A_log.sigma.6.mm.3D_feature_class**2

names(p_values_A_log.sigma.6.mm.3D_feature_class) <- colnames(perc_A)

names(rsqr_values_A_log.sigma.6.mm.3D_feature_class) <- colnames(perc_A)



# ## features B
vector_log.sigma.6.mm.3D_feature_class_max_B <- vector(,6)
names(vector_log.sigma.6.mm.3D_feature_class_max_B) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")
vector_log.sigma.6.mm.3D_feature_class_max_B["log.sigma.6.mm.3D_firstorder"] <- sum(grepl("log.sigma.6.mm.3D_firstorder", features_B))
vector_log.sigma.6.mm.3D_feature_class_max_B["log.sigma.6.mm.3D_glcm"] <- sum(grepl("log.sigma.6.mm.3D_glcm", features_B))
vector_log.sigma.6.mm.3D_feature_class_max_B["log.sigma.6.mm.3D_gldm"] <- sum(grepl("log.sigma.6.mm.3D_glrlm", features_B))
vector_log.sigma.6.mm.3D_feature_class_max_B["log.sigma.6.mm.3D_glrlm"] <- sum(grepl("log.sigma.6.mm.3D_glrlm", features_B))
vector_log.sigma.6.mm.3D_feature_class_max_B["log.sigma.6.mm.3D_glszm"] <- sum(grepl("log.sigma.6.mm.3D_glszm", features_B))
vector_log.sigma.6.mm.3D_feature_class_max_B["log.sigma.6.mm.3D_ngtdm"] <- sum(grepl("log.sigma.6.mm.3D_ngtdm", features_B))

text_disc_B <- text1_vs_text2_B + text1_vs_text3_B + text3_vs_text2_B
vector_log.sigma.6.mm.3D_feature_class_count_B <- matrix(0,6,6)
colnames(vector_log.sigma.6.mm.3D_feature_class_count_B) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")
rownames(vector_log.sigma.6.mm.3D_feature_class_count_B) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_B)))) {
  if (grepl( "log.sigma.6.mm.3D_firstorder", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_firstorder"] <- vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_firstorder"] + text_disc_B[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glcm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_glcm"] <- vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_glcm"] + text_disc_B[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_gldm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_gldm"] <- vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_gldm"] + text_disc_B[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glrlm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_glrlm"] <- vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_glrlm"] + text_disc_B[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glszm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_glszm"] <- vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_glszm"] + text_disc_B[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_ngtdm", colnames(text_disc_B)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_ngtdm"] <- vector_log.sigma.6.mm.3D_feature_class_count_B[,"log.sigma.6.mm.3D_ngtdm"] + text_disc_B[,features_class_index]
  }
}

perc_B_log.sigma.6.mm.3D_feature_class <- sweep(vector_log.sigma.6.mm.3D_feature_class_count_B, 2, 3*vector_log.sigma.6.mm.3D_feature_class_max_B, FUN = '/') * 100
#perc_B_log.sigma.6.mm.3D_feature_class <- vector_log.sigma.6.mm.3D_feature_class_count_B / (3 * vector_log.sigma.6.mm.3D_feature_class_max_B) * 100

M <- perc_B_log.sigma.6.mm.3D_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_B_log.sigma.6.mm.3D_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_B_log.sigma.6.mm.3D_feature_class <- vector(,6)
rho_values_B_log.sigma.6.mm.3D_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_B <- cor.test(t(perc_B_log.sigma.6.mm.3D_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_B_log.sigma.6.mm.3D_feature_class[indx] <- rho_p_B$p.value
  rho_values_B_log.sigma.6.mm.3D_feature_class[indx] <- rho_p_B$estimate
}

rsqr_values_B_log.sigma.6.mm.3D_feature_class <- rho_values_B_log.sigma.6.mm.3D_feature_class**2

names(p_values_B_log.sigma.6.mm.3D_feature_class) <- colnames(perc_B)

names(rsqr_values_B_log.sigma.6.mm.3D_feature_class) <- colnames(perc_B)



# ## features C
vector_log.sigma.6.mm.3D_feature_class_max_C <- vector(,6)
names(vector_log.sigma.6.mm.3D_feature_class_max_C) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")
vector_log.sigma.6.mm.3D_feature_class_max_C["log.sigma.6.mm.3D_firstorder"] <- sum(grepl("log.sigma.6.mm.3D_firstorder", features_C))
vector_log.sigma.6.mm.3D_feature_class_max_C["log.sigma.6.mm.3D_glcm"] <- sum(grepl("log.sigma.6.mm.3D_glcm", features_C))
vector_log.sigma.6.mm.3D_feature_class_max_C["log.sigma.6.mm.3D_gldm"] <- sum(grepl("log.sigma.6.mm.3D_glrlm", features_C))
vector_log.sigma.6.mm.3D_feature_class_max_C["log.sigma.6.mm.3D_glrlm"] <- sum(grepl("log.sigma.6.mm.3D_glrlm", features_C))
vector_log.sigma.6.mm.3D_feature_class_max_C["log.sigma.6.mm.3D_glszm"] <- sum(grepl("log.sigma.6.mm.3D_glszm", features_C))
vector_log.sigma.6.mm.3D_feature_class_max_C["log.sigma.6.mm.3D_ngtdm"] <- sum(grepl("log.sigma.6.mm.3D_ngtdm", features_C))

text_disc_C <- text1_vs_text2_C + text1_vs_text3_C + text3_vs_text2_C
vector_log.sigma.6.mm.3D_feature_class_count_C <- matrix(0,6,6)
colnames(vector_log.sigma.6.mm.3D_feature_class_count_C) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")
rownames(vector_log.sigma.6.mm.3D_feature_class_count_C) <- volume_sizes
for (features_class_index in seq(length(colnames(text_disc_C)))) {
  if (grepl( "log.sigma.6.mm.3D_firstorder", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_firstorder"] <- vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_firstorder"] + text_disc_C[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glcm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_glcm"] <- vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_glcm"] + text_disc_C[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_gldm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_gldm"] <- vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_gldm"] + text_disc_C[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glrlm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_glrlm"] <- vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_glrlm"] + text_disc_C[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_glszm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_glszm"] <- vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_glszm"] + text_disc_C[,features_class_index]
  } else if (grepl( "log.sigma.6.mm.3D_ngtdm", colnames(text_disc_C)[features_class_index], fixed = TRUE)) {
    vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_ngtdm"] <- vector_log.sigma.6.mm.3D_feature_class_count_C[,"log.sigma.6.mm.3D_ngtdm"] + text_disc_C[,features_class_index]
  }
}

perc_C_log.sigma.6.mm.3D_feature_class <- sweep(vector_log.sigma.6.mm.3D_feature_class_count_C, 2, 3*vector_log.sigma.6.mm.3D_feature_class_max_C, FUN = '/') * 100
#perc_C_log.sigma.6.mm.3D_feature_class <- vector_log.sigma.6.mm.3D_feature_class_count_C / (3 * vector_log.sigma.6.mm.3D_feature_class_max_C) * 100

M <- perc_C_log.sigma.6.mm.3D_feature_class
df_ggplot <- reshape2::melt(t(M))
colnames(df_ggplot) <- c('Filter', 'Volume', 'Percentage')
df_ggplot$Volume <- as.factor(df_ggplot$Volume)
ggplot(df_ggplot, aes(x = Volume, y = Filter, fill = Percentage, label=round(Percentage,0))) +
  geom_tile() +
  labs(x = NULL, y = NULL, fill = "Percentage") +
  scale_fill_gradient2(mid="#FBFEF9",low="#0C6291",high="#A63446", limits=c(55,100), midpoint = 77.5) +
  scale_x_discrete(expand=c(0,0)) +
  scale_y_discrete(expand=c(0,0),limits=rev) +
  geom_text(size = 6.5) +
  theme_grey(base_size = 22)
ggsave(paste(dir_save_plots,"perc_C_log.sigma.6.mm.3D_feature_class.pdf",sep = ''), width = 9, height = 8, units = "in")

p_values_C_log.sigma.6.mm.3D_feature_class <- vector(,6)
rho_values_C_log.sigma.6.mm.3D_feature_class <- vector(,6)
for (indx in seq(6)) {
  rho_p_C <- cor.test(t(perc_C_log.sigma.6.mm.3D_feature_class)[,indx], as.numeric(volume_sizes), method = 'spearman')
  p_values_C_log.sigma.6.mm.3D_feature_class[indx] <- rho_p_C$p.value
  rho_values_C_log.sigma.6.mm.3D_feature_class[indx] <- rho_p_C$estimate
}

rsqr_values_C_log.sigma.6.mm.3D_feature_class <- rho_values_C_log.sigma.6.mm.3D_feature_class**2

names(p_values_C_log.sigma.6.mm.3D_feature_class) <- colnames(perc_C)

names(rsqr_values_C_log.sigma.6.mm.3D_feature_class) <- colnames(perc_C)

p_values_log.sigma.6.mm.3D_features_scanners <- rbind(p_values_A_log.sigma.6.mm.3D_feature_class, p_values_B_log.sigma.6.mm.3D_feature_class, p_values_C_log.sigma.6.mm.3D_feature_class)
r_log.sigma.6.mm.3Dd_log.sigma.6.mm.3D_features_scanners <- rbind(rsqr_values_A_log.sigma.6.mm.3D_feature_class, rsqr_values_B_log.sigma.6.mm.3D_feature_class, rsqr_values_C_log.sigma.6.mm.3D_feature_class)
rownames(r_log.sigma.6.mm.3Dd_log.sigma.6.mm.3D_features_scanners) <- c('Scanner A', 'Scanner B', 'Scanner C')
colnames(r_log.sigma.6.mm.3Dd_log.sigma.6.mm.3D_features_scanners) <- c("log.sigma.6.mm.3D_firstorder", "log.sigma.6.mm.3D_glcm", "log.sigma.6.mm.3D_gldm", "log.sigma.6.mm.3D_glrlm", "log.sigma.6.mm.3D_glszm", "log.sigma.6.mm.3D_ngtdm")

pdf(file = paste(dir_save_plots,"r_squared_log.sigma.6.mm.3D_features_scanners.pdf",sep = ''), width = 7.38, height = 7.88)
corrplot(t(r_log.sigma.6.mm.3Dd_log.sigma.6.mm.3D_features_scanners), p.mat = t(p_values_log.sigma.6.mm.3D_features_scanners), 
         sig.level = c(.001, .01, .05), insig='label_sig', pch.cex = 1.5, 
         pch.col = "yellow", method = 'color', addCoef.col ='black', 
         tl.col="black",number.cex=1.5, tl.cex=1.4,tl.srt=30)
dev.off()
  
