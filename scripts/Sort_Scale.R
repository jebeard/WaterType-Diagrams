apply_sort_scale <- function(data, units, sort_only = F, scaling = "inv_average",
                             molar_mass = c(39.0983, 18.0383, 55.845, 40.078, 22.9897, 35.453, 61.0879, 96.0626, 62.0049, 94.9714),
                             valence = c(1,1,2,2,1,1,1,2,1,3),
                             minor_names = c("Fe", "SO4", "NH4", "NO3", "NO2", "K", "PO4"),   
                             major_names = c("Ca", "HCO3", "Na", "Cl"), 
                             req_colorder = c("K", "NH4", "Fe", "Ca", "Na",  "Cl", "HCO3", "SO4", "NO3", "PO4")
                             ) {

ver <- 0.01 # Jack Beard 28/08/19 - function created to convert to mEq, perform scaling on minor ions and sort into order for plotting
ver <- 0.01 # Jack Beard 15/03/20 - added functionality to only sort, fixed errors in conversions from mg

  # select columns with observations in 
  dat_cols <- colnames(data)%in%req_colorder
  obs_data <- data[,dat_cols]
  
  # replace NAs to avoid complications
  obs_data[is.na(obs_data)] <- -1
  
  # reorder cols if needed
  reord_df <- obs_data
  for (i in 1:length(req_colorder)) {
    pos <- grepl(req_colorder[i], colnames(obs_data)) # find columns with specified names
    column <- obs_data[pos]
    reord_df[,i] <- column # replace with correct column order
  }
  colnames(reord_df) <- req_colorder
  
  if (sort_only == T){final_df <- reord_df} else {
    
    # conversion (if needed)
    if (units == "mg") {
      mg_means <- colMeans(reord_df)
      # conversion mg/l -> mEq/l
      conv <- valence/molar_mass
      data_conv <- sweep(reord_df, 2, conv, FUN="*") #data.frame(mapply(`*`, reord_df, conv))
      mEq_means <- colMeans(reord_df)
    }
    
    # return minor, major ions columns
    minor_pos <- grepl(paste(minor_names, collapse="|"), colnames(data_conv))
    minor_ions <- data_conv[minor_pos]
    
    major_pos <- grepl(paste(major_names, collapse="|"), colnames(data_conv))
    major_ions <- data_conv[major_pos]
    
    # perform scaling on minor ions columns
    if (scaling == "inv_average") {
      scale_factor_percol <- 1 / colMeans(minor_ions)
      minor_scaled <- mapply(`*`, minor_ions, scale_factor_percol)
    } else {
      minor_scaled <- mapply(`*`, minor_ions, scaling)
    }
    
    # return as dataframe
    if(is.vector(minor_scaled)){
      scaled <- cbind(data.frame(major_ions), t(data.frame(minor_scaled)))
      rownames(scaled) <- 1
    } else {
      scaled <- data.frame(major_ions, minor_scaled)
    }
    scaled_ordered <- scaled
    
    # reorder cols AGAIN
    for (i in 1:length(req_colorder)) {
      pos <- grepl(req_colorder[i], colnames(scaled)) # find columns with specified names
      column <- scaled[pos]
      scaled_ordered[[i]] <- column # replace with correct column order
    }
    
    # convert values to numeric, return as dataframe
    matrix <- apply(scaled_ordered, MARGIN = c(1,2), FUN = function(x) as.numeric(unlist(x)) )
    final_df <- data.frame(matrix)
    final_df[final_df<0] <- NA
    colnames(final_df) <- req_colorder
  }
  
  return (final_df)
}
