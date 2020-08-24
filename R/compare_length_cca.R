#' Compare length difference between inside and outside CCA
#'
#' @param dir
#' @param data
#' @param file
#' @param xlim
#' @param ylim
#'
#' @return plots
#'
#' @author Chantel Wetzel
#' @export
#'
compare_length_cca <- function(dir, data, file = "hkl_cca_comparison", xlim = NULL, ylim = NULL){

  sub_data = data[data$Source == "nwfsc_hkl",] 
  if (dim(sub_data)[1] == 0) { stop ("No hook & line data in the data frame.")}


  if(is.null(xlim)) { xlim = c(min(sub_data[,"Length"], na.rm = TRUE), max(sub_data[,"Length"], na.rm = TRUE)) }
  if(is.null(ylim)) { ylim = c(0, 0.25) }


  pngfun(wd = file.path(dir, "plots"), file = paste0(file, '.png'), w = 7, h = 7, pt = 12)

  par(mfcol = c(9, 2),
      mar = c(0.2,0.2,0.2,0.2),
      oma = c(4,4,1,1))

  years <- sort(unique(sub_data$Year))
  colvec <- c(rgb(1, 0, 0, alpha = 0.8), rgb(0, 0, 1, alpha = 0.5))

  # empty plot for legend
  plot(0, type = 'n', axes = FALSE)
  legend('left',
         bty = 'n',
         fill = colvec,
         cex = 1.5,
         legend = c("Lengths outside the CCA",
                    "Lengths inside the CCA"))

  mtext("Length (cm)", side = 1, line = 2.5, outer = TRUE)

  for (y in years) 
  {
    # make empty plot (note: xlim and ylim were set by trial and error)
    plot(0, type = 'n',
         xlim = xlim,
         xaxs = 'i',
         ylim = ylim,
         yaxs = 'i',
         axes = FALSE)
    grid()

    if (par()$mfg[2] == 1) {
      axis(2, las = 1)
    }

    if (par()$mfg[1] == par()$mfg[3] | y == max(years)) {
      axis(1)
    }

    lengths_out <- sub_data$Length[sub_data$Year == y & sub_data$Areas == "non_CCA"]
    lengths_in  <- sub_data$Length[sub_data$Year == y & sub_data$Areas == "CCA"]

    hist(lengths_out,
         breaks = seq(xlim[1], xlim[2], 2),
         freq = FALSE,
         col = colvec[1],
         add = TRUE)
    if (length(lengths_in > 0)) {
      hist(lengths_in,
           breaks = seq(xlim[1], xlim[2], 2),
           freq = FALSE,
           col = colvec[2],
           add = TRUE)
    }
    legend('topleft', legend = NA, bty = 'n', title = y, cex = 1.5)
    legend('right', legend = NA, bty = 'n',
           title = paste0("N lens outside = ",
                          length(lengths_out),
                          "\nN lens inside = ",
                          length(lengths_in),
                          " (",
                          round(100 * length(lengths_in) / length(lengths_out) ),
                          "%)"),
           cex = 1.0)
  } # years

  dev.off()


  # Compare length distribution by sex within and outside the CCA
  pngfun(wd = file.path(dir, "plots"), file = paste0(file, '_by_sex.png'), w = 7, h = 7, pt = 12)

  par(mfcol = c(length(unique(sub_data$Sex)) + 1, 1),
      mar = c(0.5, 0.5, 0.5, 0.5),
      oma = c(4,4,1,1))

  sex <- sort(unique(sub_data$Sex))
  colvec <- c(rgb(1, 0, 0, alpha = 0.8), rgb(0, 0, 1, alpha = 0.5))

  # empty plot for legend
  plot(0, type = 'n', axes = FALSE)
  legend('center',
         bty = 'n',
         fill = colvec,
         cex = 1.5,
         legend = c("Length by sex outside the CCA",
                    "Length by sex inside the CCA"))
  mtext("Length (cm)", side = 1, line = 2.5, outer = TRUE)

  for (y in sex) 
  {
    # make empty plot (note: xlim and ylim were set by trial and error)
    plot(0, type = 'n',
         xlim = xlim,
         xaxs = 'i',
         ylim = ylim,
         yaxs = 'i',
         axes = FALSE)
    grid()

    if (par()$mfg[2] == 1) {
      axis(2, las = 1)
    }

    if (par()$mfg[1] == par()$mfg[3]) {
      axis(1)
    }

    lengths_out <- sub_data$Length[sub_data$Sex == y & sub_data$Areas == "non_CCA"]
    lengths_in  <- sub_data$Length[sub_data$Sex == y & sub_data$Areas == "CCA"]

    hist(lengths_out,
         breaks = seq(xlim[1], xlim[2], 2),
         freq = FALSE,
         col = colvec[1],
         add = TRUE)
    if (length(lengths_in > 0)) {
      hist(lengths_in,
           breaks = seq(xlim[1], xlim[2], 2),
           freq = FALSE,
           col = colvec[2],
           add = TRUE)
    }
    legend('topleft', legend = NA, bty = 'n', title = y, cex = 1.5)
    legend('right', legend = NA, bty = 'n',
           title = paste0("N lens outside = ",
                          length(lengths_out),
                          "\nN lens inside = ",
                          length(lengths_in)),
           cex = 1.0)
  } # years

  dev.off()

  return(NULL)
}
