# Bland-Altman
# Shade ICCC red/amber/green
# Output; % points above and below confidence limits, whether difference increases with magnitude and the spread of distance from the mean



# Select two measures or select to automatically detect number of measures and run for all possible pairs








#'Compflow


#' @author Hayman, \email{l.hayman.1@@research.gla.ac.uk}
#' This function takes in data for two sets of measurements and produces a comparison.


#' @import ggplot2
#' @import plyr
#' @import irr
#' @import ggpubr
#' @import ggplotify
#' @import gridExtra


#' @param Subdirectory A subdirectory of your working directory, in which you store your dataset CSV files. Required.
#' @param TrainingData The name of your training dataset, which is used to generate cutpoints. Required.
#' @param CutPointStatus The survival status you wish to use to define your cutpoints. Required.
#' @param CutPointTime The time variable you wish to use to define your cutpoints. Required.
#' @param minprop The minimum proportion of cases to be included in a group. Default is 0.1 if none is provided.
#' @param Variables A list of all variables you wish to be included. Names must match and be identical across datasets. Required.
#' @param Palette Use 'Greyscale' if a greyscale plot is wanted. Otherwise, red/blue is the default palette.


#' @return CompFlow returns a scatter plot, a Bland-Altman plot and an accompanying ICC.

#' @examples CutFlow(Subdirectory = "CutFlowData", TrainingData = "Glasgow", CutPointStatus = "CSS", CutPointTime = "CSS_2017", minprop = 0.1, Variables = c("GD_PercPositiveCellsinHealthyStroma", "GD_PercPositiveCellsinHealthyEpithelium", "GD_PercPositiveCellsinHealthyTissue", "GD_PercPositiveCellsinTumourStroma", "GD_PercPositiveCellsinTumourEpithelium", "GD_PercPositiveCellsinTumourTissue", "CD8_PercPositiveCellsinHealthyStroma", "CD8_PercPositiveCellsinHealthyEpithelium", "CD8_PercPositiveCellsinHealthyTissue", "CD8_PercPositiveCellsinTumourStroma", "CD8_PercPositiveCellsinTumourEpithelium", "CD8_PercPositiveCellsinTumourTissue"))

#' @export


# library(ggplot2)
# library(ggpubr)
# library(plyr)
# library(irr)
# library(ggplotify)
# library(gridExtra)
# Data <- read.csv(file.choose(), fileEncoding = 'UTF-8-BOM')
# CompFlow(Data, ICCModel = "twoway", ICCType = "agreement", ICCUnit = "single", Density = "Yes")

#Function
CompFlow <-
  function(Data,
           ICCModel = NULL,
           ICCType = NULL,
           ICCUnit = NULL,
           Density = "No") {
    #### Read data and check for errors ####

    # Save original working directory and set to restore on exit
    WD <- toString(getwd())
    on.exit(setwd(WD), add = TRUE)


    # Check that there are two columns
    if (ncol(Data) != 2 | length(Data[, 1]) != length(Data[, 2]))
      stop("Data should contain two columns of equal length")



    #### Create a subdirectory for output ####

    # Define and check subdirectory name
    Number <- 1
    OutputDirectory <-
      paste("CompFlow", Sys.Date(), Number, sep = '_')
    while (file.exists(OutputDirectory))
    {
      Number <- Number + 1
      OutputDirectory <-
        paste("CompFlow", Sys.Date(), Number, sep = '_')
    }

    # Create the subdirectory
    dir.create(OutputDirectory)
    setwd(OutputDirectory)
    OD <- toString(getwd())



    #### Deposit raw datasets into a folder for record keeping ####

    # Create and set a subdirectory
    dir.create("0.OriginalData")
    setwd("0.OriginalData")

    # Write datasets to csv files
    write.csv(Data, paste("RawData.csv", sep = ""), row.names = FALSE)

    #Revert to output subdirectory
    setwd(OD)



    #### Configure Data ####

    # Create and set a subdirectory
    dir.create("1.ConfiguredData")
    setwd("1.ConfiguredData")

    # Calculate difference and mean
    DataForICC <- Data
    Data$Difference <- Data[, 1] - Data[, 2]
    Data$Mean <- (Data[, 1] + Data[, 2]) / 2
    DataDifference <- Data$Difference
    DifferenceMean <- mean(DataDifference, na.rm = T)
    DifferenceStDev <- sd(DataDifference, na.rm = T)
    DifferenceUpperLimit <-
      DifferenceMean + (DifferenceStDev * 1.96)
    DifferenceLowerLimit <-
      DifferenceMean - (DifferenceStDev * 1.96)

    write.csv(Data, paste("DataConfigured.csv", sep = ""), row.names = FALSE)

    ScatterValues <-
      data.frame(DifferenceMean,
                 DifferenceStDev,
                 DifferenceLowerLimit,
                 DifferenceUpperLimit)
    write.csv(ScatterValues,
              paste("ScatterValues.csv", sep = ""),
              row.names = FALSE)

    xAxisLabel <- colnames(Data)[1]
    yAxisLabel <- colnames(Data)[2]
    colnames(Data)[1] <- "Variable1"
    colnames(Data)[2] <- "Variable2"



    # Calculate ICC
    ICCModelList <- list("oneway", "twoway")
    ICCTypeList <- list("consistency", "agreement")
    ICCUnitList <- list("single", "average")
    if (ICCModel %in% ICCModelList &
        ICCType %in% ICCTypeList & ICCUnit %in% ICCUnitList) {
      ICCData <-
        icc(
          DataForICC,
          model = ICCModel,
          type = ICCType,
          unit = ICCUnit,
          conf.level = 0.95
        )
    }
    ICC <- round(ICCData$value, digits = 3)
    ICCText <- paste("ICCC = ", ICC, sep = "")

    if (ICC < 0.5) {
      SubtitleColour <- "red"
    }

    if (ICC >= 0.5 & ICC < 0.75) {
      SubtitleColour <- "orange"
    }

    if (ICC >= 0.75) {
      SubtitleColour <- "green"
    }


    theme_SC <- function() {
      theme_classic() %+replace%
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    }


    theme_BA <- function() {
      theme_classic() %+replace%
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, colour = SubtitleColour)
        )
    }







    # Revert to output subdirectory
    setwd(OD)





    #### Generate Plots ####

    # Create and set a subdirectory
    dir.create("2.Plots")
    setwd("2.Plots")

    ## Scatter plots
    # Generate scatter plot
    Scatter <-
      ggplot(Data, aes(x = Variable1, y = Variable2)) +
      geom_point() +
      labs(title = "Scatter Plot", x = xAxisLabel, y = yAxisLabel) +
      theme_SC()

    # Generate scatter plot with density
    ScatterDensity <-
      Scatter +
      stat_density_2d(aes(fill = ..level..), geom = "polygon") +
      scale_fill_gradient(low = "blue", high = "red")

    # # Export scatter plot
    # FileName <- paste("Scatter", ".tiff", sep = "")
    # tiff(
    #   FileName,
    #   width = 1500,
    #   height = 1500,
    #   units = "px",
    #   bg = "transparent",
    #   res = 300,
    #   compression = "lzw"
    # )
    #
    # print(Scatter)
    # dev.off()
    #
    # # Export scatter plot with density
    # if (Density == "Yes") {
    #   FileName <- paste("Scatter_Density", ".tiff", sep = "")
    #   tiff(
    #     FileName,
    #     width = 1500,
    #     height = 1500,
    #     units = "px",
    #     bg = "transparent",
    #     res = 300,
    #     compression = "lzw"
    #   )
    #   print(ScatterDensity)
    #   dev.off()
    # }



    ## Bland-Altman
    AbsDiffMax <- max(abs(DataDifference), na.rm = T)
    yLimMax <- AbsDiffMax
    yLimMin <- AbsDiffMax * (-1)

    Data$LimitStatus <- "In"

    Data$LimitStatus[Data$Difference > DifferenceUpperLimit |
                       Data$Difference < DifferenceLowerLimit] <-
      "Out"

    PercOut <-
      round((sum(Data$LimitStatus == "Out") / nrow(Data) * 100), digits = 1)


    PercOutText <-
      paste(PercOut, "% of points outside of confidence limits", sep = "")

    ShapiroResult1 <- shapiro.test(Data[, 1])
    ShapiroResult2 <- shapiro.test(Data[, 2])
    ShapiroP1 <- ShapiroResult1$p.value
    ShapiroP2 <- ShapiroResult2$p.value

    ifelse (
      ShapiroP1 > 0.05 &
        ShapiroP2 > 0.05,
      PearsonResult <-
        cor.test(Data$Difference, Data$Mean),
      PearsonResult <- NA
    )


    if (!is.na(PearsonResult)) {
      PearsonP <- PearsonResult$p.value

      PearsonP <-
        format(round(PearsonP, 3),
               nsmall = 3,
               scientific = FALSE)

      PropBiasText <-
        paste("Proportional bias (Pearson) - p = ", PearsonP, sep = "")
    }

    if (is.na(PearsonResult)) {
      PropBiasText <-
        paste("Proportional bias (Pearson) - p = ", PearsonResult, sep = "")
    }

    lay2 <- rbind(c(1, 2))

    lay3 <- rbind(c(1, 2, 3))

    BlandAltman <-
      ggplot(Data, aes(x = Mean, y = Difference, colour = LimitStatus)) +
      ylim(yLimMin, yLimMax) +
      scale_color_manual(breaks = c("In", "Out"),
                         values = c("green", "red")) +
      geom_point(size = 2, shape = 23) +
      geom_hline(yintercept = DifferenceMean,
                 linetype = "dashed",
                 color = "green") +
      geom_hline(yintercept = DifferenceLowerLimit,
                 linetype = "dashed",
                 color = "red") +
      geom_hline(yintercept = DifferenceUpperLimit,
                 linetype = "dashed",
                 color = "red") +
      labs(title = "Bland-Altman Plot") +
      theme_BA()

    BlandAltmanDensity <-
      BlandAltman + stat_density_2d(aes(fill = ..level..), geom = "polygon") +
      scale_fill_gradient(low = "blue", high = "red")


    # # Export Bland Altman
    # FileName <- paste("Bland-Altman", ".tiff", sep = "")
    # tiff(
    #   FileName,
    #   width = 1500,
    #   height = 1500,
    #   units = "px",
    #   bg = "transparent",
    #   res = 300,
    #   compression = "lzw"
    # )
    # print(BlandAltman)
    # dev.off()
    #
    # # Export Bland Altman with density
    # if (Density == "Yes") {
    #   FileName <- paste("Bland-Altman_Density", ".tiff", sep = "")
    #   tiff(
    #     FileName,
    #     width = 1500,
    #     height = 1500,
    #     units = "px",
    #     bg = "transparent",
    #     res = 300,
    #     compression = "lzw"
    #   )
    #   print(BlandAltmanDensity)
    #   dev.off()
    # }





    ## Composite plots

    # Horizontal Scatter plot + Bland-Altman
    FileName <-
      paste("ScatterPlusBlandAltmanHorizontal", ".tiff", sep = "")
    tiff(
      FileName,
      width = 3000,
      height = 1500,
      units = "px",
      bg = "transparent",
      res = 300,
      compression = "lzw"
    )

    ScatterGrob <- as.grob(Scatter)

    BlandAltmanGrob <- as.grob(BlandAltman)

    ICCGrob <- text_grob(ICCText,
                         just = "centre")

    OutGrob <- text_grob(PercOutText,
                         just = "centre")

    PropBiasGrob <- text_grob(PropBiasText,
                              just = "centre")

    GrobPlots <-
      arrangeGrob(grobs = list(ScatterGrob, BlandAltmanGrob),
                  layout_matrix = lay2)
    GrobLabels <-
      arrangeGrob(grobs = list(OutGrob, ICCGrob, PropBiasGrob),
                  layout_matrix = lay3)
    grid.arrange(GrobPlots,
                 GrobLabels,
                 nrow = 2,
                 heights = c(10, 1))

    dev.off()



    # Horizontal Scatter plot + Bland-Altman with density
    if (Density == "Yes") {
    FileName <-
      paste("ScatterPlusBlandAltmanDensityHorizontal", ".tiff", sep = "")
    tiff(
      FileName,
      width = 3000,
      height = 1500,
      units = "px",
      bg = "transparent",
      res = 300,
      compression = "lzw"
    )

    ScatterGrob <- as.grob(ScatterDensity)

    BlandAltmanGrob <- as.grob(BlandAltmanDensity)

    ICCGrob <- text_grob(ICCText,
                         just = "centre")

    OutGrob <- text_grob(PercOutText,
                         just = "centre")

    PropBiasGrob <- text_grob(PropBiasText,
                              just = "centre")

    GrobPlots <-
      arrangeGrob(grobs = list(ScatterGrob, BlandAltmanGrob),
                  layout_matrix = lay2)
    GrobLabels <-
      arrangeGrob(grobs = list(OutGrob, ICCGrob, PropBiasGrob),
                  layout_matrix = lay3)
    grid.arrange(GrobPlots,
                 GrobLabels,
                 nrow = 2,
                 heights = c(10, 1))

    dev.off()
    }









    setwd(WD)


  }
