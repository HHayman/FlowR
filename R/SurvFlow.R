#'Survflow


#' @author Hayman, \email{l.hayman.1@@research.gla.ac.uk}
#' This function takes in a dataset(s), produces cut points, recodes your datasets and runs appropriate survival analysis.


#' @import survival
#' @import maxstat
#' @import survminer
#' @import tidyverse
#' @import ggpubr
#' @import ggplot2
#' @import gplots


#' @param Data A Coded dataset
#' @param Variables A list of variables (coded as 0 and 1) for analysis
#' @param LegendLabels Optional labels for legend
#' @param Identifier Identifier variable for cases
#' @param PlotTitles Optional plot titles
#' @param SurvivalStatus A status variable (coded as 0 and 1) for survival analysis
#' @param SurvivalTime A survival time variable - continuous
#' @param SurvivalTimeUnit The unit of time for survival time. Months etc
#' @param xYearSurvivalVar The number of years to be used to calculate 'X' years survival. Default = 5
#' @param SurvBase Takes TRUE or FALSE. Toggle to activate base survival analysis.


#' @return Takes a coded dataset (use CutFlow) and runs survival analysis for each variable, by each survival outcome, and returns output.


#' @examples SurvFlow(Glasgow,  Variables = c("GD_PercPositiveCellsinHealthyTissue_Coded", "GD_PercPositiveCellsinTumourTissue_Coded", "CD8_PercPositiveCellsinHealthyTissue_Coded", "CD8_PercPositiveCellsinTumourTissue_Coded"), LegendLabels = c("Low", "High"), Identifier = "TMA_ID", PlotTitles = c("GD Healthy Stroma", "GD Healthy Epithelium", "GD Healthy Tissue", "GD Tumour Stroma", "GD Tumour Epithelium", "GD Tumour Tissue", "CD8 Healthy Stroma", "CD8 Healthy Epithelium", "CD8 Healthy Tissue", "CD8 Tumour Stroma", "CD8 Tumour Epithelium", "CD8 Tumour Tissue"), SurvivalStatus = c("CSS", "OS", "DFS", "RFS"), SurvivalTime = c("CSS_2017", "CSS_2017", "DFSmonths", "DFSmonths"), xYearSurvivalVar = 5, SurvivalTimeUnit = "Months", SurvBase = TRUE)

#' @export




#Function
SurvFlow <-
  function(Data,
           Variables,
           LegendLabels = NULL,
           Identifier,
           PlotTitles = NULL,
           SurvivalStatus,
           SurvivalTime,
           SurvivalTimeUnit,
           xYearSurvivalVar = 5,
           SurvBase = FALSE) {
    #### Read data and check for errors ####

    # Save original working directory and set to restore on exit
    WD <- toString(getwd())
    on.exit(setwd(WD), add = TRUE)

    # Check that dataset is a dataframe
    if (!inherits(Data, "data.frame"))
      #stop("data should be an object of class data.frame")
      Data <- as.data.frame(Data)

    # Check that identifier is present in the dataset
    if (!all(Identifier %in% colnames(Data)))
      stop("Identifier not found in the data: ")

    # Check that survival variables are in the dataset
    if (!all(c(SurvivalStatus, SurvivalTime) %in% colnames(Data)))
      stop("Specify correct column names containing SurvivalStatus and SurvivalTime values.")

    # Check that variable names are present in the dataset
    if (!all(Variables %in% colnames(Data)))
      stop("Some variables are not found in the data: ",
           paste(setdiff(Variables, colnames(Data)), collapse = ", "))



    #### Create a subdirectory for output ####

    # Define and check subdirectory name
    DataFileName <- deparse(substitute(Data))
    Number <- 1
    OutputDirectory <-
      paste("SurvFlow", DataFileName, Sys.Date(), Number, sep = '_')
    while (file.exists(OutputDirectory))
    {
      Number <- Number + 1
      OutputDirectory <-
        paste("SurvFlow", DataFileName, Sys.Date(), Number, sep = '_')
    }

    # Create the subdirectory
    dir.create(OutputDirectory)
    setwd(OutputDirectory)
    OD <- toString(getwd())




    #### Baseline survival analysis, per status, per variable ####

    # Check if base survival is requested
    if (SurvBase == TRUE) {
      # Create and set a subdirectory
      BaseSurvDirectoryName <-
        paste("BaseSurvival", sep = "")
      dir.create(BaseSurvDirectoryName)
      setwd(BaseSurvDirectoryName)
      BSD <- toString(getwd())

      for (j in 1:length(SurvivalStatus)) {
        # Create and set a subdirectory
        DirectoryName <-
          paste("BaseSurvival_", SurvivalStatus[j], sep = "")
        dir.create(DirectoryName)
        setwd(DirectoryName)


        for (i in 1:length(Variables)) {
          #  Create temporary dataset
          TempData <-
            subset(Data,
                   select = c(
                     Identifier,
                     SurvivalStatus[j],
                     SurvivalTime[j],
                     Variables[i]
                   ))

          TimeName <- SurvivalTime[j]
          StatusName <- SurvivalStatus[j]
          IDName <- Identifier
          VarName <- Variables[i]

          colnames(TempData)[4] <- "Variable"
          colnames(TempData)[2] <- "Status"
          colnames(TempData)[3] <- "Time"


          TempData <- na.omit(TempData)

          Events <- TempData %>% group_by(Variable) %>%
            summarise(Events_Sum = sum(Status))

          Cases <- TempData %>% group_by(Variable) %>% tally()

          SurvOutputTable <-
            merge(Cases, Events, by.x = "Variable", all.y = TRUE)
          colnames(SurvOutputTable)[1] <- "Group"
          colnames(SurvOutputTable)[2] <- "Cases"
          colnames(SurvOutputTable)[3] <- "Events"

          LabX <-
            paste(StatusName, " (", SurvivalTimeUnit, ")", sep = "")
          LabY <- "Survival Outcome Probability"
          ifelse (
            is.null(LegendLabels),
            LegLabs <- unique(TempData$Variable),
            LegLabs <- LegendLabels
          )


          if (nrow(SurvOutputTable) <= 2) {
            ## Calculate X-year survival and add to SurvOutputTable table
            # Create independent tables for each group
            TempData0 <- subset(TempData, Variable == 0)
            TempData1 <- subset(TempData, Variable == 1)

            # Calculate times value
            xTime <- 12 * xYearSurvivalVar

            # Fit survival by x # of years
            xYearSurvival <-
              summary(survfit(Surv(Time, Status) ~ Variable, data = TempData), times = xTime)
            xYearSurvival0 <-
              summary(survfit(Surv(Time, Status) ~ Variable, data = TempData0), times = xTime)
            xYearSurvival1 <-
              summary(survfit(Surv(Time, Status) ~ Variable, data = TempData1), times = xTime)

            # Define x-year survival values
            xYearSurvivalValues <-
              c(
                paste(
                  (round(xYearSurvival0$surv, digits = 2) * 100),
                  "% (",
                  (round(xYearSurvival0$lower, digits = 2) * 100),
                  "%, ",
                  (round(xYearSurvival0$upper, digits = 2) * 100),
                  "%)",
                  sep = ""
                ),
                paste(
                  (round(xYearSurvival1$surv, digits = 2) * 100),
                  "% (",
                  (round(xYearSurvival1$lower, digits = 2) * 100),
                  "%, ",
                  (round(xYearSurvival1$upper, digits = 2) * 100),
                  "%)",
                  sep = ""
                )
              )

            # Define variable name for x-year survival and add to SurvOutputTable table
            xYearSurvivalName <-
              paste(xYearSurvivalVar, "-Year Survival", sep = "")
            SurvOutputTable$xYearSurvivalColumn <-
              xYearSurvivalValues
            colnames(SurvOutputTable)[4] <- xYearSurvivalName
          }


          ## Run cox hazard analysis and add to SurvOutputTable table
          # Fit univariate cox hazard regression
          UniCoxFit <-
            coxph(Surv(Time, Status) ~ Variable, data = TempData) %>% gtsummary::tbl_regression(exp = TRUE)
          UniCoxFitTable <- UniCoxFit$table_body

          # Add univariate cox regression to table
          UniCoxValuesHR <-
            paste(
              round(UniCoxFitTable$estimate, digits = 2),
              " (",
              round(UniCoxFitTable$conf.low, digits = 2),
              ", ",
              round(UniCoxFitTable$conf.high, digits = 2),
              ")",
              sep = ""
            )
          UniCoxValuesP <-
            paste(round(UniCoxFitTable$p.value, digits = 2), sep = "")

          SurvOutputTable$Cox_HR <- UniCoxValuesHR
          SurvOutputTable$Cox_HR_P <- UniCoxValuesP



          # Fit multivariate cox hazard regression



          # Calculate logrank and add to SurvOutputTable table
          LogRankData <-
            survdiff(Surv(Time, Status) ~ Variable, data = TempData)
          LogRankP <-
            pchisq(LogRankData$chisq,
                   length(LogRankData$n) - 1,
                   lower.tail = FALSE)
          SurvOutputTable$Logrank_P <- round(LogRankP, digits = 3)


          ## Additional formatting of SurvOutputTable table
          # Rename the groups in SurvOutputTable table
          SurvOutputTable$Group <- LegLabs


          ## Creation of plots
          # Define title for plot
          ifelse(
            !is.null(PlotTitles),
            Title <-
              PlotTitles[i],
            Title <-
              VarName
          )



          SurvOutputTable <<- SurvOutputTable


          # Define colour palette
          PaletteList <-
            c(
              "red",
              "blue",
              "turquoise4",
              "goldenrod4",
              "mediumorchid4",
              "dodgerblue1",
              "green",
              "magenta3"
            )
          PaletteGroups <- nrow(SurvOutputTable)
          Palette <- PaletteList[1:PaletteGroups]


          ## Generate plot
          # Open tiff file
          FileName <- paste(VarName, ".tiff", sep = "")
          tiff(
            FileName,
            width = 1500,
            height = 1500,
            units = "px",
            bg = "transparent",
            res = 300,
            compression = "lzw"
          )

          custom_theme <- function() {
            theme_test() %+replace%
              theme(
                plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold", size = 15)
              )
          }

          suppressMessages(
          plot <- ggsurvplot(
            fit = survfit(Surv(Time, Status) ~ Variable, data = TempData),
            data = TempData,
            linetype = "solid",
            size = 2,
            censor.shape = 124,
            censor.size = 4,
            palette = Palette,
            break.time.by = 12,
            title = Title,
            font.main = c(10, "bold", "Black"),
            xlab = LabX,
            font.x = c(10, "bold.italic", "Black"),
            ylab = LabY,
            font.y = c(10, "bold.italic", "Black"),
            legend = "none",
            font.tickslab = 8,
            ggtheme = custom_theme(),
            pval = FALSE,
            pval.size = 5,
            pval.coord = c(0.007, 0.05),
            pval.method = FALSE,
            pval.method.size = 5,
            pval.method.coord = c(0.007, 0.1),
            conf.int = TRUE,
            conf.int.style = "ribbon",
            conf.int.alpha = 0.1,
            surv.median.line = "hv",
            risk.table = FALSE,
            cumevents = FALSE,
            cumcensor = FALSE,
            tables.col = "strata",
            fontsize = 4,
            tables.y.text.col = TRUE,
            risk.table.pos = "out"
          )
          )

          # Define Y axis expansion per variable group
          yAxisExp <- 0.2
          VariableLevels <- nrow(SurvOutputTable)
          if (nrow(SurvOutputTable) >= 3) {
            yAxisExp <- 0.2 + (0.15 * (VariableLevels - 2))
          }


          TT1 <- ttheme_minimal(
            core = list(
              bg_params = list(
                fill = Palette,
                alpha = 0.1,
                col = NA
              ),
              fg_params = list(fontface = "bold")
            ),
            colhead = list(fg_params = list(
              col = "Black", fontface = 4L
            )),
            rowhead = list(fg_params = list(
              col = "orange", fontface = 3L
            )),
            base_size = 5
          )

          suppressMessages(
          plot$plot <-
            plot$plot + annotation_custom(
              tableGrob(SurvOutputTable, rows = NULL, theme = TT1),
              xmin = -Inf,
              xmax = Inf,
              ymin = -Inf,
              ymax = 0
            ) +
            scale_y_continuous(expand = c(yAxisExp, 0),
                               breaks = c(0, 0.25, 0.5, 0.75, 1))
          )






          # Print plot
          print(plot)
          dev.off()



        }

        setwd(BSD)
      }


      setwd(OD)

    }




    # #### Faceted survival analysis, per status, per variable ####
    #
    # # Check if facet survival is requested
    # if (SurvFacet == TRUE) {
    #   # Create and set a subdirectory
    #   FacetSurvDirectoryName <-
    #     paste("FacetedSurvival", sep = "")
    #   dir.create(FacetSurvDirectoryName)
    #   setwd(FacetSurvDirectoryName)
    #   FSD <- toString(getwd())
    #
    #
    #   for (k in 1:length(Facets)) {
    #     FacetName <- Facets[k]
    #
    #     for (j in 1:length(SurvivalStatus)) {
    #       # Create and set a subdirectory
    #       DirectoryName <-
    #         paste(FacetName, "_Survival_", SurvivalStatus[j], sep = "")
    #       dir.create(DirectoryName)
    #       setwd(DirectoryName)
    #
    #
    #
    #       for (i in 1:length(Variables)) {
    #         #  Create temporary master dataset
    #         TempData <-
    #           subset(
    #             Data,
    #             select = c(
    #               Identifier,
    #               SurvivalStatus[j],
    #               SurvivalTime[j],
    #               Variables[i],
    #               Facets[k]
    #             )
    #           )
    #
    #         TimeName <- SurvivalTime[j]
    #         StatusName <- SurvivalStatus[j]
    #         IDName <- Identifier
    #         VarName <- Variables[i]
    #         FacetName <- Facets[k]
    #
    #         colnames(TempData)[4] <- "Variable"
    #         colnames(TempData)[2] <- "Status"
    #         colnames(TempData)[3] <- "Time"
    #         colnames(TempData)[5] <- "Facet"
    #
    #         TempData <- na.omit(TempData)
    #
    #         FacetLevelsSub <- unique(TempData$Facet)
    #
    #
    #         for (m in 1:length(FacetLevelsSub)) {
    #
    #           # Subset dataset by facet level
    #           FacetLevel <- FacetLevelsSub[[m]]
    #           TempDataSub <- subset(TempData, TempData$Facet == FacetLevel)
    #
    #
    #           # Generate plots by facet level
    #           Events <- TempDataSub %>% group_by(Variable) %>%
    #             summarise(Events_Sum = sum(Status))
    #
    #           Cases <- TempDataSub %>% group_by(Variable) %>% tally()
    #
    #           SurvOutputTable <-
    #             merge(Cases, Events, by.x = "Variable", all.y = TRUE)
    #           colnames(SurvOutputTable)[1] <- "Group"
    #           colnames(SurvOutputTable)[2] <- "Cases"
    #           colnames(SurvOutputTable)[3] <- "Events"
    #
    #           LabX <-
    #             paste(StatusName, " (", SurvivalTimeUnit, ")", sep = "")
    #           LabY <- "Survival Outcome Probability"
    #           ifelse (
    #             is.null(LegendLabels),
    #             LegLabs <- unique(TempDataSub$Variable),
    #             LegLabs <- LegendLabels
    #           )
    #
    #
    #           # Check number of levels in facet, in TempDataSub
    #           TempDataSubLevelCheck <- length(unique(TempDataSub$Variable))
    #
    #
    #           if (nrow(SurvOutputTable) <= 2 & TempDataSubLevelCheck == 2) {
    #             ## Calculate X-year survival and add to SurvOutputTable table
    #             # Create independent tables for each group
    #             TempDataSub0 <- subset(TempDataSub, Variable == 0)
    #             TempDataSub1 <- subset(TempDataSub, Variable == 1)
    #
    #             # Calculate times value
    #             xTime <- 12 * xYearSurvivalVar
    #
    #             # Fit survival by x # of years
    #             xYearSurvival <-
    #               summary(survfit(Surv(Time, Status) ~ Variable, data = TempDataSub), times = xTime)
    #             xYearSurvival0 <-
    #               summary(survfit(Surv(Time, Status) ~ Variable, data = TempDataSub0),
    #                       times = xTime)
    #             xYearSurvival1 <-
    #               summary(survfit(Surv(Time, Status) ~ Variable, data = TempDataSub1),
    #                       times = xTime)
    #
    #             # Define x-year survival values
    #             xYearSurvivalValues <-
    #               c(
    #                 paste(
    #                   (round(
    #                     xYearSurvival0$surv, digits = 2
    #                   ) * 100),
    #                   "% (",
    #                   (round(
    #                     xYearSurvival0$lower, digits = 2
    #                   ) * 100),
    #                   "%, ",
    #                   (round(
    #                     xYearSurvival0$upper, digits = 2
    #                   ) * 100),
    #                   "%)",
    #                   sep = ""
    #                 ),
    #                 paste(
    #                   (round(
    #                     xYearSurvival1$surv, digits = 2
    #                   ) * 100),
    #                   "% (",
    #                   (round(
    #                     xYearSurvival1$lower, digits = 2
    #                   ) * 100),
    #                   "%, ",
    #                   (round(
    #                     xYearSurvival1$upper, digits = 2
    #                   ) * 100),
    #                   "%)",
    #                   sep = ""
    #                 )
    #               )
    #
    #             # Define variable name for x-year survival and add to SurvOutputTable table
    #             xYearSurvivalName <-
    #               paste(xYearSurvivalVar, "-Year Survival", sep = "")
    #             SurvOutputTable$xYearSurvivalColumn <-
    #               xYearSurvivalValues
    #             colnames(SurvOutputTable)[4] <- xYearSurvivalName
    #           }
    #
    #
    #           if (TempDataSubLevelCheck == 2){
    #           ## Run cox hazard analysis and add to SurvOutputTable table
    #           # Fit univariate cox hazard regression
    #           UniCoxFit <-
    #             coxph(Surv(Time, Status) ~ Variable, data = TempDataSub) %>% gtsummary::tbl_regression(exp = TRUE)
    #           UniCoxFitTable <- UniCoxFit$table_body
    #
    #           # Add univariate cox regression to table
    #           UniCoxValuesHR <-
    #             paste(
    #               round(UniCoxFitTable$estimate, digits = 2),
    #               " (",
    #               round(UniCoxFitTable$conf.low, digits = 2),
    #               ", ",
    #               round(UniCoxFitTable$conf.high, digits = 2),
    #               ")",
    #               sep = ""
    #             )
    #           UniCoxValuesP <-
    #             paste(round(UniCoxFitTable$p.value, digits = 2), sep = "")
    #
    #           SurvOutputTable$Cox_HR <- UniCoxValuesHR
    #           SurvOutputTable$Cox_HR_P <- UniCoxValuesP
    #         }
    #
    #
    #
    #           # Fit multivariate cox hazard regression
    #
    #
    #
    #           if (TempDataSubLevelCheck == 2){
    #           # Calculate logrank and add to SurvOutputTable table
    #           LogRankData <-
    #             survdiff(Surv(Time, Status) ~ Variable, data = TempDataSub)
    #           LogRankP <-
    #             pchisq(LogRankData$chisq,
    #                    length(LogRankData$n) - 1,
    #                    lower.tail = FALSE)
    #           SurvOutputTable$Logrank_P <- round(LogRankP, digits = 3)
    #           }
    #
    #
    #           ## Additional formatting of SurvOutputTable table
    #           # Rename the groups in SurvOutputTable table
    #           LegLabs <- unique(TempDataSub$Variable)
    #           SurvOutputTable$Group <- LegLabs
    #
    #
    #           ## Creation of plots
    #           # Define title for plot
    #           ifelse(
    #             !is.null(PlotTitles) &
    #               !is.na(PlotTitles[i]),
    #             Title <-
    #               PlotTitles[i],
    #             Title <-
    #               paste("Kaplan-Meier Plot - ", VarName, sep = "")
    #           )
    #
    #           SurvOutputTable <- SurvOutputTable %>% arrange(-Group)
    #
    #
    #           # Define colour palette
    #           PaletteList <-
    #             c(
    #               "red",
    #               "blue",
    #               "turquoise4",
    #               "goldenrod4",
    #               "mediumorchid4",
    #               "dodgerblue1",
    #               "green",
    #               "magenta3"
    #             )
    #           PaletteGroups <- nrow(SurvOutputTable)
    #           Palette <- PaletteList[1:PaletteGroups]
    #
    #
    #           ## Generate plot
    #           # Open tiff file
    #           FileName <- paste(VarName, "_", FacetLevel, ".tiff", sep = "")
    #           tiff(
    #             FileName,
    #             width = 1500,
    #             height = 1500,
    #             units = "px",
    #             bg = "transparent",
    #             res = 300,
    #             compression = "lzw"
    #           )
    #
    #           suppressMessages(
    #           plot <- ggsurvplot(
    #             fit = survfit(Surv(Time, Status) ~ Variable, data = TempDataSub),
    #             data = TempDataSub,
    #             linetype = "solid",
    #             size = 2,
    #             censor.shape = 124,
    #             censor.size = 4,
    #             palette = Palette,
    #             break.time.by = 12,
    #             title = Title,
    #             font.main = c(10, "bold", "Black"),
    #             xlab = LabX,
    #             font.x = c(10, "bold.italic", "Black"),
    #             ylab = LabY,
    #             font.y = c(10, "bold.italic", "Black"),
    #             legend = "none",
    #             legend.labs = NULL,
    #             font.tickslab = 8,
    #             ggtheme = theme_test(),
    #             pval = FALSE,
    #             pval.size = 5,
    #             pval.coord = c(0.007, 0.05),
    #             pval.method = FALSE,
    #             pval.method.size = 5,
    #             pval.method.coord = c(0.007, 0.1),
    #             conf.int = TRUE,
    #             conf.int.style = "ribbon",
    #             conf.int.alpha = 0.1,
    #             surv.median.line = "hv",
    #             risk.table = FALSE,
    #             cumevents = FALSE,
    #             cumcensor = FALSE,
    #             tables.col = "strata",
    #             fontsize = 4,
    #             tables.y.text.col = TRUE,
    #             risk.table.pos = "out"
    #           )
    #           )
    #
    #           # Define Y axis expansion per variable group
    #           yAxisExp <- 0.2
    #           VariableLevels <- nrow(SurvOutputTable)
    #           if (nrow(SurvOutputTable) >= 3) {
    #             yAxisExp <- 0.2 + (0.15 * (VariableLevels - 2))
    #           }
    #
    #
    #           TT1 <- ttheme_minimal(
    #             core = list(
    #               bg_params = list(
    #                 fill = Palette,
    #                 alpha = 0.1,
    #                 col = NA
    #               ),
    #               fg_params = list(fontface = "bold")
    #             ),
    #             colhead = list(fg_params = list(
    #               col = "Black", fontface = 4L
    #             )),
    #             rowhead = list(fg_params = list(
    #               col = "orange", fontface = 3L
    #             )),
    #             base_size = 9
    #           )
    #
    #           suppressMessages(
    #           plot$plot <-
    #             plot$plot + annotation_custom(
    #               tableGrob(
    #                 SurvOutputTable,
    #                 rows = NULL,
    #                 theme = TT1
    #               ),
    #               xmin = 70,
    #               xmax = 130,
    #               ymin = -Inf,
    #               ymax = 0
    #             ) +
    #             scale_y_continuous(expand = c(yAxisExp, 0),
    #                                breaks = c(0, 0.25, 0.5, 0.75, 1))
    #           )
    #
    #           # Print plot
    #           print(plot)
    #           dev.off()
    #
    #         }
    #
    #
    #
    #
    #
    #       }
    #
    #       setwd(FSD)
    #
    #     }
    #
    #
    #   }
    #
    #
    #   setwd(OD)
    #
    #
    # }









    # Return to base working directory
    setwd(WD)


  }
