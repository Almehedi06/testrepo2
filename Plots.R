packages <- c("tidyverse", "survival", "xlsx", "stringr", "lubridate", "NADA", "here", "gt", "webshot", "zoo", "car", "ggpubr", "dlookr", "rlang", "scales", "multcompView", "rcompanion", "tseries", "forecast", "smooth", "KbMvtSkew", "corrplot")

package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

webshot::install_phantomjs()


raw <- read_csv(here("Dataframes", "df2014_2018_complete_raw.csv"))
processed <- read_csv(here("Dataframes", "df2014_2018_QAQC.csv"))
processed_storm <- read_csv(here("Dataframes", "df2014_2018_storm_QAQC.csv"))


raw <- raw %>% pivot_longer(cols = Precipitation:Recession_Rate, names_to = "Variable", values_to = "Value")
processed <- processed %>% pivot_longer(cols = Precipitation:Recession_Rate, names_to = "Variable", values_to = "Value")
processed_storm <- processed_storm %>% pivot_longer(cols = Precipitation:Recession_Rate, names_to = "Variable", values_to = "Value")


raw %>% ggplot(aes(sample = Value)) + 
  stat_qq() + stat_qq_line() +
  xlab("Theoretical") + ylab("Sample") + facet_wrap(.~Variable, scales = "free", strip.position = "left", labeller = variable_labeller) +
  theme(strip.placement = "outside", strip.background = element_blank(), panel.spacing = unit(1, "lines")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(here("Plots", "QQPlot_of_Features", "raw_qq.png"), width = 30, height = 20, units = "cm")

processed %>% ggplot(aes(sample = Value)) + 
  stat_qq() + stat_qq_line() +
  xlab("Theoretical") + ylab("Sample") + facet_wrap(.~Variable, scales = "free", strip.position = "left", labeller = variable_labeller) +
  theme(strip.placement = "outside", strip.background = element_blank(), panel.spacing = unit(1, "lines")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(here("Plots", "QQPlot_of_Features", "processed_qq.png"), width = 30, height = 20, units = "cm")

processed_storm %>% ggplot(aes(sample = Value)) + 
  stat_qq() + stat_qq_line() +
  xlab("Theoretical") + ylab("Sample") + facet_wrap(.~Variable, scales = "free", strip.position = "left", labeller = variable_labeller) +
  theme(strip.placement = "outside", strip.background = element_blank(), panel.spacing = unit(1, "lines")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggsave(here("Plots", "QQPlot_of_Features", "processed_storm_qq.png"), width = 30, height = 20, units = "cm")


create_histogram <- function(data, grouping, fill = NULL, fillName, title, xlab, ylab, name) {
  data %>% group_by_at(grouping) %>% 
    ggplot() + geom_histogram(aes(x = Value, fill = fill), alpha = 0.5) +
    #scale_x_continuous(trans = "sqrt") +
    scale_y_continuous(trans = "sqrt") +
    xlab(xlab) + ylab(ylab) + facet_wrap(.~Variable) + facet_wrap(.~Variable, scales = "free_x", strip.position = "bottom", labeller = variable_labeller) +
    theme(plot.xlab = element_text(face="bold")) + 
    theme(plot.ylab = element_text(face="bold")) +
    theme_bw() + guides(fill = FALSE) + theme(strip.placement = "outside", strip.background = element_blank(), panel.spacing = unit(1, "lines")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggsave(here("Plots", "Distribution_of_Features", name), width = 20, height = 20, units = "cm")
}

variable_names <- list(
  "Precipitation" = "Precipitation (in)",
  "Recession_Rate" = "Recession_Rate (in/hr)",
  "Soil_Cond" = "Soil_Cond (μS/cm)",
  "Soil_MC" = "Soil_MC (actual/potential)",
  "Temp_Air" = "Temp_Air (degC)",
  "Temp_Moist" = "Temp_Moist (degC)",
  "Water_Depth" = "Water_Depth (ft)"
)

variable_labeller <- function(variable, value){
  return(variable_names[value])
}

create_histogram(raw, vars(Variable), raw$Variable, "Feature", "Histogram of Feature", "Feature Value", "Count", "Histogram_raw.png")
create_histogram(processed, vars(Variable), processed$Variable, "Feature", "Histogram of Feature", "Feature Value", "Count", "Histogram_proc.png")
create_histogram(processed_storm, vars(Variable), processed_storm$Variable, "Feature", "Histogram of Feature", "Feature Value", "Count", "Histogram_proc_storm.png")


getNormality <- function() {
  
  variables <- c("Precipitation", "Recession_Rate", "Soil_Cond", "Soil_MC", "Temp_Air", "Temp_Moist", "Water_Depth")
  df <- data.frame(matrix(ncol = 5, nrow = 0))
  names <- c("Feature", "Measure", "Raw", "Processed", "Event-Based")
  colnames(df) <- names 
  
  for (variable in variables) {
    raw_normality <- raw %>% filter(Variable == variable) %>% normality()
    processed_normality <- processed %>% filter(Variable == variable) %>% normality()
    processed_storm_normality <- processed_storm %>% filter(Variable == variable) %>% normality()
    
    df_normality <- data.frame(Feature = variable, Measure = "Shapiro-Wilk Normality", Raw = raw_normality$p_value, Processed = processed_normality$p_value, "Processed Storm" = processed_storm_normality$p_value)
    print(df_normality)
    df <- rbind(df, df_normality)
    
    raw_vals <- raw %>% filter(Variable == variable)
    processed_vals <- processed %>% filter(Variable == variable)
    processed_storm_vals <- processed_storm %>% filter(Variable == variable)
    
    raw_skew <- PearsonSkew(raw_vals$Value)
    processed_skew <- PearsonSkew(processed_vals$Value)
    processed_storm_skew<- PearsonSkew(processed_storm_vals$Value)
    
    df_skew <- data.frame(Feature = variable, Measure = "Pearson Skew", Raw = raw_skew, Processed = processed_skew, "Processed Storm" = processed_storm_skew)
    df <- rbind(df, df_skew)
    show(df)
  }
  df %>% gt() %>% gtsave(here("Plots", "Tables", "skewness_normality.png"))
}

getNormality()

raw <- read_csv(here("Dataframes", "df2014_2018_complete_raw.csv"))
raw["Datetime"] <- NULL
processed <- read_csv(here("Dataframes", "df2014_2018_QAQC.csv"))
processed["Datetime"] <- NULL
processed_storm <- read_csv(here("Dataframes", "df2014_2018_storm_QAQC.csv"))
processed_storm <- drop_na(processed_storm)
processed_storm["Datetime"] <- NULL

png(here("Plots", "Correlation_of_Features", "corr.png"), width = 465, height = 150, units='mm', res = 300)
par(mfrow = c(1,3))

corrplot(round(cor(raw), 2), title = "Raw Dataset", method = "shade", diag = FALSE, addCoef.col = 'black', tl.col = 'black', tl.srt = 45, mar=c(0,0,6,6))
corrplot(round(cor(processed), 2), title = "Processed Dataset", method = "shade", diag = FALSE, addCoef.col = 'black', tl.col = 'black', tl.srt = 45, mar=c(0,0,6,6))
corrplot(round(cor(processed_storm), 2), title = "Event-Based", method = "shade", diag = FALSE, addCoef.col = 'black', tl.col = 'black', tl.srt = 45, mar=c(0,0,6,6))
dev.off()






