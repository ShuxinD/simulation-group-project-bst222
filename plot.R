#' make simulation result plot

## setup ----
library(ggplot2)
library(data.table)

rm(list = ls())
gc()

## load result data ----
scen1Results <- fread(file.path("results", "scen1Results.csv"))
scen21Results <- fread(file.path("results", "scen21Results.csv"))
scen22Results <- fread(file.path("results", "scen22Results.csv"))

## get the dataset for plotting----
generatePlotData <- function(results_dt){
  # effect_error <- results_dt[[1,c(4)]]
  plot_dt <- rbind(results_dt[, .(lowCI=quantile(regCal1, 0.025),
                                  point_est=median(regCal1),
                                  highCI=quantile(regCal1, 0.975)), by=sampleSize][, mod:="model1"],
                   results_dt[, .(lowCI=quantile(regCal2, 0.025),
                                  point_est=median(regCal2),
                                  highCI=quantile(regCal2, 0.975)), by=sampleSize][, mod:="model2"],
                   results_dt[, .(lowCI=quantile(regCal3, 0.025),
                                  point_est=median(regCal3),
                                  highCI=quantile(regCal3, 0.975)), by=sampleSize][, mod:="model3"])
  # View(plot_dt)
  return(plot_dt)
}

## summary the table for plotting ----
plotDT_all <- rbind(cbind(generatePlotData(scen1Results), scen= "scen1"),
                    cbind(generatePlotData(scen21Results),scen= "scen21"),
                    cbind(generatePlotData(scen22Results),scen= "scen22"))
plotDT_all[, sampleSize:= factor(sampleSize, levels = c("100", "500","1000", "2000"))]
errorPM_dt <- data.table(scen=c("scen1", "scen21", "scen22"),
                         errorPM = c(scen1Results[1,errorPM], scen21Results[1,errorPM], scen22Results[1,errorPM]))

## overall plot ----
ggplot(plotDT_all, aes(x=1, y = point_est)) +
  geom_pointrange(size=0.5, aes(ymin = lowCI, ymax = highCI, color = sampleSize), position = position_dodge(0.8)) + 
  geom_hline(yintercept = 0.14, linetype="solid", color = 1, size = 0.5) + 
  ylab("Effect") + xlab("Calibration model") +
  scale_x_discrete(labels=c("model1" = "Model 1", "model2" = "Model 2", "model3" = "Model 3")) + 
  labs(color = "Calibration Sample Size") + 
  # facet_wrap(vars(scen), scales = "free") +
  facet_grid(scen~mod,scales = "free_y",  space = "fixed") +
  geom_hline(data = errorPM_dt, aes(yintercept = errorPM), linetype= "dashed", color = 2, size = 0.5) +
  # geom_hline(yintercept = effect_error, linetype= "dashed", color = 2, size = 0.5) +
  theme_minimal() + 
  theme(legend.position = "top",
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size=1))

## plot scen1 ----
plot_scen1 <- ggplot(plotDT_all[scen=="scen1",], aes(x=1, y = point_est)) +
  geom_pointrange(size=0.5, aes(ymin = lowCI, ymax = highCI, color = sampleSize), position = position_dodge(0.8)) + 
  geom_hline(yintercept = 0.14, linetype="solid", color = 1, size = 0.5) + 
  ylab("Effect") + xlab("Calibration model") +
  scale_x_discrete(labels=c("model1" = "Model 1", "model2" = "Model 2", "model3" = "Model 3")) + 
  labs(color = "Calibration Sample Size") + 
  facet_wrap(vars(mod), scales = "free") +
  #facet_grid(scen~mod,scales = "free_y",  space = "fixed") +
  geom_hline(data = errorPM_dt[scen=="scen1",], aes(yintercept = errorPM), linetype= "dashed", color = 2, size = 0.5) +
  # geom_hline(yintercept = effect_error, linetype= "dashed", color = 2, size = 0.5) +
  theme_minimal() + 
  theme(legend.position = "top",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size=1))

## plot scen21 ----
plot_scen21 <- ggplot(plotDT_all[scen=="scen21",], aes(x=1, y = point_est)) +
  geom_pointrange(size=0.5, aes(ymin = lowCI, ymax = highCI, color = sampleSize), position = position_dodge(0.8)) + 
  geom_hline(yintercept = 0.14, linetype="solid", color = 1, size = 0.5) + 
  ylab("Effect") + xlab("Calibration model") +
  scale_x_discrete(labels=c("model1" = "Model 1", "model2" = "Model 2", "model3" = "Model 3")) + 
  labs(color = "Calibration Sample Size") + 
  facet_wrap(vars(mod), scales = "free") +
  #facet_grid(scen~mod,scales = "free_y",  space = "fixed") +
  geom_hline(data = errorPM_dt[scen=="scen21",], aes(yintercept = errorPM), linetype= "dashed", color = 2, size = 0.5) +
  # geom_hline(yintercept = effect_error, linetype= "dashed", color = 2, size = 0.5) +
  theme_minimal() + 
  theme(legend.position = "top",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size=1))

## plot scen22 ----
plot_scen22 <- ggplot(plotDT_all[scen=="scen22",], aes(x=1, y = point_est)) +
  geom_pointrange(size=0.5, aes(ymin = lowCI, ymax = highCI, color = sampleSize), position = position_dodge(0.8)) + 
  geom_hline(yintercept = 0.14, linetype="solid", color = 1, size = 0.5) + 
  ylab("Effect") + xlab("Calibration model") +
  scale_x_discrete(labels=c("model1" = "Model 1", "model2" = "Model 2", "model3" = "Model 3")) + 
  labs(color = "Calibration Sample Size") + 
  facet_wrap(vars(mod), scales = "free") +
  #facet_grid(scen~mod,scales = "free_y",  space = "fixed") +
  geom_hline(data = errorPM_dt[scen=="scen22",], aes(yintercept = errorPM), linetype= "dashed", color = 2, size = 0.5) +
  # geom_hline(yintercept = effect_error, linetype= "dashed", color = 2, size = 0.5) +
  theme_minimal() + 
  theme(legend.position = "top",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), panel.border = element_rect(colour = "black", fill=NA, size=1))

cairo_pdf(file.path("results/", "plot_scen1.pdf"), height = 5, width = 5)
plot_scen1
dev.off()

cairo_pdf(file.path("results/", "plot_scen21.pdf"), height = 5, width = 5)
plot_scen21
dev.off()

cairo_pdf(file.path("results/", "plot_scen22.pdf"), height = 5, width = 5)
plot_scen22
dev.off()
