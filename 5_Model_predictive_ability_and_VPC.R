library(ggplot2)
library(pROC)
library(plotROC)

#Set working directory
setwd("C:/filepath")

########################################################################################################################################
###ROC

#HepB0, Penta1, Penta2, Penta3, MCV1, AllPenta
dat.hepb0 <- read.csv("hepB0_timely_fitted_obs.csv", header=TRUE); dat.hepb0$vaccine <- c(rep("HepB0", nrow(dat.hepb0)))
dat.penta1 <- read.csv("penta1_timely_fitted_obs.csv", header=TRUE); dat.penta1$vaccine <- c(rep("Penta 1", nrow(dat.penta1)))
dat.penta2 <- read.csv("penta2_timely_fitted_obs.csv", header=TRUE); dat.penta2$vaccine <- c(rep("Penta 2", nrow(dat.penta2)))
dat.penta3 <- read.csv("penta3_timely_fitted_obs.csv", header=TRUE); dat.penta3$vaccine <- c(rep("Penta 3", nrow(dat.penta3)))
dat.mcv1 <- read.csv("mcv1_timely_fitted_obs.csv", header=TRUE); dat.mcv1$vaccine <- c(rep("MCV1", nrow(dat.mcv1)))
dat.return <- read.csv("return_timely_fitted_obs.csv", header=TRUE); dat.return$vaccine <- c(rep("All Penta", nrow(dat.return)))

dat.comb <- rbind(dat.hepb0, dat.penta1, dat.penta2, dat.penta3, dat.mcv1, dat.return)


# Set up the PNG device with 300 dpi
png("ROC.plot.png", width = 10, height = 10, units = "in", res = 300)
#par(mfrow=c(1,3))
#DTP1
#dat.comb <- dat.comb.dpt1
cols <- c("darkorange1", "aquamarine4", "darkblue", "cornflowerblue", "wheat4",
          "magenta")
cc <- sort(unique(dat.comb$vaccine)); ff <- 0
yy <- seq(0.05,0.45,by=0.05)
for (i in 1:6){
  dat.comb1 <- dat.comb[dat.comb$vaccine==cc[i],]
  g <- roc(obs ~ fitted.mean, print.auc = TRUE, data = dat.comb1) # calculate the C statistics
  if (i==1) plot(g, print.auc = FALSE, legacy.axes = TRUE, col=cols[i], main="ROC curve of the six models", ylim=c(0,1), cex.lab=1.5)  #legacy.axes = TRUE for 1-specifity
  if (i==1) grid(nx=10, ny=10, col="lightgray", equilogs = TRUE)
  if (i>1) plot(g, print.auc = FALSE, legacy.axes = TRUE, col=cols[i], add=TRUE)  #legacy.axes = TRUE for 1-specifity
  ff[i] <- round(auc(g),2)
  #text(0.6, yy[i], paste(cc[i], ff), col=cols[i])
}
leg.lab <- paste(cc, ff, sep=", ")
legend("bottomright", leg.lab, col=cols, lty=1, ncol=1, lwd=2)

dev.off()


########################################################################################################################################
###VPC

#Variance partition coefficient
dat <- read.csv("VPC_results.csv", header = TRUE)
head(dat)

# Convert "Vaccine" column to factor with desired order
dat$Vaccine <- factor(dat$Vaccine, levels = unique(dat$Vaccine))
dat$Level <- factor(dat$Level, levels = unique(dat$Level))

p <- ggplot(data = dat, aes(x = Vaccine, y = var_part_coeff, fill=Level)) + 
  xlab("") + ylab("Variance partition coefficient (%)") + 
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 2)) +
  geom_bar(stat="identity", position="dodge", colour="black") + 
  theme(strip.text.x = element_text(size=12, face="bold", colour="black"), 
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size=12, colour="black"), 
        axis.text.y = element_text(size=10),
        legend.title = element_text(size=12), 
        legend.text = element_text(size=12), 
        legend.position="top",
        panel.background = element_rect(fill = "white", colour = "grey50", linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.5, linetype = 'solid',colour = "grey90"),
        panel.grid.minor = element_line(linewidth = 0.25, linetype = 'solid', colour = "grey90"),
        plot.margin = unit(c(0, 1, 0, 1), "cm")) +
  scale_fill_manual(values = c("#880406", "#fb361f", "#fcb058"))

ggsave(filename= "VPC_plot_new.png", plot=p, height=8, width=8, units="in", device = "png", dpi=300)