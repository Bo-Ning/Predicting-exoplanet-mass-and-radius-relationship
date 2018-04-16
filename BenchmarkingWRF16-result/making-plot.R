rm(list = ls())
setwd("~/Documents/Research/[2017Astro]NonparamM-R/code/WRF16-result/")

### Read data for MLE.fit ####
M.points <- read.table("M.points.csv")$V1
R.points <- read.table("R.points.csv")$V1
Mass.marg <- read.table("Mass.marg.Rdata")
Radius.marg <- read.table("Radius.marg.Rdata")
M.cond.R <- read.table("M.cond.R.Rdata")
M.cond.R.var <- read.table("M.cond.R.var.Rdata")
M.cond.R.upper <- read.table("M.cond.R.upper.Rdata")
M.cond.R.lower <- read.table("M.cond.R.lower.Rdata")
R.cond.M <- read.table("R.cond.M.Rdata")
R.cond.M.var <- read.table("R.cond.M.var.Rdata")
R.cond.M.upper <- read.table("R.cond.M.upper.Rdata")
R.cond.M.lower <- read.table("R.cond.M.lower.Rdata")

Mass.marg <- as.numeric(gsub(",", "", as.character(Mass.marg$V2)))[2:101]
Radius.marg <- as.numeric(gsub(",", "", as.character(Radius.marg$V2)))[2:101]
M.cond.R <- as.numeric(gsub(",", "", as.character(M.cond.R$V2)))[2:101]
R.cond.M <- as.numeric(gsub(",", "", as.character(R.cond.M$V2)))[2:101]
M.cond.R.var <- as.numeric(gsub(",", "", as.character(M.cond.R.var$V2)))[2:101]
M.cond.R.lower <- as.numeric(gsub(",", "", as.character(M.cond.R.lower$V2)))[2:101]
M.cond.R.upper <- as.numeric(gsub(",", "", as.character(M.cond.R.upper$V2)))[2:101]
R.cond.M.var <- as.numeric(gsub(",", "", as.character(R.cond.M.var$V2)))[2:101]
R.cond.M.lower <- as.numeric(gsub(",", "", as.character(R.cond.M.lower$V2)))[2:101]
R.cond.M.upper <- as.numeric(gsub(",", "", as.character(R.cond.M.upper$V2)))[2:101]

M.cond.R.sd <- sqrt(M.cond.R.var)
R.cond.M.sd <- sqrt(R.cond.M.var)

raw.data <- read.csv("../data-WRF16.csv", header = T) # read dataset
Mass.obs <- raw.data$mass[1:60]
Radius.obs <- raw.data$radii[1:60]
Mass.sigma <- raw.data$s.d.mass[1:60]
Radius.sigma <- raw.data$s.d.radii[1:60]

# save-to-pdf function
savepdf <- function(pdf.name, Myplot) {
  pdf(file = pdf.name, width = 6, height = 6)
  print(Myplot)
  dev.off()
}
####################### Figure 1 #############################
darkblue <- rgb(0,49,87,maxColorValue = 256) # color

library(ggplot2)
library(extrafont)
# create data.frame
nonparam.df <- data.frame(
  radius.np = R.points,
  M.cond.R.mean.np = M.cond.R,
  M.cond.R.upper.np = M.cond.R+M.cond.R.sd,
  M.cond.R.lower.np = M.cond.R-M.cond.R.sd
)
WRF16.less8.df <- data.frame(
  radius.less8 = R.points,
  M.cond.R.mean.less8 = 1.6*R.points^1.8
)
WRF16.less4.df <- data.frame(
  radius.less4 = R.points[R.points <= 4],
  M.cond.R.mean.less4 = 2.7*R.points[R.points <= 4]^1.3
)
data.points <- data.frame(M.obs = Mass.obs, R.obs = Radius.obs,
                          M.max = Mass.sigma+Mass.obs, R.max = Radius.obs+Radius.sigma,
                          M.min = Mass.obs-Mass.sigma, R.min = Radius.obs-Radius.sigma)

figure1 <- ggplot() +
  geom_path(data = nonparam.df, aes(radius.np, M.cond.R.mean.np),
            color = darkblue, size = 1.5) +
  geom_ribbon(data = nonparam.df, 
              aes(x = radius.np, 
                  ymin = M.cond.R.lower.np, ymax = M.cond.R.upper.np),
              fill = darkblue, alpha = 0.3) +
  geom_path(data = WRF16.less8.df, aes(radius.less8, M.cond.R.mean.less8),
            col = "#006666", size = 1.5) +
  geom_ribbon(data = WRF16.less8.df, 
              aes(x = radius.less8, 
                  ymin = M.cond.R.mean.less8-2.9, ymax = M.cond.R.mean.less8+2.9),
              fill = "#66FFFF", alpha = 0.6)  +
  geom_ribbon(data = WRF16.less4.df, 
              aes(x = radius.less4, 
                  ymin = M.cond.R.mean.less4-1.9, ymax = M.cond.R.mean.less4+1.9),
              fill = "#66B2FF", alpha = 1) + 
  geom_path(data = WRF16.less4.df, aes(radius.less4, M.cond.R.mean.less4),
            col = "#0066CC", size = 1) +
  geom_point(data = data.points, aes(x = R.obs, y = M.obs), 
             col = "grey10", alpha = 0.6, size = 0.5) +
  geom_errorbar(data = data.points, 
                aes(x = R.obs, ymin = M.min, ymax = M.max),
                col = "grey20", alpha = 0.6, size = 0.2) +
  geom_errorbarh(data = data.points, 
                 aes(x = R.obs, y = M.obs, xmin = R.min, xmax = R.max),
                 col = "grey20", alpha = 0.6, size = 0.2) +
  xlab(expression(paste("Radius (R"["Earth"],")"))) +
  ylab(expression(paste("Mass (M"["Earth"],")"))) + 
  coord_cartesian(ylim = c(-5,80), xlim = c(0.3, 8)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title = element_text(hjust = 0.5),
        text=element_text(family="Helvetica", size=12, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  annotate("text", x=2.2, y=75, size = 6,
           label= as.character(expression(paste("< 8R"["Earth"]," Nonparam"))), 
           parse=TRUE, col = darkblue) +
  annotate("text", x=2, y=67, size = 6,
           label= as.character(expression(paste("< 8R"["Earth"]," WRF16"))), 
           parse=TRUE, col = "#66FFFF") +
  annotate("text", x=2, y=59, size = 6,
           label= as.character(expression(paste("< 4R"["Earth"]," WRF16"))), 
           parse=TRUE, col = "#0066CC") 
  # ggtitle("Mass-Radius Relations") 

pdf.name <- "MRrelations.pdf"
savepdf(pdf.name, figure1)

######################### Figure 2 ###############################
# read bootstrap data
Mass.marg.boot <- read.table("Mass.marg.boot.csv")
Radius.marg.boot <- read.table("Radius.marg.boot.csv")
M.cond.R.boot <- read.table("M.cond.R.boot.csv")
R.cond.M.boot <- read.table("R.cond.M.boot.csv")
M.cond.R.var.boot <- read.table("M.cond.R.var.boot.csv")
M.cond.R.quantile.boot <- read.table("M.cond.R.quantile.boot.csv")
R.cond.M.var.boot <- read.table("R.cond.M.var.boot.csv")
R.cond.M.quantile.boot <- read.table("R.cond.M.quantile.boot.csv")

Mass.marg.boot <- as.matrix(Mass.marg.boot)
Radius.marg.boot <- as.matrix(Radius.marg.boot)
M.cond.R.boot <- as.matrix(M.cond.R.boot)
R.cond.M.boot <- as.matrix(R.cond.M.boot)
M.cond.R.var.boot <- as.matrix(M.cond.R.var.boot)
R.cond.M.var.boot <- as.matrix(R.cond.M.var.boot)
M.cond.R.lower.boot <- as.matrix(M.cond.R.quantile.boot)[1, ]
M.cond.R.upper.boot <- as.matrix(M.cond.R.quantile.boot)[2, ]
R.cond.M.lower.boot <- as.matrix(R.cond.M.quantile.boot)[1, ]
R.cond.M.upper.boot <- as.matrix(R.cond.M.quantile.boot)[2, ]

M.cond.R.sd.boot <- sqrt(M.cond.R.var.boot)
R.cond.M.sd.boot <- sqrt(R.cond.M.var.boot)
M.cond.R.median.boot <- rowMedians(M.cond.R.boot)
M.cond.R.lower.boot <- rowQuantiles(M.cond.R.boot, probs = 0.14)
M.cond.R.upper.boot <- rowQuantiles(M.cond.R.boot, probs = 0.86)

library(matrixStats)
M.pureFe <- exp((-0.4938 + 
                   sqrt(0.4938^2 - 4*0.0975*(0.7932-R.points[R.points>0.2])))/(2*0.0975))
upper.bound <- rowMins(cbind(M.cond.R.upper[R.points>0.2], M.pureFe))
upper.bound <- c(upper.bound[1], upper.bound)
lower.bound <- M.cond.R.lower
lower.bound[lower.bound < 0] <- 0

nonparam.pred <- data.frame(
  radius = R.points,
  M.cond.R.mean = M.cond.R,
  M.cond.R.upper.np = M.cond.R.lower,
  M.cond.R.lower.np = M.cond.R.upper
)

nonparam.boot <- data.frame(
  radius = R.points,
  M.cond.R.upper.boot = M.cond.R.upper.boot,
  M.cond.R.lower.boot = M.cond.R.lower.boot
)

data.points <- data.frame(M.obs = Mass.obs, R.obs = Radius.obs,
                          M.max = Mass.sigma+Mass.obs, R.max = Radius.obs+Radius.sigma,
                          M.min = Mass.obs-Mass.sigma, R.min = Radius.obs-Radius.sigma)
upper.bound <- data.frame(R = R.points[R.points < 3.2], 
                          upper.bound = upper.bound[R.points < 3.2])
lower.bound <- data.frame(R = R.points[R.points < 1.8],
                          lower.bound = lower.bound[R.points < 1.8])

figure2 <- ggplot() +
  geom_path(data = nonparam.pred, aes(radius, M.cond.R.mean),
            color = darkblue, size = 1) +
  geom_ribbon(data = nonparam.pred, 
              aes(x = radius, 
                  ymin = M.cond.R.lower, ymax = M.cond.R.upper),
              fill = darkblue, alpha = 0.3) + 
  # geom_ribbon(data = nonparam.boot, 
  #             aes(x = radius, 
  #                 ymin = M.cond.R.lower.boot, ymax = M.cond.R.upper.boot),
  #             fill = "#66FFFF", alpha = 0.6) + 
  geom_point(data = data.points, aes(x = R.obs, y = M.obs), 
             col = "grey10", alpha = 0.6, size = 0.5) +
  geom_errorbar(data = data.points, 
                aes(x = R.obs, ymin = M.min, ymax = M.max),
                col = "grey20", alpha = 0.6, size = 0.2) +
  geom_errorbarh(data = data.points, 
                 aes(x = R.obs, y = M.obs, xmin = R.min, xmax = R.max),
                 col = "grey20", alpha = 0.6, size = 0.2) +
  geom_path(data = upper.bound, aes(x = R, y = upper.bound), color = "red", 
            size = 1, linetype = 2) +
  geom_path(data = lower.bound, aes(x = R, y = lower.bound), color = "red", 
            size = 1, linetype = 2) +
  xlab(expression(paste("Radius (R"["Earth"],")"))) +
  ylab(expression(paste("Mass (M"["Earth"],")"))) + 
  coord_cartesian(ylim = c(-5,80), xlim = c(0.3, 8)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title = element_text(hjust = 0.5),
        text=element_text(family="Helvetica", size=12, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
 # ggtitle("Nonparametric M-R relation \n with its predicted intervals") 

pdf.name <- "figure2-MRpredictedintervals.pdf"
savepdf(pdf.name, figure2)

######################### Figure 3 ###############################
library(matrixStats)

nonparam.sd <- data.frame(
  radius = R.points,
  M.cond.R.sd = rowMedians(M.cond.R.sd.boot),
  M.cond.R.sd.upper.np = rowQuantiles(M.cond.R.sd.boot, probs = 0.86),
  M.cond.R.sd.lower.np = rowQuantiles(M.cond.R.sd.boot, probs = 0.14)
)
WRF16.less8.sd <- data.frame(
  radius.less8 = R.points,
  M.cond.R.sd.less8 = rep(3.146134, length(R.points)),
  M.cond.R.upper.less8 = rep(4.189529, length(R.points)),
  M.cond.R.lower.less8 = rep(2.337358, length(R.points))
)
WRF16.less4.sd <- data.frame(
  radius.less4 = R.points[R.points < 4],
  M.cond.R.sd.less4 = rep(2.235562, length(R.points[R.points < 4])),
  M.cond.R.upper.less4 = rep(2.981101, length(R.points[R.points < 4])),
  M.cond.R.lower.less4 = rep(1.655242, length(R.points[R.points < 4]))
)

# define color
darkblue <- rgb(0,49,87,maxColorValue = 256)

figure3 <- 
  ggplot() +
  geom_path(data = nonparam.sd, aes(radius, M.cond.R.sd),
            color = darkblue, size = 1) +
  geom_ribbon(data = nonparam.sd, 
              aes(x = radius, 
                  ymin = M.cond.R.sd.lower.np, ymax = M.cond.R.sd.upper.np),
              fill = darkblue, alpha = 0.3) +
  geom_path(data = WRF16.less8.sd, aes(radius.less8, M.cond.R.sd.less8),
             col = "#66FFFF", size = 1) +
  geom_ribbon(data = WRF16.less8.sd, 
              aes(x = radius.less8, 
                  ymin = M.cond.R.lower.less8, ymax = M.cond.R.upper.less8),
              fill = darkblue, alpha = 0.1) + 
  geom_path(data = WRF16.less4.sd, aes(radius.less4, M.cond.R.sd.less4),
            col = "#0066CC", size = 1) +
  geom_ribbon(data = WRF16.less4.sd, 
              aes(x = radius.less4, 
                  ymin = M.cond.R.lower.less4, ymax = M.cond.R.upper.less4),
              fill = darkblue, alpha = 0.1) + 
  
  # geom_path(data = WRF16.less8.sd, aes(radius.less8, M.cond.R.sd.less8),
  #           col = "#66FFFF", size = 1) +
  # geom_path(data = WRF16.less8.sd, aes(radius.less8, M.cond.R.upper.less8),
  #           col = "#66FFFF", size = 0.5) +
  # geom_path(data = WRF16.less8.sd, aes(radius.less8, M.cond.R.lower.less8),
  #           col = "#66FFFF", size = 0.5) +
  # geom_path(data = WRF16.less4.sd, aes(radius.less4, M.cond.R.sd.less4),
  #           col = "#0066CC", size = 1) +
  # geom_path(data = WRF16.less4.sd, aes(radius.less4, M.cond.R.upper.less4),
  #           col = "#0066CC", size = 0.5) +
  # geom_path(data = WRF16.less4.sd, aes(radius.less4, M.cond.R.lower.less4),
  #           col = "#0066CC", size = 0.5) +
  
  # geom_ribbon(data = WRF16.less8.sd, 
  #             aes(x = radius.less8, 
  #                 ymin = M.cond.R.lower.less8, ymax = M.cond.R.upper.less8),
  #             fill = "#66FFFF", alpha = 0.6)  +
  xlab(expression(paste("Radius (R"["Earth"],")"))) +
  ylab(expression(paste("Intrinsic scatter of M-R relation"))) + 
  coord_cartesian(ylim = c(1,6), xlim = c(0.3, 8)) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title = element_text(hjust = 0.5),
        text=element_text(family="Helvetica", size=12, face = "bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) 

pdf.name <- "figure3-sdofMassgivenRadius.pdf"
savepdf(pdf.name, figure3)


