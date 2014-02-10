data <- read.csv("data.csv", header=TRUE, sep=";",dec=",")
library(ggplot2)
levels(data$Langue) <- c("D", "F", "D")

# relation bivariée

png(filename="figure1.png", width=500, height=500)
ggplot(data, aes(x=EtrTaux2012, y=VotTaux2014)) + geom_point(aes(size=Pop2012)) + geom_smooth(aes(group=1),method="lm") + theme_bw() + xlab("Proportion d'étrangers") + ylab('Proportion de "oui"') + theme(legend.title=element_text("Population du canton")) + scale_size_continuous(name="Population du canton")  + theme(legend.position="bottom")
dev.off()

summary(lm(VotTaux2014 ~ EtrTaux2012, data=data))

# introduisons l'effet de la religion et de la langue

# la religion en tant que telle n'a pas d'effet
ggplot(data, aes(x=Protestants, y=VotTaux2014)) + geom_point(aes(size=Pop2012)) + geom_smooth(aes(group=1),method="lm") + theme_bw()

summary(lm(VotTaux2014 ~ EtrTaux2012 +  Langue + Protestants, data=data))

reg2 <- lm(VotTaux2014 ~  Langue + Protestants, data=data)
data$resid <- reg2$residuals
ggplot(data, aes(x=EtrTaux2012, y=resid)) + geom_point(aes(size=Pop2012)) + geom_smooth(aes(group=1),method="lm") + theme_bw() + xlab("Proportion d'étrangers") + ylab('Résidus') + theme(legend.title=element_text("Population du canton")) + scale_size_continuous(name="Population du canton")  + theme(legend.position="bottom")
