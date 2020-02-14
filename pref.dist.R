setwd("F:/A_in_prep/Chambers/data")
pref <- read.csv("pref.csv")
hist(pref$prefs)
#newdata <- mydata[ which(gender=='F' & age > 65),]
schisto <- pref[which(pref$species == 's' & pref$treatment != "empty"),]
echino <- pref[which(pref$species == 'e' & pref$treatment != "empty"),]
empty <- pref[which(pref$treatment == 'empty'),]
p.control <- pref[which(pref$treatment == 'positive'),]

s.neg <- pref[which(pref$species == 's' & pref$treatment == "empty"),]
s.positive <- pref[which(pref$species == 's' & pref$treatment == "positive"),]
s.svu <- pref[which(pref$species == 's' & pref$treatment == "svu"),]
s.evu <- pref[which(pref$species == 's' & pref$treatment == "evu"),]
s.evs <- pref[which(pref$species == 's' & pref$treatment == "evs"),]

e.neg <- pref[which(pref$species == 'e' & pref$treatment == "empty"),]
e.positive <- pref[which(pref$species == 'e' & pref$treatment == "positive"),]
e.svu <- pref[which(pref$species == 'e' & pref$treatment == "svu"),]
e.evu <- pref[which(pref$species == 'e' & pref$treatment == "evu"),]
e.evs <- pref[which(pref$species == 'e' & pref$treatment == "evs"),]

hist(schisto$prefs)
hist(echino$prefs)
hist(empty$prefs, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(p.control$prefs)

hist(s.neg$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(s.positive$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(s.svu$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(s.evu$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(s.evs$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))

hist(e.neg$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(e.positive$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(e.svu$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(e.evu$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))
hist(e.evs$prefs, breaks = 6, xlab = "Preference Index", main ='', xlim = c(-1, 1), ylim = c(0,6))

shapiro.test(empty$prefs)
shapiro.test(schisto$prefs)
shapiro.test(echino$prefs)

shapiro.test(s.neg$prefs)
shapiro.test(s.positive$prefs)
shapiro.test(s.svu$prefs)
shapiro.test(s.evu$prefs)
shapiro.test(s.evs$prefs)

shapiro.test(e.neg$prefs)
shapiro.test(e.positive$prefs)
shapiro.test(e.svu$prefs)
shapiro.test(e.evu$prefs)
shapiro.test(e.evs$prefs)
