options(stringsAsFactors = F)
df <- read.csv("~/Desktop/Yahoo Ticker Symbols - Jan 2016.csv")

df[df$X.1=="MCX",1]

df[df$X.1=="HKG",]

df[df$X.1=="LSE",]

z <- (df[match(unique(df$X.1), df$X.1),3:4])[-(1:2),]


Mexico = c("MEX")
Brazil = "SAO"
Australia = c("NCM","ASX","SHH")
Italy = c("TLO","MIL")
Singapore = c("SES")
HongKong = c("HKG")
UK = c("LSE","IOB","ISE")
Russia = "MCX"
Canada = c("TOR","VAN")
France = "PAR"
Germany = c("FRA","DUS","BER","MUN","EUX","GER","STU","HAN","HAM")

