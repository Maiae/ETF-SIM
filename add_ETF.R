### Adding new ETF to the list of available ETF's
newList <- rbind(etfList, c("HACK.AX","BetaShares Global Cybersecurity ETF" ))
newList <- newList[order(newList$ASX_CODE),]
rownames(newList) <- seq(1:149)
etfList <- newList
save(etfList, file="data/etf_data.rda")