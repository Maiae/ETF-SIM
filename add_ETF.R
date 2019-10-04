### Adding new ETF to the list of available ETF's
newList <- rbind(etfList, c("ETHI.AX",
														"BetaShares Global Sustainability Leaders ETF" ))
newList <- newList[order(newList$ASX_CODE),]
rownames(newList) <- seq(1:154)
etfList <- newList
save(etfList, file="data/etf_data.rda")