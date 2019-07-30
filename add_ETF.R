### Adding new ETF to the list of available ETF's
newList <- rbind(etfList, c("CRED.AX",
														"BetaShares Australian Investment Grade Corporate Bond ETF (CRED)" ))
newList <- newList[order(newList$ASX_CODE),]
rownames(newList) <- seq(1:150)
etfList <- newList
save(etfList, file="data/etf_data.rda")