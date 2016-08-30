#make .csv for FX data

#95% CI all models
spreads_FX_95=data.frame(NZDUSD_ZARUSD1999out[1:126],NZDUSD_ZARUSD2000out[1:126],NZDUSD_ZARUSD2002out[1:126],NZDUSD_ZARUSD2007out[1:126],NZDUSD_ZARUSD2009out[1:126],NZDUSD_ZARUSD2012out[1:126],meanout99[1:126],lowout99[1:126],upout99[1:126],meanout99_g[1:126],lowout99_g[1:126],upout99_g[1:126],meanout00[1:126],lowout00[1:126],upout00[1:126],meanout00_g[1:126],lowout00_g[1:126],upout00_g[1:126],meanout02[1:126],lowout02[1:126],upout02[1:126],meanout02_g[1:126],lowout02_g[1:126],upout02_g[1:126],meanout07[1:126],lowout07[1:126],upout07[1:126],meanout07_g[1:126],lowout07_g[1:126],upout07_g[1:126],meanout09[1:126],lowout09[1:126],upout09[1:126],meanout09_g[1:126],lowout09_g[1:126],upout09_g[1:126],meanout12[1:126],lowout12[1:126],upout12[1:126],meanout12_g[1:126],lowout12_g[1:126],upout12_g[1:126])

write.csv(spreads_FX_95,"spreads_FX_95.csv")