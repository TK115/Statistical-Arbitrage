#make .csv for UKX data

sp2ukout=spreadout2uk*10
sp8ukout=spreadout8uk*10
sp12ukout=spreadout12uk*10
sp16ukout=spreadout16uk*10
sp20ukout=spreadout20uk*10

#95% CI all models
spreads_8uk_10_95=data.frame(sp2ukout,sp8ukout,sp12ukout,sp16ukout,sp20ukout,meanout2uk,lowout2uk,upout2uk,meanout2uk_g,lowout2uk_g,upout2uk_g,meanout8uk,lowout8uk,upout8uk,meanout8uk_g,lowout8uk_g,upout8uk_g,meanout12uk,lowout12uk,upout12uk,meanout12uk_g,lowout12uk_g,upout12uk_g,meanout16uk,lowout16uk,upout16uk,meanout16uk_g,lowout16uk_g,upout16uk_g,meanout20uk,lowout20uk,upout20uk,meanout20uk_g,lowout20uk_g,upout20uk_g)

write.csv(spreads_8uk_10_95,"spreads_8uk_10_95.csv")