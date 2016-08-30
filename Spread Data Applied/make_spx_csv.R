#make .csv for SPX data

sp3in=spreadin3*10
sp3out=spreadout3*10
sp9in=spreadin9*10
sp9out=spreadout9*10
sp11in=spreadin11*10
sp11out=spreadout11*10
sp17in=spreadin17*10
sp17out=spreadout17*10
sp31in=spreadin31*10
sp31out=spreadout31*10
sp45in=spreadin45*10
sp45out=spreadout45*10

# 90% CI without SV-model (with "in" data)
spreads_20_10_90=data.frame(sp3in,sp3out,sp9in,sp9out,sp11in,sp11out,sp17in,sp17out,sp31in,sp31out,sp45in,sp45out,meanin3,lowin3,upin3,meanin3_g,lowin3_g,upin3_g,meanout3,lowout3,upout3,meanout3_g,lowout3_g,upout3_g,meanin9,lowin9,upin9,meanin9_g,lowin9_g,upin9_g,meanout9,lowout9,upout9,meanout9_g,lowout9_g,upout9_g,meanin11,lowin11,upin11,meanin11_g,lowin11_g,upin11_g,meanout11,lowout11,upout11,meanout11_g,lowout11_g,upout11_g,meanin17,lowin17,upin17,meanin17_g,lowin17_g,upin17_g,meanout17,lowout17,upout17,meanout17_g,lowout17_g,upout17_g,meanin31,lowin31,upin31,meanin31_g,lowin31_g,upin31_g,meanout31,lowout31,upout31,meanout31_g,lowout31_g,upout31_g,meanin45,lowin45,upin45,meanin45_g,lowin45_g,upin45_g,meanout45,lowout45,upout45,meanout45_g,lowout45_g,upout45_g)

write.csv(spreads_20_10_90,"spreads_20_10_90.csv")

#95% CI all models
spreads_20_1_95=data.frame(sp3out,sp9out,sp11out,sp17out,sp31out,sp45out,meanout3,lowout3,upout3,meanout3_g,lowout3_g,upout3_g,meanout3_sv,lowout3_sv,upout3_sv,meanout9,lowout9,upout9,meanout9_g,lowout9_g,upout9_g,meanout9_sv,lowout9_sv,upout9_sv,meanout11,lowout11,upout11,meanout11_g,lowout11_g,upout11_g,meanout11_sv,lowout11_sv,upout11_sv,meanout17,lowout17,upout17,meanout17_g,lowout17_g,upout17_g,meanout17_sv,lowout17_sv,upout17_sv,meanout31,lowout31,upout31,meanout31_g,lowout31_g,upout31_g,meanout31_sv,lowout31_sv,upout31_sv,meanout45,lowout45,upout45,meanout45_g,lowout45_g,upout45_g,meanout45_sv,lowout45_sv,upout45_sv)

write.csv(spreads_20_10_95,"spreads_20_10_95.csv")