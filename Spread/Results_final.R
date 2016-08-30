### RESULTS

factor_mag=0.2
factor_bb=9/4
factor_two=2

# SPX
# t-model
ret3s1=0.26302787*factor_mag; ret3s2=-0.1701991*factor_mag
trade3s1=4; trade3s2=0
vola3s1=0.05576258*factor_mag; vola3s2=0.05767008*factor_mag

ret9s1=-0.003201718*factor_mag; ret9s2=0.02276537*factor_mag
trade9s1=1; trade9s2=1
vola9s1=0.01252656*factor_mag; vola9s2=0.01387169*factor_mag

ret11s1=0.1154271*factor_mag; ret11s2=0.1858135*factor_mag
trade11s1=5; trade11s2=3
vola11s1=0.03739819*factor_mag; vola11s2=0.08243626*factor_mag

ret17s1=0.0999404*factor_mag; ret17s2=0.0848594*factor_mag
trade17s1=1; trade17s2=1
vola17s1=0.00153398*factor_mag; vola17s2=0.006102428*factor_mag

ret31s1=0; ret31s2=0
trade31s1=0; trade31s2=0
vola31s1=0; vola31s2=0

ret45s1=-0.1943415*factor_mag; ret45s2=-0.1943415*factor_mag
trade45s1=0; trade45s2=0
vola45s1=0.0533842*factor_mag; vola45s2=0.0533842*factor_mag

# Gaussian-model
ret3s1_g=0.11190238*factor_mag; ret3s2_g=-0.18622192*factor_mag
trade3s1_g=5; trade3s2_g=0
vola3s1_g=0.05723273*factor_mag; vola3s2_g=0.05654003*factor_mag

ret9s1_g=-0.003312878*factor_mag; ret9s2_g=0.01948689*factor_mag
trade9s1_g=1; trade9s2_g=1
vola9s1_g=0.01245601*factor_mag; vola9s2_g=0.01325699*factor_mag

ret11s1_g=0.0870965*factor_mag; ret11s2_g=0.1867792*factor_mag
trade11s1_g=5; trade11s2_g=3
vola11s1_g=0.03040009*factor_mag; vola11s2_g=0.08740133*factor_mag

ret17s1_g=0.0809415*factor_mag; ret17s2_g=0.08026586*factor_mag
trade17s1_g=1; trade17s2_g=1
vola17s1_g=0.00163489*factor_mag; vola17s2_g=0.003394132*factor_mag

ret31s1_g=0; ret31s2_g=0
trade31s1_g=0; trade31s2_g=0
vola31s1_g=0; vola31s2_g=0

ret45s1_g=0; ret45s2_g=0
trade45s1_g=0; trade45s2_g=0
vola45s1_g=0; vola45s2_g=0

# Bollinger bands
ret3s1_bb=0.002609152*factor_bb; ret3s2_bb=-0.03420021*factor_bb
trade3s1_bb=2; trade3s2_bb=1
vola3s1_bb=0.004258048*factor_bb; vola3s2_bb=0.006331942*factor_bb

ret9s1_bb=-0.0003851668*factor_bb; ret9s2_bb=-0.0007584346*factor_bb
trade9s1_bb=5; trade9s2_bb=2
vola9s1_bb=0.00120138*factor_bb; vola9s2_bb=0.001589647*factor_bb

ret11s1_bb=0.04740633*factor_bb; ret11s2_bb=0.03492264*factor_bb
trade11s1_bb=4; trade11s2_bb=1
vola11s1_bb=0.006381572*factor_bb; vola11s2_bb=0.009852952*factor_bb

ret17s1_bb=0.02803031*factor_bb; ret17s2_bb=0.03201643*factor_bb
trade17s1_bb=5; trade17s2_bb=0
vola17s1_bb=0.00841921*factor_bb; vola17s2_bb=0.009895863*factor_bb

ret31s1_bb=0.01248983*factor_bb; ret31s2_bb=-0.009611625*factor_bb
trade31s1_bb=7; trade31s2_bb=2
vola31s1_bb=0.004993835*factor_bb; vola31s2_bb=0.006524315*factor_bb

ret45s1_bb=0.01709259*factor_bb; ret45s2_bb=0.001450348*factor_bb
trade45s1_bb=5; trade45s2_bb=3
vola45s1_bb=0.007847477*factor_bb; vola45s2_bb=0.008229475*factor_bb

# Z-Score
ret3z=-0.01697756*factor_two
trade3z=1
vola3z=0.00499066*factor_two

ret9z=0.0001767541*factor_two
trade9z=1
vola9z=0.001057063*factor_two

ret11z=0.01954738*factor_two
trade11z=1
vola11z=0.005745735*factor_two

ret17z=0
trade17z=0
vola17z=0

ret31z=0
trade31z=0
vola31z=0

ret45z=0
trade45z=0
vola45z=0

###
strat1_ret=c(ret3s1,ret9s1,ret11s1,ret17s1,ret31s1,ret45s1)
strat1_ret_g=c(ret3s1_g,ret9s1_g,ret11s1_g,ret17s1_g,ret31s1_g,ret45s1_g)
strat1_ret_bb=c(ret3s1_bb,ret9s1_bb,ret11s1_bb,ret17s1_bb,ret31s1_bb,ret45s1_bb)
z_score_ret=c(ret3z,ret9z,ret11z,ret17z,ret31z,ret45z)

strat1_vola=c(vola3s1,vola9s1,vola11s1,vola17s1,vola31s1,vola45s1)
strat1_vola_g=c(vola3s1_g,vola9s1_g,vola11s1_g,vola17s1_g,vola31s1_g,vola45s1_g)
strat1_vola_bb=c(vola3s1_bb,vola9s1_bb,vola11s1_bb,vola17s1_bb,vola31s1_bb,vola45s1_bb)
z_score_vola=c(vola3z,vola9z,vola11z,vola17z,vola31z,vola45z)

plot(strat1_vola,strat1_ret,xlab="Annualised Execution Volatility",ylab="Annualised Cumulative Return",main="Mean-Reversion Strategy SPX Spread Data",ylim=c(-0.05,0.11),xlim=c(0,0.02))
points(jitter(strat1_vola_g),jitter(strat1_ret_g),col="blue")
points(jitter(strat1_vola_bb),jitter(strat1_ret_bb),col="green")
points(jitter(z_score_vola),jitter(z_score_ret),col="red")
legend("bottomright",c("t Model","Gaussian Model","Bollinger Bands","Z-Score"),pch=1,col=c("black","blue","green","red"),cex=0.8)

###
strat2_ret=c(ret3s2,ret9s2,ret11s2,ret17s2,ret31s2,ret45s2)
strat2_ret_g=c(ret3s2_g,ret9s2_g,ret11s2_g,ret17s2_g,ret31s2_g,ret45s2_g)
strat2_ret_bb=c(ret3s2_bb,ret9s2_bb,ret11s2_bb,ret17s2_bb,ret31s2_bb,ret45s2_bb)
z_score_ret=c(ret3z,ret9z,ret11z,ret17z,ret31z,ret45z)

strat2_vola=c(vola3s2,vola9s2,vola11s2,vola17s2,vola31s2,vola45s2)
strat2_vola_g=c(vola3s2_g,vola9s2_g,vola11s2_g,vola17s2_g,vola31s2_g,vola45s2_g)
strat2_vola_bb=c(vola3s2_bb,vola9s2_bb,vola11s2_bb,vola17s2_bb,vola31s2_bb,vola45s2_bb)
z_score_vola=c(vola3z,vola9z,vola11z,vola17z,vola31z,vola45z)

plot(strat2_vola,strat2_ret,xlab="Annualised Execution Volatility",ylab="Annualised Cumulative Return",main="Band-Reversion Strategy SPX Spread Data",ylim=c(-0.08,0.08),xlim=c(0,0.025))
points(jitter(strat2_vola_g),jitter(strat2_ret_g),col="blue")
points(jitter(strat2_vola_bb),jitter(strat2_ret_bb),col="green")
points(jitter(z_score_vola),jitter(z_score_ret),col="red")
legend("bottomright",c("t Model","Gaussian Model","Bollinger Bands","Z-Score"),pch=1,col=c("black","blue","green","red"),cex=0.8)

###############################

# UKX
# t-model
ret2uks1=0.0161869*factor_mag; ret2uks2=-0.006335875*factor_mag
trade2uks1=17; trade2uks2=10
vola2uks1=0.07619458*factor_mag; vola2uks2=0.09786191*factor_mag

ret8uks1=-0.1081344*factor_mag; ret8uks2=-0.02622708*factor_mag
trade8uks1=16; trade8uks2=9
vola8uks1=0.03710263*factor_mag; vola8uks2=0.05136065*factor_mag

ret12uks1=-0.4735562*factor_mag; ret12uks2=-0.6776507*factor_mag
trade12uks1=22; trade12uks2=12
vola12uks1=0.1347666*factor_mag; vola12uks2=0.163455*factor_mag

ret16uks1=0.102737*factor_mag; ret16uks2=0.04664204*factor_mag
trade16uks1=12; trade16uks2=7
vola16uks1=0.07844268*factor_mag; vola16uks2=0.09609253*factor_mag

ret20uks1=0.03700255*factor_mag; ret20uks2=0.03592568*factor_mag
trade20uks1=4; trade20uks2=2
vola20uks1=0.02406473*factor_mag; vola20uks2=0.03231834*factor_mag

# Gaussian-model
ret2uks1_g=0.02517283*factor_mag; ret2uks2_g=-0.01790553*factor_mag
trade2uks1_g=17; trade2uks2_g=10
vola2uks1_g=0.07313537*factor_mag; vola2uks2_g=0.09910574*factor_mag

ret8uks1_g=-0.1032611*factor_mag; ret8uks2_g=0.1535554*factor_mag
trade8uks1_g=19; trade8uks2_g=16
vola8uks1_g=0.04475459*factor_mag; vola8uks2_g=0.0490194*factor_mag

ret12uks1_g=-0.07803151*factor_mag; ret12uks2_g=-0.2733168*factor_mag
trade12uks1_g=23; trade12uks2_g=16
vola12uks1_g=0.1335519*factor_mag; vola12uks2_g=0.151341*factor_mag

ret16uks1_g=0.05711912*factor_mag; ret16uks2_g=0.001024157*factor_mag
trade16uks1_g=12; trade16uks2_g=7
vola16uks1_g=0.07680783*factor_mag; vola16uks2_g=0.09474794*factor_mag

ret20uks1_g=0.03933864*factor_mag; ret20uks2_g=0.05399453*factor_mag
trade20uks1_g=13; trade20uks2_g=8
vola20uks1_g=0.03812538*factor_mag; vola20uks2_g=0.06173256*factor_mag

# Bollinger bands
ret2uks1_bb=-0.003396317*factor_bb; ret2uks2_bb=0.01054831*factor_bb
trade2uks1_bb=4; trade2uks2_bb=1
vola2uks1_bb=0.006543727*factor_bb; vola2uks2_bb=0.007716208*factor_bb

ret8uks1_bb=0.006222548*factor_bb; ret8uks2_bb=0.02429683*factor_bb
trade8uks1_bb=4; trade8uks2_bb=2
vola8uks1_bb=0.003676662*factor_bb; vola8uks2_bb=0.004384032*factor_bb

ret12uks1_bb=0.08579688*factor_bb; ret12uks2_bb=0.1002127*factor_bb
trade12uks1_bb=8; trade12uks2_bb=3
vola12uks1_bb=0.01409554*factor_bb; vola12uks2_bb=0.01834178*factor_bb

ret16uks1_bb=0.002762629*factor_bb; ret16uks2_bb=0.01460136*factor_bb
trade16uks1_bb=2; trade16uks2_bb=1
vola16uks1_bb=0.004149944*factor_bb; vola16uks2_bb=0.01149574*factor_bb

ret20uks1_bb=0.006788427*factor_bb; ret20uks2_bb=-0.007094596*factor_bb
trade20uks1_bb=4; trade20uks2_bb=1
vola20uks1_bb=0.004297121*factor_bb; vola20uks2_bb=0.006876809*factor_bb

# Z-Score
ret2ukz=0.01463852*factor_two
trade2ukz=1
vola2ukz=0.002150033*factor_two

ret8ukz=0.02217547*factor_two
trade8ukz=2
vola8ukz=0.002711626*factor_two

ret12ukz=0.0240503*factor_two
trade12ukz=1
vola12ukz=0.005817352*factor_two

ret16ukz=0.01517001*factor_two
trade16ukz=3
vola16ukz=0.005700138*factor_two

ret20ukz=0.004429708*factor_two
trade20ukz=1
vola20ukz=0.00408355*factor_two

###
strat1_ret_uk=c(ret2uks1,ret8uks1,ret12uks1,ret16uks1,ret20uks1)
strat1_ret_g_uk=c(ret2uks1_g,ret8uks1_g,ret12uks1_g,ret16uks1_g,ret20uks1_g)
strat1_ret_bb_uk=c(ret2uks1_bb,ret9s1_bb,ret12uks1_bb,ret16uks1_bb,ret20uks1_bb)
z_score_ret_uk=c(ret2ukz,ret8ukz,ret12ukz,ret16ukz,ret20ukz)

strat1_vola_uk=c(vola2uks1,vola8uks1,vola12uks1,vola16uks1,vola20uks1)
strat1_vola_g_uk=c(vola2uks1_g,vola8uks1_g,vola12uks1_g,vola16uks1_g,vola20uks1_g)
strat1_vola_bb_uk=c(vola2uks1_bb,vola8uks1_bb,vola12uks1_bb,vola16uks1_bb,vola20uks1_bb)
z_score_vola_uk=c(vola2ukz,vola8ukz,vola12ukz,vola16ukz,vola20ukz)

plot(strat1_vola_uk,strat1_ret_uk,xlab="Annualised Execution Volatility",ylab="Annualised Cumulative Return",main="Mean-Reversion Strategy UKX Spread Data",xlim=c(0,0.032),ylim=c(-0.1,0.2))
points(jitter(strat1_vola_g_uk),jitter(strat1_ret_g_uk),col="blue")
points(jitter(strat1_vola_bb_uk),jitter(strat1_ret_bb_uk),col="green")
points(jitter(z_score_vola_uk),jitter(z_score_ret_uk),col="red")
legend("bottomleft",c("t Model","Gaussian Model","Bollinger Bands","Z-Score"),pch=1,col=c("black","blue","green","red"),cex=0.8)

###
strat2_ret_uk=c(ret2uks2,ret8uks2,ret12uks2,ret16uks2,ret20uks2)
strat2_ret_g_uk=c(ret2uks2_g,ret8uks2_g,ret12uks2_g,ret16uks2_g,ret20uks2_g)
strat2_ret_bb_uk=c(ret2uks2_bb,ret8uks2_bb,ret12uks2_bb,ret16uks2_bb,ret20uks2_bb)
z_score_ret_uk=c(ret2ukz,ret8ukz,ret12ukz,ret16ukz,ret20ukz)

strat2_vola_uk=c(vola2uks2,vola8uks2,vola12uks2,vola16uks2,vola20uks2)
strat2_vola_g_uk=c(vola2uks2_g,vola8uks2_g,vola12uks2_g,vola16uks2_g,vola20uks2_g)
strat2_vola_bb_uk=c(vola2uks2_bb,vola8uks2_bb,vola12uks2_bb,vola16uks2_bb,vola20uks2_bb)
z_score_vola_uk=c(vola2ukz,vola8ukz,vola12ukz,vola16ukz,vola20ukz)

plot(strat2_vola_uk,strat2_ret_uk,xlab="Annualised Execution Volatility",ylab="Annualised Cumulative Return",main="Band-Reversion Strategy UKX Spread Data",ylim=c(-0.13,0.22),xlim=c(0,0.045))
points(jitter(strat2_vola_g_uk),jitter(strat2_ret_g_uk),col="blue")
points(jitter(strat2_vola_bb_uk),jitter(strat2_ret_bb_uk),col="green")
points(jitter(z_score_vola_uk),jitter(z_score_ret_uk),col="red")
legend("bottomright",c("t Model","Gaussian Model","Bollinger Bands","Z-Score"),pch=1,col=c("black","blue","green","red"),cex=0.8)

###
# FX
# t-model
ret99s1=0.02502479*factor_two; ret99s2=0.0328493*factor_two
trade99s1=4; trade99s2=1
vola99s1=0.01900125*factor_two; vola99s2=0.02961574*factor_two

ret00s1=0.007394985*factor_two; ret00s2=-0.03105211*factor_two
trade00s1=3; trade00s2=1
vola00s1=0.06893368*factor_two; vola00s2=0.06916108*factor_two

ret02s1=0.06415557*factor_two; ret02s2=-0.02609881*factor_mag
trade02s1=3; trade02s2=1
vola02s1=0.02004834*factor_two; vola02s2=0.0268948*factor_two

ret07s1=0.03467677*factor_two; ret07s2=-0.00797954*factor_two
trade07s1=2; trade07s2=0
vola07s1=0.008762801*factor_two; vola07s2=0.02508441*factor_two

ret09s1=0.02027058*factor_two; ret09s2=-0.06020404*factor_two
trade09s1=2; trade09s2=0
vola09s1=0.01847837*factor_two; vola09s2=0.04536814*factor_two

ret12s1=0.04163803*factor_two; ret12s2=-0.07954737*factor_two
trade12s1=2; trade12s2=0
vola12s1=0.013568*factor_two; vola12s2=0.04409881*factor_two

# Gaussian-model
ret99s1_g=0.02917036*factor_two; ret99s2_g=-0.008923422*factor_two
trade99s1_g=2; trade99s2_g=1
vola99s1_g=0.01646956*factor_two; vola99s2_g=0.02674864*factor_two

ret00s1_g=0.007394985*factor_two; ret00s2_g=-0.03105211*factor_two
trade00s1_g=3; trade00s2_g=1
vola00s1_g=0.06893368*factor_two; vola00s2_g=0.06916108*factor_two

ret02s1_g=0.06244007*factor_two; ret02s2_g=0.03976952*factor_two
trade02s1_g=3; trade02s2_g=1
vola02s1_g=0.02253489*factor_two; vola02s2_g=0.02657524*factor_two

ret07s1_g=0.06260921*factor_two; ret07s2_g=0.002693786*factor_two
trade07s1_g=5; trade07s2_g=2
vola07s1_g=0.01982025*factor_two; vola07s2_g=0.02901575*factor_two

ret09s1_g=-0.003346411*factor_two; ret09s2_g=-0.02982698*factor_two
trade09s1_g=5; trade09s2_g=2
vola09s1_g=0.03496818*factor_two; vola09s2_g=0.05430604*factor_two

ret12s1_g=0.04017152*factor_two; ret12s2_g=-0.07954737*factor_two
trade12s1_g=2; trade12s2_g=0
vola12s1_g=0.01293872*factor_two; vola12s2_g=0.04388605*factor_two

# Bollinger bands
ret99s1_bb=0.06267696*factor_bb; ret99s2_bb=0.02985329*factor_bb
trade99s1_bb=4; trade99s2_bb=2
vola99s1_bb=0.02187219*factor_bb; vola99s2_bb=0.04103769*factor_bb

ret00s1_bb=-4.625104e-05*factor_bb; ret00s2_bb=0.06404183*factor_bb
trade00s1_bb=3; trade00s2_bb=1
vola00s1_bb=0.06091879*factor_bb; vola00s2_bb=0.06140447*factor_bb

ret02s1_bb=0.02829152*factor_bb; ret02s2_bb=-0.00800245*factor_bb
trade02s1_bb=3; trade02s2_bb=1
vola02s1_bb=0.02488473*factor_bb; vola02s2_bb=0.02530754*factor_bb

ret07s1_bb=0.005827422*factor_bb; ret07s2_bb=0.0899957*factor_bb
trade07s1_bb=3; trade07s2_bb=2
vola07s1_bb=0.0244336*factor_bb; vola07s2_bb=0.02650966*factor_bb

ret09s1_bb=0.02880289*factor_bb; ret09s2_bb=0.1285762*factor_bb
trade09s1_bb=4; trade09s2_bb=1
vola09s1_bb=0.05112254*factor_bb; vola09s2_bb=0.06476498*factor_bb

ret12s1_bb=0.08460031*factor_bb; ret12s2_bb=0.1086715*factor_bb
trade12s1_bb=4; trade12s2_bb=3
vola12s1_bb=0.05145464*factor_bb; vola12s2_bb=0.0472939*factor_bb

# Z-Score
ret99z=0.03880204*factor_two
trade99z=1
vola99z=0.019052*factor_two

ret00z=0
trade00z=0
vola00z=0

ret02z=-0.07213727*factor_two
trade02z=0
vola02z=0.01871216*factor_two

ret07z=0.08962868*factor_two
trade07z=2
vola07z=0.02319646*factor_two

ret09z=0.1064505*factor_two
trade09z=2
vola09z=0.03068225*factor_two

ret12z=0
trade12z=0
vola12z=0

###
strat1_ret_fx=c(ret99s1,ret00s1,ret02s1,ret07s1,ret09s1,ret12s1)
strat1_ret_g_fx=c(ret99s1_g,ret00s1_g,ret02s1_g,ret07s1_g,ret09s1_g,ret12s1_g)
strat1_ret_bb_fx=c(ret99s1_bb,ret00s1_bb,ret02s1_bb,ret07s1_bb,ret09s1_bb,ret12s1_bb)
z_score_ret_fx=c(ret99z,ret00z,ret02z,ret07z,ret09z,ret12z)

strat1_vola_fx=c(vola99s1,vola00s1,vola02s1,vola07s1,vola09s1,vola45s1)
strat1_vola_g_fx=c(vola99s1_g,vola00s1_g,vola02s1_g,vola07s1_g,vola09s1_g,vola12s1_g)
strat1_vola_bb_fx=c(vola99s1_bb,vola00s1_bb,vola02s1_bb,vola07s1_bb,vola09s1_bb,vola12s1_bb)
z_score_vola_fx=c(vola99z,vola00z,vola02z,vola07z,vola09z,vola12z)

plot(strat1_vola_fx,strat1_ret_fx,xlab="Annualised Execution Volatility",ylab="Annualised Cumulative Return",main="Mean-Reversion Strategy FX Spread Data",ylim=c(-0.18,0.22),xlim=c(0,0.16))
points(jitter(strat1_vola_g_fx),jitter(strat1_ret_g_fx),col="blue")
points(jitter(strat1_vola_bb_fx),jitter(strat1_ret_bb_fx),col="green")
points(jitter(z_score_vola_fx),jitter(z_score_ret_fx),col="red")
legend("bottomright",c("t Model","Gaussian Model","Bollinger Bands","Z-Score"),pch=1,col=c("black","blue","green","red"),cex=0.8)

###
strat2_ret_fx=c(ret99s2,ret00s2,ret02s2,ret07s2,ret09s2,ret12s2)
strat2_ret_g_fx=c(ret99s2_g,ret00s2_g,ret02s2_g,ret07s2_g,ret09s2_g,ret12s2_g)
strat2_ret_bb_fx=c(ret99s2_bb,ret00s2_bb,ret02s2_bb,ret07s2_bb,ret09s2_bb,ret12s2_bb)
z_score_ret_fx=c(ret99z,ret00z,ret02z,ret07z,ret09z,ret12z)

strat2_vola_fx=c(vola99s2,vola00s2,vola02s2,vola07s2,vola09s2,vola12s2)
strat2_vola_g_fx=c(vola99s2_g,vola9s2_g,vola02s2_g,vola07s2_g,vola09s2_g,vola12s2_g)
strat2_vola_bb_fx=c(vola99s2_bb,vola00s2_bb,vola02s2_bb,vola07s2_bb,vola09s2_bb,vola12s2_bb)
z_score_vola_fx=c(vola99z,vola00z,vola02z,vola07z,vola09z,vola12z)

plot(strat2_vola_fx,strat2_ret_fx,xlab="Annualised Execution Volatility",ylab="Annualised Cumulative Return",main="Band-Reversion Strategy SPX Spread Data",ylim=c(-0.2,0.35),xlim=c(0,0.18))
points(jitter(strat2_vola_g_fx),jitter(strat2_ret_g_fx),col="blue")
points(jitter(strat2_vola_bb_fx),jitter(strat2_ret_bb_fx),col="green")
points(jitter(z_score_vola_fx),jitter(z_score_ret_fx),col="red")
legend("bottomright",c("t Model","Gaussian Model","Bollinger Bands","Z-Score"),pch=1,col=c("black","blue","green","red"),cex=0.8)

######################## TABLES ##########################

spx1=matrix(c(strat1_ret,strat1_ret_g,strat1_ret_bb,z_score_ret,strat1_vola,strat1_vola_g,strat1_vola_bb,z_score_vola),ncol=6,nrow=8,byrow=TRUE)
rownames(spx1)=c("Returns (t)","Returns (Gaussian)","Returns (Bollinger)","Returns (Z-Score)","Volatility (t)","Volatility (Gaussian)","Volatility (Bollinger)","Volatility (Z-Score)")
colnames(spx1)=c("1991","1995","1996","1999","2006","2013")

(spx1_table=as.table(spx1))

spx2=matrix(c(strat2_ret,strat2_ret_g,strat2_ret_bb,z_score_ret,strat2_vola,strat2_vola_g,strat2_vola_bb,z_score_vola),ncol=6,nrow=8,byrow=TRUE)
rownames(spx2)=c("Returns (t)","Returns (Gaussian)","Returns (Bollinger)","Returns (Z-Score)","Volatility (t)","Volatility (Gaussian)","Volatility (Bollinger)","Volatility (Z-Score)")
colnames(spx2)=c("1991","1995","1996","1999","2006","2013")

(spx2_table=as.table(spx2))

########################################################

ukx1=matrix(c(strat1_ret_uk,strat1_ret_g_uk,strat1_ret_bb_uk,z_score_ret_uk,strat1_vola_uk,strat1_vola_g_uk,strat1_vola_bb_uk,z_score_vola_uk),ncol=5,nrow=8,byrow=TRUE)
rownames(ukx1)=c("Returns (t)","Returns (Gaussian)","Returns (Bollinger)","Returns (Z-Score)","Volatility (t)","Volatility (Gaussian)","Volatility (Bollinger)","Volatility (Z-Score)")
colnames(ukx1)=c("2003","2006","2008","2011","2013")

(ukx1_table=as.table(ukx1))

ukx2=matrix(c(strat2_ret_uk,strat2_ret_g_uk,strat2_ret_bb_uk,z_score_ret_uk,strat2_vola_uk,strat2_vola_g_uk,strat2_vola_bb_uk,z_score_vola_uk),ncol=5,nrow=8,byrow=TRUE)
rownames(ukx2)=c("Returns (t)","Returns (Gaussian)","Returns (Bollinger)","Returns (Z-Score)","Volatility (t)","Volatility (Gaussian)","Volatility (Bollinger)","Volatility (Z-Score)")
colnames(ukx2)=c("2003","2006","2008","2011","2013")

(ukx2_table=as.table(ukx2))

#########################################################
fx1=matrix(c(strat1_ret_fx,strat1_ret_g_fx,strat1_ret_bb_fx,z_score_ret_fx,strat1_vola_fx,strat1_vola_g_fx,strat1_vola_bb_fx,z_score_vola_fx),ncol=6,nrow=8,byrow=TRUE)
rownames(fx1)=c("Returns (t)","Returns (Gaussian)","Returns (Bollinger)","Returns (Z-Score)","Volatility (t)","Volatility (Gaussian)","Volatility (Bollinger)","Volatility (Z-Score)")
colnames(fx1)=c("1999","2000","2002","2007","2009","2012")

(fx1_table=as.table(fx1))

fx2=matrix(c(strat2_ret_fx,strat2_ret_g_fx,strat2_ret_bb_fx,z_score_ret_fx,strat2_vola_fx,strat2_vola_g_fx,strat2_vola_bb_fx,z_score_vola_fx),ncol=6,nrow=8,byrow=TRUE)
rownames(fx2)=c("Returns (t)","Returns (Gaussian)","Returns (Bollinger)","Returns (Z-Score)","Volatility (t)","Volatility (Gaussian)","Volatility (Bollinger)","Volatility (Z-Score)")
colnames(fx2)=c("1999","2000","2002","2007","2009","2012")

(fx2_table=as.table(fx2))
