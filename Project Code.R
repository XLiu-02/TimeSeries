# read data #
champ = read.csv("/Users/xiaoyingziliu/Downloads/monthly_champagne_sales.csv")

# convert to ts #
ts <- ts(champ$Sales, start = c(1964,1), frequency = 12)
plot(ts,main="Monthly sales of French champagne",xlab="Year",ylab="Monthly sales (millions) ")
plot(decompose(ts,type=c("multiplicative")))
plotts.sample.wge(ts)

# linear trend
co.wge(ts)
wbg.boot.wge(ts)

# unit root
h=ur.kpss(ts,type='mu',lags='short')

## Modeling and Evaluation##
## ARMA
aic5.wge(ts,p=0:10,q=0:2)
fit92=est.arma.wge(ts,p=9,q=2)
ljung.wge(fit92$res,p=9,q=2)
ljung.wge(fit92$res,p=9,q=2,K=48) 
plotts.sample.wge(fit92$res,lag.max=48,arlimits=TRUE)
fore.92=fore.arma.wge(ts,phi=fit92$phi,theta = fit92$theta, n.ahead=20,lastn = TRUE, limits=FALSE)
fitar92=gen.arma.wge(n=100, phi = fit92$phi, theta = fit92$theta,  sn=617)
plotts.sample.wge(fitar92)

## ARIMA
d1=artrans.wge(ts,phi.tr = 1)
aic5.wge(d1,p=0:10,q=0:2)
fit10=est.arma.wge(d1,p=10,q=0)
ljung.wge(fit10$res,p=10,q=0)
ljung.wge(fit10$res,p=10,q=0,K=48) 
plotts.sample.wge(fit10$res,lag.max=48,arlimits=TRUE)
fore.10=fore.arima.wge(ts,phi=fit10$phi, d=1, theta= 0, n.ahead=20,lastn = TRUE, limits=FALSE)
fitar10=gen.arima.wge(n=100, phi = fit10$phi,d=1,theta=0, sn=17)
plotts.sample.wge(fitar10)

## season
est.ar.wge(ts,p=15,type = 'burg')
sB12=artrans.wge(ts,phi.tr = c(rep(0,11),1))
plotts.sample.wge(sB12)
aic5.wge(sB12,p=0:10,q=0:2)
fit1=est.ar.wge(sB12,p=1,type = 'burg')
ljung.wge(fit1$res,p=1,q=0)
ljung.wge(fit1$res,p=1,q=0,K=48) 
plotts.sample.wge(fit1$res,lag.max=48,arlimits=TRUE)
fore.1=fore.arima.wge(ts,phi=fit1$phi,s=12,n.ahead=20,lastn = TRUE, limits=FALSE)
fitar1=gen.arima.wge(n=100, phi = fit1$phi,s=12,sn=455)
plotts.sample.wge(fitar1)