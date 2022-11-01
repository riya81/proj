write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"))
library(IndexNumber)
prices<-c(25,28,31,34,39,42)
index.number.serie(prices,name="Price", opt.plot=TRUE, opt.summary=FALSE)
quantities<-c(500,650,800,700,690,850)
index.number.serie(quantities,name="Quantity", opt.plot=TRUE, opt.summary=FALSE)
commodity<-c(40,50,45,55,65,70)
index.number.serie(commodity,name="Price", opt.plot=TRUE, opt.summary=FALSE) 

prices1<-matrix(c(100,125,50,40,5,140,200,80,60,10),nrow=2,byrow=TRUE)
aggregated.index.number(prices1,"serie","BDutot","Price",opt.plot=FALSE,opt.summary=FALSE)

prices2<-matrix(c(40,35,32,13,52,42,47,19),nrow=2,byrow=TRUE)
weights<-matrix(c(230,270,140,60),nrow=1,byrow=TRUE)
laspeyres.index.number(prices2,weights,"Price",opt.plot=FALSE,opt.summary=FALSE)
weight1<-matrix(c(230,270,140,60,470,330,260,140),nrow=2,byrow=TRUE)
paasche.index.number(prices2,weight1,"Price",opt.plot=FALSE,opt.summary=FALSE)
fisher.index.number(prices2,weight1,"Price",opt.plot=FALSE,opt.summary=FALSE)
edgeworth.index.number(prices2,weight1,"Price",opt.plot=FALSE,opt.summary=FALSE)


x<-4.5
y<-9.8
a<-x+y
b<-x*y
a
b

x<-4.5
y<-9.8
e<-x<=y
e

x<-4.5
y<-9.8
is.character(x)
is.numeric(x)

x<-4.5
y<-9.8
rbind(x,y)
cbind(x,y)

c<-c(23,27,19)
f<-(c*9/5)+32
f

h<-c(180,165,160,130)
w<-c(87,58,65,100)
h<-h/100
bmi<-w/(h^2)
bmi

weight<-bmi>25
weight

r<-c(0.7,0.9,0.1,0.6,5.7,4.9,8.2,8.4,9.0,11.2)
print(mean(r))
print(sd(r))
print(cumsum(r))
print(max(r))

m<-matrix(c(2:10),nrow=3,byrow=TRUE)
n<-matrix(c(12:20),nrow=3,byrow=TRUE)
m
n
x<-m+n
y<-m-n
z<-m*n
w<-m/n
x
y
z
w


l1=list(6,7,8)
l2=list(3,2,4)
print("lists")
l1
l2
print("conversion of lists to vectors:")
v1=unlist(l1)
v2=unlist(l2)
print("vectors:")
v1
v2

l<-list(1,2,9.8,7,5,9,6)
print(length(l))

l1<-list("happiness","is","everything")
l2<-list("follow","what","your","heart","says")
ml<-c(l1,l2)
print("merged list")
ml


l<-list(1,2,3,4,5,6)
m<-list(8,7,6,5,4)
l
m

a<-array(c(1:27),dim=c(3,3,3))
a






base<-matrix(c(35,30,40,107),nrow=1,byrow = TRUE)
current<-matrix(c(42,35,38,120),nrow=1,byrow=TRUE)
simp_agg=(sum(current)/sum(base))*100
print(paste("simple aggreagate index =",simp_agg))


p0<-c(10,12,14,16,18)
p1<-c(12,14,16,18,20)
p0q0<-c(100,144,196,256,324)
p1q1<-c(144,196,256,324,400)
p1q0<-c(p1*p0)
#a)
lp = (sum(p1q0)/sum(p0q0))*100
lp
#b)
psch = (sum(p1q1)/sum(p1q0))*100
psch
#c)
fisch = sqrt((sum(p1q0)/sum(p0q0))*(sum(p1q1)/sum(p1q0))*100)
fisch
#d)
tvi=sum(p1q1)/sum(p0q0)
tvi
#e)
edge=((sum(p1q0)+sum(p1q1))/(sum(p0q0)+sum(p1q0)))*100
edge
#f)
brown=(((sum(p1q0)/sum(p0q0))+(sum(p1q1)/sum(p1q0)))/2)*100
brown

library(IndexNumber)

#c
fbi<-c(376,392,408,380,392,400)
cbi=index.number.serie(fbi, "fbi", opt.plot =FALSE, opt.summary = FALSE)
cbi


x<-c(120,132,140,145,152,160,172,178,182,190)
y=152
shift=c((x/y)*100)
shift

pb<-c(30,8,14,22,25)
pc<-c(47,12,18,15,30)
w<-c(7,1,4,2,1)
p<-c((pc/pb)*100)
p
pw<-c(p*w)
pw
li=sum(pw)/sum(w)
li

p0<-c(40,30,50,20,60,50,150)
p1<-c(50,45,60,40,75,60,200)
v<-c(30,20,10,5,15,15,5)
p<-c((p1/p0)*100)
p
pv<-c(p*v)
pv
li=sum(pv)/sum(v)
li

fisch = 229.5648
x = fisch
y=480-fisch
x
y



m<-c(200,150,140,100,120)
n<-c(6,4,"x",3,4)
mn<-c(m*n)
mn
li=sum(mn)/sum(n)
print(li)


p0<-c(15.99,5.29,8.99)
p1<-c(17.99,7.49,9.39)
q0<-c(12.8,18.7,53.6)
q1<-c(14.2,18.2,59.2)

#LESPEYRES
lespeyres_QI<-(sum(q1*p0)/sum(q0*p0))*100
print(lespeyres_QI)

#PAASCHES
paasches_QI<-(sum(q1*p1)/sum(q0*p1))*100
paasches_QI

#FISHERS INDEX
fisher_index<-sqrt((sum(p1*q0)/sum(p0*q0))*(sum(p1*q1)/sum(p1*q0))*100)
print(fisher_index)

#MARSHALL EDGEWORTH QUANTITY INDEX
me<-((sum(q1)*(p0+p1/2))/(sum(q0)*(p0+p1/2)))*100
me

#BROWN INDEX
BI<-sum(q1)

#coefficient of variation
taxes<-c(5000,4500,7200,5000,3800,4100,5500,6000,2100,2230,5400,7500,6570,1280,3500,3100,5500,5000,3260,2100)
total<-sum(taxes)
mean<-total/20
print(mean)

std_dev<-sd(taxes)
print(std_dev)

coeff_var<-(std_dev/mean)*100
coeff_var

plot(taxes,coeff_var,tax,covar,pch=16)

#measures of dispersion

range<-7500-1280
range

std_dev<-sd(taxes)
print(std_dev)

var<-variance(taxes)
var

#DESCRIPTIVE STATISTICS
avg<-mean(taxes)
avg

med<-median(taxes)
med

mode<-mode(taxes)
mode

#double exponential smoothing forecast
year<-c(1994,1995,1996,1997,1998,1999,2000)
production_yt<-c(42,49,62,75,92,125,158)

#calculate alpha
n=7
alpha=2/(n+1)
alpha

beta=0.35

#forecast
Ct_1994<-c(42)
Ct_1995<-c(49-f1994)
Ct_1996<-c(62-f1995)
Ct_1997<-c(75-f1996)
Ct_1998<-c(92-f1997)
Ct_1999<-c(125-f1998)
Ct_2000<-c(158-f1999)


#Level
Ct = alpha*Yt + (1-alpha)(Ct-1 + Tt-1)
#Trend
Tt = beta(Ct-(Ct-1)) + (1 - beta)*(Tt-1)
#orecast
Ft+1 = (Ct + Tt)


Tt_1994<-c(0.35*(42-0)+(1-0.35)*0) 
Tt_1994
Tt_1995<-c(0.35*(49-42)+(1-0.35)*14.7)
Tt_1995
Tt_1996<-c(0.35*(62-49)+(1-0.35)*12.005)
Tt_1996
Tt_1997<-c(0.35*(75-62)+(1-0.35)*12.35325)
Tt_1997
Tt_1998<-c(0.35*(92-75)+(1-0.35)*12.57961)
Tt_1998
Tt_1999<-c(0.35*(125-92)+(1-0.35)*14.12675)
Tt_1999
Tt_2000<-c(0.35*(158-125)+(1-0.35)*20.73239)
Tt_2000

Tt<-c(Tt_1994,Tt_1995,Tt_1996,Tt_1997,Tt_1998,Tt_1999,Tt_2000)
Tt

Ct_1994<-c(0.25*42)
Ct_1994
Ct_1995<-c((0.25*49) + (1-0.25)*(42 + 14.7))
Ct_1995
Ct_1996<-c((0.25*62) + (1-0.25)*(49 + 12.005))
Ct_1996
Ct_1997<-c((0.25*75) + (1-0.25)*(62 + 12.35325))
Ct_1997
Ct_1998<-c((0.25*92) + (1-0.25)*(75 + 12.57961))
Ct_1998
Ct_1999<-c((0.25*125) + (1-0.25)*(92 + 14.12675))
Ct_1999
Ct_2000<-c((0.25*158) + (1-0.25)*(125 + 20.73239))
Ct_2000

Ct<-c(Ct_1994,Ct_1995,Ct_1996,Ct_1997,Ct_1998,Ct_1999,Ct_2000)


Ft_1994<-c(10.5)
Ft_1995<-Ct_1994+Tt_1994
Ft_1996<-Ct_1995+Tt_1995
Ft_1997<-Ct_1996+Tt_1996
Ft_1998<-Ct_1997+Tt_1997
Ft_1999<-Ct_1998+Tt_1998
Ft_2000<-Ct_1999+Tt_1999

Ft_1994
Ft_1995
Ft_1996
Ft_1997
Ft_1998
Ft_1999
Ft_2000

Ft<-c(Ft_1994,Ft_1995,Ft_1996,Ft_1997,Ft_1998,Ft_1999,Ft_2000)

result<-data.frame(year,production_yt,Ct,Tt,Ft)
print(result)

Ft_2001<-Ct_2000+Tt_2000
print("Forecast value of year 2001 using double exponential smoothing:-")
Ft_2001

















