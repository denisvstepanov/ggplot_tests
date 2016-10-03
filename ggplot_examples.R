library(ggplot2)
x <- runif(100,-1,1)
y <- rnorm(100,0,1)
df = data.frame('x'=x,'y'=25*x**2+y, 'z'=-25*x**2-y)
df['class'] <- sign(x)
# (1)
ggplot(df,aes(x,y,color=y))+geom_point()
# (2)
ggplot(df,aes(x,y,color=y))+geom_path()
# (3)
ggplot(df,aes(x,y,color=y))+geom_line()
# (4)
ggplot(df,aes(x,color=y))+geom_bar(stat='bin',bins=20)
ggplot(df,aes(x,color=y))+geom_bar(stat='identity')
# (5)
ggplot(df,aes(x,y,color=y))+geom_smooth()
ggplot(df,aes(x,y,color=y))+geom_smooth(method = 'glm')
ggplot(df,aes(x,y,color=y))+geom_smooth(span=0.1)
# factors
x <- runif(100,-3,3)
x <- as.integer(x)
y <- rnorm(100,0,1)
df = data.frame('x'=x,'y'=25*x**2+y, 'z'=-25*x**2-y)
df['class'] <- sign(x)
ggplot(df,aes(x,y,color=y, shape=factor(x)))+geom_point()
ggplot(df,aes(x,y,color=y, shape=x))+geom_point()
# ====== geom_point examples ===================================================
# (1)
base = ggplot(df,aes(x,y))
# (2)
base+geom_point()
# (3)
base+geom_point(aes(color=df$class))
# (4)
base+geom_point(aes(shape=factor(df$class)))
# (5)
base+geom_point(aes(size = y))
# (6)
base+geom_point(aes(color=y))+scale_colour_gradient(low = "red")
# (7)
base+geom_point(aes(shape=factor(df$class))) + scale_shape(solid = FALSE)
# (8)
base+geom_point(color='red',size=3,shape=0)
# (9)
base+geom_point(alpha=0.5)
# (10)
base+geom_point(shape=21,color='red',fill='green',size=5,stroke=5)
# (11) aesthetics
base+geom_point(aes(y,x,color=y,alpha=1,fill=df$class))
base+geom_point(aes(y,x,color=y,shape=factor(df$class),size=5,alpha=0.1,fill=x))
# ====== geom_path examples ====================================================
# generate geometric brownian motion
T = 1
N = 1000
t = seq(0,N-1,1)/N
dt = T/N
mu = 0.5
S0 = 0.5
sigma = 0.1
# standard brownian motion
W = rnorm(N)
W = cumsum(W)*sqrt(dt)
# geometric brownian motion
X = (mu - sigma**2*0.5)*t+sigma*W
S = S0*exp(X)
df = data.frame('t'=t,'S'=S,'W'=W)
library(reshape2)
df = melt(df,id.vars = "t", measure.vars = c("S","W"))
# (0)
base = ggplot(df,aes(x=t,y=value,color=variable))
# (1)
base+geom_path()
base+geom_line()
base+geom_path(color='red',size=1)
# (2)
base+geom_path(size=3,aes(color=variable,alpha=0.5))
base+geom_path(size=3,alpha=0.5,aes(color=variable))
base+geom_line(size=3,aes(color=variable,alpha=0.5))

base+geom_line(aes(color=variable,alpha=0.5),size=3)

base+geom_line(aes(color=t))

ggplot(df,aes(x=t,y=value,group=round(df$value),color=round(df$value)))+geom_path()
round(df$value)
# ====== geom_smooth examples ==================================================
X = rnorm(N)
Y = rnorm(N)+5
t = seq(0,N-1,1)
df = data.frame('t'=t,'X'=X,'Y'=Y)
df = melt(df,id.vars = "t", measure.vars = c("X","Y"))
base = ggplot(df,aes(x=t,y=value,color=variable))
base+geom_smooth(span=0.3)
# ====== geom_bar examples =====================================================
# (1)
ggplot(df,aes(x=value,color=variable,fill=variable))+geom_bar(stat='bin')
# (2)
ggplot(df,aes(x=value,group=variable))+geom_bar(stat='bin')
# ====== geom_histogram examples ===============================================
# (1)
ggplot(df,aes(x=value,color=variable,fill=variable))+geom_histogram()
# (2)
ggplot(df,aes(x=value,color=variable))+geom_histogram()
# (3)
ggplot(df,aes(x=value,fill=variable))+geom_histogram()
# (4)
ggplot(df,aes(x=value,fill=variable))+geom_histogram(stat='bin',bins=10)
# (5)
ggplot(df,aes(x=value,group=variable))+geom_histogram(stat='bin',bins=10)
ggplot(df,aes(x=value))+geom_histogram(stat='bin',bins=10)
ggplot(df,aes(x=value,y=variable))+geom_histogram()
# ====== geom_tile examples ====================================================
require(mvtnorm)
x1 = seq(-3, 3, length.out=100)
x2 = seq(-3, 3, length.out=100)
z = matrix(0, length(x1), length(x2))
for (i in 1:length(x1)) {
  a = x1
  b = x2[i]
  z[,i] = dmvnorm(cbind(a,b))
}
# reshape the data
require(reshape2)
dat <- melt(z)
# (1)
ggplot(dat, aes(x=Var2, y=Var1))+geom_contour(aes(z=value))
# (2) incorrect binwidth
ggplot(dat, aes(x=Var2, y=Var1))+geom_contour(aes(z=value),binwidth=0.05)
# (3) 
ggplot(dat, aes(x=Var2, y=Var1))+geom_contour(aes(z=value),bins=30)
# (4)
ggplot(dat, aes(x=Var2, y=Var1, z=value))+geom_tile(aes(fill=value))
# (5)
ggplot(dat, aes(x=Var2, y=Var1, z=value))+geom_tile(aes(fill=value),linetype='dotted',color='black',size=2)
# (6)
ggplot(dat, aes(x=Var2, y=Var1))+geom_tile(aes(fill=value),linetype='dotted')
# (7)
ggplot(dat, aes(x=Var2, y=Var1))+geom_contour(aes(z=value),position = position_dodge(width=0.01))
# (8) no output:
ggplot(dat, aes(x=Var2, y=Var1))+geom_contour(aes(fill=value),position ='stack')
# ====== scale examples ========================================================
x <- runif(100,-3.1,3.1)
x <- as.integer(x)
y <- rnorm(100,0,1)
df = data.frame('x'=x,'y'=25*x**2+y, 'z'=-25*x**2-y)
df['class'] <- sign(x)
breaks <- c(-3,-2,-1,0,1,2,3)
lab <- c("-3","-2","-1","0","1","2","3")
ggplot(df,aes(factor(x)))+geom_bar()+scale_x_discrete(name='x',breaks=breaks,labels=lab)
ggplot(df,aes(x))+geom_bar()+scale_x_continuous(name='x',breaks=breaks,labels=lab)

