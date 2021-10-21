w1<-function(t0,R)
{
  22200*(1+0.04)^(R-t0)*((1+0.04)^(t0-19)-1)/0.04
}

w<-function(R)
{
  22200*(1+0.04)^(R-19)-1/0.04
}

h1<-function(R,t0)
{
  3783*(1+0.04)^(R-75.64)*((1+0.04)^(75.64-t0+1)-1)/0.04
}

h2<-function(R,t0)
{
  3783*(1+0.04)^(R-75.64)*((1+0.04)^(75.64-t0+1)-1)/0.04
}

v1<-function(R,t0)
{
  11100*(1+0.04)^(R-75.64)*((1+0.04)^(75.64-t0+1)-1)/0.04
}

v<-function(R,t0)
{
  11100*(1-(1+0.04)^(R-75.64))/0.04
}
  
  eu<-function(R,t0){
    out=0
    R=ceiling(R)
    for (x in 1:(R-1)){
      out=out+0.0007*0.0687*exp(0.0687*x)*(log(22667.6*w1(R,t0))+log(22667.6*v1(R,t0))-log(1777.4*h1(R,t0)))
    }
    #out=out+(0.03+0.007*0.0687*exp(0.0687*R))*(log(22667.6*w(R))+log(22667.6*v(R,t0))-log(1777.4*h2(R,t0)))+(log(22667.6*w(R))+log(22667.6*v(R,t0)))*(1-0.0007*exp(0.0687*75.64)-0.03)
    for (x in (R+1):(75.64-1)){
      out=out+0.0007*0.0687*exp(0.0687*x)*(log(22667.6*w(R))+log(22667.6*v(R,t0))-log(1777.4*h2(R,t0)))
    }
    out=out+(0.03+0.007*0.0687*exp(0.0687*R))*(log(22667.6*w(R))+log(22667.6*v(R,t0))-log(1777.4*h2(R,t0)))+(log(22667.6*w(R))+log(22667.6*v(R,t0)))*(1-0.0007*exp(0.0687*75.64)-0.03)
    out
  }

 optimize(eu, interval=c(50, 75), maximum =TRUE,t0=20)
  eu(67,30)

  y<-double(20)
  for (i in 1:20)
    y[i]=eu(55+i,30)
  which.max(y)
  #####
来自最优化optim的帮助文件

#定义函数
fw <- function (x)
  10*sin(0.3*x)*sin(1.3*x^2) + 0.00001*x^4 + 0.2*x+80

#花函数图像    
plot(fw, -50, 50, n = 1000, main = "optim() minimising 'wild function'")

#寻找最优点
res <- optim(50, fw, method = "SANN",
             control = list(maxit = 20000, temp = 20, parscale = 20))
res
