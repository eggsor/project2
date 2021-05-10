library(e1071)
#multi
setwd("C:\\Users\\hp\\Desktop\\mgs\\double")
txtdata<-read.table("textmul.txt",head=FALSE);
picdata<-read.table("imagemul.txt",head=FALSE);
labeltree<- read.table("label.txt", head=FALSE);
lent=length(txtdata[1,])
lenp=length(picdata[1,])
len=lent#+lenp
wid=length(labeltree[,1])


dset=data.frame(txtdata)#, picdata)

xset=t(dset)
ytotal=labeltree
as.matrix(ytotal)
yt=t(ytotal)
y1=as.numeric(yt[1,])
y=t(y1)

c=t(as.numeric(replicate(wid,1)))
x=rbind(xset,c)




tol=1e-4
M=5000

len=len+1


a1=t(as.numeric(integer(len)))
a1_last=a1

i=1
res=c(0)
loss=c(0)

ty=t(y)
A=x%*%t(x)

B1=t(as.numeric(replicate(len,-1)))#*(x%*%ty)
B2=x%*%ty
B=t(B1)*B2
d=0
beta=0
grad=-B

t1=proc.time()

repeat{
  
 
  
  J0 <- (a1%*%x-y)%*%t(a1%*%x-y)

  
  #a0_hat <- a0 - alpha*mean(wida0+a1%*% x-y) +beta*a0_last
  
  lenbeta=t(as.numeric(replicate(len,abs(beta))))
  d=grad+t(lenbeta)*d #d lie
  com=t(d)%*%A%*%d
  Ad=A%*%d
  alpha=-t(d)%*%grad/com
 # print(alpha)
  
  lenalpha=t(as.numeric(replicate(len,-abs(alpha))))
  
  
  a1_hat <- a1 - lenalpha*t(d)
  a1 <- a1_hat
  print(2)
  
  grad=t(-a1_hat%*%A)-B
  beta=(t(grad)%*%A%*%d)/com
  #print(beta)

  
  
  J1 <- (a1%*%x-y)%*%t(a1%*%x-y)
  
  res <- append(res,1)
  print(1)
  
  loss=append(loss,J0)
  print(J0)
  
  if( abs(J1-J0) < 1e-8 )
    break
  i <- i + 1
}

t2=proc.time()
t=t2-t1
print(t)

iteration=2:length(loss)
conjugate_gradient_loss=loss
plot(iteration,conjugate_gradient_loss[p], type = 'l')

print(length(res))

