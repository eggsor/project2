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


dset=data.frame( txtdata)#, picdata

x=t(dset)
ytotal=labeltree
as.matrix(ytotal)
yt=t(ytotal)
y1=as.numeric(yt[1,])
y=t(y1)



alpha=0.05
beta= 0.1
tol=1e-8
M=5000

loss=c(0)

a0=0
a1=t(as.numeric(integer(len)))
a0_last=0
a1_last=a1

i=1
res=c(0)

lenbeta=t(as.numeric(replicate(len,beta)))
lenalpha=t(as.numeric(replicate(len,alpha)))
ty=t(y)
#time
t1=proc.time()

repeat{
  
  wida0=t(as.numeric(replicate(wid,a0)))
  
  J0 <- (wida0+a1%*%x-y)%*%t(wida0+a1%*%x-y)
  print(3)
  
  
  a0_hat <- a0 - alpha*mean(wida0+a1%*% x-y) +beta*a0_last
  
  
  a1_hat <- a1 - lenalpha*((wida0+a1%*% x-y)%*%t(x)) +lenbeta*a1_last
  print(2)
  
  a0_last=a0_hat-a0
  wida0h=t(as.numeric(replicate(wid,a0_hat)))
  a1_last=a1_hat-a1
  
  a0 <- a0_hat
  a1 <- a1_hat
  
  J1 <- (wida0h+a1%*%x-y)%*%t(wida0h+a1%*%x-y)
  
  res <- append(res,a0)
  print(1)
  
  print(J1)
  print(J0)
  loss=append(loss,J0)
  if( abs(J1-J0) < tol  )
    break
  i <- i + 1
}
t2=proc.time()
t=t2-t1
print(t)

iteration=2:length(loss)
heavy_ball_loss=loss
plot(iteration,heavy_ball_loss[iteration], type = 'l')

print(length(res))

