#���������� ����������� � ������� ��������� �������������
size = 1000; 
kk = 1 + 3.3 * log(size)
xx <- rcauchy(n = size, location=0, scale=1) #������������� ����
xx
hist(xx,probability = TRUE, main = paste("���������� �������������. n = 1000"),breaks = kk);
a = min(xx)
b = max(xx)
step = (b-a) / kk
x = array(c(T,F),dim=c(kk))
for(i in 1:kk + 1){
  x[i] =round( a + i * step, 0) 
}
curve(dnorm(x), col = "blue", add = T) #������ ��������� ����������� ����������� �������������

#���������� ����������� � ������� ��������� �������������
size = 1000; 
kk = 1 + 3.3 * log(size)
D <- DExp(rate = 1/sqrt(2)) #������������� �������
xx <-r(D)(size)            #������������� �������
xx
hist(xx,probability = TRUE, main = paste("�������. n = 1000"),breaks = kk);
a = min(xx)
b = max(xx)
step = (b-a) / kk
x = array(c(T,F),dim=c(kk))
for(i in 1:kk + 1){
  x[i] =round( a + i * step, 0) 
}
curve(dnorm(x), col = "blue", add = T) #������ ��������� ����������� ����������� �������������

#���������� ����������� � ������� ��������� �������������
size = 1000; 
kk = 1 + 3.3 * log(size)
xx <- rnorm(n = size, mean = 0, sd  = 1);  #���������� �������������
#xx <- rcauchy(n = size, location=0, scale=1) #������������� ����
#xx <- rpois(n = size, lambda = 10)  #������������� �������
#xx <- runif(n = size,-sqrt(3),sqrt(3))   #����������� �������������
#D <- DExp(rate = 1/sqrt(2)) #������������� �������
#xx <-r(D)(size)            #������������� �������
xx
hist(xx,probability = TRUE, main = paste("���������� �������������. n = 1000"),breaks = kk);
a = min(xx)
b = max(xx)
step = (b-a) / kk
x = array(c(T,F),dim=c(kk))
for(i in 1:kk + 1){
  x[i] =round( a + i * step, 0) 
}
curve(dnorm(x), col = "blue", add = T) #������ ��������� ����������� ����������� �������������

#���������� ����������� � ������� ��������� �������������
size = 1000; 
kk = 1 + 3.3 * log(size)
xx <- rpois(n = size, lambda = 10)  #������������� �������
xx
hist(xx,probability = TRUE, main = paste("�������. n = 1000"),breaks = kk);
a = min(xx)
b = max(xx)
step = (b-a) / kk
x = array(c(T,F),dim=c(kk))
for(i in 1:kk + 1){
  x[i] =round( a + i * step, 0) 
}
curve(dnorm(x), col = "blue", add = T) #������ ��������� ����������� ����������� �������������

#���������� ����������� � ������� ��������� �������������
size = 1000; 
kk = 1 + 3.3 * log(size)
xx <- runif(n = size,-sqrt(3),sqrt(3))   #����������� �������������
xx
hist(xx,probability = TRUE, main = paste("�����������. n = 1000"),breaks = kk);
a = min(xx)
b = max(xx)
step = (b-a) / kk
x = array(c(T,F),dim=c(kk))
for(i in 1:kk + 1){
  x[i] =round( a + i * step, 0) 
}
curve(dnorm(x), col = "blue", add = T) #������ ��������� ����������� ����������� �������������
