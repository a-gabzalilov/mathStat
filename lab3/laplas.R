#Построение гистограммы и функции плотности распределения
size = 20; 
kk = 1 + 3.3 * log(size)
size1 = 100;
D <- DExp(rate = 1/sqrt(2)) #распределение Лапласа
xx <-r(D)(size)            #распределение Лапласа
yy <-r(D)(size1)            #распределение Лапласа
xx
a = min(xx)
b = max(xx)
step = (b-a) / kk
x = array(c(T,F),dim=c(kk))
y = array(c(T,F),dim=c(kk))
for(i in 1:kk + 1){
  x[i] =round( a + i * step, 0) 
  y[i] =round( a + i * step, 0) 
}
boxplot(xx, yy)