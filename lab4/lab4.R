#Эмперические функции рапсределения
#Для распределения ПУассона поменять промежуток на (6,14)
n = 100;
count = 5;
while(count >0){
m = round((1 + 3.322 * log10(n)), 0.1) * 20;
if (count == 1){selection = rnorm(n, 0, 1);}#Нормальное распределение 
if (count == 2){selection <- rcauchy(n = n, location=0, scale=1)} #распределение Коши
if (count == 3){selection = rlaplace(n, 0, 1/sqrt(2))} #рапсределение Лапласа
if (count == 4){selection <- rpois(n = n, lambda = 10)}  #распределение Пуассона
if (count == 5){selection <- runif(n = n,-sqrt(3),sqrt(3))} #равномерное распределение
selection = sort(selection)
a = -4;
b = 4;
step = (b - a) / m;
ni = 0
array= seq(from = a, to = b - step, by = step)
ni[1] = 0
for(i in 2:m){
  arr = selection[selection < array[i]]
  ni[i] = length(arr) / n
}
x = runif(1000, -4, 4)
x = sort(x)
if (count == 1){Y = pnorm(x, 0, 1)} #Нормальное распределение
if (count == 2){Y = pcauchy(x,0,1)}  #распределение Коши
if (count == 3){Y = plaplace(x,0, 1/sqrt(2))} #рапсределение Лапласа
#if (count == 4){Y = ppois(x,10)} #распредштеление Пуассона
if (count == 5){Y = punif(x,-sqrt(3),sqrt(3))} #равномерное распределение
plot(array, ni, type = "s", main = "n = 100")
lines(x, Y, col="#FF05F6")


#Ядерные оценки плотности распределения
if (count == 1){selection = rnorm(100, 0, 1);}#Нормальное распределение 
if (count == 2){selection = rcauchy(n = 100, location=0, scale=1)} #распределение Коши
if (count == 3){selection = rlaplace(n, 0, sqrt(1/2))} #рапсределение Лапласа
#if (count == 1){selection = rpois(n = 100, lambda = 10)}  #распределение Пуассона
if (count == 5){selection = runif(n = 100,-sqrt(3),sqrt(3))} #равномерное распределение
x = runif(1000, -4, 4)
x = sort(x)
plot(density(selection, adjust=2), xlim = c(-4, 4), ylim = c(0,0.8), col = "#FF05F6", main = "2h")
if (count == 1){lines(x, dnorm(x, 0, 1), col="black")} #Нормальное распределение 
if (count == 2){lines(x, dcauchy(x, 0, 1), col="black")} #распределение Коши
if (count == 3){lines(x, dlaplace(x,0, 1/sqrt(2) ), col="black")} #рапсределение Лапласа
#lines(round(x),dpois(round(x), 10), col="black") #распределение Пуассона
if (count == 5){lines(x, dunif(x,-sqrt(3),sqrt(3) ), col="black")} #равномерное распределение
count = count -1;
}
