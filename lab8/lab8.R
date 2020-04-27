u = 1.96
n = 20;
x = rnorm(n, 0, 1)
x_ = mean(x)
s = sqrt(sum((x-x_)^2) / n)
t = 2.093; 
left_m = x_ - s*t/sqrt(n-1)
right_m = x_ + s*t/sqrt(n-1)
t1 = 32.852; 
t2 = 8.907;  
left_s = s*sqrt(n)/sqrt(t1)
right_s = s*sqrt(n)/sqrt(t2)
left_ma = x_ - s*u/sqrt(n)
right_ma = x_ + s*u/sqrt(n)
m_4 = sum((x-x_)^4)/n
e = m4/s^4 - 3
left_sa = s * (1 - 0.5 * u * sqrt((e+2)/n))
right_sa = s * (1 + 0.5 * u * sqrt((e+2)/n))
left_m
right_m
left_s
right_s
left_ma
right_m
left_sa
right_sa
