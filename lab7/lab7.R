n = 100
selection = rnorm(n,0,1)
x_ = mean(selection)
print(x_)
s_2 = sum((selection - x_)^2)/n
sigma = sqrt(s_2)
print(sigma)


k = 1 + 3.3 * log10(n)
k = round(k, 0) - 1
print(k)
selection = sort(selection);
a = selection[1];
b = selection[n];
h = (b - a) / k;
ab<-c();
for (i in 1:k)
    ab[i] = a + (i - 1) * h;
ab[1] = -Inf;
ab[k + 1] = Inf;
print("ab")
ab
n_i <- c()
p_i <- c()
np_i <-c()
n_inp_i <- c()
ans <- c()
for(i in 1:k)
{
  n_i[i] = length(selection[selection <= ab[i+1] & selection >= ab[i]])
  p_i[i] = pnorm(ab[i + 1])- pnorm(ab[i]);
  np_i[i] = n * p_i[i]
  n_inp_i[i] = n_i[i] - np_i[i]
  ans[i] = (n_inp_i[i]^2) / np_i
}
print("n_i")
n_i
sum(n_i)
print("p_i")
p_i
sum(p_i)
print("np_i")
np_i
sum(np_i)
print("n_i-n*p_i")
n_inp_i
sum(n_inp_i)
print(ans)
ans
sum(ans)
