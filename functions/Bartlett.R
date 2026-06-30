## Bartlett: 
## sqrt(n)( hat.rho[h] - rho[h]) aprox Normal(0, w[h])
Bartlett = function(ma = NULL, ar = NULL, m = 3000, lag.max = 10){
rho = ARMAacf(ma = ma, ar = ar, lag.max = lag.max+m)
j = 1:m
w = c()
for(h in 1:lag.max){
	w[h] = sum((rho[abs(h+j)+1]+rho[abs(h-j)+1]-2*rho[h+1]*rho[j+1])^2)
}
w
}