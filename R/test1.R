# simple example using Retail trade statistics
# airline model

s<-rjd3toolkit::Retail$RetailSalesTotal

q0<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1))

print(q0$initial$likelihood$bic)
print(q0$final$likelihood$bic)
# same with calendar effects

td<-rjd3toolkit::td(s=s)
easter<-rjd3toolkit::easter_variable(s=s)

q1<-rjd3sax::ltdarima_estimation(log(s), regular=c(0,1,1), seasonal=c(0,1,1), X=cbind(td, easter), eps=1e-15)

print(q1$initial$likelihood$bic)
print(q1$final$likelihood$bic)
matplot(cbind(q1$initial$regression$coefficients, q1$final$regression$coefficients), type='b', col=c('gray', 'blue'))


q2<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), X=cbind(td, easter), fixed_var=FALSE, eps=1e-15)

print(q2$initial$likelihood$bic)
print(q2$final$likelihood$bic)
matplot(cbind(q2$initial$regression$coefficients, q2$final$regression$coefficients), type='b', col=c('gray', 'blue'))


