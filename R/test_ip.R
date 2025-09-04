ltd<-function(s, log=FALSE){
  if (is.null(s)) return (NULL)
  if (length(s)<36) return(NULL)
  if (any(is.na(s))) return(NULL)
  if (!any(s!=0)) return(NULL)
  if (log && any(s<=0)) return(NULL)
  if (log)s<-log(s)
  td<-rjd3toolkit::td(s=s)
  easter<-rjd3toolkit::easter_variable(s=s)

  q<-rjd3sax::ltdarima_estimation(s, regular=c(0,1,1), seasonal=c(0,1,1), X=cbind(td, easter), eps=1e-15)
  return(q)
}

ww<-NULL
zz<-NULL

daic<-function(s, title){
  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z))})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$initial$likelihood$aic-r$final$likelihood$aic})
  ww<<-c(ww,z)
  hist(z, breaks=15, main=title)

  print(sum(z>0)/length(z))

  z<-lapply(s, function(z){ltd(rjd3toolkit::clean_extremities(z), TRUE)})
  sel<-sapply(z, is.null)
  if (any(sel))z<-z[-which(sel)]

  z<-sapply(z, function(r){r$initial$likelihood$aic-r$final$likelihood$aic})
  zz<<-c(zz,z)
  hist(z, breaks=15, main=paste0(title, "(logs)"))

  print(sum(z>0)/length(z))

}

load("./Data/IP_BELGIUM")
daic(IP_Belgium, "Belgium")

load("./Data/IP_GERMANY")
daic(IP_Germany, "Germany")

load("./Data/IP_FRANCE")
daic(IP_France, "France")

load("./Data/IP_SPAIN")
daic(IP_Spain, "Spain")

load("./Data/IP_ITALY")
daic(IP_Italy, "Italy")



