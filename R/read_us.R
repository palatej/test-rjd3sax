fileName <- "./Data/ip_nsa.txt"
connection <- file(fileName,open="r")
ls <-readLines(connection)

all<-list()
name<-NULL
cur<-NULL
year<-0
data<-NULL
for (i in 1:length(ls)){
  l<-gsub('\"', '', ls[i])
  q<-unlist(strsplit(l, ":"))
  if (length(q) == 2){
    #create the previous series if any
    if (! is.null(data)){
      s<-ts(data = data, frequency = 12, start = c(year, 1))
      all[[name]]<-s
    }
    cur<-q[1]
    name<-l
    year<-0
    data<-NULL
  }else{
    q<-unlist(strsplit(l, " +"))[-1]
    if (year == 0){
      year=as.integer(q[1])
    }
      data<-c(data, q[-1])

  }


}
