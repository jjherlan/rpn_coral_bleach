require(data.table)
data <- fread("C://ESMOI_HOBO_Temps.csv",sep=',')
str(data)
Daily_Mean1 <- data[,list(avg=mean(H10530652)),by=list(Date)]
Daily_Mean2 <- data[,list(avg=mean(H10530651)),by=list(Date)]
Daily_Mean3 <- data[,list(avg=mean(H10530653)),by=list(Date)]
Daily_Mean4 <- data[,list(avg=mean(H10530652b)),by=list(Date)]

#dataset is sorted by day

attach(Daily_Mean1)
summary(Daily_Mean1)
g1<- ggplot(Daily_Mean1, aes(Date, avg))
is.data.frame(Daily_Mean1)
newDate1 <- as.Date(Date, format="%m/%d/%Y")

attach(Daily_Mean2)
summary(Daily_Mean2)
g1<- ggplot(Daily_Mean2, aes(Date, avg))
is.data.frame(Daily_Mean2)
newDate2 <- as.Date(Date, format="%m/%d/%Y")

attach(Daily_Mean3)
summary(Daily_Mean3)
g1<- ggplot(Daily_Mean3, aes(Date, avg))
is.data.frame(Daily_Mean3)
newDate3 <- as.Date(Date, format="%m/%d/%Y")

attach(Daily_Mean4)
summary(Daily_Mean4)
g1<- ggplot(Daily_Mean4, aes(Date, avg))
is.data.frame(Daily_Mean4)
newDate4 <- as.Date(Date, format="%m/%d/%Y")

lg1 <- ggplot()+
  geom_line(data = Daily_Mean1, aes(x=newDate1, y=avg, color="SE 15 m"))+
  geom_line(data = Daily_Mean2, aes(x=newDate2, y=avg, color="NA 25 m"))+
  geom_line(data = Daily_Mean3, aes(x=newDate3, y=avg, color="North 15 m"))+
  geom_line(data = Daily_Mean4, aes(x=newDate4, y=avg, color="SE2 15 m"))+
  scale_x_date(labels = date_format("%b %Y")) + 
  scale_y_continuous(limits = c(18.0, 27.0), expand=c(0,0))+
  labs(y=expression(paste("Temperature (",degree~C,")")))+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="#000000", size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.line.x = element_line(size=0.5),
        axis.line.y = element_line(size=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position = c("bottom"))
lg1

require(data.table)
data <- fread("C://ESMOI_HOBO_Temps.csv",sep=',')
str(data)
Daily_Mean1 <- data[,list(avg=mean(H10530652)),by=list(Date)]
Daily_Mean2 <- data[,list(avg=mean(H10530651)),by=list(Date)]
Daily_Mean3 <- data[,list(avg=mean(H10530653)),by=list(Date)]
Daily_Mean4 <- data[,list(avg=mean(NOAA)),by=list(Date)]

#dataset is sorted by day

attach(Daily_Mean1)
summary(Daily_Mean1)
g1<- ggplot(Daily_Mean1, aes(Date, avg))
is.data.frame(Daily_Mean1)
newDate1 <- as.Date(Date, format="%m/%d/%Y")

attach(Daily_Mean2)
summary(Daily_Mean2)
g1<- ggplot(Daily_Mean2, aes(Date, avg))
is.data.frame(Daily_Mean2)
newDate2 <- as.Date(Date, format="%m/%d/%Y")

attach(Daily_Mean3)
summary(Daily_Mean3)
g1<- ggplot(Daily_Mean3, aes(Date, avg))
is.data.frame(Daily_Mean3)
newDate3 <- as.Date(Date, format="%m/%d/%Y")

attach(Daily_Mean4)
summary(Daily_Mean4)
g1<- ggplot(Daily_Mean4, aes(Date, avg))
is.data.frame(Daily_Mean4)
newDate4 <- as.Date(Date, format="%m/%d/%Y")

lg2 <- ggplot()+
  geom_line(data = Daily_Mean1, aes(x=newDate1, y=avg, color="Southeast 15 m"))+
  geom_line(data = Daily_Mean2, aes(x=newDate2, y=avg, color="Southeast 25 m"))+
  geom_line(data = Daily_Mean3, aes(x=newDate3, y=avg, color="North 15 m"))+
  geom_line(data = Daily_Mean4, aes(x=newDate4, y=avg, color="NOAA"))+
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 months")) +
  scale_y_continuous(limits = c(18.0, 28.0), expand=c(0,0), 
                     breaks = c(18.0, 19.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 26.0, 27.0, 28.0))+
  scale_colour_manual(values = c("blue", "green", "brown", "orange"))+
  labs(y=expression(paste("Temperature (",degree~C,")")))+
  geom_hline(yintercept = 26.5, colour = "Red", linetype = 2)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="#000000", size=12),
        axis.text.x = element_text(size=12),
        axis.text.y = element_text(size=12),
        axis.line.x = element_line(size=0.5),
        axis.line.y = element_line(size=0.5),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.position = c("bottom"))
lg2


  
  