#Read the file from the working directory
household_power_consumption<-read.table("household_power_consumption.txt",sep = ";", header=T)
#get the correct data set, little bit long way but I find it the easiest

household_power_consumption<-read.table("household_power_consumption.txt",sep = ";", header=T)
data_2007_1<-household_power_consumption[household_power_consumption$Date=="1/2/2007",]
data_2007_2<-household_power_consumption[household_power_consumption$Date=="2/2/2007",]
data_2007<-cbind (data_2007_1,data_2007_2)
Global_active_power<-as.character(data_2007$Global_active_power)
Global_active_power<-as.numberic(Global_active_power)

#plot 2
png(file = "plot2.png")
plot2.png<-plot (Global_active_power~data_2007[,2], type = "l", ylab="Global active power (killowats)")
dev.off()