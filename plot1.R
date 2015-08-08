plot1<-function(){
    
    ### OBTAINING THE DATA FROM THE SOURCE
    datafile<- get_datafile()
    
    
    ### Read data table in rows (10000 rows each time) and 
    ### Convert date and time
    ### return data with date 1-2/2/2007
    data <- read_table(datafile)
    
    ### Plot graph
    plot_graph1(data)
}



plot_graph1<-function(data) {
    par(mfrow=c(1,1))
    png("plot1.png", bg="transparent")
    hist(data$Global_active_power, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
    dev.off()
}

read_table<-function(file_name) {
    col_name <- c("Date", "DateTime", "Global_active_power", "Global_reactive_power", "Voltage", "Global_intensity", "Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    nrows = 100000 # read n lines each time
    data <- NULL
    
    # connection and open file for read
    con <- file(file_name)
    open(con)
    table_header <- readLines(con,n=1) # read header
    # read  10000 rows each time
    while (length(line_block <- readLines(con, n=nrows))>0) {
        subr <- grep("^[12]/2/2007", substr(line_block,1,8))
        data_sub <- read.table(text=line_block[subr], sep = ";",stringsAsFactors=F, col.names =col_name)    ## Read feature name, activity label
        data<- rbind(data, data_sub)
    }
    close(con)
    data <-cbind(as.Date(data[,1],"%d/%m/%Y"), strptime(paste(data[,1], data[,2]),"%d/%m/%Y %H:%M:%S"), data[,3:9])
    names(data) <- col_name
    return(data)
}

get_datafile<-function(){
    fileurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    zipname <- "./data/exdata_data_household_power_consumption.zip"
    datafile <- "./data/household_power_consumption.txt"
    
    # Creating the data folder
    if (!file.exists("data")) {
        dir.create("data")
    }
    
    ## Download the file
    if (!file.exists(zipname)){
        download.file(fileurl, destfile=zipname, mode="wb")
    }
    # Unzipping the file 
    if (!file.exists(datafile)){
        unzip(zipname, exdir="./data")
    }
    return(datafile)
}


