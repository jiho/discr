#' Split Deployments: convert raw field data from, grouped by data type, into individual deployments including all data types
#' 
#' @param directory path to the directory where raw data sits (project parent folder)
#' @param initialLag length of time after deployment start to omit from deployment data, i.e. acclimation
#' @param duration length of time (including initial lag) that a deployment lasts
#' 
#' @export
#' 
#' @importFrom plyr a_ply
#' @importFrom ply ddply
#' @importFrom ply adply
#' @importFrom stringr str_c
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_replace
#' @importFrom doMC registerDoMC
#' @import multicore
#' @import doMC
#' 
#' 

split_deployments <- function(directory, initialLag, duration)

  
# suppress default messages from package functions
{
  suppressMessages(library("plyr"))
  suppressMessages(library("stringr"))
  suppressMessages(library("doMC"))
  suppressMessages(library("multicore"))
  registerDoMC()      # start parallel computation engine
  
  
  
  ## First test whether the deployment directory already exists and prompt the user about overwrite ------
  if (file.exists(str_c(directory,"deployments", sep="/"))) {
    choice = menu(c("Continue", "Abort"), F, title="\nThis script is usually run *before* the \"deployments\" directory exists.\nThe \"deployments\" directory already exists and may contain pictures,\nraw data (compass, etc.), and results (larvae tracks etc.).\nIf you continue, pictures and data will be overwritten but results will be left alone.\nWhat do you want to do?")
    if (choice == 1) {
      message("OK, continuing")
    } else if (choice ==2 ) {
      message("Aborting")
      return(invisible(NULL))
    } else {
      message("I could not understand your choice")
      return(invisible(NULL))
    }
  }
  
  
  ## Next, read the deployment log -------
  
  message("Read deployment log")
  log = read.table(paste(directory,"/log-disc_deployment.csv",sep=""), header=TRUE, sep=",", as.is=TRUE)  #read in file
  log$dateTime = str_c(log$date, log$timeIn, sep=" ") #create a column for date and time combined
  log$dateTime = as.POSIXct(strptime(log$dateTime, format="%Y-%m-%d %H:%M", tz="GMT")) # creates a universal date value. see as.POSIXct help for details
  log$date = as.POSIXct(strptime(log$date, format= "%Y-%m-%d", tz="GMT"))
  # note the required format for date/time in your log file, see strptime help for details
  
  
  # we extend the time range of sensor data 1 minute when writing searching for deployment data 
  fuzGps = 60       # for gps (sampling period is 4s...so this gives us 15 extra measurements)
  fuzlight = 60      # for light/temp sensor (sampling period = 10 s)
  fuzCompass = 60   # for compass (sampling period = 2 s)
  

  ## Now, create the deployment folders using the log -------
  
  message("Extract data")
  # for each deployment (row in the log) 
  for (i in 1:nrow(log)) {
    
    # select current deployment
    cLog = log[i,]
    
    # identify the directory (date folder) containing data for this day
    dataSource = paste(directory, "/", cLog$date, sep="")
    
    # check that there are files inside of the directory
    if (! file.exists(dataSource)) { # if it doesn't exist / have files
      # display that deployment date has no data and go to the next one
      message(sprintf("%5i",cLog$deployNb), " no data")
      
    } else { # and if it does have files....
        
      
      ## Load all of the raw data, grouped by date----
      
      # start by detecting whether the current deployment is on a new day or not, and read data from new date folder if it is
      if (any(i == 1, log$date[i] != log$date[i-1]) ) {
        # this is a new day, show it
        message(cLog$date)
        
        # clean-up from temporary files from previous date
        if (exists("imgTmp")) { # this is created below if first date
          if (file.exists(imgTmp)) {
            system(str_c("rm -Rf ", imgTmp)) # we delete it each time
          }
        }
          
        
        ## get IMAGES first
        
        # collect all images from this date into one temporary directory
        message(" pictures : ", appendLF=F)
        # directory where images are at in date folder
        imgSource = paste(dataSource, "GOPRO", sep="/")
        # create the new temp directory
        imgTmp = tempfile(pattern="GOPROtmp", tmpdir=dataSource)
        dir.create(imgTmp)
        # list all .JPG images in the directory
        imgFilesSource = list.files(imgSource, pattern=glob2rx("*.JPG"), recursive=T)
        # re-sort them in numerical order
        splitImgs = str_split_fixed(imgFilesSource, fixed("/"), 2) # split directory name and image name in path
        dir = splitImgs[,1] # and create an object for all image directories
        dirNb = as.numeric(str_replace(dir, "GOPRO", "")) # split directory number from character text
        img = splitImgs[,2] # create an object for all image names
        imgNb = as.numeric(str_replace(str_replace(img, "GOPR", ""), ".JPG", ""))  # split image number from character text
        imgFilesSource = imgFilesSource[order(dirNb, imgNb)] #order images 1-> by their directory and image numbers
        
        
        # create a hard link in the temp directory in the order of the images
        message(length(imgFilesSource), " frames")
        message("  reorder frames")
        imgNbTmp = 1:length(imgFilesSource)
        imgFilesTmp = str_c(imgNbTmp, ".jpg")
        commands = str_c("ln ", imgSource, "/", imgFilesSource, " ", imgTmp, "/", imgFilesTmp, sep="") # system call to ln command
        a_ply(commands, 1, system, .progress="text")
        

        # get times of all images from exif data
        message("  get date and time for all frames")
        
        # exif does not accept all images at once so we cut the image list in pieces and feed them individually
        #   this also allows to use parallel computation to send each piece to a different core
        imgTimes = data.frame(imgNbTmp, imgFilesSource, imgFilesTmp)
        # cut in pieces of 1000 images
        imgTimes$nBin = round_any(imgTimes$imgNbTmp, 1000)
        
        
        # get image capture times from exif data
        imgTimes = ddply(imgTimes, ~nBin, function(x) {
          cImgs = str_c(imgTmp, "/", x$imgFilesTmp, collapse=" ")
          x$dateTime = system(str_c("exif -m -t CreateDate ", cImgs), intern=TRUE) # system call to 'exif' from libexif, see notes
          return(x)
        }, .parallel=T)
        # convert those into universal R time objects
        imgTimes$dateTime = as.POSIXct(strptime(imgTimes$dateTime, format="%Y:%m:%d %H:%M:%S", tz="GMT"))
        

        ## get GPS data second
        
        message(" gps      : ", appendLF=F)
        GPSFILE = list.files(str_c(dataSource, "GPS", sep="/"), pattern=glob2rx("*.csv"), full=T) # search for .csv file in GPS folder of date
        
        if (length(GPSFILE) > 0) { # if it finds it
          message("GT-31 GPS data found")
          gpsOK = TRUE
          
          # read gps log
          gps = adply(GPSFILE, 1, read.csv, skip=1, as.is=T,strip.white=T) # read and combine any gps files 
          
          # reorganize dataframe
          gps$Time = str_c(gps$Hour, gps$Min, gps$Sec, sep=":") # append data from file for Time
          gps$Date = str_c(gps$Year, gps$Month, gps$Day, sep="-") # append ata from file for date
          gps$DateTime = str_c(gps$Date, gps$Time, sep=" ") # append date and time
          gps = gps[,c("Lat", "Long", "DateTime", "Speed.km.h.")] # subset the data
          names(gps) = c("lat", "lon", "date", "speed") # and rename the variables
          
          
          # and convert to R time
          gps$date = as.POSIXct(strptime(gps$date, format="%Y-%m-%d %H:%M:%S", tz="GMT")) # and convert it to universal R time
          # note the date formatting above
          
        } else { # if it doesnt find it, say so
          message("no")
          gpsOK = FALSE
        }
        
        
        ## Get light-source data third

        message(" light     : ", appendLF=F)
        
        lightFiles = list.files(str_c(dataSource, "LIGHT", sep="/"), pattern=glob2rx("*.csv"), full=T) # look for .csv file in LIGHT of date folder
        
        if (length(lightFiles) > 0) { # if it finds one
          message("OK")
          lightOK = TRUE

          # read and format the file
          light = adply(lightFiles, 1, read.csv, skip=1) # combine all light files into one table, skipping the first row because of the specific light.csv format
          light = light[,3:5] # then we eliminate all but columns of interest
          colnames(light) <- c("date", "Temp", "Intensity") #and we rename them
          
          # convert date to a POSIXct object
          light$date = as.POSIXct(strptime(light$date, format="%m/%d/%y %I:%M:%S %p", tz="GMT")) # and convert date to R time
          #note the use of %I and %p for the 12-hour date format
          
        } else { # if it doesnt find one, say so
          message("no")
          lightOK = FALSE
        }
        

        ## get COMPASS data fourth
        
        message(" compass   : ", appendLF=F)
        
        CCFiles = list.files(str_c(dataSource, "CC", sep="/"), pattern=glob2rx("*.TXT"), full=T) # look for .TXT files in CC folder
        if (length(CCFiles) > 0) { # if it finds it...
          message("Custom Compass data found")
          compassOK = TRUE
          
          # read CSV files
          compass = adply(CCFiles, 1, read.table,skip=1) #read and combine any .csv files
          
          # sort text file into date and heading variables
          splitHeading = str_split_fixed(compass$V2, fixed(","), 2)
          compass$date= str_c(compass$V1, splitHeading[,1], sep=" ")
          compass$heading = splitHeading[,2]
          compass = compass[, c("heading", "date")]
          
          # keep only one record per second, if sampling rate was higher
          compass = compass[match(unique(compass$date), compass$date),]
          
          # convert date to a POSIXct object
          compass$date = as.POSIXct(compass$date, format = "%Y-%m-%d %H:%M:%S", tz="GMT")
          #note date format
          
        } else {
          message("no")
          compassOK = FALSE
        }
        
      }
      
      
      
      
      ##  ORGANIZE DATA INTO DEPLOYMENT FOLDERS----------
      
      # display current deployment number
      message(sprintf("%5i",cLog$deployNb), appendLF=F)
      
      # determine start and end time for this deployment
      startTime = cLog$dateTime + initialLag*60
      endTime = cLog$dateTime + duration*60
      
      # create output directory
      dataDestination = str_c(directory, "/deployments/", cLog$deployNb)
      dir.create(dataDestination, recursive=T, showWarnings=FALSE)
      
      
      ## IMAGES first
      
      # find start and end image for this deployment
      imgIdxRange = closest.index(imgTimes$dateTime, c(startTime, endTime), rule=2)
      if (imgIdxRange[1] == 1 | imgIdxRange[2] == nrow(imgTimes)) { 
        warning("Reached beginning or end of picture record. Some data may be missing for deployment ", cLog$deployNb)
        cat("! Reached beginning/end !")
      }
      
      # check the validity of this image range
      # compute the total time span that images currently selected represent
      timeSpan = diff(imgTimes$dateTime[imgIdxRange])
      units(timeSpan) = "mins"
      
      # check that the time span is compatible with deployment duration
      theorSpan = (duration - initialLag)
      if (abs(theorSpan - as.numeric(timeSpan)) > 3) {
        warning("Wrong time span in images extracted for deployment ", cLog$deployNb)
        cat(" ! Wrong time span !")
      }
      
      
      # write a diagnostic message on progress
      cat(" frames ", sprintf("%5i",imgTimes$imgNbTmp[imgIdxRange[1]]), "->", sprintf("%5i",imgTimes$imgNbTmp[imgIdxRange[2]]), "=", format(timeSpan, digits=3))
      
      
      # move images to destination
      # compute the array of images of interest
      imgFilesRange = imgTimes$imgFilesTmp[seq(imgIdxRange[1], imgIdxRange[2], by=1)]
      imgFilesRange = str_c(imgTmp, "/", imgFilesRange, collapse=" ")
      imgDestination = str_c(dataDestination, "/pics")
      imgFilesRangeDest = str_c(imgDestination, "/", imgFilesRange, collapse=" ")
      if (file.exists(imgDestination)) {
        system(str_c("rm -f ",imgDestination,"/*.jpg"))
      } else {
        dir.create(imgDestination, recursive=T)
      }
      system(str_c("mv", imgFilesRange, imgDestination, sep=" "))
      
      
      ## GPS second
      
      if (gpsOK) {
        # extract relevant gps data
        cGps = gps[gps$date > (startTime-fuzGps) & gps$date < (endTime+fuzGps), ]
        
        if (nrow(cGps) > 0) {
          # write the GPS information
          write.table(cGps, paste(dataDestination, "/gps_log.csv", sep=""), row.names=FALSE, sep=",")
          cat("  +gps")
        }
      }
      

      ## LIGHT third
      
      if (lightOK) {
        
        # extract relevant light data
        clight = light[light$date > startTime-fuzlight & light$date < endTime+fuzlight, ]
        
        if (nrow(clight) > 0) {
          # write the light information
          write.table(clight, paste(dataDestination, "/light_log.csv", sep=""), row.names=FALSE, sep=",")
          cat(" +light")
        }
      }
      
      
      
      ## COMPASS fourth
      
      if (compassOK) {
        
        # extract relevant compass data
        cCompass = compass[compass$date > startTime-fuzCompass & compass$date < endTime+fuzCompass, ]
        
        if (nrow(cCompass) > 0) {
          # write the compass information
          write.table(cCompass, paste(dataDestination, "/compass_log.csv", sep=""), row.names=FALSE, sep=",")
          cat(" +compass")
        }
      }
      
      cat("\n")
    }
  }
}


#' @rdname split_deployments
#' @export
spdeploy <- split_deployments