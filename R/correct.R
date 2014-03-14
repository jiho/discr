#' Correct tracks (camera frame of reference) based on rotation of DISC to get polar-relevant coordinates
#' 
#' @param dir path the to the deployment directory
#' 
#' @export
#' 
#' @importFrom plyr llply
#' @importFrom plyr ldply
#' @importFrom stringr str_c
#' @import boot
#' @import circular
#' 

disc_correct <- function(dir, ...) {

  aquariumDiam = 20.32
  cameraCompassAngle = 0
  lookingUp = TRUE
  
# Turn warnings off - minor ones produced due to 'unrecognized maker notes'
options(warn=-1)

## Read and reformat larvae tracks
#------------------------------------------------------------

# larvae tracks are recorded from ImageJ "Manual tracking"
trackLarva = read.table(str_c(dir,"larvae_track.txt",sep="/"), header=T, sep="\t")
trackLarva = trackLarva[,-1]

# Split larvae tracks in a list, one element per larva
tracks = split(trackLarva, trackLarva$trackNb)
nbTracks = length(tracks)

# Suppress duplicate positions (two positions recorded for the same frame)
# There should not be any but this can happen with the manual tracking plugin not working properly
tracks = llply(tracks, function(t){
	# When there are duplicates, we keep the last one
	t = t[!duplicated(t$imgNb, fromLast=T),]
	return(t)
})

# Add time stamps to the tracks
tracks = llply(tracks, function(t){
	# get the number of the images where the larva is detected
	images = t$imgNb

	# for each, read the exact time with split seconds
	# the 'system' function allows to access the shell, hence access exiftool
	picTimes = system(paste("exiftool -T -p '$CreateDate.$SubsecTime' ", paste(dir,"/pics/",images,".jpg", sep="", collapse=" "), sep=""), intern=TRUE)
	options("digits.secs" = 2)
	picTimes = as.POSIXct(strptime(picTimes, format="%Y:%m:%d %H:%M:%OS"))

	# keep split seconds but also round times to full seconds
	picTimes = data.frame(imgNb=images, exactDate=picTimes, date=round(picTimes))

	# add to each track
	t = merge(t, picTimes)
})


## Read and reformat compass tracks
#------------------------------------------------------------

# read compass record
# it can either be a record from the numerical compass or from the backup, physical compass
if (file.exists(str_c(dir,"compass_log.csv",sep="/"))) {

	# This is the log of the numeric compass
	trackCompass = read.table(str_c(dir,"compass_log.csv",sep="/"), header=TRUE, sep=",", as.is=TRUE)
	trackCompass$date = as.POSIXct(strptime(trackCompass$date, format="%Y-%m-%d %H:%M:%S"))
	compassSource = "numeric"

	# correct for the angle between the camera and the compass
	# the correction depends on the configuration of the camera
	if (lookingUp) {
		trackCompass$heading = trackCompass$heading - cameraCompassAngle
	} else {
		trackCompass$heading = trackCompass$heading + cameraCompassAngle
	}
	# this means that trackCompass$heading now correspond to the heading of the *top* of the picture

	# convert to the appropriate circular class
	trackCompass$heading = circular(trackCompass$heading, unit="degrees", template="geographics", modulo="2pi")

} else if (file.exists(str_c(dir,"compass_track.txt",sep="/"))) {

	# Else we default to a manual record of the compass track
	# The goal is to make it look like the numerical one, so that we can use the same code afterwards
	trackCompass = read.table(str_c(dir,"compass_track.txt",sep="/"), header=TRUE, sep="\t", as.is=TRUE)
	trackCompass = trackCompass[,-1]
	compassSource = "manual"

	# remove possible duplicates
	trackCompass = trackCompass[!duplicated(trackCompass$imgNb, fromLast=T),]

	# compute the time associated with each image
	# this can be different from those in the tracks since we can subsample the compass and the track at a different interval

	# get the number of the images where the larva is detected
	images = trackCompass$imgNb
	# for each, read the time using exiftool
	picTimes = system(paste("exiftool -T -p '$CreateDate.$SubsecTime' ", paste(dir,"/pics/",images,".jpg", sep="", collapse=" "), sep=""), intern=TRUE)
	options("digits.secs" = 2)
	picTimes = data.frame(imgNb=images, date=as.POSIXct(strptime(picTimes, format="%Y:%m:%d %H:%M:%OS")))
	# add to the compass track
	trackCompass = merge(trackCompass, picTimes)

	# read the coordinates of the center of the compass, to be able to compute the movement in polar coordinates
	coordCompass = read.table(str_c(dir,"coord_compass.txt",sep="/"), header=TRUE, sep="\t", col.names=c("X","x", "y"))
  coordCompass = coordCompass[,c("x","y")]

	# convert the coordinates to polar ones
	trackCompass[,c("theta","rho")] = car2pol(trackCompass[,c("x","y")], coordCompass[,c("x","y")])

	# convert the bearing to the appropriate circular class
	trackCompass$heading = trig2geo(trackCompass$theta)

} else {

    # We do not have a compass track, we just assume constant bearing from start to finish
    # detect start and finish of each track
    dateRange <- ldply(tracks, function(x) range(x$exactDate))
    # use a zero angle for all those dates
    trackCompass = data.frame(date=c(dateRange[,2], dateRange[,3]), heading=0)
}

# Interpolate compass headings at every point in time in the tracks
tracks = llply(tracks, function(x, compass) {
	# The approx.circular function is described in lib_circular_stats.R
	x$compass = approx.circular(compass$date, compass$heading, x$exactDate)$y
	return(x)
}, trackCompass)



## Compute larvae tracks in a cardinal reference
#------------------------------------------------------------

# Read calibration data
coordAquarium = read.table(str_c(dir,"coord_aquarium.txt",sep="/"), header=TRUE, sep="\t", col.names=c("nb","X","Y","Perim"))
coordAquarium = coordAquarium[,-1]

# Only keep larvae positions where a compass reading is available
tracks = llply(tracks, function(x){x=x[!is.na(x$compass),]})

# Correct for the rotation of the compass relative to the bearing in the first frame
# for all larvae tracks
for (l in 1:nbTracks) {

	t = tracks[[l]]

	# convert track to polar coordinates
	t[,c("theta","rho")] = car2pol(t[,c("x","y")], c(coordAquarium$X,coordAquarium$Y))

	# theta is in trigonometric reference:
	# 	measures the angle, in radians, between the horizontal and the larva, in counter clockwise direction.
	# we convert it to a compass heading (from the north and clockwise)
	t$theta = trig2geo(t$theta)

	# initialize the data.frame for corrected tracks
	tCor = t

	# then correct for the rotation by subtracting the compass heading of the top of the picture
	tCor$theta = t$theta - t$compass

	# for uncorrected track to be comparable to the corrected one, they need to start in the same reference as the corrected track. So we subtract the *first* compass angle to every frame
	t$theta = t$theta - t$compass[1]

	if (lookingUp) {
		# Switch the direction of rotation when we look at the aquarium and compass from below
		# Indeed, in that case the East appears to be on the left
		# This computes the symmetry and puts E and W where they belong

		# _set_ the direction of measure to counter clockwise
		# (does not alter the numbers, just the attributes)
		a = circularp(t$theta)
		a$rotation="counter"
		circularp(t$theta) = a
		circularp(tCor$theta) = a
		# _convert_ back to clockwise (this actually changes the numbers and compute the symmetry)
		t$theta = conversion.circular(t$theta, units="degrees", rotation="clock")
		tCor$theta = conversion.circular(tCor$theta, units="degrees", rotation="clock")
	}

	# recompute cartesian positions from the polar definition
	t[,c("x","y")] = pol2car(t[,c("theta","rho")])
	tCor[,c("x","y")] = pol2car(tCor[,c("theta","rho")])

	# convert x, y, and rho to human significant measures (cm)
	# we use the diameter of the aquarium as a reference
	# we are given its value in cm and we have the perimeter, hence the diameter, in pixels
	px2cm = aquariumDiam/(coordAquarium$Perim/pi)
	t[,c("x","y","rho")] = t[,c("x","y","rho")] * px2cm
	tCor[,c("x","y","rho")] = tCor[,c("x","y","rho")] * px2cm

	# make position angles real bearings: they are already measured clockwise from the north, now we also make sure they only contain positive value, just because that looks better
	t$theta = (t$theta + 360) %% 360
	tCor$theta = (tCor$theta + 360) %% 360

	# reorganize the columns of the dataframe
	colNames = c("trackNb", "sliceNb", "imgNb", "exactDate", "date", "x", "y", "theta", "rho", "compass")
	t = t[,colNames]
	tCor = tCor[,colNames]
	# add a column that tells whether the track is corrected or not
	t$correction=FALSE
	tCor$correction=TRUE

	# store it in the initial list
	tracks[[l]] = list(original=t, corrected=tCor)

}


## Compute tracks characteristics   - BELOW HERE NEEDS WORK **** - MF
#------------------------------------------------------------

# Take omitted frames into account in larvae tracks
# fetch the names of all images
imagePath = paste("ls -1 ",dir,"/pics/*.jpg | cut -d '/' -f 3 | cut -d '.' -f 1", sep="")
images = sort(as.numeric(system(imagePath, intern=T))) 
# images = sort(as.numeric(system("ls -1 ../pics/*.jpg | cut -d '/' -f 3 | cut -d '.' -f 1", intern=T)))
# there are two levels of nesting of lists, hence the double llply construct
tracks = llply(tracks, .fun=function(tr, ...){
	llply(tr, .fun=function(x, imgNames) {
		# prepare a full, empty data.frame with one line per image
		t = as.data.frame(matrix(nrow=length(imgNames),ncol=length(names(x))))
		names(t) = names(x)
		# specify the content of columns that must not be empty
		t$trackNb = x$trackNb[1]
		t$correction = x$correction[1]
		t$imgNb = imgNames
		# set classes similarly to the original data.frame
		class(t$date) = class(x$date)
		class(t$exactDate) = class(x$exactDate)
		# fill with values from the orignal data.frame and leave NAs elsewhere
		t[ t$imgNb %in% x$imgNb,] = x;
		return(t);}
	, ...)}
, images)


# Compute swimming direction and speed
for (i in 1:nbTracks) {
	tracks[[i]] = llply(tracks[[i]], function(t) {
		# Compute swimming directions
		# compute swimming vectors in x and y directions
		# = position at t+1 - position at t
		dirs = t[2:nrow(t),c("x","y")] - t[1:(nrow(t)-1),c("x","y")]
		dirs = rbind(NA,dirs)
		# convert to headings by considering that these vectors originate from 0,0
		headings = car2pol(dirs, c(0,0))$theta
		# convert to the appropriate circular class
		headings = conversion.circular(headings, units="degrees", template="geographics", modulo="2pi")
		# store that in the orignal dataframe
		t$heading = headings

		# Compute speeds in cm/s
		# compute time difference between pictures
		dirs$interval = c(NA,as.numeric(diff(t$exactDate)))
		# compute speed from displacement and interval
		t$speed = sqrt(dirs$x^2 + dirs$y^2) / dirs$interval

		return(t)
	})

	# Suppress speed for the uncorrected data: it does not make sense because the corrected "trajectory" is never really travelled
	tracks[[i]][["corrected"]]$speed = NA
}


## Saving tracks for statistical analysis and plotting
#------------------------------------------------------------
# Concatenate all tracks into one data.frame
tracks = do.call("rbind", do.call("rbind", tracks))

# Write it to a csv file
write.table(tracks, file=str_c(dir,"tracks.csv",sep="/"), sep=",", row.names=F)

}


#' @rdname disc_correct
#' @export
dcorrect <- disc_correct