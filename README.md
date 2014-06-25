# Drifting In Situ Chamber User Software (in S)

Well, in R actually, but `discuss` looks better than `discusr` ;)

## Installation

1. Install R from <http://cran.rstudio.com/>

2. Install RStudio from <http://www.rstudio.com/products/rstudio/download/>. RStudio is a graphical interface for R which will make it easier to work with discuss data.

3. Install `discuss`. `discuss` is an R package but it is not (yet) available in the official R packages repositories. To install it, start RStudio and, in the console, type

        install_packages("devtools")
        devtools::install_bitbucket("discuss", "jiho")

4. Load `discuss` and check your installation with

        library("discuss")
        disc_check()
    
    `disc_check()` will give you platform-specific pointers to install the software `discuss` depends on. `discuss` needs
    
    - A java JRE to run the image manipulation parts; from <https://www.java.com/en/download/>
    - The `exif` executable from `libexif` to extract timestamp from images; installation is usually done through a package manager
    - ImageMagick (the `convert` executable) to resize images to more manageable sizes; from <http://www.imagemagick.org/> or through a package manager


## Basic usage

### Data collection

Data collected with the DISC is stored in one directory. A subdirectory is created for each deployment leg (usually per day). Within each leg, a subdirectory is made for each sensor on the DISC (camera, compass, light sensor etc.). The information for every leg and every deployment within each leg is written down in a log file, in the form of a spreadsheet (saved as a Comma Separated Values, `*.csv`, file), which is stored in the top level DISC directory.

The final hierarchy usually looks like

    DISC_A/
        leg_1/
            pics/
                G001234.JPG
                G001235.JPG
                G001236.JPG
                ...
            compass/
                DATALOG.txt
            hobo/
                123459.hobo
                123459.csv
            ...
        leg_2/
            pics/
            compass/
            hobo/
            ...
        leg_log.csv
        deployment_log.csv

The format for the `leg_log.csv` file is:

      leg, gopro_start, gopro_stop, gopro_dir, gopro_offset, cc_start, ...
    leg_1,    12:22:25,   17:35:01,      pics,           -6, 12:20:10, ...
    leg_2,         ...,

It has the leg directory name and information for each sensor. The column names are in the form `sensorName_informationLabel`. The underscore (`_`) in the middle is important. The usual information for each sensor is :

- `start` and `stop` : the start and stop time of the sensor (not used by `discuss` but important to record)
- `dir` : the name of the directory in which the sensor data is stored. When absent, this is supposed to be the name of the sensor itself. Pictures should always be stored in a directory called `pics`; compass data should always be stored in a directory called `compass`; the rest of the names are free.
- `offset` : the time offset (in seconds) between the sensor time and a reference time (usually the time of a computer). When absent, it is supposed to be 0. The offset is *added* to the timestamp of the data to correct it back to the reference time. So it should be negative when the sensor is early and positive when it is late. For example, if the sensor records 08:55:12 but the actual time is 08:55:10, the sensor is early and the offset is -2.

Other columns can be added but should *not* have an underscore in their names. Use dots (`personal.comments`) or capitals (`personalComments`) to separate words.


The format for the `deployment_log.csv` is:

    deployId,   leg, date_start,  date_stop, time_start, time_stop
           1, leg_1, 2014-05-22, 2014-05-22,   23:31:23,  23:52:10
           2, leg_1, 2014-05-22, 2014-05-23,   23:54:34,  00:05:12
           3, leg_1, 2014-05-23, 2014-05-23,   00:08:15,  00:29:12
         ...

It has

- `deployId` : a *unique* deployment identifier. It can be anything but is usually an integer number, which makes it easy to specify ranges of deployments.
- `leg` : the corresponding leg directory name
- `date_start`, `date_stop` : date in format YYYY-MM-DD.
- `time_start`, `time_stop` : time in format HH:MM:SS; the combination of date and time allows deployments to cross midnight

The deployment log usually has other columns such as fish species, meteorological conditions, etc. The names of those columns are free, but try to avoid special characters (accents, parentheses, exponents, etc.)


### Extract deployments

The directory described above holds the raw data. To be analysed, it needs to be split into deployments. The deployments are smaller than the raw data and are usually stored elsewhere.

Start RStudio a create a new project (`File > New Project...`). Use a new directory and create an empty project. This directory will be your working directory. `discuss` will store the deployments in a sub-directory called "deployments" there.

In the console, load `discuss`

    library("discuss")

and split the data with

    disc_split_deployments(raw="/path/to/raw/data")

If you want to extract only a few deployments, use

    disc_split_deployments(raw="/path/to/raw/data", ids=10:20)

for deployments 10 to 20 or

    disc_split_deployments(raw="/path/to/raw/data", ids=c("1a", "2a", "2b", "6"))

for deployments 1a, 2a, 3b and 6, for example. (NB: This highlights why having integer deployment identifiers is easier.)

See `?disc_split_deployments` for more information.


### Process deployments

Open the RStudio project created above. This sets your working directory correctly.

In the console load `discuss`

    library(`discuss`)

The process deployments with a command such as

    disc(1:20, actions=c("calib", "track"))

to calibrate the arena dimensions and track the larva on deployments 1 to 20. Actions can be:

- "camera compass angle" : measure the angle between the camera and the compass
- "calibrate" : measure the arena on an image, to 
- "track" : track one (or several) larva(e) manually
- "correct" : correct larvae tracks in cardinal reference
- "stats" : compute statistics on larvae positions

Default actions are "calibrate", "track", "correct", and "stats".

Check your progress with

    disc_status()
    # or
    dstatus()
    # for short

and now `disc()` again. Good luck!


### Analyse data

Open the RStudio project create above, to set the working directory. Load `discuss` with

    library("discuss")

Check available data with

    disc_status()

Collect statistics for all larvae in a `data.frame` with

    assemble("stats")

Alternatively, you can select a few deployments only with

    assemble("stats", ids=1:10)

You can also collect all tracks, or gps data, or hobo data, etc. with

    assemble("tracks_rotated")
    assemble("gps_log")
    assemble("hobo_log")

What `assemble()` does is look for files with the given pattern in their name, read them all and concatenate the result.

Now you are ready to do you analyses in R. Discuss provides a few helpful functions

    ?circ.stats
    ?scale_x_circular
    ?polar
    ?circular_dotplot



## Advanced usage

disc_setwd()

New sensors

## Credit

`discuss` is written by [Jean-Olivier Irisson](http://www.obs-vlfr.fr/~irisson/ "jean-olivier irisson : work"), at Université Pierre et Marie Curie ([UPMC](http://www.upmc.fr/)). All code is released under the [GNU General Public License v3.0](https://www.gnu.org/copyleft/gpl.html "The GNU General Public License v3.0 - GNU Project - Free Software Foundation").

The DISC instrument is developed by [Claire Paris](https://www.rsmas.miami.edu/users/cparis/ "Physical-Biological Interactions - Paris' Lab") at the Rosenstiel School of Marine and Atmospheric Sciences ([RSMAS](http://www.rsmas.miami.edu/ "| The Rosenstiel School of Marine and Atmospheric Science at the University of Miami")) of the University of Miami.

Most of the image analysis functionality relies on [ImageJ](http://imagej.net/ "ImageJ") by Wayne Rasband.

Circular statistics are performed with the package [circular](http://cran.r-project.org/package=circular "CRAN - Package circular") for R.

