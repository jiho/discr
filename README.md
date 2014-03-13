# Drifting In Situ Chamber User Software (in S)

Well, in R actually, but `discuss` looks better than `discusr` ;)

## Installation

1. Install R from <http://cran.rstudio.com/>

2. Install software `discuss` depends on
    - A java JRE from <https://www.java.com/en/download/>
    - The `exif` executable from `libexif` through a package manager (see further down for instructions)

3. Install `discuss`. To do that, start R and

        install_packages("devtools")
        devtools::install_bitbucket("discuss", "jiho")

4. Start R, load `discuss` and check your installation with

        library("discuss")
        disc_check()
    
    `disc_check()` will give you platform-specific pointers to install missing dependencies, if there are any.

## Usage

Start R and load `discuss`

    library("discuss")

[Split deployments]

By default `discuss` works in the current directory. Set its working directory to the place where your deployments are with

    disc_setwd("/path/to/your/deployments")

Set this project's setting with

    disc_conf()
    ?disc_conf

Then run analysis commands with

    disc(1:20, actions=c("calib", "track"))

to calibrate the aquarium dimensions and track the larva on deployments 1 to 20.

Check your progress with

    disc_status()

and now `disc` again. Good luck!

## Credit

`discuss` is written by [Jean-Olivier Irisson](http://www.obs-vlfr.fr/~irisson/ "jean-olivier irisson : work"), at Université Pierre et Marie Curie ([UPMC](http://www.upmc.com/ "UPMC: #1 Ranked Hospital in Pittsburgh and Pennsylvania")). All code is released under the [GNU General Public License v3.0](https://www.gnu.org/copyleft/gpl.html "The GNU General Public License v3.0 - GNU Project - Free Software Foundation").

The DISC instrument is developed by [Claire Paris](https://www.rsmas.miami.edu/users/cparis/ "Physical-Biological Interactions - Paris' Lab") at the Rosenstiel School of Marine and Atmospheric Sciences ([RSMAS](http://www.rsmas.miami.edu/ "| The Rosenstiel School of Marine and Atmospheric Science at the University of Miami")) of the University of Miami.

Most of the image analysis functionality relies on [ImageJ](http://imagej.net/ "ImageJ") by Wayne Rasband.

Circular statistics are performed with the package [circular](http://cran.r-project.org/package=circular "CRAN - Package circular") for R.

