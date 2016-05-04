#' Compute statistics (circular and activity-level) on a deployment's trajectory data and create plots
#'
#' @param dir path the to the deployment directory
#' @param bin.angle precision at which to bin angles
#' @param sub subsample positions every sub seconds to make them independent
#' @param verbose output messages on the console when TRUE
#' @param ... passthrough argument
#'
#' @export
#' @family action functions
#'
#' @seealso \code{\link{summary.circular}} for a description of the output statistics.
#'
#' @importFrom lubridate ymd_hms
#' @importFrom plyr round_any ddply count rbind.fill
#' @importFrom stringr str_replace fixed str_c
#' @import ggplot2
#'
#' @examples
#' # get example deployments included with the package
#' deploys <- system.file("extdata", "deployments", package = "discr")
#' # copy them to a writable, temporary directory
#' temp <- tempdir()
#' file.copy(deploys, temp, recursive=TRUE)
#' dd <- paste0(temp, "/deployments/")
#' deploy1 <- paste0(dd, "1")
#'
#' # run the action
#' disc_conf(deploy.dir=dd)
#' disc_stats(dir=deploy1, verbose=TRUE)
#' # inspect results
#' list.files(deploy1)
#' read.csv(paste0(deploy1, "/stats.csv"))
#'
#' # subsample positions
#' disc_stats(dir=deploy1, sub=10, verbose=TRUE)
#' read.csv(paste0(deploy1, "/stats.csv"))
#' # Note the difference in n compared to above
disc_stats <- function(dir, bin.angle=0, sub=NULL, verbose=FALSE, ...) {

  disc_message("Compute statistics")

  # checks
  tracksFile <- make_path(dir, .files$rotated.tracks)
  assert_that(file.exists(tracksFile))

  if ( verbose ) disc_message("read and process tracks")

  # read tracks
  t <- read.csv(tracksFile, stringsAsFactors=FALSE)
  t$theta <- as.bearing(t$theta)
  t$heading <- as.bearing(t$heading)
  t$cameraHeading <- as.bearing(t$cameraHeading)
  t$dateTime <- ymd_hms(t$dateTime)

  # bin angles if required
  if ( bin.angle != 0 ) {
  	t$theta <- round_any(t$theta, bin.angle)
  	t$heading <- round_any(t$heading, bin.angle)
  }


  # subsample the data if needed
  if ( verbose ) disc_message("subsample data if needed")
  tComplete <- t
  # TODO invert tComplete and t (use t_subsampled or t_sub)
  t <- ddply(t, ~trackNb+rotation, function(x) {
    subN <- subsample_n(x$dateTime, sub=sub, verbose=verbose)
    x <- x[seq(1, nrow(x), by=subN),]
    return(x)
  })

  # compute position statistics
  # i.e. statistics about how concentrated the positions are in the reference of the chamber or in a cardinal reference
  if ( verbose ) disc_message("compute position statistics")
  position_stats <- ddply(t, ~trackNb+rotation, function(x) {
    stats <- summary.circular(x$theta)
    return(stats)
  })
  # position_stats$kind <- "position"

  # compute movement statistics
  # i.e. statistics about the swimming direction and speed of the larva
  if ( verbose ) disc_message("compute movement statistics")
  movement_stats <- ddply(tComplete, ~trackNb+rotation, function(x) {
    # swimming direction
    dir_stats <- summary.circular(na.omit(x$heading))
    names(dir_stats) <- str_c("dir.", names(dir_stats))

    # turning angle
    turns <- abs(na.omit(x$turnAngle))
    turn_stats <- data.frame(
      n = length(turns),
      abs.mean = mean(turns),
      freq.gt45 = sum(turns > 45) / length(turns)
    )
    names(turn_stats) <- str_c("turn.", names(turn_stats))

    # swimming speed
    speeds <- na.omit(x$speed)
    speed_stats <- data.frame(
      n = length(speeds),
      mean = mean(speeds),
      sd = sd(speeds),
      median = median(speeds),
      mad = mad(speeds)
    )
    names(speed_stats) <- str_c("speed.", names(speed_stats))

    stats <- cbind(dir_stats, turn_stats, speed_stats)
    return(stats)
  })
  # movement_stats$kind <- "direction"

  # combine both
  stats <- cbind(position_stats, dplyr::select(movement_stats, -trackNb, -rotation))

  # add mention of binning
  stats$bin.angle <- bin.angle

  # store the stats results
  destFile <- make_path(dir, .files$stats)
  write.csv(stats, file=destFile, row.names=FALSE)



  # prepare plots
  plots <- list()

  # compass rotation
  if ( verbose ) disc_message("plot compass rotation")
  # for one track only (it's enough)
  p <- ggplot(dplyr::filter(tComplete, rotation == "raw")) + polar() +
    geom_point(aes(x=cameraHeading, y=elapsed.min), size=2) +
    scale_y_continuous(limits=c(min(tComplete$elapsed.min, na.rm=T) - 20, max(tComplete$elapsed.min, na.rm=T)), breaks=seq(0, max(tComplete$elapsed.min, na.rm=T), by=2)) + 
    # geom_point(aes(x=cameraHeading, y=dateTime), size=2) +
    # scale_y_continuous(limits=c(min(tComplete$dateTime, na.rm=T) - 3600, max(tComplete$dateTime, na.rm=T) + 3600)) +
    # TODO fix this: does not work so I can't shift the min away from the center
    facet_grid(trackNb~.) +
    labs(title="Compass rotation")
  plots <- c(plots, list(compass_rotation=p))

  # trajectory
  if ( verbose ) disc_message("plot trajectory")
  # get arena radius to limit the plot
  diameter <- getOption("disc.diameter")
  radius <- diameter / 2
  radiusT <- radius + 1 # add tolerance for limits
  # draw the arena
  circleFun <- function(center = c(0,0), radius = 1, npoints = 100){
      tt <- seq(0, 2*pi, length.out = npoints)
      xx <- center[1] + radius * cos(tt)
      yy <- center[2] + radius * sin(tt)
      return(data.frame(x = xx, y = yy))
  }
  p <- ggplot(tComplete, aes(x=x, y=y)) +
    geom_path(data=circleFun(radius=radius), color="white") +
    geom_path(aes(colour=elapsed.min)) +
    facet_grid(trackNb~rotation) +
    coord_equal(xlim=c(-radiusT, radiusT), ylim=c(-radiusT, radiusT)) +
    scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL) +
    labs(title="Trajectory")
  plots <- c(plots, list(trajectory=p))


  # positions
  sub <- str_c(round(stats$mean), "\u00B0 (r=", round(stats$r, 3), ", p=", round(stats$p.value, 3), ")", collapse=" | ")
  posTitle <- bquote(atop(Positions, scriptstyle(.(sub))))


  # bin angles
  bin <- max(c(5, bin.angle))

  # # position dotplot
  # if ( verbose ) disc_message("plot positions dotplot")
  # tBinned <- ddply(t, ~trackNb+rotation, function(x, bin) {
  #   x$theta <- as.numeric(round_any(x$theta, bin))
  #   x$theta[x$theta==360] <- 0
  #
  #   # create a data.frame with count
  #   counts <- count(x, "theta")
  #
  #   # repeat each point the appropriate number of times
  #   d <- adply(counts, 1, function(x) {
  #     data.frame(theta=x$theta, count=1:x$freq)
  #   }, .expand=F)
  #
  #   # make the scale prettier
  #   d$count <- 10 + d$count
  #
  #   return(d)
  # }, bin=bin)
  # p <- ggplot(tBinned) + polar() + labs(title=posTitle) +
  #   geom_point(aes(x=theta, y=count)) +
  #   geom_segment(aes(x=mean, y=0, xend=mean, yend=r*10, linetype=signif), data=stats) +
  #   # geom_segment(aes(x=mean, y=0, xend=mean, yend=r*10, linetype=signif), data=stats) +
  #   scale_linetype_manual(values=c("solid", "dashed")) +
  #   scale_y_continuous(name="r", limits=c(0, max(tBinned$count)), breaks=c(0, 10/2, 10), labels=c(0, 0.5, 1)) +
  #   facet_grid(trackNb~rotation)
  # # TODO edit labels in first plot to remove N, S, E, W; that probably involved setting two plots up with grid.arrange.
  # plots <- c(plots, list(position_dotplot=p))


  if ( verbose ) disc_message("plot positions histogram")
  # position histogram
  p <- ggplot(t) + polar() + labs(title=posTitle) +
    geom_histogram(aes(x=theta), binwidth=bin) +
    geom_segment(aes(x=mean, y=-10, xend=mean, yend=-10+r*10, linetype=signif), data=stats) +
    scale_linetype_manual(values=c("solid", "dashed")) +
    scale_y_continuous(name="r", breaks=c(-10, -10/2, 0), labels=c(0, 0.5, 1)) +
    facet_grid(trackNb~rotation)
  plots <- c(plots, list(position_histogram=p))


  if ( verbose ) disc_message("plot directions histogram")
  p <- ggplot(tComplete) + polar() + labs(title="Swimming directions") +
    geom_histogram(aes(x=heading), binwidth=bin, na.rm=TRUE) +
    geom_segment(aes(x=dir.mean, y=-10, xend=dir.mean, yend=-10+dir.r*10, linetype=dir.signif), data=stats) +
    scale_linetype_manual(values=c("solid", "dashed")) +
    scale_y_continuous(name="r", breaks=c(-10, -10/2, 0), labels=c(0, 0.5, 1)) +
    facet_grid(trackNb~rotation)
  plots <- c(plots, list(direction_histogram=p))

  if ( verbose ) disc_message("plot turning angles")
  p <- ggplot(tComplete) +
    geom_histogram(aes(x=turnAngle), binwidth=bin, na.rm=TRUE) +
    scale_x_continuous(limits=c(-180, 180)) +
    facet_grid(trackNb~rotation)
  plots <- c(plots, list(turn_angle_histogram=p))


  if ( verbose ) disc_message("plot swimming speeds")
  p <- ggplot(dplyr::filter(tComplete, rotation=="raw")) + labs(title="Swimming speed (cm/s)") +
    geom_histogram(aes(x=speed), binwidth=0.05, na.rm=TRUE) +
    geom_vline(aes(xintercept=speed.mean), data=dplyr::filter(stats, rotation=="raw")) +
    facet_grid(trackNb~.)
  plots <- c(plots, list(speed_histogram=p))


  # plot them to a file
  if ( verbose ) disc_message("save plots as PDF")
  destFile <- str_replace(destFile, fixed(".csv"), ".pdf")
	pdf(file=destFile, width=7, height=1+3*length(unique(t$trackNb)), pointsize=10)
	# set a theme with smaller fonts and grey background
	theme_set(theme_gray(10))
	dummy = l_ply(plots, print, .progress="text")
	# close PDF file
	dummy = dev.off()

  return(invisible(plots))
}
