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
#' @import grid
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
  if (is.null(sub)) {
    t_sub <- t
  } else {
    if ( verbose ) disc_message("subsample position data")
    t_sub <- ddply(t, ~trackNb+rotation, function(x) {
      x_sub <- x[1,]
      for (i in 2:nrow(x)) {
        # add data only if it is at least `sub` seconds away from the previous time
        if (difftime(x$dateTime[i], tail(x_sub, 1)$dateTime, units="secs") >= sub) {
          x_sub <- rbind(x_sub, x[i,])
        }
      }
      # TODO check if this for loop could be avoided
      return(x_sub)
    })
  }

  # compute position statistics
  # i.e. statistics about how concentrated the positions are in the reference of the chamber or in a cardinal reference
  if ( verbose ) disc_message("compute position statistics")
  position_stats <- ddply(t_sub, ~trackNb+rotation, function(x) {
    stats <- summary.circular(x$theta)
    return(stats)
  })
  # position_stats$kind <- "position"

  # compute movement statistics
  # i.e. statistics about the swimming direction and speed of the larva
  if ( verbose ) disc_message("compute movement statistics")
  movement_stats <- ddply(t, ~trackNb+rotation, function(x) {
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


  # plots
  disc_message("Plot results")

	# set a theme with smaller fonts and grey background
	theme_set(theme_gray(10) + theme(legend.margin=unit(0, "cm")))

  for (track in sort(unique(stats$trackNb))) {
    if ( verbose ) disc_message("plot track ", track)

    # select data for current track
    c_t <- dplyr::filter(t, trackNb==track)
    c_t_sub <- dplyr::filter(t_sub, trackNb==track)
    c_stats <- dplyr::filter(stats, trackNb==track)

    # prepare plot list
    plots <- list()

    if ( verbose ) disc_message("plot compass rotation")
    tot_duration <- max(c_t$elapsed.min, na.rm=T)
    p <- ggplot(dplyr::filter(c_t, rotation == "raw")) + polar() +
      # plot compass positions
      geom_point(aes(x=cameraHeading, y=elapsed.min, fill=elapsed.min), size=2, colour=alpha("black", 0.5), shape=21) +
      # shift them away from the center
      scale_y_continuous(limits=c(-tot_duration, NA), breaks=seq(0, tot_duration, by=2)) +
      scale_fill_distiller(palette="YlGnBu") +
      labs(title="Compass rotation")
    plots <- c(plots, list(compass_rotation=ggplotGrob(p)))
  
    if ( verbose ) disc_message("plot trajectory")
    # get arena radius to limit the plot
    diameter <- getOption("disc.diameter")
    rad <- diameter / 2 + 1 # add tolerance for aquarium size
    p <- ggplot(c_t, aes(x=x, y=y)) + labs(title="Trajectory") +
      # draw aquarium
      annotation_custom(
        grob=grid::circleGrob(r=unit(0.5,"npc"), gp=grid::gpar(col="white", lwd=2)),
        xmin=-rad, xmax=rad, ymin=-rad, ymax=rad
      ) +
      # draw trajectories (superpose a black and a coloured version)
      geom_path(size=1.25, colour="grey40") +
      geom_path(aes(colour=elapsed.min)) +
      facet_grid(.~rotation) +
      # nicer plot settings
      scale_colour_distiller(palette="YlGnBu") +
      theme(legend.position="top") +
      # force equal scales and no grid
      coord_equal(xlim=c(-rad, rad), ylim=c(-rad, rad)) +
      scale_x_continuous(breaks=NULL) + scale_y_continuous(breaks=NULL)
    plots <- c(plots, list(trajectory=ggplotGrob(p)))

    # angle binning (5 degrees minimum, or bin.angle)
    bin <- max(c(5, bin.angle))

    if ( verbose ) disc_message("plot positions histogram")
    c_stats$rot.label <- with(c_stats, str_c(rotation, "\n",round(mean),"\u00B0, r=", round(r, 2), ", p=", round(p.value, 3)))
    c_t_sub <- dplyr::left_join(c_t_sub, dplyr::select(c_stats, rotation, rot.label), by="rotation")
    # position histogram
    p <- ggplot() + polar() + labs(title="Positions") + facet_grid(.~rot.label) +
      # histogram of positions
      geom_histogram(aes(x=theta), data=c_t_sub, binwidth=bin) +
      # mean angle and Rayleigh r
      geom_segment(aes(x=mean, y=-10, xend=mean, yend=-10+r*10, linetype=signif), data=c_stats) +
      scale_linetype_manual("Directionality", limits=c(TRUE, FALSE), breaks=c(TRUE, FALSE), labels=c("signif.", "non-signif."), values=c("solid", "dashed")) +
      scale_y_continuous(name="Rayleigh's r", breaks=c(-10, -10/2, 0), labels=c(0, 0.5, 1)) +
      # larger legend key to make it more readable
      theme(legend.position="top", legend.key.width=unit(1, "cm"))
    # change the direction labels for the raw positions
    g <- ggplotGrob(p)
    g$grobs[[5]]$children[[4]]$children[[1]]$label <- c("top", "", "", "")
    # grid.draw(g)
    plots <- c(plots, list(position_histogram=g))

    if ( verbose ) disc_message("plot directions histogram")
    c_stats$rot.label <- with(c_stats, str_c(rotation, "\n",round(dir.mean),"\u00B0, r=", round(dir.r, 2), ", p=", round(dir.p.value, 3)))
    c_t <- dplyr::left_join(c_t_sub, dplyr::select(c_stats, rotation, rot.label), by="rotation")
    p <- ggplot() + polar() + labs(title="Swimming directions") + facet_grid(.~rot.label) +
      geom_histogram(aes(x=heading), data=c_t, binwidth=bin, na.rm=TRUE) +
      geom_segment(aes(x=dir.mean, y=-10, xend=dir.mean, yend=-10+dir.r*10, linetype=dir.signif), data=c_stats) +
      scale_linetype_manual("Directionality", limits=c(TRUE, FALSE), breaks=c(TRUE, FALSE), labels=c("signif.", "non-signif."), values=c("solid", "dashed")) +
      scale_y_continuous(name="Rayleigh's r", breaks=c(-10, -10/2, 0), labels=c(0, 0.5, 1)) +
      # larger legend key to make it more readable
      theme(legend.position="top", legend.key.width=unit(1, "cm"))
    # change the direction labels for the raw positions
    g <- ggplotGrob(p)
    g$grobs[[5]]$children[[4]]$children[[1]]$label <- c("top", "", "", "")
    # grid.draw(g)
    plots <- c(plots, list(direction_histogram=g))

    if ( verbose ) disc_message("plot turning angles")
    p <- ggplot(c_t) + facet_grid(.~rotation) + labs(title="Turning angles") +
      geom_histogram(aes(x=turnAngle), binwidth=bin, na.rm=TRUE) +
      scale_x_continuous("angle", limits=c(-180, 180))
    plots <- c(plots, list(turn_angle_histogram=ggplotGrob(p)))

    if ( verbose ) disc_message("plot swimming speeds")
    p <- ggplot(c_t) + labs(title="Swimming speeds", x="speed (cm/s)") +
      geom_histogram(aes(x=speed, y=..density..), binwidth=0.05, na.rm=TRUE) +
      geom_density(aes(x=speed), na.rm=TRUE)
      # geom_vline(aes(xintercept=speed.mean), data=c_stats, na.rm=TRUE)
    plots <- c(plots, list(speed_histogram=ggplotGrob(p)))

    # plot them to a file
    destFile <- make_path(dir, str_c("plots_", track, ".pdf"))
  	pdf(file=destFile, width=7, height=4.5, pointsize=10)
    l_ply(plots, function(x) {
      grid.newpage()
      grid.draw(x)
    })
  	# close PDF file
  	dev.off()
  }

  return(invisible(stats))
}
