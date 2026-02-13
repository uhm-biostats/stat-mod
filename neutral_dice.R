# function for rolling dice
two_dice <- function(dd = NULL) {
    xy <- data.frame(x = c(0.25, 0.25, 0.25, 0.5, 0.75, 0.75, 0.75),
                     y = c(0.25, 0.5, 0.75, 0.5, 0.25, 0.5, 0.75),
                     one = c(F, F, F, T, F, F, F),
                     two = c(T, F, F, F, F, F, T),
                     tre = c(T, F, F, T, F, F, T),
                     fur = c(T, F, T, F, T, F, T),
                     fiv = c(T, F, T, T, T, F, T),
                     six = c(T, T, T, F, T, T, T))


    if(is.null(dd)) {
        d1 <- xy[xy[, 2 + sample(6, 1)], 1:2]
        d2 <- xy[xy[, 2 + sample(6, 1)], 1:2]
    } else {
        d1 <- xy[xy[, 2 + dd[1]], 1:2]
        d2 <- xy[xy[, 2 + dd[2]], 1:2]
    }

    pushViewport(viewport(x = unit(0.65, units = "npc"),
                          y = unit(0.85, units = "npc"),
                          just = c("left", "center"),
                          width = unit(0.3, "npc"),
                          height = unit(0.15, "npc")))


    # die 1
    pushViewport(viewport(x = unit(0.25, units = "npc"),
                          y = unit(0.5, units = "npc"),
                          width = unit(0.4, "npc"),
                          height = unit(0.8, "npc")))
    grid.roundrect()
    grid.points(x = d1$x,
                y = d1$y,
                pch = 16, size = unit(0.25, units = "npc"))

    popViewport()

    # die 2
    pushViewport(viewport(x = unit(0.75, units = "npc"),
                          y = unit(0.5, units = "npc"),
                          width = unit(0.4, "npc"),
                          height = unit(0.8, "npc")))
    grid.roundrect()
    grid.points(x = d2$x,
                y = d2$y,
                pch = 16, size = unit(0.25, units = "npc"))

    popViewport()

    popViewport()

    return(c(nrow(d1), nrow(d2)))
}

# all spider images
ff <- list.files("img/prob", pattern = "t_.*\\.png", full.names = TRUE)
nanana <- lapply(ff, function(f) {
    image_read(f) |>
        image_transparent("white", fuzz = 10)
})

for(i in 1:length(nanana)) {
    image_write(nanana[[i]], ff[i])
}


names(nanana) <- names(ff) <- gsub(".*t_", "", ff) |>
    substr(1, 4)


# function for drawing spider grid
#' @param slocs names of spiders

draw_nanana <- function(slocs, idead = NULL, inew = NULL) {
    xy <- expand.grid(x = 1:6, y = 1:6)

    pushViewport(
        viewport(x = unit(0.65, "npc"), y = unit(0.35, "npc"),
                 width = unit(0.7, "npc"), height = unit(0.7, "npc"),
                 xscale = c(0, 6.5), yscale = c(0, 6.5))
    )

    grid.rect(gp = gpar(fill = "white"))

    gp <- replicate(nrow(xy), gpar(fill = "white"), simplify = FALSE)

    if(!is.null(idead)) {
        gp[[idead]] <- gpar(fill = "black")
    }

    if(!is.null(inew)) {
        gp[[inew]] <- gpar(fill = hsv(0.8, 0.4, 0.8))
    }

    for(i in 1:nrow(xy)) {
        grid.rect(x = xy$x[i], y = xy$y[i],
                  width = 1, height = 1,
                  default.units = "native", gp = gp[[i]])

        grid.text(as.numeric(xy[i, ]), c(xy$x[i], 0.25), c(0.25, xy$y[i]),
                  default.units = "native",
                  gp = gpar(fontsize = 18))
        grid.raster(nanana[[slocs[i]]],
                    x = xy$x[i], y = xy$y[i],
                    height = 0.9,
                    default.units = "native")
        # grid.text(slocs[i],
        #           x = xy$x[i], y = xy$y[i],
        #           default.units = "native")
    }

    popViewport()


}

# function to draw metacommunity
#' @param slocs names of spiders
#' @param p their relative abundances

draw_metacomm <- function(s, p) {
    d <- data.frame(s = s, p = p, lab = ff[s])
    d <- d[order(d$p), ]
    d$s <- factor(d$s, levels = d$s)
    d$lab <- sprintf("<img src='%s' height = '20'/>", d$lab)

    gp <- ggplot(d, aes(p, s)) +
        geom_bar(stat = "identity") +
        scale_y_discrete(labels = d$lab) +
        scale_x_continuous(expand = FALSE, limits = c(0, 0.35)) +
        xlab("Proportion in\nmetacommunity") +
        ylab("") +
        geom_text(aes(label = p),
                  vjust = 0.5,
                  hjust = c(rep(-0.1, 3), rep(1.1, 4)),
                  color = c(rep("black", 3), rep("white", 4)),
                  size = 3) +
        theme(panel.grid.major.y = element_blank(),
              axis.text.y = ggtext::element_markdown(),
              axis.ticks.length.y = unit(0, "in"),
              axis.text.x = element_text(size = 8),
              text = element_text(size = 10),
              plot.background = element_rect(fill = "gray90"),
              panel.background = element_rect(fill = "gray80"))

    gd <- ggplotGrob(gp)

    pushViewport(
        viewport(x = unit(0, "npc"), y = unit(0, "npc"),
                 just = c("left", "bottom"),
                 width = unit(0.295, "npc"), height = unit(1, "npc"))
    )


    grid.draw(gd)
    grid.rect(gp = gpar(col = "gray90", fill = "transparent"))

    popViewport()
}


draw_message <- function(text, s = NULL) {
    pushViewport(
        viewport(unit(0.32, "npc"), unit(0.85, "npc"),
                 just = c("left", "center"),
                 width = unit(0.32, "npc"), height = unit(0.25, "npc"))
    )

    grid.text(text, x = 1, y = 0.5, hjust = 1.1, vjust = 0.5)

    if(!is.null(s)) {
        grid.raster(nanana[[s]], x = 0, y = 0.5, height = 0.5,
                    hjust = -0.2, vjust = 0.5)
    }

    popViewport()

}


# setup ----
set.seed(123)

p <- c(0.24, 0.12, 0.06, 0.06, 0.34, 0.06, 0.12)
slocs <- c(names(nanana),
           sample(names(nanana), size = 29,
                  replace = TRUE, prob = p)) |>
    sample()


png("img/prob/game/f0.png", width = 5, height = 5, units = "in", res = 220)
grid.newpage()
grid.rect(gp = gpar(fill = "gray90", col = "gray90"))

draw_nanana(slocs)
draw_metacomm(names(nanana), p)
draw_message("roll dice to start")

dev.off()

for(i in 2:8) {
    png(sprintf("img/prob/game/f%s-1.png", i),
        width = 5, height = 5, units = "in", res = 220)
    # death ----
    grid.newpage()
    grid.rect(gp = gpar(fill = "gray90", col = "gray90"))

    dd <- two_dice()
    idead <- dd[1] + (dd[2] - 1) * 6

    draw_nanana(slocs, idead)
    draw_metacomm(names(nanana), p)
    draw_message(sprintf("spider at (%s) dies",
                         paste(dd, collapse = ", ")))

    dev.off()

    # birth or imm ----
    png(sprintf("img/prob/game/f%s-2.png", i),
        width = 5, height = 5, units = "in", res = 220)

    grid.newpage()
    grid.rect(gp = gpar(fill = "gray90", col = "gray90"))

    dd <- two_dice()
    birth <- dd[1] > 2

    if(birth) {
        draw_message("first die > 2: birth")
    } else {
        draw_message(expression("first die" <= 2*": dispersal"))
    }

    draw_nanana(slocs, idead)
    draw_metacomm(names(nanana), p)

    dev.off()

    # choose replacement ----
    png(sprintf("img/prob/game/f%s-3.png", i),
        width = 5, height = 5, units = "in", res = 220)

    grid.newpage()
    grid.rect(gp = gpar(fill = "gray90", col = "gray90"))

    if(birth) {
        dd <- two_dice()

        inew <- dd[1] + (dd[2] - 1) * 6
        snew <- slocs[inew]

        draw_message(sprintf("(%s) gives birth",
                             paste(dd, collapse = ", ")))
        draw_nanana(slocs, idead = idead, inew = inew)
    } else {
        foo <- two_dice(dd)
        snew <- sample(names(nanana), 1, prob = p)
        draw_message("immigrates", snew)
        draw_nanana(slocs, idead)
    }

    draw_metacomm(names(nanana), p)

    dev.off()

    # replacement ----
    png(sprintf("img/prob/game/f%s-4.png", i),
        width = 5, height = 5, units = "in", res = 220)

    slocs[idead] <- snew

    grid.newpage()
    grid.rect(gp = gpar(fill = "gray90", col = "gray90"))

    foo <- two_dice(dd)
    draw_nanana(slocs)
    draw_metacomm(names(nanana), p)

    dev.off()

    # speciation
    png(sprintf("img/prob/game/f%s-5.png", i),
        width = 5, height = 5, units = "in", res = 220)

    dd <- sample(1:6, 2, replace = TRUE)
    if(sum(dd) == 2) dd[1] <- 4


    grid.newpage()
    grid.rect(gp = gpar(fill = "gray90", col = "gray90"))

    foo <- two_dice(dd)
    draw_message("speciation? no")

    draw_nanana(slocs)
    draw_metacomm(names(nanana), p)

    dev.off()

}




