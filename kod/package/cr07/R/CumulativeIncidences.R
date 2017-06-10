#plotCuminc("time", "risk", "group", data, 10) -> x

#' @title making cuminc data for plotting and testing
#' @name plotCuminc
#' @description list with cumulative incidences and a test of differences between them
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int
#' @param target conf intervals point
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc

plotCuminc <- function(time,
                        risk,
                        group,
                        data,
                        target,
                        rho = 0,
                        cens = 0){

    data <- as.data.frame(data)

    risks <- as.data.frame(unique(data[, risk]))
    risks <- filter(risks, risks != cens)
    risks <- as.vector(as.character(risks[,1]))

    groups <- as.data.frame(unique(data[, group]))
    groups <- filter(groups, !is.na(groups))
    groups <- as.vector(as.character(groups[,1]))

    ci <- cuminc(ftime = data[, time],
                 fstatus = data[, risk],
                 group = data[,group],
                 rho = rho,
                 cencode = cens)
    #delete testing
    ci <- ci[-length(ci)]

    #make long format
    aggNames <- names(ci)
    toPlot <- c()

    for(i in aggNames){
        tmp <- as.data.frame(ci[[i]])
        tmp$name <- i
        toPlot <- as.data.frame(rbind(toPlot, tmp))
        }

    gr <- toPlot$name
    for(i in risks){
        gr <- unlist(strsplit(gr, i))}
    gr <- gsub(" ", "", gr)
    gr <- gr[gr != ""]
    toPlot$group <- gr

    ri <- toPlot$name

    for(i in groups){
        ri <- unlist(strsplit(ri, i))
        }


    ri <- gsub(" ", "", ri)
    ri <- ri[ri != ""]
    toPlot$risk <- ri

    toPlot <- toPlot[, !names(toPlot) %in% "name"]

    #adding conf intervals
    toPlot$lowerBound <- sapply(1:nrow(toPlot), function(x){
        est <- toPlot[x, "est"]
        var <- toPlot[x, "var"]
        exp(log(est) - 1.96*sqrt(var)/est)
    })

    toPlot$upperBound <- sapply(1:nrow(toPlot), function(x){
        est <- toPlot[x, "est"]
        var <- toPlot[x, "var"]
        exp(log(est) + 1.96*sqrt(var)/est)
    })

    colnames(toPlot)[which(colnames(toPlot) == "est")] <- "prob"

    barsData <- expand.grid(risks, groups)

    low <- c()
    up <- c()
    prob <- c()
    for(i in 1:nrow(barsData)){
        tmpBounds <- as.numeric(bounds(barsData[i,1],barsData[i,2], target))
        low <- c(low, tmpBounds[1])
        prob <- c(prob, tmpBounds[2])
        up <- c(up, tmpBounds[3])

    }

    barsData <- cbind(barsData, low, prob, up)
    colnames(barsData)[1:2] <- c("risk", "group")
    rm(low,up, prob)

    #making a plot
    plot1 <- ggplot(data = toPlot, aes(time, prob, color = group)) +
        geom_step(size=1) +
        facet_grid(~risk, scales = "free")

    #adding errorbars
    plot1 <- plot1 +
        geom_errorbar(data = barsData, aes(x = target, ymin = low, ymax = up), size = 0.75, alpha = 0.7)

    #making it beauty
    plot1 <- plot1 +
        theme_minimal() +
        ggtitle("Cumulative incidence function") +
        theme(plot.title = element_text(size=13, face="bold", hjust = 0.5)) +
        scale_y_continuous("Cumulative incidences", limits = c(0,1)) +
        scale_x_continuous("Time")+
        theme(legend.title = element_text(size=10, face="bold"))+
        scale_color_discrete(name="Group", labels = groups)

}




#' @title Log rank test for competing risks
#' @name testCuminc
#' @description Log rank test for competing risks
#' @param time time must be numeric
#' @param risk can be numeric or factor/character
#' @param group can be numeric or factor/character
#' @param data can be data frame or matrix
#' @param conf.int level of two sided conf int
#' @param conf.type "none", "plain", "log" (default), "log-log
#' @export
#' @importFrom dplyr filter
#' @importFrom cmprsk cuminc

testCuminc <- function(time,
                       risk,
                       group,
                       data,
                       rho = 0,
                       cens = 0){

    data <- as.data.frame(data)

    ci <- cuminc(ftime = data[,time],
                 fstatus = data[, risk],
                 group = data[,group],
                 rho = rho,
                 cencode = cens)

    tab <- as.data.frame(ci[[length(ci)]])
    p <- as.data.frame(t(tab$pv))
    colnames(p) <- rownames(tab)

    kable(p, digits = 4,
          caption = "Log rank test for competing risks",
          format.args = list(scientific = FALSE))
    }
