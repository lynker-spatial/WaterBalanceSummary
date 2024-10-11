#' Create Water Balance Plot
#'
#' This function generates a water balance plot using a data frame in long format. It refactors the variable names, sums total values of variables to accurately reflect inputs and outputs, and creates faceted plots for the different variables. The function handles data for both CABCM and Terra Climate datasets, producing a combined plot that shows both the faceted bar plots and the total water balance.
#'
#' @param x A data frame in long format (e.g., output from `balance_data()`), containing columns for `divide_id`, `var`, `value`, and `lab`. The data should be specific to either CABCM or Terra Climate.
#'
#' @return This function does not return a value. It displays and prints a combined plot consisting of:
#' \itemize{
#'   \item A faceted bar plot showing the summed values of each variable, facetted by the `lab` column.
#'   \item A combined bar plot showing the positive and negative values of variables, with different colors for each variable.
#' }
#'
#' @details
#' The `CreateWaterBalancePlot` function performs the following operations:
#' \itemize{
#'   \item **Data Refactoring**: Reorders the data and assigns labels to the variables.
#'   \item **Faceted Bar Plot**: Creates a faceted bar plot showing the summed values for each variable by `divide_id`.
#'   \item **Combined Water Balance Plot**: Generates a stacked bar plot combining positive and negative values with custom color palettes for CABCM and Terra Climate datasets.
#'   \item **Color Mapping**: Uses custom color palettes to differentiate between various variables.
#'   \item **Plot Combination**: Combines the faceted and total water balance plots into a single grid layout.
#' }
#'@export



CreateWaterBalancePlot <- function(x){


  var <- lab <- value <- divide_id <- NULL

  #Reorder the data and create a new column designating what the acronyms mean
  if ("cabcm_v8" %in% unique(x$source)) {
    x$var<-factor(x$var, levels = c("ppt", "aet", "run", "rch", "str", "ERR"))
    x$lab<-recode(x$var, "aet" = "Actual evapotranspiration (AET)",
                  "rch" = "Recharge (RCH)",
                  "run" = "Runoff (RUN)",
                  "str" = "Change in Soil Water Storage (positive/negative)",
                  "ppt" = "Precipitation (PPT)",
                  "ERR" = "Error")
    x <- x %>%
      group_by(divide_id, var, lab) %>%
      summarize(value = sum(value)) %>%
      ungroup()

    p3<-ggplot()+
      geom_bar(data=x, aes(x = divide_id, y = value), stat="identity")+
      # note that the plot is now facetted by ~lab, and not by ~var
      facet_wrap(~lab, ncol=1, scales="free_y")+
      scale_y_continuous(breaks=pretty_breaks(n=3))+
      labs(x=NULL, y="mm")+
      theme(panel.margin = unit(1, "mm"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right",
            legend.key.size = unit(0.3, "cm"))

    dat1<-subset(x, value>=0)
    dat1$var<-recode(dat1$var, "ERR"="err.neg", "str"="str.pos")
    dat2<-subset(x, value<0)
    dat2$var<-recode(dat2$var, "ERR"="err.pos", "str"="str.neg")

    # replace error by its opposite to get symmetrical stacks.
    x[x$var == "ERR", "value"] <- -x[x$var == "ERR", "value"]

    # dat1<-subset(x, value>=0)
    # dat1$var<-recode(dat1$var, "ERR"="err.neg", "str"="str.pos")
    # dat2<-subset(x, value<0)
    # dat2$var<-recode(dat2$var, "ERR"="err.pos", "str"="str.neg")

    # reorder levels: from top to bottom of graph:
    breaks<-c("aet","err.neg", "str.pos", "run", "rch", "ppt", "err.pos", "str.neg")
    dat1$var<-factor(dat1$var, levels=breaks)
    dat2$var<-factor(dat2$var, levels=breaks)

    brew<-c(rev(brewer.pal(length(levels(droplevels(dat1$var))), "Reds")), brewer.pal(length(levels(droplevels(dat2$var))), "Blues"))
    # For some reason, in the scale_fill_manual() call below,
    # the colours listed in the vector specified by the values=
    # argument are not matched to the breaks specified by the
    # breaks= argument according to their level order, but
    # according to their alphabetical order.
    # Therefore I need to reorder the vector specifying the colours:
    brew<-brew[order(breaks)]
    # Ensure the factor levels are correct
    breaks <- c("err.neg", "str.pos", "ppt", "err.pos", "str.neg", "aet", "rch", "run")
    dat1$var <- factor(dat1$var, levels = breaks)
    dat2$var <- factor(dat2$var, levels = breaks)
    # Define the color palette
    # Custom shades of red
    custom_reds <- c("aet" = "#BC2F27", "rch" = "#DC4B37", "run" = "#E97352", "err.pos" = "#610F14", "str.neg" = "#A51113")
    custom_blues <- c("err.neg" = "#2272B3", "ppt" = "#6BAED6", "str.pos" = "#4292C7")

    # Combine the palettes
    color_mapping <- c(custom_blues, custom_reds)


    custom_labels <- c(
      "ppt" = "Precipitation",
      "err.neg" = "Positive Error",
      "err.pos" = "Negative Error",
      "aet" = "Actual ET",
      "rch" = "Recharge",
      "run" = "Runoff",
      "str.pos" = "Positive Storage",
      "str.neg" = "Negative Storage"
    )

    p7 <- ggplot() +
      geom_bar(data = dat1, aes(x = divide_id, y = value, fill = var), stat = "identity") +
      geom_bar(data = dat2, aes(x = divide_id, y = value, fill = var), stat = "identity") +
      scale_fill_manual(values = color_mapping, breaks = breaks, labels = custom_labels) +
      scale_y_continuous(breaks = pretty_breaks(n = 7)) +
      labs(x=NULL, y="mm", fill=NULL, title = "CABCM Water Balance", subtitle = "01/01/1979 - 01/09/2019")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.key.size=unit(0.3, "cm"),
            plot.title = element_text(size = 14, margin = margin(b = 5)),
            plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 5)))

    # Combine the plots with plot_grid
    combined_plot <- plot_grid(
      p7, p3,
      ncol = 1,
      align = 'v',
      axis = "lr"
    )


  } else {

    x$var<-factor(x$var, levels = c("ppt", "aet", "run", "str", "ERR"))

    x$lab<-recode(x$var, "aet" = "Actual Evapotranspiration (AET)",
                  "ppt" = "Precipitation (PPT)",
                  "run" = "Runoff (RUN)",
                  "ERR" = "Error (positive/negative)",
                  "str" = "Change in Soil Water Storage (positive/negative)")
    x <- x %>%
      group_by(divide_id, var, lab) %>%
      summarize(value = sum(value)) %>%
      ungroup()

    p3<-ggplot()+
      geom_bar(data=x, aes(x = divide_id, y = value), stat="identity")+
      # note that the plot is now facetted by ~lab, and not by ~var
      facet_wrap(~lab, ncol=1, scales="free_y")+
      scale_y_continuous(breaks=pretty_breaks(n=3))+
      labs(x=NULL, y="mm")+
      theme(panel.margin = unit(1, "mm"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right",
            legend.key.size = unit(0.3, "cm"))

    terra_dat1<-subset(x, value>=0)
    terra_dat1$var<-recode(terra_dat1$var, "ERR"="err.neg", "str"="str.pos")
    terra_dat2<-subset(x, value<0)
    terra_dat2$var<-recode(terra_dat2$var, "ERR"="err.pos", "str"="str.neg")

    # replace error by its opposite to get symmetrical stacks.
    x[x$var == "ERR", "value"] <- -x[x$var == "ERR", "value"]


    # # divide the dataset in positive and negative values again
    terra_dat1<-subset(x, value>=0)
    terra_dat1$var<-recode(terra_dat1$var, "ERR"="err.neg", "str"="str.pos")
    terra_dat2<-subset(x, value<0)
    terra_dat2$var<-recode(terra_dat2$var, "ERR"="err.pos", "str"="str.neg")

    assign("terra_dat1", terra_dat1, envir = WaterBalanceSummaryEnv)
    assign("terra_dat2", terra_dat2, envir = WaterBalanceSummaryEnv)

    # reorder levels: from top to bottom of graph:
    terra_breaks<-c("aet","err.neg","run", "str.neg", "str.pos", "ppt", "err.pos")
    terra_dat1$var<-factor(terra_dat1$var, levels=terra_breaks)
    terra_dat2$var<-factor(terra_dat2$var, levels=terra_breaks)


    terra_brew<-c(rev(brewer.pal(length(levels(droplevels(terra_dat1$var))), "Reds")), brewer.pal(length(levels(droplevels(terra_dat2$var))), "Blues"))

    terra_brew<-terra_brew[order(terra_breaks)]
    # Ensure the factor levels are correct
    terra_breaks <- c("err.neg", "ppt", "str.pos", "err.pos", "str.neg", "aet", "run")
    terra_dat1$var <- factor(terra_dat1$var, levels = terra_breaks)
    terra_dat2$var <- factor(terra_dat2$var, levels = terra_breaks)
    # Define the color palette
    # Custom shades of red
    terra_custom_reds <- c("aet" = "#DC4B37", "run" = "#E97352", "err.pos" = "#610F14", "str.neg" = "#9D231F")
    terra_custom_blues <- c("err.neg" = "#3182BD","str.pos" = "#A6C8DE", "ppt" = "#6BAED6")
    # Combine the palettes
    terra_color_mapping <- c(terra_custom_blues, terra_custom_reds)

    terra_custom_labels <- c(
      "ppt" = "Precipitation",
      "err.neg" = "Negative Error",
      "err.pos" = "Positive Error",
      "aet" = "Actual ET",
      "run" = "Runoff",
      "str.pos" = "Negative Storage",
      "str.neg" = "Positive Storage"
    )

    p7<-ggplot() +
      geom_bar(data = terra_dat1, aes(x=divide_id, y=value, fill=var),stat = "identity") +
      geom_bar(data = terra_dat2, aes(x=divide_id, y=value, fill=var),stat = "identity") +
      scale_fill_manual(values = terra_color_mapping, breaks=terra_breaks, labels = terra_custom_labels)+
      scale_y_continuous(breaks=pretty_breaks(n=7))+
      labs(x=NULL, y="mm", fill=NULL, title = "Terra Climate Water Balance", subtitle = "01/01/1979 - 12/01/2023")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.key.size=unit(0.3, "cm"),
            plot.title = element_text(size = 14, margin = margin(b = 5)),
            plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 5)))

    # Combine the plots with plot_grid
    combined_plot <- plot_grid(
      p7, p3,
      ncol = 1,
      align = 'v',
      axis = "lr"
    )
  }


  # Draw the combined plot
  grid.newpage()
  grid.draw(combined_plot)
}
