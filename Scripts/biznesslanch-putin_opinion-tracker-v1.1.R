Sys.setlocale("LC_CTYPE", "russian")

# Load packages ----------------
library(plotly)
library(broom)
library(devtools)
library(lubridate)
library(tidyverse)

# load Data ---------------
# Note: need to read in API key first (in separate script)
source_url("https://raw.githubusercontent.com/biznesslanch/Russian-Public-Opinion-Tracker/master/Scripts/biznesslanch-putin_opinion-get_data.R",
       encoding = "UTF-8")

# remove No answer and hard to say from Levada and FOM data
levada_data <- levada_data %>% select(-No.Answer)
fom_data    <- fom_data %>% select(-hard_to_say)

combined_data <- rbind(levada_data, vtsiom_data, fom_data)
combined_data <- combined_data %>% filter(Date >= "2012-03-04")
combined_data <- combined_data %>% mutate(Date = as.POSIXct(Date + 1, tz="GMT+3", origin="1970-01-01")) #add 1 day to Date to get correct dates
combined_data$date_num <- as.numeric(combined_data$Date)*1000

# create shortcuts for plotly plot -------------------------
m <- loess(Approve ~ date_num, span=0.1, data= combined_data)
l <- loess(Disapprove ~ date_num, span=0.1, data= combined_data)
tdy_date_dt <- Sys.Date()
tdy_date <- format(tdy_date_dt, "%m/%d/%Y")
update <- "Last updated:"
update_cap <- paste(update, tdy_date, sep=" ")

axy <- list(title="Approval/Disapproval", hoverformat=".1f")
axx <-  list(title="", type="date",
             tickmode="array",
             tickvalue = combined_data$Date,
             spikemode="across", spikecolor="black", spikethickness=1, spikedash="solid", spikesnap="cursor",
             rangeselector = list(
               buttons = list(
                 list(
                   count = 3, 
                   label = "3 mo", 
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 6, 
                   label = "6 mo", 
                   step = "month",
                   stepmode = "backward"),
                 list(
                   count = 1, 
                   label = "YTD", 
                   step = "year",
                   stepmode = "todate"),
                 list(
                   count = 2, 
                   label = "2 yrs", 
                   step = "year",
                   stepmode = "backward"),
                 list(step = "all"))),
             rangeslider=list(type="date", visible=TRUE, thickness=0.0))



# plot -------------------------------------------------------------

opinion_plot <- combined_data %>% na.omit() %>% plot_ly(., x= ~date_num, height = "600", width = "1100") %>% 
  add_markers(y= ~Approve, showlegend=FALSE, color =I("royalblue3"), alpha=0.2,
              hoverinfo = "none") %>%
  add_markers(y= ~Disapprove, showlegend=FALSE, color =I("red2"), alpha=0.2,
              hoverinfo = "none") %>% 
  add_lines(y= ~fitted(loess(Approve ~ date_num, span=0.1)), showlegend=TRUE,
            line= list(color="rgb(58,95,205"),
            name="Approval",
            hoverinfo="text",
            text= ~paste("Date:", format(Date, "%m-%d-%Y"),
                         "<br>Approval:", round(m$fitted,1))) %>%
  add_lines(y= ~fitted(loess(Disapprove ~ date_num, span=0.1)), showlegend=FALSE,
            line= list(color="red2"),
            name="Disapproval",
            hoverinfo="text",
            text= ~paste("Date:", format(Date, "%m-%d-%Y"),
                         "<br>Disapproval:", round(l$fitted,1))) %>%
  add_ribbons(data = augment(m),
              ymin = ~.fitted - 1.96 * .se.fit,
              ymax = ~.fitted + 1.96 * .se.fit,
              color= I("royalblue3"), alpha=0.4, showlegend=FALSE, hoverinfo="none",
              line=list(color="rgba(46,91,187,0.0)")) %>% 
  add_ribbons(data = augment(l),
              ymin = ~.fitted - 1.96 * .se.fit,
              ymax = ~.fitted + 1.96 * .se.fit,
              color= I("red2"), alpha=0.4, showlegend=FALSE, hoverinfo="none",
              line=list(color="rgba(220,14,14,0.0)")) %>% 
  add_annotations(xref="paper",yref="paper", xanchor="left",
                  x=0.65,y=1.075,
                  text="Putin Approval Rating<br><sup>From March 2012</sup><br>",
                  showarrow=F, align="left",
                  font=list(size=26,family="Arial")) %>%
  add_annotations(xref="paper",yref="paper", xanchor="left",
                  x=0.65,y=0.97,
                  text=update_cap,
                  showarrow=F, align="left",
                  font=list(size=12,family="Arial")) %>%
  layout(xaxis=axx, autosize=F,
         yaxis=axy, 
         font=list(family="arial"),
         hovermode="compare",
         hoverlabel=list(font=list(size=16)),
         spikedistance=-1,
         hoverdistance=-1,
         annotations = list(
           list(xref = "paper", yref="paper", xanchor="middle", yanchor="middle",
                x=0.325, y=1.025, showarrow=F, align="middle",
                text = "Key",
                font = list(size=12, family="Arial", color="#404040")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.375, y = 1, showarrow = F, align = "left",
                text = "<b>Lower bound</b>",
                font = list(size = 10, family = "Arial", color = "#404040")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.375, y = 1.04, showarrow = F, align = "left",
                text = "<b>Higher bound</b>",
                font = list(size = 10, family = "Arial", color = "#404040")),
           
           list(xref = "paper", yref = "paper", xanchor = "left", yanchor = "middle",
                x = 0.485, y = 1.02, showarrow = F, align = "left",
                text = "<b>Estimate</b>",
                font = list(size = 10, family = "Arial", color = "#404040"))),
         shapes= list(
           list(type = "rectangle",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 1, y1 = 1.04,
                fillcolor = "#d9d9d9",
                line = list(width = 0)),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 1, y1 = 1,
                line = list(width = 2, color = "#8c8c8c")),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 1.04, y1 = 1.04,
                fillcolor = "#bfbfbf",
                line = list(width = 2, color = "#8c8c8c")),
           
           list(type = "line",
                xref = "paper", yref = "paper",
                x0 = 0.45, x1 = 0.48, y0 = 1.02, y1 = 1.02,
                fillcolor = "#bfbfbf",
                line = list(width = 2, color = "#404040")))) %>%
  config(displayModeBar=F)
  
opinion_plot

# note, this last line will not run w/o the API key
api_create(opinion_plot, filename = "biznesslanch-ru_opinion-tracker-v1.1")
