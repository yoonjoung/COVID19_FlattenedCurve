library(shiny)

library(dplyr)
library(tidyverse)
library(readxl)

library(Matrix)
library(lubridate)
library(stringr)
library(stringi)

library(jsonlite)
library(httr)
library(rlist)
library(zoo)

library(plotly)
library(RColorBrewer) 

#library(conflicted)
#conflict_prefer("filter", "dplyr")
#conflict_prefer("rename", "dplyr")
#conflict_prefer("mutate", "dplyr")
#conflict_prefer("arrange", "dplyr")

date<-as.Date(Sys.time(	), format='%d%b%Y')

# This creates shiny app to display Maryland Covid data  
# There are four parts in this document:
# 0. Database update 
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP     

#********** 0. Database update **********----

#setwd("~/Dropbox/0 Project/COVID19_SouthKorea/Shiny_OECD/")

# 0.1 dtapop----
#estimates:Total population, both sexes combined, as of 1 July (thousands)
    dtapop<-read_excel("WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx")
    
    dtapop<-dtapop%>%
        rename(country = "Region, subregion, country or area *",
               pop = "2020")%>%
        filter(Type=="Country/Area")%>%
        select(country, pop, region)%>%
        mutate(pop=as.numeric(pop))

# 0.2 dtaCOVID----
    url<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
    dtacases<-read.csv(url)%>%
        rename(country = Country.Region,
               region = Province.State)%>%
        gather(variable, value, starts_with("X"))%>%
        rename(date = variable,
               cases = value)%>%
        select(country, date, cases)%>%
        mutate(country=as.character(country))%>%
        group_by(country, date)%>%
        summarize_all(funs(sum))%>%ungroup()
    
    url<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
    dtadeaths<-read.csv(url)%>%
        rename(country = Country.Region,
               region = Province.State)%>%
        gather(variable, value, starts_with("X"))%>%
        rename(date = variable,
               deaths = value)%>%
        select(country, date, deaths)%>%
        mutate(country=as.character(country))%>%
        group_by(country, date)%>%
        summarize_all(funs(sum))%>%ungroup()

    

    dtacovid<-left_join(dtacases, dtadeaths, by = c("country", "date"))%>%
        mutate(date=mdy(substring(date, 2)) )

    #prep for join/merge
    dtacovid<-dtacovid%>%
        mutate(
            country=ifelse(country=="Bolivia","Bolivia (Plurinational State of)", country),
            country=ifelse(country=="Brunei","Brunei Darussalam", country),
            country=ifelse(country=="Burma","Myanmar", country),
            country=ifelse(country=="Congo (Brazzaville)","Congo", country),
            country=ifelse(country=="Congo (Kinshasa)","Democratic Republic of the Congo", country),
            country=ifelse(country=="Cote d'Ivoire","C?te d'Ivoire", country),
            country=ifelse(country=="Iran","Iran (Islamic Republic of)", country),
            country=ifelse(country=="Korea, South","Republic of Korea", country),
            country=ifelse(country=="Laos","Lao People's Democratic Republic", country),
            country=ifelse(country=="Moldova","Republic of Moldova", country),
            country=ifelse(country=="Russia","Russian Federation", country), 
            country=ifelse(country=="Syria","Syrian Arab Republic", country),
            country=ifelse(country=="Taiwan*","China, Taiwan Province of China", country),      
            country=ifelse(country=="Tanzania","United Republic of Tanzania", country),   
            country=ifelse(country=="US","United States of America", country),
            country=ifelse(country=="Venezuela","Venezuela (Bolivarian Republic of)", country),     
            country=ifelse(country=="Vietnam","Viet Nam", country),
            country=ifelse(country=="West Bank and Gaza","State of Palestine", country)
        )

# 0.3 dta----
    dta<-full_join(dtacovid, dtapop, by = "country")

    dta<-dta%>%arrange(country, date)%>%
        mutate(
            newcases=cases-lag(cases),
            newcases=ifelse(country!=lag(country), NA, newcases),
            
            newdeaths=deaths-lag(deaths),
            newdeaths=ifelse(country!=lag(country), NA, newdeaths),
            
            incidence=round(100000*cases/(pop*1000), 1), # confirmed cases per 100,000 pop
            cfr=round(100*deaths/cases, 1), # deaths per 100 confirmed cases   
            mortality=round(100000*deaths/(pop*1000), 1) # deaths per 100000 pop
        )%>%arrange(country, date)%>%
        mutate(
            newcasessmooth =c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(newcases, 14)), 
            newdeathssmooth=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,rollmean(newdeaths, 14)), 
            newcasessmooth=ifelse(country!=lag(country), NA, newcasessmooth),
            newdeathssmooth=ifelse(country!=lag(country), NA, newdeathssmooth),
            newcasessmooth =round(newcasessmooth, 3),  
            newdeathssmooth=round(newdeathssmooth, 3),
            
            newcasessmoothpp=round(100*newcasessmooth/pop, 3),   
            newdeathssmoothpp=round(100*newdeathssmooth/pop, 3),
            
            oecd=(country=="Australia"
                  | country=="Austria"
                  | country=="Belgium"
                  | country=="Canada"
                  | country=="Chile"
                  | country=="Czechia"
                  | country=="Denmark"
                  | country=="Estonia"
                  | country=="Finland"
                  | country=="France"
                  | country=="Germany"
                  | country=="Greece"
                  | country=="Hungary"
                  | country=="Iceland"
                  | country=="Ireland"
                  | country=="Israel"
                  | country=="Italy"
                  | country=="Japan"
                  | country=="Latvia"
                  | country=="Lithuania"
                  | country=="Luxembourg"
                  | country=="Mexico"
                  | country=="Netherlands"
                  | country=="New Zealand"
                  | country=="Norway"
                  | country=="Poland"
                  | country=="Portugal"
                  | country=="Slovakia"
                  | country=="Slovenia"
                  | country=="Republic of Korea"
                  | country=="Spain"
                  | country=="Sweden"
                  | country=="Switzerland"
                  | country=="Turkey"
                  | country=="United Kingdom"
                  | country=="United States of America"), 
            
            country=ifelse(country=="Republic of Korea", "South Korea", country),
            country=ifelse(country=="China, Taiwan Province of China", "Taiwan", country),
            country=ifelse(country=="United States of America", "United States", country)
            
        )

    #data quality problem: cumulative deaths and cases going down in some cases 
    #Force negative smooth numbers to 0 
    
    dta<-dta%>%
        mutate(
            newcasessmooth   =ifelse(newcasessmooth<0,    0, newcasessmooth),
            newcasessmoothpp =ifelse(newcasessmoothpp<0,  0, newcasessmoothpp),
            newdeathssmooth  =ifelse(newdeathssmooth<0,   0, newdeathssmooth),
            newdeathssmoothpp=ifelse(newdeathssmoothpp<0, 0, newdeathssmoothpp)
        )

# 0.4 dtacurve----
    dtacurve<-dta%>%filter(oecd==TRUE)%>%filter(date>="2020-02-01")
    
    #length(unique(dta$country))
    #length(unique(dtacurve$country))
    
    dtacurve<-dtacurve%>%
        arrange(country, date)%>%
        group_by(country)%>%
        mutate(
            day = row_number(),
            maxday = max(day),
            retroday = maxday - day, #retrospective day
            latestmonth= maxday-day<=30, 
            newcasessmoothpplatestmonth = newcasessmoothpp,
            newcasessmoothpplatestmonth = ifelse(latestmonth!=1, NA,
                                                 newcasessmoothpplatestmonth), 
            
            latest=date==max(date), 

            startdate=min(date),
            startcases=newcasessmooth,
            startcases=ifelse(date!=startdate, NA, startcases), 
            startcasespp=round(100*startcases/pop, 1))%>%
        fill(startcases, startcasespp)%>%
        fill(startcases, startcasespp, .direction = "up")%>%
        mutate(
            peakcases=max(newcasessmooth, na.rm = TRUE),
            peakcasespp=round(100*peakcases/pop, 1),
            peakdate=as.character(date),
            peakdate=ifelse(peakcases!=newcasessmooth, "", peakdate),
            peakdate=ymd(substring(peakdate, 1)) )%>%
        fill(peakdate)%>%
        fill(peakdate, .direction = "up")%>%
        mutate(
            endcases=newcasessmooth,
            endcases=ifelse(date<=peakdate, NA, endcases), 
            endcases=ifelse(newcasessmooth>startcases, NA, endcases),
            enddate=as.character(date),
            enddate=ifelse(is.na(endcases)==TRUE, "", enddate),
            enddate=ifelse(is.na(lag(endcases))==FALSE & is.na(endcases)==FALSE, "", enddate),
            enddate=ymd(substring(enddate, 1)))%>%
        fill(enddate)%>%
        fill(enddate, .direction = "up")%>%
        mutate(
            latestcasespp=newcasessmoothpp, 
            latestcasespp=ifelse(latest==FALSE, NA, latestcasespp),
            latestlevel=round((latestcasespp-startcasespp)/(peakcasespp-startcasespp), 3)
        )%>%
        fill(latestlevel)%>%
        fill(latestlevel, .direction = "up")%>%
        fill(latestcasespp)%>%
        fill(latestcasespp, .direction = "up")%>%    
        ungroup()%>%
        select(country, oecd, date, day, retroday, latest, cases, deaths, pop,  incidence, cfr, mortality, starts_with("new"), starts_with("start"), starts_with("peak"), starts_with("end"), starts_with("latest"), -endcases)
    
    dtacurve<-dtacurve%>%
        mutate(latestcasespp=round(ifelse(latest==FALSE, NA, latestcasespp), 1),
               incidence=round(incidence, 0),
               mortality=round(mortality, 0))

# 0.5 Define country list---- 
countrylist<-as.vector(unique(dtacurve$country))

retrodaylist<-c("All", "Last 6 months")

# 0.6 Color list----
    colorlist<- list(color = c(
        "#ffd92f",
        "#66c2a5",
        "#66c2a5",
        "#a6d854",
        "#e78ac3",
        "#8da0cb",
        "#66c2a5",
        "#8da0cb",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#8da0cb",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#ffd92f",
        "#8da0cb",
        "#8da0cb",
        "#66c2a5",
        "#a6d854",
        "#66c2a5",
        "#ffd92f",
        "#66c2a5",
        "#8da0cb",
        "#66c2a5",
        "#8da0cb",
        "#8da0cb",
        "#ffd92f",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#66c2a5",
        "#a6d854"
        ))
    
    
#********** 1. USER INTERFACE **********----
    
    ui<-fluidPage(
        
        # Header panel 
        headerPanel("COVID-19 waves: comparative perspectives among high-resource settings"),
        
        # Title panel 
        titlePanel(" "),
        
        # Main page for output display 
        mainPanel(
            width = 10,
            
            tabsetPanel(type = "tabs",
                        
                        tabPanel("Part 1: Latest snapshot by country",                    
                                 
                                 h4(strong("New daily cases by country")),    
                                 plotlyOutput("plot_country_newcases",
                                              width = 500, height = 500), 
                                 h5("The x-axis shows 14-day rolling average of daily new cases per 100,000 population"),
                                 h5("Colors represent",
                                    a("geogrpahic regions and sub_regions by UN:",
                                      href="https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/docs/regions/"), 
                                    span(style="color: #ffd92f", "Asia-Pacific, "),
                                    span(style="color: #8da0cb", "Eastern Europe, "),
                                    span(style="color: #e78ac3", "Latin American and Caribbean, "),
                                    span(style="color: #a6d854", "North America, "),
                                    span(style="color: #66c2a5", "Western European and other.")
                                    ),  
                                 
                                 hr(),
                                 
                                 #h4("Cumulative incidence (left) and cumulative mortality rates (right) by country"),    
                                 
                                 #plotlyOutput("plot_country_cumcases_cumdeaths"), 
                                 #h5(strong("Cumulative incidence"),
                                 #   "(number of reported cases per 100,000 population) and", 
                                 #   strong("cumulative mortality rates"), 
                                 #   "(number of reported COVID-19 deaths per 100,000 population)."),
                                 
                                 #hr(), 
                                 
                                 h6("Data source:", 
                                    "All COVID-19 data (i.e., cumulative confirmed cases and deaths by day) come from",
                                    a("JHU/CSSE", href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"),
                                    ", accessed on", date, ".",
                                    "All data on country population come from", 
                                    a("UN World Population Prospects 2019", href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage"), 
                                    ", accessed on March 29, 2020."),   
                                 h6("Application last updated on July 14, 2022."),
                                 h6("For typos, errors, and questions:", 
                                    a("contact YJ Choi at www.iSquared.global", href="https://www.isquared.global/YJ")), 
                                 
                        ),
                        
                        tabPanel("Part 2: Country Trends by Region",         
                                 
                                 selectInput("country", 
                                             "Select a country",
                                             choices = countrylist, 
                                             selected = "United States"), 
                                 
                                 selectInput("retroday", 
                                             "Select a period",
                                             choices = retrodaylist, 
                                             selected = "Last 6 months"),   
                                 
                                 h4(strong("New daily cases per 100,000 population, 14-day average")),    
                                 plotlyOutput("plot_trend_cases",
                                              width = 800, height = 500), 
                                 
                                 h5("The x-axis is date. The y-axis is 14-day rolling average of daily new cases per 100,000 population."),
                                 h5(span(style="color: #195F90", 
                                         "The darker blue section of the line represents the lastest 30 days."), 
                                    #span(style="color: #BEBEBE", 
                                    #     "The gray line is the US national data.")),
                                    span(style="color: #ABABAB", 
                                          "The light gray box represents the number of daily new cases 20 or lower per 100,000.")), 
                                 hr()
                                 
                        ),
                        
                        tabPanel("Annex",           
                                 hr()
                        )
            )
        )
    )


#********** 2. SERVER ********** ----

server<-function(input, output) {
    
    ##### general text output of inputs #####
    output$text_country <- renderText({
        paste(input$country) 
    })    
    
    ##### output: Tab 1 #####
    
    output$plot_country_newcases <- renderPlotly({
        
        dtafig<-dtacurve%>%filter(is.na(latestcasespp)==FALSE)%>%filter(date==max(date))  
        #dim(dtafig)
        #table(dtafig$date)
        
        dtafig$country<-factor(dtafig$country, 
                             levels = unique(dtafig$country) 
                             [order(dtafig$latestcasespp, decreasing = FALSE)])
        
        maxx<-max(dtafig$latestcasespp)+5
        
        plot_ly(dtafig, y=~country, x=~latestcasespp, type = 'bar', orientation='h', marker=colorlist,
                text = ~latestcasespp, textposition = 'outside')%>%
            layout(
                #title = c("Daily new COVID-19 cases per 100,000 population"),     
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=10)),
                xaxis = list(title = "per 100,000 population", 
                             range=c(0,maxx)) 
            )
        
    })
    
    output$plot_country_cumcases_cumdeaths <- renderPlotly({
        
        dtafig<-dtacurve%>%filter(latest==TRUE)%>%mutate(incidence=round(incidence,0))    
        
        dtafig$country<-factor(dtafig$country, 
                             levels = unique(dtafig$country) 
                             [order(dtafig$incidence, decreasing = FALSE)])
        
        maxx<-max(dtafig$incidence)+5
        
        fig1<-plot_ly(dtafig, y=~country, x=~incidence, type = 'bar', orientation='h', marker=colorlist,
                      text = ~incidence, textposition = 'outside')%>%
            layout(
                title = c("COVID-19 confirmed cases per 100,000 population"),     
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=10)),
                xaxis = list(title = "per 100,000 population",
                             range=c(0,maxx)) 
            )
        
        dtafig<-dtacurve%>%filter(latest==TRUE)%>%mutate(mortality=round(mortality,0))        
        
        dtafig$country<-factor(dtafig$country, 
                             levels = unique(dtafig$country) 
                             [order(dtafig$mortality, decreasing = FALSE)])
        
        fig2<-plot_ly(dtafig, y=~country, x=~mortality, type = 'bar', orientation='h', marker=colorlist,
                      name= "country",
                      text = ~mortality, textposition = 'outside')%>%
            layout(
                title = c("COVID-19 deaths per 100,000 population"),     
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=10)),
                xaxis = list(title = "per 100,000 population") 
            )
        
        subplot(fig1, fig2,
                nrows=1, margin=0.05, shareY = FALSE, titleY = TRUE) %>%
            layout(  title ="", 
                     legend = list(font=list(size=9), 
                                   orientation = "h", xanchor = "center",  
                                   x=0.5, y=-0.15) 
            )
    })
    ##### output: Tab 2 #####
    
    output$plot_trend_cases <- renderPlotly({

        panel <- . %>% 
            plot_ly(x = ~date)%>% 
            add_trace(
                y = 0, type = 'scatter', mode = 'lines',
                line= list(color = "#F1F1F1") ) %>%    
            add_trace(
                y = 20, type = 'scatter', mode = 'lines',
                fill = 'tonexty',fillcolor='#EEEEEE', opacity=0.1,
                line= list(color = "#F1F1F1") ) %>%    
            add_lines(y = ~newcasessmoothpp,
                      line= list(color = "#70A8CF"),  
                      hoverinfo = 'text',
                      text = ~paste(
                          '</br> Country: ', country,          
                          '</br> Date: ', date,
                          '</br> New cases per 100,000: ', newcasessmoothpp)  )%>%
            add_trace(
                y = ~newcasessmoothpplatestmonth, type = 'scatter', mode = 'lines',
                line= list(color = "#195F90", width = 5) ) %>%
            add_trace(
                y = ~latestcasespp, type = 'scatter', mode = 'markers',
                marker = list(size = 5,color ="#70A8CF"),
                text = ~latestcasespp, 
                textfont=list(size=10, color="#70A8CF"), 
                textposition = "bottom left"
            )%>%  
            add_annotations(
                text = ~unique(country),
                x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)  )%>%
            layout(
                showlegend = FALSE,
                yaxis=list(title=" " , 
                           range=c(0,maxy), showgrid = FALSE),
                xaxis=list(title="" , range=c(mindate,maxdate), showgrid = FALSE)
            ) 
        
        dtafig<-dtacurve%>%
            mutate(latestcasespp=round(ifelse(latest==FALSE, NA, latestcasespp), 0))%>%
            filter(country==input$country)%>%
            filter(if (input$retroday=="Last 6 months") retroday<=180 
                   else retroday>=0)
        
        mindate<-min(dtafig$date)
        maxdate<-max(dtafig$date)                
        maxy<-max(dtafig$newcasessmoothpp)

        dtafig%>%
            group_by(country) %>%
            do(p = panel(.)) %>%
            subplot(shareX = TRUE, shareY = TRUE)  
        
    })    
}


#********** 3. CREATE APP **********----

shinyApp(ui, server)