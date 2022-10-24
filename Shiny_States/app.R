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

#******************************
# 0. Database update 
#******************************

#setwd("~/Dropbox/0 Project/COVID19_US/Shiny_States/")

# 0.1 dtaRegion -----
#estimates:Total population, both sexes combined, as of 1 July (thousands)
dtaregion<-read_excel("StatesByCensusRegionAndDivision.xlsx", sheet="Sheet1") %>%
    select(state, region, division)%>%
    filter(is.na(state)==F)  

# 0.2 dtaPop -----
#estimates:Total population, both sexes combined, as of 1 July (thousands)
dtapop<-read.csv("co-est2019-alldata.csv")

dtapop<-dtapop%>%
    mutate(state=as.character(STNAME), 
           pop=as.numeric(POPESTIMATE2019))%>%
    filter(COUNTY==0)%>%
    select(state, pop)

# 0.3 dtaCOVID -----
url<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

dtacases<-read.csv(url)%>%
    rename(country = Country_Region,
           state = Province_State,
           county = Admin2)%>%
    select(country, state, county, FIPS, Combined_Key, starts_with("X"))%>%
    gather(variable, value, starts_with("X"))%>%
    rename(date = variable,
           cases = value)%>%
    mutate(country=as.character(country),
           state=as.character(state),
           county=as.character(county))%>%
    select(country, state, date, cases)%>%
    group_by(country, state, date)%>%
    summarize_all(funs(sum))%>%ungroup()

url<-"https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

dtadeaths<-read.csv(url)%>%
    rename(
        state = Province_State,
        county = Admin2)%>%
    select(state, county, FIPS, Combined_Key, starts_with("X"))%>%
    gather(variable, value, starts_with("X"))%>%
    rename(date = variable,
           deaths = value)%>%
    mutate(
        state=as.character(state),
        county=as.character(county))%>%
    select(state, date, deaths)%>%
    group_by(state, date)%>%
    summarize_all(funs(sum))%>%ungroup()

    #str(dtacases)
    #str(dtadeaths)

dtacovid<-left_join(dtacases, dtadeaths, by = c("state", "date"))%>%
    mutate(date=mdy(substring(date, 2)) )

    #dim(dtacases)
    #dim(dtadeaths)
    #dim(dtacovid)

# 0.4 dta -----
dta<-full_join(dtacovid, dtapop, by = "state")

    #dim(dtacovid)
    #dim(dtapop)
    #dim(dta)

temp<-dta%>%filter(is.na(pop)==TRUE) 
table(temp$state) #territories without pop: DROP THESE below

temp<-dta%>%filter(is.na(cases)==TRUE)
table(temp$state) #territories/States without COVID data = 0

dta<-dta%>%filter(is.na(pop)==FALSE)%>%
    arrange(state, date)%>%
    mutate(
        newcases=cases-lag(cases),
        newcases=ifelse(state!=lag(state), NA, newcases),
        
        newdeaths=deaths-lag(deaths),
        newdeaths=ifelse(state!=lag(state), NA, newdeaths),
        
        incidence=round(100000*cases/pop, 3), # confiremd cases per 100,000 pop
        cfr=round(100*deaths/cases, 3), # deaths per 100 confirmed cases   
        mortality=round(100000*deaths/pop, 3) # deaths per 100000 pop
    )%>%arrange(state, date)%>%
    mutate(
        newcasessmooth =c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                          rollmean(newcases, 14)), 
        newdeathssmooth=c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                          rollmean(newdeaths, 14)), 
        newcasessmooth=ifelse(state!=lag(state), NA, newcasessmooth),
        newdeathssmooth=ifelse(state!=lag(state), NA, newdeathssmooth),
        newcasessmooth =round(newcasessmooth, 3),  
        newdeathssmooth=round(newdeathssmooth, 3),
        
        newcasessmoothpp=round(100000*newcasessmooth/pop, 1),   
        newdeathssmoothpp=round(100000*newdeathssmooth/pop, 1)
        
    )

#data quality problem: cumulative deaths and acses going down in some cases 

summary(dta$newcases)
summary(dta$newdeaths)
temp<-dta%>%filter(newcases<0 | newdeaths<0)
dim(temp)
head(temp, nrow(temp))

temp<-dta%>%filter(newcasessmooth<0 | newdeathssmooth<0)
dim(temp)
head(temp, nrow(temp))    

#Force negative smooth numbers to 0 : mainly for new deaths in 8 observations 

dta<-dta%>%
    mutate(
        newcasessmooth   =ifelse(newcasessmooth<0,    0, newcasessmooth),
        newcasessmoothpp =ifelse(newcasessmoothpp<0,  0, newcasessmoothpp),
        newdeathssmooth  =ifelse(newdeathssmooth<0,   0, newdeathssmooth),
        newdeathssmoothpp=ifelse(newdeathssmoothpp<0, 0, newdeathssmoothpp)
    )

dta<-dta%>%filter(date>="2020-03-01")

# 0.5 dtaUS -----

dtaus<-dtacovid%>%
    select(date, cases, deaths)%>%
    group_by(date)%>%
    summarize_all(funs(sum))%>% #collapse cases and deaths 
    mutate(pop=sum(dtapop$pop))%>% #total popualtion 
    arrange(date)%>%
    mutate(
        newcases=cases-lag(cases),
        newdeaths=deaths-lag(deaths),
        
        newcasessmooth =c(NA,NA,NA,NA,NA,NA,rollmean(newcases, 7)), 
        newdeathssmooth=c(NA,NA,NA,NA,NA,NA,rollmean(newdeaths, 7)), 
        
        newcasessmooth =round(newcasessmooth, 3),  
        newdeathssmooth=round(newdeathssmooth, 3),
        
        newcasessmoothpp_US=round(100000*newcasessmooth/pop, 1),   
        newdeathssmoothpp_US=round(100000*newdeathssmooth/pop, 1))%>%
    filter(date>="2020-03-01")%>%
    select(date, newcasessmoothpp_US)

# 0.6 dtacurve-----

dtacurve<-left_join(dta, dtaregion, by = c("state"))

    #dim(dta)
    #dim(dtaus)
    #dim(dtacurve)

dtacurve<-left_join(dtacurve, dtaus, by = c("date"))

    #dim(dtacurve)

dtacurve<-dtacurve%>%
    arrange(country, state, date)%>%
    group_by(state)%>%
    mutate(
        day = row_number(),
        maxday = max(day),
        retroday = maxday - day, #retrospective day
        latestmonth= maxday-day<=30, 
        newcasessmoothpplatestmonth = newcasessmoothpp,
        newcasessmoothpplatestmonth = ifelse(latestmonth!=1, NA,
                                             newcasessmoothpplatestmonth), 
        
        latest=date==max(date), 
        
        peakcasespp=round(max(newcasessmoothpp, na.rm = TRUE), 1), 
        peakdate=as.character(date),
        peakdate=ifelse(peakcasespp!=newcasessmoothpp, "", peakdate),
        peakdate=ymd(substring(peakdate, 1)) )%>%
    fill(peakdate)%>%
    fill(peakdate, .direction = "up")%>%
    mutate(
        latestcasespp=newcasessmoothpp, 
        latestcasespp=ifelse(latest==FALSE, NA, latestcasespp),
        latestlevel=round((latestcasespp)/(peakcasespp), 3)
    )%>%
    fill(latestlevel)%>%
    fill(latestlevel, .direction = "up")%>%
    ungroup()%>%
    select(country, region, division, state, date, 
           day, retroday, 
           cases, deaths, pop, incidence, cfr, mortality, 
           starts_with("new"), starts_with("start"), 
           starts_with("peak"), starts_with("latest"))
    
# 0.7 Color list ----
#color by USDA region and division

colorlist<- list(color = c(
    '#fee08b' ,
    '#a6d96a' ,
    '#a6d96a' ,
    '#fee08b' ,
    '#a6d96a' ,
    '#a6d96a' ,
    '#fdae61' ,
    '#fee08b' ,
    '#fee08b' ,
    '#fee08b' ,
    '#fee08b' ,
    '#a6d96a' ,
    '#a6d96a' ,
    '#d9ef8b' ,
    '#d9ef8b' ,
    '#d9ef8b' ,
    '#d9ef8b' ,
    '#fee08b' ,
    '#fee08b' ,
    '#fdae61' ,
    '#fee08b' ,
    '#fdae61' ,
    '#d9ef8b' ,
    '#d9ef8b' ,
    '#fee08b' ,
    '#d9ef8b' ,
    '#a6d96a' ,
    '#d9ef8b' ,
    '#a6d96a' ,
    '#fdae61' ,
    '#fdae61' ,
    '#a6d96a' ,
    '#fdae61' ,
    '#fee08b' ,
    '#d9ef8b' ,
    '#d9ef8b' ,
    '#fee08b' ,
    '#a6d96a' ,
    '#fdae61' ,
    '#fdae61' ,
    '#fee08b' ,
    '#d9ef8b' ,
    '#fee08b' ,
    '#fee08b' ,
    '#a6d96a' ,
    '#fdae61' ,
    '#fee08b' ,
    '#a6d96a' ,
    '#fee08b' ,
    '#d9ef8b' ,
    '#a6d96a' ))

# 0.8 Define county and other input list ----
#regionlist<-as.vector(unique(dtacurve$region))
regionlist<-c("Midwest", "Northeast", "South", "West")
retrodaylist<-c("All", "Last 6 months")

#********** 1. USER INTERFACE **********----

ui<-fluidPage(
    
    # Header panel 
    headerPanel("COVID-19 waves in the US: How is my state doing? Seeing is believing"),
    
    # Title panel 
    titlePanel(" "),

    # Main page for output display 
    mainPanel(
        width = 10,
        
        tabsetPanel(type = "tabs",

                    tabPanel("Part 1: Latest snapshot by state",                    

                             h4(strong("New daily cases by state")),    
                             plotlyOutput("plot_state_newcases",
                                          width = 500, height = 500), 
                             
                             h5("The x-axis shows 14-day rolling average of daily new cases per 100,000 population"),
                             h5("Colors represent",
                                a("geogrpahic regions by USDA:",
                                  href="https://www.ars.usda.gov/northeast-area/beltsville-md-bhnrc/beltsville-human-nutrition-research-center/docs/regions/"), 
                                span(style="color: #e66101", "Orange: northeast, "),
                                span(style="color: #fdae61", "light orange: south, "),
                                span(style="color: #d9ef8b", "light green: midwest, "),
                                span(style="color: #a6d96a", "green: west)")),  
                             
                             hr(),
                             
                             #h4("Cumulative incidence (left) and cumulative mortality rates (right) by state"),    

                             #plotlyOutput("plot_state_cumcases_cumdeaths"), 
                             #h5(strong("Cumulative incidence"),
                             #   "(number of reported cases per 100,000 population) and", 
                             #   strong("cumulative mortality rates"), 
                             #   "(number of reported COVID-19 deaths per 100,000 population)."),
                             
                             #hr(), 
                             
                             h6("Data source:", 
                                "All COVID-19 data (i.e., cumulative confirmed cases and deaths by day) come from",
                                a("JHU/CSSE", href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data"),
                                ", accessed on", date, ".",
                                "All data on state population come from", 
                                a("US Census Bureau", href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html#par_textimage"), 
                                ", accessed on March 29, 2020."),   
                             h6("Application last updated on July 2, 2022."),
                             h6("For typos, errors, and questions:", 
                                a("contact YJ Choi at www.iSquared.global", href="https://www.isquared.global/YJ")), 
                             
                    ),
                    
                    tabPanel("Part 2: State Trends by Region",         
                             
                             selectInput("region", 
                                         "Select a region",
                                         choices = regionlist, 
                                         selected = "South Atlantic"), 
                             
                             selectInput("retroday", 
                                         "Select a period",
                                         choices = retrodaylist, 
                                         selected = "Last 6 months"),   
                             
                             h4(strong("New daily cases per 100,000 population, 14-day average")),    
                             plotlyOutput("plot_trend_cases",
                                          width = 1000, height = 600), 
                             
                             h5("The x-axis is date. The y-axis is 14-day rolling average of daily new cases per 100,000 population."),
                             h5(span(style="color: #195F90", 
                                      "The darker blue section of the line represents the lastest 30 days."), 
                                 span(style="color: #BEBEBE", 
                                      "The gray line is the US national data.")),
                                 #span(style="color: #ABABAB", 
                                 #     "The light gray box represents the number of daily new cases 10 or lower per 100,000.")), 
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
    output$text_region <- renderText({
        paste(input$region) 
    })    

    
    ##### output: Tab 1 #####
    
    output$plot_state_newcases <- renderPlotly({

    dtafig<-dtacurve%>%filter(is.na(latestcasespp)==FALSE)%>%filter(date==max(date))  
    #dim(dtafig)
    #table(dtafig$date)
    
    dtafig$state<-factor(dtafig$state, 
                         levels = unique(dtafig$state) 
                         [order(dtafig$latestcasespp, decreasing = FALSE)])
    
    maxx<-max(dtafig$latestcasespp)+5
    
    plot_ly(dtafig, y=~state, x=~latestcasespp, type = 'bar', orientation='h', marker=colorlist,
            text = ~latestcasespp, textposition = 'outside', 
            textfont = list(color = '#000000', size = 20))%>%
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
    
    output$plot_state_cumcases_cumdeaths <- renderPlotly({
        
        dtafig<-dtacurve%>%filter(latest==TRUE)%>%mutate(incidence=round(incidence,0))    
        
        dtafig$state<-factor(dtafig$state, 
                             levels = unique(dtafig$state) 
                             [order(dtafig$incidence, decreasing = FALSE)])
        
        maxx<-max(dtafig$incidence)+5
        
        fig1<-plot_ly(dtafig, y=~state, x=~incidence, type = 'bar', orientation='h', marker=colorlist,
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
        
        dtafig$state<-factor(dtafig$state, 
                             levels = unique(dtafig$state) 
                             [order(dtafig$mortality, decreasing = FALSE)])
        
        fig2<-plot_ly(dtafig, y=~state, x=~mortality, type = 'bar', orientation='h', marker=colorlist,
                      name= "State",
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
                y = 10, type = 'scatter', mode = 'lines',
                fill = 'tonexty',fillcolor='#EEEEEE', opacity=0.1,
                line= list(color = "#F1F1F1") ) %>%    
            add_lines(y = ~newcasessmoothpp_US,
                      line= list(color = "#BEBEBE"),
                      hoverinfo = 'text',
                      text = ~paste(
                          '</br> United States',          
                          '</br> Date: ', date,
                          '</br> New cases per 100,000: ', newcasessmoothpp_US)  )%>%
            add_lines(y = ~newcasessmoothpp,
                      line= list(color = "#70A8CF"),  
                      hoverinfo = 'text',
                      text = ~paste(
                          '</br> State: ', state,          
                          '</br> Date: ', date,
                          '</br> New cases per 100,000: ', newcasessmoothpp)  )%>%
            add_trace(
                y = ~newcasessmoothpplatestmonth, type = 'scatter', mode = 'lines',
                line= list(color = "#195F90") ) %>%
            add_trace(
                y = ~latestcasespp, type = 'scatter', mode = 'markers',
                marker = list(size = 5,color ="#70A8CF"),
                text = ~latestcasespp, 
                textfont=list(size=10, color="#70A8CF"), 
                textposition = "bottom left"
            )%>%  
            add_annotations(
                text = ~unique(state),
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
            filter(region==input$region)%>% 
            filter(if (input$retroday=="Last 6 months") retroday<=180 
                   else retroday>=0)
        
        mindate<-min(dtafig$date)
        maxdate<-max(dtafig$date)
        
        maxy<-max(dtafig$newcasessmoothpp)
        nobs<-ceiling(length(unique(dtafig$state))/4)    
        
        dtafig%>%
            group_by(state) %>%
            do(p = panel(.)) %>%
            subplot(nrows = nobs, shareX = TRUE, shareY = TRUE)  

    })    
}


#********** 3. CREATE APP **********----

shinyApp(ui, server)