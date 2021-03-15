#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(data.table)
require(lubridate)
require(magrittr)
require(ggplot2)
require(scales)
require(plotly)
require(stringr)
require(zoo)
require(ggthemes)

# Functions required ----
match_hk  =function(x, y, testunique =T, makeunique = F,importcol = NULL,showMessages = T, ...) {
    ##see holgerman/toolboxH

    yname = deparse(substitute(y))
    
    # 150119 unique check auf schnelles duplicated umgestellt, auto makeuniuq
    if(testunique ==T){
        check = as.numeric(sum(duplicated(stats::na.omit(y))))
        if(identical(check, 0)) return(match(x, y, incomparables=c(NA, NaN),...))
        
        if(identical(check, 0)==F  & makeunique == F) {
            if(showMessages ==T) message("Duplicated entries:\n", paste(y[duplicated(y)], collapse = "\n"))
            stop(paste(yname ,"ist nicht unique"))
        }
        
        if(identical(check, 0)==F  & makeunique == T) {
            
            ## try to make it nunique
            if(is.null(importcol)) stop("When asking for make unique, please provide vector with values to be imported")
            if(length(importcol) != length(y)) stop("When asking for make unique, please provide vector with values to be imported")
            
            datatable_da = "data.table" %in%  rownames(installed.packages())
            datatable_da
            if(datatable_da) {
                matcher = unique(data.table::data.table(index = y, importcol = importcol))
                matcher = matcher[ index %in% x]
                matchercheck = matcher[,as.numeric(sum(duplicated(stats::na.omit(index))))]
                if(identical(matchercheck, 0)==F  ) {
                    if(showMessages ==T) print(matcher[allDuplicatedEntries(matcher$index)])
                    stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
                }
            }
            
            if(datatable_da==F) {
                matcher = unique(data.frame(index = y, importcol = importcol))
                matcher = matcher[ matcher$index %in% x,]
                matchercheck = as.numeric(sum(duplicated(stats::na.omit(matcher$index))))
                if(identical(matchercheck, 0)==F  ) {
                    if(showMessages ==T) print(matcher[allDuplicatedEntries(matcher$index),])
                    stop(paste(yname ,"ist nicht unique after trying to make index and importcol unique..."))
                }
            }
            return(match(x, y, incomparables=c(NA, NaN),...))
            
        }
        
    }
    if(testunique ==F)  return(match(x, y, incomparables=c(NA, NaN),...))
}
# MAke the input dataframe from webßßßß

cases_age_inc =  fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Cases%20by%20Age_Germany.csv", encoding = "UTF-8")
cases_age_inc$variable = "NewConfCases"

cases_age_cum =  fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Cumulative%20Cases%20by%20Age_Germany.csv", encoding = "UTF-8")
cases_age_cum$variable = "AllConfCases"

death_age_inc =  fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Incident%20Deaths%20by%20Age_Germany.csv", encoding = "UTF-8")
death_age_inc$variable = "NewDeaths"

death_age_cum =  fread("https://raw.githubusercontent.com/KITmetricslab/covid19-forecast-hub-de/master/data-truth/RKI/by_age/truth_RKI-Cumulative%20Deaths%20by%20Age_Germany.csv", encoding = "UTF-8")
death_age_cum$variable = "AllDeaths"

all_age = rbind(cases_age_inc, cases_age_cum, death_age_inc, death_age_cum)


all_age[grep('ringia', location_name), location_name := "Free State of Thuringia"]
all_age[,sort(unique(location_name))]
all_age[,Date := as_date(date)]



# check for negative incidence and correct----

correct_negative_kumul = F

if(correct_negative_kumul==T) {
    
    todo_neg = all_age[value<0, .(location_name, variable, age_group)] %>% unique()
    todo_neg[,num := .I]
    stopifnot(nrow(todo_neg[grep("New", variable)])==nrow(todo_neg))
    todo_neg[,variableCum := stringr::str_replace(variable, "New", "All")]
    stopifnot(all(todo_neg$variableCum %in% all_age$variable))
    
    all_age[,id:= paste(location_name, variable, age_group)]
    
    
    todo_neg[,id_inc:= paste(location_name, variable, age_group)]
todo_neg[,id_cum:= paste(location_name, variableCum, age_group)]
todo_neg

for(mynum in todo_neg$num) {
    # mynum = 1
    myrow = todo_neg[num == mynum]
    stopifnot(nrow(myrow)==1)
    all_age_sub1 = all_age[! id %in% c(myrow$id_inc, myrow$id_cum)]  
    
    all_age_sub2_inc = all_age[id %in% myrow$id_inc]  
    all_age_sub2_cum = all_age[id %in% myrow$id_cum]  
    
    stopifnot(nrow(all_age_sub2_inc) + nrow(all_age_sub2_cum)+ nrow(all_age_sub1)==nrow(all_age))
    setorder(all_age_sub2_cum, -Date)
    
    all_age_sub2_cum[, value_ori := value]
    all_age_sub2_cum[, zeile := .I]
    
    all_age_sub2_cum[,value_davor := c(value[2:.N], NA)]
    all_age_sub2_cum[is.na(value)==F & is.na(value_davor)==F, value_davor_groesser := value<value_davor]
    
    while(sum(all_age_sub2_cum$value_davor_groesser, na.rm = T)>0) {
        message(sum(all_age_sub2_cum$value_davor_groesser, na.rm = T)  , " in work....")
        
        rows_to_NA = all_age_sub2_cum[value_davor_groesser==T, zeile + value_davor_groesser]
        
        all_age_sub2_cum[rows_to_NA, value := NA]
        
        all_age_sub2_cum[, value:= zoo::na.locf(value)]
        
        
        all_age_sub2_cum[,value_davor := c(value[2:.N], NA)]
        all_age_sub2_cum[is.na(value)==F & is.na(value_davor)==F, value_davor_groesser := value<value_davor]
        
        
        
    }
    
    all_age_sub2_cum[, plot(value_ori, value, main = myrow$id_cum)]
    abline(0,1,col = "red")
    
    # nun inc ersetzen----
    all_age_sub2_cum[, value_inc := value-value_davor]
    all_age_sub2_cum[.N, value_inc := value]
    
    all_age_sub2_inc[,value_ori := value]
    all_age_sub2_inc[,value := all_age_sub2_cum[match_hk(all_age_sub2_inc$Date, all_age_sub2_cum$Date),value_inc]]
    
    all_age_sub2_inc[, plot(value_ori, value, main = myrow$id_inc)]
    abline(0,1,col = "red")
    
    all_age = rbind(all_age_sub1, all_age_sub2_inc[,names(all_age), with = F], all_age_sub2_cum[,names(all_age), with = F])

}
}

## add dates so that alldates are present in each stratum  to allow sliding window calculation----

datehelper = all_age[grep("New", variable),.(mindate = min(Date , na.rm = T),
                                              maxdate = max(Date,na.rm = T)), .(location_name, variable)]


stopifnot(nrow(unique(datehelper[,.(mindate, maxdate)]))==1) # wenn auf regionaler ebene unterschiedliche startdatensind, mus dass angepasst werden

totalmin=unique(datehelper$mindate)
totalmin
totalmax=unique(datehelper$maxdate)
totalmax

alldatframe = data.table(expand.grid(Date  = seq(from=totalmin, to = totalmax, by = 1),
                                     location_name = unique(datehelper$location_name),
                                     variable = unique(all_age$variable),
                                     age_group = unique(all_age$age_group)))

alldatframe[,id := paste(Date, location_name, variable, age_group, sep = "__")]
all_age[,id := paste(Date, location_name, variable, age_group, sep = "__")]

all_age4 = merge(all_age, alldatframe[,.(id)], by = 'id', all = T, sort = F)
all_age4[is.na(Date), Date:= str_split(id, "__") %>% sapply(., "[", 1) %>% as_date()]
all_age4[is.na(location_name), location_name:= str_split(id, "__") %>% sapply(., "[", 2)]
all_age4[is.na(variable ), variable := str_split(id, "__") %>% sapply(., "[", 3)]
all_age4[is.na(age_group ), age_group := str_split(id, "__") %>% sapply(., "[", 4)]
all_age4[is.na(value ) & grepl("New", variable), value := 0]

setorder(all_age4, variable, age_group, location_name, Date)
all_age4[ grepl("All", variable), value := zoo::na.locf(value,na.rm = F), .(variable, age_group, location_name)]

all_age4[variable=="NewConfCases", variable2 := "Daily reported incident COVID-19-testpositives"]
all_age4[variable=="AllConfCases", variable2 := "Total reported COVID-19-testpositives"]
all_age4[variable=="NewDeaths", variable2 := "Daily reported COVID-19-deaths"]
all_age4[variable=="AllDeaths", variable2 := "Total reported COVID-19-deaths"]
all_age4[,.N,variable2]


setorder(all_age4, variable, location_name, age_group, Date)
all_age4[age_group == "unbekannt", age_group := "unknown"]
rollingwindow=7# Setting


found_locations  <- unique(c("Free State of Saxony" , all_age4[,unique(location_name)]))
found_variables  <- unique(c( "Daily reported incident COVID-19-testpositives", all_age4[,unique(variable2)]))

all_age4[, value7:= frollmean(value ,n = rollingwindow, na.rm = T, align = "right"),.(variable, age_group, location_name)]

align_direction = c("right", "left", "center")

# DEfine UI----
ui <- pageWithSidebar(
    headerPanel('Age-stratified numbers of COVID-19 testpositives and deaths'),
    sidebarPanel(
        selectInput('locations', 'Select Region', found_locations),
        selectInput('variables', 'Show', found_variables),
        dateInput("date1", "Date start:", value = "2021-01-01"),
        
        # Default value is the date in client's time zone
        dateInput("date2", "Date end:"),
        numericInput('rollingwindow', 'Rolling average (days)', 7, min = 1, max = 28),
        selectInput('align_direction', 'Rolling average type', align_direction),
        selectInput('excludeunknown', 'Exclude unknown ages', c("yes", "no")),
        selectInput('logy', 'Logarithmic scale', c("yes", "no")),
        p("Resize browser window to resize plot, click on legend to show/hide lines."),
        p("Dates refer to the time reported by the RKI."),
        br(),
        img(src = "genstat_twitter.png", height = 90, width = 90),
        br(),
        p("...proudly presented by ",
          a("IMISE/GenStat", 
            href = "http://www.imise.uni-leipzig.de/en/Groups/GenStat/"), " applying data aggregated by ",
          a("KITmetricslab", 
            href = "https://github.com/KITmetricslab/covid19-forecast-hub-de/tree/master/data-truth/RKI/by_age/"), " using comparisons of ", 
          a("daily reports from the RKI", 
            href = "https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0")),
        p("Code available on ",
          a("GitHub", 
            href="https://github.com/holgerman/covid19_germanyByAge")
         
        
          ),

    ),
    mainPanel(
        plotlyOutput('plot1', height = "600px" )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Combine the selected variables into a new data frame
    selectedData <- reactive({
        all_age5 = all_age4[variable2 == input$variables & 
                     location_name == input$locations & 
                     Date>= input$date1 &
                     Date<= input$date2]
        all_age5[, rolling_value:= frollmean(value ,n = input$rollingwindow, na.rm = T, align = input$align_direction),.(variable, age_group, location_name)]
        if(input$excludeunknown=="yes") {all_age6 = all_age5[age_group != "unknown"]}  else {all_age6 = all_age5}
        all_age6
    })
    
    
    
    output$plot1 <- renderPlotly({
    
            p1 = ggplot(selectedData(), aes(Date, rolling_value, color = age_group )) + geom_line() + facet_wrap(~location_name,scales = "free") + scale_y_log10(breaks = log_breaks(10) , label = label_comma(accuracy = 1)) + theme(axis.text.x = element_text(angle = 90, vjust = 0.4)) + theme_pander() +
            scale_x_date(breaks = date_breaks(width = "1 week")) + ylab(paste0(unique(selectedData()$variable2))) + xlab("") + labs(color = "Age Group")
            if(input$logy=="no") {p1 = p1 + scale_y_continuous(breaks = pretty_breaks(10))}  else {p1 = p1 + scale_y_log10(breaks = log_breaks(10) , label = label_comma(accuracy = 1))}
        ggplotly(p1)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
