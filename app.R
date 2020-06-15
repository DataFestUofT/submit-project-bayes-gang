library(shiny)
library(shinydashboard)
library(tidyverse)
library(raster)
library(leaflet)
library(shinythemes)
library(rgdal)
library(sf)
library(maps)
library(readxl)
library(plotly)
library(DT)

### DATA FOR CHANGE UNEMPLOYMENT TOTAL ###
data_work_class <- read_xlsx('employed worker class.xlsx', 
                             sheet = 'worker class')
data_work_class$total <- data_work_class$`Agriculture and related industries` +
  data_work_class$`Nonagricultural industries`

data_work_class$mon_year <- paste(data_work_class$month, data_work_class$year, 
                                  sep = ' ')

### DATA FOR UNEMPLOYMENT BY INDUSTRY ###
unemp_total_long <- read.csv("unemp_total_long.csv")

## need this too:
data_ind <- as.data.frame(read_excel('unemployment by industry.xlsx', sheet='tr'))

# Change null values to NA
for (i in 1:nrow(data_ind)) {
  for (j in 1:ncol(data_ind)) {
    if (data_ind[i,j]=='-') {
      data_ind[i,j] <- NA
    }
  }
}

# EDA
## Select out major industries
major_ind <- c('Mining, quarrying, and oil and gas extraction',
               'Construction', 'Manufacturing', 
               'Wholesale and retail trade',
               'Transportation and utilities',
               'Information', 'Financial activities',
               'Professional and business services',
               'Education and health services',
               'Leisure and hospitality',
               'Other services',
               'Agricultural and related private wage and salary workers',
               'Government wage and salary workers',
               'Self-employed workers, unincorporated, and unpaid family workers')

# Unemployment rate increase APR 2019 vs. APR 2020
# Total 
inc_rate_tot <- as.numeric(data_ind[4,-(1:3)])/
  as.numeric(data_ind[3,-(1:3)])

# Male
inc_rate_m <- as.numeric(data_ind[6,-(1:3)])/
  as.numeric(data_ind[5,-(1:3)])

# Female
inc_rate_f <- as.numeric(data_ind[8,-(1:3)])/
  as.numeric(data_ind[7,-(1:3)])

# Correspond industry names to rate increase
industries <- colnames(data_ind)[-(1:3)]
names(inc_rate_tot) <- industries
names(inc_rate_m) <- industries
names(inc_rate_f) <- industries

# sort(inc_rate_tot)

# Look for industries with high and low unemployment rate increase

maj_rate <- sort(inc_rate_tot[major_ind], decreasing = TRUE)

data_work_class <- data_work_class %>% rename(Date = mon_year)
data_work_class <- data_work_class %>% rename(Value = total)

### SHAPEFILES FOR MAPS ###
#states <- map_data("state") #its actually just a dataframe

### DATA FOR random intercept ###
intercept_by_state <- read.csv("intercept_by_state(1).csv")
intercept_by_state <- intercept_by_state %>% mutate(X = as.character(X))
intercept_by_state <- intercept_by_state %>% rename(rate = actual)
intercept_by_state <- intercept_by_state %>% mutate(rate = round(rate*100,2))

#intercept_by_state_combined <- right_join(states, intercept_by_state, by = "region")

### DATA for covariates ###

covariates <- read.csv("cleaned_county_data.csv")
covariates_bystate <- covariates %>% filter(is.na(County))
covariates_bystate <- covariates_bystate %>% dplyr::select(State, Fair.or.Poor.Health.....Fair.or.Poor.Health, High.School.Grad...High.School.Graduation.Rate)
covariates_bystate <- covariates_bystate %>% rename(health = Fair.or.Poor.Health.....Fair.or.Poor.Health,
                                   HSG = High.School.Grad...High.School.Graduation.Rate, 
                                   X = State)
covariates_bystate <- covariates_bystate %>% mutate(X = as.character(tolower(X)))
covariates_bystate <- covariates_bystate %>% mutate(health = round(health,2))

corona <- read.csv('cases-by-state.csv')
corona <- corona %>% select (-X)
corona <- corona %>% rename(X=state)
corona <- corona %>% mutate(X = as.character(tolower(X)))
covariates_bystate <- covariates_bystate %>% left_join(corona, by="X")

covariates_bystate <- covariates_bystate %>% mutate(region = case_when(
  X == "alabama" ~ "AL",
  X == "alaska" ~ "AK",
  X == "arizona" ~ "AZ",
  X == "arkansas" ~ "AR",
  X == "california" ~ "CA",
  X == "colorado" ~ "CO",
  X == "connecticut" ~ "CT",
  X == "delaware" ~ "DE",
  X == "florida"~ "FL",
  X == "georgia" ~ "GA",
  X == "hawaii" ~ "HI",
  X == "idaho" ~ "ID",
  X == "illinois" ~ "IL",
  X == "indiana" ~ "IN", 
  X == "iowa" ~ "IA",
  X == "kansas" ~ "KS",
  X == "kentucky"~ "KY",
  X == "louisiana" ~ "LA",
  X == "maine" ~ "ME",
  X == "maryland" ~ "MD",
  X == "massachusetts" ~ "MA",
  X == "michigan" ~ "MI",
  X == "minnesota" ~ "MN",
  X == "mississippi" ~ "MS",
  X == "missouri" ~ "MO",
  X == "montana" ~ 'MT',
  X == "nebraska"~ "NE",
  X == "nevada"~ "NV",
  X == "new hampshire"~ "NH",
  X == "new jersey"~ "NJ",
  X == "new mexico" ~ "NM",
  X == "new york" ~ "NY",
  X == "north carolina" ~ "NC",
  X == "north dakota" ~ "ND",
  X == "ohio" ~ "OH",
  X == "oklahoma" ~ "OK",
  X == "oregon" ~ "OR",
  X == "pennsylvania" ~ "PA",
  X == "rhode island" ~ "RI",
  X == "south carolina" ~ "SC",
  X == "south dakota"~ "SD",
  X == "tennessee"~ "TN",
  X == "texas"~ "TX",
  X == "utah"~ "UT",
  X == "vermont"~"VT",
  X == "virginia"~"VA",
  X == "washington"~"WA",
  X == "west virginia"~"WV",
  X == "wisconsin" ~ "WI",
  X == "wyoming" ~ "WY",
))

poverty <- read.csv("PovertyEstimates.csv")
poverty <- poverty %>% select(X, X.2)
poverty <- poverty %>% filter(X != "")
poverty <- poverty[-1,]
covariates_bystate <- covariates_bystate %>% bind_cols(poverty)
covariates_bystate <- covariates_bystate %>% rename(poverty = X.2)
covariates_bystate <- covariates_bystate %>% mutate(poverty = as.numeric(as.character(poverty)))

## Data for stay at home orders##
stayhome <- read.csv("coronavirus-first-case-after-national-emergency-announcement.csv")
stayhome <- stayhome %>% mutate(Stay.at.home.order = as.character(Stay.at.home.order))
stayhome <- stayhome %>% mutate(order = ifelse(Stay.at.home.order=="", 0,1))
stayhome <- stayhome %>% mutate(Stay.at.home.order = ifelse(Stay.at.home.order=="", NA,Stay.at.home.order))
stayhome <- stayhome %>% rename(X=State)

intercept_by_state <- intercept_by_state %>% left_join(stayhome, by ="X")

## number of days to JLR ##
nofdays <- read.csv('no of days.csv')
nofdays <- nofdays %>% filter(!is.na(Days.to.Issue.Stay.at.home.from.National.Announcment))
fit <- lm(intercept ~ Days.to.Issue.Stay.at.home.from.National.Announcment, data=nofdays) %>% fitted.values()

### FOR CORRELATION PLOTS ###
by_county <- covariates %>% filter(!is.na(County))
by_county <- by_county %>% dplyr::select(State, Fair.or.Poor.Health.....Fair.or.Poor.Health, High.School.Grad...High.School.Graduation.Rate, Children.in.Poverty.....Children.in.Poverty, JLR)
by_county <- by_county %>% rename(health = Fair.or.Poor.Health.....Fair.or.Poor.Health,
                                                    HSG = High.School.Grad...High.School.Graduation.Rate, 
                                                    poverty = Children.in.Poverty.....Children.in.Poverty,
                                                    X = State)
by_county <- by_county %>% mutate(X = as.character(tolower(X)))
by_county <- by_county %>% left_join(corona, by="X")

## data to finalize ##
final_data <- read.csv("actual_final_data.csv")
final_data <- final_data %>% select(-X)
final_data <- final_data %>% rename(cases_by_pop = casesT)
final_data <- final_data %>% rename(percent_poverty = Pctpov)
final_data <- final_data %>% rename(highschool_grad_rate = HSGr)
final_data <- final_data %>% rename(percent_bad_health = Health)
final_data <- final_data %>% mutate(percent_bad_health = 100*percent_bad_health)
final_data <- final_data %>% mutate(percent_poverty = 100*percent_poverty)


### political graphs ###
political <- read.csv("political_graphs.csv")

## model coeffs ##
model_summary <- read.csv("model_coefficients.csv")
model_summary<-model_summary%>%filter(vars != "Intercept")
model_summary <- model_summary %>% mutate(vars = as.character(vars))
model_summary[4,1] <- "% Poverty"
model_summary[3,1] <- "% of Cases by Pop"
model_summary <- model_summary %>% mutate(vars = as.factor(vars))

##THE APP##

sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Background", tabName = 'background', icon = icon("cloud")),
    menuItem("Our Research Question", tabName = 'researchq', icon = icon("question")),
    menuItem("The Data", tabName = 'data', icon = icon("table")),
    menuItem("Maps", tabName = 'maps', icon = icon("globe")),
    menuItem("Model", tabName = 'model', icon = icon("project-diagram")),
    menuItem("Results and Discussion", tabName = 'conclusions', icon = icon("user-friends")),
    menuItem("Conclusions", tabName = 'limits', icon = icon("bullseye"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "background",
            h1("An Analysis of Local Health, Social, and Political Factors on Low Income Job Loss Rate in the United States during the COVID-19 Pandemic"),
            h2("Amar Dholakia, Joshua Hug, Rachael Jaffe, Wendi Qu"),
            br(),
            h3("Background"),
            p("After COVID-19 became more severe in the US, non-essential workplaces were forced to shut down. 
            Businesses, especially small ones, were suffering from the lack of customers, or were unable to serve customers due to being shut down."),
            p("As a result, there were mass layoffs and employment plummeted."),
            p(),
            fluidRow(
              plotlyOutput("unemployment"),
            ),
            p(),
            p("As our research progressed, we noticed that different industries were affected at different levels: "),
            tags$li('Those requiring direct human contact such as restaurants or manufacturing.'),
            tags$li("Formal office work could be more easily transitioned to teleworking.
"),
            p(),
            p("We observed several other factors that might contribute to job loss."),
            tags$li("Sex"),
            tags$li("Health (both COVID related and not)"),
            tags$li("Education level"),
            tags$li("Income"),
            p(),
            p("Here is a visualization of the job loss by industry and by sex:"),
            p(),
            fluidRow(
              box(checkboxGroupInput("morf","Total or by Sex",choices = c("Total" = "total", "Male" = "male", "Female" = "female"), selected = "total"),
            )),
            fluidRow(
              plotlyOutput("byindustry",height = 900,width = 1000)
            ),
            tags$style(type='text/css', "#button { vertical-align: middle; height: 50px; width: 100%; font-size: 30px;}"),
            p(),
            p("We also thought it would be very telling to look at the stay at home orders per state as they could be indicative of job loss. Please hover over the state to see which date a stay at home order was issued."),
            plotlyOutput("statehome")
          ),
    tabItem(tabName = "researchq",
            h3("Our Research Questions"),
            p("1. Which factors most impact low income job loss due to COVID-19?"),
            p("2. Which states are more impacted by low income job loss due to COVID-19?"),
                    
            h3("Our Hypotheses:"),
            p("1. A greater number of cases should correspond with a greater low income job loss rate (LI JLR)."),
            p("2. A more educated population should correspond with a lesser LI JLR "),
            p("3. A poorer population should correspond with a greater LI JLR."),
            p("4. A healthier population should correspond with a lesser LI JLR."),
            p("5. States that generally poll democrats should correspond to higher LI JLR.")
              ),
    tabItem(tabName = "data",
            h3("Our Data Sources"),
            p("We got the majority of our data from the", 
              span(tags$a (href = "https://datacatalog.urban.org/dataset/estimated-low-income-jobs-lost-covid-19", 
                            "Urban Institute."),"They collected low income job loss rate (defined as < $40,000 per year) from", 
                    span(tags$a (href = "https://www.bls.gov/data/", 
                            "the Bureau 
                    of Labor Statistics"),  "between February 2020 and April 2020."))),
            p(),
            p("To get our covariates we used",
              span(tags$a (href = "https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/?fbclid=IwAR0VuhpYG9jPOk7l9uMMhAWR1SRyEJwWBmapAJk7oKNfKXtxJNo4VsZnCpc", 
                                                           "the US Department of Agriculture data services"), 
                   "and",
                   span(tags$a (href = "https://www.countyhealthrankings.org/app/california/2020/downloads?fbclid=IwAR3e5OglXlJfadVaxOr0iyF5_mUSr-dXrqlYwUtm_so7bIRdtGKZeiAjWus", 
                                "the county health ranking and roadmaps.")))),
            p(),
            p("All our data is by county within the United States."),
            p(),
            fluidRow(
              dataTableOutput("data")
            )
    ),
    tabItem(tabName = "conclusions",
            h3("A discussion of the results"),
            p("Below we plot a correlation between each predictor and job loss rate to display the relationships within the data. "),
            fluidRow(
              box(title = "Select Covariate",
                  selectInput('covariate', label = "Covariates", choices = c(
                    "Percent in Poor or Fair Health" = "percent_bad_health",
                    "High School Graduation Rate" = "highschool_grad_rate",
                    "Percent of people in poverty" = "percent_poverty",
                    'COVID-19 Cases by population' = "cases_by_pop"
                  ), selected = 1)
              )),
            fluidRow(
              box(plotlyOutput("correlations"),width = 12,height = NULL)),
            p("Overall, we see differing and some non linear trends between the covariates and the outcome. Consequently, we hypothesize that LI JLR rate between February 2020 and April 2020 is more related to the “politics” of a state, rather than concentration of COVID-19 or other societal factors. In other words, we would expect to see a larger LI JLR if a state had earlier stay at home orders because non-essential business closed earlier. "),
            p("Here is the average low income job loss by state based on our model. Hovering over each state you will see the average LI JLR and the date that the state issued a lockdown."),
            plotlyOutput("bystate"),
            p(),
            p('The plot above supports Hypothesis 5. Those states that closed earlier (i.e. California, Oregon, Washington State) have slighly higher LI JLR rates, meanwhile those states that did not close at all have some of the lowest LI JLR rates.'),
            p(),
            p("Here is the correlation between the the days since March 13th when President Trump declared a national emergency and the LI JLR by state estimated from the model:"),
            p("There are a few states who are not included in the plot below because they never issued a stay at home order."),
            fluidRow(
              box(plotlyOutput("numdays"),width = 12,height = NULL)
            ),
            p("As you can tell there is a negative correlation between closing and jobs lost. The earlier you close the greater number of jobs lost."),
            p(),
            p("Lastly as we have seen this pandemic has become highly politicized, we plot the following graphs:"),
            p(),
            fluidRow(
              box(plotlyOutput("dem"),width = 12,height = NULL)),
            p(),
            fluidRow(
              box(plotlyOutput("rep"),width = 12,height = NULL)),
            p("From these graphs, we see that if the percentage of the state who views align with the Democratic party increases, LI JLR increased. Meanwhile, we see the opposite trend occuring for those states where the majority aligns with the Republican party.")
            ),
    tabItem(tabName = 'limits',
            h3("Conclusions"),
            p("Our initial results were rather counterintuitive. In regards to hypothesis 1, 
            we observed no noticeable trend for county rate of COVID-19 cases and LI JLR since the 
            coefficient was within a standard error of 0; we cannot claim that 
            county rate of cases has an effect on county level LI-JLR.
            This is perhaps not surprising because most states had state-wide lockdown orders, 
            thus generalizing to county regardless of individual county infection rate. 
            Therefore we expect that the number of cases does not have a large correlation 
            with the number of low income jobs lost.
"),
            p(" In regards to Hypothesis 2, we observed that the 
            proportion of high school graduates had a strong positive 
            effect on LI JLR (a higher educated county indicated more low income 
            jobs lost).  We assumed that a higher high school graduation rate would correlate with 
            a lower low income job loss rate, but this was opposite the observed result. This ‘opposite’ effect may however be explained by states with poorer education metrics also having less restrictive lockdown orders.
"),
            p("In regards to Hypothesis 3, we saw that a higher poverty rate corresponded to a lower LI JLR (less jobs lost). This observed direction is similar to that of the high school graduation rate being correlated with more jobs being lost.
"),
            
            p("Finally, a higher rate of fair or poor health also corresponded to a larger job loss 
            rate, indicating that states with less healthy populations incurred greater rates of job loss, 
            supporting a higher low-income job loss rate. This is particularly concerning as people with medical issues already at risk for COVID-19 are also now in danger of being unable to make rent, may resort to inexpensive but unhealthy nutrition, and have trouble affording better personal and medical care.
"),
            h4("Main points:"),
            p("State ‘politics’, including number of days non-essential businesses were closed down and state political leaning were more indicative of LI JLR."),
            tags$li("States with earlier closed-down dates and more restrictive policy suffered from greater LI-JLR."),
            tags$li("States that are more liberal leaning-suffered from greater LI-JLR and converse was seen for conservative-leaning states."),
            
            h3("Limitations"),
            p("Our model is poorly indicative of which factors affected JLR because the increase in 
              JLR between February 2020 and April 2020 was so drastic in such a short period of 
              time and effectively shocked the labor market.  If we were to re-do this project 
              in the future, the model might be able to predict which factors can affect the LI 
              JLR. Also, we would be able to identify socioeconomic and political 
              factors that reinvigorate re-employment for when things go back to “normal”."),
            p("We looked only at LI JLR under the assumption that these jobs would exhibit more drastic change in LI JLR. However, high income JLR may also be worth looking into as they may have different factors contributing to unequal JLR."),
            
            h3("Future Directions"),
            p("An interesting option for further research would be to know why those living in 
            richer, more educated counties experienced a higher low income job loss rate. One possibility is that poorer and less educated counties have a much higher number of low income jobs, therefore even though some jobs were lost, there was not a large percentage of low income jobs lost. 
"),
            p("In future models, we can include the number of days in a 'locked down' state and political leaning for each state as predictors in the model to investigate their effect on low income JLR."),
            p("Also, we can model on high income JLR and compare if any significant predicting factors act similarly or differently from low income JLR."),
            p("Lastly, hopefully when the virus is better controlled and if the economy recovers, we can examine how different factors affect re-employment rate and compare with our findings here to look for factors with stronger causal relationship to JLR.")
            
            ),
    tabItem(tabName = "model",
            h3("Our Model"),
            p("To model low income job loss rate by county within state, we used a linear mixed model with a random intercept for each state."),
            p("The fixed effects estimates in this model represent 
              the change in logit job loss rate that corresponds 
              to a deviation in the mean of any of the covariates holding state 
              and other covariates constant. All percentage values are represented 
              as decimal values between 0 and 1 and our covariates are all 
              centered for easier interpretation of the coefficients and the random intercepts."), 
             p(" The random intercepts for each state are displayed below on the map. 
              This intercept can be interpreted as the average job loss rate by 
              state, since all the other variables are centered in the model. 
               State random intercept (between-group variation) explained approximately 86.2% 
               of total variation in the model, justifying its inclusion in the model. 
               Features were not inter-correlated and small standard errors suggest lack of multicollinearity and strong validity of parameter estimates."),
            p("The following covariates were added to the model: "),
            tags$li("% of COVID-19 cases per population"),
            tags$li("% of the county below the poverty line"),
            tags$li("% of the county that graduated high school"),
            tags$li("% of the county in fair or poor health"),
            p(),
            p("The plot below shows the point estimates for the coefficients of the model as well as error bars representing a deviation of 1 standard error."),
            p(),
            fluidRow(
              plotlyOutput("coeffs")
            ),
            p(),
            p("Each coefficient is centered so their magnitudes are directly comparable. 
              We can see that fair or poor health and high school graduation rate are positively associated with LI JLR, meanwhile poverty is negatively associated. 
              Since the standard error is so large for COVID-19 cases, we can say that this predictor has no effect on LI JLR. "),
            p(),
            p("Here is map of each random intercept, interpreted as average LI JLR by state. Please hover over the states to obtain the values."),
            p(),
            fluidRow(
            plotlyOutput("bystateintercept")
            )
    ),
    tabItem(tabName = "maps",
            h3("Below are choropleth maps of each covariate in the model generalized by state."),
            p(),
            p("Each map is interactive and if you hover over the state the value associated with the covariate will pop up."),
            fluidRow(
              box(title = "Which covariate to add to the map?",
                  selectInput('predictors', label = "Covariates", choices = c(
                    "Percent in Poor or Fair Health" = "health",
                    "High School Graduation Rate" = "HSG",
                    "Percent of people in poverty" = "poverty",
                    'COVID-19 Cases as of June 10th' = "cases"
                  ), selected = 1)
              )),
            # Have a row for a plot of the daily cases
            fluidRow(
              # Have a box for the plot
              plotlyOutput("maps")),
            p("Percent in Poor or Fair Health is the percentage of people in poor or fair health."),
            p("High School Graduation Rate is the percentage of people who graduated high school."),
            p("Percent of people in poverty is the percentage of people who fall below the poverty line."),
            p("COVID-19 cases is the aggregate number of cases until June 10th.")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "DataFest 2020"),
  sidebar,
  body
)

server <- function(input, output) {
    output$unemployment <- renderPlotly(
      ggplotly(ggplot(data_work_class, aes(x=Date, y=Value, group=1)) +
        geom_line() +
        ggtitle(c('Number of employed persons in all industries')) +
        ylab('# Employed persons (in thousands)') +
        xlab(NULL) +
        scale_y_continuous(limits = c(125000, 160000),
                           breaks=seq(125000, 160000, 5000)) +
        scale_x_discrete(limits=data_work_class$Date) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(angle = 0, hjust = 1),
              plot.title = element_text(hjust = 0.5)) +
        labs(caption = "Source: The US Bureau of Labor Statistics", subtitle = "April 2019 - April 2020")) 
    )
    output$byindustry <- renderPlotly(
      ggplotly(unemp_total_long %>% filter(.data[["section"]] %in% input$morf) %>% 
          ggplot(aes(x=industry, y=rate, fill=section)) +
          geom_bar(stat = 'identity', position="dodge") + 
          xlab(NULL) +
          ggtitle(c('Unemployment rate increase in major industries: April 2019 vs. April 2020')) +
          ylab('Unemployment rate increase') +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(hjust = 0.5)) +
          # geom_text(aes(label=round(rate,1)), vjust=-0.3, size=3.7) +
          scale_x_discrete(limits=names(maj_rate),
                           labels = str_wrap(unemp_total_long$industry, width = 30))+
        labs(caption = "Source: The US Bureau of Labor Statistics"))
    )
      
      output$bystate <- renderPlotly (
        plot_ly(type = "choropleth", locations=intercept_by_state$X, locationmode = "USA-states", 
        z=~intercept_by_state$rate, text = ~paste("State:", intercept_by_state$X,
                                                  "<br> Rate:", intercept_by_state$rate, "%",
                                                  "<br> Date:",intercept_by_state$Stay.at.home.order ), hoverinfo = "text") %>% layout(geo=list(scope='usa'))%>% colorbar(title = "Average JLR")
      )
      output$bystateintercept <- renderPlotly (
        plot_ly(type = "choropleth", locations=intercept_by_state$X, locationmode = "USA-states", 
                z=~intercept_by_state$rate, text = ~paste("State:", intercept_by_state$X,
                                                          "<br> Rate:", intercept_by_state$rate, "%"
                                                          ), hoverinfo = "text") %>% layout(geo=list(scope='usa'))%>% colorbar(title = "Average JLR")
      )
      output$correlations <- renderPlotly(
        # final_data %>% ggplot(aes(y=logit_JLR, x= final_data[, input$covariate]))+
        #   geom_point()+
        #   labs(x=input$covariate, y=' Logit Job Loss Rate')
        plot_ly(data=final_data, x=~final_data[, input$covariate], y=~logit_JLR) %>% layout(yaxis =list(title = "Logit LI JLR"), xaxis= list(title = input$covariate))
      )
      output$maps <- renderPlotly (
        plot_ly(type = "choropleth", locations=covariates_bystate$region, locationmode = "USA-states", 
                z=~covariates_bystate[,input$predictors], name = input$predictors) %>% layout(geo=list(scope='usa')) %>% colorbar(title = input$predictors)

      )
      
      output$statehome <- renderPlotly(
        plot_ly(type = "choropleth", locations=stayhome$X, locationmode = "USA-states",
                                  z=~stayhome$order, showscale=FALSE, text = ~paste("State:", stayhome$X,
                                                                                    "<br> Date :", stayhome$Stay.at.home.order), hoverinfo = "text") %>% layout(geo=list(scope='usa')) %>% layout(annotations = 
                   list(x = 1, y = -0.1, text = "Darker states means never issued a stay at home order. <br> Source: Business Insider", 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='right', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=12, color="black"))
                   )
      
      )
      
      output$data <- renderDataTable(
        final_data
      )
      output$numdays <- renderPlotly(
        # nofdays%>% ggplot(aes(Days.to.Issue.Stay.at.home.from.National.Announcment,intercept))+ geom_jitter()+geom_text(aes(label=STATE), size=3, check_overlap = T,nudge_y=0.005)+geom_smooth(method=lm)  +labs(title ="Low income job loss rate by days to issue state order", x = "Days to issue state order", y = "Estimated Intercept of LI JLR", caption = "Source: Business Insider, Al Jazeera " )
      
        
        plot_ly(data=nofdays, x=~Days.to.Issue.Stay.at.home.from.National.Announcment, y=~intercept,
              type = "scatter",
              mode = "markers",
              hovertext = ~paste("Logit LI JLR: ", round(intercept,2),
                            "<br> Days: ", Days.to.Issue.Stay.at.home.from.National.Announcment,
                            "<br> State: ", STATE),
              hoverinfo = "text",
              text = ~STATE) %>% add_lines(x = ~Days.to.Issue.Stay.at.home.from.National.Announcment, y = fit, mode = "lines")%>%
          layout(showlegend = F) %>% add_text(textposition = "top right") %>% layout(xaxis =list(title = "Days to Issue Stay at Home Order"), yaxis= list(title = "Logit LI JLR"), title = "Low income job loss rate by days to issue state order") %>%
                                              layout(annotations = list(x = 1, y = -0.1, text = "Source: Business Insider, Al Jazeera", 
                                                                showarrow = F, xref='paper', yref='paper', 
                                                                xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                                                                font=list(size=10, color="black")))
        )
      output$dem <- renderPlotly(
        #political %>% ggplot(aes(Lib,intercept))+ geom_jitter()+geom_text(aes(label=STATE), size=3, 
                                                            #check_overlap = T,nudge_y=0.005)  +labs(title ="Low income job loss rate by political majority", x = "% of Democrat Leaning", y = "Estimated Intercept of JLR", caption = "Source: Pew Research Center")
        plot_ly(data=political, x=~Lib, y=~intercept,
                type = "scatter",
                mode = "markers",
                hovertext = ~paste("Logit LI JLR: ", round(intercept,2),
                                   "<br> % Dem: ", Lib,
                                   "<br> State: ", STATE),
                hoverinfo = "text",
                text = ~STATE) %>% layout(showlegend = F) %>% add_text(textposition = "top left") %>% layout(xaxis =list(title = "% of Democrat Leaning"), yaxis= list(title = "Estimated Intercept of LI JLR"), title = "Low income job loss rate by political majority (Democrat)") %>%
          layout(annotations = list(x = 1, y = -0.1, text = "Source: Pew Research Center", 
                                    showarrow = F, xref='paper', yref='paper', 
                                    xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                                    font=list(size=10, color="black")))
      )
      output$rep <- renderPlotly(
        #political%>% ggplot(aes(Rep,intercept))+ geom_jitter()+geom_text(aes(label=STATE), size=3, 
                                                                    #check_overlap = T,nudge_y=0.005)  +labs(title ="Low income job loss rate by political majority", x = "% of Republican Leaning", y = "Estimated Intercept of JLR", caption = "Source: Pew Research Center")
        plot_ly(data=political, x=~Rep, y=~intercept,
                type = "scatter",
                mode = "markers",
                hovertext = ~paste("Logit LI JLR: ", round(intercept,2),
                                   "<br> % Rep: ", Lib,
                                   "<br> State: ", STATE),
                hoverinfo = "text",
                text = ~STATE) %>% layout(showlegend = F) %>% add_text(textposition = "top right") %>% layout(xaxis =list(title = "% of Republican Leaning"), yaxis= list(title = "Estimated Intercept of LI JLR"), title = "Low income job loss rate by political majority (Republican)") %>%
          layout(annotations = list(x = 1, y = -0.1, text = "Source: Pew Research Center", 
                                    showarrow = F, xref='paper', yref='paper', 
                                    xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                                    font=list(size=10, color="black")))
        )
      output$coeffs <- renderPlotly(
        
        plot_ly(data=model_summary, y=~reorder(vars,coeff), x=~coeff, type="scatter", mode="markers",
                error_x = ~list(array=se, color='#000000')) %>% layout(xaxis =list(title = "Coefficient"), yaxis= list(title = "")) 
      )
}


shiny::shinyApp(ui, server)
