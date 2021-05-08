#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(janitor)
library(ggplot2)
# library(gcookbook)
library(tidycensus)
library(png)
library(ggdist)
library(gt)
library(broom.mixed)
library(gtsummary)

covid_data <- readRDS("cleandata/covid-data.rds")
graph_baltimore <- readRDS("cleandata/baltimore-data.rds")
graph_cincinnati <- readRDS("cleandata/cincinnati-data.rds")
graph_los_angeles <- readRDS("cleandata/los-angeles-data.rds")
graph_orlando <- readRDS("cleandata/orlando-data.rds")
graph_seattle <- readRDS("cleandata/seattle-data.rds")
fit_1 <- readRDS("cleandata/baltimore-regression.rds")
fit_2 <- readRDS("cleandata/cincinnati-regression.rds")
fit_3 <- readRDS("cleandata/losangeles-regression.rds")
fit_4 <- readRDS("cleandata/orlando-regression.rds")
fit_5 <- readRDS("cleandata/seattle-regression.rds")
baltimore_posterior <- readRDS("cleandata/baltimore-posterior.rds")
cincinnati_posterior <- readRDS("cleandata/cincinnati-posterior.rds")
losangeles_posterior <- readRDS("cleandata/losangeles-posterior.rds")
orlando_posterior <- readRDS("cleandata/orlando-posterior.rds")
seattle_posterior <- readRDS("cleandata/seattle-posterior.rds")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "The Effects of COVID-19 on Domestic Disputes in US Cities",
    tabPanel("Posteriors",
             titlePanel("Regression Visualization"),
             fluidPage(
                 h4("Posterior Distribution for the Effect of COVID-19 and Race on Domestic Disputes"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             # The first arg. here sets the local variable
                             # that you use to toggle selections from the UI
                             # within the server
                             
                             "city",
                             
                             # The second position labels the selection pane
                             # / area
                             
                             "Select a City",
                             
                             # These equations link the displayed options in
                             # the UI as the user will see them to the value
                             # taken on by the local variable. User selects
                             # "Baltimore", local var becomes =
                             # "baltimore"
                             
                             choices = c("Baltimore" = "baltimore", 
                                         "Cincinnati" = "cincinnati",
                                         "Los Angeles" = "losangeles",
                                         "Orlando" = "orlando",
                                         "Seattle" = "seattle")
                         ),
                         width = 300),
                     plotOutput("posterior_plot",
                                width = 550,
                                height = 500))),
             br(),
             h3("Discussion of Posteriors"),
             p("When interpreting these graphs, it is important to know that they 
               are graphs of the posterior probability distribution of domestic disputes in each city. 
               A posterior probability, in Bayesian statistics, is the revised or updated probability of 
               an event occurring after taking into consideration new information. The posterior probability 
               is calculated by updating the prior probability using Bayes' theorem. In our case, 
               the posterior probability is the probability of event A (domestic disputes) occurring given 
               that event B (x number of COVID-19 cases) has occurred. Therefore, when interpreting the 
               plot of the posterior probability distribution of domestic disputes in Baltimore, we see that 
               in any month during the COVID-19 pandemic, Baltimore would experience about 1770 domestic 
               disputes on average. However, the true value could lie anywhere between 1730 and 1830 domestic 
               despute cases per month. The same logic applies when interpreting the plots for other cities.")),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"), 
             p("In this project I will look to conduct analysis on the effects of 
                COVID-19 on domestic disputes in US Cities. As COVID-19 cases surged 
                across the US, stay-at-home orders were put in place. Schools closed, 
                and many workers were furloughed, laid off, or told to work from home. 
                With personal movement limited and people confined to their homes, many 
                advocates became increasingly concerned about potential significant increases 
                in all forms of domestic disputes. Stay-at-home orders, intended to protect the public 
                and prevent widespread infection, left many victims of domestic violence 
                trapped with their abusers. Reflecting on my data as well as stories that 
                I heard in the research process, opened my eyes to how inequities related to 
                health are magnified during a crisis, and sheltering in place does not inflict equivalent 
                hardship on all people. With the data presented in this project, the message I 
                hope you take away is that there is still so much work to be done to ensure that people 
                who experience abuse can continue to obtain access to support, refuge, and medical care 
                when another public health disaster hits. Additionally, as local city and state mandates begin
                to ease and people begin to return to their normal routines, public health officials
                and policy makers must continue addressing the social inequities within our 
                communities as well as making healthcare more accessible to all people. 
                With this this in mind, I attempt to answer the following question: How has COVID-19 affected domestic dispute 
               cases in US Cities?"),
             h3("Data"),
             p("To accomplish this, I collect data on police calls for service 
                from five large metropolitan cities or areas: Baltimore, Maryland; Cincinnati, Ohio; 
                Los Angeles, California; Orlando, Florida; and Seattle, Washington. All of these 
                cities participate in the Police Data Initiative. Of the 32 
                police agencies participating, these cities had up-to-date
                incidence data and provided adequate documentation to identify calls about 
                domestic dispute incidents. I also use data from USA Facts which contains 
                data regarding US Coronavirus Cases & Deaths by State. This database has been 
                tracking COVID-19 data daily by state and county. I have data up to 3/11/21. 
                Finally, I leverage data from the 2018 American Community Survey (ACS). 
                The ACS helps local officials, community leaders, 
                and businesses understand the changes taking place in their communities by providing  
                detailed population and housing information about the United States."),
             h3("About Me"),
             p("My name is Alex Tsotadze and I am a junior in Adams House study Economics.
                 You can reach me at atsotadze@college.harvard.edu."),
             p("Link to my github repo: https://github.com/atsotadze/final-project-2021"),
             h3("Resources for those in need"),
             p("Crisis Text Line (text HOME to 741741)"),
             p("National Parent Hotline (call 1-855-427-2736)"),
             p("Childhelp National Child Abuse Hotline (visit https://www.childhelp.org/childhelp-hotline/. 
               opens in new tab or call 1-800-422-4453)"),
             p("National Domestic Violence Hotline (visit http://thehotline.org. opens 
               in new tab, text LOVEIS to 22522, or call 1-800-799-7233)"),
             p("Futures Without Violence (visit https://www.futureswithoutviolence.org/resources-events/get-help/. 
               opens in new tab)")),
    tabPanel("Discussion", 
             titlePanel("Methodology"),
             h3("Data cleaning process"),
             p("The most significant challenge in this process was cleaning and mergind the data sets 
               I found for this project. With millions of datapoints as well as non-uniform police 
               classification systems, I had to find a way to standardize my process in order to yield 
               valuable results. Prior to my manipulations, each state has a unique way of classifying 
               domestic dispute calls. Some cities like Baltimore and Cincinnati had one 'umprella' term 
               which captured all domestic disturbance calls. However, Los Angeles, Orlando and Seattle had 
               much more granularity in their descriptions. Therefore, in order to establish a 
               uniform police classification system, I chose to default to the classifcation systems of Baltimore
               and Cincinnati (this was both because it was the most effective way to solve this issue as well as it 
               was impossible for me to know what each indicent entailed in Baltimore and Cincinnati and therefore 
               changed my project's focus from domestic violence specifically to domestic disputes in general). Furthermore,
               each city also had a different formates for logging times and dates of incidents. Although at the surface 
               this seems un-important, standardizing this datapoint as well became a priority when calculating daily 
               new domestic dispute cases by city."),
             h3("Merging Process"),
             p("Once I was equipped with my cleaned data sets, my next challenge emerged when merging the data sets
               in order to run my linear regression. I first joined city police data with city ACS data, which provided me with
               racial population distributions within cities. With these two data sets merged, I then was able to create 
               my final data set, which contains data on new daily domestic dispute cases by city, racial population distributions 
               in cities, as well as COVID-19 daily cases in cities. I was able to successfuly complete this merge by using 
               the Federal Information Processing Standard (FIPS) county codes within the ACS data set. The FIPS codes are a five-digit 
               Federal Information Processing Standards code which uniquely identify counties and county equivalents in the United States.")),
    tabPanel("Interactivity",
             fluidPage(
                 titlePanel("Domestic disputes by City"), 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             # The first arg. here sets the local variable
                             # that you use to toggle selections from the UI
                             # within the server
                             
                             "city_name",
                             
                             # The second position labels the selection pane
                             # / area
                             
                             "Choose a City to Plot",
                             
                             # These equations link the displayed options in
                             # the UI as the user will see them to the value
                             # taken on by the local variable. User selects
                             # "Baltimore", local var becomes =
                             # "baltimore"
                             
                             choices = c("Baltimore" = "baltimore", 
                                         "Cincinnati" = "cincinnati",
                                         "Los Angeles" = "losangeles",
                                         "Orlando" = "orlando",
                                         "Seattle" = "seattle")
                         ),
                         width = 300),
                     plotOutput("line_plot",
                                width = 550,
                                height = 500))),
             h3("Domestic disputes seem to have slightly increased throughout the 
                year across most cities"),
             p("Other than Seattle, we see slight increases in domestic dispute cases 
               in Baltimore, Cincinnati, Los Angeles and Orlando. Although there are
               positive trend lines in these cities, we cannot conclude anything 
               statistically significant from these plots. However, they do paint a sobering
               picture of the struggles of many families across the United States. These 
               findings were eye opening to me, as these cities can have anywhere 
               from 60 to 300+ new domestic dispute cases per day.")),
    tabPanel("Model",
             fluidPage(
                 titlePanel("The Model"),
                 br(),
                 p(strong("Regression Model Equation:")),
                 
                 # the withMathJax function is the easiest way
                 # to write equations in a shiny app. I learned
                 # about this from peers on Demo Day.
                 
                 withMathJax('$$ domestic\\_disputes_i = \\beta_0 + 
                                        \\beta_1covid\\_cases_i + 
                                        \\varepsilon_i $$'),
                 br(),
                 br(),
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             # The first arg. here sets the local variable
                             # that you use to toggle selections from the UI
                             # within the server
                             
                             "name_of_city",
                             
                             # The second position labels the selection pane
                             # / area
                             
                             "Choose a City to Study",
                             
                             # These equations link the displayed options in
                             # the UI as the user will see them to the value
                             # taken on by the local variable. User selects
                             # "Baltimore", local var becomes =
                             # "baltimore"
                             
                             choices = c("Baltimore" = "baltimore", 
                                         "Cincinnati" = "cincinnati",
                                         "Los Angeles" = "losangeles",
                                         "Orlando" = "orlando",
                                         "Seattle" = "seattle")
                         ),
                         width = 300),
                     gt_output("regression_table"))),
             
             h3("Model Description"),
             p("In this project, I model the effects of COVID-19 new monthly domestic 
             dispute cases in the US Cities. Specifically,
              focusing on Baltimore, Cincinnati, Los Angeles, Orlando and Seattle."),
             p("I fit this using COVID-19 data from USA Facts, as well as police data 
            from the Police Data Initiative. I also 
             be using a new package, broom.mixed, which allows us to tidy 
              regression data for plotting."),
             h3("Results"),
             p("When interpreting the regression tables for each city, is important to keep
               in mind what the 'Intercept' term represents. In each case, the 'Intercept' 
               term, for example in Baltimore means that if no COVID-19 cases are present in Baltimore,
               there will be 1,764.4 total domestic disputes 
               committed in a month on average. Then, in order to understand the coefficient for COVID-19 Cases,
               you must multiply (in the same case of Baltimore) the 0.00007 by the total monthly cases and 
               add that value to the intercept in order to see how COVID-19 cases affect total monthly 
               domestic dispute totals.
               The same logic can be applied to each other city."),
             br(),
             p("Originally, I had attempted to model the effects of COVID-19 as well as race 
               as a demographic variable on new monthly domestic dispuit cases in the US Cities. However,
               because the race data I collected did not have enough granularity in terms of changes in populations at a 
               monthly level, I was unable to incorporate race into my regression as stan_glm() failed due to 
               the numbers being constants (as opposed to the monthly COVID-19 data which changes from month to month."),
             br(),
             p("However, when interpreting these results, it is important to keep 
               in mind that these results do not imply causality.")),
    tabPanel("Covid Data by County",
             titlePanel("Covid-19 Data"),
             mainPanel(plotOutput("data_plot")),
             h3("Discussion of plot"),
             p("The ongoing COVID-19 pandemic has led to approximately 127 million 
             confirmed th cases and 2.78 million deaths globally as of March 28, 2021. 
             While the virus has affected most regions throughout the United States 
             in some capacity, there are stark differences between and within regions 
             in the transmission and severity of cases. In this plot, we see that Los Angeles 
             county experienced the most COVID-19 cases.")))

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$posterior_plot <- renderPlot({
        if(input$city == "baltimore"){
            baltimore_posterior %>%
                ggplot(aes(x = Effect, y = fct_reorder(`COVID-19 Cases`, Effect))) +
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               bins = 50,
                               fill = "red",
                               alpha = 0.5) +
                labs(title = "Posterior Probability Distribution of Domestic Disputes in Baltimore",
                     subtitle = "In any month during COVID-19 in Baltimore, about 1770 domestic disputes occured",
                     x = "Domestic Disputes",
                     y = "Probability",
                     caption = "Source: USA Today (2020) & Police Data Initiative (2021)") +
                scale_y_continuous(labels = scales::percent_format()) +
                theme_classic()
        }
        else if(input$city == "cincinnati"){
            cincinnati_posterior %>%
                ggplot(aes(x = Effect, y = fct_reorder(`COVID-19 Cases`, Effect))) +
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               bins = 50,
                               fill = "red",
                               alpha = 0.5) +
                labs(title = "Posterior Probability Distribution of Domestic Disputes in Cincinnati",
                     subtitle = "In any month during COVID-19 in Cincinnati, about 3900 domestic disputes occured",
                     x = "Domestic Disputes",
                     y = "Probability",
                     caption = "Source: USA Today (2020) & Police Data Initiative (2021)") +
                scale_y_continuous(labels = scales::percent_format()) +
                theme_classic()
        }
        else if(input$city == "losangeles"){
            losangeles_posterior %>%
                ggplot(aes(x = Effect, y = fct_reorder(`COVID-19 Cases`, Effect))) +
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               bins = 50,
                               fill = "red",
                               alpha = 0.5) +
                labs(title = "Posterior Probability Distribution of Domestic Disputes in Los Angeles",
                     subtitle = "In any month during COVID-19 in Los Angeles, about 5500 domestic disputes occured",
                     x = "Domestic Disputes",
                     y = "Probability",
                     caption = "Source: USA Today (2020) & Police Data Initiative (2021)") +
                scale_y_continuous(labels = scales::percent_format()) +
                theme_classic()
        }
        else if(input$city == "orlando"){
            orlando_posterior %>%
                ggplot(aes(x = Effect, y = fct_reorder(`COVID-19 Cases`, Effect))) +
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               bins = 50, 
                               fill = "red",
                               alpha = 0.5) +
                labs(title = "Posterior Probability Distribution of Domestic Disputes in Orlando",
                     subtitle = "In any month during COVID-19 in Orlando, about 2900 domestic disputes occured",
                     x = "Domestic Disputes",
                     y = "Probability",
                     caption = "Source: USA Today (2020) & Police Data Initiative (2021)") +
                scale_y_continuous(labels = scales::percent_format()) +
                theme_classic()
        }
        else{
            seattle_posterior %>%
                ggplot(aes(x = Effect, y = fct_reorder(`COVID-19 Cases`, Effect))) +
                geom_histogram(aes(y = after_stat(count/sum(count))),
                               bins = 50, 
                               fill = "red",
                               alpha = 0.5) +
                labs(title = "Posterior Probability Distribution of Domestic Disputes in Seattle",
                     subtitle = "In any month during COVID-19 in Seattle, about 5200 domestic disputes occured",
                     x = "Domestic Disputes",
                     y = "Probability",
                     caption = "Source: USA Today (2020) & Police Data Initiative (2021)") +
                scale_y_continuous(labels = scales::percent_format()) +
                theme_classic()
        }
    })
    output$data_plot <- renderPlot(
        covid_data %>%
            ggplot(aes(x = `County Name`,
                       y = total_covid_cases)) +
            geom_point() +
            labs(title = "Total Covid Cases by County between 2020-01-22 and 2021-03-11",
                 subtitle = "The highest amount of cases were found in Los Angeles County",
                 x = " ",
                 y = "Total Covid Cases",
                 caption = "Source: USA Today COVID-19 Data") +
            scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
            coord_flip() +
            theme_classic()
    )
    output$line_plot <- renderPlot({
        if(input$city_name == "baltimore"){
            graph_baltimore %>%
                ggplot(aes(x = date,
                           y = total)) +
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm", 
                            formula = y ~ x) +
                labs(title = "Daily Domestic Disturbances in Baltimore, MD in 2020",
                     subtitle = "Can see a small increase in cases over time",
                     x = " ",
                     y = "Daily Domestic Disturbances",
                     caption = "Source: Police Data Initiative (2021)") +
                theme_bw()}
        else if(input$city_name == "cincinnati"){
            graph_cincinnati %>%
                ggplot(aes(x = date,
                           y = total)) +
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm", 
                            formula = y ~ x) +
                labs(title = "Daily Domestic Disturbances in Cincinnati, OH in 2020",
                     subtitle = "Can see a small increase in cases over time",
                     x = " ",
                     y = "Daily Domestic Disturbances",
                     caption = "Source: Police Data Initiative (2021)") +
                theme_bw()}
        else if(input$city_name == "losangeles"){
            graph_los_angeles %>%
                ggplot(aes(x = date,
                           y = total)) +
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm", 
                            formula = y ~ x) +
                labs(title = "Daily Domestic Disturbances in Los Angeles, CA in 2020",
                     subtitle = "Can see a small increase in cases over time",
                     x = " ",
                     y = "Daily Domestic Disturbances",
                     caption = "Source: Police Data Initiative (2021)") +
                theme_bw()}
        else if(input$city_name == "orlando"){
            graph_orlando %>%
                ggplot(aes(x = date,
                           y = total)) +
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm", 
                            formula = y ~ x) +
                labs(title = "Daily Domestic Disturbances in Orlando, FL in 2020",
                     subtitle = "Only had police data through June, 2020",
                     x = " ",
                     y = "Daily Domestic Disturbances",
                     caption = "Source: Police Data Initiative (2021)") +
                theme_bw()}
        else {
            graph_seattle %>%
                ggplot(aes(x = date,
                           y = total)) +
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm", 
                            formula = y ~ x) +
                labs(title = "Daily Domestic Disturbances in Orlando, FL from January, 2020 - February, 2021",
                     subtitle = "Can see a significant decrease in cases over time",
                     x = " ",
                     y = "Daily Domestic Disturbances",
                     caption = "Source: Police Data Initiative (2021)") +
                theme_bw()}
    })
    
    output$regression_table <- render_gt({
        if(input$name_of_city == "baltimore"){
            fit_1 %>%
                tbl_regression(include = everything(),
                               intercept = TRUE,
                               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
                as_gt() %>%
                tab_header(title = "Likelihood of New Total Montly Domestic Dispute Case in Baltimore",
                           subtitle = "How COVID-19 Cases Predict Likelihood of New Domestic Disputes") %>%
                tab_source_note(md("Source: USA Today and Police Data Initiative"))
        }
        else if(input$name_of_city == "cincinnati") {
            fit_2 %>%
                tbl_regression(include = everything(),
                               intercept = TRUE,
                               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
                as_gt() %>%
                tab_header(title = "Likelihood of New Total Montly Domestic Dispute Case in Cincinnati",
                           subtitle = "How COVID-19 Cases Predict Likelihood of New Domestic Disputes") %>%
                tab_source_note(md("Source: USA Today and Police Data Initiative"))
        }
        else if(input$name_of_city == "losangeles"){
            fit_3 %>%
                tbl_regression(include = everything(),
                               intercept = TRUE,
                               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
                as_gt() %>%
                tab_header(title = "Likelihood of New Total Montly Domestic Dispute Case in Los Angeles",
                           subtitle = "How COVID-19 Cases Predict Likelihood of New Domestic Disputes") %>%
                tab_source_note(md("Source: USA Today and Police Data Initiative"))
        }
        else if(input$name_of_city == "orlando"){
            fit_4 %>%
                tbl_regression(include = everything(),
                               intercept = TRUE,
                               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
                as_gt() %>%
                tab_header(title = "Likelihood of New Total Montly Domestic Dispute Case in Orlando",
                           subtitle = "How COVID-19 Cases Predict Likelihood of New Domestic Disputes") %>%
                tab_source_note(md("Source: USA Today and Police Data Initiative"))
        }
        else {
            fit_5 %>%
                tbl_regression(include = everything(),
                               intercept = TRUE,
                               estimate_fun = function(x) style_sigfig(x, digits = 5)) %>%
                as_gt() %>%
                tab_header(title = "Likelihood of New Total Montly Domestic Dispute Case in Seattle",
                           subtitle = "How COVID-19 Cases Predict Likelihood of New Domestic Disputes") %>%
                tab_source_note(md("Source: USA Today and Police Data Initiative"))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)



