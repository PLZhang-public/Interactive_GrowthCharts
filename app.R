### Patrick Zhang
### 12/16/22
### Growth Charts RShiny

## ------------------------ Background --------------------------- ##
## Website: https://www.cdc.gov/healthyweight/bmi/calculator.html

## Task: update BMI and the corresponding BMI-for-age percentile based on CDC growth charts for children and teens ages 2 through 19 years

##  formula for BMI is weight in kilograms divided by height in meters squared
# BMI = kg/m**2

## ------------------------ Issues --------------------------- ##

## 1. Interpolate LMS values? currently the csv file uses the middle of the month (except for 24 months)

## ------------------------ Optimization --------------------------- ## 
## Months to years - Prioritize medium/high 
## Feet inch fraction - Prioritize medium/high 
## Put in years calculator - Prioritize medium/high 
## Plotly - try to make this look prettier
## Try to build in x
## Try to find shading 
## Figure out way to label the lines
## Talk to david about the BIVs
## Talk to Brooke and Renee on how to make the plot better 
## Look into how consumers view this. So making it look like the growth charts

### What do we want to show clinicians
#  We want to show them what they are used to first
#  We want to give them the cool new beatufiul options with shading

## Growtchcharts serve as a ruler to compare children today to the referent population (1960s)

## ------------------------ RShiny Workflow --------------------------- ##
# 1. Define preliminary functions
#        Standardize to metric
#        Calculate BMI (untransformed)
# 2. User inputs variables: 
#        metric_in, agemos_raw,sex_in, 
#        cm_in, kg_in, 
#        inch_in, lb_in
# 3. Define conditional BMI function
#        Calculate bmip 
#        IF bmip < 95th percentile, end here
#        IF bmip => 95th percentile, recalculate
# 4. You can use plotly to chart point on growthchart

## ------------------------ Test Values --------------------------- ##
# Age in months: 85
# Sex: Boy
# Measurement System: Metric
# Height (cm) : 120
# Weight (kg) : 30

##################################################################
## import libraries
library(devtools)
library(data.table)
library(shinythemes)
library(tidyverse)
library(shiny)
library(reactable)
library(plotly)
library(rio)

## Keep all decimal points
options(digits=10)

##################################################################
## Import excel file from DNPAO website
# Ref webpage: https://www.cdc.gov/growthcharts/percentile_data_files.htm
# Ref csv: https://www.cdc.gov/growthcharts/data/zscore/bmiagerev.xls
# This is BMI-for-age charts, 2 to 20 years, LMS parameters and selected smoothed BMI (kilograms/meters squared) percentiles, by sex and age
# sex: 1 = Male, 2 = Female

data_url <- "https://www.cdc.gov/growthcharts/data/zscore/bmiagerev.xls"
BMI_ref_tbl <- import(file = data_url,which = 1) |> 
  ## Excel file has a mistake, Sex is listed as a header and value.  Remove.
  filter(Sex != "Sex") |> 
  ## Mutate all character vars to numeric
  mutate_if(is.character,as.numeric)
  

##################################################################
## ---------------- Start: Plotly Section --------------------
##################################################################

# ######## PZ Optimization ######
##### Create tbls for each sex do loop
BMI_ref_tbl_sexnames <- list("BMI_ref_tbl_male", "BMI_ref_tbl_female")

for (i in 1:2) {
    ## Filter to sex
    BMI_ref_tbl_sex <- BMI_ref_tbl |>
        filter(Sex == i) |> 
        mutate(agey=Agemos/12)
    
    ## Create sex specific dataset
    assign(BMI_ref_tbl_sexnames[[i]], BMI_ref_tbl_sex)
}
# ######## PZ Optimization ######

# ######### Create p5-p95 plot for just male ##############
# ## Define points for graph
fig_male  <- plot_ly(BMI_ref_tbl_male, x = ~agey, y = ~P5, name = '5',
               type = 'scatter', mode = 'lines', legendgroup = 'group1')

# fig_male  <- fig_male  %>% add_trace(y = ~P5, name = '5', mode = 'lines')
fig_male <- fig_male %>% add_trace(y = ~P10, name = '10', mode = 'lines', legendgroup = 'group1')
fig_male <- fig_male %>% add_trace(y = ~P25, name = '25', mode = 'lines', legendgroup = 'group1')
fig_male <- fig_male %>% add_trace(y = ~P50, name = '50', mode = 'lines', legendgroup = 'group1')
fig_male <- fig_male %>% add_trace(y = ~P75, name = '75', mode = 'lines', legendgroup = 'group1')
fig_male <- fig_male %>% add_trace(y = ~P85, name = '85', mode = 'lines', legendgroup = 'group1')
fig_male <- fig_male %>% add_trace(y = ~P90, name = '90', mode = 'lines', legendgroup = 'group1')
fig_male <- fig_male %>% add_trace(y = ~P95, name = '95', mode = 'lines', legendgroup = 'group1')

######## PZ - deleted this section on 1/3/22.  labeling doesn't work when zoomed in
# ## Add label for lines on the side as annotations
# line_label_male_le95 <- BMI_ref_tbl_male[219, c("P5", "P10", "P25", "P50", "P75", "P85", "P90", "P95")]
# 
# fig_male <- fig_male %>% add_annotations(
#     xref = 'paper',
#     x = 1,
#     y = line_label_male_le95,
#     text = colnames(line_label_male_le95),
#     xanchor = 'left',
#     yanchor = 'middle',
#     font = list(family = 'Arial',
#                 size = 12,
#                 color = 'rgba(67,67,67,1)'),
#     showarrow = FALSE)
######## PZ - deleted this section on 1/3/22.  labeling doesn't work when zoomed in

# ## QC
# fig_male
# # Good!

### Plot Layout
fig_male <- fig_male %>% layout(title = 'Boys: Ages 2-20 years | Body mass index-for-age percentiles',
                      xaxis = list(title = 'Age in Years', tick0=0, dtick=1), 
                      font=t, plot_bgcolor = "#e5ecf6",
                      yaxis = list(title = 'BMI (kg/m^2)'),
                      legend = list(title=list(text='Percentiles'), x = 1.05, y = .5, traceorder = "reversed+grouped"))

## Plot Size
# fig_male <- fig_male %>% layout(height = 1000, width = 600)

# ### QC view figure
# fig_male
# # Good

######### Create p5-p95 plot for just female ##############
# ## Define points for graph
fig_female  <- plot_ly(BMI_ref_tbl_female, x = ~agey, y = ~P5, name = '5',
                     type = 'scatter', mode = 'lines', legendgroup = 'group1')

# fig_female  <- fig_female  %>% add_trace(y = ~P5, name = '5', mode = 'lines')
fig_female <- fig_female %>% add_trace(y = ~P10, name = '10', mode = 'lines', legendgroup = 'group1')
fig_female <- fig_female %>% add_trace(y = ~P25, name = '25', mode = 'lines', legendgroup = 'group1')
fig_female <- fig_female %>% add_trace(y = ~P50, name = '50', mode = 'lines', legendgroup = 'group1')
fig_female <- fig_female %>% add_trace(y = ~P75, name = '75', mode = 'lines', legendgroup = 'group1')
fig_female <- fig_female %>% add_trace(y = ~P85, name = '85', mode = 'lines', legendgroup = 'group1')
fig_female <- fig_female %>% add_trace(y = ~P90, name = '90', mode = 'lines', legendgroup = 'group1')
fig_female <- fig_female %>% add_trace(y = ~P95, name = '95', mode = 'lines', legendgroup = 'group1')

### Plot Layout
fig_female <- fig_female %>% layout(title = 'Girls: Ages 2-20 years | Body mass index-for-age percentiles',
                                xaxis = list(title = 'Age in Years', tick0=0, dtick=1),
                                font=t, plot_bgcolor = "#e5ecf6",
                                yaxis = list(title = 'BMI (kg/m^2)'),
                                legend = list(title=list(text='Percentiles'), x = 1.05, y = .5,  traceorder = "reversed+grouped"))

## Plot Size
# fig_female <- fig_female %>% layout(height = 1000, width = 600)


##################################################################
## ---------------- End: Plotly Section --------------------
#################################################################

##################################################################
## ---------------- Start: Define Functions --------------------
##################################################################

### Calc untransformed BMI
BMI_f <- function(cm, kg){ ##BMI is weight in kilograms divided by height in meters squared
    m <- cm/100
    bmi_out <- kg/m**2
    
    return(bmi_out)
}

### Make a function for BMI calculation table

BMI_calc_f <- function(sex_in, cm_in, kg_in, agemos_in, bmi_in){
    ## Caluclate BMIz and percentile within this table
    dt <- BMI_ref_tbl %>%
        mutate(sex_userin = sex_in, cm = cm_in, kg = kg_in, agemos_userin = agemos_in, bmi = bmi_in) %>%
        select(Sex, sex_userin, Agemos, agemos_userin, cm, kg, bmi, L, M, S, P95) %>%
        filter(Sex == sex_userin, Agemos == agemos_userin) %>%
        
        ## Calc BMIz and BMIz_percentile
        mutate(bmiz = (((bmi/M) ^ L) -1) / (L*S), bmip = 100*pnorm(bmiz))
    
    setDT(dt)
    
    ## CONDITIONAL LOGIC: If over 95th percentile, calculate bmip_ext
    dt[, wtcat_text:= if(bmip<5){"underweight"} 
       else if(bmip<85){"healthy weight"} 
       else if(bmip<95){"overweight"}
       else {"obesity"}]
    
    ## CONDITIONAL LOGIC: If over 95th percentile, calculate bmip_ext
    dt[, sex_text:=ifelse(Sex==1, "boy", "girl")]
    
    ## CONDITIONAL LOGIC: If over 95th percentile, calculate bmip_ext
    dt[,':=' (agey=Agemos/12)]    
    dt[, sigma:=ifelse(Sex==1, 0.3728 + 0.5196*agey - 0.0091*agey^2, 
                       0.8334 + 0.3712*agey - 0.0011*agey^2)]
    dt[bmip>=95, bmip_ext:=90 + 10*pnorm((bmi - P95) / round(sigma,8))]
    
    return(dt)
}

##################################################################
## ---------------- End: Define Functions --------------------
##################################################################

##################################################################
## ---------------- Start: Shiny App --------------------
##################################################################
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # Application title    
                titlePanel(div(h1(strong("BMI Percentile Calculator for Children and Teens")),
                               h4(p("This calculator provides body mass index (BMI) and the corresponding BMI-for-age percentile based on CDC growth charts for children and teens ages 2 through 19 years. Because of possible rounding errors in age, weight, and height, the results from this calculator may differ slightly from BMI-for-age percentiles calculated by other programs.", br(), br(),
                                    "For people 20 years old and older, use the", 
                                    em(span("Adult BMI Calculator.", style="color:blue")), br(), br(),
                                    # "Measuring Height and Weight At Home. **Add Link**")),
                                    em(span("Measuring Height and Weight At Home.", style="color:blue")))),                                    
                               hr())), 
                
                
                # Sidebar panel
                sidebarLayout(
                    
                    sidebarPanel(
                        p(strong("BMI Calculator for Child and Teen")),
                        
                        numericInput("agemos_raw",
                                     label = "Age in Months",
                                     value = 85),
                        radioButtons("sex_in",label="Sex: ",
                                     choices = c("Boy"= 1, "Girl" = 2),
                                     inline=T),
                        
                        radioButtons("meas_in",label="Measurement System",
                                     choices = c("Metric", "English"),
                                     inline=T),
                        
                        ## Only show this panel if meas_in is Metric
                        conditionalPanel(
                            condition = "input.meas_in == 'Metric'",
                            numericInput("cm_in",
                                         label = "Height (cm)",
                                         value = 0), #120 for test
                            numericInput("kg_in",
                                         label = "Weight (kg)",
                                         value = 0), #30 for test
                        ), # End metric conditional panel
                        
                        ## Only show this panel if meas_in is English
                        conditionalPanel(
                            condition = "input.meas_in == 'English'",
                            ## English measurements
                            numericInput("inch_in",
                                         label = "Height (inch)",
                                         value = 0), #47 for test
                            numericInput("lb_in",
                                         label = "Weight (lb)",
                                         value = 0), #66 for test
                        ), # End English conditional panel
                        
                        actionButton("do", "Calculate BMI")
                    ),
                    
                    # Main panel - show generated output
                    mainPanel(
                        
                        div(p(h4(strong("Information Entered"))),
                            htmlOutput("textOut_userinput")
                        ),
                        
                        div(p(h4(strong("Results"))),
                            htmlOutput("textOut_results")
                        ), 
                        
                        div(p(h4(strong("BMI-for-age Percentile Growth Chart"))),
                            plotlyOutput('plotOut_fig')
                        ),
                        
                        div(p(h4(strong("What does this mean?"))),
                            htmlOutput("textOut_explain")
                        ), 
                        
                        div(p(h4(strong("Additional Information"))),
                            htmlOutput("textOut_notes")
                        ),
                        
                        p(strong("----- QC: BMI Data Table -----")),
                        dataTableOutput("dtable1"),
                        tableOutput("table1"),
                        
                        p(strong("----- QC: BMI Output -----")),        
                        textOutput("textOut2")
                        
                    ),
                    position = c("left", "right"),
                    fluid = TRUE
                    
                ) ## End sidebarlayout
) ## End fluidpage ui function

server <- function(input, output) {
    
##################################
### Define outputs
    
    ## If radio button is metric, clear english values
    observeEvent(input$meas_in,{
        if(input$meas_in== "Metric"){
            updateTextInput(inputId = "cm_in", value=0)
            updateTextInput(inputId = "kg_in", value=0)
        }
        if(input$meas_in== "English"){
            updateTextInput(inputId = "inch_in", value=0)
            updateTextInput(inputId = "lb_in", value=0)
        }
        
    })
    
    
    observeEvent(input$do, {
    
        ### If statement for metric/english conversion
        ## BMI will be calculated using metric.  use variables cm_2 and kg_2 as converted variables
       if(input$meas_in == "Metric"){
           cm_2 = input$cm_in
           kg_2 = input$kg_in
           # inch_in = NA
           # lb_in = NA
       } else {
           cm_2 = input$inch_in*2.54
           kg_2 = input$lb_in*0.45359237
           # cm_in = NA
           # kg_in = NA
       }  ## End if statement for metric/english conversion
        
        output$textOut_userinput <- renderUI({
            HTML(paste0(strong("Age"), " (at measurement): ", input$agemos_raw, br(), 
                        strong("Sex: "), input$sex_in, br(),
                        strong("Measure: "), input$meas_in, br(),

                        strong("Input cm: "), input$cm_in, br(),
                        strong("Input kg: "), input$kg_in, br(),
                        strong("Input inch: "), input$inch_in, br(), 
                        strong("Input lb: "), input$lb_in, br(),
                        br(),
                        strong("Converted cm: "), cm_2, br(),
                        strong("Converted kg: "), kg_2, br(), hr()
        ))}) ## End textOut_userinput

# ############# Render tables
        ### Define table1 - all variables for child
            output$table1 <- renderTable(BMI_calc_tbl)
            
            BMI_calc_tbl <- (BMI_calc_f(sex_in = input$sex_in,
                                        cm_in = cm_2,
                                        kg_in = kg_2,
                                        agemos_in = input$agemos_raw + .5,
                                        bmi_in = BMI_f(cm = cm_2, kg = kg_2)))
    
            ### Define bmip_out as true BMI output, dependent on child above or below 95th percentile
            BMI_calc_tbl <- BMI_calc_tbl |> 
                mutate(bmip_out = if(BMI_calc_tbl[1,bmip]>=95){
                    BMI_calc_tbl[1,bmip_ext]
                } else {
                    BMI_calc_tbl[1,bmip]
                })
        
        ### Define textOut2 - readable text for childs bmi status
        output$textOut2 <- renderText(if(BMI_calc_tbl[1,bmip]>=95){
            paste0("BMIP >= 95: ", BMI_calc_tbl[1,bmip_out])
        } else {
            paste0("BMIP < 95: ",BMI_calc_tbl[1,bmip_out])
            
        }) ## End textOut2
    # })  ## END: Action button to calculate bmi
        
        output$textOut_results <- renderUI({
            HTML(paste0("Based on the height and weight entered, the ", 
                    strong(paste0("BMI is ", round(BMI_calc_tbl[1,bmi], digits=2))),
                    ", placing the BMI-for-age ",
                    strong(paste0("at the ", round(BMI_calc_tbl[1,bmip_out], digits=0), " percentile")),
                    " for ", input$agemos_raw, "-month old ", BMI_calc_tbl[1,sex_text], "s.",
                    "  This child falls in the ", strong(BMI_calc_tbl[1,wtcat_text]), " BMI category.", br(), hr()
        ))}) ## End textOut_results
        
        output$textOut_explain <- renderUI({
            HTML(paste0(strong("BMI"), " is calculated using your child's weight and height and is then used to find the corresponding BMI-for-age percentile for your child's age and sex.", br(), br(),
                        strong("BMI-for-age percentile"), " hows how your child's weight compares to that of other children of the same age and sex. For example, a BMI-for-age percentile of 65 means that the child's weight is greater than that of 65% of other children of the same age and sex.", br(), hr()
        ))}) ## End textOut_explain
        
        output$textOut_notes <- renderUI({
            HTML(paste0("BMI-for-age percentile is an estimate of how a child's weight compares to that of other children of the same age and sex. For more information, visit About Child and Teen BMI. **Add Link**", br(), br(),
                        "This BMI calculator is not intended to be a substitute for professional medical advice.", br(), br(),
                        "Maintaining a BMI in the healthy weight category is one way to support overall health and well-being as you age. For more information on the importance of lifestyle approaches to healthy weight, visit Healthy Weight. **Add Link**",  br(), hr()
                        
            ))}) ## End textOut_results
        
    # ### Start PLOT for percentiles lines over 95   
        
        ## If male, create plot 
        if(input$sex_in == 1){   
            fig <- fig_male

            ## Add extended lines if high bmi
            if(BMI_calc_tbl[1,bmip] > 90){
                fig <- fig %>% add_trace(y = ~P98, name = '98', mode = 'lines', legendgroup = 'group1')
                fig <- fig %>% add_trace(y = ~P99, name = '99', mode = 'lines', legendgroup = 'group1')
                fig <- fig %>% add_trace(y = ~P99_9, name = '99.9', mode = 'lines', legendgroup = 'group1')
                fig <- fig %>% add_trace(y = ~P99_99, name = '99.99', mode = 'lines', legendgroup = 'group1')
            }
            
            ## Add point for user input
            fig <- fig %>% add_trace(x = (input$agemos_raw + .5)/12, y = BMI_f(cm = cm_2, kg = kg_2),
                                          name = 'Input Obs', mode = 'markers',
                                          size = 1.5, legendgroup = 'group5')
            
            ## Render plot
            output$plotOut_fig <- renderPlotly(fig)
            
        } else {
        ## If female, create plot 
            fig <- fig_female
            
            ## Add extended lines if high bmi
            if(BMI_calc_tbl[1,bmip] > 90){
                fig <- fig %>% add_trace(y = ~P98, name = '98', mode = 'lines', legendgroup = 'group1')
                fig <- fig %>% add_trace(y = ~P99, name = '99', mode = 'lines', legendgroup = 'group1')
                fig <- fig %>% add_trace(y = ~P99_9, name = '99.9', mode = 'lines', legendgroup = 'group1')
                fig <- fig %>% add_trace(y = ~P99_99, name = '99.99', mode = 'lines', legendgroup = 'group1')
            }
            
            ## Add point for user input
            fig <- fig %>% add_trace(x = (input$agemos_raw + .5)/12, y = BMI_f(cm = cm_2, kg = kg_2),
                                     name = 'Input Obs', mode = 'markers',
                                     size = 1.5, legendgroup = 'group5')
            
            ## Render plot
            output$plotOut_fig <- renderPlotly(fig)
            
        } ## end if male/female if statement
    }) ## End Action button
    
} ## END: Server function
    
####################################################################
# Run the application 
shinyApp(ui = ui, server = server)

##################################################################
## ---------------- End: Shiny App --------------------
##################################################################