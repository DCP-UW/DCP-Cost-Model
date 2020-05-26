#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(navbarPage(
  
  theme = shinytheme("sandstone"),
  "DCP Cost Model",
  # Top page (UI)------------
  tabPanel(p("Introduction", class="tab"),
          
           #Tags for body text and heading sizes, custom color for hyperlinks, defining class "center" for image alignment  
           tags$head(
             tags$style(HTML("
                body {
                  font-size: 14px;
                }
                
                .tab{
                font-size: 15px;
                }
                
                h6{
                  font-size: 14px;
                }
                
                h5 {
                  font-size: 16px;
                }
                
                h4 {
                  font-size: 24px;
                }
                
                .center {
                  display: block;
                  margin-left: auto;
                  margin-right: auto;
                }
                
                a {
                color: #1487B1
                }
                
                }

              "))
           ),
           
           fluidRow(
             column(12, 
                    h2("Disease Control Priorities Cost Model", align = "center"),
                    br(),

fluidRow(
  column(1,""),
  column(10,
         h6(em("*Instructions for using this app are provided at the bottom of this page.")),   
         h4(strong("Introduction to Essential Universal Health Coverage")),
            
            h5("Disease Control Priorities, Third Edition (DCP3) was a five-year collaborative effort among 
             academics and practitioners working in global health to identify priorities for improving health 
             in low-income countries (LICs) and middle-income countries (MICs). The DCP3 network comprised over 
             500 authors, 230 peer reviewers, and 33 editors who together produced 172 chapters across nine 
             volumes that were peer-reviewed through a process overseen by the US National Academies of Sciences,
             Engineering, and Medicine. The nine volumes were published over 2015-2018 by the World Bank, with 
            each book volume and chapter available both in print and online (open access) at", 
            
            a(href="http://dcp-3.org", "www.dcp-3.org."), 
            
            br(),
            br(),
            
           "Chapter 3 of volume 9 of DCP3,", 
           a(href="http://www.dcp-3.org/chapter/2551/essential-universal-health-coverage", "Universal Health Coverage and Essential Packages of Care,"), 
           "synthesized lists of interventions recommended across DCP3's nine volumes into a model health benefits package of 218 health sector interventions called 
            'essential universal health coverage' (EUHC). (The term EUHC was introduced as a distinction from the broader notion of 
            universal health coverage, which does not necessarily imply an explicit health benefit package.) A complementary list of 
            77 intersectoral policies (such as road safety and tobacco control measures) is provided elsewhere in DCP3.", 
           a(href="http://dcp-3.org/chapter/2550/essential-intersectoral-policies-health", "Link to DCP3 Intersectoral Policies."),          
           
           br(),
           br(),
            
            "EUHC was developed from 21 essential intervention packages that spanned a wide range of health topics and professional 
            communities (e.g., surgery, cancer, HIV and STIs). The interventions in these 21 packages were included because they were likely 
            to provide good value for money, be feasible to implement in low-resource settings, and address a considerable disease burden in 
            LICs and MICs. However, because the 218 EUHC interventions would place considerable demands on some health systems, particularly 
            in LICs, the DCP3 authors developed a second model list of interventions termed the 'Highest Priority Package' (HPP), which 
            comprised 115 of the 218 EUHC interventions. Interventions were included in the HPP subset of EUHC if they provided very good 
            value for money, addressed the health needs of the 'worst off' (defined as those with", a(href="https://www.medrxiv.org/content/10.1101/19003814v1", "lowest lifetime health,"), 
            "or were likely to offer substantial financial risk protection. The figure shows the process of developing the EUHC and HPP lists from 
            the 21 essential intervention packages."),
            br(),
            br(), 
            
            img(src="figure.PNG", width="1000", class="center"),
            
            br(),
            
            
            h4(strong("Definitions")), 
            h5("As discussed in the accompanying", a(href="https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30121-2/fulltext", "manuscript,"),
            "one of the major contributions of the DCP Cost Model to the growing literature on UHC 
            costs is its focus on costs at different levels of aggregation. Specifically, this study 
            looked at three relevant dimensions of costs:"),
                                                 
            h5(strong("Delivery platform,"), "which identifies the principal physical/organizational through which interventions 
                          are delivered (with logistically related interventions being delivered on the same platform):"),
         
            column(12,  
            h5("- Population-based health interventions: non-personal services typically organised by public health departments 
            (such as media campaigns and vector control)", 
             br(),
            "- Community: bringing health services close to where people live and work, mostly through sub-platforms like schools or 
             community health workers", 
            br(),
            "- Health center: clinics that deliver outpatient services (either comprehensive primary care centers or extension facilities 
                       like health posts)", 
            br(),
            "- First-level hospital: a health facility that is able to provide inpatient care, including surgery; some specialty outpatient 
             services (such as internal medicine and general surgery) are also included in this platform", 
            br(),
            "- Referral and specialised hospitals: second- and third-level facilities that provide referral inpatient and highly specialised 
             outpatient care (such as ophthalmology and urogynaecology services)")),
                      
                        
             h5(strong("Delivery timing characteristics,"), "which distinguishes interventions according to the urgency and frequency of 
             provider-client interactions:"),
         
             column(12, 
                h5("- Urgent interventions: must be delivered quickly and close to where individuals live",
                br(),
                      "- Chronic interventions: do not have such an immediacy but require recurrent and frequent interactions 
                       between individuals with chronic illnesses and their healthcare providers - hence they must also be delivered close 
                       to where individuals live", 
                br(),
                       "- Time-bound (non-urgent) interventions are generally 'elective' (in clinical terms), allowing cases to be 
                       accumulated over space and time to generate efficiencies in delivery")), 
                br(),
                       
                       h5(strong("Health system objective,"), "which characterizes interventions according to the primary outcome that they 
                          seek to improve:"),
                       
          
             column(12,  
                    h5("- Mortality reduction, age < 5 years",
                       br(),
                      "- Mortality reduction, age 5-69 years from communicable, maternal, perinatal, and nutritional conditions", 
                      br(),
                      "- Mortality reduction, age 5-69 years from noncommunicable diseases and injuries", 
                      br(),
                      "- Reduction in disability ", "due to any health condition", 
                      br(),
                       "- Improvement in non-health outcomes, including met need for palliative care, contraception, cosmetic surgery, 
                       or psychosocial support, and improvement in intellectual development during childhood"), 
                    br(),),
         
                      
         h4(strong("Costing approach")),
         h5("The", a(href="https://ars.els-cdn.com/content/image/1-s2.0-S2214109X20301212-mmc1.pdf", "online supplementary appendix"), "to the manuscript accompanying this app 
         contains detailed discussions of methods and data sources used in this model. This web-based version of the model allows users to 
         inspect the data sources and assumptions that go into each intervention. The online tool is not designed to generate precise 
         estimates of the cost of interventions to do budgeting and planning at a country level. Rather, it is meant to give the user a 
         sense of the probable magnitude of the cost of various combinations of interventions (or EUHC as a whole) in two country economic 
         contexts (LIC and lower-MIC) that in reality are very heterogeneous mixes of countries and health system arrangements. The tool 
         should be understood as an effort to produce generic estimates for broad audiences and stimulate dialogue on UHC financing and 
         priority setting with particular reference to priorities across disease targets, platforms for care delivery, urgent vs. chronic 
         care needs, and health system objectives.",
            br(),
            br(),
                    
        "The app reports 'counterfactual' annual total and incremental costs for the year 2015 for two stylized countries, 'low-income 
        countries' and 'lower-middle-income countries.' The costs are counterfactual because they assume an instantaneous shift 
        from baseline coverage to target coverage, holding all other parameters (such as intervention unit cost and population in need 
        of the intervention) constant. Epidemiological, demographic, and macroeconomic data for these two countries are taken from 
        aggregate estimates for LICs and lower-MICs as defined by the World Development Indicators (2014), the country income 
        classification system used in DCP3. EUHC and HPP costs in specific countries or regions will inevitably deviate significantly 
        from these stylized estimates.", 
        br(),
        br(),
        
        "The baseline cost (BC) of the EUHC package or HPP - i.e., the overall cost of all EUHC or HPP interventions as they are currently 
        being implemented, can be represented simply as:", 
        br(),
        br(),
        
                img(src="eq1.PNG", width="250", class="center"),
        br(),
        br(),
        
        "where", HTML(paste0("pop",tags$sub("i"))),
        "is a number of individuals in need of intervention i,", HTML(paste0("cov",tags$sub("i,0"))), "is the proportion of the population 
        in need who are currently covered by intervention i (baseline coverage), and", HTML(paste0("cost",tags$sub("i"))), "is the unit cost of the intervention 
        (incorporating both recurrent costs and annualized capital costs) per individual (given by", HTML(paste0("pop",tags$sub("i"))), ") served.",
        br(),
        br(),
        
        "The total (counterfactual) present-day cost of EUHC or HPP at full implementation can be represented as:",
        br(),
        br(),
        
        img(src="eq2.PNG", width="250", class="center"), 
        br(),
        br(),
  
        "where", HTML(paste0("cov", tags$sub("i,1"))),  
        "is the target proportion of the population in need who would be covered by intervention i (target coverage). For this analysis, we set",
        HTML(paste0("cov", tags$sub("i,1"))),
        "at 80% for all interventions, a target that is consistent with prior targets set by WHO for a variety of conditions. Since each 
        intervention is currently being delivered at a different coverage level, the coverage gap for each intervention will vary. 
        Within this framework, the incremental (counterfactual) present-day cost of EUHC or HPP is", br(),br(),
        
        img(src="eq3.PNG", width="150", class="center"), br(),br(),
        
        "or alternatively,", br(),
        
        img(src="eq4.PNG", width="350", class="center"), br(),br(),
        
        "This approach requires unit cost estimates that reflect long-run average costs rather than marginal costs. An implication 
        of using long-run average cost data is that cost structures will be more similar than different when extrapolated across regions 
        (see below). ", br(),br(),
        
        "As described below, our unit cost data include both direct service delivery costs and health system costs. We used a series of 
        markups to estimate health system costs for each intervention, and these costs were divided into facility-level 'ancillary services' 
        costs and above-facility costs:", br(),
        
        img(src="eq5.PNG", width="400", class="center"), br(),
        
       "In this analysis, alpha and beta are simply scalars derived from the literature; however, more complex functions (e.g., that 
       vary by intervention scale, delivery platform, and/or programme area) could be specified. The 'EDITABLE TABLE' and 'SCENARIO ANALYSIS' 
       tabs in this app allow the user to explore alternative assumptions regarding data inputs and model parameters.", br(),br(),
       
       "NB: in many countries, interventions that are offered through the public sector require significant cost-sharing or even 
       cost-recovery through user fees. In these cases, our model would overestimate BC for an intervention at coverage level",
       HTML(paste0("cov", tags$sub("i,0"))),
      
       "from the health system perspective, since",
       HTML(paste0("cost", tags$sub("i"))), "would be partially borne by the user. Hence for these countries, 
       within the context of costing achievement of UHC, the actual incremental cost IC' is likely to be higher than the estimated value IC, 
       as IC' includes both the cost of going from",
       HTML(paste0("cov", tags$sub("i,0"))), "to", HTML(paste0("cov", tags$sub("i,1"))), "(described above) and the cost of reducing 
       out-of-pocket payments for the proportion of the population currently covered at",  HTML(paste0("cov", tags$sub("i,0"))), ". It 
       was outside the scope of this costing exercise to generate precise, country-level estimates of CE and IC'; these would however 
       be a necessary component of country-level priority setting and resource allocation exercise."
         
        ), br(),
          
          h4(strong("Instructions for using this app")),
        h5( 
             strong("Step 1:"), "Open the 'INPUT DATA' tab to find out more about the original data sources and assumptions we made in 
             costing each intervention. This tab also allows the user to adjust the assumed share of intervention unit costs 
             (taken from the literature) due to traded goods and inspect the impact of this assumption on the final (modeled) unit cost 
             inputs.",
             br(), br(),
             strong("Step 2:"), "Open the 'EDITABLE TABLE' tab to modify the input data used to estimate total and incremental costs in 
             LICs and lower-MICs, including adjusted unit cost (given in 2016 US dollars), population in need, and baseline coverage and 
             target coverage. This tab also allows the user to specify a 'custom package,' with cost estimates presented alongside EUHC 
             and HPP cost estimates in the 'RESULT' tab. (By default, the custom package tick boxes are separate for LICs and lower-MICs.)",
             br(),br(),
             strong("Step 3:"), "Open the 'RESULT' tab to see the summary table and figures based on the changes you made in the editable 
             tables. Disaggregated costs are presented in bar graphs below the summary table. Results can also be downloaded as csv files.",
             br(),br(),
             strong("Step 4:"), "Open the 'SCENARIO ANALYSIS' tab to conduct one-, two- or multi-way sensitivity analyses by changing the 
             default settings (i.e., target coverage, tradable ratio, ancillary facility cost and above facility cost as well as multipliers). 
             Results of the user-specified sensitivity analyses can be downloaded as csv files."),
             

            h5(strong("Source code:"),"All the source codes and input data are avaialable in the following GitHub account:",
            a(href="https://github.com/DCP-UW/DCP-Cost-Model", "link to GitHub."), align = "left")),br(),
        
          column(1,"")
),
fluidRow(column(1,""),
         column(10,h6(strong("last updated 26 May 2020"), align = "right", style ="color:#900C3F")),
         column(1,""))




)
           ))
  ,
  
  tabPanel(p("Input data", class="tab"),
           fluidRow(
             column(1,""),
             column(5, "In this tab, you can see the data sources and assumptions used for 
                    each intervention by clicking the '+' button next to the intervention 'Code'. If desired, you can 
                    change the assumption regarding the proportion of each unit cost attributed to traded goods and see
                    how alternative assumptions affect the adjusted (modeled) average costs in LICs and lower-MICs. Note: 
                    these changes will not be carried over to the other tabs."),br(),
             column(3, sliderInput(inputId = "tradable2",
                                    "Proportion of unit cost attributed to traded goods.",
                                    min=0, max = 1, value=0.3,step=0.1)),
             column(2, downloadButton(outputId = "downloadinputdata", "Download input data")),
             column(1,"")),
             fluidRow(
               column(1,""),
               column(width = 10, height = "auto", DT::dataTableOutput("raw.table")),
               column(1,""))
           ),
  
  # Setting page (UI)------------
  tabPanel(p("Editable table", class="tab"),
           fluidRow(
             column(1,""),
             column(10, 
                    p("In this tab, you can edit the four parameters used to estimate the total and incremental cost of each intervention:",br(),br(),
                      "1. Adjusted (modeled) unit cost (given in 2016 US dollars)",br(),
                      "2. Population in need of the intervention", br(),
                      "3. Baseline intervention coverage", br(),
                      "4. Target intervention coverage",br(), br(),
                      
                      "These edits will appear in the figures, 'Incremental Cost of EUHC by Urgency & Platform' and 'Baseline & Incremental
                      Costs by Objective' presented on the 'RESULTS' tab.",
                      
                      br(),br(),
                      "You can also design a 'custom' health benefits package by selecting or de-selecting interventions ticked in the 
                      'Custom package' column. Total and incremental costs will be automatically calculated for your custom package (including any of your edits
                      to the unit costs, population in need, baseline coverage or target coverage below)
                      and presented in the 'RESULTS' tab in the 'Custom package' column of the table and in the two figures.
                      Note: the custom package selections and de-selections need to be done separately for LICs and 
                      lower-MICs."
                      )
                    ),br(),
             column(1,"")
                    ),
           fluidRow(
             column(1,""),
             column(10,  height = "auto",            
                    tabsetPanel(type = "tabs",
                                tabPanel("LIC", rHandsontableOutput("hot")),
                                tabPanel("lower-MIC", rHandsontableOutput("hot.lmic"))
             ),
             downloadButton(outputId = "downloadeditData_LIC", "Download the LIC editable table"),
             downloadButton(outputId = "downloadeditData_LMIC", "Download the lower-MIC editable table")
             
             ),
             column(1,"")
           )

),
  tabPanel(p("Results", class="tab"),
           sidebarPanel(
             p("The estimates for the 'custom package' and for the figures below are based on the changes made in the 'EDITABLE TABLE' tab. 
               The 'default setting' estimates for EUHC and HPP, provided here for comparison, are based on the values shown in the 
               'INPUT DATA' tab rather than the values in the 'EDITABLE TABLE' tab. The default selections for the 'custom package' are 
               the same as for the HPP.",
                br(), br(),
                "The markups for facility-level and above-facility health system costs are fixed at 50% and 17% respectively. 
               (These parameters can be adjusted in the 'SCENARIO ANALYSIS' tab.) Estimates of total and incremental costs are 
               provided to two significant digits.", 
               br(), br()),
               
             downloadButton(outputId = "downloadData", "Download final result")),
           
           mainPanel(
             fluidRow(
               column(width = 12, height = "auto", tableOutput("final.result"))
             ),br(),
             fluidRow(
               tabsetPanel(type = "tabs",
                           tabPanel("Incremental Cost of EUHC by Urgency & Platform", plotOutput("figure_urgency")),
                           tabPanel("Baseline & Incremental Costs by Objective", plotOutput("figure_objective"))
                           )
               #column(width = 6, height = "auto", plotOutput("figure_urgency"))
             )
           )),
tabPanel(p("Scenario analysis", class="tab"),
         sidebarPanel(
           p("The default setting of cost estimation in the paper used the following settings:"),br(),br(),
           p("- Target coverage = 0.80", br(),
             "- Tradable cost ratio = 0.3", br(), 
             "- Ancillary health facility cost = 0.50", br(),
             "- Above health facility cost = 0.17"
           ),
           strong("Change these settings and multipliers using the sliders below to calculate differences in incremental and total costs.") ,
           fluidRow(column(6,sliderInput(inputId = "UC_multi",
                                         "Unit cost multiplier",
                                         min=0.7, max = 1.3, value=1, step=0.1)),
                    column(6,sliderInput(inputId = "PIN_multi",
                                         "Population in need multiplier",
                                         min=0.5, max = 1.5, value=1, step=0.1))),
           fluidRow(column(6,sliderInput(inputId = "target",
                                         "Target coverage (%)",
                                         min=0, max =100,step=1, value = 80)),
                    column(6,sliderInput(inputId = "coverage_multi",
                                         "Baseline coverage adjustment",
                                         min=-0.15, max = 0.15, value=0,step=0.01))),
                    
           fluidRow(column(6,sliderInput(inputId = "HF_cost",
                                         "Ancillary facility cost (%)",
                                         min=0, max = 100, value=50,step=1)),
                    column(6,sliderInput(inputId = "above_cost",
                                         "Above facility cost (%)",
                                         min=0, max = 100, value=17,step=1))),  
           
           fluidRow(column(6,sliderInput(inputId = "fertility_multi_lic",
                                         "Total fertility rate (LICs) multiplier",
                                         min=0.87, max = 1.1, value=1,step=0.01)),
                    column(6,sliderInput(inputId = "fertility_multi_lmic",
                                         "Total fertility rate (lower-MICs) multiplier",
                                         min=0.8, max = 1.2, value=1,step=0.01))), 
           
           fluidRow(column(6,sliderInput(inputId = "tradable",
                                "Tradable ratio",
                                min=0, max = 1, value=0.3,step=0.1))),
           
           br(),
           strong("You can download the detailed input data based on the settings above."),br(),br(),
           downloadButton(outputId = "downloadData_detail_LIC", "Download LIC data"),br(),br(),
           downloadButton(outputId = "downloadData_detail_LMIC", "Download lower-MIC data")
         ),
         
         mainPanel(
           fluidRow(
             column(width = 12, height = "auto", tableOutput("result.user.table")),
             column(width = 12, height = "auto", plotOutput("result.figure1"))
             
           )
         )
)


)
)