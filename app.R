library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse) # seperat function
library(dplyr)
library(rsconnect)
library(DT)
library(plotly)
library(devtools)
library(dashboardthemes)
require(maps)
require(mapdata)
library(ggplot2)
library(ggrepel)
library(formattable)
library(ggplot2)
library(countrycode)
library(shinycssloaders)
library(wordcloud)
library(tidytext)
library(tidyr)
data("stop_words")

packageVersion('plotly')

dark=read_csv("Agora.csv")
dark_net=dark %>%
  rename(Item_Description= "Item Description") %>%
  filter(Category!="0.12780125125 BTC" & Category!="0.1905617980645162 BTC" &
           Category!= "home of the Body Bags  shotty  and mag  Kind Hearts and Gentle People" & 
           Category!="the Body Bags  shotty  and Mac make-up To: Kind Hearts and Gentle People" &
           Category!="the Body Bags  shotty  and mag To: Kind Hearts and Gentle People" ) %>%
  filter(!is.na(Price)) %>% 
  separate(Price, into = c("Price", "Currency"),   sep = " ", fill = "warn") %>%
  filter( Currency!="a" & Currency!="is" & Currency!="grams" & Price!="5g") %>%
  mutate(Price=as.numeric(Price)) %>%
  filter(!is.na(Category)) %>% 
  separate(Category, into = c("Category", "Sub_Category", "Product"),    sep = "/", fill = "warn")




# Define UI for application that draws a histogram
ui <- dashboardPage(   
  
  dashboardHeader(title= "Dark net marketplace"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Presentation", tabName = "present", icon=icon("broadcast-tower")),
      menuItem("Data",tabName ="datapage", icon = icon("database")  ),
      menuItem("Some numbers", tabName="basicanal", icon = icon("dashboard")),
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
     
       menuItem("Vendors", tabName = "vendors", icon = icon("dollar-sign") ),
               
      menuItem("Categories", tabName = "categories", icon = icon("chart-pie"), 
          menuItem("Drugs", tabName = "drugs", icon = icon("minus-circle")),
          menuItem("Services", tabName = "serv", icon = icon("phone-volume")),
          menuItem( "Counterfeits" , tabName = "fake", icon = icon("money-bill-alt")),
          menuItem( "Weapons" , tabName = "weapon", icon = icon("bomb")),
          menuItem( "Drug paraphernalia" , tabName = "para", icon = icon("circle")),
          menuItem( "Forgeries" , tabName = "forger", icon = icon("file-alt"))
             ),
      menuItem("Maps", tabName = "maps", icon = icon("globe-africa"))
    )
    
  ),
  dashboardBody(   shinyDashboardThemes(
    theme = "grey_dark"
  ),
  
  
  tabItems(
    tabItem(tabName = "present", 
            fluidPage(
              
              
              h1("Dark net dashboard"),
              
              p("The dark net or also called dark web is a term that describes the portions of the Internet
                that are not open to public and hidden. It is part of the deep web. The dark net is a place where illegal 
                transactions are made. This dashboard will give you an insight view of the listings found in Agora (a dark/deep web) marketplace from the years 2014 to 2015.
                Listings, vendors, product prices as well as descriptions of the products are on this dashboard."),
              
              br(),
              p("Are you ready to dive in a part of the deep web?"),
              br(),
              p(" The database used for this study is on Kaggle. You can find it here: https://www.kaggle.com/philipjames11/dark-net-marketplace-drug-data-agora-20142015

                  "),
              br()
             
             
              )
            
            ),
    tabItem(tabName = "datapage",
            
            tabBox(id="tabset1", height = "100%",width = "100%" ,
                   tabPanel("Data", 
                            
                            fluidPage( 
                              column(12, downloadButton("downloadDataFromTable", "Download Table Data")),
                              box(status="info",width=12, DT::dataTableOutput("database")  ) )
                            
                   ),
                   tabPanel(  "Statistics", 
                              
                              fluidPage( box(status="info", title="Summary", width= 12, verbatimTextOutput("strdat") ))
                   )
                   
            )
    ),
    
    tabItem(tabName = "basicanal",
            
            box(width=12, status = "info", solidHeader = FALSE, "The database contains 109689 observations or listings and 9 variables or columns. But I have done a lot of modications like 
                  dividing the category variable into 3 variables: Category, Sub_Category and Product and the price into: Price and Currency to able 
                to manipulate the Price column and remove some incorrect rows. In this data we have 109675 listings,  3189 vendors and 14 categories of 
                listings. There are different countries of origin and destination of the products. Clear maps were made after an intensif cleaning of the data. 
                The vendor and category that have the most listings are respectively 'optiman' with 881 listings and 'Drugs' with 109689 listings. 
                On the right you can see a wordcloud that represents the words used in the data to describe the items; 
                we can see the words that were repeated the most: 'listing', 'shipping', 'pills', 'profile', 'product' and 'price'.  "),
           
            tabBox(id="tabset3", height = "50%",width =5 ,
                   tabPanel("Sum up", 
                  fluidPage(        
                   box(width="100%", tableOutput("littlesummary"),
                       tableOutput("famousvendor"), tableOutput("famouscategory")  )
                             )
                          )
                   
                   ),
            
            fluidPage(
              
              
              
              box(width=7,title = "Wordcloud of the dark net data  ", status = "info", solidHeader = TRUE,    withSpinner(plotOutput("wordcloud" , height = 550), type = getOption("spinner.type", default = 8) )   )
            
              
            )
            
            
            
            
         
            
    ),
    
    tabItem(tabName = "overview",
            
            tabBox(id="tabset4", height = "100%",width = "100%" ,
                   
                   tabPanel("Bar plot",
                            fluidPage(
                              box(width=9,title = "Listings by category", status = "info", solidHeader = TRUE, plotlyOutput("bycategories1")),
                              box(width=3, status = "info", solidHeader = FALSE, "We can see that Drugs win the first place by far in terms of number
                                  of listings, followed by Services and Counterfeits. Tobacco, Jewelry and Chemicals get the last positions. 
                                  But let's see how much percentage share of market do these categories take. ")
                              
                              
                              )
                   ),
                   
                   tabPanel("Pie chart",
                            fluidPage(
                             box(width=12,title = "Listings by category", status = "info", solidHeader = TRUE, plotlyOutput("bycategories2")),
                              box(width=12, status = "info", solidHeader = FALSE, "Drugs have 84% of the total market share of the 14 categories.
                                  The rest of the categories has almost the same percentage share of the market.
                                  It is not a surprise drugs have always generated a lot of money. Let's study now the Price variable.")
                              
                              ) ),   
                  
                   
                   tabPanel("Prices",
                            fluidPage(
                              box(width=7,title = "Price variable density", status = "info", solidHeader = TRUE, plotlyOutput("byprices")),
                              box(width=5, status = "info", solidHeader = FALSE, "As you can see the graph seems weird. We have a lot of items with 
                             low prices and very few items with high prices. But first, let's check if these high numbers are real and not just a mistake.",
                                 verbatimTextOutput("pricesummary"), "Well the currency is BITCOIN, we are not used to it so we can't really know the real value. 
                                Let's convert these prices to Dollars $ and see. Since the data obtained is from the period 2014 to 2015, let's take the average 
                                 conversion rate between BTC and USD for that period which is 340$ approximately." ,  verbatimTextOutput("pricesummarydollar"), 
                                 "75% of the listings have prices less or equal to 452$ the mean is 7535$ and the maximum price is 44 million dollars. 
                                 This amount is simply too high to be considered as a right amount. It is clearly a mistake. Below you can find the listings with 
                                 highest prices for each category." ),
                              box(width=12, status = "info", solidHeader = FALSE, DT::dataTableOutput("maxprice")  )
                              
                              
                            )   ),
                   
                   
                   tabPanel("90% of the data",
                            fluidPage(
                              box(width=7,title = "Price variable density", status = "info", solidHeader = TRUE, plotlyOutput("byprices90")),
 
  
                 
                              box(width = 5, status = "info", solidHeader = FALSE, "After checking the last data table that contains the listings 
                               with highest prices for every category, I concluded that these can't be their prices and they are not relevant
                               thus they should be removed. For that I removed the last and highest 10% of the data in terms of price. Below a summary
                                  of our Price variable.",
                                  verbatimTextOutput("pricesummary90"), "After removing only 10% of the highest prices, we have listings that cost up to 
                                  1500$. The distribution is clearer than the previous one. It is right-skewed, this means that the Price variable is highly assymmetric.
                                  There is a lot of items sold at a lower price but not so many items that are being sold at a really high price. You can 
                                  find below a data table that contains the listings that have the highest prices for each category." ,  verbatimTextOutput("pricesummarydollar90") ),
                                            
 
                                box(width=12, status = "info", solidHeader = FALSE, DT::dataTableOutput("maxprice90")  )
                              
                              
                              )   ),
                   
                   tabPanel("Prices by category", 
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                "This plot shows the density plot of the price of each category. For this plot, we kept only 80% of the data. We notice that
                                almost all the data for the information and data categories have low price compared to the categories of weapons, jewelry, electronics 
                                and chemicals."),
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("bypricescat", height = 500 ))
                              
                            )  ),
                   tabPanel("Box plots",
                            fluidPage(
                            
                            
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "With the box plots the contrast between weapons and data and information is more obvious. Weapons are more expensive 
                                  compared to the data or the items that belong to the info category. Drugs and eletronics are also expensive. "),
                              box(width=12, status = "info", solidHeader = FALSE, plotlyOutput("catboxplot", height = 500 ))
                              
                              
                              
                              )    ),
                  
                   
                   tabPanel("By rates",
                            fluidPage(
                              box(width=7,title = "Rating variable density", status = "info", solidHeader = TRUE, plotlyOutput("byratings")),
                              box(width=5, status = "info", solidHeader = FALSE, "In general the items are very well rated. With a mean and a median that are equal to 4.9,
                                  we almost don't have many items that are bad rated.",
                                  verbatimTextOutput("ratesummary"), " " ,  verbatimTextOutput("ratesummarydollar") )
                               
                              
                              
                              )   )
                         
                   
                   
                   
            )
            
            
                   ),
    
    tabItem(tabName = "vendors",
            tabBox(id="tabset44",  height = "100%",width = "100%" ,
                   
                   tabPanel("By category",
                              fluidPage(
                                box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                    "We have here the top 20 vendors that have the highest number of listings. Vendors like optiman sexyhomer mssource and profesorhouse 
                                     get respectively the four first positions. They are probably the ones who control the Agora market. We can see also how many listings
                                    of each category a vendor has thus we are able to see who is  specialized in what. For example, optiman though he sells all types of 
                                    items, he sells Drug paraphernalia the most. Sexyhomer is specialized in counterfeits. Mssource sells only drugs. 
                                    Another thing that caught my attention is that some vendors deal with drugs only. "),
                                box(width=12, status = "info", solidHeader = FALSE, plotlyOutput("byvendors", height = 500 ))
                                
                                
                              ) ),
                   
                   tabPanel("By rate",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "This plot shows the top 20 vendor seen earlier and the rates given to them. Thomascheer and TheDigital
                                  are relatively less rated than sexyhomer, optiman or captainkirk. Another interesting fact is the vendors that have the highest
                                  number of listings are the best rated."),
                              box(width=12, status = "info", solidHeader = FALSE, plotlyOutput("rateboxplot", height = 500 ))
                              
                              
                            )),
                   
                   tabPanel("By price",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "We notice that prices and the variances of prices differ from a vendor to another. In fact optiman and sexyhomer, the two top vendors 
                                  in term of number of listings, have low prices except some products or items that are quite expensive. However mssource,
                                  about 75% of his listing prices range between 200$ and 1500$ . We also notice that the vendors that are specialized in drugs or at least most of their listings
                                  are drugs have higher prices. That makes sense, drugs are expensive!"),
                              box(width=12, status = "info", solidHeader = FALSE, plotlyOutput("pricevendorboxplot", height = 500 ))
                              
                              
                            )),
                   tabPanel("Scatter plot",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "The scatter plot represents the vendors according to their average price and the number of listings they have.  "),
                              box(width=12, status = "info", solidHeader = FALSE, plotlyOutput("vendorscatterplot", height = 500 ))
                              
                              
                            ))
                   
                   )   ),
    
    tabItem(tabName = "drugs",
            
            tabBox(id="tabset5", height = "100%",width = "100%" ,
                   
                   tabPanel("By subcategory",
                            fluidPage(
                              box(width=6,title = "Drug listings by subcategory", status = "info", solidHeader = TRUE, plotlyOutput("bydrugsubcat")),
                              box(width=6,title = " ", status = "info", solidHeader = FALSE, tableOutput("bydrugs"), verbatimTextOutput("summarydrugs")),
                              box(width=12, status = "info", solidHeader = FALSE, "I wanted to dig a little deeper and analyse the subcategories. 
                                  According to the bar plot above, the cannabis takes the lead in the drug market, followed by the ecstasy and the stimulants.
                                  Every subcategory of drug is in itself sold in different forms or sub-subcategory if I may say that.")
                              
                              
                              ) ),   
                   tabPanel("Density Prices",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "This plot shows the density plot of the price of each subcategory.We took just the first 80% of the data. We notice that
                                 the densities are quite similar."),
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("subcatprice", height = 500 ))
                              
                              
                              )
                   ),
                   
                   tabPanel("Box plots",
                            fluidPage(
                              box(width=9,title = "Drugs prices box plots ", status = "info", solidHeader = TRUE, plotlyOutput("subcatboxplot" , height = 500)),
                              box(width=3, status = "info", solidHeader = FALSE, "The median prices of the drug subcategories are kind of close to each other
                              however their variances are relatively different. In fact drugs like RCs, ecstasy, dissociatives and stimulants have a large variance
                                  compared to the steroids and the prescriptions.  ")
                              
                              
                              )   ),
                   
                   tabPanel("Items desciption ",
                            fluidPage(
                              box(width=6,title = "Most repeated words ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("drugwordfreq"), type = getOption("spinner.type", default = 8) ) ),
                              box(width=6, status = "info", solidHeader = FALSE, withSpinner(plotOutput("drugwordcloud", height = 500), type = getOption("spinner.type", default = 8) ) ),
                              box(width=12, status = "info", solidHeader = FALSE, "The most repeated words are 'quality' 'pure', 'pills'. You can also 
                                  find in the wordcloud words like 'cocaine' 'cannabis' and 'xtc' that refers to 'ecstasy'. ")
                              
                            )    )
                   
             
                   
            )
            
            
            ),
    
    
    tabItem(tabName = "serv",
            
            tabBox(id="tabset6", height = "100%",width = "100%" ,
                   
                   tabPanel("By subcategory",
                            fluidPage(
                              box(width=6,title = "Service listings by subcategory", status = "info", solidHeader = TRUE, plotlyOutput("byservsubcat")),
                              box(width=6,title = " ", status = "info", solidHeader = FALSE, tableOutput("byservs"), verbatimTextOutput("summaryserv")),
                              box(width=12, status = "info", solidHeader = FALSE, "Most of the service listings are Money. But what does it mean money?
                                  What we want to say by Money is Buying/exchanging money, Bitcoin Mining, Cloud Mining, lottery or even fake money/credit cards.")
                              
                              
                              ) ),   
                   tabPanel("Density Prices",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "Money, hacking and 'other' subcategories have a  right-skewed distribution. Unlike Travel subcategory that seems to have a lot
                                  more expensive items. "),
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("subcatpriceserv", height = 500 ))
                              
                              
                              )
                            ),
                   
                   tabPanel("Box plots",
                            fluidPage(
                              box(width=9,title = "Service prices box plotss ", status = "info", solidHeader = TRUE, plotlyOutput("subcatboxplotserv" , height = 500)),
                              box(width=3, status = "info", solidHeader = FALSE, " The box plots show that the items that belong to travel subcategory have a distribution different from 
                                  the rest. Its median price is way higher than the median price of the other subcategories. Half of the travel listings prices range between
                                  600$-1300$, while most of the other subcategories items have prices less than 600$.")
                              
                              
                              )   ),
                   
                   tabPanel("Items desciption ",
                            fluidPage(
                              box(width=6,title = "Most repeated words ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("servwordfreq"), type = getOption("spinner.type", default = 8) ) ),
                              box(width=6, status = "info", solidHeader = TRUE, withSpinner(plotOutput("servwordcloud", height = 500), type = getOption("spinner.type", default = 8) ) ),
                              box(width=12, status = "info", solidHeader = FALSE, "The most repeated words are 'account', 'credit', 'card', 'paypal'. You can also find 'hacking', 'anonymous', 'country', 'visa' ")
                              
                            )    )
                   
            )
            
            
    ),
    
    
    
    
    
    
    tabItem(tabName = "fake",
            
            tabBox(id="tabset6", height = "100%",width = "100%" ,
                   
                   tabPanel("By subcategory",
                            fluidPage(
                              box(width=6,title = "Conterfeits listings by subcategory", status = "info", solidHeader = TRUE, plotlyOutput("byfakesubcat")),
                              box(width=6,title = " ", status = "info", solidHeader = FALSE, tableOutput("byfake"), verbatimTextOutput("summaryfake")),
                              box(width=12, status = "info", solidHeader = FALSE, "For the counterfeits the subcategory that has the highest number of listings
                                  is by far watches, followed by money and then clothing. In fact people tend to create fake watches and bills more often .  ")
                              
                              
                              ) ),   
                   tabPanel("Density Prices",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "This plot shows the density plot of the price of each subcategory. Again, we took only the first 80% of the data. We notice that
                                  all the densities a alike."),
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("subcatpricefake", height = 500 ))
                              
                              
                              )
                            ),
                   
                   tabPanel("Box plots",
                            fluidPage(
                              box(width=9,title = " Counterfeits prices box plots ", status = "info", solidHeader = TRUE, plotlyOutput("subcatboxplotfake" , height = 500)),
                              box(width=3, status = "info", solidHeader = FALSE, " There is no surprise the median prices of money, watches and electronics are a little higher than
                                  clothes and accessories prices. The same thing for the variance.")
                              
                              
                              )   ),
                   
                   tabPanel("Items desciption ",
                            fluidPage(
                              box(width=6,title = "Most repeated words ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("fakewordfreq"), type = getOption("spinner.type", default = 8) ) ),
                              box(width=6, status = "info", solidHeader = FALSE, withSpinner(plotOutput("fakewordcloud", height = 500), type = getOption("spinner.type", default = 8) ) ),
                              box(width=12, status = "info", solidHeader = FALSE, " The most repeated words are material, quality, nike, brand...certainly for clothes. Also steel, replica,
                                  diameter, rolex... for watches. We can find also bills, original, authentic...")
                              
                            )    )
                   
                   
            )
            
            
    ),
    
    
    tabItem(tabName = "weapon",
            
            tabBox(id="tabset6", height = "100%",width = "100%" ,
                   
                   tabPanel("By subcategory",
                            fluidPage(
                              box(width=6,title = "Weapon listings by subcategory", status = "info", solidHeader = TRUE, plotlyOutput("byweaponsubcat")),
                              box(width=6,title = " ", status = "info", solidHeader = FALSE, tableOutput("byweapon"), verbatimTextOutput("summaryweapon")),
                              box(width=12, status = "info", solidHeader = FALSE, "For the weapons the subcategory that has the highest number of listings
                                  is by far the lethal firearms. Followed of course by their ammunitions. Melee weapons take the third position. ")
                              
                              
                              ) ),   
                   tabPanel("Density Prices",
                            fluidPage(
                              box(width=12,title="Click on + to read the analysis ", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                  "This plot shows the density plot of the price of each subcategory.We took the first 80% of the data. We notice that
                                  the densities of  the non-lethal firearms, the melee weapons and the ammunitions are right-skewed. The distribution of the 
                                  lethal firearms is uniform. "),
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("subcatpriceweapon", height = 500 ))
                              
                              
                              )
                            ),
                   
                   tabPanel("Box plots",
                            fluidPage(
                              box(width=9,title = "Weapon listings box plot", status = "info", solidHeader = TRUE, plotlyOutput("subcatboxplotweapon" , height = 500)),
                              box(width=3, status = "info", solidHeader = FALSE, " It is clear that the distribution of the lethal firearms prices is different from 
                                  the others. Its median value is by far the highest and half of the lethal firearms prices range between 300$-1000$ while all the 
                                  rest have almost all their prices less than 400$ except for some values.")
                              
                              
                              )   ),
                   
                   tabPanel("Items desciption ",
                            fluidPage(
                              box(width=6,title = "Most repeated words ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("weaponwordfreq"), type = getOption("spinner.type", default = 8) ) ),
                              box(width=6, status = "info", solidHeader = FALSE, withSpinner(plotOutput("weaponwordcloud", height = 500), type = getOption("spinner.type", default = 8) ) ),
                              box(width=12, status = "info", solidHeader = FALSE, "The most repeated words for the weapon category are  shipping, gun, 9mm, weight..")
                              
                            )    )
                   
                   
            )
            
            
    ),
    
    
    
    tabItem(tabName = "para",
            
            tabBox(id="tabset6", height = "100%",width = "100%" ,
                   
                   tabPanel("By subcategory",
                            fluidPage(
                              box(width=6,title = "Drug listings by subcategory", status = "info", solidHeader = TRUE, plotlyOutput("byparasubcat")),
                              box(width=6,title = " ", status = "info", solidHeader = FALSE, tableOutput("bypara"), verbatimTextOutput("summarypara")),
                              box(width=12, status = "info", solidHeader = FALSE, " For the drug paraphernalia the subcategory that has the highest number of listings
                                  is pipes followed by containers and stashes.  ")
                              
                              
                              ) ),   
                   tabPanel("Density Prices",
                            fluidPage(
                             
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("subcatpricepara", height = 500 ))
                              
                              
                              )
                            ),
                   
                   tabPanel("Box plots",
                            fluidPage(
                              box(width=9,title = "Evolution of weapons ", status = "info", solidHeader = TRUE, plotlyOutput("subcatboxplotpara" , height = 500)),
                              box(width=3, status = "info", solidHeader = FALSE, " The distribution of the subcategories is similar.")
                              
                              
                              )   ),
                   
                   tabPanel("Items desciption ",
                            fluidPage(
                              box(width=6,title = "Most repeated words", status = "info", solidHeader = TRUE, withSpinner(plotOutput("parawordfreq"), type = getOption("spinner.type", default = 8) ) ),
                              box(width=6, status = "info", solidHeader = FALSE, withSpinner(plotOutput("parawordcloud", height = 500), type = getOption("spinner.type", default = 8) ) ),
                              box(width=12, status = "info", solidHeader = FALSE, "The most repeated words are weed, sick, grinder, stash and vaporizer....")
                              
                            )    )
                   
                   
            )
            
            
    ),
    
    
    tabItem(tabName = "forger",
            
            tabBox(id="tabset6", height = "100%",width = "100%" ,
                   
                   tabPanel("By subcategory",
                            fluidPage(
                              box(width=6,title = "Forgeries listings by subcategory", status = "info", solidHeader = TRUE, plotlyOutput("byforgersubcat")),
                              box(width=6,title = " ", status = "info", solidHeader = FALSE, tableOutput("byforger"), verbatimTextOutput("summaryforger")),
                              box(width=12, status = "info", solidHeader = FALSE, "For the forgeries the subcategory that has the highest number of listings
                                  is physical documents.  ")
                              
                              
                              
                              ) ),   
                   tabPanel("Density Prices",
                            fluidPage(
                        
                              box(width=12, status = "info", solidHeader = FALSE, plotOutput("subcatpriceforger", height = 500 ))
                              
                              
                              )
                            ),
                   
                   tabPanel("Box plots",
                            fluidPage(
                              box(width=12,title = "Evolution of weapons ", status = "info", solidHeader = TRUE, plotlyOutput("subcatboxplotforger" , height = 500))
                          
                              
                              
                              )   ),
                   
                   tabPanel("Items desciption ",
                            fluidPage(
                              box(width=6,title = "Most repeated words ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("forgerwordfreq"), type = getOption("spinner.type", default = 8) ) ),
                              box(width=6, status = "info", solidHeader = FALSE, withSpinner(plotOutput("forgerwordcloud", height = 500), type = getOption("spinner.type", default = 8) ) )
                              
                            )    )
                   
                   
            )
            
            
    ),
    
    
    tabItem(tabName = "maps", 
            tabBox ( id="tabset8", height = "100%", width = "100%" ,
                 
                       
                       tabPanel("Countries of origins",
                                fluidPage(
                                  box(width=12,title = "the main points of origin (please wait the map may take several seconds)  ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("origincount"), type = getOption("spinner.type", default = 8)  ) ),
                                  box(width=12, status = "primary", solidHeader = FALSE, "The US is way above any other country by comparison and is clearly the winner, followed by Australia, England and Germany.")
                                  
                                )    ),   
                       tabPanel("Countries of destinations",
                                fluidPage(
                                  box(width=12,title = "the main points of destination (please wait the map may take several seconds)  ", status = "info", solidHeader = TRUE, withSpinner(plotOutput("destcount"),  type = getOption("spinner.type", default = 8) )),
                                  box(width=12, status = "info", solidHeader = FALSE, "Again, the USA takes the lead, followed by Australia, England and Germany.")
                                  
                                  
                                )    )   
                       
                       
                     ))
    
    
    
    
    
    
    
    
    
    
                            )
  
  
  
    )
  
  
  
            )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$database <- DT::renderDataTable({dark_net} ,
                                         #extensions="Responsive",
                                         options = list(
                                           columnDefs = list(list(className = 'dt-center', targets = 5)),
                                           pageLength = 5,
                                           lengthMenu = c(5, 10, 15, 20), scrollX = TRUE)   )
  
  
  
  
  output$strdat= renderPrint ({
    summary(dark_net) })
  
  #############################################################################################
  
  
  summarytable=reactive({
  
     listing= dark_net %>%
      count
   
   vendor= dark_net %>%
     group_by(Vendor) %>%
     summarise(numb=n()) %>%
     count %>%
     collect
     
   categories= dark_net %>%
     group_by(Category) %>%
     summarise(cat=n()) %>%
     count %>%
     collect
   
    
   origins= dark_net %>%
     group_by(Origin) %>%
     summarise(ori=n()) %>%
     count %>%
     collect
   
   destinations=dark_net %>%
     group_by(Destination) %>%
     summarise(dest=n()) %>%
     count %>%
     collect 
   listing=listing %>% 
     rename(Values=n) %>%
      mutate(labels= "listings")%>%
     rbind(c(vendor$n,  "Vendors" ) ) %>%
   rbind(c( categories$n, "Categories")) %>%
     rbind(c( origins$n, "Countries of origin")) %>%
     rbind(c( destinations$n, "Countries of destination"))
   
   summarytable=listing
   
 } )
  
  
  output$littlesummary <-renderTable(
    
    summarytable(),rownames=FALSE 
  )
 
  
  output$famousvendor= renderTable({
    
    ll=dark_net %>%
      group_by(Vendor)%>%
      summarise(listings=n() ) %>%
      top_n(1) %>%
      collect
    
  })
  
  
  
  output$famouscategory= renderTable({
    
    ll=dark_net %>%
      group_by(Categories=Category)%>%
      summarise(listings=n() ) %>%
      top_n(1) %>%
      collect
    
  })
  
  
  output$wordcloud=renderPlot({

    
    f=data_frame( text=dark_net$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
  
  
  
 
 #########################################################################################################
 
   
   output$bycategories1=renderPlotly({
     ss= dark_net  %>%
       group_by(Category) %>%
       summarise(numbertrans=n()) %>%
       collect
     
     plot_ly(ss, x = ~numbertrans, y = ~Category,  type = 'bar'  , orientation = 'h',
             marker = list(color = "darkorchid"  ) ) %>%
       
       layout(title = "", autosize = T,
              xaxis = list(title = "Number of listings"),
              yaxis = list(title = "By Category"))
     
   })
   
   
   
  output$bycategories2=renderPlotly({
    ss= dark_net %>%
      group_by(Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, labels = ~Category, values = ~numbertrans,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Category ,numbertrans ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Categories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
 ######################################################################## 
  output$byprices= renderPlotly({

    
    
    ggplot(dark_net, aes(as.numeric(dark_net$Price))) + 
      geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
      geom_density(fill = "#ff4d4d", alpha = 0.5) + 
      theme(panel.background = element_rect(fill = '#ffffff')) + 
      ggtitle("Density with Histogram overlay")
    
    
    
  })
  
  dark_net_dollar= reactive({
    dark_net_dollar=dark_net %>%
      mutate(Dollar_Price=Price*340)
    
  })
  
  
  output$pricesummary= renderPrint({
    summary( as.numeric(dark_net$Price)) 
    
  })
 
  output$pricesummarydollar= renderPrint({
    summary( as.numeric(dark_net_dollar()$Dollar_Price)) 
    
  })
  
  output$maxprice= DT::renderDataTable({
    dark_net_dollar() %>%
      group_by(Category) %>%
      filter(Dollar_Price==max(Dollar_Price)) },options = list(
        columnDefs = list(list(className = 'dt-center', targets = 5)),
        pageLength = 5, scrollX = TRUE) 
    
  )
  
  ###########################################################################
  dark_net_90=reactive({
  dark_net_90= dark_net_dollar() %>%
    filter(Dollar_Price<=quantile(Dollar_Price, 0.9))
  })
  
  output$byprices90= renderPlotly({
    ggplot(dark_net_90(), aes(Dollar_Price)) + 
      geom_histogram(aes(y = ..density..), alpha = 0.7, fill = "#333333") + 
      geom_density(fill = "#ff4d4d", alpha = 0.5) + 
      theme(panel.background = element_rect(fill = '#ffffff')) + 
      ggtitle("Density with Histogram overlay")
  })
  
  output$pricesummary90= renderPrint({
    summary( as.numeric(dark_net_90()$Dollar_Price)) 
    
  })
  
  output$maxprice90= DT::renderDataTable({
    dark_net_90() %>%
      group_by(Category) %>%
    filter(Dollar_Price==max(Dollar_Price)) },options = list(
      columnDefs = list(list(className = 'dt-center', targets = 5)),
      pageLength = 5, scrollX = TRUE) 
    
  )
  
  
  
  ###########################################################################
  
 world_data= reactive({ 
  world_data = map_data("world") %>% 
    as_tibble() %>% 
    mutate(iso = countrycode(region, "country.name", "iso2c"))
 })
  
  output$origincount= renderPlot({ 
    
      
      suppressWarnings(origin <- countrycode(dark_net$Origin, "country.name", "iso2c") %>% 
                         as_tibble() %>% 
                         setNames("iso") %>% 
                         filter(!is.na(iso)))
   origin= origin %>% 
      group_by(iso) %>% 
      summarise(freq = n()) 
   
   origin_data <- left_join(world_data(), origin, by = "iso") %>% 
     filter(!is.na(freq))
   
   ggplot(origin_data, aes(long, lat)) +
     geom_polygon(aes(group = group, fill = freq)) +
     labs(x = "Longitudes", y = "Latitudes", title = "Origin of Products sold on Agora") +
     coord_equal()
    
  })
  
  
  output$destcount= renderPlot({ 
    
    
    suppressWarnings(dest <- countrycode(dark_net$Destination, "country.name", "iso2c") %>% 
                       as_tibble() %>% 
                       setNames("iso") %>% 
                       filter(!is.na(iso)))
    
    dest=dest %>% 
      group_by(iso) %>% 
      summarise(freq = n()) 
    
    dest_data <- left_join(world_data(), dest, by = "iso") %>% 
      filter(!is.na(freq))
    
    
    ggplot(dest_data, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = freq)) +
      labs(x = "Longitudes", y = "Latitudes", title = "Destination of Products sold on Agora") +
      coord_equal()
  })
  ###########################################################################
  
  
 
  output$bypricescat= renderPlot({
    ll=dark_net %>%
      filter(Price<= quantile(Price, 0.8))
    
   ggplot(ll, aes(x = ll$Price)) + 
      geom_density(aes(fill = ll$Category)) + 
      facet_grid(~ll$Category) +
      facet_wrap(~ll$Category, scales="free", ncol=4) +  
      ggtitle("Kernel density estimate of Prices by category")   

    
  })
  
 
  output$catboxplot= renderPlotly({

  
    plot_ly(dark_net_90(), y = ~Dollar_Price, color = ~Category, type = "box")
    
  })
  
  
  
  
  
  output$byvendors= renderPlotly({ 
    pp=dark_net %>%
      group_by(Vendor) %>%
      summarise(freq=n()) %>%
      top_n(20) %>%
      collect
    
    ll= dark_net %>%
      filter(Vendor %in% pp$Vendor) %>%
      group_by( Vendor, Category) %>%
      summarise(Listings=n()) %>%
      collect

    p= ggplot(ll,  aes(x = Vendor , y = Listings , fill = Category)) + 
      ggtitle("Top 20 vendors by the number of listings")
    p + geom_bar(stat = "identity", position = "stack") + coord_flip() 
    
    
    
  })
  
  
  
  output$byratings= renderPlotly({
    
    
    ll= dark_net_90() %>%
      filter(!is.na(Rating)) %>% 
      separate(Rating, into = c("Rating", "out_of"),   sep = "/", fill = "warn") %>%
      mutate(Rating=as.numeric(Rating)) %>%
      filter(!is.na(Rating) )
    
    ggplot(ll, aes(Rating)) + 
      geom_density(fill = "#ff4d4d", alpha = 0.5) + 
      theme(panel.background = element_rect(fill = '#ffffff')) + 
      ggtitle("Density of ratings")
  })
  
  output$ratesummary= renderPrint({
    ll= dark_net_90() %>%
      filter(!is.na(Rating)) %>% 
      separate(Rating, into = c("Rating", "out_of"),   sep = "/", fill = "warn") %>%
      mutate(Rating=as.numeric(Rating)) %>%
      filter(!is.na(Rating) )
    
    summary( as.numeric(ll$Rating)) 
    
  })
  
  
  output$rateboxplot= renderPlotly({
    pp=dark_net %>%
      group_by(Vendor) %>%
      summarise(freq=n()) %>%
      top_n(20) %>%
      collect
    
 
    
    ll= dark_net_90() %>%
      filter(!is.na(Rating)) %>% 
      separate(Rating, into = c("Rating", "out_of"),   sep = "/", fill = "warn") %>%
      mutate(Rating=as.numeric(Rating)) %>%
      filter(!is.na(Rating) & Vendor %in% pp$Vendor) 
    
    plot_ly(ll, y = ~Rating, color = ~Vendor, type = "box")
    
  })
  
  
  output$pricevendorboxplot= renderPlotly({
    pp=dark_net %>%
      group_by(Vendor) %>%
      summarise(freq=n()) %>%
      top_n(20) %>%
      collect
    
    
    
    ll= dark_net_90() %>%
      filter(Vendor %in% pp$Vendor) %>%
      group_by( Vendor ) 
      collect
    plot_ly(ll, y = ~Dollar_Price, color = ~Vendor, type = "box")
    
  })
  
  
  output$vendorscatterplot= renderPlotly({
    pp=dark_net_90() %>%
      group_by(Vendor) %>%
      summarise(freq=n()) %>%
      top_n(100) %>%
      collect
    

    
    
    ll= dark_net_90() %>%
      
      filter(Vendor %in% pp$Vendor ) %>%
      group_by( Vendor ) %>%
      summarise(Listings=n(), Average_Price=mean(Dollar_Price)) %>%
     collect
    
    
    plot_ly(
      type = 'scatter',
      x = ll$Average_Price,
      y = ll$Listings,
      text = paste("Vendor: ", ll$Vendor,
                   "Price: ", ll$Average_Price,
                   "Listings: ", ll$Listings ),
      hoverinfo = 'text',
      mode = 'markers',
      transforms = list(
        list(
          type = 'groupby',
          groups = ll$Listings
        
        )
      )
    )
    
  })
  
   
 ################################################################################
  

  
  output$bydrugsubcat=renderPlotly({
    ss= dark_net %>%
      filter(Category=='Drugs') %>%
      group_by(Sub_Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, x = ~Sub_Category, y = ~ss$numbertrans,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "subcategories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
  output$bydrugs=renderTable({
    outtable= dark_net %>%
      filter(Sub_Category=="Cannabis" | Sub_Category=="Ecstasy" ) %>%
      group_by(Sub_Category, Product) %>%
      summarise(numbertrans=n()) %>%
      collect
  })
  
  output$summarydrugs= renderPrint({
    ll=dark_net_90() %>%
      filter(Category=="Drugs")
   
     summary(ll$Dollar_Price)
    
  })

  
  output$subcatprice= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Drugs")
    
   
    ggplot(ll, aes(x = ll$Dollar_Price)) + 
      geom_density(aes(fill = ll$Sub_Category)) + 
      facet_grid(~ll$Sub_Category) +
      facet_wrap(~ll$Sub_Category, scales="free", ncol=4) +  
      ggtitle("Kernel density estimate of Prices by category")   
    
    
  })
  
  
  output$subcatboxplot= renderPlotly({
    ll=dark_net_90() %>%
      filter(Category=="Drugs")

    plot_ly(ll, y = ~Dollar_Price, color = ~Sub_Category, type = "box")
    
  })

  
  output$drugorigincount= renderPlotly({ 
    pp=dark_net %>%
      filter(Category=="Drugs") %>%
      group_by(Origin) %>%
      summarise(freq=n()) %>%
      top_n(10)
    
   ll= dark_net %>%
     filter(Category=="Drugs" & Origin %in% pp$Origin ) %>%
      group_by( Origin, Sub_Category) %>%
      summarise(freq=n()) %>%
       collect

    
    p= ggplot(ll,  aes(x = Origin, y = freq, fill = Sub_Category)) + 
       ggtitle("Countries of origin")
     p + geom_bar(stat = "identity", position = "stack")
     
     
     
  })
  
  
  output$drugdestcount= renderPlotly({ 
    
    pp=dark_net %>%
      filter(Category=="Drugs") %>%
      group_by(Destination) %>%
      summarise(freq=n()) %>%
      top_n(10)
    
    ll= dark_net %>%
      filter(Category=="Drugs" & Destination %in% pp$Destination ) %>%
      group_by( Destination, Sub_Category) %>%
      summarise(freq=n()) %>%
      collect
    
    
    p= ggplot(ll,  aes(x = Destination, y = freq, fill = Sub_Category)) + 
      ggtitle("Countries of destination")
    p + geom_bar(stat = "identity", position = "stack")
    
    
  })
  
  
  
  output$drugwordfreq= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Drugs")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > 5000) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    
  })
  
  
  output$drugwordcloud=  renderPlot({  
    ll=dark_net_90() %>%
      filter(Category=="Drugs")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
 ############################################################################
  
  output$byservsubcat=renderPlotly({
    ss= dark_net %>%
      filter(Category=='Services') %>%
      group_by(Sub_Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, x = ~Sub_Category, y = ~ss$numbertrans,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "subcategories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
  output$byservs=renderTable({
    outtable= dark_net_dollar() %>%
      filter(Category=="Services" ) %>%
      group_by(Sub_Category) %>%
      summarise(Minimum=min(Dollar_Price) , Maximum= max(Dollar_Price), Mean=mean(Dollar_Price), Median=median(Dollar_Price) ) %>%
      collect
  })
  
  output$summaryserv= renderPrint({
    ll=dark_net_90() %>%
      filter(Category=="Services")
    
    summary(ll$Dollar_Price)
    
  })
  
  
  output$subcatpriceserv= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Services")
    
    
    ggplot(ll, aes(x = ll$Dollar_Price)) + 
      geom_density(aes(fill = ll$Sub_Category)) + 
      facet_grid(~ll$Sub_Category) +
      facet_wrap(~ll$Sub_Category, scales="free", ncol=2) +  
      ggtitle("Kernel density estimate of Prices by category")   
    
    
  })
  
  output$subcatboxplotserv= renderPlotly({
    ll=dark_net_90() %>%
      filter(Category=="Services")
    
    plot_ly(ll, y = ~Dollar_Price, color = ~Sub_Category, type = "box")
    
  })
  
  
  output$servwordfreq= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Services")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > 150) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    
  })
  
  
  output$servwordcloud=  renderPlot({  
    ll=dark_net_90() %>%
      filter(Category=="Services")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
#########################################################################
  
  
  output$byfakesubcat=renderPlotly({
    ss= dark_net %>%
      filter(Category=='Counterfeits') %>%
      group_by(Sub_Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, x = ~Sub_Category, y = ~ss$numbertrans,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "subcategories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
  output$byfake=renderTable({
    outtable= dark_net_dollar() %>%
      filter(Category=="Counterfeits" ) %>%
      group_by(Sub_Category) %>%
      summarise(Minimum=min(Dollar_Price) , Maximum= max(Dollar_Price), Mean=mean(Dollar_Price), Median=median(Dollar_Price) ) %>%
      collect
  })
  
  output$summaryfake= renderPrint({
    ll=dark_net_90() %>%
      filter(Category=="Counterfeits")
    
    summary(ll$Dollar_Price)
    
  })
  
  output$subcatpricefake= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Counterfeits")
    
    
    ggplot(ll, aes(x = ll$Dollar_Price)) + 
      geom_density(aes(fill = ll$Sub_Category)) + 
      facet_grid(~ll$Sub_Category) +
      facet_wrap(~ll$Sub_Category, scales="free", ncol=2) +  
      ggtitle("Kernel density estimate of Prices by category")   
    
    
  })
  
  output$subcatboxplotfake= renderPlotly({
    ll=dark_net_90() %>%
      filter(Category=="Counterfeits")
    
    plot_ly(ll, y = ~Dollar_Price, color = ~Sub_Category, type = "box")
    
  })
  output$fakewordfreq= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Counterfeits")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > 500) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    
  })
  
  
  output$fakewordcloud=  renderPlot({  
    ll=dark_net_90() %>%
      filter(Category=="Counterfeits")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
  #########################################################################
  
  output$byweaponsubcat=renderPlotly({
    ss= dark_net %>%
      filter(Category=='Weapons') %>%
      group_by(Sub_Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, x = ~Sub_Category, y = ~ss$numbertrans,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "subcategories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
  output$byweapon=renderTable({
    outtable= dark_net_dollar() %>%
      filter(Category=="Weapons" ) %>%
      group_by(Sub_Category) %>%
      summarise(Minimum=min(Dollar_Price) , Maximum= max(Dollar_Price), Mean=mean(Dollar_Price), Median=median(Dollar_Price) ) %>%
      collect
  })
  
  output$summaryweapon = renderPrint({
    ll=dark_net_90() %>%
      filter(Category=="Weapons")
    
    summary(ll$Dollar_Price)
    
  })
  
  output$subcatpriceweapon= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Weapons")
    
    
    ggplot(ll, aes(x = ll$Dollar_Price)) + 
      geom_density(aes(fill = ll$Sub_Category)) + 
      facet_grid(~ll$Sub_Category) +
      facet_wrap(~ll$Sub_Category, scales="free", ncol=2) +  
      ggtitle("Kernel density estimate of Prices by category")   
    
    
  })
  
  output$subcatboxplotweapon= renderPlotly({
    ll=dark_net_90() %>%
      filter(Category=="Weapons")
    
    plot_ly(ll, y = ~Dollar_Price, color = ~Sub_Category, type = "box")
    
  })
  
  output$weaponwordfreq= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Weapons")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > 30) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    
  })
  
  
  output$weaponwordcloud=  renderPlot({  
    ll=dark_net_90() %>%
      filter(Category=="Weapons")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
  #############################################################################
  
  
  output$byparasubcat=renderPlotly({
    ss= dark_net %>%
      filter(Category=='Drug paraphernalia') %>%
      group_by(Sub_Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, x = ~Sub_Category, y = ~ss$numbertrans,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "subcategories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
  output$bypara=renderTable({
    outtable= dark_net_dollar() %>%
      filter(Category=="Drug paraphernalia" ) %>%
      group_by(Sub_Category) %>%
      summarise(Minimum=min(Dollar_Price) , Maximum= max(Dollar_Price), Mean=mean(Dollar_Price), Median=median(Dollar_Price) ) %>%
      collect
  })
  
  output$summarypara = renderPrint({
    ll=dark_net_90() %>%
      filter(Category=="Drug paraphernalia")
    
    summary(ll$Dollar_Price)
    
  })
  
  output$subcatpricepara = renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Drug paraphernalia")
    
    
    ggplot(ll, aes(x = ll$Dollar_Price)) + 
      geom_density(aes(fill = ll$Sub_Category)) + 
      facet_grid(~ll$Sub_Category) +
      facet_wrap(~ll$Sub_Category, scales="free", ncol=2) +  
      ggtitle("Kernel density estimate of Prices by category")   
    
    
  })
  
  output$subcatboxplotpara = renderPlotly({
    ll=dark_net_90() %>%
      filter(Category=="Drug paraphernalia")
    
    plot_ly(ll, y = ~Dollar_Price, color = ~Sub_Category, type = "box")
    
  })
  
  output$parawordfreq= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Drug paraphernalia")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > 50) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    
  })
  
  
  output$parawordcloud=  renderPlot({  
    ll=dark_net_90() %>%
      filter(Category=="Drug paraphernalia")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
 ############################################################################
  
  
  output$byforgersubcat=renderPlotly({
    ss= dark_net %>%
      filter(Category=='Forgeries') %>%
      group_by(Sub_Category) %>%
      summarise(numbertrans=n()) %>%
      collect
    
    plot_ly(ss, x = ~Sub_Category, y = ~ss$numbertrans,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "subcategories"),
             yaxis = list(title = "Number of listings"))
    
    
  })
  
  output$byforger=renderTable({
    outtable= dark_net_dollar() %>%
      filter(Category=="Forgeries" ) %>%
      group_by(Sub_Category) %>%
      summarise(Minimum=min(Dollar_Price) , Maximum= max(Dollar_Price), Mean=mean(Dollar_Price), Median=median(Dollar_Price) ) %>%
      collect
  })
  
  output$summaryforger = renderPrint({
    ll=dark_net_90() %>%
      filter(Category=="Forgeries")
    
    summary(ll$Dollar_Price)
    
  })
  
  output$subcatpriceforger = renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Forgeries")
    
    
    ggplot(ll, aes(x = ll$Dollar_Price)) + 
      geom_density(aes(fill = ll$Sub_Category)) + 
      facet_grid(~ll$Sub_Category) +
      facet_wrap(~ll$Sub_Category, scales="free", ncol=2) +  
      ggtitle("Kernel density estimate of Prices by category")   
    
    
  })
  
  output$subcatboxplotforger = renderPlotly({
    ll=dark_net_90() %>%
      filter(Category=="Forgeries")
    
    plot_ly(ll, y = ~Dollar_Price, color = ~Sub_Category, type = "box")
    
  })
  
 
  output$forgerwordfreq= renderPlot({
    ll=dark_net_90() %>%
      filter(Category=="Forgeries")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word, sort = TRUE) %>%
      filter(n > 100) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col() +
      xlab(NULL) +
      coord_flip()
    
  })
  
  
  output$forgerwordcloud=  renderPlot({  
    ll=dark_net_90() %>%
      filter(Category=="Forgeries")
    
    f=data_frame( text=ll$Item_Description)
    f=f %>%  unnest_tokens(word , text )
    
    f %>%
      anti_join(stop_words) %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
 
  
   }

# Run the application 
shinyApp(ui = ui, server = server)

