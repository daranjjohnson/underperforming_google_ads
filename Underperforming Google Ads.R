# Global ####

# Set the locale, otherwise, an error can be thrown.
Sys.setlocale('LC_ALL','C') 

# Use pacman to load libraries to make it easier to download
# if not installed.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny,
               tidyverse,
               shinythemes,
               config,
               pwr,
               RAdwords,
               DT,
               scales)

# Authenicate access - this will open a browser window
# for you to authenicate. Without the Basic Access token
# this will error later, when we try to retrive data.
google_ads_auth <- doAuth(save = T)

# UI ####
ui <- fluidPage(theme = shinytheme("lumen"),
                
                titlePanel("Google Ads: Under Performing Ads"),
                
                fluidRow(
                        
                        column(3,
                               dateInput("begin_date_select",
                                         "Ad Begin Date:",
                                         min = Sys.Date() - 180,
                                         max = Sys.Date(),
                                         value = Sys.Date()-90),
                               
                               dateInput("end_date_select",
                                         "Ad End Date:",
                                         min = Sys.Date() - 180,
                                         max = Sys.Date(),
                                         value = Sys.Date()),
                               
                               selectInput('conv_category', 'Evaluate by CTR or Conv. Rate (select one):',
                                           c("Pick one" = "",
                                             "CTR" = "ctr",
                                             "Conversion Rate" = "conv_rate")),
                               
                               actionButton("goButton", "Calculate")
                        ),
                        
                        column(9,
                               
                               
                               # A list of the poor performing ads.
                               div(DT::dataTableOutput("ads_table", width = "100%", height = "auto"), 
                                   style = "font-size:80%")
                               
                        )
                        
                )
                
)

# Server ####
server <- shinyServer(function(input, output) {
        
        
                
        # This fuction will do all the work on the ads 
        # and return the data to the calling function. 
        # Get Ads Data ####  
        ad_data <- function(begin_date, 
                            end_date, 
                            account_id,
                            conv_category) {

                # We're going to use the AD_PERFORMANCE_REPORT
                # to retrieve the data from Google Ads.
                # We want to make sure we're getting the raw data - 
                # CTR and Coversion Rate would be useless here since 
                # we need the raw numbers to feed into our model.
                body <- statement(select = c('CampaignName',
                                             'CampaignStatus',
                                             'AdGroupName',
                                             'AdGroupStatus',
                                             'Id',
                                             'Status',
                                             'Impressions', 
                                             'Clicks', 
                                             'Conversions'),
                                  report = "AD_PERFORMANCE_REPORT",
                                  start = begin_date,
                                  end = end_date)
                
                # Query Adwords API and load data as dataframe.
                # Use Adwords Account Id (MCC Id will not work)
                get_data <- getData(clientCustomerId = account_id, 
                                    google_auth = google_ads_auth,
                                    statement = body)
                
                # Now that we have the raw data, we're going to do a few things to it:
                # 1. rename some of the columns to something more R like.
                # 2. filter for those ads that have over 100 impressions (larger sample size),
                #    remove adgroup and ad_id that equal '(not set)',
                #    finally, evaluate only ads that are enabled, in adgroups that are enabled
                #    from campaigns that are enabled.
                # 3. calcuate CTR, Conversion Rate, and add two new columns:
                #    ctr_update and conv_rate_update.
                # 4. Select only those columns needed going forward. 
                #    For example, we no longer need Campaignstate.
                ads_data <- get_data %>% 
                        dplyr::rename(campaign = Campaign,
                                      adgroup = Adgroup, 
                                      ad_id = AdID,
                                      clicks = Clicks,
                                      impressions = Impressions,
                                      transactions = Conversions) %>%
                        dplyr::filter(impressions > 100,
                                      adgroup != '(not set)',
                                      ad_id != '(not set)',
                                      Campaignstate == 'enabled',
                                      Adgroupstate == 'enabled',
                                      Adstate == 'enabled') %>%
                        dplyr::mutate(ctr = round(clicks/impressions, 5),
                                      conv_rate = round(transactions/impressions, 5),
                                      ctr_update = FALSE,
                                      conv_rate_update = FALSE) %>%
                        dplyr::select(campaign,
                                      adgroup,
                                      ad_id,
                                      clicks,
                                      impressions,
                                      transactions,
                                      ctr,
                                      conv_rate,
                                      ctr_update,
                                      conv_rate_update)
                
                # At the beginning we passed in conv_category to the fuction.
                # conv_category can be one of two values: 'ctr' for CTR or 
                # conv_rate for Conversion Rate.
                # Calculate output for one or the other.
                if(conv_category == 'ctr'){
                        
                        # # Calculate for CTR.
                        ads_data <- ads_data %>% arrange(campaign, adgroup, desc(ctr))
                        
                        # We'll start with the first ad of the first adgroup.
                        # This will be the ad with the best CTR in the adgroup.
                        # We need to assign the ad group.
                        last_adgroup <- ads_data[1,'adgroup']
                        top_ad <- 1
                        
                         # i will equal the next ad.
                        i <- 2
                        
                        # We'll loop through all the ads.
                        while(i <= nrow(ads_data)){
                                
                                # Check if this is a new adgroup - if so,
                                # this is the best performing ad in the set - 
                                # because we sorted it that way -
                                # skip to the next ad, after assigning 
                                # last_adgroup to the current adgroup and 
                                # assigning top_ad to the current ad.
                                if(last_adgroup != ads_data[i,'adgroup']){
                                        
                                        last_adgroup = ads_data[i,'adgroup']
                                        top_ad <- i
                                        i <- i + 1
                                        next
                                }
                                
                                # The following is the sample size (# of impressions) and
                                # the # of clicks from the top CTR ad in the ad group.
                                a_sample_size <- as.numeric(ads_data[top_ad,'impressions'])
                                a_comps <- as.numeric(ads_data[top_ad,'clicks'])
                                
                                # The following is the sample size (# of impressions) and
                                # the # of clicks from the current ad in the ad group.
                                b_sample_size <- as.numeric(ads_data[i,'impressions'])
                                b_comps <- as.numeric(ads_data[i,'clicks'])
                                
                                # We're going to use the prop.test() and use the current
                                # ad as the NULL or A in an A/B test. We're checking to 
                                # see if the top ctr ad is statistically significantly 
                                # greater than the A (current ad). If it is, we'll signal to change 
                                # the current ad.  
                                # We'll use a 0.13 as a p-value as the value for change.
                                # Why 0.13 - it's a prime and it seems resonable.
                                myProp <- prop.test(x = c (a_comps, b_comps), 
                                                    n = c (a_sample_size, b_sample_size), 
                                                    alternative = 'greater')
                                
                                # Check to make sure you have a p-value.
                                if(is.nan(myProp$p.value)){
                                        
                                        i <- i + 1
                                        next
                                }
                                
                                # If the p-value is less than 0.13,
                                # update ctr_update to TRUE. 
                                if(myProp$p.value < 0.13) {
                                        
                                        ads_data[i, 'ctr_update'] <- TRUE
                                        
                                }
                                
                                 # Move to the next row.
                                i <- i + 1
                                
                        }
                        
                        # Output only the ads that have ctr_update == to TRUE.
                        output <- ads_data %>% dplyr::filter(ctr_update == TRUE) %>%
                                dplyr::select(campaign,
                                              adgroup,
                                              ad_id,
                                              impressions,
                                              clicks,
                                              ctr) %>%
                                dplyr::mutate(ctr = percent(ctr)) %>%
                                dplyr::arrange(campaign,
                                               adgroup,
                                               ad_id)
                        
                        
                } else {
                        
                        # In the alternate - this is for conversion rate - so order by 
                        # campaign, adgroup, and conv_rate.
                        ads_data <- ads_data %>% arrange(campaign, adgroup, desc(conv_rate))
                        
                        # We'll start with the first ad of the first adgroup.
                        # This will be the ad with the best CTR in the adgroup.
                        # We need to assign the ad group.
                        last_adgroup <- ads_data[1,'adgroup']
                        top_ad <- 1
                        
                        # i will equal the next ad.
                        i <- 2
                        
                        # We'll loop through all the ads.  
                        while(i <= nrow(ads_data)){
                                
                                # Check if this is a new adgroup - if so,
                                # this is the best performing ad in the set - 
                                # because we sorted it that way -
                                # skip to the next ad, after assigning 
                                # last_adgroup to the current adgroup and 
                                # assigning top_ad to the current ad.
                                if(last_adgroup != ads_data[i,'adgroup']){
                                        
                                        last_adgroup = ads_data[i,'adgroup']
                                        top_ad <- i
                                        i <- i + 1
                                        next
                                }
                                
                                # The following is the sample size (# of impressions) and
                                # the # of transactions from the top converting ad in the ad group.
                                a_sample_size <- as.numeric(ads_data[top_ad,'impressions'])
                                a_comps <- as.numeric(ads_data[top_ad,'transactions'])
                                
                                # The following is the sample size (# of impressions) and
                                # the # of transactions from the current ad in the ad group.
                                b_sample_size <- as.numeric(ads_data[i,'impressions'])
                                b_comps <- as.numeric(ads_data[i,'transactions'])
                                
                                # We're going to use the prop.test() and use the current
                                # ad as the NULL or A in an A/B test. We're checking to 
                                # see if the top converting ad is statistically significantly 
                                # greater than the A (current ad). If it is, we'll signal to change 
                                # the current ad.  
                                # We'll use a 0.13 as a p-value as the value for change.
                                # Why 0.13 - it's a prime and it seems resonable.
                                myProp <- prop.test(x = c (a_comps, b_comps), 
                                                    n = c (a_sample_size, b_sample_size), 
                                                    alternative = 'greater')
                                
                                # Check to make sure you have a p-value.
                                if(is.nan(myProp$p.value)){
                                        
                                        i <- i + 1
                                        next
                                }
                                
                                # If the p-value is less than 0.13,
                                # update conv_rate_update to TRUE. 
                                if(myProp$p.value < 0.13) {
                                        
                                        ads_data[i, 'conv_rate_update'] <- TRUE
                                        
                                }
                                
                                # Move to the next row.
                                i <- i + 1
                                
                        }
                        
                        # Output only the ads that have conv_rate_update == to TRUE.
                        output <- ads_data %>% dplyr::filter(conv_rate_update == TRUE) %>%
                                dplyr::select(campaign,
                                              adgroup,
                                              ad_id,
                                              impressions,
                                              transactions,
                                              conv_rate) %>%
                                dplyr::mutate(conv_rate = percent(conv_rate)) %>%
                                dplyr::arrange(campaign,
                                               adgroup,
                                               ad_id)
                        
                }
                
                # Return the dataset.
                return(output)
        }
        
        
        
        # This fuction will do all the work on the ads 
        # and return the data to the calling fuction.      
        get_ad_data <- reactive({
                
                # If the actionButton ("goButton") 
                # was not pressed, exit the function.
                if(input$goButton == 0){return()}
                
                # Isolate and assign the inputs.
                begin_date <- isolate(input$begin_date_select)
                end_date <- isolate(input$end_date_select)
                conv_category <- isolate(input$conv_category)
                
                # This is your Google Ads Account ID.
                # You can hard-code it here, or use a 
                # dropdown in the UI, as well as passing multiple
                # which you can loop through by calling the 
                # ad_data() function and combining the dataframes
                # with rbind().
                account_id <- "XXX-XXX-XXXX"
                
                # Validate the dates and conv_category. 
                validate_text <- paste('Woops! Your begin date is ', begin_date,
                                       ' and your end date is ', end_date, 
                                       '. I think you see the issue ;-) 
                                        Just adjust the dates and re-run. :-)',
                                       sep = '')
                
                validate(
                        
                        need(begin_date < end_date, validate_text)
                        
                )
                
                validate(
                        
                        need(conv_category != '', "Opps! Select an Evaluation Category :-)")
                        
                )
                
                # Call the ad_data function.
                ad_data <- ad_data(begin_date, 
                                   end_date, 
                                   account_id, 
                                   conv_category)  
                
                 # Return ad_data dataframe to the caller.
                return(ad_data)
                
        })
        
        # Output Data Table ####
        output$ads_table <- DT::renderDataTable({
                
                # If the goButton hasn't been pressed, 
                # don't make a table.
                if(input$goButton == 0){return()}
                
                # We're just going to assign 
                # the data returned from the 
                # reactive function to a new variable.
                # The main reason I do this, is because
                # later it can be easier to troubleshoot.
                data_rtn <- get_ad_data()
                
                # Then simply pass the table 
                # to the datatable() function.
                datatable(data_rtn)
        })    
        
}) # Close the opening shinyServer(function(input, output) {

shinyApp(ui = ui, server = server)
