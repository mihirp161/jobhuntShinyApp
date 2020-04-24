#Shiny app: Mihir Patel

options(warn = -1)
install_packs <- function(x){
   for( i in x ){
      #require returns TRUE invisibly if it was able to load package
      if( ! require( i , character.only = T ) ){
         #  If package was not able to be loaded then re-install
         install.packages( i , dependencies = T )
         #Load package after installing
         require( i , character.only = T )
      }
   }
}
#Then try/install packages...
install_packs(c("tidyverse" , "shiny" , "shinyWidgets", "shinythemes", "xml2",
                "rvest", "here", "wordcloud", "tm", "treemapify", "RColorBrewer", "ggmap",
                "BiocManager", "directlabels"))

# Un-CRAN packages
# BiocManager::install("EBImage") #this package is wrapped inside an .exe
library(EBImage)


##--------------------------------------------------------------- Scrape the data --------------------------------------------
## this is a procedure to scrap the local indeed.com Data Science jobs within 150 miles radius

#please let me know if you would like to know how I got the start and the end page in indeed
# scrape_results <- function(page_result_start, page_result_end, page_url){
#       # loop that scraps from all pages
# 
#       # squencing the pages based on the result
#       page_results <- seq(from = page_result_start, to = page_result_end, by = 10)
#       
#       # vector to stre all data
#       full_df <- data.frame()
#       
#       for(i in seq_along(page_results)) {
#       
#       first_page_url <- page_url
#       
#       url <- paste0(first_page_url, "&start=", page_results[i])
#       
#       page <- xml2::read_html(url)
#       
#       # Sys.sleep pauses R for three seconds before it resumes
#       # Putting it there avoids error messages such as "Error in open.connection(con, "rb") : Timeout was reached"
#       Sys.sleep(3)
#       
#       # get the job title
#       job_title <- page %>%
#                  rvest::html_nodes("div") %>%
#                  rvest::html_nodes(xpath = '//*[@data-tn-element = "jobTitle"]') %>%
#                  rvest::html_attr("title")
#       
#       
#       # get job location
#       job_location <- page %>%
#                      rvest::html_nodes(".location") %>%
#                      rvest::html_text()
#       
#       # get company name
#       company_name <- page %>%
#                    rvest::html_nodes("span")  %>%
#                    rvest::html_nodes(xpath = '//*[@class="company"]')  %>%
#                    rvest::html_text() %>%
#                    stringi::stri_trim_both()
#       
#       # get job links
#       links <- page %>%
#              rvest::html_nodes("div") %>%
#              rvest::html_nodes(xpath = '//*[@data-tn-element="jobTitle"]') %>%
#              rvest::html_attr("href")
#       
#       
#       job_description <- c()
#       company_rating <- c()
#       
#       # get in to each job page and retrieve all of the job description
#       for(i in seq_along(links)) {
#       
#       url <- paste0("https://www.indeed.com/", links[i])
#       page <- xml2::read_html(url)
#       
#       # get job description
#       job_description[[i]] <- page %>%
#                                  rvest::html_nodes("span")  %>%
#                                  rvest::html_nodes(xpath = '//*[@id="jobDescriptionText"]') %>%
#                                  rvest::html_text() %>%
#                                  stringi::stri_trim_both()
#       if(identical(page %>%
#                    rvest::html_nodes(xpath = "//meta[contains(@itemprop, 'ratingValue')]") %>%
#                    rvest::html_attr("content") %>%
#                    unique(), character(0))
#          )
#       {
#       
#          company_rating[[i]] <- 0.0
#       }
#       else{
#          company_rating[[i]] <- page %>%
#                                  rvest::html_nodes(xpath = "//meta[contains(@itemprop, 'ratingValue')]") %>%
#                                  rvest::html_attr("content") %>%
#                                  unique()
#       }
#       }
#       # store all the information
#       df <- data.frame(job_title, company_name, job_location, job_description,company_rating)
#       full_df <- rbind(full_df, df)
#       
#       }
#    return(full_df)
# }

# CAUTION: Start & End pages will change based on how many pages are there!!
# data_scientist <- scrape_results(page_result_start= 10, page_result_end= 180,page_url= "https://www.indeed.com/jobs?q=data+science&l=Tampa%2C+FL&radius=100")
# readr::write_csv(data_scientist, "data_scientist.csv")

# data_engineer <- scrape_results(page_result_start= 10, page_result_end=190,page_url="https://www.indeed.com/jobs?q=data+engineer&l=Tampa%2C+FL&radius=100")
# readr::write_csv(data_engineer, "data_engineering.csv")

# data_analyst <- scrape_results(page_result_start=10,page_result_end=200,page_url="https://www.indeed.com/jobs?q=data+analyst&l=Tampa%2C+FL&radius=100")
# readr::write_csv(data_analyst, "data_analyst.csv")

#--------------------------------------------------------------- Read in Data -------------------------------------------------------
# read some data
data_scientist <- readr::read_csv("data_scientist.csv")
data_engineer <- readr::read_csv("data_engineering.csv")
data_analyst <- readr::read_csv("data_analyst.csv")

# some cleaning to remove special characters
data_scientist$job_description <- gsub("[\r\n]", "", data_scientist$job_description)
data_analyst$job_description <- gsub("[\r\n]", "", data_analyst$job_description)
data_engineer$job_description <- gsub("[\r\n]", "", data_engineer$job_description)

#combine every type of data professionals into one data frame
all_df <- list(data_analyst, data_engineer, data_scientist)
names(all_df) <- c("data_analyst", "data_engineer", "data_scientist")
all_df <- Map(cbind, all_df, profession_df = names(all_df)) #here we have list of dataframe for each professional

#get the rData file
#save(data_scientist, data_analyst, data_engineer,all_df, file = "patel_final_project_rdata.RData")
#save.image(file = "patel_final_project_rdata.RData") # creating ".RData" in current working directory
#unlink("patel_final_project_rdata.RData")


### MAP (Recommended to leave this commented it takes time)

##api activation (Due to privacy, I have done the mapping with following code, and then just render the image in Shiny)

#api <- "XYZ" #YOUR API KEY HERE!
#ggmap::register_google(key = api)

## unlist all of the dataframe into one 
# all_df_unlisted <- dplyr::bind_rows(all_df)
# all_df_unlisted$profession_df <- as.factor(all_df_unlisted$profession_df)
# 
# all_df_unlisted <- dplyr::mutate(all_df_unlisted,address_complete= paste(company_name,job_location))

##get latitude and longitude for all address and store in code

##apply the geocode function to address clumns

#all_df_unlisted_geo <- lapply(all_df_unlisted$address_complete, ggmap::geocode)

# all_df_unlisted_geo <- data.frame(matrix(unlist(all_df_unlisted_geo), nrow = length(all_df_unlisted_geo), byrow = T), stringsAsFactors = F)
# 
# colnames(all_df_unlisted_geo) <- c("lon", "lat")

##join the previous dataframe to the one with data
#all_df_unlisted <- cbind(all_df_unlisted, all_df_unlisted_geo[!names(all_df_unlisted_geo) %in% names(all_df_unlisted)])

#give missing ratings a generous score of 1.0
#all_df_unlisted$company_rating[is.na(all_df_unlisted$company_rating)] <-  1.0

##readr::write_csv(all_df_unlisted, "all_df_unlisted.csv")

#--------------------------------------------------------------- Shiny App --------------------------------------------------------

#ui.r
ui <- fluidPage(
   #shiny app universal theme
   theme = shinythemes::shinytheme("flatly"), 
   
   #shiny app title
   shiny::titlePanel(div(HTML("<em>Data Science Job Market Analysis</em>"))),
   
   shiny::sidebarLayout(
      
      #left & right panels
      position= c("left", "right"), fluid= T,
      
      # panel widgets
      shiny::sidebarPanel(
         shinyWidgets::chooseSliderSkin("Modern"),
         shinyWidgets::setSliderColor(c("powderblue", "coral"), c(1,2)),
         shiny::sliderInput("width", "Plot Width (%)", min = 1, max = 100, value = 100),
         shiny::sliderInput("height", "Plot Height (px)", min = 1, max = 800, value = 800),
         shiny::uiOutput("filter_plot"),
         shiny::conditionalPanel(
            "input.rd == 'Compare College Majors'",
            sliderInput("sl_filter1", "Filter College Majors", min = 1, max = 45, value = 6)
         ),
         shiny::conditionalPanel(
            "input.rd == 'Compare Core Skills'",
            sliderInput("sl_filter2", "Filter Core Skills", min = 1, max = 25, value = 5)
         ),
         shiny::conditionalPanel(
            "input.rd == 'Compare Fundamental Skills'",
            sliderInput("sl_filter3", "Filter Fundamental Skills", min = 1, max = 30, value = 5)
         ),
         shiny::conditionalPanel(
            "input.rd == 'Compare Soft Skills'",
            sliderInput("sl_filter4", "Filter Soft Skills", min = 1, max = 50, value = 5)
         ),
         shiny::conditionalPanel(
            "input.rd == 'Compare Experience Demand'",
            sliderInput("sl_filter5", "Filter Experience Demand", min = 1, max = 30, value = 6)
         )
      ),
      
      #right side panel where plot is rendered
      shiny::mainPanel(
         shiny::uiOutput("plot")
      )
   )
)

#server.r
server <- function(input, output, session) {
   
   #each radio button serves as the index position. We pick plots based on these
   output$filter_plot<- renderUI({
      shinyWidgets::prettyRadioButtons(inputId = "rd",
                         label = "Select Option", thick = T,
                         choices = c("Compare College Majors",
                                     "Compare Degrees Demand",
                                     "Compare Core Skills",
                                     "Compare Fundamental Skills",
                                     "Compare Soft Skills",
                                     "Compare Experience Demand",
                                     "Examine Common Job Titles",
                                     "Examine Companies",
                                     "Explore the Buzz Words",
                                     "Explore Job Locations"),
                         animation = "pulse", status = "info",
                         selected = "Compare College Majors")
   })
   
   #open the google map image (privacy reasons I couldn't make it interactable)
   img <- shiny::reactive({EBImage::readImage("KEEP_THIS_IN_SAME_DIRECTORY___MAP.png")})
   
   # these are plot renderers, some of these are dynamic if the data allows them to be so
   output$plot1<- shiny::renderPlot({
      
      ### MAJORS
      # get all of the majors
      majors <- all_df %>%
         purrr::map(~dplyr::mutate(., Statistics = grepl("Statistics|statistics", job_description))) %>%
         purrr::map(~dplyr::mutate(., Biostatistics = grepl("Biostatistics|biostatistics|Bio-statistics", job_description))) %>%
         purrr::map(~dplyr::mutate(., Mathematics = grepl("Mathematics|mathematics", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Computer Science` = grepl("Computer Science|computer science", job_description))) %>%
         purrr::map(~dplyr::mutate(., Engineering = grepl("Engineering|engineering", job_description))) %>%
         purrr::map(~dplyr::mutate(., Finance = grepl("Finance|finance", job_description))) %>%
         purrr::map(~dplyr::mutate(., Economics = grepl("Economics|economics", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Information Technology` = grepl("Information Technology|information technology", job_description))) %>%
         purrr::map(~dplyr::mutate(., Physics = grepl("Physics|physics", job_description))) %>%
         purrr::map(~dplyr::select(., profession_df:Physics)) %>%
         purrr::map(~tidyr::gather(., major, key, Statistics:Physics)) %>%
         purrr::map(~dplyr::group_by(., major, profession_df)) %>%
         purrr::map(~dplyr::summarise(., count = sum(key, na.rm = T))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         purrr::map(~dplyr::filter(., proportions > (input$sl_filter1/100))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         do.call(rbind, .)
      
      #plot it
      ggplot2::ggplot(majors, aes(x = profession_df, y = proportions, fill = major)) +
         geom_bar(position = "stack", stat = "identity") +
         geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(proportions*100, 0), "%")),alpha=0.2, color= "slategray4",
                    show.legend= F)+
         theme_bw() +
         labs(title = "Popular majors required for professionals",
              x= "Professional",
              caption= paste0("filter ", input$sl_filter1,"%"))+
         guides(fill=guide_legend(ncol= 7)) +
         theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "slategray4"),
               axis.title.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y = element_blank(),
               legend.position = "top",
               plot.caption = element_text(face= "italic")) +
         scale_x_discrete(labels = c("Data Analyst", "Data Engineer", "Data Scientist")) +
         scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"), name = "Majors: ")
      
   })

   output$plot2<- shiny::renderPlot({
      
      ### DEGREES
      #get the degrees
      degrees <- all_df %>%
         purrr::map(~dplyr::mutate(., Bachelor = stringr::str_detect(job_description, "Bachelor's|Bachelors|BSc|bsc|undergraduate"))) %>%
         purrr::map(~dplyr::mutate(., Master = stringr::str_detect(job_description, "Master's|Masters|MSc|msc|graduate"))) %>%
         purrr::map(~dplyr::mutate(., PhD = stringr::str_detect(job_description, "PhD"))) %>%
         purrr::map(~dplyr::mutate(., master_PhD = stringr::str_detect(job_description, "Master's.*PhD|Masters.*PhD|MSc.*PhD|msc.*phd|graduate.*PhD"))) %>%
         purrr::map(~dplyr::select(., Bachelor, Master, PhD, profession_df)) %>%
         purrr::map(~tidyr::gather(., education, key, Bachelor:PhD)) %>%
         purrr::map(~dplyr::group_by(., education, profession_df)) %>%
         purrr::map(~dplyr::summarise(., count = sum(key, na.rm = T))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), proportions = count / total)) %>%
         do.call(rbind, .)
      
      ggplot2::ggplot(degrees, aes(x= profession_df, y = proportions, fill= education)) +
         geom_bar(position = "stack", stat = "identity") +
         geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(proportions*100, 0), "%")),alpha=0.2, color= "slategray4",
                    show.legend= F)+
         theme_bw() +
         labs(title = "Minimum degrees required among each profession",
              x= "Professional",
              caption = "margin-of-error 1%\n filter NA") +
         theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "slategray4"),
               axis.title.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y = element_blank(),
               legend.position = "top",
               plot.caption = element_text(face= "italic")) +
         scale_x_discrete(labels = c("Data Analyst", "Data Engineer", "Data Scientist")) +
         scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"), labels = c("Bachelor","Master","PhD"), name = "Degrees: ")
      
   })
   
   output$plot3<- shiny::renderPlot({
      ### CORE SKILLS
      # get core skills
      core_skills <- all_df %>%
         purrr::map(~dplyr::mutate(., R = grepl("\\bR\\b|\\br\\b", job_description))) %>%
         purrr::map(~dplyr::mutate(., Python = grepl("Python|python", job_description))) %>%
         purrr::map(~dplyr::mutate(., SQL = grepl("SQL|sql", job_description))) %>%
         purrr::map(~dplyr::mutate(., SAS = grepl("SAS|sas", job_description))) %>%
         purrr::map(~dplyr::mutate(., Hadoop = grepl("Hadoop|hive|\\bmapreduce\\b|\\bhbase\\b", job_description))) %>%
         purrr::map(~dplyr::mutate(., Perl = grepl("Perl", job_description))) %>%
         purrr::map(~dplyr::mutate(., `C++` = grepl("C++|c++", job_description, fixed=T))) %>%
         purrr::map(~dplyr::mutate(., `C#` = grepl("C#|c#", job_description, fixed=T))) %>%
         purrr::map(~dplyr::mutate(., Java = grepl("\\bJava\\b|\\bjava\\b", job_description))) %>%
         purrr::map(~dplyr::mutate(., Scala = grepl("Scala", job_description))) %>%
         purrr::map(~dplyr::mutate(., Tensorflow = grepl("Tensorflow|tensorflow", job_description))) %>%
         purrr::map(~dplyr::mutate(., Javascript = grepl("Javascript|javascript", job_description))) %>%
         purrr::map(~dplyr::mutate(., Spark = grepl("Spark|spark", job_description))) %>%
         purrr::map(~dplyr::mutate(., Git = grepl("Git.*", job_description))) %>%
         purrr::map(~dplyr::mutate(., Bash = grepl("command line|bash|terminal|shell|command-line", job_description))) %>%
         purrr::map(~dplyr::mutate(., `MS Excel` = grepl("\\<Excel\\>", job_description))) %>%
         purrr::map(~dplyr::mutate(., Tableau = grepl("Tableau|tableau", job_description))) %>%
         purrr::map(~dplyr::mutate(., Julia = grepl("Julia|julia", job_description))) %>%
         purrr::map(~dplyr::select(., profession_df:Julia)) %>%
         purrr::map(~tidyr::gather(., skill, key, R:Julia)) %>%
         purrr::map(~dplyr::group_by(., skill, profession_df)) %>%
         purrr::map(~dplyr::summarise(., count = sum(key, na.rm = T))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         purrr::map(~dplyr::filter(., proportions > (input$sl_filter2/100))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         do.call(rbind, .)
      
      colourCount <- length(unique(core_skills$proportions)) # number of levels
      getPalette <- grDevices::colorRampPalette(brewer.pal(9, "Set1")) #get set 1 colors ramp
      
      ggplot2::ggplot(core_skills, aes(x = profession_df, y = proportions, fill = skill)) +
         geom_bar(position = "stack", stat = "identity") +
         geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(proportions*100, 0), "%")),alpha=0.2, color= "slategray4",
                    show.legend= F)+
         theme_bw() +
         labs(title = "Core skills needed in each Professions",
              x= "Professional",
              caption= paste0("filter ", input$sl_filter2,"%"))+
         guides(fill=guide_legend(ncol= 11)) +
         theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "slategray4"),
               axis.title.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y = element_blank(),
               legend.position = "top",
               plot.caption = element_text(face= "italic")) +
         scale_x_discrete(labels = c("Data Analyst", "Data Engineer", "Data Scientist")) +
         scale_fill_manual(values = grDevices::colorRampPalette(RColorBrewer::brewer.pal(12, "Set3"))(colourCount),
                           name = "Core Skills: ") 
      
   })
   
   output$plot4<- shiny::renderPlot({
      ### FUNAMENTAL SKILLS
      # get fundamental skills
      
      fundamental_skills <- all_df %>%
         purrr::map(~dplyr::mutate(., `Machine Learning` = grepl("Machine Learning|machine learning", job_description))) %>%
         purrr::map(~dplyr::mutate(., Probability = grepl("Probability|probability", job_description))) %>%
         purrr::map(~dplyr::mutate(., Programming = grepl("Programming|programming", job_description))) %>%
         purrr::map(~dplyr::mutate(., Statistics = grepl("Statistics|statistics", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Data Structures & Algorithms` = grepl("algorithms|Algorithm|\\bdata structures\\b", job_description))) %>%
         purrr::map(~dplyr::mutate(., Quantitative = grepl("Quantitative|quantitative", job_description))) %>%
         purrr::map(~dplyr::mutate(., Qualitative = grepl("Qualitative|qualitative", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Data Mining` = grepl("Data Mining|data mining", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Text Mining` = grepl("Text Mining|text mining", job_description))) %>%
         purrr::map(~dplyr::mutate(., Modeling = grepl("Modeling|modeling|data modeling|Data Modeling", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Deep Learning` = grepl("Deep Learning|deep learning", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Neural Networks` = grepl("Neural Networks|neural networks", job_description))) %>%
         purrr::map(~dplyr::select(., profession_df:`Neural Networks`)) %>%
         purrr::map(~tidyr::gather(., skill, key, `Machine Learning`:`Neural Networks`)) %>%
         purrr::map(~dplyr::group_by(., skill, profession_df)) %>%
         purrr::map(~dplyr::summarise(., count = sum(key, na.rm = T))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         purrr::map(~dplyr::filter(., proportions > (input$sl_filter3/100))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         do.call(rbind, .)
      
      ggplot2::ggplot(fundamental_skills, aes(x = profession_df, y = proportions, fill = skill)) +
         geom_bar(position = "stack", stat = "identity") +
         geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(proportions*100, 0), "%")),alpha=0.2, color= "slategray4",
                    show.legend= F)+
         theme_bw() +
         labs(title = "Fundamental skills required in each Professions",
              x= "Professional",
              caption= paste0("filter ", input$sl_filter3,"%"))+
         guides(fill=guide_legend(ncol= 9)) +
         theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "slategray4"),
               axis.title.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y = element_blank(),
               legend.position = "top",
               plot.caption = element_text(face= "italic")) +
         scale_x_discrete(labels = c("Data Analyst", "Data Engineer", "Data Scientist")) +
         scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"), name = "Fundamental Skills: ")
   })
   
   output$plot5<- shiny::renderPlot({
      ### SOFT SKILLS
      # extract soft skills
      
      soft_skills <- all_df %>%
         purrr::map(~dplyr::mutate(., Communication = grepl("Communicate|communicate|Communication", job_description))) %>%
         purrr::map(~dplyr::mutate(., Teamwork = grepl("teamwork|Teamwork", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Problem Solving` = grepl("problem solving|Problem Solving", job_description))) %>%
         purrr::map(~dplyr::mutate(., Creative = grepl("Creative|creative", job_description))) %>%
         purrr::map(~dplyr::mutate(., `Detail Oriented` = grepl("Detail Oriented|looking for detail", job_description))) %>%
         purrr::map(~dplyr::mutate(., Articulative = grepl("Articulate|articulate", job_description))) %>%
         purrr::map(~dplyr::select(., profession_df:Articulative)) %>%
         purrr::map(~tidyr::gather(., skill, key, Communication:Articulative)) %>%
         purrr::map(~dplyr::group_by(., skill, profession_df)) %>%
         purrr::map(~dplyr::summarise(., count = sum(key, na.rm = T))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         purrr::map(~dplyr::filter(., proportions > (input$sl_filter4/100))) %>%
         purrr::map(~dplyr::mutate(., total = sum(.$count), 
                                   proportions = count / total)) %>%
         do.call(rbind, .)
      
      ggplot2::ggplot(soft_skills, aes(x = profession_df, y = proportions, fill = skill)) +
         geom_bar(position = "stack", stat = "identity") +
         geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(proportions*100, 0), "%")),alpha=0.2, color= "slategray4",
                    show.legend= F)+
         theme_bw() +
         labs(title = "Soft skills recommended for each professional",
              x= "Professional",
              caption= paste0("filter ", input$sl_filter4,"%"))+
         guides(fill=guide_legend(ncol= 9)) +
         theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "slategray4"),
               axis.title.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y = element_blank(),
               legend.position = "top",
               plot.caption = element_text(face= "italic")) +
         scale_x_discrete(labels = c("Data Analyst", "Data Engineer", "Data Scientist")) +
         scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"),name = "Soft Skills: ")
   })
   
   output$plot6<- shiny::renderPlot({
      ### EXPERIENCE 
      # get experience needed
      
      experiences <- all_df %>%
         purrr::map(~stringr::str_extract(.$job_description, "[0-9]+\\+ years")) %>%
         purrr::map(., as.factor) %>%
         purrr::map(., table) %>%
         purrr::map(~dplyr::tibble(., experiences = as.factor(names(.)), count = as.integer(.))) %>%
         purrr::map(~dplyr::arrange(., desc(count))) %>%
         purrr::map(~dplyr::mutate(., total = sum(count), 
                                   proportions = count / total)) %>%
         purrr::map(~dplyr::filter(., proportions > (input$sl_filter5/100))) %>%
         purrr::map(~dplyr::mutate(., total = sum(count), 
                                   proportions = count / total)) %>%
         Map(cbind, ., df = c("Data Analyst", "Data Engineer", "Data Scientist")) %>%
         do.call(rbind, .)
      
      ggplot2::ggplot(experiences, aes(x = df, y = proportions, fill = experiences)) +
         geom_bar(position = "stack", stat = "identity") +
         geom_label(position = position_stack(vjust = 0.5), aes(label = paste0(round(proportions*100, 0), "%")),alpha=0.2, color= "slategray4",
                    show.legend= F)+
         theme_bw() +
         guides(fill=guide_legend(ncol= 7)) +
         labs(title = "Experience for Each Professional",
              x= "Professional",
              caption= paste0("filter ", input$sl_filter5,"%"))+
         theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "slategray4"),
               axis.title.y = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.y = element_blank(),
               legend.position = "top",
               plot.caption = element_text(face= "italic")) +
         scale_x_discrete(labels = c("Data Analyst", "Data Engineer", "Data Scientist")) +
         scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"),name = "Experience: ")
      
   })
   
   output$plot7<- shiny::renderPlot({
      ### JOB TITLES
      
      #get job titles
      job_titles<- all_df %>%
         purrr::map(~dplyr::group_by(., job_title)) %>%
         purrr::map(~dplyr::summarise(., n = n())) %>%
         purrr::map(~dplyr::arrange(.,desc(n))) %>%
         purrr::map(~dplyr::filter(., n > 5)) %>%
         Map(cbind, ., profession_df = c("Data Analyst", "Data Engineer", "Data Scientist")) %>%
         dplyr::bind_rows()
      
      job_titles$profession_df <- as.factor(job_titles$profession_df)
      
      ggplot2::ggplot(job_titles, aes(area = n,
                                                       fill = profession_df,
                                                       label = job_title,
                                                       subgroup = profession_df,
                                                       subgroup2 = job_title)) +
         geom_treemap() +
         geom_treemap_subgroup_border(colour = "mintcream") +
         geom_treemap_subgroup2_border(colour = "slategray4") +
         geom_treemap_subgroup_text(place = "centre",
                                    grow = T,
                                    alpha = 0.6,
                                    colour = "palevioletred4",
                                    fontface = "italic",
                                    min.size = 0) +
         geom_treemap_text(reflow = T, colour = "mintcream", place = "topleft",
                           grow = T, min.size= 2, alpha= 0.9)+
         theme_bw() +
         labs(title =  paste0("Most popular job titles"))+
         theme(legend.position = "none",
               plot.title = element_text(hjust = 0.5, size = 39, face = "bold", color = "slategray4")) +
         scale_fill_manual(values = c("#66C2A5","gold3","#8DA0CB"))
      
   })
   
   output$plot8<- shiny::renderPlot({
      
      ### COMPANIES
      # split companies and get company job listing
      
      companies<- all_df %>%
         purrr::map(~dplyr::group_by(., company_name)) %>%
         purrr::map(~dplyr::summarise(., n = n())) %>%
         purrr::map(~dplyr::arrange(., desc(n))) %>%
         purrr::map(~dplyr::filter(., n > 5)) %>%
         Map(cbind, ., profession_df = c("Data Analyst", "Data Engineer", "Data Scientist"))%>%
         dplyr::bind_rows()
      
      companies$profession_df <- as.factor(companies$profession_df)
      
      ggplot2::ggplot(companies, aes(area = n,
                                                      fill = profession_df,
                                                      label = company_name,
                                                      subgroup = profession_df,
                                                      subgroup2 = company_name)) +
         geom_treemap() +
         geom_treemap_subgroup_border(colour = "mintcream") +
         geom_treemap_subgroup2_border(colour = "slategray4") +
         geom_treemap_subgroup_text(place = "centre",
                                    grow = T,
                                    alpha = 0.6,
                                    colour = "palevioletred4",
                                    fontface = "italic",
                                    min.size = 0) +
         geom_treemap_text(reflow = T, colour = "mintcream", place = "topleft",
                           grow = T, min.size= 2, alpha= 0.9)+
         theme_bw() +
         labs(title =  paste0("Most job posting companies"))+
         theme(legend.position = "none",
               plot.title = element_text(hjust = 0.5, size = 39, face = "bold", color = "slategray4")) +
         scale_fill_manual(values = c("#66C2A5","gold3","#8DA0CB")) 
   })
   
   output$plot9<- shiny::renderPlot({
      
      ### WORDCLOUD
      # unlist all of the dataframe into one 
      all_df_unlisted <- dplyr::bind_rows(all_df)
      all_df_unlisted$profession_df <- as.factor(all_df_unlisted$profession_df)
      
      # word cloud
      word_cloud <- tm::tm_map(tm::tm_map(tm::VCorpus(tm::VectorSource(all_df_unlisted$job_description)), removePunctuation),
                               PlainTextDocument)
      
      word_cloud <- tm::tm_map(word_cloud, removeWords, c("services", "the", "andor", "ability", "using", "new", "one", "help", "you", 
                                                          "status", "must", "will", "including", "can","data", "this",
                                                          "may", "within", "across", "years",tm::stopwords('english')))
      dtm <- tm::TermDocumentMatrix(word_cloud) 
      matrix <- as.matrix(dtm) 
      words <- sort(rowSums(matrix),decreasing= T) 
      df <- data.frame(word = names(words),freq=words)
      df<- df[!(df$freq < 350), ] #drop the words less than 350 frequency, leading to some crashes if I allow changes
      
      df<- df[!(df$word =="the" | df$word =="our" | df$word =="you" | df$word == "best" | df$word =="need" |
                   df$word =="use" | df$word =="part" | df$word =="make"| df$word =="well"| df$word =="this"), ] 
      
      #render the wordcloud (wanted to do wordcloud2, but htmlwidget was acting weird)
      wordcloud::wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, random.order= F, rot.per=0.35, 
                           colors=brewer.pal(8, "Set2"))
   })
   
   #open the image on the first and "plot" it (not really, we are just opening the image)
   output$raster <- shiny::renderPlot({
      
      #all_df_unlisted <- readr::read_csv("all_df_unlisted.csv")
      
      #tampa <- ggmap::geocode("university of south florida,tampa,fl")
      
      #tampa_map <- ggmap::get_map(location = tampa, zoom = 8)
      
      ## generate map
      
      # ggmap::ggmap(tampa_map) +
      #    geom_point(data = all_df_unlisted, aes(x= lon, y= lat,
      #                                           fill = cut(company_rating, c(-Inf, 2.5, 3.5, Inf))),
      #               size = 2.2, alpha= 0.8,shape=25, colour= "cornflowerblue")+
      #    theme_bw() +
      #    labs(title = "Job locations within 150 miles of Tampa",
      #         x= "Latitude",
      #         y= "Longitude")+
      #    scale_fill_manual(name = "Company Rating: ",
      #                       values = c("(-Inf,2.5]" = "indianred4",
      #                                  "(2.5,3.5]" = "goldenrod3",
      #                                  "(3.5, Inf]" = "lawngreen"),
      #                       labels = c("1.0-2.5", "2.5-3.5", "3.5-5.0"))+
      #    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15, color = "slategray4"),
      #          legend.position = "top")+
      #    ggsave("KEEP_THIS_IN_SAME_DIRECTORY___MAP.png", dpi= 400, width = 8, height = 8, units = "in")
      
      shiny::req(img())
      plot(img(), all= T)
   })
   
   # depending on the radio button, we pick the index, and then we plot render the plots in the dashboard
   
   output$plot <- renderUI({
      
      shiny::req(input$rd) # put this here inorder to stop warnings which thinks if cases are NULL
      
      if(input$rd=="Compare College Majors"){
         
         shiny::plotOutput("plot1", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Compare Degrees Demand"){
         
         shiny::plotOutput("plot2", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Compare Core Skills"){
         
         shiny::plotOutput("plot3", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Compare Fundamental Skills"){
         
         shiny::plotOutput("plot4", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Compare Soft Skills"){
         
         shiny::plotOutput("plot5", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Compare Experience Demand"){
         
         shiny::plotOutput("plot6", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Examine Common Job Titles"){
         
         shiny::plotOutput("plot7", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Examine Companies"){
         
         shiny::plotOutput("plot8", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Explore the Buzz Words"){
         
         shiny::plotOutput("plot9", width = paste0(input$width, "%"), height = input$height)
      }
      else if(input$rd=="Explore Job Locations"){
         
         shiny::plotOutput("raster", width = paste0(input$width, "%"), height = input$height)
      }
   })
   
}

#Run the app
shiny::shinyApp(ui = ui, server = server)