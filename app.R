library(tidyverse)
library(httr)
library(rvest)
library(shiny)
library(shinyjs)
library(staplr)

# download_pdf function
# example: download_pdf("chr1:176696608-176697356", "test.pdf")
download_pdf <- function(session, url, outname) {
  
  # receive pdf link
  pdf_link <- jump_to(session, url) %>% 
    read_html() %>%
    html_nodes("#pdfLink") %>%
    html_attr("href") %>%
    str_replace(., "..", "https://genome.ucsc.edu/") %>% 
    jump_to(session, .) %>% 
    read_html() %>%
    html_node(xpath = "/html/body/div[4]/ul[1]/li[1]/a") %>%
    html_attr("href") %>%
    str_replace(., "..", "https://genome.ucsc.edu")
  
  # pdf_link
  download.file(pdf_link, destfile = outname)
}

# read bed file, add coords and output name
read_bed <- function(bedfile_path, pdfdir){
  # read BED file
  bed <- read_tsv(bedfile_path, col_names = F) %>% 
    select(chr=X1, start=X2, end=X3, label=X4) %>% 
    rowwise() %>% 
    mutate(coords = paste0(chr,":",start,"-",end),
           outname = paste0(pdfdir,paste(label, chr, start, end, sep = "_", collapse = ""), ".pdf")) %>% 
    select(coords, outname)
  bed
}

# initialize session
init <- function(login, password, session_name, db) {

  sessionUrl <- paste0("https://genome.ucsc.edu/s/", login,"/", session_name)
  
  main_url <- "https://genome.ucsc.edu/cgi-bin/hgTracks?db=mm9&lastVirtModeType=default&lastVirtModeExtraState=&virtModeType=default&virtMode=0&nonVirtPosition=&position=chr7%3A26664573%2D26693533&hgsid=966765435_KMLSgA0F83EB9IJZmt7zQ5uPhnOr"  
  main_url <- str_replace(main_url,"mm9",db)
  main_url <- str_extract(main_url, "(?<=^)(.+)(?<=position=)")
  
  loginUrl <- "https://genome.ucsc.edu/cgi-bin/hgLogin?hgLogin.do.displayLoginPage=1"

  # create session
  session <- html_session(loginUrl)
  
  # login to the UCSC server
  form <- html_form(session)[[1]] %>% 
    set_values(., hgLogin_userName = login, hgLogin_password = password) %>% 
    submit_form(session, .)
  
  # go to main screen with all sessions 
  tmp <- jump_to(session, URLencode(sessionUrl))
  
  list(session=session, main_url=main_url)
}

########### UI and server part

key_available <- TRUE


ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      #users {
         color: red;
      }
   "))),
  
  useShinyjs(),
  tags$h4("UCSC download pdf page"),
  textOutput(outputId = "users"),
  tags$p("This tool allows you to extract all pdfs associated with coordinates in BED file"),
  # tags$a(href="link to github", "Sources of this tool"),
  textInput(inputId = "session", label = "Session name", value = ""),
  textInput(inputId = "login", label = "Login", value = ""),
  passwordInput(inputId = "password", label= "Password",value = ""),
  textInput(inputId = "db", label = "Database", value = "mm9"),
  fileInput(inputId = "bedfile", label = "Bed file"),
  actionButton(inputId = "go", label = "Extract pdfs"),
  downloadButton(outputId = "downloadData", "Download combined pdf")
)

# Server part
server <- function(input, output, session) {
  
  have_key <- FALSE
  message <- ""
  buttonMessage <- "Extract pdfs"
  
  # hide download pdf button
  shinyjs::toggle("downloadData")
  
  # disable extract pdf button by default
  shinyjs::disable("go")
  
  # semaphore: when session ended release the key
  onSessionEnded(function() key_available <<- TRUE)
  
  # check every 1 sec if key is available, and hold it if it is available
  observe({
    invalidateLater(1000)
    
    if(key_available){
      key_available <<- FALSE
      have_key <<- TRUE
    }
    if (!have_key) {
      message <<- "Wait please. Another user is currently active"
      buttonMessage <<- "Wait please..."
      shinyjs::disable("go")
    } else {
      shinyjs::enable("go")
      message <<- ""
      buttonMessage <<- "Extract pdf"
    }
    output$users <- renderText(message)
    updateActionButton(session, inputId = "go", label = buttonMessage)
  })
  
  
  
  observeEvent(input$go,{
    
    # disable pdf download button
    shinyjs::hide("downloadData")
    
    # disable download button
    shinyjs::toggle("go")
    
    # cleaning up previous run if the number of files is more then 30
    pdfs <- list.files(pattern = "*.pdf")
    if (length(pdfs) > 30) {unlink(pdfs)}
    
    # tmp dir for pdf
    pdfdir <- tempfile("tmp", tmpdir = "./")
    if (!dir.exists(pdfdir)) {
      dir.create(pdfdir)  
    }
    
    # create name for combined pdf ex. tmp111111_combined.pdf
    combined_name <- paste0(pdfdir, "_combined.pdf")
    
    # load bed file
    bed <- read_bed(input$bedfile$datapath, pdfdir = paste0(pdfdir,"/"))
    withProgress(message = 'Downloading files', value = 0, {
      
      setProgress(detail = "Init session")
      # init session
      init_params <- init(input$login, input$password, input$session, input$db)
      
      # Number of times we'll go through the loop
      n <- nrow(bed)
      
      for (i in 1:n) {
        # Increment the progress bar, and update the detail text.
        incProgress(1/n, detail = paste("Doing file", i))
        
        correct_url <- paste0(init_params$main_url, as.character(bed[i, 1]))
        download_pdf(init_params$session, correct_url, as.character(bed[i, 2]))
      }
      setProgress(detail = "Combine pdf files to one")
      
      # combine all files
      staplr::staple_pdf(input_directory = pdfdir, output_filepath = combined_name)  
      
      # remove directory with pdf files
      unlink(pdfdir, recursive = TRUE)
      
      # show link for combined pdf download
      shinyjs::show("downloadData")
      shinyjs::toggle("go")
    })
    
    # create link
    output$downloadData <- downloadHandler(
      filename = combined_name,
      content = function(file) {
        file.copy(combined_name, file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)

