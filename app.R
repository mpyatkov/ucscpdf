library(tidyverse)
library(httr)
library(rvest)
library(shiny)
library(shinyjs)
library(staplr)
library(readxl)
library(tools)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)

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

## returns new (zoomed) coordinates of fragment
calculate_zoom_factor <- function(start, end, zoom){

  frag_len <-  floor((end-start)/2)
  mid_point <-  start+frag_len
  new_start <- mid_point - zoom*frag_len
  new_end <- mid_point + zoom*frag_len
  
  list(start = floor(new_start),
       end = floor(new_end))
}

## read and save bed/xls as table
export_table_as_pdf <- function(file_path, outdir, add_annotations = TRUE){
  ## for bed file without colnames
  ## for xlsx should be header provided

  ## landscape mode
  OUTPUT_HEIGHT <- 8.5
  OUTPUT_WIDTH <- 11
  CHUNK_SIZE <- 30
  
  ext <- tools::file_ext(file_path)
  file_data <- NULL
  
  if (ext == "xlsx" || ext == "xls") {
    file_data <- read_excel(file_path, col_names = T) 
  } else {
    file_data <- read_tsv(file_path, col_names = F) 
  }
  
  ## if annotations are not required make portrait mode
  ## change default number of chunks
  ## show only 3 first column
  if(!add_annotations){
    file_data <- file_data %>% 
      select(chr = 1, start = 2, end = 3)
    
    ## portrait mode
    OUTPUT_HEIGHT <- 11
    OUTPUT_WIDTH <- 8.5
    CHUNK_SIZE <- 40
  }
  
  ## split data.frame if it has more then 40 rows
  
  n <- nrow(file_data)
  r <- rep(1:ceiling(n/CHUNK_SIZE),each=CHUNK_SIZE)[1:n]
  dlist <- split(file_data,r)
  
  map(dlist, function(chunk_table){
    print(chunk_table)
    cowplot::plot_grid(ggtexttable(chunk_table, rows = NULL, theme = ttheme("minimal", base_size = 8)))
  }) %>% 
    marrangeGrob(nrow =1, ncol=1) %>% 
    ggsave(str_c(outdir, "/00000_coordinates.pdf"), plot = ., width = OUTPUT_WIDTH, height = OUTPUT_HEIGHT)
  
}

## read bed/xls/xlsx files
read_data <- function(file_path, pdfdir, zoom){
  ext <- tools::file_ext(file_path)
  file_data <- NULL
  
  if (ext == "xlsx" || ext == "xls") {
    file_data <- read_excel(file_path, col_names = T) %>% 
      select(chr = 1, start = 2, end = 3)
  } else {
      file_data <- read_tsv(file_path, col_names = F) %>% 
      select(chr = 1, start = 2, end = 3)  
  }
  
  file_data <- pmap_dfr(file_data, function(chr, start, end){
    new_coords <- calculate_zoom_factor(start, end, zoom)
    tibble(chr = chr, start = new_coords$start, end = new_coords$end)
  }) %>% 
    mutate(id = row_number()) %>% 
    rowwise() %>% 
    mutate(coords = paste0(chr,":",start,"-",end),
           outname = paste0(pdfdir,
                            paste(str_pad(id,width = 3, pad = "0"), 
                                  chr, 
                                  start, 
                                  end, 
                                  sep = "_", collapse = ""), 
                            ".pdf")) %>% 
    select(coords, outname)
  file_data
}

# # read bed file, add coords and output name
# read_bed <- function(bedfile_path, pdfdir, zoom){
#   # read BED file
#   bed <- read_tsv(bedfile_path, col_names = F) %>% 
#     select(chr=X1, start=X2, end=X3) %>% 
#     rowwise() %>% 
#     mutate(coords = paste0(chr,":",start,"-",end),
#            outname = paste0(pdfdir,paste(chr, start, end, sep = "_", collapse = ""), ".pdf")) %>% 
#     select(coords, outname)
#   bed
# }

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
  tags$p("This tool allows you to extract all pdfs associated with coordinates in BED/XLSX file"),
  # tags$a(href="link to github", "Sources of this tool"),
  textInput(inputId = "login", label = "Login", value = ""),
  passwordInput(inputId = "password", label= "Password",value = ""),
  textInput(inputId = "session", label = "Session name", value = ""),
  textInput(inputId = "db", label = "Database", value = "mm9"),
  selectInput("zoom", "Zoom out:",
              c("No zoom" = "1",
                "1.5X" = "1.5",
                "3X" = "3",
                "10X" = "10",
                "100X" = "100")),
  checkboxInput(inputId = "need_annotations", "Include additional columns from xls/bed files to the pdf", value = TRUE),
  tags$a("example of input file",href="example.xlsx"),
  fileInput(inputId = "bedfile", label = "bed (without header) / xlsx (with header!) files", multiple = TRUE),
  actionButton(inputId = "go", label = "Extract pdfs"),
  downloadButton(outputId = "downloadData", "Download zip archive")
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
    pdfs <- list.files(pattern = "*.zip")
    if (length(pdfs) > 5) {unlink(pdfs)}
    
    # resulted directory
    result_dir <- str_c(Sys.time() %>% str_replace_all(.," |:","_"),"_UCSC")
    if (!dir.exists(result_dir)) {
      dir.create(result_dir)  
    }
    
    ## walk through all bed/xls files
    pwalk(input$bedfile, function(name,size,type,datapath){

      # tmp dir for pdf
      pdfdir <- tempfile("tmp", tmpdir = "./")
      if (!dir.exists(pdfdir)) {
        dir.create(pdfdir)  
      }
      
      # create name for combined pdf ex. tmp111111_combined.pdf
      combined_name <- str_c(file_path_sans_ext(name),"_ucsc.pdf")
      
      ## create pdf with table
      export_table_as_pdf(datapath, pdfdir, input$need_annotations)

      # load bed file
      bed <- read_data(datapath, pdfdir = paste0(pdfdir,"/"), as.numeric(input$zoom))
      withProgress(message = 'Downloading files',detail = str_c('Processing ',name), value = 0, {
        
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
        
        # remove tmp* directory with pdf files
        unlink(pdfdir, recursive = TRUE)
        
        ## mv to download directory
        file.copy(from=str_c("./",combined_name), str_c("./",result_dir,"/",combined_name))
        file.remove(combined_name)
      }) ## withProgress
      
    }) ## pwalk

    ## make zip archive
    fnames_for_archive <- list.files(result_dir, full.names = T)
    zip_name <- str_c(result_dir,".zip")
    zip(zip_name, fnames_for_archive)

    ## remove result_dir
    unlink(result_dir, recursive = TRUE) ## activate to remove result_dir
    
    # show link for combined pdf download
    shinyjs::show("downloadData")
    shinyjs::toggle("go")

    # create link
    output$downloadData <- downloadHandler(
      filename = zip_name,
      content = function(file) {
        file.copy(zip_name, file)
      }
    )
  })
}

shinyApp(ui = ui, server = server)
