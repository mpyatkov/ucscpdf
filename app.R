library(httr)
library(rvest)
library(shiny)
library(shinyjs)
library(qpdf)
library(readxl)
library(tools)
library(ggpubr)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(stringr)
library(purrr)
library(dplyr)
library(readr)
library(chromote)

# Helper operator for NULL handling
`%||%` <- function(x, y) if (is.null(x)) y else x

## check dependecies for packages
# packrat:::recursivePackageDependencies("ggpubr", ignore = "", lib.loc = .libPaths()[1])
# rsconnect::appDependencies(appDir= "/projectnb/wax-dk/max/src/ucscpdf/")

#' Fetch session names from UCSC Genome Browser
#'
#' Uses Chromote to login to UCSC Genome Browser and fetch the list of saved sessions.
#' Automatically handles Cloudflare CAPTCHA challenge via headless Chrome.
#'
#' @param username UCSC username
#' @param password UCSC password
#' @return List with names (character vector) and chrome (Chromote session), or NULL on error
#' @examples
#' \dontrun{
#'   sessions <- get_session_names("ucsc_login", "ucsc_password")
#' }
get_session_names <- function(username, password) {
  b <- NULL
  
  tryCatch({
    # Create Chromote session
    b <- ChromoteSession$new()
    #b$Network$clearBrowserCache()
    b$Network$clearBrowserCookies()

        # Navigate to login page
    b$go_to("https://genome.ucsc.edu/cgi-bin/hgLogin?hgLogin.do.displayLoginPage=1")
    Sys.sleep(2)
    
    # Submit login form via JavaScript
    login_script <- sprintf(
      "(function() {
        document.querySelector('#userName').value = '%s';
        document.querySelector('#password').value = '%s';
        document.querySelector('[name=\\\"hgLogin.do.displayLogin\\\"]').click();
        return 'submitted';
      })();",
      username, password
    )
    
    b$Runtime$evaluate(login_script)
    
    # Wait for login and Cloudflare challenge (max 15 seconds)
    for (i in 1:15) {
      Sys.sleep(1)
      url_res <- b$Runtime$evaluate("document.location.href")
      url <- url_res$result$value
      # Check if navigated away from login page
      if (!grepl("hgLogin", url) && nchar(url) > 0) {
        break
      }
    }
    
    # Navigate to session management page
    b$go_to("https://genome.ucsc.edu/cgi-bin/hgSession?hgS_doMainPage=1")
    Sys.sleep(3)
    
    # Change DataTables pagination to show 100 sessions (default is 10)
    # The select element is: <select name="sessionTable_length" aria-controls="sessionTable">
    b$Runtime$evaluate("
      (function() {
        var select = document.querySelector('select[name=\"sessionTable_length\"]');
        if (select) {
          select.value = '100';
          // Trigger change event for DataTables to update
          var event = new Event('change', { bubbles: true });
          select.dispatchEvent(event);
        }
        return 'done';
      })();
    ")
    Sys.sleep(2)  # Wait for table to reload with 100 rows
    
    # Get HTML
    html_res <- b$Runtime$evaluate("document.documentElement.outerHTML")
    html <- html_res$result$value
    doc <- read_html(html)
    
    # Find all table rows and extract session names
    rows <- html_nodes(doc, "tr")
    session_names <- character()
    
    for (row in rows) {
      row_text <- html_text(row)
      
      # Look for rows containing session names (links in session table)
      if (nchar(row_text) > 10 && nchar(row_text) < 500) {
        links <- html_nodes(row, "a")
        for (link in links) {
          link_text <- html_text(link)
          # Session names are non-empty and don't contain common UI text
          if (nchar(link_text) > 0 && 
              nchar(link_text) < 100 &&
              !grepl("^(Email|Delete|View|Save|Cancel|Login|Create|Help|Sign out|public|Change password)", link_text)) {
            session_names <- c(session_names, link_text)
          }
        }
      }
    }
    # Return unique session names
    return(list(names = unique(session_names), chrome = b))
    
  }, error = function(e) {
    # Log error for debugging
    message("Error fetching sessions: ", e$message)
    # Clean up and return NULL
    if (!is.null(b)) {
      tryCatch(b$close(), error = function(e) {})
    }
    return(NULL)
  })
}

# init_chromote function - creates authenticated Chromote session for PDF download
# example: init_chromote("ucsc_login", "password", "my_session", "hg38")
init_chromote <- function(login, password, session_name, db) {
  b <- NULL
  
  tryCatch({
    # Create Chromote session
    b <- ChromoteSession$new()
    b$Network$clearBrowserCookies()
    
    # Navigate to login page
    b$go_to("https://genome.ucsc.edu/cgi-bin/hgLogin?hgLogin.do.displayLoginPage=1")
    Sys.sleep(2)
    
    # Submit login form via JavaScript
    login_script <- sprintf(
      "(function() {
        document.querySelector('#userName').value = '%s';
        document.querySelector('#password').value = '%s';
        document.querySelector('[name=\\\"hgLogin.do.displayLogin\\\"]').click();
        return 'submitted';
      })();",
      login, password
    )
    
    b$Runtime$evaluate(login_script)
    
    # Wait for login and Cloudflare challenge (max 15 seconds)
    for (i in 1:15) {
      Sys.sleep(1)
      url_res <- b$Runtime$evaluate("document.location.href")
      url <- url_res$result$value
      if (!grepl("hgLogin", url) && nchar(url) > 0) {
        break
      }
    }
    
    # Navigate to session URL
    sessionUrl <- paste0("https://genome.ucsc.edu/s/", login, "/", session_name)
    b$go_to(sessionUrl)
    Sys.sleep(3)
    
    # Build main_url for PDF downloads
    main_url <- paste0("https://genome.ucsc.edu/cgi-bin/hgTracks?db=", db, "&position=")
    
    list(chrome = b, main_url = main_url)
    
  }, error = function(e) {
    message("Error initializing Chromote session: ", e$message)
    return(NULL)
  })
}

# download_pdf_chromote function - download PDF using Chromote session
# example: download_pdf_chromote(chrome_session, "chr1:176696608-176697356", "test.pdf")
download_pdf_chromote <- function(chrome, url, outname) {
  
  tryCatch({
    # Navigate to URL
    chrome$go_to(url)
    Sys.sleep(3)
    
    # Get page source using JavaScript
    html_res <- chrome$Runtime$evaluate("document.documentElement.outerHTML")
    html <- html_res$result$value
    doc <- read_html(html)
    
    # Find PDF link
    pdflink <- doc %>% 
      html_node("#pdfLink") %>% 
      html_attr("href")
    
    if (is.na(pdflink)) {
      message("PDF link not found at: ", url)
      return(NULL)
    }
    
    # Convert relative URL to absolute for pdflink (e.g., "../cgi-bin/..." -> "https://genome.ucsc.edu/cgi-bin/...")
    if (grepl("^\\.\\.", pdflink)) {
      pdflink <- sub("^\\.\\.", "https://genome.ucsc.edu/", pdflink)
    }
    
    # Navigate to PDF download page
    chrome$go_to(pdflink)
    Sys.sleep(3)
    
    # Get HTML and find download link
    html_res <- chrome$Runtime$evaluate("document.documentElement.outerHTML")
    html <- html_res$result$value
    doc <- read_html(html)
    
    # Find all links containing 'pdf'
    all_links <- html_attr(html_nodes(doc, "a"), "href")
    download_links <- all_links[grepl("pdf", all_links, ignore.case = TRUE)]
    
    if (length(download_links) == 0) {
      message("No PDF download links found")
      return(NULL)
    }
    
    # Get first PDF link
    download_link <- download_links[1]
    
    # Convert relative URL to absolute (e.g., "../trash/..." -> "https://genome.ucsc.edu/trash/...")
    if (grepl("^\\.\\.", download_link)) {
      download_link <- sub("^\\.\\.", "https://genome.ucsc.edu/", download_link)
    }
    
    # Download PDF using httr
    response <- GET(download_link, write_disk(outname, overwrite = TRUE))
    
    if (response$status_code == 200) {
      message("Downloaded: ", outname)
    } else {
      message("Failed to download: ", response$status_code)
    }
    
  }, error = function(e) {
    message("Error downloading PDF: ", e$message)
    return(NULL)
  })
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
export_table_as_pdf <- function(file_path, outdir, title_text = "", add_annotations = TRUE){
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
    tab <- ggtexttable(chunk_table, rows = NULL, 
                       theme = ttheme(base_size = 8, padding = unit(c(15, 3), "mm"))) %>% 
      tab_add_title(text = title_text, face = "bold", size = 8, padding = unit(1, "line"))
    cowplot::plot_grid(tab)
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
  
  # Two-column layout: login credentials | session selection
  fluidRow(
    column(
      width = 6,
      tags$h5("UCSC Credentials"),
      textInput(inputId = "login", label = "Login", value = "", placeholder = "Enter UCSC username"),
      passwordInput(inputId = "password", label = "Password", value = "", placeholder = "Enter UCSC password"),
      textInput(inputId = "db", label = "Database", value = "mm9", placeholder = "e.g., mm9, hg38"),
      selectInput("zoom", "Zoom out:",
                  c("3X" = "3",
                    "1.5X" = "1.5",
                    "10X" = "10",
                    "100X" = "100")),
      checkboxInput(inputId = "need_annotations", "Include additional columns from xls/bed files to the pdf", value = TRUE)
    ),
    column(
      width = 6,
      tags$h5("Session Selection"),
      fluidRow(
        column(
          width = 8,
          selectInput(
            inputId = "session", 
            label = "Session name", 
            choices = NULL,  # Populated by refresh
            selectize = TRUE,  # Allows typing + selection
            multiple = FALSE
          )
        ),
        column(
          width = 4,
          actionButton(
            inputId = "refresh_sessions", 
            label = "", 
            icon = icon("refresh"),
            title = "Refresh Session List"
          ),
          tags$p(" ", style = "margin-bottom: 10px;"),
          textOutput(outputId = "session_count", inline = TRUE)
        )
      ),
      tags$p("Or enter session name manually above", style = "font-size: 0.85em; color: #666;")
    )
  ),
  
  tags$a("example of input file", href = "example.xlsx"),
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

  # Session count display initialization
  output$session_count <- renderText({
    "Click 'Refresh' to load sessions"
  })

  # Enable/disable refresh button based on login/password
  observe({
    login_valid <- nchar(trimws(input$login %||% "")) > 0
    password_valid <- nchar(trimws(input$password %||% "")) > 0
    
    if (login_valid && password_valid) {
      shinyjs::enable("refresh_sessions")
    } else {
      shinyjs::disable("refresh_sessions")
    }
  })

  # Fetch sessions when refresh button is clicked
  observeEvent(input$refresh_sessions, {
    # Disable refresh button during fetch (debouncing)
    shinyjs::disable("refresh_sessions")
    
    withProgress(message = "Loading sessions...", value = 1, {
      # Fetch sessions (returns list with names and chrome)
      sessions_result <- get_session_names(input$login, input$password)
      
      if (is.null(sessions_result)) {
        # Error occurred - show message
        showNotification(
          "Unable to load sessions. Please check your credentials and try again.",
          type = "error",
          duration = 5
        )
        message("Session fetch failed - check credentials or network")
        # Reset session count display
        output$session_count <- renderText({
          "Error loading sessions"
        })
      } else if (length(sessions_result$names) == 0) {
        # No sessions found
        updateSelectInput(session, "session", choices = character(0))
        output$session_count <- renderText({
          "0 sessions found"
        })
        showNotification(
          "No sessions found. You can still enter a session name manually.",
          type = "warning",
          duration = 5
        )
      } else {
        # Success - update dropdown
        setProgress(value = 1)
        updateSelectInput(session, "session", choices = sessions_result$names)
        output$session_count <- renderText({
          paste0(length(sessions_result$names), " session(s) loaded")
        })
        showNotification(
          paste("Loaded", length(sessions_result$names), "session(s)"),
          type = "message",
          duration = 3
        )
      }
    })
    
    # Re-enable refresh button after a short delay
    invalidateLater(1000)
    observe({
      shinyjs::enable("refresh_sessions")
    })
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
      pdfdir <- normalizePath(pdfdir)

      # create name for combined pdf ex. tmp111111_combined.pdf
      combined_name <- str_c(file_path_sans_ext(name),"_ucsc.pdf")
      
      ## create pdf with table
      title_text <- str_glue("Session: {input$session}\nDB: {input$db}\nZoom: {input$zoom}x")
      export_table_as_pdf(datapath, pdfdir, title_text, input$need_annotations)

      # load bed file
      bed <- read_data(datapath, pdfdir = paste0(pdfdir,"/"), as.numeric(input$zoom))
      withProgress(message = 'Downloading files',detail = str_c('Processing ',name), value = 0, {
        
        setProgress(detail = "Init session")
        # init Chromote session
        init_params <- init_chromote(input$login, input$password, str_trim(input$session), input$db)
        
        if (is.null(init_params)) {
          setProgress(detail = "Session init failed")
          stop("Failed to initialize Chromote session")
        }
        
        # Number of times we'll go through the loop
        n <- nrow(bed)
        
        for (i in 1:n) {
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste("Doing file", i))
          correct_url <- paste0(init_params$main_url, bed$coords[i])
          outname <- bed$outname[i]
          download_pdf_chromote(init_params$chrome, correct_url, outname)
        }
        
        # Close Chromote session after batch
        tryCatch(init_params$chrome$close(), error = function(e) {})
        
        setProgress(detail = "Combine pdf files to one")
        
        # combine all files
        pdffiles <- sort(list.files(pdfdir, pattern = "pdf", full.names = T))
        qpdf::pdf_combine(input = pdffiles, output = combined_name)

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
    gc()
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

