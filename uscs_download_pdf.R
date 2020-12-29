library(tidyverse)
library(httr)
library(rvest)
library(staplr)

#### Parameters
# UCSC session name
session <- "session name"
# UCSC login
login <- "username"
# UCSC password
password <- "password"
# database name
db <- "mm9"
# path to bed file
bed_path <- "./file_with_regions.bed"
#### end parameters

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

# create tmp directory
pdfdir <- tempfile("tmp", tmpdir = "./")
if (!dir.exists(pdfdir)) {
  dir.create(pdfdir)  
}

# load bed file
bed <- read_bed(bed_path, pdfdir = paste0(pdfdir,"/"))

# initialize
init_params <- init(login, password, session, db)

# download pdfs to tmp directory
apply(bed, 1, function(x) {
  correct_url <- paste0(init_params$main_url, as.character(x[1]))
  download_pdf(init_params$session, correct_url, as.character(x[2]))
})

# create name for combined pdf (ex. tmp1234567_combined.pdf)
combined_name <- paste0(pdfdir, "_combined.pdf")

# combine all files
staplr::staple_pdf(input_directory = pdfdir, output_filepath = combined_name)

# remove directory with pdf files
unlink(pdfdir, recursive = TRUE)
