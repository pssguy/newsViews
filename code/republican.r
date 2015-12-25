output$googleImage <- renderImage({
#   # A temp file to save the output.
#   # This file will be removed later by renderImage
#   outfile <- tempfile(fileext='.png')
#   
#   # Generate the PNG
#   png(outfile, width=400, height=300)
#   hist(rnorm(input$obs), main="Generated in renderImage()")
#   dev.off()
  
  # Return a list containing the filename
  list(src = 'repCandidates538.png',
       contentType = 'image/png',
       width = 450,
       height = 600,
       alt = "")
}, deleteFile = FALSE)

output$wikiImage <- renderImage({

  list(src = 'GOPCandidates.png',
       contentType = 'image/png',
    #   width = 450,
    #   height = 600,
       alt = "")
}, deleteFile = FALSE)


output$presText <- renderUI({
  #a(href='http://fivethirtyeight.com/datalab/how-media-interest-in-the-gop-candidates-compares-to-public-interest/')
#  paste0("fivethirtyeight.com recently ran an article on comparing",
#   tags$a(href='http://fivethirtyeight.com/datalab/how-media-interest-in-the-gop-candidates-compares-to-public-interest/', " media and public interest in GOP candidates"))

test <-  paste0("fivethirtyeight.com recently ran an article on comparing",
         a(href='http://fivethirtyeight.com/datalab/how-media-interest-in-the-gop-candidates-compares-to-public-interest/', " media and public interest in GOP candidates"))
  test
  })


output$repTable <- DT::renderDataTable({
  
  #print(glimpse(data))
  
  repData %>% 
    group_by(surname) %>% 
    summarize(av=round(mean(count, na.rm=T),0),max=max(count, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(desc(av)) %>% 
    rename(Candidate=surname,Average=av, Maximum=max) %>% 
    DT::datatable(rownames=FALSE,options= list(paging = FALSE, searching = FALSE,info=FALSE))
  
})