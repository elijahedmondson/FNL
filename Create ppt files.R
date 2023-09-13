
library(officer)
library(magrittr)
library(flextable)
library(magick)

study = "MHL Skimune_set2"
investigator = " NCL"
#path = "F:/QuPath/MHL 230588 Stracker/export/"
path = "C:/Users/edmondsonef/Desktop/Skimune_set2/"
img.file <- list.files(path, all.files = FALSE,#pattern = ".png", 
           full.names = TRUE)
img.file.name <- list.files(path, all.files = FALSE,#pattern = ".png", 
                            full.names = FALSE)

number <- length(img.file)
doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content")#, master = "Office Theme")
doc <- ph_with(doc, value = study, location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = format(Sys.Date()), location = ph_location_type(type = "dt"))
doc <- ph_with(doc, value = "Manifest", location = ph_location_type(type = "sldNum"))

Manifest <- list.files(path = path, #pattern = "-a.png", 
                       all.files = FALSE, full.names = TRUE)
Manifest <- as.data.frame(Manifest)
set_flextable_defaults(font.size = 8, theme_fun = theme_booktabs, padding = 1)#, table.layout = "fixed")
ft <- flextable(Manifest)
ft <- autofit(ft)
doc <- ph_with(x = doc, ft, location = ph_location_type(type = "body") )

for(i in 1:number) {
    doc <- add_slide(doc)
    doc <- ph_with(doc, value = paste(img.file.name[i]), location = ph_location_type(type = "title"), use_loc_size = T)
    #doc <- ph_with(doc, value = paste(img.file.name[i]), location = ph_location_type(type = "ftr"))
    doc <- ph_with(doc, value = format(Sys.Date()), location = ph_location_type(type = "dt"))
    doc <- ph_with(doc, value = paste("slide",i), location = ph_location_type(type = "sldNum"))
    sizes <- lapply(img.file[i], function(x) {
      z <- magick::image_read(x)
      z <- magick::image_data(z)
      attr(z, "dim")[-1]
    })
    sizes <- do.call(rbind, sizes)
    width <- sizes[, 1]/150
    height <- sizes[, 2]/150
    doc <- ph_with(x = doc, external_img(img.file[i], width = width, height = height, unit = "cm"), 
                   location = ph_location_left(), use_loc_size = FALSE)
}


print(doc, target = paste0("C:/Users/edmondsonef/Desktop/",study,investigator,".pptx"))


