
library(officer)
library(magrittr)
library(flextable)

study = "MHL 231335"
investigator = " Ho"
path = "F:/QuPath/MHL 231335 Ho/export/"
img.file <- list.files(path, pattern = ".png", all.files = FALSE,
           full.names = TRUE)
img.file.name <- list.files(path, pattern = ".png", all.files = FALSE,
                            full.names = FALSE)

number <- length(img.file)
doc <- read_pptx()
doc <- add_slide(doc, layout = "Title and Content")#, master = "Office Theme")
doc <- ph_with(doc, value = study, location = ph_location_type(type = "title"))
doc <- ph_with(doc, value = format(Sys.Date()), location = ph_location_type(type = "dt"))
doc <- ph_with(doc, value = "Manifest", location = ph_location_type(type = "sldNum"))

Manifest <- list.files(path = path, pattern = "-a.png", all.files = FALSE, full.names = TRUE)
Manifest <- as.data.frame(Manifest)
set_flextable_defaults(font.size = 8, theme_fun = theme_booktabs, padding = 1)
ft <- flextable(Manifest)
ft <- autofit(ft)
doc <- ph_with(x = doc, ft, location = ph_location_type(type = "body") )

for(i in 1:number) {
    doc <- add_slide(doc)
    doc <- ph_with(doc, value = paste(img.file.name[i]), location = ph_location_type(type = "title"))
    #doc <- ph_with(doc, value = paste(img.file.name[i]), location = ph_location_type(type = "ftr"))
    doc <- ph_with(doc, value = format(Sys.Date()), location = ph_location_type(type = "dt"))
    doc <- ph_with(doc, value = paste("slide",i), location = ph_location_type(type = "sldNum"))
    doc <- ph_with(x = doc, external_img(img.file[i]), location = ph_location_type(type = "body"), use_loc_size = TRUE)
}


print(doc, target = paste0("C:/Users/edmondsonef/Desktop/",study,investigator,".pptx"))
