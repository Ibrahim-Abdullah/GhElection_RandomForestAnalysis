
#Author : Ibrahim Abdullah
#Date : November 30, 2016
#Data Mining Final Project : Intra and Inter Analysis of Manifesto's of Major Political Party's in Ghana.
#Team : Group B

#Read all the pdf files in the directory into a vector

files <- list.files(pattern = "pdf$",  full.names = TRUE)

#Print all the read files 
files

# convert each PDF file that is named in the vector into a text file 
# text file is created in the same directory as the PDFs
# C:/Program Files/xpdf/bin64/pdftotext.exe is the path to my pdftotext.exe. This is 
# PDF extraction engine to hel convert the pdf files. 
# Hence, before you can run this code, you have to download the xpdf engine from  http://www.foolabs.com/xpdf/download.html.
# If you're using windows, the extrated folder you should have three more folders: bin32, bin64 and doc. 
# The first two are programs for 32- and 64-bit systems, respectively. 
# The last contains documentation on the programs.
# Finally you may need to update your system path so it points to where the xpdf tools are installed. 
# make sure you update you environment variables to point to where the the xpdf files are kept.
#
#In this case, We are usinf the pdfttext.exe to extract the pdf files int text files.
# NB: THE PATH TO MY pdftotext.exe IS DIFFERENT FROM YOURS, SO HAVE TO CHANGE IT TO POINT TO YOURS.


lapply(files, function(i) system(paste('"C:/Program Files/xpdf/bin64/pdftotext.exe"', 
                                         paste0('"', i, '"')), wait = FALSE) )
