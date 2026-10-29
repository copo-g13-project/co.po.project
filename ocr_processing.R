library(magick)
library(tesseract)

# इमेजचा पाथ
img_path <- "C:/Users/prati/Downloads/Teal Student Marks Spreadsheet.png"

# PSM मोड 6 (Uniform block of text) सेट करून इंजिन तयार करणे
engine <- tesseract(options = list(tessedit_pageseg_mode = 6))

# इमेज रीड करून OCR करणे
img <- image_read(img_path)
extracted_text <- ocr(img, engine = engine)

# फाईलमध्ये सेव्ह करणे
writeLines(extracted_text, "extracted_text.txt")
cat("डेटा एक्सट्रॅक्ट झाला आहे! extracted_text.txt फाईल तपासा.\n")