
tryCatch({
  library(sysfonts)
  library(showtext)
  
  print("Attempting to add 'Source Sans 3' from Google Fonts...")
  sysfonts::font_add_google("Source Sans 3", "Source Sans 3")
  print("Success: 'Source Sans 3' added.")
  
  print("Registered families:")
  print(sysfonts::font_families())
  
}, error = function(e) {
  print(paste("Error adding font:", e$message))
})
