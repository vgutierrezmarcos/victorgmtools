
tryCatch({
  library(sysfonts)
  library(showtext)
  
  print("Attempting to add 'Nunito Sans' from Google Fonts...")
  sysfonts::font_add_google("Nunito Sans", "Nunito Sans")
  print("Success: 'Nunito Sans' added.")
  
  print("Registered families:")
  print(sysfonts::font_families())
  
}, error = function(e) {
  print(paste("Error adding font:", e$message))
})
