library(telegram.bot)
library(imager)    # image loading and processing
library(dplyr)     # data manipulation
library(ggplot2)   # data visualization
library(tidyr)     # data wrangling
library(ggvoronoi) # visualization
library(kableExtra)


updater <- Updater(token = "6245433581:AAG09__0LnZtNNS5ArA_HdHq_4uWimbOREY")

start <- function(bot, update) {
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = sprintf("Hello %s!", update$message$from$first_name))
  
  # Get user profile photos
  photos <- bot$getUserProfilePhotos(user_id = update$message$chat_id)
  
  # Check if photos are available
  if (!is.null(photos$photos) && length(photos$photos) > 0) {
    # Download user profile photo
    file_id <- photos$photos[[1L]][[1L]]$file_id
    photo_file <- paste0("photo_", update$message$chat_id, ".jpeg")  # Unique file name
    bot$getFile(file_id, destfile = photo_file)
    
    # Load an image into R
    img <- imager::load.image(file = photo_file)
    
    # Print the image object out
    #print(img)
    
    
    # Represent the image as a data frame
    img_df <- as.data.frame(img)
    
    # Show a table of the first 10 rows of the data frame
    img_df %>% 
      arrange(x, y, cc) %>% # sort by columns for viewing
      filter(row_number() < 10) %>% # Select top 10 columns
      kable("html") %>%  # Display table in R Markdown
      kable_styling(full_width = F) # Don't take up full width
    
    
    
    # Add more expressive labels to the colors
    img_df <- img_df %>% 
      mutate(channel = case_when(
        cc == 1 ~ "Red",
        cc == 2 ~ "Green", 
        cc == 3 ~ "Blue"
      ))
    
    # Reshape the data frame so that each row is a point
    img_wide <- img_df %>%
      select(x, y, channel, value) %>%
      spread(key = channel, value = value) %>%
      mutate(
        color = rgb(Red, Green, Blue)
      )
    
    
    
    # Plot points at each sampled location
    ggplot(img_wide) +
      geom_point(mapping = aes(x = x, y = y, color = color)) +
      scale_color_identity() # use the actual value in the `color` column
    
    
    
    
    ggplot(img_wide) +
      geom_point(mapping = aes(x = x, y = y, color = color)) +
      scale_color_identity() + # use the actual value in the `color` column
      scale_y_reverse() + # Orient the image properly (it's upside down!)
      theme_void() # Remove axes, background
    
    
    
    # Take a sample of rows from the data frame
    sample_size <- 2000
    img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]
    
    # Plot only the sampled points
    ggplot(img_sample) +
      geom_point(mapping = aes(x = x, y = y, color = color)) +
      scale_color_identity() + # use the actual value in the `color` column
      scale_y_reverse() + # Orient the image properly (it's upside down!)
      theme_void() # Remove axes, background
    
    
    
    # Create random weights for point size
    img_sample$size <- runif(sample_size)
    
    # Plot only the sampled points
    ggplot(img_sample) +
      geom_point(mapping = aes(x = x, y = y, color = color, size = size)) +
      guides(size = FALSE) + # don't show the legend
      scale_color_identity() + # use the actual value in the `color` column
      scale_y_reverse() + # Orient the image properly (it's upside down!)
      theme_void() # Remove axes, background
    
    
    
    # Use the amount of blue present in each point to determine the size
    ggplot(img_sample) +
      geom_point(mapping = aes(x = x, y = y, color = color, size = Blue)) +
      guides(size = FALSE) + # don't show the legend
      scale_color_identity() + # use the actual value in the `color` column
      scale_y_reverse() + # Orient the image properly (it's upside down!)
      theme_void() # Remove axes, background
    
    
    
    # Create a Voronoi Diagram of the sampled points
    ggplot(img_sample) +
      geom_voronoi(mapping = aes(x = x, y = y, fill = color)) +
      scale_fill_identity() +
      scale_y_reverse() +
      theme_void()
    
    
    # Detect edges in the image
    edges <- cannyEdges(img)
    
    
    
    # Convert the edge image to a data frame for manipulation
    edges_df <- edges %>%  
      as.data.frame() %>% 
      select(x, y) %>% # only select columns of interest
      distinct(x, y) %>% # remove duplicates
      mutate(edge = 1) # indicate that these observations represent an edge
    
    # Join on the edges data
    img_wide <- img_wide %>% 
      left_join(edges_df)
    
    # Apply a low weight to the non-edge points
    img_wide$edge[is.na(img_wide$edge)] <- .05
    
    # Re-sample from the image, applying a higher probability to the edge points
    img_edge_sample <- img_wide[sample(nrow(img_wide), sample_size, prob = img_wide$edge), ]
    
    # Re-create the voronoi diagram with the re-sampled data
    final = ggplot(img_edge_sample) +
      geom_voronoi(mapping = aes(x = x, y = y, fill = color)) +
      scale_fill_identity() +
      guides(fill = FALSE) +
      scale_y_reverse() +
      theme_void() # Remove axes, background
    
    print(final)
    # Generate unique file name
    output_file <- paste0("plot_", update$message$chat_id, ".jpg")
    
    # Save the ggplot object as a JPEG image
    ggsave(filename = output_file, plot = final, device = "jpeg")
    
    
    
    # Send photo
    bot$sendPhoto(chat_id = update$message$chat_id,
                  photo = output_file  # Pass photo as a named argument
    )
  } else {
    bot$sendMessage(chat_id = update$message$chat_id,
                    text = "No profile photo available.")
  }
}




updater <- updater + MessageHandler(echo, MessageFilters$text)

start_handler <- CommandHandler("start", start)
updater <- updater + start_handler

updater$start_polling()


