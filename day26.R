# 1.
library(raster)
local_path <- "data/foco-elev-cm.tif"  # Adjust path accordingly
r <- raster(local_path)

# 2. 
# Checking the structure
r

# 3. 
# Multiply raster by 0.0328084 to convert to feet
r_feet <- r * 0.0328084

# 4. 
# Extract values as a data frame
df <- as.data.frame(values(r_feet), na.rm = TRUE)
colnames(df) <- "elevation_feet"  # Name the column as elevation_feet

# 5. 
library(ggpubr)

# Create the density plot
density_plot <- ggplot(df, aes(x = elevation_feet)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Elevation Data in Feet",
       x = "Elevation (Feet)",
       y = "Density") +
  theme_minimal()

# Display the plot
print(density_plot)

# 6. save the plot
ggsave("elevation_density_plot.png", plot = density_plot, path = "img/")
