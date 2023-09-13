#  entropy increase

x <- 1:length(unique(amenities$amenity))

entropy_steps <- log(x) |> round(3)

for (i in x) {
  
  grid[entropy==entropy_steps[i],]
  
}
