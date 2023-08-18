                   ##########################################
                   # ACM40960 - Projects in Maths modelling #
                   #               Final Project            # 
                   #                                        #
                   #      Abhimanyu Premachandran Nair      #
                   #          Student No : 22204176         #
                   ##########################################


################################################################################

# Load the required package
library(ggplot2)

# Assign the constant values
G <- 6.67430e-11  # Gravitational constant (m^3 kg^-1 s^-2)
M_sun <- 1.989e30  # Mass of the Sun (kg)
M_earth <- 5.972e24  # Mass of the Earth (kg)
AU <- 1.496e11  # Astronomical Unit (m)
r_earth <- 6371000  # Earth's radius (m)
ra <- 2.7 * AU  # Pre-chosen radius for the asteroid belt (m)
v_earth <- 29780 # Velocity of Earth around the Sun (m/s)
omega <- v_earth / AU # Angular velocity of Earth around Sun

#------------------------------------------------------------------------------#
# Perform Backward Integration                                                 #
#------------------------------------------------------------------------------#

# Initial conditions for backward integration
r_init <- AU  # Initial radial distance (at the time of impact) (m)
v_init <- sqrt(G * M_sun / AU)  # Initial velocity (at the time of impact) (m/s), purely radially outward
theta <- 0 # Initial angle of Earth with the X-axis (at the time of impact) (degrees)

# Time parameters
dt <- 100 # Time step (s)
t_bwd <- 0 # Start time for backward integration
t_bwd_final <- -100000000  # End time for backward integration, before t_bwd

# Initialize arrays to store trajectory data
bwd_earth_pos <- list()
bwd_earth_vel <- list()
bwd_aster_pos <- list()
bwd_aster_vel <- list()

# Backward integration (from ra to an earlier time)
while (t_bwd > t_bwd_final && r_init <= ra) {
  
  if (r_init > ra) break
  
  # Calculate gravitational force on the asteroid
  Fg <- -G * M_sun * M_earth / r_init^2
  
  # Calculate acceleration
  a <- Fg / M_earth
  
  # Update velocity and position using Newton's equations of motion
  v_init <- v_init + a * dt
  r_init <- r_init + v_init * dt + 0.5 * a * dt^2
  
  # Update the velocity and positions of Earth
  x_earth <- AU * ((cos(theta)))
  y_earth <- AU * (-(sin(theta)))
  
  # Store Earth's position and velocity
  bwd_earth_pos[[length(bwd_earth_pos) + 1]] <- c(x_earth, y_earth)
  bwd_earth_vel[[length(bwd_earth_vel) + 1]] <- c(v_earth, 0)
  
  # Store asteroid's position and velocity
  bwd_aster_pos[[length(bwd_aster_pos) + 1]] <- c(r_init, 0)
  bwd_aster_vel[[length(bwd_aster_vel) + 1]] <- c(v_init, 0)
  
  t_bwd <- t_bwd - dt
  theta <- theta + omega * dt
}

#------------------------------------------------------------------------------#
# Perform Forward Integration                                                  #
#------------------------------------------------------------------------------#

# Initial conditions for forward integration
dt <- 100
t_fwd <- 0 # Start time for forward integration
t_fwd_final <- 100000000  # End time for forward integration, after t_fwd

# Initialize arrays to store trajectory data
fwd_earth_pos <- list()
fwd_earth_vel <- list()
fwd_aster_pos <- list()
fwd_aster_vel <- list()

# Forward integration (from ra to a later time)
r_init <- ra  # Reset initial conditions
v_init <- -sqrt(G * M_sun / ra)  # Reset initial velocity

while (t_fwd < t_fwd_final) {
  # Calculate gravitational force on the asteroid
  Fg <- -G * M_sun * M_earth / r_init^2
  
  # Calculate acceleration
  a <- Fg / M_earth
  
  # Update velocity and position using Newton's equations of motion
  v_init <- v_init + a * dt
  r_init <- r_init + v_init * dt + 0.5 * a * dt^2
  
  x_earth <- AU * cos(theta)
  y_earth <- AU * sin(theta)
  
  # Store Earth's position and velocity
  fwd_earth_pos[[length(fwd_earth_pos) + 1]] <- c(x_earth, y_earth)
  fwd_earth_vel[[length(fwd_earth_vel) + 1]] <- c(v_earth, 0)
  
  # Store asteroid's position and velocity
  fwd_aster_pos[[length(fwd_aster_pos) + 1]] <- c(r_init, 0)
  fwd_aster_vel[[length(fwd_aster_vel) + 1]] <- c(v_init, 0)
  
  t_fwd <- t_fwd + dt
  theta <- theta - omega * dt
}

final_fwd_earth_posn <- fwd_earth_pos[[length(fwd_earth_pos)]]

#------------------------------------------------------------------------------#
# Checking whether the impact happens or not                                   #
#------------------------------------------------------------------------------#

# Check if the asteroid impacts the Earth during forward integration
impact = FALSE
for (i in 1:length(fwd_aster_pos)) {
  aster_posn <- fwd_aster_pos[[i]]
  dist_to_earth <- sqrt(sum((aster_posn - final_fwd_earth_posn)^2))
  
  if (dist_to_earth < r_earth) {
    impact = TRUE
    impact_ind = i
    break
  }
}

# Print impact information
if (impact) {
  cat("Asteroid impacts the Earth!\n")
  cat("Time step at impact:", impact_ind * dt, "s\n")
  
  # Final positions and velocities at r=ra
  final_fwd_earth_velo <- fwd_earth_vel[[length(fwd_earth_vel)]]
  final_fwd_aster_posn <- fwd_aster_pos[[impact_ind]]
  final_fwd_aster_velo <- fwd_aster_vel[[impact_ind]]
  
  # Print final results
  cat("Position of Earth at the Time of Impact: ", final_fwd_earth_posn, "\n")
  cat("Velocity of Earth at the Time of Impact: ", final_fwd_earth_velo, "\n")
  cat("Position of Asteroid at the Time of Impact: ", final_fwd_aster_posn, "\n")
  cat("Final Asteroid Velocity:", final_fwd_aster_velo, "\n")
} else {
  cat("Asteroid does not impact the Earth.\n")
}

#------------------------------------------------------------------------------#
# Plotting the trajectory of the Earth and the Asteroid                        #
#------------------------------------------------------------------------------#

# Extract x and y coordinates from the trajectory data
earth_x <- sapply(fwd_earth_pos, "[[", 1)
earth_y <- sapply(fwd_earth_pos, "[[", 2)
aster_x <- sapply(fwd_aster_pos, "[[", 1)
aster_y <- sapply(fwd_aster_pos, "[[", 2)

# Combine trajectory data
trajectory_1 <- data.frame(
  Time = rep(c(1:length(earth_x)), each = 2), # Each Earth/Asteroid position is repeated twice for animation
  Earth_X = rep(earth_x, each = 2),
  Earth_Y = rep(earth_y, each = 2),
  Aster_X = rep(aster_x, each = 2),
  Aster_Y = rep(aster_y, each = 2)
)

# Plot the trajectories
ggplot(trajectory_1, aes(x = Earth_X, y = Earth_Y)) +
  geom_point(color = "black", size = 1) +
  geom_point(aes(final_fwd_earth_posn[[1]], 
                 final_fwd_earth_posn[[2]]),
             color = "blue", size = 10) +
  geom_point(aes(0, 0), color = "orange", size = 20) +
  geom_point(aes(final_fwd_aster_posn[[1]],0),
             color = "red", size = 2) +
  labs(x = "X Position (m)", y = "Y Position (m)",
       title = "Trajectories of Earth and Asteroid") +
  theme_minimal() +
  guides(color = guide_none())  # To hide the legend for color

#------------------------------------------------------------------------------#
# Running N simulations for forward integration                                #
#------------------------------------------------------------------------------#

# Here, theta value is taken as N
# The loop runs through every angle from 0, till it reaches 359 degrees

for (N in 0:359) {
  
  #------------------------------------------------------------------------------#
  # Perform Backward Integration                                                 #
  #------------------------------------------------------------------------------#
  
  # Initial conditions for backward integration
  r_init <- AU  # Initial radial distance (at the time of impact) (m)
  v_init <- sqrt(G * M_sun / AU)  # Initial velocity (at the time of impact) (m/s), purely radially outward
  theta <- N # Initial angle of Earth with the X-axis (at the time of impact) (degrees)
  
  # Time parameters
  dt <- 100 # Time step (s)
  t_bwd <- 0 # Start time for backward integration
  t_bwd_final <- -100000000  # End time for backward integration, before t_bwd
  
  # Initialize arrays to store trajectory data
  bwd_earth_pos <- list()
  bwd_earth_vel <- list()
  bwd_aster_pos <- list()
  bwd_aster_vel <- list()
  
  # Backward integration (from ra to an earlier time)
  while (t_bwd > t_bwd_final && r_init <= ra) {
    
    if (r_init > ra) break
    
    # Calculate gravitational force on the asteroid
    Fg <- -G * M_sun * M_earth / r_init^2
    
    # Calculate acceleration
    a <- Fg / M_earth
    
    # Update velocity and position using Newton's equations of motion
    v_init <- v_init + a * dt
    r_init <- r_init + v_init * dt + 0.5 * a * dt^2
    
    # Update the velocity and positions of Earth
    x_earth <- AU * ((cos(theta)))
    y_earth <- AU * (-(sin(theta)))
    
    # Store Earth's position and velocity
    bwd_earth_pos[[length(bwd_earth_pos) + 1]] <- c(x_earth, y_earth)
    bwd_earth_vel[[length(bwd_earth_vel) + 1]] <- c(v_earth, 0)
    
    # Store asteroid's position and velocity
    bwd_aster_pos[[length(bwd_aster_pos) + 1]] <- c(r_init, 0)
    bwd_aster_vel[[length(bwd_aster_vel) + 1]] <- c(v_init, 0)
    
    t_bwd <- t_bwd - dt
    theta <- theta + omega * dt
  }
  
  #----------------------------------------------------------------------------#
  # Perform Forward Integration                                                #
  #----------------------------------------------------------------------------#
  
  dt <- 100
  t_fwd <- 0 # Start time for forward integration
  t_fwd_final <- 100000000  # End time for forward integration, after t_fwd
  
  # Initialize arrays to store trajectory data
  fwd_earth_pos <- list()
  fwd_earth_vel <- list()
  fwd_aster_pos <- list()
  fwd_aster_vel <- list()
  impact_res <- list()
  
  # Forward integration (from ra to a later time)
  r_init <- ra  # Reset initial conditions
  v_init <- -sqrt(G * M_sun / r_init)  # Reset initial velocity
  
  while (t_fwd < t_fwd_final) {
    # Calculate gravitational force on the asteroid
    Fg <- -G * M_sun * M_earth / r_init^2
    
    # Calculate acceleration
    a <- Fg / M_earth
    
    # Update velocity and position using Newton's equations of motion
    v_init <- v_init + a * dt
    r_init <- r_init + v_init * dt + 0.5 * a * dt^2
    
    x_earth <- AU * cos(theta)
    y_earth <- AU * sin(theta)
    
    # Store Earth's position and velocity
    fwd_earth_pos[[length(fwd_earth_pos) + 1]] <- c(x_earth, y_earth)
    fwd_earth_vel[[length(fwd_earth_vel) + 1]] <- c(v_earth, 0)
    
    # Store asteroid's position and velocity
    fwd_aster_pos[[length(fwd_aster_pos) + 1]] <- c(r_init, 0)
    fwd_aster_vel[[length(fwd_aster_vel) + 1]] <- c(v_init, 0)
    
    t_fwd <- t_fwd + dt
    theta <- theta - omega * dt
  }
  
  final_fwd_earth_posn <- fwd_earth_pos[[length(fwd_earth_pos)]]
  
  #----------------------------------------------------------------------------#
  # Checking for impact                                                        #
  #----------------------------------------------------------------------------#
  
  # Check if the asteroid impacts the Earth during forward integration
  impact = FALSE
  for (i in 1:length(fwd_aster_pos)) {
    aster_posn <- fwd_aster_pos[[i]]
    dist_to_earth <- sqrt(sum((aster_posn - final_fwd_earth_posn)^2))
    
    if (dist_to_earth < r_earth) {
      impact = TRUE
      impact_ind = i
      break
    }
  }
  
  impact_res[[length(impact_res) + 1]] <- c(impact = impact, impact_ind = impact_ind)
  
  # Print impact information
  if (impact) {
    cat("Asteroid impacts the Earth!\n")
    cat("Time step at impact:", impact_ind * dt, "s\n")
    
    # Final positions and velocities at r=ra
    final_fwd_earth_velo <- fwd_earth_vel[[length(fwd_earth_vel)]]
    final_fwd_aster_posn <- fwd_aster_pos[[impact_ind]]
    final_fwd_aster_velo <- fwd_aster_vel[[impact_ind]]
    
    # Print final results
    cat("Position of Earth at the Time of Impact: ", final_fwd_earth_posn, "\n")
    cat("Velocity of Earth at the Time of Impact: ", final_fwd_earth_velo, "\n")
    cat("Position of Asteroid at the Time of Impact: ", final_fwd_aster_posn, "\n")
    cat("Final Asteroid Velocity:", final_fwd_aster_velo, "\n")
  } else {
    cat("Asteroid does not impact the Earth.\n")
  }
}

#------------------------------------------------------------------------------#
# Tweaking the initial conditions for ra.                                      #
#------------------------------------------------------------------------------#

delta <- c(1e-9, 1e-6, 1e-3)
impact_results <- list()

for (d in delta) {
  for (N in 0:359) {
    # Initial conditions for backward integration
    r_init <- AU  # Initial radial distance (at the time of impact) (m)
    v_init <- sqrt(G * M_sun / AU)  # Initial velocity (at the time of impact) (m/s), purely radially outward
    theta <- N # Initial angle of Earth with the X-axis (at the time of impact) (degrees)
    
    # Time parameters
    dt <- 100 # Time step (s)
    t_bwd <- 0 # Start time for backward integration
    t_bwd_final <- -100000000  # End time for backward integration, before t_bwd
    
    # Initialize arrays to store trajectory data
    bwd_earth_pos <- list()
    bwd_earth_vel <- list()
    bwd_aster_pos <- list()
    bwd_aster_vel <- list()
    
    # Backward integration (from ra to an earlier time)
    while (t_bwd > t_bwd_final && r_init <= ra) {
      
      if (r_init > ra) break
      
      # Calculate gravitational force on the asteroid
      Fg <- -G * M_sun * M_earth / r_init^2
      
      # Calculate acceleration
      a <- Fg / M_earth
      
      # Update velocity and position using Newton's equations of motion
      v_init <- v_init + a * dt
      r_init <- r_init + v_init * dt + 0.5 * a * dt^2
      
      # Update the velocity and positions of Earth
      x_earth <- AU * ((cos(theta)))
      y_earth <- AU * (-(sin(theta)))
      
      # Store Earth's position and velocity
      bwd_earth_pos[[length(bwd_earth_pos) + 1]] <- c(x_earth, y_earth)
      bwd_earth_vel[[length(bwd_earth_vel) + 1]] <- c(v_earth, 0)
      
      # Store asteroid's position and velocity
      bwd_aster_pos[[length(bwd_aster_pos) + 1]] <- c(r_init, 0)
      bwd_aster_vel[[length(bwd_aster_vel) + 1]] <- c(v_init, 0)
      
      t_bwd <- t_bwd - dt
      theta <- theta + omega * dt
    }
    
    #--------------------------------------------------------------------------#
    # Perform Forward Integration                                              #
    #--------------------------------------------------------------------------#
    
    dt <- 100
    t_fwd <- 0 # Start time for forward integration
    t_fwd_final <- 100000000  # End time for forward integration, after t_fwd
    
    # Initialize arrays to store trajectory data
    fwd_earth_pos <- list()
    fwd_earth_vel <- list()
    fwd_aster_pos <- list()
    fwd_aster_vel <- list()
    impact_res <- list()
    
    # Forward integration (from ra to a later time)
    r_init <- (1 + d) * ra  # Reset initial conditions
    v_init <- -sqrt(G * M_sun / r_init)  # Reset initial velocity
    
    while (t_fwd < t_fwd_final) {
      # Calculate gravitational force on the asteroid
      Fg <- -G * M_sun * M_earth / r_init^2
      
      # Calculate acceleration
      a <- Fg / M_earth
      
      # Update velocity and position using Newton's equations of motion
      v_init <- v_init + a * dt
      r_init <- r_init + v_init * dt + 0.5 * a * dt^2
      
      x_earth <- AU * cos(theta)
      y_earth <- AU * sin(theta)
      
      # Store Earth's position and velocity
      fwd_earth_pos[[length(fwd_earth_pos) + 1]] <- c(x_earth, y_earth)
      fwd_earth_vel[[length(fwd_earth_vel) + 1]] <- c(v_earth, 0)
      
      # Store asteroid's position and velocity
      fwd_aster_pos[[length(fwd_aster_pos) + 1]] <- c(r_init, 0)
      fwd_aster_vel[[length(fwd_aster_vel) + 1]] <- c(v_init, 0)
      
      t_fwd <- t_fwd + dt
      theta <- theta - omega * dt
    }
    
    final_fwd_earth_posn <- fwd_earth_pos[[length(fwd_earth_pos)]]
    
    
    # Check if the asteroid impacts the Earth during forward integration
    impact = FALSE
    impact_index = NULL
    
    for (i in 1:length(fwd_aster_pos)) {
      asteroid_position <- fwd_aster_pos[[i]]
      dist_to_earth <- sqrt(sum((asteroid_position - final_fwd_earth_posn)^2))
      
      if (dist_to_earth < r_earth) {
        impact = TRUE
        impact_ind = i
        break
      }
    }
    
    # Store impact result for the current delta value
    impact_results[[as.character(d)]] <- list(impact = impact, impact_ind = impact_ind)
  }
}

# Print impact results for different delta values
for (d in delta) {
  result <- impact_results[[as.character(d)]]
  cat("Delta:", d, "- ", ifelse(result$impact, "Asteroid impacts the Earth!", "Asteroid does not impact the Earth."), "\n")
  if (result$impact) {
    cat("Time step at impact:", result$impact_ind * dt, "s\n")
    cat("Asteroid Position at Impact:\n")
    cat(fwd_aster_pos[[result$impact_ind]], "\n")
  }
}

#------------------------------------------------------------------------------#
# Adding Jupiter to the system                                                 #
#------------------------------------------------------------------------------#

# Constants for Jupiter
M_jupiter <- 1.898e27  # Mass of Jupiter (kg)
r_jupiter <- 5.2 * AU  # Average distance of Jupiter from the Sun (m)

#------------------------------------------------------------------------------#
# Running N simulations for forward integration                                #
#------------------------------------------------------------------------------#

# Here, theta value is taken as N
# The loop runs through every half angle from 0, till it reaches 359.5 degrees

for (N in 0:359) {
  
  #------------------------------------------------------------------------------#
  # Perform Backward Integration                                                 #
  #------------------------------------------------------------------------------#
  
  # Initial conditions for backward integration
  r_init <- AU  # Initial radial distance (at the time of impact) (m)
  v_init <- sqrt(G * M_sun / AU)  # Initial velocity (at the time of impact) (m/s), purely radially outward
  theta <- N # Initial angle of Earth with the X-axis (at the time of impact) (degrees)
  
  # Time parameters
  dt <- 100 # Time step (s)
  t_bwd <- 0 # Start time for backward integration
  t_bwd_final <- -100000000  # End time for backward integration, before t_bwd
  
  # Initialize arrays to store trajectory data
  bwd_earth_pos <- list()
  bwd_earth_vel <- list()
  bwd_aster_pos <- list()
  bwd_aster_vel <- list()
  
  # Backward integration (from ra to an earlier time)
  while (t_bwd > t_bwd_final && r_init <= ra) {
    
    if (r_init > ra) break
    
    # Calculate gravitational force on the asteroid
    Fg <- -G * M_sun * M_earth / r_init^2
    
    # Calculate acceleration
    a <- Fg / M_earth
    
    # Update velocity and position using Newton's equations of motion
    v_init <- v_init + a * dt
    r_init <- r_init + v_init * dt + 0.5 * a * dt^2
    
    # Update the velocity and positions of Earth
    x_earth <- AU * ((cos(theta)))
    y_earth <- AU * (-(sin(theta)))
    
    # Store Earth's position and velocity
    bwd_earth_pos[[length(bwd_earth_pos) + 1]] <- c(x_earth, y_earth)
    bwd_earth_vel[[length(bwd_earth_vel) + 1]] <- c(v_earth, 0)
    
    # Store asteroid's position and velocity
    bwd_aster_pos[[length(bwd_aster_pos) + 1]] <- c(r_init, 0)
    bwd_aster_vel[[length(bwd_aster_vel) + 1]] <- c(v_init, 0)
    
    t_bwd <- t_bwd - dt
    theta <- theta + omega * dt
  }
  
  #----------------------------------------------------------------------------#
  # Perform Forward Integration                                                #
  #----------------------------------------------------------------------------#
  
  dt <- 100
  t_fwd <- 0 # Start time for forward integration
  t_fwd_final <- 100000000  # End time for forward integration, after t_fwd
  
  # Initialize arrays to store trajectory data
  fwd_earth_pos <- list()
  fwd_earth_vel <- list()
  fwd_aster_pos <- list()
  fwd_aster_vel <- list()
  impact_res <- list()
  
  # Forward integration (from ra to a later time)
  r_init <- ra  # Reset initial conditions
  v_init <- -sqrt(G * M_sun / r_init)  # Reset initial velocity
  
  while (t_fwd < t_fwd_final) {
    
    # Calculate gravitational force on the asteroid
    Fg <- -G * M_sun * M_earth / r_init^2
    
    # Calculate acceleration
    ag <- Fg / M_earth
    
    # Calculate gravitational force on the asteroid
    Fj <- -G * M_sun * M_jupiter / r_init^2
    
    # Calculate acceleration
    aj <- Fg / M_jupiter
    
    # Effective acceleration
    a = ag - aj
    
    # Update velocity and position using Newton's equations of motion
    v_init <- v_init + a * dt
    r_init <- r_init + v_init * dt + 0.5 * a * dt^2
    
    x_earth <- AU * cos(theta)
    y_earth <- AU * sin(theta)
    
    # Store Earth's position and velocity
    fwd_earth_pos[[length(fwd_earth_pos) + 1]] <- c(x_earth, y_earth)
    fwd_earth_vel[[length(fwd_earth_vel) + 1]] <- c(v_earth, 0)
    
    # Store asteroid's position and velocity
    fwd_aster_pos[[length(fwd_aster_pos) + 1]] <- c(r_init, 0)
    fwd_aster_vel[[length(fwd_aster_vel) + 1]] <- c(v_init, 0)
    
    t_fwd <- t_fwd + dt
    theta <- theta - omega * dt
  }
  
  final_fwd_earth_posn <- fwd_earth_pos[[length(fwd_earth_pos)]]
  
  #----------------------------------------------------------------------------#
  # Checking for impact                                                        #
  #----------------------------------------------------------------------------#
  
  # Check if the asteroid impacts the Earth during forward integration
  impact = FALSE
  for (i in 1:length(fwd_aster_pos)) {
    aster_posn <- fwd_aster_pos[[i]]
    dist_to_earth <- sqrt(sum((aster_posn - final_fwd_earth_posn)^2))
    
    if (dist_to_earth < r_earth) {
      impact = TRUE
      impact_ind = i
      break
    }
  }
  
  impact_res[[length(impact_res) + 1]] <- c(impact = impact, impact_ind = impact_ind)
  
  # Print impact information
  if (impact) {
    cat("Asteroid impacts the Earth!\n")
    cat("Time step at impact:", impact_ind * dt, "s\n")
    
    # Final positions and velocities at r=ra
    final_fwd_earth_velo <- fwd_earth_vel[[length(fwd_earth_vel)]]
    final_fwd_aster_posn <- fwd_aster_pos[[impact_ind]]
    final_fwd_aster_velo <- fwd_aster_vel[[impact_ind]]
    
    # Print final results
    cat("Position of Earth at the Time of Impact: ", final_fwd_earth_posn, "\n")
    cat("Velocity of Earth at the Time of Impact: ", final_fwd_earth_velo, "\n")
    cat("Position of Asteroid at the Time of Impact: ", final_fwd_aster_posn, "\n")
    cat("Final Asteroid Velocity:", final_fwd_aster_velo, "\n")
  } else {
    cat("Asteroid does not impact the Earth.\n")
  }
}
