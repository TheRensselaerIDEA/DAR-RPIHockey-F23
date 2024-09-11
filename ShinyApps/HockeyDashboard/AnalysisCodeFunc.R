# Hockey Analysis Help Functions
# Version: 06 Oct 2023 (Updated with TM's new functions)

# Make a graphable raster object given a file path and file name
makeRaster = function(filepath, filename){
  image_jpeg = readJPEG(str_c(filepath, filename))
  rasterGrob(image_jpeg, width = unit(1,"npc"), height = unit(1,"npc"),
             interpolate = FALSE)
}

# Sequence helper functions

# Function that takes in a data frame, a player, and the variable we want from it, and returns the appropriate column
# Also works with a vector of variables, where all will be returned
getPlayerVar = function(sequence, player, var){
  sequence %>% select(str_c(player, var))
} 

# Function that takes in a data frame and returns that variable for all players. Also works with multiple variables
allPlayerVar = function(sequence, var){
  sequence %>% select(ends_with(var))
}

# Function that takes in a sequence and returns the names of all players. Assumes x is a variable
getPlayers = function(sequence, puck = TRUE){
  players = sequence %>%
    # Get all x columns
    allPlayerVar('x') %>% 
    # Get their names
    colnames() %>% 
    # Chop off the last character
    str_sub(end = -2)
  
  if(!puck){
    # Remove the puck
    players %<>% discard(~ . == 'P')
  }
  
  players
}

# Takes in a sequence, a player, and a frame, and returns the player's x and y on that frame as a list
frameXY = function(sequence, player, frame){
  sequence %>% 
    getPlayerVar(player, c('x', 'y')) %>% 
    slice(frame) %>% 
    as.list %>% 
    set_names(c('x', 'y'))
}

# Sequence formatting functions
# Arctangent that only gives positive angles in degrees
atan3 = function(y, x){
  # A negative number modulo a positive is a positive number
  (atan2(y, x) * 180 / pi) %% 180
}

# Function that returns distance between two points
getDistance = function(point_1, point_2){
  sqrt(sum((point_1-point_2)^2))
}

# From the "(x, y)" coordinates, gets the value of x
xFromCoords = function(coordinates){
  # Use a regular expression to find the number preceding the comma
  x_string = str_extract(coordinates, '[-\\d]+(?=,)')
  
  # Convert to integer
  as.integer(x_string)
}

# From the "(x, y)" coordinates, gets the value of y
yFromCoords = function(coordinates){
  # Use a regular expression to find the number after the comma and space
  y_string = str_extract(coordinates, '(?<=, )[-\\d]+')
  
  # Convert to integer
  y_int = as.integer(y_string)
  
  # Flip Y coordinates
  ysize - y_int
}

# Converts pixels per frame to feet per second
feetPerSecond = function(pixels_per_frame, frames_per_sec = fps){
  pixels_per_frame * frames_per_sec  / 10
}

# Cartesian coordinates to magnitude and direction
cartToMagDir = function(x, y){
  magnitude = getDistance(c(x, y), c(0, 0))
  direction = atan3(y, x)
  
  list(mag = magnitude, dir = direction)
}
# Converts an x and y data frame for each player to the rate of change of those columns, in cartesian or magnitude/direction
rateOfChangeCols = function(x.df, y.df, players, cartCords, column_names, 
                            in_pixels_per_frame = TRUE, start_rows_to_ignore = 0){
  # If this isn't true, something is wrong
  stopifnot(dim(x.df) == dim(y.df))
  
  # For acceleration, we want to cut out the first row
  # Indexing with -0 returns nothing
  if(start_rows_to_ignore > 0){
    x.df = x.df[-(1:start_rows_to_ignore), ]
    y.df = y.df[-(1:start_rows_to_ignore), ]
  }
  # Speed is dx/dt but dt is 1 (each observation is 1 frame apart)
  sx = x.df %>%
    # Diff requires a matrix as input
    as.matrix %>% 
    diff
  
  sy = y.df %>% 
    as.matrix %>% 
    diff
  
  if(in_pixels_per_frame){
    sx %<>% apply(c(1, 2), feetPerSecond)
    sy %<>% apply(c(1, 2), feetPerSecond)
  }
  sx.df = as.data.frame(sx)
  sy.df = as.data.frame(sy)
  
  column_names = c(str_c(players, column_names[1]), str_c(players, column_names[2]))
  
  # Create rows of NA, needed because every time we derive, the first row is lost
  first_rows.df = matrix(NA, start_rows_to_ignore + 1, ncol(sx.df) + ncol(sy.df)) %>% 
    as.data.frame %>% 
    set_names(column_names)
  
  if(!cartCords){
    # Overwrite sx.df and sy.df with magnitude and direction instead
    for(i in 1:nrow(sx.df)){
      for(j in 1:ncol(sx.df)){
        mag_dir = cartToMagDir(sx.df[i, j], sy.df[i, j])
        sx.df[i, j] = mag_dir$mag
        sy.df[i, j] = mag_dir$dir
      }
    }
  }
  
  rbind(first_rows.df, 
        cbind(sx.df, sy.df) %>% set_names(column_names))
}

# Apply cubic interpolation on a sequence
cubicSmooth = function(sequence){
  for(colname in colnames(sequence %>% select(-frames))){
    y = sequence[[colname]]
    x = sequence$frames
    x2 = x ^ 2
    x3 = x ^ 3
    
    linear_model = lm(y ~ x + x2 + x3)
    
    coefs.m = linear_model$coefficients %>% as.matrix %>% replace_na(0)
    
    mult_matrix.m = cbind(rep(1, nrow(sequence)), x, x2, x3)
    
    sequence[colname] = mult_matrix.m %*% coefs.m
  }
  sequence
}

removeDuplicates = function(column){
  diffs = column %>% unlist %>% diff
  
  first_non_na = column %>% is.na %>% '!'() %>% which %>% min
  
  append(diffs, 1, after = 0)
  
  duped = diffs == 0
  
  column[duped] = NA
  
  column
}

# Format Sequence: formats sequence to any variety of options
formatSequence = function(sequence, puckPos = FALSE, interpolate = FALSE, cartCords = FALSE, velocity = FALSE, acceleration = FALSE, puckToGoal = FALSE, insideDots = FALSE){
  
  # Filter out rows and columns of all -1s
  sequence %<>% 
    select(-Ses.Frm) %>% 
    rename("P" = "Puck") %>%
    # Change -1, -1s to NAs
    mutate_all(~na_if(., "(-1, -1)")) %>% 
    # Change the old default puck value to NAs
    mutate_all(~na_if(., "(21648, -15466)")) %>% 
    # Remove any columns that have all NAs
    select_if(apply(., 2, function(c) any(!is.na(c)) ))
  
  # Get everyone's x and y
  player_x = map(sequence, xFromCoords)
  player_y = map(sequence, yFromCoords)
  
  players = names(player_x)
  
  frames = 1:nrow(sequence)
  
  sequence = frames %>% 
    bind_cols(player_x) %>% 
    bind_cols(player_y) %>% 
    set_names(c('frames', str_c(players, 'x'), str_c(players, 'y')))
  
  if('Px' %in% colnames(sequence)){
    sequence %<>% 
      mutate(Px = removeDuplicates(Px)) %>% 
      mutate(Py = removeDuplicates(Py))
  }
  
  if(interpolate) sequence %<>% cubicSmooth
  
  if(velocity){
    velocity_names = if(cartCords) c('xv', 'yv') else c('mv', 'dv')
    # Get x and y again, in case they were smoothed
    new_x = sequence %>% allPlayerVar('x')
    new_y = sequence %>% allPlayerVar('y')
    
    velocity_cols = rateOfChangeCols(new_x, new_y, players, cartCords, velocity_names)
    
    sequence %<>% bind_cols(velocity_cols)
  }
  
  if(acceleration){
    acc_names = if(cartCords) c('xa', 'ya') else c('ma', 'da')
    # Get the velocity values in cartesian coordinates
    new_x = sequence %>% allPlayerVar('x')
    new_y = sequence %>% allPlayerVar('y')
    
    velocity_xy = rateOfChangeCols(new_x, new_y, players, cartCords = TRUE, c('x', 'y'))
    
    vx = velocity_xy %>% allPlayerVar('x')
    vy = velocity_xy %>% allPlayerVar('y')
    
    acceleration_cols = rateOfChangeCols(vx, vy, players, cartCords, acc_names, in_pixels_per_frame = FALSE, start_rows_to_ignore = 1)
    
    sequence %<>% bind_cols(acceleration_cols)
  }
  
  if(puckToGoal && "P" %in% colnames(data.frame(player_x))){
    puckToRightPipe.v = sqrt((sequence$Px - pipes_x)^2 + (sequence$Py - rpipe_y)^2)
    puckToLeftPipe.v = sqrt((sequence$Px - pipes_x)^2 + (sequence$Py - lpipe_y)^2)
    
    sequence$Prp = puckToRightPipe.v
    sequence$Plp = puckToLeftPipe.v
  }
  
  if(insideDots && "P" %in% colnames(data.frame(player_x))){
    insideDots.v = abs(425 - sequence$Py) < 220
    sequence$Pid = insideDots.v
  }
  sequence
}

# Sequence loading functions
mergeTwoPasses = function(df1, df2){
  # Designate the data frame with more frames in it
  if(nrow(df1) > nrow(df2)){
    larger = df1
    smaller = df2
  } else {
    larger = df2
    smaller = df1
  }
  
  # Insert empty rows into the smaller data frame until they have equal numbers of rows
  if(nrow(smaller) != nrow(larger)){
    smaller[(nrow(smaller) + 1):nrow(larger), ] = NA
    smaller$frames = larger$frames 
  }
  
  # If a variable is in smaller but not larger, add it to larger
  for(colname in colnames(smaller)){
    if(!colname %in% colnames(larger)){
      larger[colname] = smaller[colname]
    }
  }
  
  larger
}

combinePasses = function(folder){
  # Get all rink ICP csv files in folder
  pass_files = list.files(folder, pattern = '*rink_ICP.csv$')
  
  # Read all the csvs
  pass_list = pass_files %>% map(function(x) data.frame(read.csv(str_c(folder, '/', x), header=TRUE, row.names=NULL)))
  
  # Format them
  formatted_passes = map(pass_list, formatSequence, velocity = T, interpolate = T)
  
  # Merge all passes together
  accumulated_passes = formatted_passes[[1]]
  for(pass in formatted_passes){
    accumulated_passes %<>% mergeTwoPasses(pass)
  }
  
  sequence_info = read.csv(str_c(folder, '/info.csv'), header = F)
  
  info_data = list(possessionFrame = sequence_info[1,1], 
                   shotFrame = sequence_info[1,2], outcome = sequence_info[1,3], 
                   handedness = (sequence_info[1,4] == 'R'))
  
  info <<- rbind(info, info_data)
  
  accumulated_passes
}

# Goal shot stats functions

# Whether or not value is within the min and max of bounds. Bounds can be anything 
# with a minimum and maximum
# Value is still inside if distance from bounds is less than lenience
# Uses vectorized operations, so works with vectors
inside = function(value, bounds, inclusive = TRUE, lenience = 0){
  bounds %<>% unlist
  
  if(inclusive){
    is_inside = value >= (min(bounds) - lenience) & value <= (max(bounds) + lenience)
  } else {
    is_inside = value > (min(bounds) - lenience) & value < (max(bounds) + lenience)
  }
  
  is_inside
}
# Returns the angle between two xy points
twoPointsAngle = function(xy1, xy2){
  # Converting to as.numeric lets us pass the object created by frameXY
  xy1 %<>% as.numeric
  xy2 %<>% as.numeric
  
  dx = xy1[1] - xy2[1]
  dy = xy1[2] - xy2[2]
  
  # Angle is the inverse tangent of the hypotenuse created by drawing a line between the two points
  angle = atan3(dy, dx)
  
  angle
}

# Whether or not points are within a certain range of a line
pointOnLine = function(x_column, y_column, slope, intercept, lenience = 0){
  line_distance = map2_dbl(x_column, y_column, function(x, y){
    # The closest point on a line X to a point Y is the one that lies 
    # on the line perpendicular to X that contains Y
    perp_slope = -1/slope
    perp_intercept = y + x/slope
    
    # Find the closest point by taking the intersection of both lines
    intersection_x = (perp_intercept - intercept) / (slope + 1 / slope)
    intersection_y = slope * intersection_x + intercept
    
    # Calculate the distance between the two points
    distance = norm(c(intersection_x, intersection_y) - c(x, y), '2')
    return(distance)
  })
  
  on_line = line_distance <= lenience
  
  return(on_line)
}
# Given an xy vector, the angle to the left and right goal pipe
pipeAngles = function(xy){
  atan3(xy$y - c(lpipe_y, rpipe_y), xy$x - pipes_x)
}
# The x, y, and angles from every player to the puck on the specified frame
playerXYandAnglesToPuck = function(sequence, frame, exclude = c()){
  exclude = append(exclude, 'P')
  
  puck_shot_xy = sequence %>% frameXY('P', frame)
  
  player_x = sequence %>% 
    allPlayerVar('x') %>%
    select(-str_c(exclude, 'x')) %>%
    slice(frame,)
  
  player_y = sequence %>%
    allPlayerVar('y') %>%
    select(-str_c(exclude, 'y')) %>%
    slice(frame,)
  
  player_angles = map2(player_x, player_y, ~ twoPointsAngle(puck_shot_xy, c(.x, .y)))
  
  player_data = cbind.data.frame(player_x %>% as.numeric, player_y %>% as.numeric, player_angles %>% as.numeric, str_sub(colnames(player_x), end = -2)) %>% set_names(c('x', 'y', 'angle', 'player'))
  
  player_data
}
# Return the slope and intercept of a line between two xy points
lineBetween = function(xy1, xy2){
  slope = (xy1$y - xy2$y)/(xy1$x - xy2$x)
  intercept = xy1$y - slope * xy1$x 
  
  list(slope = slope, intercept = intercept)
}

# TRM's new function:
# Find the offensive and defensive players that are blocking the goal.
goalBlockers = function(sequence, frame, shooter){
  puck_shot_xy = sequence %>% frameXY('P', frame)
  
  pipe_angles = pipeAngles(puck_shot_xy)
  
  # Get X,Y location, angle (to ?) and player tag of all players tracked on
  # for the play, but omit the shooter.
  player_data = playerXYandAnglesToPuck(sequence, frame, exclude = shooter)
  
  #print("player_data")
  #print(player_data)
  
  left_pipe_xy = list(x = pipes_x, y = lpipe_y)
  right_pipe_xy = list(x = pipes_x, y = rpipe_y)
  
  # The players inside the triangle are the ones inside the pizza slice top angle, 
  # within the x bounds of the triangle, and within the y bounds of the triangle. 
  # Players are not a single point, so add lenience so they can be inside the triangle
  # if only part of them is.
  
  # In spite of the name of this variable, all players within the
  # pizza slice are returned.
  # A data frame is returned with player x,y, angles to the puck and label.
  # The same applies to left_pipe_defenders, right_pipe_defenders, and
  # defenders data frames.
  inslice_defenders = player_data %>% 
    filter(inside(angle, pipe_angles)) %>% 
    filter(inside(x, c(puck_shot_xy$x, pipes_x), lenience = 50)) %>% 
    filter(inside(y, c(puck_shot_xy$y, lpipe_y, rpipe_y), lenience = 50))
  
  #print("inslice_defenders")
  #print(inslice_defenders)
  
  # Players on left pipe to shot line
  lpipe_line = lineBetween(puck_shot_xy, left_pipe_xy)
  
  left_pipe_defenders = player_data %>% 
    filter(pointOnLine(x, y, lpipe_line$slope, lpipe_line$intercept, 50))
  
  #print("left_pipe_defenders")
  #print(left_pipe_defenders)
  
  # Players on right pipe to shot line
  rpipe_line = lineBetween(puck_shot_xy, right_pipe_xy)
  
  right_pipe_defenders = player_data %>% 
    filter(pointOnLine(x, y, rpipe_line$slope, rpipe_line$intercept, 50))
  
  #print("right_pipe_defenders")
  #print(right_pipe_defenders)
  
  # Consolidate playes in pizza slice with those on the left and right lines from
  # the goal posts to the puck at the point of the shot. Note: There will be
  # redundancies in the three data frames, they are resolved.
  defenders = inslice_defenders %>% union(left_pipe_defenders) %>% union(right_pipe_defenders)
  
  #print("defenders")
  #print(defenders)
  
  shooter_team = str_sub(shooter, end = 1)
  
  # Get the blocking players on the defense.
  blocking_defense <- defenders %>% 
    filter(str_sub(player, end = 1) == shooter_team)
  #print("blocking_defense")
  #print(blocking_defense)
  
  
  # Get the blocking players on the offense.
  blocking_offense <- defenders %>% 
    filter(str_sub(player, end = 1) != shooter_team)
  #print("blocking_offense")
  #print(blocking_offense)
  
  
  blocking_players <- list(blocking_defense,blocking_offense)
  
  #return(c(same_team_defenders, other_team_defenders))
  return(blocking_players)
}


# How many players are in the goalie pizza slice at a given frame
goalDefenders = function(sequence, frame, shooter){
  puck_shot_xy = sequence %>% frameXY('P', frame)
  
  pipe_angles = pipeAngles(puck_shot_xy)
  
  player_data = playerXYandAnglesToPuck(sequence, frame, exclude = shooter)
  
  left_pipe_xy = list(x = pipes_x, y = lpipe_y)
  right_pipe_xy = list(x = pipes_x, y = rpipe_y)
  
  # The players inside the triangle are the ones inside the pizza slice top angle, within the x bounds of the triangle, and within the y bounds of the triangle. Players are not a single point, so add lenience so they can be inside the triangle if only part of them is
  inslice_defenders = player_data %>% 
    filter(inside(angle, pipe_angles)) %>% 
    filter(inside(x, c(puck_shot_xy$x, pipes_x), lenience = 50)) %>% 
    filter(inside(y, c(puck_shot_xy$y, lpipe_y, rpipe_y), lenience = 50))
  
  lpipe_line = lineBetween(puck_shot_xy, left_pipe_xy)
  
  rpipe_line = lineBetween(puck_shot_xy, right_pipe_xy)
  
  left_pipe_defenders = player_data %>% 
    filter(pointOnLine(x, y, lpipe_line$slope, lpipe_line$intercept, 50))
  
  right_pipe_defenders = player_data %>% 
    filter(pointOnLine(x, y, rpipe_line$slope, rpipe_line$intercept, 50))
  
  defenders = inslice_defenders %>% union(left_pipe_defenders) %>% union(right_pipe_defenders)
  
  shooter_team = str_sub(shooter, end = 1)
  
  same_team_defenders = defenders %>% 
    filter(str_sub(player, end = 1) == shooter_team) %>% 
    nrow
  
  other_team_defenders = defenders %>% 
    filter(str_sub(player, end = 1) != shooter_team) %>% 
    nrow 
  
  return(c(same_team_defenders, other_team_defenders))
}

# Returns whether or not the goalie's line of sight is blocked by a player on the shot frame
goalieSightBlocked = function(sequence, shooter, goalie, frame){
  lenience = 50
  
  puck_shot_xy = sequence %>% frameXY('P', frame)
  goalie_xy = sequence %>% frameXY(goalie, frame)
  
  # A goalie's sight is blocked if someone is on the line between the goalie and the puck
  sight_line = lineBetween(goalie_xy, puck_shot_xy)
  
  player_data = playerXYandAnglesToPuck(sequence, frame, exclude = c(goalie, shooter))
  
  # Find all the players who are within a range of the line
  blocking_players = player_data %>% 
    filter(pointOnLine(x, y, sight_line$slope, sight_line$intercept, lenience))
  
  nrow(blocking_players) > 0
}

# Function that takes in a vector of size 2 of x and y and returns distance to the goal
goalDistance = function(xy){
  getDistance(c(xy$x, xy$y), c(pipes_x, ysize/2))
}

# Function that takes in a vector of size 2 of x and y and returns angle to the goal
# Coordinates are in terms of the half rink image
goalAngle = function(xy){
  y_from_goal = xy$x - pipes_x
  x_from_goal = ysize / 2 - xy$y
  
  atan3(y_from_goal, x_from_goal)
}

# Function that takes in a sequence, the shooting player, the goalie, and the frame the shot happened, and returns details about the player on the frame
# Allow either the sequence or sequence index to be specified
goalShotStats = function(sequence_index = -1, frame = info$shotFrame[sequence_index], shooter = 'V1', goalie = 'GH', sequence = sequences[[sequence_index]]){
  # The statistics we want are puck distance and angle from goal, goalie position, player speed, puck speed, possession time, and whether the royal road was crossed
  # Get the puck coordinates and velocity magnitude
  puck_xyv = sequence %>% getPlayerVar('P', c('x', 'y', 'mv'))
  
  puck_shot_xy = frameXY(sequence, 'P', frame)
  # And use them to get distance and angle to goal
  puck_dist = goalDistance(puck_shot_xy)
  puck_angle = goalAngle(puck_shot_xy)

# For puck speed, get an average of velocity magnitude over the next 3 frames
  puck_speed = mean(puck_xyv$Pmv[frame:(frame+3)], na.rm = TRUE)
  # For player speed, get an average of the surrounding 5 frames
  shooter_mv = sequence %>% getPlayerVar(shooter, 'mv')
  shooter_speed = shooter_mv[(frame-2):(frame+2), ] %>% unlist %>% mean
  
  possession_time = frame - info$possessionFrame[sequence_index]
  
  # Get goalie distance and angle same way for puck
  goalie_xy = sequence %>% getPlayerVar(goalie, c('x', 'y'))
  
  goalie_shot_xy = frameXY(sequence, 'GH', frame)
  
  goalie_dist = goalDistance(goalie_shot_xy)
  goalie_angle = goalAngle(goalie_shot_xy)
  
  goal_defenders = goalDefenders(sequence, frame, shooter)
  same_team_defenders = goal_defenders[1]
  opp_team_defenders = goal_defenders[2]
  
  # TRM: added code to find closest defender and calculate distance from the point of the shot.
  # Get the locations, angles to the puck and player label of all players within 
  # the pizza slice. The function goal_blockers() is new (#TRM)
  goal_blockers = goalBlockers(sequence, frame, shooter)
  
  #print("goal_blockers")
  #print(goal_blockers)
  
  blk_off <- goal_blockers[[1]]
  blk_def <- goal_blockers[[2]]
  
  #print("def blockers:")
  #print(blk_off)
  #print(blk_def)
  
  pk_x <- as.numeric(puck_shot_xy[1])
  pk_y <- as.numeric(puck_shot_xy[2])
  
  #print("pk_x")
  #print(pk_x)
  #print("pk_y")
  #print(pk_y)
  
  # Find the nearest defensive player to the point of the shot who is within
  # the pizza slice. 
  goalie_sight_blocked = goalieSightBlocked(sequence, shooter, goalie, frame)
  
  num_def_plrs <- length(blk_def[,1])
  #print("num_def_plrs")
  #print(num_def_plrs)
  
  # Initialize distance and label variables. If these values show-up
  # in the final data frame, their rows should be cleaned before 
  # starting analytics.
  shortest_dist <- 100000
  closest_angle <- 360
  closest_def <- ''
  
  if(num_def_plrs > 0){
    for(iplyr in 1:num_def_plrs){
      #print("label: blk_def[iplyr,4]")
      #print(blk_def[iplyr,4])
      angle <- blk_def[iplyr,3]
      label <- blk_def[iplyr,4]
      def_x <- as.numeric(blk_def[iplyr,1])
      def_y <- as.numeric(blk_def[iplyr,2])
      delta_x <- def_x - pk_x
      delta_y <- def_y - pk_y
      def_dist <- sqrt((delta_x * delta_x) + (delta_y * delta_y))
      if(def_dist < shortest_dist){
        shortest_dist <- def_dist
        closest_def <- label
        closest_angle <- angle
      }
    }
  }
  
  #print("def player shortest distance from shot:")
  #print(shortest_dist)
  #print(typeof(puck_speed))
  
  # TRM: end added code  
  
  handedness = info$handedness[sequence_index] %>% as.numeric
  
  # result.df = data.frame(puck_dist, puck_angle, puck_speed, shooter_speed, goalie_dist, goalie_angle, possession_time, same_team_defenders, opp_team_defenders, goalie_sight_blocked, handedness) %>% 
  #   set_names(c('puckDist', 'puckAngle', 'puckSpeed', 'shooterSpeed', 'goalieDist', 'goalieAngle', 'posTime', 'sameDefenders', 'oppDefenders', 'goalieScreened', 'rightHanded'))

  # NEW (TRM): Get shot outcome and add to the output df
  shot_outcome <- info$outcome[sequence_index]
  
  # NEW (TRM): change GB (goalie block) to S (save)
  if(shot_outcome == "GB"){shot_outcome <- 'S'}
  
  # NEW (TRM): Added closest defender label, angle and distance. 
  # Changed same_team_defenders and opp_team_defenders data frame column titles to NumOffense and NumDefense. 
  result.df <- data.frame(puck_dist, puck_angle, puck_speed, shooter_speed, 
                          goalie_dist, goalie_angle, possession_time, same_team_defenders, 
                          opp_team_defenders, handedness, closest_def, shortest_dist, 
                          closest_angle, shot_outcome) %>% 
    set_names(c('puckDist', 'puckAngle', 'puckSpeed', 'shooterSpeed', 
                'goalieDist', 'goalieAngle', 'posTime', 'NumOffense', 
                'NumDefense', 'rightHanded', 'closestDef', 'defDist', 
                'defAngle', 'shotOutcome'))
  
  result.df
}

# Goal shot model functions
factorBool = function(fact){
  fact %>% as.numeric %>% '-'(1) %>% as.logical
}

gradePRAUC=function(pred, goals){
  PRAUC(pred, goals)
}

gradeBalAcc = function(classpred, silent = FALSE){
  truepos = sum(classpred[1,] & classpred[2,])
  falsepos = sum(classpred[1,] & !classpred[2,])
  trueneg = sum(!classpred[1,] & !classpred[2,])
  falseneg = sum(!classpred[1,] & classpred[2,])
  
  specificity = truepos/(truepos + falseneg)
  sensitivity = trueneg/(trueneg + falsepos)
  
  if(!silent){
    print(str_c('Specificity: ', specificity)) 
    print(str_c('Sensitivity: ', sensitivity)) 
  }
  
  (specificity + sensitivity) / 2
}

predictedProbs = function(model, validation){
  kind = class(model)
  
  if('randomForest' %in% kind){ pred = (predict(model, validation, type = 'prob')[,2]) }
  if('lda' %in% kind){ pred = (predict(model, validation)$posterior[,2])}
  if('lognet' %in% kind) { pred = predict(model, validation %>% as.matrix, s = 0, type = 'response') %>% as.numeric}
  if('svm' %in% kind) { pred = (predict(model, validation, probability = TRUE) %>% attr('probabilities'))[,2] }
  
  pred
}

makeModel = function(model_func, features, test_train = FALSE){
  train = t_shots.df[,features]
  valid = v_shots.df[,features]
  
  if(sum(features) < 2){
    return(list(acc = NA, model = NA, class = NA, features = NA))
  }
  
  # Black magic trick to see if functions match
  if(suppressWarnings(all(deparse(model_func) == deparse(glmnet)))){
    model = glmnet(train,t_goals,family="binomial",alpha=1,nlambda=500,standardize=FALSE)
  } else {
    model = model_func(t_goals ~ ., train) 
  }
  
  test_set = if(test_train) train else valid
  goals = if(test_train) t_goals else v_goals
  
  pred = predictedProbs(model, test_set)
  
  classpred = rbind(pred > 0.5 %>% as.numeric, factorBool(goals) %>% as.numeric)
  
  list(acc = gradeBalAcc(classpred, silent = TRUE), 
              prauc = gradePRAUC(pred, goals), model = model, class = classpred, 
              features = features, func = model_func)
}

torad = function(deg){
  deg * pi / 180
}

# X and Y values for plotting on half rink image
shotStatX = function(shots_stats){
  cos(torad(shots_stats$puckAngle)) * shots_stats$puckDist + 425
}

shotStatY = function(shots_stats){
  1890 - sin(torad(shots_stats$puckAngle)) * shots_stats$puckDist
}

standardized_shot = function(shots_stats, xy){
  puck_dist = goalDistance(xy)
  puck_angle = goalAngle(xy)
  
  result.m = colMeans(shots_stats)
  result.m['puckDist'] = puck_dist
  result.m['puckAngle'] = puck_angle
  
  result.m %>% as.list
}

standardized_shot_odds = function(x, y, shots_stats, model){
  shot = standardized_shot(shots_stats, list(x = x, y = y)) 

  score = predictedProbs(model, shot[1:2] %>% as.data.frame)
  
  score
}

halfRinkGraph = function(data){
  ggplot(data) +
    ylim(2000, 1000) +
    xlim(0,ysize) +
    annotation_custom(half_rink_raster, xmin=0, xmax=ysize, ymin=-2000, ymax=-1000) +
    theme(axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

corrMatrix = function(data1, data2 = data1, thresh){
  correlations = cor(data1, data2)
  
  correlations.df = correlations %>% 
    melt %>% 
    mutate(value = modify_if(value, ~ near(., 1), ~ NA))
  
  corPval = function(x, y) {
    cor.test(data1[,x], data2[,y])$p.value }
  
  pvals = outer(1:ncol(data1), 1:ncol(data2), Vectorize(corPval))
  significant = pvals %>% 
    'dim<-'(NULL) < thresh
  
  ggplot(correlations.df) +
    geom_tile(aes(x = Var1, y = Var2, fill = value)) +
    geom_point(aes(x = Var1, y = Var2, color = significant)) +
    labs(x = NULL, y = NULL, fill = 'Correlation', color = 'Significant') +
    scale_fill_gradientn(colors = c('red', 'white','green'), 
                         values = rescale(c(min(correlations), 0, max(correlations))), na.value = 'black') +
    scale_color_discrete(type = c('cyan', 'black')) +
    theme(axis.text.x = element_text(angle = -60, hjust = 0))
}

# Splits kables into multiple to be printed separately
splitKable = function(data, splits, sigfigs = getOption('digits')){
  progress = 1
  width = ceiling(ncol(data) / splits)
  for(i in 1:splits){
    next_progress = progress + width
    # Progress - 1 so we don't double count any columns
    # Minimum so we don't go past the last column
    split_data = data[,progress:min(next_progress - 1, ncol(data))]
    # Digits specifies 
    print(kable(split_data, digits = sigfigs))
    
    progress = next_progress
  }
}
