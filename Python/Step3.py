
# -----------------------------------------------------------------------
# Step3_Tracking_GUI
#
# The final step in the player identification and trackinhg application
# suite. This is a Graphical User Interface (GUI) driven application that
# permits the user to identify on-ice objects including players, referees,
# and the puck to be tracked. On susequent frames automatic location of
# all identified on-ice objects is attempted and the user is given the
# opportunity to alter matches with objects on the previous frame or to
# re-identify any that have been missed by the auto-tracker. New objects
# entering the field of view can also be identified for tracking. 
#
# Input: one instance of empty_ice_subtracted_subseqence_of_on-ice_action_video.MP4
# Input: player_skeletons_video.MP4
#
# Output: for the sequence, tracked_on-ice_objects_video.MP4
# Output: object ice-contact point tracks on the rink view
# Output: a player_silhouette_video.MP4 for each on-ice object
# 
# Initial create: 6 March, 2023 by T. Morgan
# -----------------------------------------------------------------------
# import packages
# ----------------------------------------------------------------------
import numpy as np
import copy
from numpy import array
import math
import argparse
import cv2,os,sys
import csv
from moviepy.editor import *
from bisect import bisect
import os
import copy

import pdb

# CHANGES HERE:
# Creation of track temp and rink temp folders
# Using 'o' to see unmarked image
# Uncomment print process frame number
# Change in how rink ICP is calculated manually (click on the ice contact point to get the correct rink ICP value)

assert len(sys.argv) == 4, "Usage: Project name, sequence number, pass number"

project_name = sys.argv[1]
seq_num = sys.argv[2]
pass_num = sys.argv[3]

seq_filepath = f'./{project_name}/Sequences/Files_Seq_{str(seq_num)}'
seq_file = f'{seq_filepath}/Seq_{str(seq_num)}.mp4'
sub_file = f'{seq_filepath}/subs.mp4'
rink_file = './Rink.jpeg'

cap_seq = cv2.VideoCapture(seq_file)
cap_sub = cv2.VideoCapture(sub_file)
rink_im = cv2.imread(rink_file)

assert cap_seq.isOpened(), f'{seq_file} does not exist'
assert cap_sub.isOpened(), f'{sub_file} does not exist'
# If rink file is not present, rink_im will be none
assert rink_im is not None, f'{rink_file} does not exist'

rink_temp_dir = os.path.join(seq_filepath, 'Rink_Temp')
track_temp_dir = os.path.join(seq_filepath, 'Track_Temp')

if not os.path.exists(rink_temp_dir):
   os.makedirs(rink_temp_dir)

if not os.path.exists(track_temp_dir):
   os.makedirs(track_temp_dir)

# Generic image display function
def im_display(image, title, scale=1, x_move=0, y_move=0):
    image_sc = cv2.resize(image, (0,0), fx=scale, fy=scale)
    cv2.imshow(title, image_sc)
    cv2.moveWindow(title,x_move,y_move)
    cv2.waitKey(0)

# Returns the list of the y coordinates of where the categories start on the UI. Has an extra term at the end representing the end of the UI
def makeYstarts():
    total = 0
    y_starts = [0]

    for item in category_sizes:
        total += item * box_height + box_height
        y_starts.append(total)

    return y_starts

blue_color = (255,0,0)
green_color = (0,255,0)
red_color = (0,0,255)
white_color = (255,255,255)
black_color = (0,0,0)
magenta_color = (255,0,255)
cyan_color = (255,255,0)
yellow_color = (0,255,255)

hot_key = False

# Which category is being selected
category_select = None

categories = ['visitor', 'home', 'goalie', 'referee', 'puck']
# The color associated with marking for each category
mark_colors = [magenta_color, red_color, green_color, yellow_color, black_color]
# Number of objects in each category
category_sizes = [5, 5, 2, 4, 0]
# The x coordinate of the main category and object buttons
category_button_x = 80
object_button_x = 60
# The height of each box in the ui
box_height = 25
# The x coordinate category and object names start at
category_name_x = 10
object_name_x = 25
# The y coordinates the categories start at
category_y_starts = makeYstarts()
# The highest number of object that has been tracked
active_counters = [0,0,0,0,0]
# The UI
man_picks_im = np.zeros((max(category_y_starts),100,3), dtype=np.uint8)
# The index of the object to be marked
nf_index = -1

# List of final center of moments (CM) coordinates. It has a copy of all on-ice
# objects (players, refs, puck) for each frame processed.
session_obj_CM_image_coords = []

# List of final ice contact point image pixel coordinates. It has a copy of all on-ice
# objects (players, refs, puck) for all frames processed.
session_obj_ICP_image_coords = []

# List of final ice contact point rink x,y coordinates. It has a copy of all on-ice
# objects (players, refs, puck) for all frames processed.
session_obj_ICP_rink_coords = []

# Initialize list to store object pixel coordinates for the current frame.
# These will start with the user marked points and then be updated to
# CMs. It is the size of one copy of all on-ice objects (players, goalies, refs, puck).
frame_obj_CM_image_coords = []

# List of final ice-object rink x,y contact points for current frame. One entry for each on-ice object.
frame_obj_ICP_rink_coords = []

# List of final ice-object image pixel contact points for current frame. One entry for each on-ice object.
frame_obj_ICP_image_coords = []

# List of clicked coordinates for objects for current frame
frame_obj_click_coords = []

# List of last known player positions plus frame
player_positions = []

def dotproduct(v1, v2):
  return sum((a*b) for a, b in zip(v1, v2))

def length(v):
  return math.sqrt(dotproduct(v, v))

def calculate_angle(v1, v2):
    radians = math.acos(dotproduct(v1, v2) / (length(v1) * length(v2)))
    return (math.degrees(radians))

def vectorize(point1, point2):
    print("point 1: ", point1)
    print("point 2: ", point2)
    if(point1 == point2):
        print("SAME POINTS")
        return None

    return np.array([point1[0]-point2[0], point1[1]-point2[1]])


def calculate_projected_angle(session_obj_CM_image_coords, iVHRP, i_ent, player_positions): 
    # Check how many frames the player has been missed, how many total frames have gone by 
    missed_frames = player_positions[iVHRP][i_ent][0]
    current_player_position =  player_positions[iVHRP][i_ent][1]
    n_frames = len(session_obj_CM_image_coords)


    #pdb.set_trace()
    one_previous_frame_position = session_obj_CM_image_coords[n_frames-2][iVHRP][i_ent]
    two_previous_frame_position = session_obj_CM_image_coords[n_frames-3][iVHRP][i_ent]
    # This is a special case when the player is first marked it wont have any previous positions
    # Retrun 360 degrees and check all around the player for next marking 
    #pdb.set_trace()
    print("two previous frames: ", two_previous_frame_position)
    print("one previous frame: ", one_previous_frame_position)
    print("current position ", player_positions[1][0])
    if(one_previous_frame_position == (-1,-1) or two_previous_frame_position == (-1,-1)):
        return 360
    # Vectorize the two points and find the difference in angle
    else: 
        one_previous_to_current_vector = vectorize( one_previous_frame_position, current_player_position)
        two_previous_to_one_previous = vectorize(two_previous_frame_position, one_previous_frame_position)
        angle = calculate_angle(one_previous_to_current_vector, two_previous_to_one_previous)
        print("angle: ", angle)


# Function to initialize the lists. The lists contain lists representing each category, and those lists contain the coordinates for each object
def initialize_list(list_name):
    list_name.clear()

    for size in category_sizes:
        empty_list = [(-1, -1) for i in range(size)]

        if size == 0: empty_list = [(-1, -1)]

        list_name.append(empty_list)
    # Space for frame and pass number
    list_name.append([(-1, -1)])

initialize_list(frame_obj_CM_image_coords)
initialize_list(frame_obj_ICP_image_coords)
initialize_list(frame_obj_ICP_rink_coords)
initialize_list(frame_obj_click_coords)
initialize_list(player_positions)

# Update the frame and ui windows
def updateScreens():
    cv2.imshow("mark the puck and players", man_picks_im)
    cv2.imshow("Image Window", frame)

# Get ready for mouse input
def setMouseCallback(nf = False):
    ui_func = nf_UI_click_mouse if nf else ff_UI_click
    mark_func = nf_Mark_click if nf else ff_Mark_click

    cv2.setMouseCallback("mark the puck and players", ui_func)
    cv2.setMouseCallback("Image Window", mark_func)

# Returns the size of a category
def categorySize(category):
    category_index = categories.index(category)

    return category_sizes[category_index]

# Gives the center of the circle for a given player category and index of player (0 means the dot next to just the category name)
def selectCircleCenter(category, index = 0):
    category_index = categories.index(category)

    y = category_y_starts[category_index] + box_height * index + box_height // 2

    if index == 0: x = category_button_x
    else: x = object_button_x

    return x, y

# In the man_picks_im ui, reset the category buttons to green
def resetCategoryButtons():
    # Get the center of every category circle
    category_centers = [selectCircleCenter(category) for category in categories]

    for center in category_centers: cv2.circle(man_picks_im, center=center, radius=8, color=green_color, thickness=-2) 

# Returns the chosen category and turns the corresponding circle red
def choosecategoryAndColorChoice(select_y):
    for box_start, category in zip(category_y_starts, categories):
        # Check if the y value is within the box
        y_min = box_start

        if category == 'goalie': y_max = box_start + box_height * (categorySize('goalie') + 1)
        else: y_max = box_start + box_height

        if select_y >= y_min and select_y <= y_max:
            index = categoryAndIndexFromY(select_y)[1]

            circle_center = selectCircleCenter(category, index)
            # Mark the circle red
            cv2.circle(man_picks_im, center=circle_center, radius=8, color=green_color, thickness=-2) 
            cv2.circle(man_picks_im, center=circle_center, radius=5, color=red_color, thickness=-2)
            # Return the choice
            return category, index

    # Return None if the click wasn't on a box
    return None, -1

# Objects that should be green are made green
def setObjectColors():
    global category_select, nf_index
    for cat_ind, category in enumerate(categories):
        for i in range(category_sizes[cat_ind]):
            # Y of the category start, plus the category box, plus one box for every object, then start halfway down the next box
            circle_center = selectCircleCenter(category, i + 1)

            # All objects up to the last one selected are turned green
            if i < active_counters[cat_ind]:
                cv2.circle(man_picks_im, center=circle_center, radius=8, color=green_color, thickness=-2)

            # The currently selected object is turned green
            if (i + 1) == nf_index and category == category_select:
                cv2.circle(man_picks_im, center=circle_center, radius=8, color=green_color, thickness=-2)

# ---------------------------------------------------------------------------
# UI object identification panel.
# First frame selection of on-ice object category to track (visitor, home,
# referee, puck). Once a category is selected, cursor location selections
# on the main image will result in the addition of an object to the category
# until all slots for the category are exhausted (6 for visitor and home, 4
# for referee, 1 for puck) or another category is selected on the panel.
#
# First frame cursor click event handler. This frame requires manual marking
# of all on-ice players and refs plus the puck.
# ---------------------------------------------------------------------------

# Set the category based on a click in the man_picks_im ui
def ff_UI_click(event, x, y, flags, param):
    global category_select, nf_index
            
    # If the left mouse button wasn't released, do nothing
    if event != cv2.EVENT_LBUTTONUP: return

    # New category selected - reset all catgory buttons
    resetCategoryButtons()

    # Update the selected category
    selected_category, selected_index = choosecategoryAndColorChoice(y)
    if selected_category is not None:
        category_select = selected_category
        nf_index = selected_index

    # And update the windows
    updateScreens()

# Given a category and object index, return the identifier for that object
def objectName(category, object_index):
    if category == 'goalie':
        # The first goalie is the home goalie, the second the away
        name = ('GH' if object_index == 1 else 'GV')
    elif category == 'puck':
        name = 'Puck'
    else:
        # First letter of category name uppercased plus the index (e.g. 'H1')
        name = category[0].upper() + str(object_index)

    return name

# Returns the color of the marking for the given category
def categoryColor(category):
    category_index = categories.index(category)

    return mark_colors[category_index]

def ff_Mark_click(event, x, y, flags, param):
    global category_select, nf_index

    # If category select hasn't been set yet
    if category_select == None: return

    # If the left mouse button wasn't released, do nothing
    if event != cv2.EVENT_LBUTTONUP: return

    # Set the position of the object label centered below the CM
    label_delta_x = -10
    label_delta_y = 20

    counter_index = categories.index(category_select)

    if category_select == 'goalie':
        active_counters[counter_index] = nf_index
    # Add 1 to active counter, so when we next click it's the next player, unless we're marking the puck
    elif category_select != 'puck':
        active_counters[counter_index] += 1    

    object_index = active_counters[counter_index]

    player_limit = categorySize(category_select)

    # When we go to the next frame, start from where we ended unless we're now over the player limit
    nf_index = min(object_index, player_limit)

    # If we've already exhausted the list, don't mark anything
    if object_index > player_limit: return

    # Store the coordinates
    frame_obj_CM_image_coords[counter_index][object_index - 1] = (x, y)
    frame_obj_click_coords[counter_index][object_index - 1] = (x, y)

    # Place the marking circle
    mark_color = categoryColor(category_select)
    cv2.circle(frame, center=(x, y), radius=5, color=mark_color, thickness=-2)

    # Put the object name on the marking
    name = objectName(category_select, object_index)
    cv2.putText(frame, name, ((x + label_delta_x),(y + label_delta_y)), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color,2)

    # Set the object we just marked to be green
    setObjectColors()

    updateScreens()

# Given a key press, match it with the correct category
def keyToCategory(key):
    match chr(key):
        case 'v':
            return 'visitor'
        case 'r':
            return 'referee'
        case 'h':
            return 'home'
        case 'p':
            return 'puck'
        case 'g':
            return 'goalie'
    # If given anything unspecified, returns none
    return None

def keyToIndex(key, category):
    # Specifying for goalie
    if key == ord('h') and category == 'goalie': index = 1
    elif key == ord('v') and category == 'goalie': index = 2
    # Otherwise, just take a number
    else: index = key - ord('0')

    # If value is out of bounds, return none
    if index > categorySize(category) or index < 1: return None

    return index

# Given the y location of a click on the UI, returns the proper category and index
def categoryAndIndexFromY(y):
    # Index is where the y value would go if it was sorted into the list, minus 1
    # This gives us the largest value y is larger than. Basically what category we clicked on
    category_index = bisect(category_y_starts, y) - 1

    category = categories[category_index]

    # We get how many boxes down were clicked by taking y distance from the category start and dividing by 25, then truncating
    remainder = y - category_y_starts[category_index]
    object_index = remainder // box_height

    return category, object_index

def updateSelectObjects():
    global category_select, nf_index
    # Set the puck button to green
    cv2.circle(man_picks_im, center=selectCircleCenter('puck'), radius=8, color=green_color, thickness=-2)

    # Make sure objects are colored correctly
    setObjectColors()

    # Turn on object select button and give it a blue dot
    circle_center = selectCircleCenter(category_select, nf_index)
    cv2.circle(man_picks_im, center=circle_center, radius=8, color=green_color, thickness=-2) 
    cv2.circle(man_picks_im, center=circle_center, radius=5, color=blue_color, thickness=-2)

    updateScreens()

def nf_UI_click_key(key):
    global category_select, nf_index
    # Take key input for category
    key_category = keyToCategory(key)

    # Check if key category is a valid category
    if key_category == None: return

    category_select = key_category

    # If the category is puck, we know what needs to be marked already
    if key_category == 'puck':
        nf_index = 0
    else:
        # Take new key input for index
        index_key = cv2.waitKey(0) & 0xFF
        key_index = keyToIndex(index_key, category_select)

        # Check if key index is valid
        if key_index == None: return

        nf_index = key_index

    updateSelectObjects()

def nf_UI_click_mouse(event, x, y, flags, param):
    global category_select, nf_index
    
    # If not a left click release, return
    if event != cv2.EVENT_LBUTTONUP: return

    category_select, nf_index = categoryAndIndexFromY(y)

    # If we just clicked on a non puck category and not an object, do nothing
    if nf_index == 0 and category_select != 'puck': return

    updateSelectObjects()

def nf_Mark_click(event, x, y, flags, param):
    global category_select, nf_index, frame_obj_CM_image_coords

    # If not a left click, return
    if event != cv2.EVENT_LBUTTONUP: return

    # Set the position of the object label centered below the CM
    label_delta_x = -10
    label_delta_y = 20

    counter_index = categories.index(category_select)

    # If the new object is of a higher number than the highest before, set the active counter to it. Otherwise it stays the same
    active_counters[counter_index] = max(active_counters[counter_index], nf_index)

    frame_obj_CM_image_coords[counter_index][nf_index - 1] = (x, y)
    frame_obj_click_coords[counter_index][nf_index - 1] = (x, y)

    name = objectName(category_select, nf_index)

    if category_select == 'puck':
        label_delta_x = 10
        label_delta_y = 10
    else:
        cv2.circle(man_picks_im, center=selectCircleCenter(category_select, nf_index), radius=8, color=green_color, thickness=-2)

    mark_color = categoryColor(category_select)

    cv2.circle(frame, center=(x, y), radius=5, color=mark_color, thickness=-2)

    cv2.putText(frame, name, ((x + label_delta_x),(y + label_delta_y)), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

    updateScreens()

# Draws the white lines that separate boxes onto the ui
def UIwhiteLines():
    line_height = 0
    xmax = man_picks_im.shape[1]

    while line_height < man_picks_im.shape[0]:
        cv2.line(man_picks_im, (1, line_height), (xmax, line_height), white_color, thickness=2)

        line_height += box_height

    return man_picks_im

# Draws the dots and writes the text for the categories onto the ui
def UIcategories():
    for category in categories:
        # Create the green dots
        circle_center = selectCircleCenter(category)
        cv2.circle(man_picks_im, center=circle_center, radius=8, color=green_color, thickness=-2)        

        # Write the category names
        name_y = circle_center[1] + box_height // 4
        cv2.putText(man_picks_im, category, ((category_name_x),(name_y)), cv2.FONT_HERSHEY_SIMPLEX, 0.5, red_color, 1)

    return man_picks_im

# Create and set the buttons and names for each object in each category
def UIobjects():
    global category_select, nf_index, frame_num, start_frame
    for cat_ind, category in enumerate(categories):
        for i in range(category_sizes[cat_ind]):
            # Y of the category start, plus the category box, plus one box for every object, then start halfway down the next box
            circle_center = selectCircleCenter(category, i + 1)

            # Create the white circle for the object
            cv2.circle(man_picks_im, center=circle_center, radius=8, color=white_color, thickness=-2)

            # Write the object names
            name = objectName(category, i + 1)
            text_y = circle_center[1] + box_height // 4
            cv2.putText(man_picks_im, name, ((object_name_x),(text_y)), cv2.FONT_HERSHEY_SIMPLEX, 0.5, red_color, 1)

    setObjectColors()

    # Reset the last chosen object to have a dot
    if category_select is not None:
        circle_center = selectCircleCenter(category_select, nf_index)
        circle_color = red_color if frame_num == start_frame else blue_color

        cv2.circle(man_picks_im, center=circle_center, radius=5, color=circle_color, thickness=-2)    


def buildUI():
    UIwhiteLines()

    UIcategories()

    UIobjects()
# -------------------------------------------------------------------------------------
# CV2_GUI_IoU: cv2 based graphical user interface with player contour intersection
# over union frame to frame tracking. Manual marking of players and referees on the
# first frame. IoU tracking to the next frame with user fix-up available. User 
# marking of the puck on each frame.
# -------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------------------
# various trial filters for bit map erosion
# ----------------------------------------------------------------------
kernel0 = array([[1,1],
                [1,1]],dtype=np.uint8)

kernel1 = array([[1,1,1],
                [1,1,1],
                [1,1,1]],dtype=np.uint8)

kernel2 = array([[1,1,1,1],
                [1,1,1,1],
                [1,1,1,1],
                [1,1,1,1]],dtype=np.uint8)

kernel3 = array([[1,1,1,1,1],
                [1,1,1,1,1],
                [1,1,1,1,1],
                [1,1,1,1,1],
                [1,1,1,1,1]],dtype=np.uint8)

kernel4 = array([[1,1,1,1,1,1],
                [1,1,1,1,1,1],
                [1,1,1,1,1,1],
                [1,1,1,1,1,1],
                [1,1,1,1,1,1],
                [1,1,1,1,1,1]],dtype=np.uint8)


# list of image filenames
image_files = []

# list of rink overhead view filenames
rink_files = []

# get rink image number of rows and columns
rink_rows,rink_cols,rink_channels = rink_im.shape
print("rink_rows,rink_cols:",rink_rows,rink_cols)

xy_scale = 1.0

# ----------------------------------------------------------------------
# Set processing parameters
# ----------------------------------------------------------------------

stop_marking = False

# Initialize active select
# catagory_select = "none"

# Keypoints acceptance threshold. Can find more points by making smaller.
# Orginal from internet example
threshold = 0.1

#threshold = 0.01
#threshold = 0.15

# GetValidPairs parameters
n_interp_samples = 10
#n_interp_samples = 5

# changing this does not seem to do anything
paf_score_th = 0.1
#paf_score_th = 0.01

# Confidence threshold for determining if a body part pairing is valid.
# 0.7 means that 70% of the points on the interpolated line between the
# body parts align with the PAF.
#conf_th = 0.7
conf_th = 0.6

#conf_th = 0.01

# Minimum number of body part pairs for acceptance as a skeleton
min_pairs = 2

# Set minimum IoU core for match of previous with current frame skeletons.
min_IoU_score = 0.25

# Set maximum frame to frame skeleton distance for match of previous
# with current frame skeletons.
max_ff_dist = 30.0

# ----------------------------------------------------------------------
# Display parameters
# -----------------------------------------------------------------------
# Scaling factor for displayed image so it fits nicely on the screen.
#xy_scale = 1.5

# 2000 by 850
#xy_scale = 0.5

xy_scale = 1.0

# -----------------------------------------------------------------------
# Frame processing parameters.
# -----------------------------------------------------------------------
# absolute frame number counter
frame_num = 1

# Open the file of the action video (MP4)
cap = cv2.VideoCapture(seq_file)
print("Is input file open:",seq_file,cap.isOpened())

# Initialize marking counter
mark_count = 0

# List of final on-ice object labels, One entry for each on-ice object.
object_labels = ['V1', 'V2', 'V3', 'V4' , 'V5',
                'H1', 'H2', 'H3', 'H4', 'H5',
                'GH', 'GV',
                'R1', 'R2', 'R3', 'R4',
                'Puck']

# read next frame
start_frame = 1

frame_num = 1

while cap_seq.isOpened():
    # NOTE: cv2 reads images as B,G,R
        
    # Read separate file of input images.
    # These will be used as background for marking display.
    ret_frame, frame = cap_seq.read()

    # Create a deep copy so that the unmarked frame can be seen
    orig_frame = copy.deepcopy(frame)

    # Read separate file of input images that have been background
    # subtracted and masked.
    # These will be used by findContours to locater on-ice objects.
    ret_sub, sub_image = cap_sub.read()
      

    # There was a next frame
    if ret_frame:
        if(frame_num >= start_frame):
            print("process frame:",frame_num)

            # Put the tracking pass number and frame number in its slot at the 
            # end of the coordinates lists.
            # frame_obj_CM_image_coords list that will later be written to a CSV file (minus 1 because we move on a frame before recording the last)
            frame_obj_CM_image_coords[5][0] = (int(pass_num), frame_num - 1)
            #print("frame:",frame_num,"obj CMs from cursor picks:",frame_obj_CM_image_coords)

            # frame_obj_ICP_image_coords list that will later be written to a CSV file.
            frame_obj_ICP_image_coords[5][0] = (int(pass_num), frame_num - 1)

            # frame_obj_ICP_rink_coords list that will later be written to a CSV file.
            frame_obj_ICP_rink_coords[5][0] = (int(pass_num), frame_num - 1)
           
            #im_display(frame,"input frame", xy_scale)
            
            raw_rows,raw_cols,raw_channels = frame.shape

            # On first frame, setup camera to rink view transform matrix.
            if(frame_num == start_frame):
                print("Input image rows and cols:",raw_rows,raw_cols)
                # This set of values is for 1920 by 1080 pixel images.
                # Image coordinates captured on-screen with the cursor.
                if(raw_rows == 1080):
                    UL_src = (746,250)
                    LL_src = (56,970)

                    UR_src = (1174,250)
                    LR_src = (1860,982)


                # This set of values is for 1280 by 720 pixel images.
                # Image coordinates captured on-screen with the cursor.
                elif(raw_rows == 720):
                    UL_src = (487,168)
                    LL_src = (47,565)

                    UR_src = (795,168)
                    LR_src = (1224,565)

                # Rink template coordinates
                # Far goal line left and right ends.
                UL_dest = (110,850)
                LL_dest = (1960,850)

                # Near goal line left and right ends.
                UR_dest = (110,0)
                LR_dest = (1960,0)

                # calculate the warping matrix
                dest_pts = np.float32([UL_dest, UR_dest, LL_dest, LR_dest])
                orig_pts = np.float32([UL_src, UR_src, LL_src, LR_src])
                H_warp = cv2.getPerspectiveTransform(orig_pts, dest_pts)

                

                # We will not warp the endzone view image because of the down-ice
                # perspective. It causes too much distortion and blows-up the
                # foreground mesh of the protective screen. Instead, we will use
                # the matix values to directly transform ice contact point pixel
                # coordinates to rink coordinates for plotting on the overhead
                # rink template.

                M11 = H_warp[0][0]
                M12 = H_warp[0][1]
                M13 = H_warp[0][2]
                #print("M11,M12,M13:",M11,M12,M13)

                M21 = H_warp[1][0]
                M22 = H_warp[1][1]
                M23 = H_warp[1][2]
                #print("M21,M22,M23:",M21,M22,M23)

                M31 = H_warp[2][0]
                M32 = H_warp[2][1]
                M33 = H_warp[2][2]
                #print("M31,M32,M33:",M31,M32,M33)

            # End of transform matrix calculations.

            # Grab a copy of the input frame to use for silhouette extraction
            silh_frame = sub_image.copy()

            #print("raw image frame x cols by y rows; chans:",raw_cols,raw_rows,raw_channels)
            #xy_scale = 0.5
            #im_display(frame,"input frame", xy_scale)
            #xy_scale = 1.0
            #im_scaled = cv2.resize(frame, (0,0), fx=1.0, fy=1.0)
           
            # Don't warp the endzone view. Too much distortion that results in a very
            # wide range of contour sizes that cannot be easily screened. 
            #frame = cv2.warpPerspective(frame, H_warp, (2000, 850), borderValue=white_color)
   
            # Re-display the category and object selection panel.
            # Copy of input frame for redisplay of location picks.

            buildUI()
            # ----------------------------------------------------------------------------
            # Prepare the input frame for on-ice object (OIO) Bounding Box (BB) discovery.
            # ----------------------------------------------------------------------------
            raw_rows,raw_cols,raw_channels = frame.shape
            #print("raw image frame x cols by y rows; chans:",raw_cols,raw_rows,raw_channels)
            #print("frame type:")
            #print(type(frame))
            #print("frame min,max:",np.amin(frame),np.amax(frame))
   
            # Note: Now producing sub_im_inv in Step 2 and reading in Step 3.
            #im_display(sub_image,"sub_image", 0.7)
            
            sub_rows,sub_cols,sub_channels = sub_image.shape
            #print("subtracted frame x cols by y rows; channels:",sub_cols,sub_rows,sub_channels)
            #im_display(sub_image,"sub_image", 0.7)

            if(raw_rows == 1080):
                points = np.array([[0, 0], [1920, 0], [1920, 200], [0, 200]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))

                # Left upper triangle
                points = np.array([[0, 200], [735, 200], [735, 255], [0, 800]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))

                # Right upper triangle
                points = np.array([[1150, 200], [1920, 200], [1920, 225], [1920, 800]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))

                # Lower left corner 
                points = np.array([[0, 850], [900, 1080], [890, 1080], [0, 1080]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))

                # Lower right corner 
                points = np.array([[900, 1080], [910, 1080], [1920, 1080], [1920, 900]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))

            if(raw_rows == 720):
                # Blank out the areas outside the rink.
                points = np.array([[0, 0], [1280, 0], [1280, 170], [0, 170]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))
                points = np.array([[0, 160], [430, 160], [430, 165], [0, 540]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))
                points = np.array([[800, 160], [1280, 160], [1280, 165], [1280, 450]])
                cv2.fillPoly(sub_image,pts=[points],color=(white_color))

            #im_display(sub_im_inv,"blanked sub_im_inv", 0.5)

                
            # generate and display the home player mask
            # This color set works best for red, black and white uniform combinations.
            home_lower = np.array([0, 0, 220], dtype=np.uint8)
            home_upper = np.array([255 ,255, 255], dtype=np.uint8)

            home_mask = cv2.inRange(sub_image, home_lower, home_upper)
            #im_display(home_mask,"home player mask", 0.7)

            # Set erosion kernel for dilate function. 3 by 3 gives best result.          
            kernel = np.ones((5,5), np.uint8)
            #kernel = np.ones((3,3), np.uint8)
            #kernel = np.ones((2,2), np.uint8)

            # Apply mild erosion to bit map to cleanup the potential contoured objects.
            eroded_mask = cv2.erode(home_mask, kernel)
            #eroded_mask = cv2.erode(home_mask, kernel0)

            #im_display(eroded_mask,"eroded_mask", 0.7)

            # find the player outline
            contours, hierarchy = cv2.findContours(eroded_mask,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)
            #contours, hierarchy = cv2.findContours(home_mask,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)
            #contours, hierarchy = cv2.findContours(dilated_eroded_mask,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)

            # Sort contours largest to smallest by area
            contours = sorted(contours, key=cv2.contourArea, reverse=True)

            cv2.drawContours(sub_image, contours, -1, red_color, 1)
            #im_display(sub_image,"player contours", 0.7)
            
            # After the first frame, save a copy of the previous frame's CM's.
            if(frame_num > start_frame):
                old_contour_CMs = contour_CMs.copy()
                #print("old_contour_CMs:",old_contour_CMs)

            # Total number of entries = 6 home players + 6 visitor players + 4 refs + puck = 17
            nVHRP = 17

            # Build a list of object ID text for display
            id_text = ['V1', 'V2', 'V3', 'V4' , 'V5',
                        'H1', 'H2', 'H3', 'H4', 'H5',
                        'GH', 'GV',
                        'R1', 'R2', 'R3', 'R4',
                        'Puck']

            #print("id_text:",id_text)

            # Lists for contour number, center of moments and area. Re-initialize for each
            # new frame.
            good_contours = []
            contour_maxy = []
            contour_area = []
            contour_CMs = []

            # Loop over all contours found on main image and screen for player/ref
            # size silhouettes. This is just to show the user what was found.
            num_contours = len(contours)
            #print("num contours found:",num_contours)
            for icon in range(0,num_contours):
                cont_area = cv2.contourArea(contours[icon])
                #print("icon, contour area:",cont_area)

                # Get the contour bounding rectangle (BB). 
                rec_UL_x,rec_UL_y,rec_width,rec_height = cv2.boundingRect(contours[icon])
                
                # Use the rectangle to get aspect ratio - currently unused
                #aspect_ratio = rec_height / rec_width
                #print("aspect ratio:",aspect_ratio)
                
                # First screen on contour area.
                # Accept all contours with areas with a range bigger than small objects that
                # are not players and smaller than very large objects that are probably
                # findContours artifacts.
                if((cont_area >= 150) and (cont_area <= 10000)):
                    # Draw the contour
                    cv2.drawContours(frame, contours, icon, cyan_color, 1)
                   
                    # Set the standard size of a single player contour at the far end of the ice
                    # at 500 to 600 pixels. The size of the far goalie is 553 pixels.
                    min_standard_size = 200.0
                    max_standard_size = 750.0

                    # Get contour center of moments
                    Moments = cv2.moments(contours[icon])
                    cmX = int(Moments["m10"] / Moments["m00"])
                    cmY = int(Moments["m01"] / Moments["m00"])

                    # Convert the CM to rink coordinates.
                    x_in = cmX
                    y_in = cmY
                    #print("x_in,y_in:",x_in,y_in)

                    x_out = ((M11 * x_in) + (M12 * y_in) + M13) / ((M31 * x_in) + (M32 * y_in) + M33)
                    y_out = ((M21 * x_in) + (M22 * y_in) + M23) / ((M31 * x_in) + (M32 * y_in) + M33)

                    #print("CM rink x position:",x_out)

                    # Scale the acceptable range of areas based on the x position of the contour. This
                    # will further reduce the acceptable size range and allow the identification of
                    # multiple OIO's in a contour. This is done in true rink coordinates.
                    area_scaler = 2000.0 / (2000.0 - x_out)
                    #print("area_scaler:", area_scaler)

                    # Determine the standard player size at the location of the contour
                    local_min_standard_size = area_scaler * min_standard_size
                    local_max_standard_size = area_scaler * max_standard_size

                    if(x_out > 1350): local_max_standard_size = local_max_standard_size + ((x_out - 1350) * 0.05)
                    
                    #print("local_min_standard_size, local_max_standard_size:",local_min_standard_size,local_max_standard_size)

                    # Contours having areas within a specific size range that should contain only
                    # one OIO. Adjusted dynamically for location in the field of view.
                    if((cont_area >= local_min_standard_size) and (cont_area <= local_max_standard_size)):
                        #print("single OIO contour, area:",cont_area)
                        # Draw the contour
                        #cv2.drawContours(sub_im_inv, contours, icon, cyan_color, 1)
                        cv2.drawContours(frame, contours, icon, cyan_color, 1)
                        
                        # Add entry to center of moments and area lists
                        good_contours.append(contours[icon])
                        contour_area.append(cont_area)
                        contour_CMs.append([cmX,cmY])
                                        
                        # Find the max vertical location of the contour
                        #print("contours[icon]:",contours[icon])
                        len_cont = len(contours[icon])
                        #print("number of elements in contour:",icon,len_cont)
                        y_max = 0

                        for iel in range(0,len_cont):

                            #print("icon,iel:",icon,iel)
                                    
                            y_val = contours[icon][iel][0][1]
                            x_val = contours[icon][iel][0][0]
                            if(y_val > y_max):
                                y_max = y_val
                                x_max = x_val
                                idx_y_max = iel
                    
                        #print("y_max, idx_y_max:",y_max, idx_y_max)

                        # Save image pixel location of the bottom of the contour as the OIO location.
                        contour_maxy.append([x_max,y_max])

                        cont_maxy = y_max
                        #print("cont_maxy:",cont_maxy)

                        # Display the max vertical location of the contour
                        #cv2.circle(frame, center=(x_max,y_max),8, color=red_color, thickness=-2)

                        # Use center of moments for OIO pixel location.
                        # Plot the center of moments for the contour.
                        #cv2.circle(sub_im_inv, center=(cmX,cmY), radius=5, color=blue_color, thickness=-2)
                        cv2.circle(frame, center=(cmX,cmY), radius=5, color=blue_color, thickness=-2)


                        # WARNING: this fixed to large frame size.
                        y_shift = int(50.0 * (1080.0 - cmY) / 1080.0)
                        #print("y_shift:", y_shift)

                        # label the contour with its area
                        cv2.putText(frame, str(int(cont_area)), (cmX,cmY+y_shift), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

                        # Get the contour bounding rectangle (BB). 
                        rec_UL_x,rec_UL_y,rec_width,rec_height = cv2.boundingRect(contours[icon])

                        # These are the BB corners of the contour.
                        min_x = rec_UL_x
                        max_x = min_x + rec_width
                        min_y = rec_UL_y
                        max_y = min_y + rec_height
                        #print("Original contour BB min_x, max_x, min_y, max_y:",min_x, max_x, min_y, max_y)

                        # Initialize the extended BB parameters. At the start there are none, so 
                        # set to the inital BB corners. 
                        min_ext_x = min_x
                        max_ext_x = max_x
                        min_ext_y = min_y
                        max_ext_y = max_y

                    # Contours that are possibly too big for a single player
                    elif((cont_area >= local_max_standard_size) and (cont_area <= 10000)):
                        #print("multiple OIO contour, area:",cont_area)

                        cv2.drawContours(frame, contours, icon, cyan_color, 1)
                        
                        # Add entry to center of moments and area lists
                        #good_contours.append(contours[icon])
                        #contour_area.append(cont_area)
                        #contour_CMs.append([cmX,cmY])
                
                        # Find the max vertical location of the contour
                        #print("contours[icon]:",contours[icon])
                        len_cont = len(contours[icon])
                        #print("number of elements in contour:",icon,len_cont)
                        y_max = 0
                        for iel in range(0,len_cont):
                            #print("icon,iel:",icon,iel)
                            y_val = contours[icon][iel][0][1]
                            x_val = contours[icon][iel][0][0]
                            if(y_val > y_max):
                                y_max = y_val
                                x_max = x_val
                                idx_y_max = iel

                        #print("y_max, idx_y_max:",y_max, idx_y_max)
       
                        #contour_maxy.append([x_max,y_max])

                        cont_maxy = y_max
                        #print("cont_maxy:",cont_maxy)

                        # Display the max vertical location of the contour
                        #cv2.circle(frame, center=(x_max,y_max),8, color=red_color, thickness=-2)

                        # Plot the center of moments for the contour
                        #cv2.circle(sub_im_inv, center=(cmX,cmY), radius=5, color=blue_color, thickness=-2)
                        #cv2.circle(frame, center=(cmX,cmY), radius=5, color=magenta_color, thickness=-2)

                        # WARNING: this fixed to large frame size.
                        y_shift = int(50.0 * (1080.0 - cmY) / 1080.0)
                        #print("y_shift:", y_shift)

                        # label the contour with its area
                        cv2.putText(frame, str(int(cont_area)), (cmX,cmY+y_shift), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

                        # Get the contour bounding rectangle (BB). 
                        rec_UL_x,rec_UL_y,rec_width,rec_height = cv2.boundingRect(contours[icon])

                        # These are the BB corners of the contour.
                        min_x = rec_UL_x
                        max_x = min_x + rec_width
                        min_y = rec_UL_y
                        max_y = min_y + rec_height
                        #print("Original contour BB min_x, max_x, min_y, max_y:",min_x, max_x, min_y, max_y)
                            
                        # Initialize the extended BB parameters. At the start there are none, so 
                        # set to the inital BB corners. 
                        min_ext_x = min_x
                        max_ext_x = max_x
                        min_ext_y = min_y
                        max_ext_y = max_y
                           
                        # Get contour and reduce to local BB coordinates.
                        #print("BB, contour:",BB,BB_contours[BB])
                        BB_cnt = contours[icon]
                        #print("BB, contour:",BB,BB_cnt)
                        BB_cnt = BB_cnt - (min_x,min_y)                
                        #print("BB, contour:",BB,BB_cnt)

                        # Extract the BB from the subtracted inverted image
                        BB_sub_inv =  np.copy(sub_image[(min_y):(max_y),(min_x):(max_x)])
                        BB_rows,BB_cols,BB_chans = BB_sub_inv.shape

                        # Set the max area for an OIO contour inside the merged contour.
                        #print("BB_rows,BB_cols:",BB_rows,BB_cols) 
                        #BB_max_area = (BB_cols - 2) * (BB_rows - 2)
                        BB_max_area = int(0.7 * cont_area)
                        BB_min_area = int(0.025 * cont_area)
                        #print("BB_max_area:",BB_max_area)
                            
                        # Calculate a scale factor so BB displays are a good size on the screen.                     
                        disp_scale = 500.0 / BB_rows
                        #print("display scale:",disp_scale)

                        #im_display(BB_sub_inv,"BB_sub_inv", disp_scale)

                        erode_count = 0
                        total_useful_BB_contours = 0
                        while((total_useful_BB_contours < 2) and erode_count < 10):

                            erode_count = erode_count + 1
                            #print("-----------------------------------------------")
                            #print("erode_count:",erode_count)

                            BB_temp = BB_sub_inv.copy()
                            # Extract the BB from the original eroded mask
                            BB_em_orig =  np.copy(eroded_mask[(min_y):(max_y),(min_x):(max_x)])
                            #im_display(BB_em_orig,"BB original eroded mask", disp_scale)

                            # Invert the mask for erosion
                            BB_em_orig_inv = cv2.bitwise_not(BB_em_orig)

                            #kernel = np.ones((6,6), np.uint8)
                            #BB_em_orig_em_inv = cv2.erode(BB_em_orig_inv, kernel)
                            #im_display(BB_em_orig_em_inv,"6x6 BB_em_orig_em_inv", disp_scale)

                            kernel = np.ones((erode_count,erode_count), np.uint8)
                            BB_em_orig_em_inv = cv2.erode(BB_em_orig_inv, kernel)
                            #im_display(BB_em_orig_em_inv,"8x8 BB_em_orig_em_inv", disp_scale)

                            #kernel = np.ones((10,10), np.uint8)
                            #BB_em_orig_em_inv = cv2.erode(BB_em_orig_inv, kernel)
                            #im_display(BB_em_orig_em_inv,"10x10 BB_em_orig_em_inv", disp_scale)

                            # Invert back for findContours
                            BB_em_orig_em = cv2.bitwise_not(BB_em_orig_em_inv) 
                            #im_display(BB_em_orig_em,"8x8 BB_em_orig_em", disp_scale)

                            # Apply findContours to the BB of the multi-OIO contour.
                            #BB_contours, BB_hierarchy = cv2.findContours(BB_mask,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)
                            #BB_contours, BB_hierarchy = cv2.findContours(BB_eroded_mask_inv,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)
                            BB_contours, BB_hierarchy = cv2.findContours(BB_em_orig_em,cv2.RETR_TREE,cv2.CHAIN_APPROX_SIMPLE)
                            #cv2.drawContours(BB_temp, BB_contours, -1, blue_color, 1)
                            #im_display(BB_temp,"BB_sub_inv with contours", disp_scale)

                            # Sort contours largest to smallest by area
                            BB_contours = sorted(BB_contours, key=cv2.contourArea, reverse=True)

                            #print("useful contours areal range:",BB_min_area,BB_max_area)
                            num_BB_contours = len(BB_contours)
                            #print("num BB_contours found:",num_BB_contours)

                            # Count the number of contours in the useful range of areal size.
                            num_useful_BB_contours = 0
                            for BB_icon in range(0,num_BB_contours):
                                BB_cont_area = cv2.contourArea(BB_contours[BB_icon])
                                #print("icon, contour area:",BB_cont_area)

                                # Avoid dealing with real small contours and very big ones
                                #if((BB_cont_area >= 150) and (BB_cont_area <= BB_max_area)):
                                if((BB_cont_area >= BB_min_area) and (BB_cont_area <= BB_max_area)):
                                    num_useful_BB_contours = num_useful_BB_contours + 1
                                    #print("useful contours found, area:",BB_cont_area)

                            total_useful_BB_contours = num_useful_BB_contours
                            #print("num useful BB_contours found:",num_useful_BB_contours)


                        num_new_OIOs = 0

                        for BB_icon in range(0,num_BB_contours):
                            BB_cont_area = cv2.contourArea(BB_contours[BB_icon])
                            #print("icon, contour area:",BB_cont_area)
                               
                            # Avoid dealing with real small contours and very big ones
                            #if((BB_cont_area >= 150) and (BB_cont_area <= BB_max_area)):
                            if((BB_cont_area >= BB_min_area) and (BB_cont_area <= BB_max_area)):
                                #print("---------------------------------------------------")
                                #print("Merged contour rec_UL_x,rec_UL_y:",rec_UL_x,rec_UL_y)
                                #print("Merged contour rec_width,rec_height:",rec_width,rec_height)

                                #print("OIO BB_icon, BB_contour area:",BB_icon,BB_cont_area)
                                # Get the BB contour bounding rectangle (BB). 
                                BB_rec_UL_x,BB_rec_UL_y,BB_rec_width,BB_rec_height = cv2.boundingRect(BB_contours[BB_icon])
                                #print("OIO contour BB_rec_width,BB_rec_height:",BB_rec_width,BB_rec_height)
                                #print("OIO contour BB_rec_UL_x,BB_rec_UL_y:",BB_rec_UL_x,BB_rec_UL_y)
                                #cv2.drawContours(BB_sub_inv, BB_contours, BB_icon, red_color, 1)
                          
                                # Get contour center of moments
                                BB_Moments = cv2.moments(BB_contours[BB_icon])
                                BB_cmX = int(BB_Moments["m10"] / BB_Moments["m00"])
                                BB_cmY = int(BB_Moments["m01"] / BB_Moments["m00"])
                                #print("OIO BB_CM image pixels x,y:",BB_cmX,BB_cmY)
                                #cv2.circle(BB_sub_inv, center=(BB_cmX,BB_cmY), radius=5, color=blue_color, thickness=-2)

                                # Convert to full frame coordinates
                                x = BB_cmX + rec_UL_x
                                y = BB_cmY + rec_UL_y
                                   
                                contour_CMs.append([x,y])

                                # Find the max vertical location of the contour
                                #print("contours[icon]:",contours[icon])
                                len_cont = len(BB_contours[BB_icon])
                                #print("number of elements in contour:",icon,len_cont)
                                y_max = 0
                                for iel in range(0,len_cont):
                                    #print("icon,iel:",icon,iel)
                                    y_val = BB_contours[BB_icon][iel][0][1]
                                    x_val = BB_contours[BB_icon][iel][0][0]
                                    if(y_val > y_max):
                                        y_max = y_val
                                        x_max = x_val
                                        idx_y_max = iel

                                # Convert to full frame coordinates
                                x_max = x_max + rec_UL_x
                                y_max = y_max + rec_UL_y

                                #print("y_max, idx_y_max:",y_max, idx_y_max)

                                BB_contours[BB_icon] = BB_contours[BB_icon] + [rec_UL_x,rec_UL_y]
                                good_contours.append(BB_contours[BB_icon])
                                contour_area.append(BB_cont_area)
                                contour_maxy.append([x_max,y_max])

                                #print("OIO BB_CM frame pixels x,y:",x,y)
                                cv2.circle(frame, center=(x,y), radius=5, color=blue_color, thickness=-2)
                                #print("---------------------------------------------------")

                                num_new_OIOs = num_new_OIOs + 1


                        # Insert new code.
                        # If only one OIO subcontour is found in the BB contour, try again with a 3x3 kernel. 


                        # If no OIO subcontours were found in the BB contour, default to the merged 
                        # contour CM.
                        if(num_new_OIOs == 0):
                            cv2.circle(frame, center=(cmX,cmY), radius=5, color=blue_color, thickness=-2)
                            cv2.circle(BB_sub_inv, center=(cmX,cmY), radius=5, color=blue_color, thickness=-2)

                        #im_display(BB_sub_inv,"BB_sub_inv with accepted contours", disp_scale)
          
            # ----------------------------------------------------------------
            # end for BB in range(0, BB_num): End of target and OIO discovery.
            # ----------------------------------------------------------------


                 

            num_contours = len(contour_area)
            #print("VHRP num_contours:",num_contours)
                      
            # Initialize a list of the number of each type of on-ice object
            VHRP_lens = []
            # visitors
            VHRP_lens.append(5)
            # home
            VHRP_lens.append(5)
            # goalie
            VHRP_lens.append(2)
            # ref
            VHRP_lens.append(4)
            # puck
            VHRP_lens.append(1)
            #print("VHRP_lens:",VHRP_lens)
        
            # -------------------------------------
            # Mark previous tracks on the new frame
            # -------------------------------------
            #print("VHRP_lens:",VHRP_lens)

            n_frames = len(session_obj_ICP_image_coords)

            last_puck_x = -1
            last_puck_y = -1

            # Loop over all previous frames tracking results
            for iframe in range(0, n_frames):
                              
                # Loop over types of on-ice objects (visitor,home,goalie,ref)
                for iVHRP in range(0,4):
                    nVHRP = VHRP_lens[iVHRP]
                    
                    
    
                    # Loop over all possible marked on-ice objects
                    for ient in range(0,nVHRP):
                        
                        
                        x = session_obj_CM_image_coords[iframe][iVHRP][ient][0]
                        y = session_obj_CM_image_coords[iframe][iVHRP][ient][1]
                        
                        if((x > 0) and (y > 0)):
                            #print("Marking trail for frame, iVHRP, ient, x,y:",frame_num,iVHRP,ient,x,y)
                            # Visitor
                            if(iVHRP == 0):
                                cv2.circle(frame, center=(x,y), radius=2, color=magenta_color, thickness=-2)
                            # Home
                            elif(iVHRP == 1):
                                cv2.circle(frame, center=(x,y), radius=2, color=red_color, thickness=-2)

                            # Goalie
                            elif(iVHRP == 2):
                                cv2.circle(frame, center=(x,y), radius=2, color=cyan_color, thickness=-2)

                            # Ref
                            elif(iVHRP == 3):
                                cv2.circle(frame, center=(x,y), radius=2, color=yellow_color, thickness=-2)


                # Second loop over all possible marked on-ice objects to see
                # if there are any overlaps due to occlusions. The CM's
                # will be the same.
                # Loop over types of on-ice objects (visitor,home,ref)
                for iVHRP in range(0,4):
                    nVHRP = VHRP_lens[iVHRP]

                    #print("session_obj_ICP_image_coords[iframe][iVHRP]:",session_obj_ICP_image_coords[iframe][iVHRP])

                    # Loop over all possible marked on-ice objects for each type
                    for ient in range(0,nVHRP):

                        x = session_obj_ICP_image_coords[iframe][iVHRP][ient][0]
                        y = session_obj_ICP_image_coords[iframe][iVHRP][ient][1]
                        #print("1 - overlap check: x,y:",x,y)

                        if((x > 0) and (y > 0)):

                            # Loop over types of on-ice objects (visitor,home,goalie, ref)
                            for jVHRP in range(0,4):
                                mVHRP = VHRP_lens[jVHRP]

                                # Loop over all possible marked on-ice objects for each type
                                for jent in range(0,mVHRP-1):
                                    # Skip if the same on-ice object
                                    if((iVHRP == jVHRP) and (ient == jent)): break

                                    xx = session_obj_ICP_image_coords[iframe][jVHRP][jent][0]
                                    yy = session_obj_ICP_image_coords[iframe][jVHRP][jent][1]
                                    #print("2 - overlap check: xx,yy:",xx,yy)

                                    '''    
                                    # Optional text marking of CM
                                    if((xx > 0) and (yy > 0)):
                                        if((xx == x) and (yy == y)):
                                            #print("3 - overlap check: x,y,xx,yy:",x,y,xx,yy)

                                            cv2.circle(frame, center=(x,y), radius=2, color=green_color, thickness=-2)

                                            # for determining on-ice object id number for text marking of CM
                                            if(iVHRP == 0):
                                                id_num = i_ent
                                            elif(iVHRP == 1):
                                                id_num = 5 + i_ent
                                            elif(iVHRP == 2):
                                                id_num = 10 + i_ent
                                            elif(iVHRP == 3):
                                                id_num = 12 + i_ent
                                            elif(iVHRP == 4):
                                                id_num = 16
                                            # label object once we have sorted out the overlaps due to occlusions
                                            cv2.putText(frame, id_text[id_num], (x,y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, red_color, 2)
                                    '''
    
                # Mark the puck and its back trail in black
                x = session_obj_ICP_image_coords[iframe][4][0][0]
                y = session_obj_ICP_image_coords[iframe][4][0][1]
                if((x > 0) and (y > 0)):
                    cv2.circle(frame, center=(x,y), radius=2, color=black_color, thickness=-2)

                    if((last_puck_x > 0) and (last_puck_y > 0)):
                        cv2.line(frame, (last_puck_x, last_puck_y), (x, y), black_color, thickness=2)

                    last_puck_x = x
                    last_puck_y = y

            # --------------------------------------------
            # End marking previous tracks on the new frame
            # --------------------------------------------

            #print("frame:",frame_num,"marked frame_obj_CM_image_coords:",frame_obj_CM_image_coords)

            # Initialize a list for recording contours that are matched with
            # on-ice objects.
            contour_matched = []
            for i in range(0,num_contours):
                contour_matched.append(0)                

            # plot all current frame contours
            #frame_copy = frame.copy()
            #cv2.drawContours(frame, contours, -1, white_color, 1)
            #im_display(frame_copy,"player contours", xy_scale)

            
            # -----------------------------------------------------------------------------------
            # Beginning with the second frame, loop over all identified players and refs matching
            # their previous frame location to the nearest OIO on the new frame.
            # -----------------------------------------------------------------------------------
            if(frame_num >= start_frame):
               
                #print("------------------------")
                #print("1:before update session_obj_ICP_image_coords:",session_obj_ICP_image_coords)

                # These are the CMs from user marking.
                #print("start matching; frame:",frame_num,"frame_obj_CM_image_coords:",frame_obj_CM_image_coords)
                #print("start frame, on_ice_cms:",frame_num,on_ice_cms)

                # Four kinds of things being marked. Six visitor, six home and four referees
                # handled here. Puck handled separtely below.

                # Loop over object categories except the puck looking for marked objects.
                for iVHRP in range(0,4):

                    # Determine the number of existing objects in the category.
                    num_entries = len(frame_obj_CM_image_coords[iVHRP])
                    #if(iVHRP == 1): 
                    #print("iVHRP:",iVHRP,"num_entries:",num_entries)

                    num_good_contours = len(good_contours)
                    #print("num_good_contours:",num_good_contours)

                    # Loop over all existing marked on-ice objects in the category. Will have
                    # positive values for x and y.
                    #print("---- start i_ent loop")
                    for i_ent in range(0,num_entries):
                        #print("i_ent:",i_ent)
                        i_ent_X = frame_obj_CM_image_coords[iVHRP][i_ent][0]
                        i_ent_Y = frame_obj_CM_image_coords[iVHRP][i_ent][1]
                        #if(iVHRP == 1):
                        #   print("1: iVHRP:",iVHRP,"i_ent_X:",i_ent_X,"i_ent_Y:",i_ent_Y)

                        # Calculate the on-ice object number
                        # Visiting
                        if(iVHRP == 0):
                            ice_obj = i_ent
                            
                        # Home
                        elif(iVHRP == 1):
                            ice_obj = i_ent + 5
                        # Goalies
                        elif(iVHRP == 2):
                            ice_obj = i_ent + 10
                        # Refs
                        elif(iVHRP == 3):
                            ice_obj = i_ent + 12
                        
                        
                        

                        # Make sure we are referencing a valid on-ice object (one that has been marked).
                        # Must be valid image pixel coordinates. 
                        if((i_ent_X >= 0) and (i_ent_Y >= 0)):
                            
                            #if(iVHRP == 1): 
                            #print("2: iVHRP:",iVHRP,"i_ent_X:",i_ent_X,"i_ent_Y:",i_ent_Y)
                            #print("iVHRP, old_cmX, old_cmY:",iVHRP,old_cmX,old_cmY)

                            min_CS_dist = 20

                            min_icon = -1

                            # Initialize icon number of minimum distance between current and past silhouette
                            # contour CM's to -1. If it is still -1 after the search, no match was made.

                            # Loop over all discovered OIO CM's on the new frame to find a match with one
                            # on the past frame.
                            num_CMs = len(contour_CMs)
                            #print("number of new OIO CMs:",num_CMs)
                            #print("contour_CMs:",contour_CMs)

                            for icon in range(0,num_CMs):
                                # Get new frame OIO center of moments from list created above.
                                cmX = contour_CMs[icon][0]
                                cmY = contour_CMs[icon][1] 

                                #print("new frame OIO to match x,y:",cmX,cmY)
                                # Silhouette contact point with the ice in pixels
                                maxy_x = contour_maxy[icon][0]
                                maxy_y = contour_maxy[icon][1]

                                # This gets the contour number of the object being matched from the old frame,
                                # icon is the contour number of the object found on the new frame the is currently
                                # being used to find a match.
                                #jcon = contour_icon[icon]
                                #print("icon, cmX, cmY:",icon,cmX,cmY)
                                                          
                                if((cmX > 0) and (cmY > 0)):
                                    CS_dist = math.sqrt(((cmX - i_ent_X)**2) + ((cmY - i_ent_Y)**2))
                                    #print("i_ent coord to contour center distance:",CS_dist)

                                    if(CS_dist < min_CS_dist):
                                        min_CS_dist = CS_dist
                                        min_cmX = cmX
                                        min_cmY = cmY
                                        min_icon = icon
                                        #print("min_CS_dist i_ent:",i_ent)
                                        min_maxy_x = maxy_x
                                        min_maxy_y = maxy_y

                                        # for determining on-ice object id number for text marking of CM
                                        if(iVHRP == 0):
                                            id_num = i_ent
                                            obj_color = magenta_color
                                        elif(iVHRP == 1):
                                            id_num = 5 + i_ent
                                            obj_color = red_color
                                        elif(iVHRP == 2):
                                            id_num = 10 + i_ent
                                            obj_color = cyan_color
                                        elif(iVHRP == 3):
                                            id_num = 12 + i_ent
                                            obj_color = green_color
                                        elif(iVHRP == 4):
                                            id_num = 16
                                            obj_color = black_color

                                        #cv2.putText(frame, id_text[id_num], (min_cmX+10,min_cmY+20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)
                                        #print("matched iVHRP:",iVHRP,"id_num:",id_num)
                                        #min_jcon = jcon

                            # Check to see if a match was made.
                            #print("min_icon:",min_icon)

                            if(min_icon > -1):
                                #print("OIO match was made on frame:",frame_num,"iVHRP:",iVHRP,"i_ent:",i_ent,"x,y:",min_cmX,min_cmY)
 
                                # Record the contour match
                                contour_matched[min_icon] = contour_matched[min_icon] + 1
  
                                
                                # Get the contour bounding rectangle (BB). 
                                rec_UL_x,rec_UL_y,rec_width,rec_height = cv2.boundingRect(good_contours[min_icon])
                          
                                # These are the BB corners of the contour.
                                min_x = rec_UL_x
                                max_x = min_x + rec_width
                                min_y = rec_UL_y
                                max_y = min_y + rec_height
                                #print("Original contour BB min_x, max_x, min_y, max_y:",min_x, max_x, min_y, max_y)
                        
                                # Initialize the extended BB parameters. At the start there are none, so 
                                # set to the inital BB corners. 
                                min_ext_x = min_x
                                max_ext_x = max_x
                                min_ext_y = min_y
                                max_ext_y = max_y

                                # Extract the input contour BB from the warped image.
                                # This should be a skater.
                                player_im = np.copy(silh_frame[(min_y):(max_y),(min_x):(max_x)])

                                #im_display(player_im,"player contour", disp_scale)

                                # Write to temp file for that player
                                player_im_sc = cv2.resize(player_im, (0,0), fx=3, fy=3)

                                # save the contour BB of object in a file
                                #print("Write silhouette for object:",i_ent,"jcon",min_icon,"frame:",frame_num)
                                silh_filename = f'./{project_name}/Sequences/Files_Seq_{str(seq_num)}/Silh_Temp/F{str(frame_num)}_O{str(ice_obj)}.jpeg'
                                #print("silh_filename:",silh_filename)
                                
                                # maintain a list of OIO silhouette file names - turn on later if MP4 videos are needed.
                                #silh_files.append(silh_filename)
                                
                                # Write the OIO silhouette image to a temp file.
                                cv2.imwrite(silh_filename, player_im_sc)


                                

                                #print("------------------------")
                                #print("1a:before update session_obj_ICP_image_coords:",session_obj_ICP_image_coords)

                                #on_ice_cms[iVHRP][i_ent] = (min_cmX,min_cmY)
                                #print("final min_cmX,min_cmY:",on_ice_cms[iVHRP][i_ent])

                                #print("final i_ent:", i_ent)

                                # Update the entry with the object CM pixel coordinates.
                                frame_obj_CM_image_coords[iVHRP][i_ent] = (min_cmX,min_cmY)

                                player_positions[iVHRP][i_ent] = (0, (min_cmX, min_cmY))
                                #print("updated CM image x,y; frame:",frame_num,"final min_cmX,min_cmY:",frame_obj_CM_image_coords[iVHRP][i_ent])

                                #print("------------------------")
                                #print("1b:before update session_obj_ICP_image_coords:",session_obj_ICP_image_coords)
                               
                                # for determining on-ice object id number for text marking of CM
                                if(iVHRP == 0):
                                    id_num = i_ent
                                    obj_color = magenta_color
                                elif(iVHRP == 1):
                                    id_num = 5 + i_ent
                                    obj_color = red_color
                                elif(iVHRP == 2):
                                    id_num = 10 + i_ent
                                    obj_color = cyan_color
                                elif(iVHRP == 3):
                                    id_num = 12 + i_ent
                                    obj_color = green_color
                                elif(iVHRP == 3):
                                    id_num = 16
                                    obj_color = black_color
                                
                            

                                #print("iVHRP:",iVHRP,"i_ent:",i_ent,"id_num:",id_num)

                                
                                # Label the on-ice object on the user display.
                                # Position the label according to how many matches of this contour this
                                # one is.
                                #print("min_icon,contour_matched[min_icon]:",min_icon,contour_matched[min_icon])

                                #print("interesting: ", contour_matched, " min icon:", min_icon )
                                #print("look here: ", contour_matched[min_icon])

                                #print(f"{contour_matched[min_icon]}:min_icon,contour_matched[min_icon]")
                                if(contour_matched[min_icon] == 0):
                                    cv2.putText(frame, "text", (min_cmX,min_cmY+20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)
                                
                                if(contour_matched[min_icon] == 1):
                                    cv2.putText(frame, id_text[id_num], (min_cmX-10,min_cmY+20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

                                if(contour_matched[min_icon] == 2):
                                    cv2.putText(frame, id_text[id_num], (min_cmX+10,min_cmY+20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)
                                                        
                                if(contour_matched[min_icon] == 3):
                                    cv2.putText(frame, id_text[id_num], (min_cmX-25,min_cmY), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

                                if(contour_matched[min_icon] == 4):
                                    cv2.putText(frame, id_text[id_num], (min_cmX+25,min_cmY), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)
                                
                                if(contour_matched[min_icon] == 5):
                                    cv2.putText(frame, id_text[id_num], (min_cmX-10,min_cmY-20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

                                if(contour_matched[min_icon] >= 6):
                                    cv2.putText(frame, id_text[id_num], (min_cmX-10,min_cmY+20), cv2.FONT_HERSHEY_SIMPLEX, 0.5, black_color, 2)

                                #print("just put text:",id_num,id_text[id_num],i_ent,min_cmX,min_cmY,min_CS_dist)
               
                                

                                # Save Object Silhouette ice contact point (ICP) image pixel coordinates
                                frame_obj_ICP_image_coords[iVHRP][i_ent] = (int(min_maxy_x),int(min_maxy_y))
                                #print("ICP image; frame:",frame_num," final min_maxy_x,min_maxy_y:",frame_obj_ICP_image_coords[iVHRP][i_ent])

                                # Convert the ice contact point to rink coordinates and plot on the rink view.
                                x_in = min_maxy_x
                                y_in = min_maxy_y
                                #print("x_in,y_in:",x_in,y_in)

                                # Plot the on-ice contact point on the image frame.
                                cv2.circle(frame, center=(int(x_in),int(y_in)), radius=5, color=obj_color, thickness=-2)

                                x_out = ((M11 * x_in) + (M12 * y_in) + M13) / ((M31 * x_in) + (M32 * y_in) + M33)
                                y_out = ((M21 * x_in) + (M22 * y_in) + M23) / ((M31 * x_in) + (M32 * y_in) + M33)
                                #print("x_out,y_out:",x_out,y_out)

                                # Save Object Silhouette ice contact point (ICP) in true rink coordinates
                                frame_obj_ICP_rink_coords[iVHRP][i_ent] = (int(x_out),int(y_out))
                                #print("ICP rink; frame", frame_num,"final x_out,y_out:",frame_obj_ICP_rink_coords[iVHRP][i_ent])

                                # Plot the on-ice contact point on the rink grid.
                                cv2.circle(rink_im, center=(int(x_out),int(y_out)), radius=5, color=obj_color, thickness=-2)

                                # If this is the first frame, label the object.
                                if(frame_num == 2):
                                    cv2.putText(rink_im, id_text[id_num], (int(x_out),int(y_out)), cv2.FONT_HERSHEY_SIMPLEX, 1.0, black_color, 3)

                            # No match was made.
                            else:
                                # Redisplay the text label of a missed object so the user can see what it
                                # is and where it stopped.
                                # for determining on-ice object id number for text marking of CM
                                if(iVHRP == 0):
                                    id_num = i_ent
                                    obj_color = magenta_color
                                elif(iVHRP == 1):
                                    id_num = 5 + i_ent
                                    obj_color = red_color
                                elif(iVHRP == 2):
                                    id_num = 10 + i_ent
                                    obj_color = cyan_color
                                elif(iVHRP == 3):
                                    id_num = 12 + i_ent
                                    obj_color = green_color
                                # Puck 
                                elif(iVHRP == 4):
                                    id_num = 16
                                    obj_color = black_color

                                print("Object missed; iVHRP:",iVHRP,"i_ent:",i_ent,"id_num:",id_num,id_text[id_num],)
                                cv2.putText(frame, id_text[id_num], (i_ent_X,i_ent_Y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, red_color, 2)

                                # If we didn't find a contour, set the x and y to the values from obj cm
                                # Which will be the mark coordinates if it was marked and -1, -1 if it wasn't
                                x_in, y_in = frame_obj_click_coords[iVHRP][i_ent]

                                if (x_in, y_in) != (-1, -1):
                                    x_out = ((M11 * x_in) + (M12 * y_in) + M13) / ((M31 * x_in) + (M32 * y_in) + M33)
                                    y_out = ((M21 * x_in) + (M22 * y_in) + M23) / ((M31 * x_in) + (M32 * y_in) + M33)
                                else:
                                    x_out, y_out = x_in, y_in

                                pp = player_positions[iVHRP][i_ent]
                                player_positions[iVHRP][i_ent] = (pp[0] + 1, pp[1])

                                frame_obj_ICP_rink_coords[iVHRP][i_ent] = (int(x_out), int(y_out))

                                #print("No contour match was made on the new frame; iVHRP:",iVHRP,"i_ent:",i_ent)
                                # Set the object button yellow. This will require some code to figure out the button
                                # location on the button selection panel.
                                
                                # Mark the button yellow.
                                #center_x = 
                                #center_y = 
                                #cv2.circle(man_picks_im, center=(center_x,center_y), radius=8, color=yellow_color, thickness=-2)


                        #else:
                        #    print("Invalid object image pixel coordimates; iVHRP:",iVHRP,"i_ent:",i_ent)


                #print("------------------------")
                #print("2:before update session_obj_ICP_image_coords:",session_obj_ICP_image_coords)
           

                # ----------------------
                # For tracking the puck.
                # ----------------------
                old_puck_cmX = frame_obj_CM_image_coords[4][0][0]
                old_puck_cmY = frame_obj_CM_image_coords[4][0][1]
                #print("current puck coords:",frame_obj_CM_image_coords[3][0])
                #print("current frame_obj_CM_image_coords:",frame_obj_CM_image_coords)
 
                # Use the last cursor pick as default. Will be overwritten if the puck
                # can be auto-tracked. 
                # Convert the puck location to rink coordinates and plot on the rink view.
                x_in = frame_obj_CM_image_coords[4][0][0]
                y_in = frame_obj_CM_image_coords[4][0][1]
                #print("x_in,y_in:",x_in,y_in)

                # Save puck ice contact point (ICP) image pixel coordinates
                frame_obj_ICP_image_coords[4][0] = (int(x_in),int(y_in))

                if (x_in, y_in) != (-1, -1):
                    x_out = ((M11 * x_in) + (M12 * y_in) + M13) / ((M31 * x_in) + (M32 * y_in) + M33)
                    y_out = ((M21 * x_in) + (M22 * y_in) + M23) / ((M31 * x_in) + (M32 * y_in) + M33)
                else:
                    x_out, y_out = x_in, y_in
                #print("x_out,y_out:",x_out,y_out)

                # Plot the on-ice contact point on the rink grid.
                cv2.circle(rink_im, center=(int(x_out),int(y_out)), radius=5, color=black_color, thickness=-2)

                # Save puck contact point with the ice in true rink x,y coordinates
                frame_obj_ICP_rink_coords[4][0] = (int(x_out),int(y_out))
                #print("puck frame_obj_ICP_rink_coords:",frame_obj_ICP_rink_coords[3][0])

                min_CS_dist = 5
                puck_contour_found = False

                num_contours = len(contours)
                for icon in range(0,num_contours):
                    cont_area = cv2.contourArea(contours[icon])
                    #print("icon, puck contour area:",cont_area)

                    # Screen contours by area to find puck sized silhouettes.
                    if((cont_area >= 3) and (cont_area <= 40)):
                        #print("icon, contour area:",cont_area)

                        # Get contour center of moments
                        Moments = cv2.moments(contours[icon])
                        puck_cmX = int(Moments["m10"] / Moments["m00"])
                        puck_cmY = int(Moments["m01"] / Moments["m00"])

                        if((cmX > 0) and (cmY > 0)):
                            
                            CS_dist = math.sqrt(((puck_cmX - old_puck_cmX)**2) + ((puck_cmY - old_puck_cmY)**2))
                            #print("puck contour center distance:",CS_dist)

                            # Check to make sure this is not a spurious contour, new puck location
                            # must be close to previous location.
                            if(CS_dist < min_CS_dist):
                                puck_contour_found = True
                                min_CS_dist = CS_dist
                                min_CS_area = cont_area
                                min_cmX = puck_cmX
                                min_cmY = puck_cmY
 
                # Plot the center of moments for the puck contour if found
                if(puck_contour_found == True):
          
                    cv2.circle(frame, center=(min_cmX,min_cmY), radius=5, color=black_color, thickness=-2)
                    cv2.putText(frame, "puck", (min_cmX+10,min_cmY+10), cv2.FONT_HERSHEY_SIMPLEX, 0.4, black_color, 2)

                    # Put in the puck category slot.
                    frame_obj_CM_image_coords[4][0] = (min_cmX,min_cmY)
                    #print("puck final cont_area, min_CS_dist, min_cmX, min_cmY:",min_CS_area,min_CS_dist,min_cmX,min_cmY)

                    old_puck_cmX = min_cmX
                    old_puck_cmY = min_cmY

                    # Convert the puck location to rink coordinates and plot on the rink view.
                    x_in = min_cmX
                    y_in = min_cmY
                    #print("x_in,y_in:",x_in,y_in)

                    # Save puck ice contact point (ICP) image pixel coordinates
                    frame_obj_ICP_image_coords[4][0] = (int(x_in),int(y_in))

                    x_out = ((M11 * x_in) + (M12 * y_in) + M13) / ((M31 * x_in) + (M32 * y_in) + M33)
                    y_out = ((M21 * x_in) + (M22 * y_in) + M23) / ((M31 * x_in) + (M32 * y_in) + M33)
                    #print("x_out,y_out:",x_out,y_out)

                    # Plot the on-ice contact point on the rink grid.
                    cv2.circle(rink_im, center=(int(x_out),int(y_out)), radius=5, color=black_color, thickness=-2)

                    # Save puck contact point with the ice in true rink coordinates
                    frame_obj_ICP_rink_coords[4][0] = (int(x_out),int(y_out))
                    #print("puck frame_obj_ICP_rink_coords:",frame_obj_ICP_rink_coords[3][0])


                # Update the final list of this frame's on-ice CM's for players, refs and the puck.
                # Get an independent copy of the current frame final CM coordinates after the 
                # user has quit working on the frame and append it to the permanent record of
                # all frames.

                # If it's the start frame, we haven't recorded anything yet and it would just put a line of -1s
                if frame_num != start_frame:
                    frame_obj_CM_image_coords_copy = copy.deepcopy(frame_obj_CM_image_coords)
                    session_obj_CM_image_coords.append(frame_obj_CM_image_coords_copy)

                    # Save current frame Silhouette ice contact point image pixels with previous frame results
                    frame_obj_ICP_image_coords_copy = copy.deepcopy(frame_obj_ICP_image_coords)
                    session_obj_ICP_image_coords.append(frame_obj_ICP_image_coords_copy)

                    # Save current frame Silhouette ice contact point rink x,y with previous frame results
                    frame_obj_ICP_rink_coords_copy = copy.deepcopy(frame_obj_ICP_rink_coords)
                    session_obj_ICP_rink_coords.append(frame_obj_ICP_rink_coords_copy)

                #print("-----------------------------------")
                #print("frame",frame_num,"after update session_obj_ICP_image_coords:",session_obj_ICP_image_coords)
                #print("-----------------------------------")

                #print("------------------------")
                #print("frame_num:",frame_num)
                #print("object rink coords:",frame_obj_ICP_rink_coords)
                #print("------------------------")
                #print("after update session_obj_ICP_image_coords:",session_obj_ICP_image_coords)
 
                #print("frame frame_obj_CM_image_coords:",frame_obj_CM_image_coords)
  
                # ------------------------------------------------------------
                # End of previous frame contour matching.
                # ------------------------------------------------------------
                
                        # ----------------------------------------------------
            # Manually mark the puck and all players to be tracked
            # ----------------------------------------------------
            # Reset clicks to be all -1
            initialize_list(frame_obj_click_coords)
            #print("setup named windows")

            # This is to display the user selection frame.
            cv2.namedWindow("mark the puck and players", cv2.WINDOW_NORMAL)
            cv2.resizeWindow("mark the puck and players", 100, 500)
           
            # This is to display the image frame.
            cv2.namedWindow("Image Window", cv2.WINDOW_NORMAL)
            cv2.resizeWindow("Image Window", 1280, 720)

            skip_key = False
            # First frame mouse click initialization processor.
            if(frame_num == start_frame):
                #print("show images and setup mouse callbacks")

                # Initial display of windows; wait for a keypress
                updateScreens()
                #print("first frame mouse callbacks")
                setMouseCallback()


                # turned off - resetting hot_key to true even when not using hot keys
                initial_key = cv2.waitKey(6000)
                # This is to display the frame on the full screen.
                if initial_key not in (-1, ord('n')): 
                    hot_key = True
                    print('hot keys selected')
                elif initial_key == ord('n'):
                    skip_key = True
                    end_key = initial_key
                
            # All subsequent frames use mouse click update processor.
            else:
                while hot_key:
                    # Display the image and wait for a keypress.
                    updateScreens()
                    #run hot keys until a "n" is entered
                    key = cv2.waitKey(0) & 0xFF
                    if(key == ord("n") or key == ord('e')):
                        skip_key = True
                        end_key = key
                        break
                    if key == ord('o'):
                        cv2.imshow('Image Window', orig_frame)
                        cv2.waitKey(0) & 0xFF
                    nf_UI_click_key(key)
                    cv2.setMouseCallback("Image Window", nf_Mark_click)
                    
                if hot_key == False:
                    # Display the image and wait for a keypress.
                    updateScreens()

                    setMouseCallback(nf = True)

            # keep looping until the 'q' key is pressed
            while True:

                #print("top of while, show images, wait for key press, hot_key:",hot_key)
                              
                # wait for a key press
                if skip_key:
                    key = end_key
                else:
                    key = cv2.waitKey(0) & 0xFF
                    

                #print("key:",key)

                # If the 'n' key is pressed, break from the loop and go to 
                # the next frame.
                if key == ord("n"):
                    #print("n key selected")
                    refPt = []
                    iclick  = -1


                    # Reset marking counter
                    mark_count = 0
                   
                    # Save the rink view image with ice contact point markings in a file.
                    RinkView_filename = './'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/Rink_Temp/'+'F'+str(frame_num)+'.jpeg'
                    cv2.imwrite(RinkView_filename, rink_im)

                    # maintain a list of file names
                    rink_files.append(RinkView_filename)

                    #print("===================================")
                    #print("frame:",frame_num)
                    #print("final frame frame_obj_CM_image_coords:",frame_obj_CM_image_coords)
                    #print("new on-ice_coords:",frame_obj_CM_image_coords)
                    #print("===================================")

                    # Save the final image with location markings and silhouettes in a file named by frame number.
                    Image_filename = './'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/Track_Temp/'+'F'+str(frame_num)+'.jpeg'
                    
                    # maintain a list of file names
                    image_files.append(Image_filename)

                    #print("frame",frame_num,"write Image filename:",Image_filename)
                    cv2.imwrite(Image_filename, frame)

                    break

                elif key == ord("e"):
                    #print("end key selected, marking ended")
                    stop_marking = True

                    #print("-----------------------------------")
                    #print("session ended; session_obj_ICP_image_coords:",session_obj_ICP_image_coords)
                    #print("-----------------------------------")


                if(stop_marking == True): break



            if(stop_marking == True): break

            # ---------------------------------
            # end if(frame_num > start_frame):
            # end of matching loop
            # ---------------------------------
        

        frame_num = frame_num + 1

        #print("incremented frame number:",frame_num)

    else:
        break
    



#print("-----------------------------------")
#print("end of marking; session_obj_ICP_image_coords:",session_obj_ICP_image_coords)
#print("-----------------------------------")

#print("image file names:",image_files)

# Marking done, write on-ice locations to a CSV file
# data to be written row-wise in csv file

# Set the output on-ice objects tracking video filename
tracking_filename = './'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/'+'P'+str(pass_num)+'_tracks.mp4'

# Set the output rink overhead view video filename
RinkView_filename = './'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/'+'P'+str(pass_num)+'_RinkView.mp4'

# Set the output on-ice objects image pixel contact points csv filename
image_ICP_filename = './'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/'+'P'+str(pass_num)+'_image_ICP.csv'
#print("filename:",filename)

# Set the output on-ice objects rink x,y contact points csv filename
rink_ICP_filename = './'+'/'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/'+'P'+str(pass_num)+'_rink_ICP.csv'
#print("filename:",filename)

# Set the output on-ice objects center of moments points csv filename
obj_CM_filename = './'+'/'+project_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/'+'P'+str(pass_num)+'_obj_CM.csv'
#print("filename:",filename)




def write_csv_file(file, data_type, nrows ):
    # Initialize file 
    with file: 
          
        # Initialize row 
        write = csv.writer(file)
        write.writerow(fields)
        for irow in range(0,nrows):
            row_data = []

            # Tracking pass number, Frame number
            row_data.append(data_type[irow][5][0])
            
            # Visiting team data
            for ivis in range(0,5):
                row_data.append(data_type[irow][0][ivis])

            # Home team data    
            for ihome in range(0,5):
                row_data.append(data_type[irow][1][ihome])
            
            # Goalie data 
            for igoalie in range(0,2):
                row_data.append(data_type[irow][2][igoalie])

            # Ref data                     
            for iref in range(0,4):
                row_data.append(data_type[irow][3][iref])

            # Puck data                    
            row_data.append(data_type[irow][4][0])

            # Write row to csv
            write.writerow(row_data)
    return

#write file headers
fields = ['Ses-Frm','V1', 'V2', 'V3', 'V4', 'V5', 'H1', 'H2', 'H3', 'H4', 'H5', 'GH', 'GV', 'R1', 'R2', 'R3', 'R4', 'Puck'] 

#writes image_ICP csv file
file = open(image_ICP_filename, 'w+', newline ='')    
nrows = len(session_obj_ICP_image_coords)
write_csv_file(file, session_obj_ICP_image_coords, nrows)

#writes obj_CM csv file
file = open(obj_CM_filename, 'w+', newline ='')
nrows = len(session_obj_CM_image_coords)
write_csv_file(file, session_obj_CM_image_coords, nrows)

#writes rink_ICP csv file
file = open(rink_ICP_filename, 'w+', newline ='')  
nrows = len(session_obj_ICP_rink_coords)
write_csv_file(file, session_obj_ICP_rink_coords, nrows)



#write images as an MP4 video
clip = ImageSequenceClip(image_files, fps = 24) 
clip.write_videofile(tracking_filename, fps = 24)

#write rink overhead views as an MP4 video
clip = ImageSequenceClip(rink_files, fps = 24) 
clip.write_videofile(RinkView_filename, fps = 24)
