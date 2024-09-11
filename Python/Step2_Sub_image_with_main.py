# -----------------------------------------------------------------------
# Step2_Skeleton_generation.py
#
# This is the second step in the three step end-to-end player tracking
# processing suite. 
#
# Generate player skeletons from on-ice player images.
#
# An input video subsequence is processed frame-by-frame with the
# Open_Pose AI application to locate and mark player body part key
# points from which skelton-like maps of the player's body pose can
# be constructed. The results are output as an MP4 file that is input
# to Step3 and will serve as background for the final Step3 results. 
#
# An intermediate processing data product, empty ice subtracted frames
# are also output as an MP4 file. These will serve as primary image
# input to Step 3.
# 
# Input: one instance of subseqence_of_on-ice_action_video.MP4
# Output: player_skeletons_video.MP4
# Output: empty_ice_subtracted_subseqence_of_on-ice_action_video.MP4
#
# Initial create: 3 March, 2023 by T. Morgan
#
# -----------------------------------------------------------------------
# import packages
# ----------------------------------------------------------------------
import numpy as np
from numpy import array
from scipy import ndimage
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

import math

from matplotlib import cm
from matplotlib import colors
from matplotlib.colors import hsv_to_rgb

from pylab import rcParams
import argparse
import cv2,os,sys
import glob
import matplotlib.pyplot as plt
import imutils
import csv
from copy import deepcopy

from PIL import Image, ImageOps
from PIL import ImageEnhance

from moviepy.editor import *

from skimage import filters

from scipy import ndimage as ndi


from skimage.segmentation import watershed
from skimage.feature import peak_local_max

from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_samples, silhouette_score

from sklearn.cluster import spectral_clustering

from sklearn.feature_extraction import image

from sklearn.cluster import DBSCAN
from sklearn import metrics
from sklearn.datasets import make_blobs
from sklearn.preprocessing import StandardScaler

# For OPEN POSE
import time
from random import randint

# ----------------------------------
# Generic image display function
# ----------------------------------
def im_display(image, title, scale=0.0, x_move=0, y_move=0):

    image_sc = cv2.resize(image, (0,0), fx=scale, fy=scale)
    cv2.imshow(title, image_sc)
    cv2.moveWindow(title,x_move,y_move)
    cv2.waitKey(0)

    return

# ----------------------------------------------------------------------
# Endzone video for testing.
# ----------------------------------------------------------------------

def sub_image(Proj_name, seq_num):
    # BGR red - used for player contour display
    blue_color = (255,0,0)
    green_color = (0,255,0)
    red_color = (0,0,255)
    white_color = (255,255,255)
    black_color = (0,0,0)
    magenta_color = (255,0,255)
    cyan_color = (255,255,0)
    yellow_color = (0,255,255)
    
    # ----------------------------------------------------------------------
    # Start frame.
    # ----------------------------------------------------------------------
    #print("starting main program")
    start_frame = 1

    # List of the subtracted image temp files.
    

    # ----------------------------------------------------------------------
    # Display parameters
    # -----------------------------------------------------------------------
    # Scaling factor for displayed image so it fits nicely on the screen.

    xy_scale = 0.4

    # -----------------------------------------------------------------------
    # Frame processing parameters.
    # -----------------------------------------------------------------------
    # absolute frame number counter
    frame_num = 1

    # -----------------------------------------------------------------------
    # Open input file, skip ahead to the desired input frame and grab it.
    # Then create a true direct overhead image ready for OIO detection.
    # -----------------------------------------------------------------------
    # Open input video feed.
    input_filename = './'+Proj_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+ '/Seq_'+str(seq_num)+'.mp4'
    cap = cv2.VideoCapture(input_filename)
    print("Is file open:",input_filename,cap.isOpened())

    # read in frame size to determine what parameters should be used
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    speed = int(cap.get(cv2.CAP_PROP_FPS))

    
    print("width: ", width, " height: ", height)
    if(width == 1280 and height == 720):
        print("mens frame")        
        empty_im = cv2.imread("Empty_ice_frame_mens.jpeg")
    elif(width == 1920 and height == 1080):
        print("womens frame")        
        empty_im = cv2.imread("Empty_ice_frame_womens.jpeg")
    
    #empty_im_filename = cv2.imread("Empty_ice_frame.jpeg")
    #empty_im_filename = './'+Proj_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+ '/Empty_ice_frame.jpeg'
    #print("empty_im_filename:",empty_im_filename)

    # this is the one that is giving bad results
    #empty_im = cv2.imread(empty_im_filename)
    #im_display(empty_im,"empty_image", xy_scale)


    
    # TEST
    #empty_im_filename_2 = './'+Proj_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+ '/Good_Empty_ice_frame.jpeg'
    #print("empty_im_filename_2:",empty_im_filename_2)
    #empty_im_2 = cv2.imread(empty_im_filename_2)
    #im_display(empty_im_2,"empty_image_2", xy_scale)


    # define the contrast and brightness value
    # Alpha = Contrast control ( 0 to 127)
    # Beta = Brightness control (0 to 100)
    # define the alpha and beta
    #alpha = 1 # Contrast control

    # higher contrast of the empty image makes things worse
    #beta = 50 # Brightness control

    # call convertScaleAbs function
    #empty_im = cv2.convertScaleAbs(empty_im, alpha=alpha, beta=beta)
    #im_display(empty_im,"empty_image adjusted", xy_scale)

    

    #sub_image = cv2.subtract(empty_im_2, empty_im)
    #im_display(sub_image,"sub_image 2 minus 1", xy_scale)
    
    #sub_image = cv2.subtract(empty_im, empty_im_2)
    #im_display(sub_image,"sub_image 1 minus 2", xy_scale)





    
    im_rows,im_cols,im_channels = empty_im.shape
    #print("empty image frame x cols by y rows; chans:",im_cols,im_rows,im_channels)

    image_file = []
  


    #print("Input frame height, width, frame rate:",height,width,speed)
           
    # Create output subtracted image filename.
    sub_filename = './'+Proj_name+'/Sequences/'+'Files_Seq_'+str(seq_num)+'/subs.mp4'
    #print("sub file name  ", sub_filename)

    # Open output video feed
    video_writer = cv2.VideoWriter(sub_filename, cv2.VideoWriter_fourcc(*"mp4v"), speed, (width, height))

    # read next frame
    while cap.isOpened():
 
        # NOTE: cv2 reads images as B,G,R
        ret, frame = cap.read()
        #print(ret)

        # Note: Endzone view extracted video sequence - no longer have to 
        # skip frames when 60 fps, sequence extractor has taken care of the duplicates.
            
        # there was a next frame
        if ret:

            frame_grey = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
            frame_contrast = frame_grey.std()
            #print("frame contrast:",frame_contrast)

            
            cols, rows, chans = frame.shape
            frame_brightness = np.sum(frame) / (255 * cols * rows)
            #print("frame brightness:",frame_brightness)



            empty_grey = cv2.cvtColor(empty_im, cv2.COLOR_BGR2GRAY)
            empty_contrast = empty_grey.std()
            #print("empty_im contrast",empty_contrast)

            cols, rows, chans = empty_im.shape
            empty_brightness = np.sum(empty_im) / (255 * cols * rows)
            #print("empty_im brightness:",empty_brightness)

            delta_contrast = empty_contrast - frame_contrast
            if(delta_contrast < 0): delta_contrast = -delta_contrast
            delta_brightness = empty_brightness - frame_brightness
            #print("delta contrast and brightness:",delta_contrast,delta_brightness)

            alpha = 1.0 + delta_contrast
            
            if(alpha > 1.4): alpha = 1.4
            if (alpha < .06): alpha = 1.4
            beta = 255.0 * delta_brightness

            #print("alpha, beta:",alpha, beta)

            # Try higher brightness on the frame image
            #alpha = 1.0 # Contrast control
            #beta = 25 # Brightness control
            frame = cv2.convertScaleAbs(frame, alpha=alpha, beta=beta)
            #im_display(frame,"frame image adjusted", xy_scale)

            #alpha = 0.6 # Contrast control
            #beta = -23.0 # Brightness control
            #empty_im = cv2.convertScaleAbs(empty_im, alpha=alpha, beta=beta)
            #im_display(empty_im,"empty_im adjusted", xy_scale)

            # ----------------------------------------------------------------------
            # Subtract the empty ice image (no players or refs).
            # This gets rid of all the on-ice markings and leaves only the players
            # and refs.
            # ----------------------------------------------------------------------
            sub_image = cv2.subtract(empty_im, frame)
            #sub_image = cv2.subtract(frame, empty_im)

            # Invert the colors after subtraction so the background is white again.
            # This will be the input to OpenPose.
            sub_im_inv = cv2.bitwise_not(sub_image)
            #print("sub image shape: ", sub_im_inv.shape)
            
            # Blank out the stands depending on what size the image is
            if(width == 1280 and height == 720):  
                # Blank out the areas outside the rink.
                points = np.array([[0, 0], [1280, 0], [1280, 170], [0, 170]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))
                points = np.array([[0, 160], [430, 160], [430, 165], [0, 540]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))
                points = np.array([[800, 160], [1280, 160], [1280, 165], [1280, 450]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))

                # Set display scale.
                xy_scale = 1.0

            elif(width == 1920 and height == 1080): 
                # These values are for 1920x1080 frame size
                # Top
                points = np.array([[0, 0], [1920, 0], [1920, 200], [0, 200]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))

                # Left upper triangle
                points = np.array([[0, 200], [735, 200], [735, 255], [0, 800]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))

                # Right upper triangle
                points = np.array([[1150, 200], [1920, 200], [1920, 225], [1920, 800]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))

                # Lower left corner 
                points = np.array([[0, 850], [900, 1080], [890, 1080], [0, 1080]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))

                # Lower right corner 
                points = np.array([[900, 1080], [910, 1080], [1920, 1080], [1920, 900]])
                cv2.fillPoly(sub_im_inv,pts=[points],color=(white_color))

                # Set display scale.
                xy_scale = 0.7
            
            # generate and display the home player mask
            # This color set works best for red, black and white uniform combinations.
            home_lower = np.array([0, 0, 220], dtype=np.uint8)
            home_upper = np.array([255 ,255, 255], dtype=np.uint8)

            home_mask = cv2.inRange(sub_im_inv, home_lower, home_upper)
            #im_display(home_mask,"home player mask", xy_scale)

            # NOTE: At this point the home_mask is only a single channel
            # Set erosion kernel for dilate function. 3 by 3 gives best result.          
            #kernel = np.ones((3,3), np.uint8)
            #kernel = np.ones((2,2), np.uint8)

            # Apply mild erosion to bit map to cleanup the potential contoured objects.
            # For 27_Jan_23 data use this:
            kernel = np.ones((4,4), np.uint8)
            eroded_mask = cv2.erode(home_mask, kernel)
            #eroded_mask = cv2.erode(home_mask, kernel0)

            
            # Diagnostic displays
            #im_display(frame,"input frame", xy_scale)

            #im_display(empty_im,"empty_image", xy_scale)

            #im_display(sub_image,"sub_image", xy_scale)

            #im_display(sub_im_inv,"sub_im_inv", xy_scale)

            #im_display(eroded_mask,"eroded_mask", xy_scale)

            # Writes frame to mp4 file
            video_writer.write(sub_im_inv)

            # advance the frame count
            frame_num = frame_num + 1

        # End of file reached, release the caps 
        else: 
            print("end of file reached at frame:",frame_num)
            video_writer.release()
            cap.release()
            break     

   