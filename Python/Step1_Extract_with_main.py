# -----------------------------------------------------------------------
# Step1_Extract_frames.py
#
# This is the initial step in the three step end-to-end player tracking
# processing suite. 
#
# Extract sequences of frames from an MP4 file and write as new MP4's.
#
# The user interactively marks the beginning and end frames of sub-
# sequences of an input raw MP4 video file. The marked saegments are
# extracted and output as separate MP4 files that can be input to 
# Step2 of the processing sequence.
#
# Input: on-ice_action_video.MP4
# Output: multiple instances of subseqence_of_on-ice_action_video.MP4
#
# Initial create: 3 March, 2023 by T. Morgan
# -----------------------------------------------------------------------
# import packages
# ----------------------------------------------------------------------


from pylab import rcParams
import argparse
import cv2,os,sys
import glob
import matplotlib.pyplot as plt
import imutils
import csv
from copy import deepcopy

from moviepy.editor import *






# --------------------------------
# Generic image display function.
# --------------------------------
def im_display(image, title, scale=0.0, x_move=0, y_move=0):

    image_sc = cv2.resize(image, (0,0), fx=scale, fy=scale)
    cv2.imshow(title, image_sc)
    cv2.moveWindow(title,x_move,y_move)
    cv2.waitKey(0)

# ----------------------------------------------------------------------
# Main input video for frame sequence extraction.
# ----------------------------------------------------------------------

    

def cut_sequences(seq_num, filename, Proj_name):

    
    # ----------------------------------------------------------------------
    # Start frame for the main input video.
    # ----------------------------------------------------------------------
    frame_num = int(input("start frame number:"))
    print("selected start frame:",frame_num)
    # text display color
    red_color = (0,0,255)
    
    # editing flags
    stop_editing = False
    seq_start = False


    # Set sequence start frame to zero. It will be used to determine if the
    # current frame should be saved for output to sequence video.

    # ----------------------------------------------------------------------
    # Display parameters
    # -----------------------------------------------------------------------
    
    # Open video feed
    cap = cv2.VideoCapture(filename)
    print("cap:",cap)
    print("Is file open:",filename,cap.isOpened())
    cap.set(cv2.CAP_PROP_POS_FRAMES,frame_num)

    # Read in frame size to determine what parrameters should be used
    height = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))
    width = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
    speed = int(cap.get(cv2.CAP_PROP_FPS))
    
    # This is to display the image frame.
    cv2.namedWindow("Image Window", cv2.WINDOW_NORMAL)
    cv2.resizeWindow("Image Window", width, height )

    # Read next frame
    while cap.isOpened():
        
        # NOTE: cv2 reads images as B,G,R
        ret, frame = cap.read()
        
        # If there was a next frame
        if ret:

            # Keep looping until the 'q' key is pressed
            while True:
                
                # Display curent image with frame number 
                cv2.putText(frame, str(frame_num), ((923),(163)), cv2.FONT_HERSHEY_SIMPLEX, 1.0, red_color, 3)
                cv2.imshow("Image Window", frame)
                key = cv2.waitKey(1) & 0xFF

                # If the 'n' key is pressed, break from the loop and go to the next frame.
                if key == ord("n"):
                    
                    #Add end of the frame to video writer after checking seq was initialized 
                    if(seq_start == True): video_writer.write(frame)
                    break

                # case: start of session, nothing yet selected.
                elif ((seq_start == False) and (key == ord("s"))):
                    print("1:seq_start, seq_end:",seq_start)
                    
                    # Create a path in directory for the new sequence folder
                    sequence_folder_path = os.path.join(Proj_name+ "/Sequences",'Files_Seq_' + str(seq_num))
                    os.makedirs(sequence_folder_path, exist_ok= True)
                    video_filename = './'+Proj_name+'/Sequences/'+'Files_Seq_' + str(seq_num) + '/Seq_'+str(seq_num)+'.mp4'
                    
                    # Start video writer 
                    video_writer = cv2.VideoWriter(video_filename, cv2.VideoWriter_fourcc(*"mp4v"), speed, (width, height))
                    print("sequence video file name:",video_filename)
                    seq_start = True
                    break
             
                # case: no outstanding starts, end session.
                elif (key == ord("q") or (key == ord("e")) and (seq_start == True)):
                    
                    # Release video writer 
                    video_writer.release()
                    stop_editing = True
                    break
                
                # case: rewind by 30 frames if no outstanding start
                elif((seq_start == False) and (key == ord("r"))):
                    frame_num -= 30
                    cap.set(cv2.CAP_PROP_POS_FRAMES,frame_num)
                    break
            
                # case: speed up by 30 frames if no outstanding start
                elif((seq_start == False) and key == ord("f")):
                    frame_num += 30
                    cap.set(cv2.CAP_PROP_POS_FRAMES,frame_num)
                    break

            if(stop_editing == True): break
            
            # Increase frame count by one 
            frame_num = frame_num + 1

        # End of the file has been reached 
        if not ret: 
            print("end of file reached at frame:",frame_num)
            cap.release()
            break 
        
    # Release video writer and destroy the open window   
    cv2.waitKey(1)
    cap.release()
    cv2.destroyAllWindows()
    return 1
    