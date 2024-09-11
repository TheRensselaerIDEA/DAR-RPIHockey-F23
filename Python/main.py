
import Step1_Extract_with_main
import Step2_Sub_image_with_main
#import Step2_OIO_finder_V2_with_main

import cv2, os

# Asks user for video clip name and project name 
print("This program lets you analyze different ice hockey sequences.")
video_name = str(input( "Enter a video clip to analyze: "))
video_file = video_name + '.mp4'
Proj_name = str(input("Enter project name: "))

# Creates directories for sequences to be put into
folder_path = "./" + Proj_name + "/Sequences"
os.makedirs(Proj_name, exist_ok= True)
sequence_folder_path = os.path.join(Proj_name, "Sequences")
os.makedirs(sequence_folder_path, exist_ok= True)

# Initialize sequence count 
n_sequences = 1

while True:
    #Displays various options
    print("Pick one of the three options: \n1) Cut sequences \n"
          "2) Enter new video clip \n3) Subtract all the sequences in the folder\n"
          "4) Subtract a specific sequence \n5) Quit")
    option = str(input("Your pick: "))

    #Asks for new video file clip 
    if(option == str(2)):
        video_name = str(input("Enter new video name: "))
        video_file = video_name + ".mp4"
        
    #Starts video to be cut 
    if(option == str(1) or (option == str(2))):
        
        # Keeps track of total number of sequences 
        while True:
            n_sequences += Step1_Extract_with_main.cut_sequences(n_sequences, video_file, Proj_name)
            cv2.waitKey(1)
            next_option = str(input("Would you like to cut another sequence from this video?(Y/N): "))
            
            # Breaks out of loop when user is done cutting sequences from current video 
            if(next_option != "Y" and next_option != "y"): break
           
    # Cut all the sequences in the file 
    elif(option == str(3)):
        # Get total number of sequences and do step 2 on each 
        sequences_in_folder = os.listdir(folder_path)
        for sequence_number in range(1, len(sequences_in_folder)+1):
            Step2_Sub_image_with_main.sub_image(Proj_name, sequence_number)
        
    # Cut a specific sequence
    elif(option == str(4)):
        selected_sequence = int(input("Which sequence would you like to subtract?"))
        Step2_Sub_image_with_main.sub_image(Proj_name, selected_sequence)
        
    # End program   
    elif(option == str(5)):
        break
    
    # Invalid comaand entered
    elif(option != str(2)):
        print("Enter a valid command!\n")
   
    
   
        
        
    





