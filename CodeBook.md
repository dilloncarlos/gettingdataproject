Description of Dataset1 and Dataset2
===========

Description of the variables stored in Dataset1 and Dataset2 


### Dataset1

Each row represents a different experiment, in which data were acquired on the six following activities: WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING. Only the mean and standard deviation variables are presented in Dataset1.

The following is a reprentation of the columns of Dataset1, beginning with subject identity and activity type. 

 [1] "subject"                        
 [2] "activity"                       
 [3] "tBodyAcc-mean()-X"              
 [4] "tBodyAcc-mean()-Y"              
 [5] "tBodyAcc-mean()-Z"              
 [6] "tBodyAcc-std()-X"               
 [7] "tBodyAcc-std()-Y"               
 [8] "tBodyAcc-std()-Z"               
 [9] "tGravityAcc-mean()-X"           
[10] "tGravityAcc-mean()-Y"           
[11] "tGravityAcc-mean()-Z"           
[12] "tGravityAcc-std()-X"            
[13] "tGravityAcc-std()-Y"            
[14] "tGravityAcc-std()-Z"            
[15] "tBodyAccJerk-mean()-X"          
[16] "tBodyAccJerk-mean()-Y"          
[17] "tBodyAccJerk-mean()-Z"          
[18] "tBodyAccJerk-std()-X"           
[19] "tBodyAccJerk-std()-Y"           
[20] "tBodyAccJerk-std()-Z"           
[21] "tBodyGyro-mean()-X"             
[22] "tBodyGyro-mean()-Y"             
[23] "tBodyGyro-mean()-Z"             
[24] "tBodyGyro-std()-X"              
[25] "tBodyGyro-std()-Y"              
[26] "tBodyGyro-std()-Z"              
[27] "tBodyGyroJerk-mean()-X"         
[28] "tBodyGyroJerk-mean()-Y"         
[29] "tBodyGyroJerk-mean()-Z"         
[30] "tBodyGyroJerk-std()-X"          
[31] "tBodyGyroJerk-std()-Y"          
[32] "tBodyGyroJerk-std()-Z"          
[33] "tBodyAccMag-mean()"             
[34] "tBodyAccMag-std()"              
[35] "tGravityAccMag-mean()"          
[36] "tGravityAccMag-std()"           
[37] "tBodyAccJerkMag-mean()"         
[38] "tBodyAccJerkMag-std()"          
[39] "tBodyGyroMag-mean()"            
[40] "tBodyGyroMag-std()"             
[41] "tBodyGyroJerkMag-mean()"        
[42] "tBodyGyroJerkMag-std()"         
[43] "fBodyAcc-mean()-X"              
[44] "fBodyAcc-mean()-Y"              
[45] "fBodyAcc-mean()-Z"              
[46] "fBodyAcc-std()-X"               
[47] "fBodyAcc-std()-Y"               
[48] "fBodyAcc-std()-Z"               
[49] "fBodyAcc-meanFreq()-X"          
[50] "fBodyAcc-meanFreq()-Y"          
[51] "fBodyAcc-meanFreq()-Z"          
[52] "fBodyAccJerk-mean()-X"          
[53] "fBodyAccJerk-mean()-Y"          
[54] "fBodyAccJerk-mean()-Z"          
[55] "fBodyAccJerk-std()-X"           
[56] "fBodyAccJerk-std()-Y"           
[57] "fBodyAccJerk-std()-Z"           
[58] "fBodyAccJerk-meanFreq()-X"      
[59] "fBodyAccJerk-meanFreq()-Y"      
[60] "fBodyAccJerk-meanFreq()-Z"      
[61] "fBodyGyro-mean()-X"             
[62] "fBodyGyro-mean()-Y"             
[63] "fBodyGyro-mean()-Z"             
[64] "fBodyGyro-std()-X"              
[65] "fBodyGyro-std()-Y"              
[66] "fBodyGyro-std()-Z"              
[67] "fBodyGyro-meanFreq()-X"         
[68] "fBodyGyro-meanFreq()-Y"         
[69] "fBodyGyro-meanFreq()-Z"         
[70] "fBodyAccMag-mean()"             
[71] "fBodyAccMag-std()"              
[72] "fBodyAccMag-meanFreq()"         
[73] "fBodyBodyAccJerkMag-mean()"     
[74] "fBodyBodyAccJerkMag-std()"      
[75] "fBodyBodyAccJerkMag-meanFreq()" 
[76] "fBodyBodyGyroMag-mean()"        
[77] "fBodyBodyGyroMag-std()"         
[78] "fBodyBodyGyroMag-meanFreq()"    
[79] "fBodyBodyGyroJerkMag-mean()"    
[80] "fBodyBodyGyroJerkMag-std()"     
[81] "fBodyBodyGyroJerkMag-meanFreq()"

### Dataset2

Dataset2 is a further tidying of Dataset1.

The following is a reprentation of the columns of Dataset1, beginning with subject identity and activity type. Note that now in this tidier dataset, the data are presented as means of means, and are hence more compact. 

 [1] "subject"                        
 [2] "activity"                       
 [3] "tBodyAcc-mean()-X"              
 [4] "tBodyAcc-mean()-Y"              
 [5] "tBodyAcc-mean()-Z"              
 [6] "tGravityAcc-mean()-X"           
 [7] "tGravityAcc-mean()-Y"           
 [8] "tGravityAcc-mean()-Z"           
 [9] "tBodyAccJerk-mean()-X"          
[10] "tBodyAccJerk-mean()-Y"          
[11] "tBodyAccJerk-mean()-Z"          
[12] "tBodyGyro-mean()-X"             
[13] "tBodyGyro-mean()-Y"             
[14] "tBodyGyro-mean()-Z"             
[15] "tBodyGyroJerk-mean()-X"         
[16] "tBodyGyroJerk-mean()-Y"         
[17] "tBodyGyroJerk-mean()-Z"         
[18] "tBodyAccMag-mean()"             
[19] "tGravityAccMag-mean()"          
[20] "tBodyAccJerkMag-mean()"         
[21] "tBodyGyroMag-mean()"            
[22] "tBodyGyroJerkMag-mean()"        
[23] "fBodyAcc-mean()-X"              
[24] "fBodyAcc-mean()-Y"              
[25] "fBodyAcc-mean()-Z"              
[26] "fBodyAcc-meanFreq()-X"          
[27] "fBodyAcc-meanFreq()-Y"          
[28] "fBodyAcc-meanFreq()-Z"          
[29] "fBodyAccJerk-mean()-X"          
[30] "fBodyAccJerk-mean()-Y"          
[31] "fBodyAccJerk-mean()-Z"          
[32] "fBodyAccJerk-meanFreq()-X"      
[33] "fBodyAccJerk-meanFreq()-Y"      
[34] "fBodyAccJerk-meanFreq()-Z"      
[35] "fBodyGyro-mean()-X"             
[36] "fBodyGyro-mean()-Y"             
[37] "fBodyGyro-mean()-Z"             
[38] "fBodyGyro-meanFreq()-X"         
[39] "fBodyGyro-meanFreq()-Y"         
[40] "fBodyGyro-meanFreq()-Z"         
[41] "fBodyAccMag-mean()"             
[42] "fBodyAccMag-meanFreq()"         
[43] "fBodyBodyAccJerkMag-mean()"     
[44] "fBodyBodyAccJerkMag-meanFreq()" 
[45] "fBodyBodyGyroMag-mean()"        
[46] "fBodyBodyGyroMag-meanFreq()"    
[47] "fBodyBodyGyroJerkMag-mean()"    
[48] "fBodyBodyGyroJerkMag-meanFreq()"
