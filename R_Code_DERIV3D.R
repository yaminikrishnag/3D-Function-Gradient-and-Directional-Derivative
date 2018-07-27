#######################################################################
# Function A(x,y,z)
#######################################################################

par(mfrow=c(3,3))

#######################################################################
# Set the Current Directory to Input file folder
#######################################################################

setwd('C:\\Users\\MANJU VISVANATHAN\\Desktop\\DERIV2D_Advanced_Numerica_Analysis_Project\\DERIV3D')

#######################################################################
#Read the Function A(x,y,z)
#######################################################################

functionA_XYZ <- read.csv(file="DERIV3D_functionA_XYZ.csv",header=FALSE) 

#######################################################################
# Assign Column Names to function A(x,y,z)
#######################################################################

colnames(functionA_XYZ) <- c("A_x","A_y","A_z")

#######################################################################
# Create an Index Column in function A(x,y,z)
#######################################################################

functionA_XYZ["Index"] <- seq.int(nrow(functionA_XYZ))

#######################################################################
# Plots for function A
#######################################################################

plot(Index_A,A_x, type = 'l', main = "Scatterplot of A(x)", ylab= "A(x)", xlab = "Index", col= "blue" )
plot(Index_A,A_y, type = 'l', main = "Scatterplot of A(y)", ylab= "A(y)", xlab = "Index", col= "red" )
plot(Index_A,A_z, type = 'l', main = "Scatterplot of A(z)", ylab= "A(z)", xlab = "Index", col= "darkgreen" )

#######################################################################
# Find Derivative for A(X)
#######################################################################

# Assign to another variable

A_x <- functionA_XYZ$A_x

Index_A <- functionA_XYZ$Index

#######################################################################
# Calculate Derivative of A(x)
#######################################################################

for(i in 1: length(Index_A))
{
  functionA_XYZ$derivative_Ax[i] = ( A_x[i+1] - A_x[i] )  /  (Index_A[i+1] - Index_A[i])
}

# Omit NA Values from the Derivative of A(X)

dAx = na.omit(functionA_XYZ$derivative_Ax)

#######################################################################
# Normalize the Derivative of function A'(X)
#######################################################################

Min_dAx = min(dAx)
Max_dAx = max(dAx)

Norm_dAx <- (dAx  - Min_dAx) / (Max_dAx - Min_dAx)

#######################################################################
# Scatterplot of Derivative of A'(X) before Normalization
#######################################################################

plot(Index_A[1:length(dAx)],dAx, type = 'l', main = "Scatterplot of A'(x)", ylab= "Derivative of A(x)", xlab = "Index", col= "blue" )


#######################################################################
# Find Derivative for A(Y)
#######################################################################

# Assign to another variable

A_y <- functionA_XYZ$A_y

Index_A <- functionA_XYZ$Index

# Calculate Derivative of A(y)

for(i in 1: length(Index_A))
{
  functionA_XYZ$derivative_Ay[i] = ( A_y[i+1] - A_y[i] )  /  (Index_A[i+1] - Index_A[i])
}

# Omit NA Values from the Derivative of A(Y)

dAy = na.omit(functionA_XYZ$derivative_Ay)

#######################################################################
# Normalize the Derivative of function A(Y)
#######################################################################

Min_dAy = min(dAy)
Max_dAy = max(dAy)

Norm_dAy <- ( dAy  - Min_dAy ) / (Max_dAy - Min_dAy)

#######################################################################
# Scatterplot of Derivative of A'(y) before Normalization
#######################################################################

plot(Index_A[1:length(dAy)],dAy, type = 'l', main = "Scatterplot of A'(y)", ylab= "Derivative of A(y)", xlab = "Index", col= "red")


#######################################################################
# Find Derivative for A(z)
#######################################################################

# Assign to another variable

A_z <- functionA_XYZ$A_z

Index_A <- functionA_XYZ$Index

# Calculate Derivative of A(z)

for(i in 1: length(Index_A))
{
  functionA_XYZ$derivative_Az[i] = ( A_z[i+1] - A_z[i] )  /  (Index_A[i+1] - Index_A[i])
}

# Omit NA Values from the Derivative of A(z)

dAz = na.omit(functionA_XYZ$derivative_Az)

#######################################################################
# Normalize the Derivative of function A(z)
#######################################################################

Min_dAz = min(dAz)
Max_dAz = max(dAz)

Norm_dAz <- ( dAz  - Min_dAz ) / (Max_dAz - Min_dAz)

#######################################################################
# Scatterplot of Derivative of A(z) before Normalization
#######################################################################

plot(Index_A[1:length(dAz)],dAz, type = 'l', main = "Scatterplot of A'(z)", ylab= "Derivative of A(z)", xlab = "Index", col= "darkgreen")


#######################################################################
# Plots for Derivative of function A
#######################################################################

#plot_ly(x = ~A_x, y = ~A_y, z = ~A_z, type = 'scatter3d', mode = 'lines', opacity = 1, line = list(width = 6,color="blue", reverscale = FALSE))

scatter3D(A_x, A_y, A_z, type= "l",clab = c("A(x,y,z)"),  xlab= "A(x)",ylab="A(y)",zlab="A(z)", main="A(x) Vs A(y) Vs A(z) ",add = FALSE)

scatter3D(dAx, dAy, dAz, type= "l",clab = c("A'(x,y,z)"),  xlab= "A'(x)",ylab="A'(y)",zlab="A'(z)", main="A'(x) Vs A'(y) Vs A'(z) ",add = FALSE)

##############################################################################################################################################
##############################################################################################################################################












#######################################################################
# Function B(X,Y,Z)
#######################################################################

par(mfrow=c(3,3))

#######################################################################
#Read the Function B(X,Y)
#######################################################################

functionB_XYZ <- read.csv(file="DERIV3D_functionB_XYZ.csv",header=FALSE) 

#######################################################################
# Assign Column Names to function B(x,y)
#######################################################################

colnames(functionB_XYZ) <- c("B_x","B_y","B_z")

#######################################################################
# Create an Index Column in function B(x,y)
#######################################################################

functionB_XYZ["Index"] <- seq.int(nrow(functionB_XYZ))

########################################################################
# Reduce Number of points of Function B to have same range as Function A
########################################################################

# Assign to another variable

Index_A <- functionA_XYZ$Index
Index_B <- functionB_XYZ$Index

B_x <- functionB_XYZ$B_x
B_y <- functionB_XYZ$B_y
B_z <- functionB_XYZ$B_z

#######################################################################
# Plots for function B
#######################################################################

plot(Index_B,B_x, type = 'l', main = "Scatterplot of B(x)", ylab= "B(x)", xlab = "Index", col= "blue" )
plot(Index_B,B_y, type = 'l', main = "Scatterplot of B(y)", ylab= "B(y)", xlab = "Index", col= "red" )
plot(Index_B,B_z, type = 'l', main = "Scatterplot of B(z)", ylab= "B(z)", xlab = "Index", col= "darkgreen" )

#######################################################################
# Ratio of No.of points in Function B(x,y) / No.of points in Function A(x,y)
#######################################################################

Split_No = as.integer( length(Index_B) / length(Index_A) )

# No.of Reduced B(x,y) values

No_Reduced_B_points = as.integer( length(Index_B) / Split_No )

#######################################################################  
# Create an array to store the reduced function B(x) values
#######################################################################

reduced_Bx  = rep(0,No_Reduced_B_points)

#######################################################################
# Find Average among each subset of points in B(x)
#######################################################################

j=1

for(i in 1: No_Reduced_B_points)
{
  reduced_Bx[i] = sum(B_x[j:(j+4)]) / 5
  
  j= j+5
}
#######################################################################
# Scatterplot of Reduced B(x) 
#######################################################################

plot(Index_B[1:length(reduced_Bx)],reduced_Bx, type = 'l', main = "Scatterplot of Reduced B(x)", ylab= "Reduced B(x)", xlab = "Index", col= "blue")

#######################################################################
# Create an array to store the reduced B(y) values
#######################################################################

reduced_By  = rep(0,No_Reduced_B_points)

#######################################################################
# Find Average among each subset of points in B(y)
#######################################################################
k=1

for(a in 1: No_Reduced_B_points)
{
  reduced_By[a] = sum(B_y[k:(k+4)]) / 5
  
  k= k+5
}
#######################################################################
# Scatterplot of Reduced B(y) 
#######################################################################

plot(Index_B[1:length(reduced_By)],reduced_By, type = 'l', main = "Scatterplot of Reduced B(y)", ylab= "Reduced B(y)", xlab = "Index", col= "red")

#######################################################################
# Create an array to store the reduced B(z) values
#######################################################################

reduced_Bz  = rep(0,No_Reduced_B_points)

#######################################################################
# Find Average among each subset of points in B(z)
#######################################################################
p=1

for(b in 1: No_Reduced_B_points)
{
  reduced_Bz[b] = sum(B_z[p:(p+4)]) / 5
  
  p= p+5
}
#######################################################################
# Scatterplot of Reduced B(z) 
#######################################################################

plot(Index_B[1:length(reduced_Bz)],reduced_Bz, type = 'l', main = "Scatterplot of Reduced B(z)", ylab= "Reduced B(z)", xlab = "Index", col= "darkgreen")

#######################################################################
# Plots for function B
#######################################################################

#plot_ly(x = ~B_x, y = ~B_y, z = ~B_z, type = 'scatter3d', mode = 'lines', opacity = 1, line = list(width = 6,color="blue", reverscale = FALSE))
scatter3D(B_x, B_y, B_z, type= "l",clab = c("B(x,y,z)"),  xlab= "B(x)",ylab="B(y)",zlab="B(z)", main="B(x) Vs B(y) Vs B(z) ",add = FALSE)

#plot_ly(x = ~reduced_Bx, y = ~reduced_By, z = ~reduced_Bz, type = 'scatter3d', mode = 'lines', opacity = 1, line = list(width = 6,color="blue", reverscale = FALSE))
scatter3D(reduced_Bx, reduced_By, reduced_Bz, type= "l",clab = c("Reduced B(x,y,z)"),  xlab= "Reduced B(x)",ylab="Reduced B(y)",zlab="Reduced B(z)", main="Reduced B(x,y,z) ",add = FALSE)

#######################################################################
# Normalize the reduced B(X)
#######################################################################

Min_Bx = min(reduced_Bx)
Max_Bx = max(reduced_Bx)

Norm_Bx <- ( reduced_Bx  - Min_Bx ) / (Max_Bx - Min_Bx)

#######################################################################
# Normalize the reduced B(y)
#######################################################################

Min_By = min(reduced_By)
Max_By = max(reduced_By)

Norm_By <- ( reduced_By  - Min_By ) / (Max_By - Min_By)


#######################################################################
# Normalize the reduced B(z)
#######################################################################

Min_Bz = min(reduced_Bz)
Max_Bz = max(reduced_Bz)

Norm_Bz <- ( reduced_Bz  - Min_Bz ) / (Max_Bz - Min_Bz)

##############################################################################################################################################
##############################################################################################################################################













#######################################################################
# Normalization Plots
#######################################################################
par(mfrow=c(2,3))

#######################################################################
# Scatterplot of A'(X) after Normalization
#######################################################################

plot(Index_A[1:length(Norm_dAx)],Norm_dAx, type = 'l', main = "Scatterplot of A'(x) after Normalization", ylab= "Derivative of A(x)", xlab = "Index", col= "blue" )

#######################################################################
# Scatterplot of A'(y) after Normalization
#######################################################################

plot(Index_A[1:length(Norm_dAy)],Norm_dAy, type = 'l', main = "Scatterplot of A'(y) after Normalization", ylab= "Derivative of A(y)", xlab = "Index", col= "red")

#######################################################################
# Scatterplot of A'(z) after Normalization
#######################################################################

plot(Index_A[1:length(Norm_dAz)],Norm_dAz, type = 'l', main = "Scatterplot of A'(z) after Normalization", ylab= "Derivative of A(z)", xlab = "Index", col= "darkgreen")

#######################################################################
# Scatterplot of B(x) after Normalization
#######################################################################

plot(Index_B[1:length(Norm_dAx)],Norm_Bx[1:length(Norm_dAx)], type = 'l', main = "Scatterplot of B(x) after Normalization", ylab= "B(x)", xlab = "Index", col= "blue")

#######################################################################
# Scatterplot of B(y) after Normalization
#######################################################################

plot(Index_B[1:length(Norm_dAy)],Norm_By[1:length(Norm_dAy)], type = 'l', main = "Scatterplot of B(y) after Normalization", ylab= "B(y)", xlab = "Index", col= "red")

#######################################################################
# Scatterplot of B(z) after Normalization
#######################################################################

plot(Index_B[1:length(Norm_dAz)],Norm_Bz[1:length(Norm_dAz)], type = 'l', main = "Scatterplot of B(z) after Normalization", ylab= "B(z)", xlab = "Index", col= "darkgreen")

##############################################################################################################################################











##############################################################################
# Value Comparison
##############################################################################
# Comparison of the value of derivatives of A(x,y,z) vs B(x,y,z) on each axis.
##############################################################################################################################################
# Ratio Comparison
##############################################################################################################################################

par(mfrow=c(3,3))

# Find ratio of A'(x) to B(x)

Ratio_X_Axis <- Norm_dAx / Norm_Bx[1:length(Norm_dAx)]

# Plot the Ratio of A'(x) to B(x)

plot(Index_B[1:length(Norm_dAx)],Ratio_X_Axis, pch = 20, main = "Ratio of A'(x) to B(x)", ylab= "Ratio", xlab = "Index", col= "blue")
plot(Index_B[1:length(Norm_dAx)],Ratio_X_Axis, ylim=c(-1,20),pch = 20, main = "Ratio of A'(x) to B(x)", ylab= "Ratio", xlab = "Index", col= "blue")

#######################################################################
#######################################################################

# Find ratio of A'(y) to B(y)

Ratio_Y_Axis <- Norm_dAy / Norm_By[1:length(Norm_dAy)]

# Plot the Ratio of A'(y) to B(y)

plot(Index_B[1:length(Norm_dAy)],Ratio_Y_Axis,pch = 20, main = "Ratio of A'(y) to B(y)", ylab= "Ratio", xlab = "Index", col= "red")
plot(Index_B[1:length(Norm_dAy)],Ratio_Y_Axis,ylim=c(-1,20), pch = 20, main = "Ratio of A'(y) to B(y)", ylab= "Ratio", xlab = "Index", col= "red")

#######################################################################
#######################################################################

# Find ratio of A'(z) to B(z)

Ratio_Z_Axis <- Norm_dAz / Norm_Bz[1:length(Norm_dAz)]

# Plot the Ratio of A'(z) to B(z)
plot(Index_B[1:length(Norm_dAz)],Ratio_Z_Axis,pch = 20, main = "Ratio of A'(z) to B(z)", ylab= "Ratio", xlab = "Index", col= "darkgreen")
#plot(Index_B[1:length(Norm_dAz)],Ratio_Z_Axis,ylim=c(-1,20),pch = 20, main = "Ratio of A'(z) to B(z)", ylab= "Ratio", xlab = "Index", col= "darkgreen")

##############################################################################################################################################
##############################################################################################################################################
# Difference Comparison 
##############################################################################################################################################
# Find difference of A'(x) to B(x)

Difference_X_Axis <- Norm_dAx - Norm_Bx[1:length(Norm_dAx)]

# Plot the Difference of A'(x) to B(x)

plot(Index_B[1:length(Norm_dAx)],Difference_X_Axis, type = 'l', main = "Difference of A'(x) to B(x)", ylab= "Difference", xlab = "Index", col= "blue")


# Find difference of A'(y) to B(y)

Difference_Y_Axis <- Norm_dAy - Norm_By[1:length(Norm_dAy)]

# Plot the Difference of A'(y) to B(y)

plot(Index_B[1:length(Norm_dAy)],Difference_Y_Axis, type = 'l', main = "Difference of A'(y) to B(y)", ylab= "Difference", xlab = "Index", col= "red")

# Find difference of A'(z) to B(z)

Difference_Z_Axis <- Norm_dAz - Norm_Bz[1:length(Norm_dAz)]

# Plot the Difference of A'(z) to B(z)

plot(Index_B[1:length(Norm_dAz)],Difference_Z_Axis, type = 'l', main = "Difference of A'(z) to B(z)", ylab= "Difference", xlab = "Index", col= "darkgreen")

##############################################################################################################################################
##############################################################################################################################################











##############################################################################################################################################
#################   VECTOR COMPARISON  ######################################################################
##############################################################################################################################################

##############################################################################################################################################
####################################  MAGNITUDE OF Vector defined by REDUCED B(X,Y,Z) function  #########################################################################
##############################################################################################################################################
par(mfrow=c(2,2))

#######################################################################
# Create an array to store the Magnitude of reduced B(x,y,z) 
#######################################################################

Magnitude_Reduced_B  = rep(0,No_Reduced_B_points)

#######################################################################
# Find MAGNITUDE of points in reduced B(x,y,z)
#######################################################################

for(m in 1: No_Reduced_B_points)
{
  Magnitude_Reduced_B[m] = sqrt(reduced_Bx[m]*reduced_Bx[m] + reduced_By[m]*reduced_By[m] + reduced_Bz[m]*reduced_Bz[m])
}

#######################################################################
# Scatterplot of MAGINTUDE of reduced B(x,y,z)
#######################################################################

#plot(Index_B[1:length(Magnitude_Reduced_B)],Magnitude_Reduced_B, type = 'l', main = "Scatterplot : MAGINTUDE of B(x,y,z)", ylab= "Magnitude of B(x,y,z)", xlab = "Index", col= "red")

#######################################################################
# Normalize the Magnitude of reduced B(x,y,z)
#######################################################################

Min_MagB = min(Magnitude_Reduced_B)
Max_MagB = max(Magnitude_Reduced_B)

Norm_MagB <- ( Magnitude_Reduced_B  - Min_MagB ) / (Max_MagB - Min_MagB)

plot(Index_B[1:length(Norm_MagB)],Norm_MagB, type = 'l', main = "Scatterplot after Normalization : MAGINTUDE of B(x,y,z)", ylab= "Magnitude of B(x,y,z)", xlab = "Index", col= "red")

##############################################################################################################################################
####################################  MAGNITUDE OF Vector  of  derivatives A(x,y,z)  #########################################################################
##############################################################################################################################################


###########################################################################
# Create an array to store the Magnitude OF Vector of derivatives A(x,y,z) 
###########################################################################

Magnitude_Derv_A  = rep(0,length(dAx))

#######################################################################
# Find MAGNITUDE of points in derivatives A(x,y,z)
#######################################################################

for(mA in 1: length(dAx))
{
  Magnitude_Derv_A[mA] = sqrt(dAx[mA]*dAx[mA] + dAy[mA]*dAy[mA] + dAz[mA]*dAz[mA])
}

#######################################################################
# Scatterplot of MAGINTUDE of derivatives A(x,y,z)
#######################################################################

#plot(Index_A[1:length(Magnitude_Derv_A)],Magnitude_Derv_A, type = 'l', main = "Scatterplot : MAGINTUDE of Derivatives A(x,y,z)", ylab= "Magnitude of Derivatives A(x,y,z)", xlab = "Index", col= "blue")

#######################################################################
# Normalize the Magnitude of Derivative of A
#######################################################################

Min_MagA = min(Magnitude_Derv_A)
Max_MagA = max(Magnitude_Derv_A)

Norm_MagA <- ( Magnitude_Derv_A  - Min_MagA ) / (Max_MagA - Min_MagA)

plot(Index_A[1:length(Norm_MagA)],Norm_MagA, type = 'l', main = "Scatterplot after Normalization: MAGINTUDE of Derivatives A(x,y,z)", ylab= "Magnitude of Derivatives A(x,y,z)", xlab = "Index", col= "blue")

##############################################################################################################################################
# Find Ratio of Magnitude of vector of Derivative of A(x,y,z)  to Magnitude of vector of B(x,y,z)    
##############################################################################################################################################

#length(Norm_MagB)
#length(Magnitude_Reduced_B)

Ratio_Magnitude <- Norm_MagA / Norm_MagB[1:length(Norm_MagA)]

# Plot the Ratio 

plot(Index_B[1:length(Norm_MagA)],Ratio_Magnitude,pch=20, main = "Ratio : Magnitude of Derivative of A / Magnitude of B", ylab= "Ratio", xlab = "Index", col= "red")
#plot(Index_B[1:length(Norm_MagA)],Ratio_Magnitude,pch=20,ylim=c(-1,20), main = "Ratio : Magnitude of Derivative of A / Magnitude of B", ylab= "Ratio", xlab = "Index", col= "red")


##############################################################################################################################################
# Find Difference of Magnitude of vector of Derivative of A(x,y,z)  and Magnitude of vector of B(x,y,z)    
##############################################################################################################################################

Difference_Magnitude <- Norm_MagA - Norm_MagB[1:length(Norm_MagA)]


# Plot the Difference of A'(y) to B(y)

plot(Index_B[1:length(Norm_MagA)],Difference_Magnitude, type = 'l', main = "Difference : Magnitude of Derivative of A - Magnitude of B", ylab= "Difference", xlab = "Index", col= "red")
##############################################################################################################################################












##############################################################################################################################################
####################################  DIRECTION OF Vector of Derivatives A(x,y,z)  #########################################################################
##############################################################################################################################################
par(mfrow=c(2,3))

# Vector of Derivative of A(x,y,z)  : (dAx,dAy,dAz)

###########################################################################
# Create an array to store the Angle of derivatives A(x,y,z) w.r.t X-axis
###########################################################################

alpha_Derv_A  = rep(0,length(dAx))

#######################################################################
# Find  Angle of derivatives A(x,y,z) w.r.t X-axis
#######################################################################

for(dCosA in 1: length(dAx))
{
  alpha_Derv_A[dCosA] =  acos ( dAx[dCosA] / sqrt(dAx[dCosA]*dAx[dCosA] + dAy[dCosA]*dAy[dCosA] + dAz[dCosA]*dAz[dCosA]) )
}

#######################################################################
# Normalize Alpha of Derivative of A
#######################################################################

Min_alpha_Derv_A = min(alpha_Derv_A)
Max_alpha_Derv_A = max(alpha_Derv_A)

Norm_alpha_Derv_A <- ( alpha_Derv_A  - Min_alpha_Derv_A ) / (Max_alpha_Derv_A - Min_alpha_Derv_A)

#######################################################################
# Scatterplot of Angle of derivatives A(x,y,z) w.r.t X-axis
#######################################################################

#plot(Index_A[1:length(alpha_Derv_A)],alpha_Derv_A, type = 'l', main = "Angle between the vector of A'(x,y,z) and X-axis", ylab= "Angle w.r.t X-axis", xlab = "Index", col= "blue")

plot(Index_A[1:length(Norm_alpha_Derv_A)],Norm_alpha_Derv_A, type = 'l', main = "Angle between the vector of A'(x,y,z) and X-axis - After Normalization", ylab= "Angle w.r.t X-axis", xlab = "Index", col= "blue")

###########################################################################
# Create an array to store the Angle of derivatives A(x,y,z) w.r.t Y-axis
###########################################################################

beta_Derv_A  = rep(0,length(dAy))

#######################################################################
# Find  Angle of derivatives A(x,y,z) w.r.t Y-axis
#######################################################################

for(dCosB in 1: length(dAy))
{
  beta_Derv_A[dCosB] =  acos ( dAy[dCosB] / sqrt(dAx[dCosB]*dAx[dCosB] + dAy[dCosB]*dAy[dCosB] + dAz[dCosB]*dAz[dCosB]) )
}

#######################################################################
# Normalize Beta of Derivative of A
#######################################################################

Min_beta_Derv_A = min(beta_Derv_A)
Max_beta_Derv_A = max(beta_Derv_A)

Norm_beta_Derv_A <- ( beta_Derv_A  - Min_beta_Derv_A ) / (Max_beta_Derv_A - Min_beta_Derv_A)

#######################################################################
# Scatterplot of Angle of derivatives A(x,y,z) w.r.t Y-axis
#######################################################################

#plot(Index_A[1:length(beta_Derv_A)],beta_Derv_A, type = 'l', main = "Angle between the vector of A'(x,y,z) and Y-axis", ylab= "Angle w.r.t Y-axis", xlab = "Index", col= "red")

plot(Index_A[1:length(Norm_beta_Derv_A)],Norm_beta_Derv_A, type = 'l', main = "Angle between the vector of A'(x,y,z) and Y-axis - After Normalization", ylab= "Angle w.r.t Y-axis", xlab = "Index", col= "red")

###########################################################################
# Create an array to store the Angle of derivatives A(x,y,z) w.r.t Z-axis
###########################################################################

gamma_Derv_A  = rep(0,length(dAz))

#######################################################################
# Find Angle of derivatives A(x,y,z) w.r.t Z-axis
#######################################################################

for(dCosC in 1: length(dAz))
{
  gamma_Derv_A[dCosC] =  acos ( dAz[dCosC] / sqrt(dAx[dCosC]*dAx[dCosC] + dAy[dCosC]*dAy[dCosC] + dAz[dCosC]*dAz[dCosC]) )
}

#######################################################################
# Normalize Gamma of Derivative of A
#######################################################################

Min_gamma_Derv_A = min(gamma_Derv_A)
Max_gamma_Derv_A = max(gamma_Derv_A)

Norm_gamma_Derv_A <- ( gamma_Derv_A  - Min_gamma_Derv_A ) / (Max_gamma_Derv_A - Min_gamma_Derv_A)

#######################################################################
# Scatterplot of Angle of derivatives A(x,y,z) w.r.t Z-axis
#######################################################################

#plot(Index_A[1:length(gamma_Derv_A)],gamma_Derv_A, type = 'l', main = "Angle between the vector of A'(x,y,z) and Z-axis", ylab= "Angle w.r.t Z-axis", xlab = "Index", col= "darkgreen")

plot(Index_A[1:length(Norm_gamma_Derv_A)],Norm_gamma_Derv_A, type = 'l', main = "Angle between the vector of A'(x,y,z) and Z-axis - After Normalization", ylab= "Angle w.r.t Z-axis", xlab = "Index", col= "darkgreen")

##############################################################################################################################################
####################################  DIRECTION Angles OF Vector of B(x,y,z)  #########################################################################
##############################################################################################################################################

# Vector of B(x,y,z)  : (reduced_Bx,reduced_By,reduced_Bz)

###########################################################################
# Create an array to store the Angle of B(x,y,z) w.r.t X-axis
###########################################################################

alpha_B  = rep(0,length(reduced_Bx))

#######################################################################
# Find  Angle of B(x,y,z) w.r.t X-axis
#######################################################################

for(AngX in 1: length(reduced_Bx))
{
  alpha_B[AngX] =  acos ( reduced_Bx[AngX] / sqrt(reduced_Bx[AngX]*reduced_Bx[AngX] + reduced_By[AngX]*reduced_By[AngX] + reduced_Bz[AngX]*reduced_Bz[AngX]) )
}

#######################################################################
# Normalize Alpha of B
#######################################################################

Min_alpha_B = min(alpha_B)
Max_alpha_B = max(alpha_B)

Norm_alpha_B <- ( alpha_B  - Min_alpha_B ) / (Max_alpha_B - Min_alpha_B)

#######################################################################
# Scatterplot of Angle of  B(x,y,z) w.r.t X-axis
#######################################################################

#plot(Index_B[1:length(alpha_B)],alpha_B, type = 'l', main = "Angle between the vector of B(x,y,z) and X-axis", ylab= "Angle w.r.t X-axis", xlab = "Index", col= "blue")

plot(Index_B[1:length(Norm_alpha_B)],Norm_alpha_B, type = 'l', main = "Angle between the vector of B(x,y,z) and X-axis - After Normalization", ylab= "Angle w.r.t X-axis", xlab = "Index", col= "blue")

###########################################################################
# Create an array to store the Angle of B(x,y,z) w.r.t Y-axis
###########################################################################

beta_B  = rep(0,length(reduced_By))

#######################################################################
# Find  Angle of  B(x,y,z) w.r.t Y-axis
#######################################################################

for(AngY in 1: length(reduced_By))
{
  beta_B[AngY] =  acos ( reduced_By[AngY] / sqrt(reduced_Bx[AngY]*reduced_Bx[AngY] + reduced_By[AngY]*reduced_By[AngY] + reduced_Bz[AngY]*reduced_Bz[AngY]) )
}

#######################################################################
# Normalize Beta of B
#######################################################################

Min_beta_B = min(beta_B)
Max_beta_B = max(beta_B)

Norm_beta_B <- ( beta_B  - Min_beta_B ) / (Max_beta_B - Min_beta_B)

#######################################################################
# Scatterplot of Angle of  B(x,y,z) w.r.t Y-axis
#######################################################################

#plot(Index_B[1:length(beta_B)],beta_B, type = 'l', main = "Angle between the vector of B(x,y,z) and Y-axis", ylab= "Angle w.r.t Y-axis", xlab = "Index", col= "red")

plot(Index_B[1:length(Norm_beta_B)],Norm_beta_B, type = 'l', main = "Angle between the vector of B(x,y,z) and Y-axis - After Normalization", ylab= "Angle w.r.t Y-axis", xlab = "Index", col= "red")

###########################################################################
# Create an array to store the Angle of  B(x,y,z) w.r.t Z-axis
###########################################################################

gamma_B  = rep(0,length(reduced_Bz))

#######################################################################
# Find Angle of  B(x,y,z) w.r.t Z-axis
#######################################################################

for(AngZ in 1: length(reduced_Bz))
{
  gamma_B[AngZ] =  acos ( reduced_Bz[AngZ] / sqrt(reduced_Bx[AngZ]*reduced_Bx[AngZ] + reduced_By[AngZ]*reduced_By[AngZ] + reduced_Bz[AngZ]*reduced_Bz[AngZ]) )
}

#######################################################################
# Normalize Gamma of B
#######################################################################

Min_gamma_B = min(gamma_B)
Max_gamma_B = max(gamma_B)

Norm_gamma_B <- ( gamma_B  - Min_gamma_B ) / (Max_gamma_B - Min_gamma_B)

#######################################################################
# Scatterplot of Angle of  B(x,y,z) w.r.t Z-axis
#######################################################################

#plot(Index_B[1:length(gamma_B)],gamma_B, type = 'l', main = "Angle between the vector of B(x,y,z) and Z-axis", ylab= "Angle w.r.t Z-axis", xlab = "Index", col= "darkgreen")

plot(Index_B[1:length(Norm_gamma_B)],Norm_gamma_B, type = 'l', main = "Angle between the vector of B(x,y,z) and Z-axis - After Normalization", ylab= "Angle w.r.t Z-axis", xlab = "Index", col= "darkgreen")













##############################################################################################################################################
####################################  Comparison of DIRECTION ANGLES  #########################################################################
##############################################################################################################################################
par(mfrow=c(3,2))

# A Vector Angles (alpha_Derv_A,beta_Derv_A,gamma_Derv_A)
# B Vector Angles (alpha_B,beta_B,gamma_B)

# A Vector Angles ( Norm_alpha_Derv_A,Norm_beta_Derv_A,Norm_gamma_Derv_A )
# B Vector Angles ( Norm_alpha_B,Norm_beta_B,Norm_gamma_B )


##############################################################################################################################################
# Ratio : X-axis
##############################################################################################################################################

Ratio_alpha <- Norm_alpha_Derv_A / Norm_alpha_B[1:length(Norm_alpha_Derv_A)]

# Plot the Ratio 

plot(Index_B[1:length(Ratio_alpha)],Ratio_alpha,pch=20, main = "Ratio : X-Axis Angles", ylab= "Ratio", xlab = "Index", col= "blue")


##############################################################################################################################################
# Difference : X-axis
##############################################################################################################################################

Difference_alpha <- Norm_alpha_Derv_A - Norm_alpha_B[1:length(Norm_alpha_Derv_A)]


# Plot the Difference 

plot(Index_B[1:length(Difference_alpha)],Difference_alpha, type = 'l', main = "Difference : X-Axis Angles", ylab= "Difference", xlab = "Index", col= "blue")


##############################################################################################################################################
# Ratio : Y-axis
##############################################################################################################################################

Ratio_beta <- Norm_beta_Derv_A / Norm_beta_B[1:length(Norm_beta_Derv_A)]

# Plot the Ratio 

plot(Index_B[1:length(Ratio_beta)],Ratio_beta,pch=20, main = "Ratio : Y-Axis Angles", ylab= "Ratio", xlab = "Index", col= "red")

##############################################################################################################################################
# Difference : Y-axis
##############################################################################################################################################

Difference_beta <- Norm_beta_Derv_A - Norm_beta_B[1:length(Norm_beta_Derv_A)]


# Plot the Difference 

plot(Index_B[1:length(Difference_beta)],Difference_beta, type = 'l', main = "Difference : Y-Axis Angles", ylab= "Difference", xlab = "Index", col= "red")


##############################################################################################################################################
# Ratio : Z-axis
##############################################################################################################################################

Ratio_gamma <- Norm_gamma_Derv_A / Norm_gamma_B[1:length(Norm_gamma_Derv_A)]

# Plot the Ratio 

plot(Index_B[1:length(Ratio_gamma)],Ratio_gamma,pch=20, main = "Ratio : Z-Axis Angles", ylab= "Ratio", xlab = "Index", col= "darkgreen")

##############################################################################################################################################
# Difference : Z-axis
##############################################################################################################################################

Difference_gamma <- Norm_gamma_Derv_A - Norm_gamma_B[1:length(Norm_gamma_Derv_A)]


# Plot the Difference 

plot(Index_B[1:length(Difference_gamma)],Difference_gamma, type = 'l', main = "Difference : Z-Axis Angles", ylab= "Difference", xlab = "Index", col= "darkgreen")


