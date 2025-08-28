clear all 
close all
I = imread("Fotos15abril\AM90ppm2.jpg");
imshow(I)
I2 = MascaraAzul2(I);
I2 = imclearborder(I2);
I2 = bwareaopen(I2, 30000);
I_blue = I(:,:,3);
stats = regionprops(I2, I_blue, 'MeanIntensity');
intensidades = cat(1, stats.MeanIntensity)
imshow(I2)
