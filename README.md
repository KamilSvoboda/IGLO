#IGLO Images Grid LayOut  
Easy way to print multiple photos on single paper

Copyright (C) 2005-2015 Kamil Svoboda <email: kamil (dot) svoboda (at sign) centrum (dot) cz
http://iglo.svoboda.biz/

This program is distributed under Apache License 2.0

##DESCRIPTION 
Script merges multiple images to another one with grid layout. 
You can use it for printing multiple photos on single paper, creating posters, wallpaper, CD/DVD covers, etc.
Script use as many as possible opened images according defined grid cell count.
Size of grid cell, count of grid cell, count of rows and columns are calculated automatically, or 
defined by user settings.

FOR MORE DETAILS VISIT [http://iglo.svoboda.biz](http://iglo.svoboda.biz)

##CHANGE-LOG:

###1.8 2016/02/22
* fixed issue #7 - exception occures when repeating is setted

###1.7 2015/03/17
* fixed issue with global functions

###1.6 2015/02/09
* new option - image placement strategy
	* Snap to paper edge
    * Center in the grid cell

###1.5 2015/02/08
* new option - paper margin (size of empty space around paper)
* fixed order of source images in final image

###1.4 2014/06/02
* New size units - inches

###1.3 2013/01/27
* new predefined paper sizes
* DPI max value is 10000 now
* Paper max size is 10000 mm now

###1.2 2009/07/07
* enhanced image placement - images on right side of the paper are aligned to the right and images on bottom side of the paper are aligned to down

###1.1 2009/01/10
* bugfix (support for Gimp 2.6) 

###1.0 2008/03/22
* new features: 
	* duplicate image(s) x times
	* fill empty cells with last image

###0.9 2008/03/21
* bugfix (incorrect behavior when image wasn't opened)

###0.8 2008/03/20
* support for Gimp 2.4.x

###0.7 2006/03/15
* bugfixes 

###0.6 2006/03/14
* bugfixes

###0.5 2006/03/3
* size of paper and image place can be set in pixels or millimetres

###0.4 2006/02/28
* set exact size of images on the paper - it collaborates with count of rows and columns
* new interface for input parameters 

###0.3 2006/02/25
* set exactly size of paper
* foreground color for empty image place

###0.2  2006/02/23
* select or set paper size
* set exactly count of rows and columns of images on the paper
* let script calculate count of rows and columns from opened images count 
* auto-rotate feature
* set DPI of the resulting image
* set method of interpolation and supersample
* flatten resulting layers - for immediate printing

###0.1 - initial release 2005/05/24
* specify number of images on paper (size of grid cell is calculated)
* set distance between grid cells (image places)
* select backgroud color (color of space between grid cells)
