; IGLO Images Grid LayOut  
; Easy way to print multiple photos on single paper
; 
; Copyright (C) 2005-2013 Kamil Svoboda <email: kamil (dot) svoboda (at sign) centrum (dot) cz
; http://iglo.svoboda.biz/

; This program is free software; you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.  
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
; 
; DESCRIPTION 
; Script merges multiple images to another one with grid layout. 
; You can use it for printing multiple photos on single paper, creating posters, wallpaper, CD/DVD covers, etc.
; Script use as many as possible opened images according defined grid cell count.
; Size of grid cell, count of grid cell, count of rows and columns are calculated automatically, or 
; defined by user settings.
; 
; FOR MORE DETAILS VISIT http://iglo.svoboda.biz
;;
; CHANGE-LOG:
; 1.4 2014/06/02
; New size units - inches
; 
; 1.3 2013/01/27
; new predefined paper sizes
; DPI max value is 10000 now
; Paper max size is 10000 mm now
;
; 1.2 2009/07/07
; enhanced image placement - images on right side of the paper are aligned to the right and images on bottom side of the paper are aligned to down
;
; 1.1 2009/01/10
; bugfix (support for Gimp 2.6) 
;
; 1.0 2008/03/22
; new features: 
; 	- duplicate image(s) x times
;	- fill empty cells with last image
;
; 0.9 2008/03/21
; bugfix (incorrect behavior when image wasn't opened)
;
; 0.8 2008/03/20
; support for Gimp 2.4.x
;
; 0.7 2006/03/15
; bugfixes 
;
; 0.6 2006/03/14
; bugfixes
;
; 0.5 2006/03/3
; size of paper and image place can be set in pixels or millimetres
;
; 0.4 2006/02/28
; set exact size of images on the paper - it collaborates with count of rows and columns
; new interface for input parameters 
;
; 0.3 2006/02/25
; set exactly size of paper
; foreground color for empty image place
;
; 0.2  2006/02/23
; select or set paper size
; set exactly count of rows and columns of images on the paper
; let script calculate count of rows and columns from opened images count 
; auto-rotate feature
; set DPI of the resulting image
; set method of interpolation and supersample
; flatten resulting layers - for immediate printing
;
; 0.1 - initial release 2005/05/24
; specify number of images on paper (size of grid cell is calculated)
; set distance between grid cells (image places)
; select backgroud color (color of space between grid cells)
; 
; KNOWN-ISSUES
; - select size by scroll bar set wrong number (GTK bug)
;
; TODO:
; - 110% method for image scaling
; - intelligent cutting of images by grid cell size (select the feature from combobox)
; - process all images from selected directory
 
;CONSTANTS
(define *scale_interpolation*
  '(("None"        0)
    ("Linear"          1)
    ("Cubic"   2)  
    )
)
   
(define *paper_size*
  '(("A4 - 210x297 mm" 210 297)
    ("A5 - 148x210 mm" 148 210)
    ("A6 - 105x148 mm" 105 148)
    ("A3 - 297x420 mm" 297 420)
	("A2 - 420x594 mm" 420 594)
	("A1 - 594x841 mm" 594 841)
	("A0 - 841x1189 mm" 841 1189)
    ("B6 - 128x182 mm" 128 182) 
    ("B5 - 182x257 mm" 182 257)
	("B4 - 257x364 mm" 257 364)
	("B3 - 364x515 mm" 364 515)
	("B2 - 515x728 mm" 515 728)
	("B1 - 728x1030 mm" 728 1030)
	("B0 - 1030x1456 mm" 1030 1456)
    ("US 4 x 6 inches" 102 152)
    ("US 5 x 7 inches" 127 178)
    ("US Letter 8.5 x 11 inches" 215.90 279.40)
    ("US Legal 8.5 x 14 inches" 215.90 355.60)
	("US Junior Legal 8 x 5 inches" 203.2 127)
	("US Ledger 17 x 11 inches" 432 279)
	("US Tabloid 11 x 17 inches" 279 432)
    ("CD cover - 120,99x119,97 mm" 120.99 119.97)  
    )
)
    
;FUNCTIONS
(define (round num) ;returns rounded number
	(let* (
        (result 0)
        (absolut (trunc num)) ;absolut digites of the real
        (decimal (- num absolut)) ;decimal digites of the real
        )
        (if (> decimal 0)
			(if (< decimal 0.5)
				(set! result absolut)
				(set! result (+ absolut 1))
			)
			(set! result absolut)
        )        
	)
)

(define (div a b) ;returns 
	(trunc(/ a b))
)

(define (less a b) ;returns less number
	(let* ((return 0))
	    (if (< a b)
			(set! return a)
			(set! return b)
	    )
	)  
)

(define (scale img draw new_width new_height)
	(let* (
		(width)
		(height)
	)
		(gimp-image-undo-group-start img)
		(gimp-image-flatten img)
		; first pass
		(set! width  (/ (car (gimp-image-width  img)) 2))
		(set! height (/ (car (gimp-image-height img)) 2))
		(gimp-selection-sharpen img)
		(gimp-image-scale img width height)		
		; second pass
		(set! width  (/ (car (gimp-image-width  img)) 2))
		(set! height (/ (car (gimp-image-height img)) 2))
		(gimp-selection-sharpen img)
		(gimp-image-scale img width height)
		(set! draw (car (gimp-image-get-active-drawable img)))
		; unsharp
		(plug-in-unsharp-mask 1 img draw 0.3 0.8 0)		
		(gimp-image-undo-group-end img)
		(gimp-displays-flush)
	)
)

(define (script-fu-images-grid-layout size paperWidth paperHeight imagePlaceWidth imagePlaceHeight units DPI duplicate row_nb col_nb space fill_empty rotate chg_ratio interpolation superSample flatten fg_color bg_color)
	(let*
		(	;global variables
		(img (gimp-image-list))		;list of opened images
		(img_nb (car img))   		;how many images are open
		(images (cadr img))   		;the array of images
		(paper 0)						;new image
		(bg 0)						;background image
		)		
		;(gimp-message (string-append "img_idx: "(number->string (- (/ img_nb duplicate) 1))))	; example of message construction (for development)	
		
		(set! img_nb (* img_nb duplicate)) ;set number of images as multiple of parameter "duplicate"

		(if (and (= paperWidth 0) (= paperHeight 0))
			(begin
				(set! paperWidth  (cadr (nth size *paper_size*))) ;pick up paper width from list of sizes
				(set! paperHeight  (caddr (nth size *paper_size*))) ;pick up paper height from list of sizes    	
				(set! paperWidth (round(*  (/ paperWidth 25.4) DPI))) ;calculate count of pixels by DPI
				(set! paperHeight (round(* (/ paperHeight 25.4) DPI))) ;calculate count of pixels by DPI
			)
			(begin     ;check if both width and height are set
				(if (= paperWidth 0)
					(set! paperWidth 1)
				)
				(if (= paperHeight 0)
					(set! paperHeight 1)  
				)    
				(if (= units 0)
					(begin
						(set! paperWidth (round(*  (/ paperWidth 25.4) DPI))) ;calculate count of pixels by DPI
						(set! paperHeight (round(* (/ paperHeight 25.4) DPI))) ;calculate count of pixels by DPI
					)      
				)
				(if (= units 1)
					(begin
						(set! paperWidth (round(* (/ paperWidth 16) DPI))) ;calculate count of pixels by DPI
						(set! paperHeight (round(* (/ paperHeight 16) DPI))) ;calculate count of pixels by DPI
					)      
				)
			)
		)
		
		(set! space (round(* (/ space 25.4) DPI))) ;calculate space between images in pixels by DPI

		;CALCULATING COUNT OF ROWS AND COLUMNS
		(if (and (not(= imagePlaceWidth 0)) (= imagePlaceHeight 0)) ;if user specify only image place width
			(begin
		      (if (= units 0)(set! imagePlaceWidth (round(*  (/ imagePlaceWidth 25.4) DPI)))) ;calculate count of pixels by DPI for image place width
			  (if (= units 1)(set! imagePlaceWidth (round(*  (/ imagePlaceWidth 16) DPI)))) ;calculate count of pixels by DPI for image place width
		      (if (> col_nb 0) ;if user set a count of columns and imagePlaceWidth check if setted count of columns is less, then calculated from imagePlaceWidth 
		        (set! col_nb (less col_nb (div (+ paperWidth space) (+ imagePlaceWidth space)))) ;set less count of columns - setted or calculated
		        (set! col_nb (div (+ paperWidth space) (+ imagePlaceWidth space))) ;calculate count of columns (paper size must be enhanced by "space" - for correct division)
		      )      
			  (if (= row_nb 0) ;if user didn't specify count of rows on paper
				(begin            
				  (set! row_nb 1) ;set count of rows to 1
				  (set! imagePlaceHeight paperHeight) ;set height of image place same as paper height
				)
				(set! imagePlaceHeight (div (- paperHeight (* space (- row_nb 1))) row_nb))  	;let rows count as user decide and calculate imagePlaceHeight from defined count of rows
			  )
		    )
		    (if (and (= imagePlaceWidth 0) (not(= imagePlaceHeight 0))) ;if user specify only image place height
			    (begin
			        (if (= units 0)(set! imagePlaceHeight (round(*  (/ imagePlaceHeight 25.4) DPI)))) ;calculate count of pixels by DPI for image place height
					(if (= units 1)(set! imagePlaceHeight (round(*  (/ imagePlaceHeight 16) DPI)))) ;calculate count of pixels by DPI for image place height
			        (if (> row_nb 0) ;if user set a count of rows and imagePlaceHeight check if setted count of rows is less, then calculated from imagePlaceHeight
			          (set! row_nb (less row_nb (div (+ paperHeight space) (+ imagePlaceHeight space)))) ;set less count of rows (setted or calculated)
			          (set! row_nb (div (+ paperHeight space) (+ imagePlaceHeight space))) ;calculate count of rows (paper size must be enhanced by "space" - for correct division)
			        )        
			        (if (= col_nb 0) ;if user didn't specify count of columns on paper
			          (begin                              
			            (set! col_nb 1) ;set count of columns to 1
			            (set! imagePlaceWidth paperWidth) ;set width of image place same as paper width
			          )
			          (set! imagePlaceWidth (div (- paperWidth (* space (- col_nb 1))) col_nb)) 	;let columns count as user decide and calculate imagePlaceWidth from defined count of columns
			        )                    
			    )      
			    (if (and (not(= imagePlaceWidth 0)) (not(= imagePlaceHeight 0)))   ;if user specify both image place width and height
			        (begin   
			            (if (= units 0)
							(begin
								(set! imagePlaceWidth (round(*  (/ imagePlaceWidth 25.4) DPI))) ;calculate count of pixels by DPI
								(set! imagePlaceHeight (round(*  (/ imagePlaceHeight 25.4) DPI))) ;calculate count of pixels by DPI
							)
			            )
						(if (= units 1)
							(begin
								(set! imagePlaceWidth (round(*  (/ imagePlaceWidth 16) DPI))) ;calculate count of pixels by DPI
								(set! imagePlaceHeight (round(*  (/ imagePlaceHeight 16) DPI))) ;calculate count of pixels by DPI
							)
			            )
						(let*(
			                 (portraitColumns (div (+ paperWidth space) (+ imagePlaceWidth space))) ;calculate count of columns in portrait orientation (paper size must be enhanced by "space" - for correct division)
			                 (portraitRows (div (+ paperHeight space) (+ imagePlaceHeight space))) ;calculate count of rows in portrait orientation -""-                                   
			                 (landscapeColumns (div (+ paperWidth space) (+ imagePlaceHeight space))) ;calculate count of columns in landscape orientation -""-
			                 (landscapeRows (div (+ paperHeight space) (+ imagePlaceWidth space))) ; calculate count of rows in landscape orientation -""-
			                )
			                (if (>= (* portraitColumns portraitRows) (* landscapeColumns landscapeRows)) ;if more images will be in portrait orientation
			                    (begin
									(set! col_nb portraitColumns)
									(set! row_nb portraitRows)            
			                    )                                                                        ;if more images will be in landscape orientation
			                    (begin
									(let* ((pom imagePlaceWidth))             ;exchange imagePlaceWidth and imagePlaceHeight
										(set! imagePlaceWidth imagePlaceHeight)
										(set! imagePlaceHeight pom)
									)
									(set! col_nb landscapeColumns)
									(set! row_nb landscapeRows)                                 
			                    )
			                )                         
						)        
			        )        
			        (begin ;if imagePlaceWidth and imagePlaceHeigh are not defined calculate count of rows and columns fromm image count
			        	(if (and (= col_nb 0) (= row_nb 0)) ;calculate count of columns and rows from image count
							(begin								
								(let*(
			            	        (squareRoot (sqrt img_nb)) ;square root of image count
			            	        (truncated (trunc squareRoot)) ;truncated squareRoot
			            	        (decimal (- squareRoot truncated)) ;decimal digites of the square root
									)
									(if (and (= decimal 0) (> img_nb 0)) ;set cols and rows by squareRoot
										(begin
											(set! col_nb squareRoot) 			;count of columns
											(set! row_nb squareRoot) 			;count of rows       
										)
										(if (and (< decimal 0.5) (> img_nb 0));set cols by truncated squareRoot and rows with increased
											(begin
												(set! col_nb truncated) 			
												(set! row_nb (+ truncated 1)) 		   
											)
											(begin												
												(set! col_nb (+ truncated 1)) ;else increase rows and cols			
				                		        (set! row_nb (+ truncated 1))
											)    
										)
									)	   
								)
							)
				            (if (and (not(= col_nb 0)) (= row_nb 0)) ;calculate row count for user defined columns      
				              (if (>= col_nb img_nb) ;if there is count of images less then count of columns 
				                (set! row_nb 1) ;set count of rows to 1
				                (if (= (fmod img_nb col_nb) 0) ;calculate count of rows - if dividing remainder is greater zero
				                  (set! row_nb (div img_nb col_nb)) ;set dividing quotient as rows count
				                  (set! row_nb (+ (div img_nb col_nb) 1)) ;increase dividing quotient
				                )
				              )      
				              (if (and (= col_nb 0) (not(= row_nb 0))) ;calculate columns count for user defined rows
				                (if (>= row_nb img_nb) ;if there is count of images less then count of rows
				                  (set! col_nb 1) ;set count of columns to 1
				                  (if (= (fmod img_nb row_nb) 0) ;if dividing remainder is greater zero
				                    (set! col_nb (div img_nb row_nb)) ;set dividing quotient as columns count
				                    (set! col_nb (+ (div img_nb row_nb) 1)) ;increase dividing quotient
				                  )
				                )
				              ) 
				            )
						)    
			        	(set! imagePlaceWidth (div (- paperWidth (* space (- col_nb 1))) col_nb)) 	;calculate imagePlaceWidth for defined count of columns
			        	(set! imagePlaceHeight (div (- paperHeight (* space (- row_nb 1))) row_nb))  	;calculate imagePlaceHeight for defined count of rows    
			        )          
			    )      
		    )
		)        

		(gimp-context-set-foreground fg_color)		;set foreground by user selection	
		(gimp-context-set-background bg_color)		;set backgroud by user selection	  	
		
		(set! paper (car (gimp-image-new paperWidth paperHeight RGB)))	; create new image
		(gimp-image-set-resolution paper DPI DPI) ;set DPI
		(gimp-image-clean-all paper)			;clean new image	
		
		(set! bg (car (gimp-layer-new paper paperWidth paperHeight RGB-IMAGE "Background" 100 NORMAL-MODE))) ;create background layer
		(gimp-image-add-layer paper bg 0)  		;add background layer to the image
		(gimp-edit-fill bg BACKGROUND-FILL)		
		;(gimp-display-new paper)
		(let(	  	
			(img_idx 0)             ;index of image in Gimp image list
			(duplicating_count 0)		;counter for duplicating images
			(row_cur 0)				;current row on the paper	
			(col_cur 0)				;current column on the paper
			(this_img 0)				;opened image
			)
					
			(while (< row_cur row_nb)       ;iterate rows
				(set! col_cur 0)			;current column on the paper
				(while (< col_cur col_nb)		;iterate columns			
					(let*(
						(x_offset (* col_cur imagePlaceWidth))	;offset of imagePlace layer in the image (paper)
						(y_offset (* row_cur imagePlaceHeight))
						(layer (car (gimp-layer-new paper imagePlaceWidth imagePlaceHeight RGB-IMAGE "Empty image place" 100 NORMAL-MODE)))
						)	
		    					
						(gimp-image-add-layer paper layer (+ (* img_idx duplicate) duplicating_count))  	;add new layer - "(img_idx * duplicate) + duplicating_count" is index of current image cell
			    		(gimp-edit-clear layer)				;clear new layer
						
			  			(if (or (< (+ (* img_idx duplicate) duplicating_count) img_nb) ;"(img_idx * duplicate) + duplicating_count" is index of current image cell																					
								(and (= fill_empty TRUE)(> img_nb 0)))	; there is any image opened and last image should be duplicated  
			  				(begin											
								(if (>= (+ (* img_idx duplicate) duplicating_count) img_nb) ;set img_idx back to last opened image when we process empty cells  ( fill_empty is set TRUE)								
									(set! img_idx (- (/ img_nb duplicate) 1))
								)									
				  				(set! this_img (aref images img_idx)) 	;get opened image
				  				(gimp-drawable-set-name layer (car (gimp-image-get-name this_img))) ;set layer name same as image name				  
								;(if (not (= (car (gimp-image-base-type this_img)) 0))	(gimp-convert-rgb this_img)) ; make sure image is in RGB land 
				  				(gimp-selection-all this_img)		;select all in the image
				  				(if (car (gimp-edit-copy-visible this_img))	;copy selected area to the buffer  											
				  						(let* 	(
				  							(sel (car (gimp-edit-paste layer TRUE)))	;paste copied image to the layer	
				  							(height (car (gimp-drawable-height sel)))   	;height of original image
				  							(width  (car (gimp-drawable-width sel)))    	;width of original image
				  							)					
						  					(if (= rotate TRUE)
								                (if (or (and (> (/ height width) 1) (< (/ imagePlaceHeight imagePlaceWidth) 1)) (and (< (/ height width) 1) (> (/ imagePlaceHeight imagePlaceWidth) 1)))
													(begin
														(gimp-drawable-transform-rotate-simple sel 0 1 0 0 0) ;the rotation with radians is not exact - simple rotation is used
														(let* ((mem height)) ;exchange width and height
														(set! height width)
														(set! width mem)
														)
													)  				                
												)
											)
											(if (= chg_ratio TRUE)
												;if allow change aspect ratio of the image							
												(gimp-drawable-transform-scale sel 0 0 imagePlaceWidth imagePlaceHeight 0 interpolation superSample 3 0)
												;if preserve aspect ration of the image
												(if (> (/ height width) (/ imagePlaceHeight imagePlaceWidth) ) ;;deside scale in height or width
													(gimp-drawable-transform-scale sel 0 0 (* width (/ imagePlaceHeight height)) imagePlaceHeight 0 interpolation superSample 3 0)
													(gimp-drawable-transform-scale sel 0 0 imagePlaceWidth (* height (/ imagePlaceWidth width)) 0 interpolation superSample 3 0)
												)						
											)						
				   							
											;calculate position of image in the imagePlace (in the layer)
				   							(let* (
												(pos_x 0)
												(pos_y 0)
												)
												(if (= (+ row_cur 1) row_nb) ;for  last row align down
													(set! pos_y (- imagePlaceHeight (car (gimp-drawable-height sel))))
												)
												(if (= (+ col_cur 1) col_nb) ;for  last column align right
													(set! pos_x (- imagePlaceWidth (car (gimp-drawable-width sel))))
												)												
												(gimp-layer-set-offsets sel pos_x pos_y)	
											)	
											(gimp-floating-sel-anchor sel)		;anchor floating image layer to the imagePlace layer 											
				  						)			  					
				  				)
				  				(gimp-selection-none this_img)
								
								(set! duplicating_count (+ duplicating_count 1))	;increase counter for duplicating images
								
								(if (>= duplicating_count duplicate)		;if duplicating is equel to "duplicate" increase image index
									(begin
										(set! img_idx (+ img_idx 1))
										(set! duplicating_count 0)
									)
								)																
			  				)
			  				(gimp-edit-fill layer FOREGROUND-FILL) ;if there is not any image for the imagePlace fill it by selected color
			  			)
						;calculate place for image layer
						(set! x_offset (+ x_offset (* space col_cur))) ;enhance x_offset by space
						(set! y_offset (+ y_offset (* space row_cur))) ;enhance y_offset by space
						
			  			(gimp-layer-set-offsets layer x_offset y_offset) ;place image layer on the paper			
					)
					(set! col_cur (+ col_cur 1))				
				)
				(set! row_cur (+ row_cur 1))			
			)
		)
		(if (= flatten TRUE) (gimp-image-flatten paper)) ;flatten all visible layers	
	    (gimp-display-new paper)
		;(gimp-image-undo-enable paper)	
	)
)

(script-fu-register
    "script-fu-images-grid-layout"
    "<Image>/Tools/Images Grid Layout"
    "Script merges multiple images to another one with grid layout"
    "Kamil Svoboda"
    "2005, Kamil Svoboda"
    "20th March 2005"
    "RGB* GRAY* INDEXED*"
    SF-OPTION "Paper _size" (mapcar car *paper_size*)
    SF-ADJUSTMENT "Paper _width (0 = from combobox)" '(0 0 10000 1 10 0 0)
    SF-ADJUSTMENT "Paper _heigh (0 = from combobox)" '(0 0 10000 1 10 0 0)
    SF-ADJUSTMENT "Image place w_idth (0 = auto)" '(0 0 10000 1 10 0 0)
    SF-ADJUSTMENT "Image place h_eight (0 = auto)" '(0 0 10000 1 10 0 0)
    SF-OPTION "Size _units" '(_"millimeter" _"1/16 inch" _"pixel")
    SF-ADJUSTMENT "_DPI of new image" '(300 1 10000 1 10 0 0)
	SF-ADJUSTMENT "Duplicate image(s) x-times" '(1 1 100 1 10 0 0)	
    SF-ADJUSTMENT "Count of _rows (0 = auto)" '(0 0 100 1 10 0 0)
    SF-ADJUSTMENT "Count of _columns (0 = auto)" '(0 0 100 1 10 0 0)
    SF-ADJUSTMENT "_Minimal space between images (mm)" '(0 0 5 1 10 1 0)   
	SF-TOGGLE "Fill empty cells with last image" TRUE
    SF-TOGGLE "Auto ro_tate images" TRUE
    SF-TOGGLE "Allow change image _aspect ratio" FALSE
    SF-OPTION "_Interpolation" (mapcar car *scale_interpolation*)
    SF-TOGGLE "Supersample" TRUE
    SF-TOGGLE "Flatten _visible layers" TRUE
    SF-COLOR "_Foreground (empty image place color)"	'(255 255 255)
    SF-COLOR "_Background (around images)"	'(255 255 255)
    ;SF-DIRNAME "Process all images from directory"   "Select directory"    
    ;script-fu-images-grid-layout 1 0 0 0 0 1 300 1 TRUE 0 0 0.5 TRUE FALSE 1 1 TRUE '(255 255 255) '(255 255 255))
)