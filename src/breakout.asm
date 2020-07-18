; **********************************************************************
; Name: Breakout game 
; Description: This is the incredible breakout game 
; Author: Or Golan 
; Date: 19/May/2018
; School: Ehad-Haham Petah-Tikva 
; **********************************************************************

IDEAL
MODEL small
STACK 200h

; **********************************************************************
; *                           Definition (Constants)                   *
; **********************************************************************

;// 1. Game definitions 
    
    ;//Game speed
	GAME_SPEED              equ 26000 
 
    ;//Screen size 
	SCREEN_WIDTH			equ 320		;screenwidth
	SCREEN_HEIGHT			equ 200		;screenheight

;   //Colors 
	; 0 - BLACK    4 - RED	      8 - DARKGRAY    C - LIGHTRED
	; 1 - BLUE     5 - MAGENTA    9 - LIGHTBLUE   D - LIGHTMAGENTA
	; 2 - GREEN    6 - BROWN	  A - LIGHTGREEN  E - YELLOW
	; 3 - CYAN     7 - LIGHTGRAY  B - LIGHTCYAN   F - WHITE
	BLACK			equ 0 
	BLUE			equ 1
	GREEN			equ 2
	CYAN			equ 3
	RED			    equ 4
	MAGENTA			equ 5
	YELLOW			equ 14
	WHITE			equ 15
	LIGHTRED        equ 0Ch  
	LIGHTMAGENTA    equ 0Dh  
     
;// 2. Bat definitions 

    ;//Initial position 
	BAT_INIT_POS_X	equ 120
	BAT_INIT_POS_Y	equ 190

	;//Bat speed 
	BAT_SPEED      equ  8
	
;// 3. Keyboard definitions 
    KEY_LEFT	equ	'a'
    KEY_RIGHT	equ	'd'

;// 4. Balls definitions 
	NUM_OF_BRICKS  equ  66        
	BALL_INIT_POS_X equ 160
	BALL_INIT_POS_Y equ 102
	
;// Timer definitions  
	Clock         equ es:6Ch  
	
; **********************************************************************
; *                           Variables                                *
; **********************************************************************
DATASEG
	
    ; /*** Bat varaibles ***/
	Bat_x      dw 120 
    Bat_y      dw 300 
	Bat_Length dw 40 
    Bat_Color  db 0 
    Temp_X     dw ?  	

    ; /*** Ball varaibles ***/
	Ball_x     dw 30
	Ball_y     dw 30	
    Ball_Color db 3 	
	Ball_X_Dir db 'r'
	Ball_y_Dir db 'd'    
	BallSlope  dw 1   				  

    ; /*** Bricks variables ***/
	Pixel_x     dw ?
	Pixel_y     dw ?	
    Pixel_Color db ?  

	;/*** Box variables      ***/
	Box_x       dw 0 
 	Box_y       dw 30
	Box_Color   db 2
	BoxSizeY    dw 10 
	BoxSizeX    dw 20
	Tmp_X       dw ? 
	Tmp_Y       dw ? 
	
	;/*** Score & Lives      ***/
	Score         dw 0   
	LivesCount    dw 3
	
	;/*** Bricks             ***/
	BricksSize_X  dw 20 	    
	BricksSize_Y  dw 10 	    
	BricksCount   dw NUM_OF_BRICKS
	BricksAlive   dw NUM_OF_BRICKS
	
	; Bricks array 
	; For each brick we save {Y,X,Color,Alive - "1"/Alive , "0" - Gone}
	bricks_Arr_YX dw 15,0,RED,1,   15,30,RED,1,      15,60,RED,1,     15,90,RED,1,     15,120,RED,1,        15,150,RED,1,      15,180,RED,1     ,15,210,RED,1     ,15,240,RED,1     ,15,270,RED,1    ,15,300,RED,1   
	              dw 30,0,BLUE,1,   30,30,BLUE,1,     30,60,BLUE,1,    30,90,BLUE,1,    30,120,BLUE,1,       30,150,BLUE,1,     30,180,BLUE,1    ,30,210,BLUE,1    ,30,240,BLUE,1    ,30,270,BLUE,1  ,30,300,BLUE,1  
                  dw 45,0,CYAN,1,   45,30,CYAN,1,     45,60,CYAN,1,    45,90,CYAN,1,    45,120,CYAN,1,       45,150,CYAN,1,     45,180,CYAN,1    ,45,210,CYAN,1    ,45,240,CYAN,1    ,45,270,CYAN,1   ,45,300,CYAN,1   
	              dw 60,0,MAGENTA,1,   60,30,MAGENTA,1,  60,60,MAGENTA,1, 60,90,MAGENTA,1, 60,120,MAGENTA,1,    60,150,MAGENTA,1,  60,180,MAGENTA,1 ,60,210,MAGENTA,1 ,60,240,MAGENTA,1 ,60,270,MAGENTA,1 ,60,300,MAGENTA,1   
                  dw 75,0,YELLOW,1,   75,30,YELLOW,1,   75,60,YELLOW,1,  75,90,YELLOW,1,  75,120,YELLOW,1,     75,150,YELLOW,1,   75,180,YELLOW,1  ,75,210,YELLOW,1  ,75,240,YELLOW,1  ,75,270,YELLOW,1 ,75,300,YELLOW,1     
	              dw 90,0,LIGHTRED,1,   90,30,LIGHTRED,1, 90,60,LIGHTRED,1, 90,90,LIGHTRED,1, 90,120,LIGHTRED,1, 90,150,LIGHTRED,1, 90,180,LIGHTRED,1 ,90,210,LIGHTRED,1 ,90,240,LIGHTRED,1 ,90,270,LIGHTRED,1 ,90,300,LIGHTRED,1  				  
				  
    ; Filenames for Bitmap files (Start/End/Over) 				   
	GameEndScreen  db 'end.bmp',0
	StartScreen    db 'start.bmp',0
	GameOverScreen db 'gameover.bmp',0
	
	; File handle (For file open) & error message (file missing ..)
	filehandle     dw ?		
	ErrorMsg       db 'Error', 13, 10,'$'	
	
	; Arrays to store bitmap 
	Header         db 54    dup (0)
	Palette        db 256*4 dup (0)
	ScrLine        db 320   dup (0)
	
	; Score string (8 characters)			  
    ScoreString db '00000000','$' 
	ScoresMsg db 'Score='

	; Lives  string (8 characters)
	LivesMsg  db 3,3,3 ; 
	SpaceMsg  db '   '

; **********************************************************************
; *                        Code                                        *
; **********************************************************************
CODESEG
	

  ;***********************************************************************
  ; Name: printlives
  ; Description : Print how many lives on screen
  ; 
  ;***********************************************************************
  proc printlives 
   
    ; *** Write empty message (Erase before write again ..)
    mov ah,13h
    mov al,0
    mov bh,0
    mov bl,LIGHTRED;color of the text (white foreground and black background)
    mov cx,3;length of string
    mov dh,0 ;y coordinate
    mov dl,0 ;x coordinate
    mov bp,offset SpaceMsg ;mov bp the offset of the string
    int 10h    
	
   ; *** Write Number of lives (string length)
    mov cx,[LivesCount] ;length of string
    mov dh,0 ;y coordinate
    mov dl,0 ;x coordinate
    mov bp,offset LivesMsg  ;mov bp the offset of the string
    int 10h
	
    ret
    endp printlives	
  ;***********************************************************************


  ;***********************************************************************
  ; Name: Open file 
  ; Description : Open file 
  ; Inputs      : DX register: File offset 
  ; Outputs     : Update [filehandle] variable 
  ;*********************************************************************** 
  proc OpenFile
	
    ; Open file  
	mov ah, 3Dh
	xor al, al
	int 21h		

	; Check file open error 
	jc openerror
	
    ; Return file handle in ax 	
	mov [filehandle], ax 
		
	ret
	
	openerror:
	  mov dx, offset ErrorMsg
	  mov ah, 9h
	  int 21h
	  ret
  endp OpenFile
  ;*********************************************************************** 
	
  ;***********************************************************************
  ; Name: Read header 
  ; Description : Read bitmap file header to Header global variable  
  ;*********************************************************************** 
  proc ReadHeader
	
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
	
  endp ReadHeader
  ;*********************************************************************** 
 
  ;***********************************************************************
  ; Name: ReadPalette 
  ; Description : Read 256*4 (1024) bytes into Pallete global variable 
  ;*********************************************************************** 
  proc ReadPalette
  
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
	
  endp ReadPalette 
	
  ;***********************************************************************
  ; Name: CopyPal 
  ; Description : Copy pallete into video memory
  ;*********************************************************************** 
  proc CopyPal
  
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	mov al,[si+2] ; Get red value.
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it.
	mov al,[si+1] ; Get green value.
	shr al,2
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2
	out dx,al ; Send it.
	add si,4 ; Point to next color.
	; (There is a null chr. after every color.)
 
    loop PalLoop
	ret
	
  endp CopyPal
  ;***********************************************************************

  ;***********************************************************************
  ; Name: CopyBitmap 
  ; Description : Read graphic line by line & display from bottom to top
  ;*********************************************************************** 
 
  proc CopyBitmap
	; BMP graphics are saved upside-down.
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	push es
	
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop:
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	;rep movsb is same as the following code:
	;mov es:di, ds:si
	;inc si
	;inc di
	;dec cx
    ;loop until cx=0
	pop cx
	loop PrintBMPLoop
	
	pop es
	ret
  endp CopyBitmap 
	
	
  ;***********************************************************************
  ; Name: Lives
  ; Description : Check if the ball is in the Y end of the screen, dec
  ;               LivesCount by one and start the level again
  ;***********************************************************************
  proc Lives
	
	; Step 1: Check whether Ball is under Bat => Player lose life 
	mov ax, [ball_y]
	cmp ax, 195    ;if the ball is under the player, dec LivesCount and start the level again		
    jne LivesExit
	
	; Step 2: Reduce life by 1 	
	dec [LivesCount]        
	
	; Step 3: Print lives again 
	call printlives 	
 
    ; Step 4: Erase Bat & ball  
    call EraseBat
    call EraseBall

    ; Step 5: Initialize bat to middle position 	
    mov [Bat_x] , BAT_INIT_POS_X       
    mov [Bat_y] , BAT_INIT_POS_Y  
	
	; Initialize ball & draw it 
	mov [Ball_x],BALL_INIT_POS_X
    mov [Ball_y],BALL_INIT_POS_y

	call DrawBat
	call DrawBall 
	
LivesExit:
	ret
  endp Lives
	
	
  ; **********************************************************************
  ; Name: ScoreToAscii
  ; Description: Change score to Ascii string 
  ; **********************************************************************
  proc ScoreToAscii 
    mov si,offset ScoreString		
		
	; Step 1: Print "00000000" (8xZeros)
    mov cx,8 		
ZeroScore_Label:  	
	mov [byte ptr si],'0'
	inc si 
    loop ZeroScore_Label 		
    lea si,[ScoreString+9]

	; Step 2: Put score in AX & 10 in BX 
    MOV AX,[Score]          ;CX = VALUE THAT I WANT TO CONVERT
    MOV BX,10         

	; Step 3: Loop on score digits , each digit is recieved via Score/10 reminder (Stored in DX)
    MOV AX,[Score]          ;CX = VALUE THAT I WANT TO CONVERT
    MOV BX,10         
	
ASC2:
    mov dx,0            ; clear dx prior to dividing dx:ax by bx
    div BX              ;DIV AX/10
    ADD DX,48           ; ADD 48 TO REMAINDER TO GET ASCII CHARACTER OF NUMBER 
    dec si              ; store characters in reverse order
    mov [si],dl
    CMP AX,0            
    JZ EXTT             ;IF AX=0, END OF THE PROCEDURE
    JMP ASC2            ;ELSE REPEAT
		
	
EXTT:
    
	;// Print score string .. 
    mov ah,13h
    mov al,0
    mov bh,0
    mov bl,YELLOW 
    mov cx,9  ; Length of string
    mov dh,0  ; Y coordinate
    mov dl,70 ; X coordinate
    mov bp,offset ScoreString ;mov bp the offset of the string
    int 10h

    ret
  endp ScoreToAscii 
	
  ; **********************************************************************
  ; Name: BatAndBallInteract
  ; Description: Check Bat and ball contention & change ball direction 
  ; **********************************************************************
  proc BatAndBallInteract  		
  
	push es
	
	; //Step 1: Compare that ball is in same y position as bat 
	mov ax,[Bat_y]		
	cmp ax,[Ball_y]
	jne BatAndBallInteract_Exit 
	
	; //Step 2: Check whether [Bat_x]+[Bat_Length] > [Ball_x] > [Bat_x]  => Bat X position is in ball range 
	mov ax,[Bat_x]	
	cmp [Ball_x],ax ;   //cmp ball_x,bat_x	
	jb BatAndBallInteract_Exit

    add ax,[Bat_Length]
	cmp [Ball_x],ax ;   //cmp ball_x,bat_x	
	ja BatAndBallInteract_Exit
	
	; //Step 3: Now we have a hit => Change ball direction (Always up after hitting ball)
    mov [Ball_y_Dir],'u'
	

    ; //Step 5: Read timer value stored in 0040:00C6 to get random number 
	mov ax, 40h
    mov es, ax
    mov ax, [es:6Ch]
	
	; //Step 6: Multiply random number by 1 => Random value is 0 or 1 (Mask 1st bit ..)
    and al, 00000001b
		
	;// Step 7: Based on al value (0/1) change X slope (1/2) 
	;//         Ball slope defines what is X movement on each main loop cycle 	
	cmp al , 0 
	je slope_label_1
	mov [BallSlope],2	
	jmp slope_label_2 
slope_label_1:
	mov [BallSlope],1	
slope_label_2:		
	
BatAndBallInteract_Exit: 

	;// Step 8: After interaction bat collide with Bat & erase bat => Let's erase it 
    call DrawBat 
	
	pop es

	ret
  endp BatAndBallInteract	
  ; **********************************************************************

  ; **********************************************************************
  ; Name: BallAndBricksInteract
  ; Description: Check bricks and ball contention & change ball direction 
  ; **********************************************************************
  proc BallAndBricksInteract  		

    ;// Load BX to point on bricks array   
	;//   [BX]   : Y location
	;//   [BX+2] : x location
	;//   [BX+6] : Brick "Alive"  '0': Inactive / '1':Exist (Alive)

    mov bx,offset bricks_Arr_YX

    ;// Loop over all bricks  	
 	mov cx,[BricksCount]	
LoopBallBricksCollisionCheck: 
	
	; Check Ball & bricks collision (Y axis) =>   ([Brick_Y]+BricksSize_Y)  > [Ball_y] > [Brick_Y]
	mov ax,[bx]         ; Brick y Start location 
	cmp [Ball_y],ax     ; cmp ball_y,brick_y	
	jb NextBrickCheck

    add ax,[BricksSize_Y]
	cmp [Ball_y],ax ;   //cmp ball_y,brick_y_end	
	ja NextBrickCheck
	
	; Check Ball & bricks collision (X axis) =>   ([Brick_X]+BricksSize_x)  > [Ball_x] > [Brick_x]
	mov ax,[bx+2]         ; Brick x Start location 
	cmp [Ball_x],ax       ; cmp ball_x,brick_x	
	jb NextBrickCheck

    add ax,[BricksSize_X]
	cmp [Ball_x],ax ;   //cmp ball_x,brick_x_end	
	ja NextBrickCheck
	
	; Skip bricks which are not "alive"
	cmp [byte ptr (bx+6)],0h        
    je 	NextBrickCheck
	
	; // Collision detected !! => Let's deal with it .. 
	
    ; // Step 1. Set brick as inactive 
	mov [byte ptr (bx+6)],0h

    ; // Step 2. Change ball y direction (If direction was up than go down and vice versa)
	cmp [Ball_y_Dir],'u'
    je	BallAndBricksInteract_up_label    
    mov [Ball_y_Dir],'u'
    jmp BallAndBricksInteract_down_label
BallAndBricksInteract_up_label:
    mov [Ball_y_Dir],'d'
BallAndBricksInteract_down_label:
    
	; //Step 3. Erase Block (Draw with color black) 	
    mov ax,[bx+2]
    mov [Box_x],ax

    mov ax,[bx]
    mov [Box_y],ax

    mov [Box_Color],BLACK	
    mov [BoxSizeY],10	
    mov [BoxSizeX],20		
    call DrawBox 
   
   ; // Step 4: Update score (+100) & print it 
   add [Score],100 
   call ScoreToAscii
   
   ; //Step 5: Decrease number of "alive" bricks  
   dec [BricksAlive]  
   
   ; //Step 6: Collision with brick detected => No need to continue loop check collision with other bricks (optimization )
   jmp BallAndBricksInteract_Exit
	
   ; Collision not detected => Move pointer to point on next brick .. 	
NextBrickCheck: 	
   add bx,8
 
   ; Here we go again to next brick collision check .. 
   loop LoopBallBricksCollisionCheck
		
BallAndBricksInteract_Exit: 		
   ret
  endp BallAndBricksInteract	
  ; **********************************************************************
		
	
  ; **********************************************************************
  ; Name: EraseBat
  ; Description: Erase bat line  
  ; **********************************************************************
  proc EraseBat  		
	
	mov cx,SCREEN_WIDTH	
	mov [Temp_X] , 0
	
EraseBatLoop: 
	push cx
		
	; Draw pixel at Bat_x,Bat_y position 	
	mov al ,BLACK  ; Black Color (Erase)
	mov bl ,0  ; Always 0   
	
	mov cx , [Temp_X]  ; X cordinates (0..320)
	mov dx , [Bat_y]   ; Y cordinates    	
	mov ah,0ch         ; Draw a pixel  
	int 10h
	
	pop cx
    inc [Temp_X]
	
	loop EraseBatLoop
	
    ret
  endp EraseBat	
  ; **********************************************************************
	
  ; **********************************************************************
  ; Name: DrawBat
  ; Description: Draws a bat on screen 
  ; Inputs: Bat_x       : Bat location 
  ;         Bat_y	
  ;         Bat_Length  : Bat size in pixels 
  ;         Bat_Color   : Bat Color    
  ; **********************************************************************
  proc DrawBat 	
	
    ; Initialize Temp with X position 	
	mov bx , [Bat_x]		
	mov [Temp_X] , bx	
	
	; Loop over CX pixels 
    mov cx , [Bat_Length]
Draw_Pixel_Label:  	

    ; Draw corrupt CX => Save it in stack 
    push cx 
	
    ; Draw pixel at Bat_x,Bat_y position 	
	mov al , [Bat_Color] ; Color in AL 
	mov bl , 0           ; Always 0   
	mov cx , [Temp_X]    ; X cordinates 
	mov dx , [Bat_y]     ; Y cordinates    	
	mov ah,0ch           ; Draw a pixel  
	int 10h

    ; Return CX from stack 
    pop cx 	
	     
    inc [Temp_X]
			
	loop Draw_Pixel_Label           	
		
exit_draw_bat: 
	ret
  endp DrawBat	
  ; **********************************************************************

  ; **********************************************************************
  ; Name: DrawBall
  ; Description: Draws a ball 
  ; Inputs: Ball_x       : Bat location 
  ;         Ball_y	
  ;         Ball_Color   : Bat Color    
  ; **********************************************************************
  proc DrawBall 	
  
    mov al , [Ball_Color] ; Color in AL 
	mov bl , 0            ; Always 0   

    ; // Draw 4 pixels in (X,Y),(X+1,Y),(X,Y+1),(X+1,Y+1)
	mov cx , [Ball_x]     ; X cordinates 
	mov dx , [Ball_y]     ; Y cordinates    	
	mov ah,0ch            ; Draw a pixel  
	int 10h	     

	mov cx , [Ball_x]     ; X cordinates 
	inc cx
  	mov dx , [Ball_y]     ; Y cordinates    	
  	mov ah,0ch            ; Draw a pixel  
	int 10h	       

	mov cx , [Ball_x]     ; X cordinates  
	mov dx , [Ball_y]     ; X cordinates 
	inc dx
  	mov ah,0ch            ; Draw a pixel  
	int 10h	         
	  
	mov cx , [Ball_x]     ; X cordinates 
	mov dx , [Ball_y]     ; X cordinates 
	inc cx
	inc dx
  	mov ah,0ch            ; Draw a pixel    
	int 10h	       
	  
	ret
	
  endp DrawBall	
  ; **********************************************************************

  ; **********************************************************************
  ; Name: DrawBox
  ; Description: Draws a pixel 
  ; **********************************************************************
  proc DrawBox 

	push ax
	push bx 
	push cx 
	push dx
	
    mov bx,[Box_x]
    mov [Tmp_X],bx

    ; Draw Y lines (After each line we increase [Box_y] hence point to next line)
DrawPixel_y_Loop_Label:
 
    mov cx, [BoxSizeX]
    push cx

    ; Draw line (BoxSizeX) pixels from [Box_x] , X offset is stored in [Tmp_X]
DrawPixel_X_Loop_Label:
	
	mov al , [Box_Color] ; Color in AL 
	mov bl , 0            ; Always 0   
	mov cx , [Tmp_X]     ; X cordinates 
	mov dx , [Box_y]     ; Y cordinates    	
	
	mov ah,0ch            ; Draw a pixel  
	int 10h	     

    inc [Tmp_X]     ; X cordinates 	
	
	dec [BoxSizeX]	
	cmp [BoxSizeX],0
	jne DrawPixel_X_Loop_Label  
		
	pop cx 
	mov [BoxSizeX],cx
	
    mov bx,[Box_x]
    mov [Tmp_X],bx

	inc [Box_y]     ; Y cordinates 	
	
	dec [BoxSizeY]
	cmp [BoxSizeY],0
	jne DrawPixel_y_Loop_Label      	

	pop dx
	pop cx 
	pop bx 
	pop ax
	
	ret
  endp DrawBox	
  ; **********************************************************************
		
  ; **********************************************************************
  ; Name: HandleBatMovement
  ; Description: Handle Bat movement based on keyboard 
  ; **********************************************************************
  proc HandleBatMovement 
  
    ;// Step 1: Check whether key was pressed   
    mov ah,1
    int 16h
    jz HandleBatFinish_Label

    ;// Step 2: Key pressed Read it to al & clear buffer 
    mov ah,0 ; there is a key in the buffer, read it and clear the buffer
    int 16h

    ;// Step 3: 'd' pressed => Move to the right => X value is increased by BAT_SPEED
    cmp al,KEY_RIGHT;'d'
    jne cont_1
  
    ;//Step 4: If Bat location higher than screen end (320-Bat length) => Do not move (We cannot move further to the right ..)
    cmp [Bat_x] , 280  ; End of Bat (320(Last X pixel) - 40 (Bat length) 
    je cont_1          ; Stop movement if bat is on screen end    
   
    ;//Step 5: OK to Move right 
    add [Bat_x] , BAT_SPEED    ; Control bat speed
    call EraseBat  
    call DrawBat
	
cont_1:  

    ;//Step 5: Move left check  
    cmp al,KEY_LEFT;'a'
    jne cont_2 

	;//Step 6: Check bat Bat is on the left most position & we try to move more to the left => Do not move .. 
    cmp [Bat_x] , 0  ;  
    je cont_2        ; Stop movement if bat is on screen end      
	
    ;//Step 5: OK to move left ..  
    sub [Bat_x] , BAT_SPEED

    ;//Step 5: Draw bat at new location & erase old Bat drawing 
    call EraseBat
    call DrawBat
cont_2:  

HandleBatFinish_Label:     
   ret	
 endp HandleBatMovement	
  ; **********************************************************************
  
  ; **********************************************************************
  ; Name: ShowBitmap
  ; Description: Show bitmap on screen (pointed by DX)
  ; **********************************************************************
  proc ShowBitmap 	    

   call OpenFile
   call ReadHeader
   call ReadPalette
   call CopyPal
   call CopyBitmap 
   ret
  endp ShowBitmap	
  ; **********************************************************************   		
		
  ; **********************************************************************
  ; Name: EraseBall
  ; Description: Erase a ball 
  ; **********************************************************************
  proc EraseBall 	
  
	mov al , BLACK        ; Black color  
	mov bl , 0            ; Always 0   
	mov cx , [Ball_x]     ; X cordinates 
	mov dx , [Ball_y]     ; Y cordinates    	
	mov ah,0ch            ; Draw a pixel  
	int 10h

	mov cx , [Ball_x]     ; X cordinates 
  	mov dx , [Ball_y]     ; Y cordinates  
	inc cx  
  	mov ah,0ch            ; Draw a pixel  
	int 10h	       

	mov cx , [Ball_x]     ; X cordinates  
	mov dx , [Ball_y]     ; X cordinates 
  	mov ah,0ch            ; Draw a pixel  
	inc dx
	int 10h	         
	  
	mov cx , [Ball_x]   ; X cordinates 
	mov dx , [Ball_y]   ; X cordinates 
 	inc cx
	inc dx
  	mov ah,0ch            ; Draw a pixel    
	int 10h	       
	  
    ret
  endp EraseBall	
  ; **********************************************************************

  ; **********************************************************************
  ; Name: MoveBall
  ; Description: Move the ball 
  ; **********************************************************************
  proc MoveBall 	
	
	;// Step 1: X Position setup
	
    ; Did we reach the X screen right size => Switch direction to the left 'l'
	cmp [Ball_x],320	
	jb MoveBall_cont1
    mov [Ball_X_Dir] , 'l'
MoveBall_cont1: 	

    ; Did we reach the X screen left size => Switch direction to the right 'r'
	cmp [Ball_x],0
	jne MoveBall_cont2
    mov [Ball_X_Dir] , 'r'
MoveBall_cont2: 	

	;// Step 2: Check ball direction (Left-'l' or right-'r') => Add/Sub [BallSlope] to Ball_x accordingly 
	cmp [Ball_X_Dir] , 'r'
	je MoveBall_Right		
	
	mov ax,[BallSlope]
  	sub [Ball_x] ,ax	 ; Left x movement	
	jmp MoveBall_left_cont	
MoveBall_Right: 
	mov ax,[BallSlope]
  	add [Ball_x] ,ax  ; Y x movement 

MoveBall_left_cont: 
	
	;// Step 3: Y Position setup
	
    ; Did we reach the y top of screen => Switch direction of the ball to be down
	cmp [Ball_y],10
	jne MoveBall_cont4
    mov [Ball_y_Dir] , 'd'
MoveBall_cont4: 	

	;// Step 4: Check ball direction (Up-'u' or Down-'d') => Add/Sub to Ball_Y accordingly 	
	cmp [Ball_y_Dir] , 'd'
	je MoveBall_down		
  	sub [Ball_y] ,1	 ; up y movement	
	jmp MoveBall_up_cont	

MoveBall_down: 
  	add [Ball_y] ,1  ; down y movement 
	
MoveBall_up_cont: 
	ret
  endp MoveBall	
  ; **********************************************************************
	
  ; **********************************************************************
  ; Name: DelayLoopProc
  ; Description: Delay loop (Otherwise game runs fast) 
  ; **********************************************************************
  proc DelayLoopProc 	
	
	mov cx,GAME_SPEED
delayloop_label:
    mov ax,9  
    loop delayloop_label   
	ret
  endp DelayLoopProc	
  ; **********************************************************************

  ; **********************************************************************
  ; Name: WaitForKey
  ; Description: Wait for key (Key value in cl register) 
  ; **********************************************************************
  proc WaitForKey
	
WaitForKeyLoop_Label:    
   mov ah,1
   int 16h
   jnz WaitForKeyLoop_Label
   
   mov ah,0 ; there is a key in the buffer, read it and clear the buffer
   int 16h
   
   cmp al,cl
   jne WaitForKeyLoop_Label
   
   ret 
  endp WaitForKey	
  ; **********************************************************************
	
  ; **********************************************************************
  ; ***                           Initialization                       ***
  ; **********************************************************************

start:
   mov ax, @data   ; Set ds to point on data segment    
   mov ds, ax
   mov es, ax	

   ; ***                           Start-up screen                     ***
	
   ; Set graphics mode 320x200x256 
   mov ax,13h
   int 10h	

   ; Show start screen 
   mov dx, offset StartScreen
   call  ShowBitmap 	    

   ; Wait for start game 
   mov cl,'s'
   call  WaitForKey

   ; ***                          Game initialization                 ***
   
TryAgainLabel:	
	
    ; Set graphics mode 320x200x256 
    mov ax,13h
    int 10h	
 	
    ; Initialize game variables (Score=0/Lives=3/AmountOfBricks=66) 
    mov [Score],0
    mov [BricksAlive],NUM_OF_BRICKS
    mov [LivesCount],3	
	
	; Initialize bat variables:  length (40 pixels) , bat location (120,180) & bat color (Green) 
    mov [Bat_x] , BAT_INIT_POS_X       
    mov [Bat_y] , BAT_INIT_POS_Y  
    mov [Bat_Length] ,40
    mov [Bat_Color] , GREEN	
    call DrawBat 

	; Initialize ball 
    mov [Ball_Color],YELLOW	
	mov [Ball_x],BALL_INIT_POS_X
    mov [Ball_y],BALL_INIT_POS_y
	call DrawBall 
 
   ; ***                      Draw 66 (NUM_OF_BRICKS) bricks on screen                 ***

   ;// Load BX to point on bricks array   
   ;//   [BX]   : Y location
   ;//   [BX+2] : x location
   ;//   [BX+6] : Brick "Alive"  '0': Inactive / '1':Exist (Alive)  
	mov bx,offset bricks_Arr_YX  ; BX points to bricks array   
    mov cx,[BricksCount]         ; CX holds number of bricks (For loop)
		
 DrawBricks_Label: 		
   ; Update [Box_x] & [Box_y] variables with brick X,Y location  
   mov ax,[bx]     ; 1st entry (16 bit) in bricks_Arr holds y location 
   mov dx,[bx+2]   ; 2nd entry (16 bit) in bricks_Arr holds x location       
   mov [Box_x],dx
   mov [Box_y],ax
   
   ; Update box color & put it in [Box_Color] variable  
   mov dx,[bx+4]
   mov [Box_Color],dl	
   
   ; Brick size is always 10x20 pixels 
   mov [BoxSizeY],10	    
   mov [BoxSizeX],20		
   
   ; Entry 6 holds brick "Dead/Alive" flag since game starts => All bricks exist .. 
   mov [byte ptr (bx+6)],1
   
   ; Draw the brick , in [Box_x] , [Box_y] location with color [Box_Color]
   call DrawBox 
   
DrawBricks_Nextblock_Label:   
    
   ; Move pointer by 8 to get to next brick data in RAM (Each brick information is 8 bytes 	
   add bx , 8   
   
   loop DrawBricks_Label 
   
   call printlives
   call DelayLoopProc 
   
   mov [Ball_y_Dir],'d'
   ; **********************************************************************
   ; ***                           main loop                            ***
   ; **********************************************************************
main_loop: 
    
   ;// Step 1: Handle Bat movement (Right/Left movement based on keyboard)   
   call HandleBatMovement      
   
   ;// Step 2: Erase ball (We would draw it again after movement)
   call EraseBall 
   
   ;// Step 3: Calculate ball new position (Ball_x,Ball_y)
   call moveBall      

   ;// Step 4: Draw ball in new position (Ball_x,Ball_y)   
   call DrawBall    

   ;// Step 5: If ball & bat "touch" => Change ball Y direction (Ball_y_Dir) from down to up 
   call BatAndBallInteract

   ;// Step 5: If ball match brick location => EraseBrick & AddScore & Reduce Brickcounter & Change Y direction 
   call BallAndBricksInteract

   ;// Step 6: Print lives & score 
   call Lives
   call ScoreToAscii   
   
   ;// Step 7: Check whether game finished (No more lifes / No more bricks)   
   cmp [LivesCount],0
   je GameOver_Label
   cmp [BricksAlive],0
   je GameFinish_Label
   
   ;// Step 8: Delay (Slow game a little ..)
   call DelayLoopProc ; Otherwise game runs too fast :)    
   
   jmp main_loop 
   ; **********************************************************************

   ; **********************************************************************
   ; ***                           Game over                           ***
   ; **********************************************************************

GameOver_Label:
    mov dx, offset GameOverScreen     ; Show game over screen 
    call  ShowBitmap 	    
    jmp WaitForKeyboard_Label
GameFinish_Label:
    mov dx, offset GameEndScreen      ; Show game End screen 
    call  ShowBitmap 	    

WaitForKeyboard_Label:    
   mov ah,0Ch   ; Read keyboard 
   mov al,07h
   int 21h

   ; 'r': Startup screen     
   cmp al,'r'
   je start_jump_label  

   ; 't': Try again (No startup screen)
   cmp al,'t'
   je Try_jump_label ; Compilation error jump to far => Jump relative close      

   ; 'q': Exit game   
   cmp al,'q'
   je exit  

   loop WaitForKeyboard_Label
   
start_jump_label:
   jmp start   

Try_jump_label:
   jmp TryAgainLabel   
   
   ; **********************************************************************
   ; ***                           Exit game                            ***
   ; **********************************************************************   
exit:
mov ax, 4c00h
int 21h
   ; **********************************************************************
END start 


      