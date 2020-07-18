## breakout_asm
Demo x86 project (Breakout game) as a fun project , The code is written in x86 assembler 

**Step #1: Download a dosbox **
Download a dosbox enviroment for your target (PC/Mac) 
Link: https://www.dosbox.com/

**Step #2: Build the game (Compile & link) **
Compile: tasm\bin\tasm  /l/zi breakout   
Link:    tasm\bin\tlink /v second
Output should be breakout.exe executable 

**Instructions**
1. Make sure ball does not hit floor , Try to hit bricks 
2. Game level end when all bricks are gone 
3. Movement 
   - 'a': Move left 
   - 'd': Move right 
4. Points: 100xpoints for each brick 



