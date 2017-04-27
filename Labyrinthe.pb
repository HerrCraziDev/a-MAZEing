
#LabWidth = 350
#LabHeigth  = 200
#CaseSize = 3

Enumeration 
  #Blank = 0
  #Wall
  #FixedWall
EndEnumeration
#Visited = 1

Structure LABCASE
  Visited.b
  Type.b
  TopWall.b
  BottomWall.b
  LeftWall.b
  RightWall.b
EndStructure

Global Dim LabMap.LABCASE(#LabWidth+4,#LabHeigth+4)

;LabMap(1,1)\Visited = 1

Macro DrawLabCase(case_x,case_y)
  pos_x = (case_x-1)*#CaseSize
  pos_y = (case_y-1)*#CaseSize
  ;pos_x+1 : pos_y+1
  If LabMap(case_x,case_y)\Visited
    Box(pos_x,pos_y,#CaseSize,#CaseSize,$FFFFFF)
  Else
    Box(pos_x,pos_y,#CaseSize,#CaseSize,$DDDDDD)
  EndIf
  
  
  If LabMap(case_x,case_y-1)\TopWall;Dessus
    LineXY(pos_x, pos_y, pos_x+#CaseSize, pos_y, $000000)
  EndIf
  If LabMap(case_x,case_y+1)\BottomWall;Dessous
    LineXY(pos_x, pos_y+#CaseSize, pos_x+#CaseSize, pos_y+#CaseSize, $000000)
  EndIf
  If LabMap(case_x-1,case_y)\LeftWall;Gauche
    LineXY(pos_x, pos_y, pos_x, pos_y+#CaseSize, $000000)
  EndIf
  If LabMap(case_x+1,case_y)\RightWall;Droite
    LineXY(pos_x+#CaseSize, pos_y, pos_x+#CaseSize, pos_y+#CaseSize, $000000)
  EndIf
EndMacro


Procedure CountNearestFreeCases(x,y)
  ProcedureReturn 4 - LabMap(x-2,y)\Visited-LabMap(x+2,y)\Visited-LabMap(x,y+2)\Visited-LabMap(x,y-2)\Visited
EndProcedure

Procedure InitLabMap()
  For i=0 To #LabWidth+4
    For j=0 To #LabHeigth+4
      LabMap(i,j)\Type = #Wall
    Next
  Next
  For i=1 To #LabWidth+2 Step 2
    For j=1 To #LabHeigth+2 Step 2
      LabMap(i,j)\Type = #Blank
    Next
  Next
  
  For x=0 To #LabWidth+4
    LabMap(x,0)\Visited = #Visited : LabMap(x,0)\Type = #FixedWall
    LabMap(x,1)\Visited = #Visited : LabMap(x,1)\Type = #FixedWall
    LabMap(x,#LabHeigth+3)\Visited = #Visited : LabMap(x,#LabHeigth+3)\Type = #FixedWall
    LabMap(x,#LabHeigth+4)\Visited = #Visited : LabMap(x,#LabHeigth+4)\Type = #FixedWall
  Next
  For y=0 To #LabHeigth+4
    LabMap(0,y)\Visited = #Visited : LabMap(0,y)\Type = #FixedWall
    LabMap(1,y)\Visited = #Visited : LabMap(1,y)\Type = #FixedWall
    LabMap(#LabWidth+3,y)\Visited = #Visited : LabMap(#LabWidth+3,y)\Type = #FixedWall
    LabMap(#LabWidth+4,y)\Visited = #Visited : LabMap(#LabWidth+4,y)\Type = #FixedWall
  Next  
  
  For i=0 To #LabWidth+2
    For j=0 To #LabHeigth+2
      With LabMap(i,j)
        \LeftWall = 1
        \RightWall = 1
        \TopWall = 1
        \BottomWall = 1
      EndWith
    Next
  Next
EndProcedure

Procedure RenderLab()
  StartDrawing(CanvasOutput(1))
  Box(0,0,#LabWidth*#CaseSize,#LabHeigth*#CaseSize,$AAAAAA)
  For i=1 To #LabWidth
    For j=1 To #LabHeigth
      DrawLabCase(i,j)
    Next
  Next
  
  LineXY(0,0,0,#LabHeigth*#CaseSize,0)
  LineXY(#LabWidth*#CaseSize-1,0,#LabWidth*#CaseSize-1,#LabHeigth*#CaseSize,0)
  LineXY(0,#LabHeigth*#CaseSize-1,#LabWidth*#CaseSize,#LabHeigth*#CaseSize-1,0)
  LineXY(0,0,#LabWidth*#CaseSize,0,0)
  
  StopDrawing()
EndProcedure

Procedure RenderMaze2()
  StartDrawing(CanvasOutput(1))
  For i=0 To #LabWidth+2
    For j=0 To #LabHeigth+2
      
      Select LabMap(i,j)\Type
        Case #Wall
          color = $000000
        Case #Blank
          color = $FFFFFF
        Case #FixedWall
          color = $FF0000
      EndSelect
      
      x = (i-2)* #CaseSize
      y = (j-2)*#CaseSize
      Box(x,y,#CaseSize,#CaseSize,color)
      
    Next
  Next
  StopDrawing()
EndProcedure

Procedure LoadMaze(File$)
  OpenFile(1,File$)
  
  Maze$ = ReadString(1,#PB_File_IgnoreEOL)
  
EndProcedure

Procedure GenerateMazeMk2(x,y)
 ; Debug "Visit "+x+","+y
  LabMap(x,y)\Visited = 1
  
  If LabMap(x,y)\Type = #FixedWall; Empêcher le traitement des murs incassables au cas ou
    ProcedureReturn 0
  EndIf
  
  freeCases = CountNearestFreeCases(x,y)
  While freeCases  ;S'il reste des cases à explorer autour
    
    ;Recherche d'une case de destination
    Repeat  
      target_x = x
      target_y = y
      direction = Random(3)
      
      Select direction
        Case 0 : target_x-2
        Case 1 : target_x+2
        Case 2 : target_y-2
        Case 3 : target_y+2
      EndSelect
      
    Until Not LabMap(target_x,target_y)\Visited
    
    ;Ouverture du mur correspondant
    wall_x = x
    wall_y = y
    
    Select direction 
      Case 0 : wall_x -1
      Case 1 : wall_x +1
      Case 2 : wall_y -1
      Case 3 : wall_y +1
    EndSelect
    LabMap(wall_x,wall_y)\Type = #Blank
    
    ;Affichage
;     RenderMaze2()
;     
;     StartDrawing(CanvasOutput(1))
;     Box((x-2)*#CaseSize,(y-2)*#CaseSize,#CaseSize,#CaseSize,$FF0000)
;     Box((wall_x-2)*#CaseSize,(wall_y-2)*#CaseSize,#CaseSize,#CaseSize,$FFFF00)
;     StopDrawing()
    
    ;Exploration de la case correspondante
    GenerateMazeMk2(target_x,target_y)
    
    freeCases = CountNearestFreeCases(x,y)
  Wend  
EndProcedure

Procedure RecursiveGenerator(x,y,depth,prev_x,prev_y)
  depth +1
  ;Debug Str(depth)+" : started"
  Debug "processing "+depth+" at "+x+", "+y+", from "+prev_x+", "+prev_y
  LabMap(x,y)\Visited = #Visited
  With LabMap(x,y)
    If x>prev_x
      \LeftWall = 0
    ElseIf x<prev_x
      \RightWall = 0
    ElseIf y<prev_y
      \BottomWall = 0
    ElseIf y>prev_y
      \TopWall = 0
    Else
      Debug "Truc pas normal !!!"
    EndIf
    prev_x=x
    prev_y=y
  EndWith
  
  Repeat
    
    If LabMap(x+1,y)\Visited And LabMap(x-1,y)\Visited And LabMap(x,y+1)\Visited And LabMap(x,y-1)\Visited
      Debug "déja vu "+CountNearestFreeCases(x,y)
      StartDrawing(CanvasOutput(1))
      Box((x-1)* #CaseSize,(y-1)* #CaseSize,#CaseSize,#CaseSize,$0000FF)
      StopDrawing()
      ProcedureReturn 1
    Else
      
      
      ;Debug "open at x : "+x+", y: "+y
      Repeat
        ;pas_bien:
        tmp_x = x
        tmp_y = y
        
        direction = Random(3)
        
        Select direction
          Case 0 : tmp_x-1
          Case 1 : tmp_x+1
          Case 2 : tmp_y-1
          Case 3 : tmp_y+1
        EndSelect
        
      Until Not LabMap(tmp_x,tmp_y)\Visited
      x = tmp_x
      y = tmp_y
      
      With LabMap(x,y)
        Select direction
          Case 0 : \LeftWall = 0
          Case 1 : \RightWall = 0
          Case 2 : \TopWall = 0
          Case 3 : \BottomWall = 0
        EndSelect
      EndWith      
      
      ;RenderLab()
      
      StartDrawing(CanvasOutput(1))
      Box((x-1)* #CaseSize,(y-1)* #CaseSize,#CaseSize,#CaseSize,depth)
      StopDrawing()
      
      ;Delay(50)
      
    EndIf
  Until RecursiveGenerator(x,y,depth,prev_x,prev_y)
  ; Debug Str(depth)+" : done"
EndProcedure

Procedure thRecursion(placeholder)
  Debug "*****Starting generation*****"
  GenerateMazeMk2(3,3)
  Debug "***********Done**************"
  RenderMaze2()
EndProcedure


InitLabMap()

OpenWindow(1,0,0,(#LabWidth+1)*#CaseSize+4,(#LabHeigth+1)*#CaseSize+4,"Labyrinthe",#PB_Window_ScreenCentered | #PB_Window_SystemMenu)
CanvasGadget(1,2,2,(#LabWidth+1)*#CaseSize,(#LabHeigth+1)*#CaseSize)

RenderMaze2()

CreateThread(@thRecursion(),1)

Repeat 
  event = WaitWindowEvent() 
  
  If EventGadget() = 1
    
    x = GetGadgetAttribute(1,#PB_Canvas_MouseX)/#CaseSize+1
    y = GetGadgetAttribute(1,#PB_Canvas_MouseY)/#CaseSize+1
    
    Select EventType()
      Case #PB_EventType_LeftClick
        
      Case #PB_EventType_RightClick
        StartDrawing(CanvasOutput(1))
        FillArea(GetGadgetAttribute(1,#PB_Canvas_MouseX),GetGadgetAttribute(1,#PB_Canvas_MouseY),0,RGB(Random(50)+200,Random(50)+200,Random(50)+200))
        StopDrawing()
        
      Case #PB_EventType_LeftDoubleClick
        
        
    EndSelect
  EndIf
  
Until  event = #PB_Event_CloseWindow
; IDE Options = PureBasic 5.60 (Windows - x64)
; CursorPosition = 1
; Folding = --
; EnableThread
; EnableXP