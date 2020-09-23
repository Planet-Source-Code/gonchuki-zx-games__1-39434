Attribute VB_Name = "modZXBasic"
Option Explicit

'ZX-BASIC
' Copyright © 2002 by gonchuki
' e-mail: gonchuki@yahoo.es

'This module is not yet complete, as some of the original
'functions are not implemented here. Some of them because
'i don't need them, and the others because they are only
'applicable to the ZX Spectrum

Private Declare Function SetPixel Lib "gdi32" Alias "SetPixelV" (ByVal hDC As Long, ByVal X As Long, ByVal y As Long, ByVal crColor As Long) As Long
Private Declare Function GetPixel Lib "gdi32" (ByVal hDC As Long, ByVal X As Long, ByVal y As Long) As Long

Private Declare Sub FillMemory Lib "kernel32.dll" Alias "RtlFillMemory" (Destination As Any, ByVal Length As Long, ByVal Fill As Byte)

Private Declare Function MoveToEx Lib "gdi32" (ByVal hDC As Long, ByVal X As Long, ByVal y As Long, lpPoint As POINTAPI) As Long
Private Declare Function LineTo Lib "gdi32" (ByVal hDC As Long, ByVal X As Long, ByVal y As Long) As Long
Private Declare Function CreatePen Lib "gdi32" (ByVal nPenStyle As Long, ByVal nWidth As Long, ByVal crColor As Long) As Long
Private Const PS_SOLID = 0

Private Declare Function SelectObject Lib "gdi32" (ByVal hDC As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long

Private Declare Function CreateRectRgn Lib "gdi32" (ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Private Declare Function SelectClipRgn Lib "gdi32" (ByVal hDC As Long, ByVal hRgn As Long) As Long

Private Declare Function SetBkColor Lib "gdi32" (ByVal hDC As Long, ByVal crColor As Long) As Long
Private Declare Function SetTextColor Lib "gdi32" (ByVal hDC As Long, ByVal crColor As Long) As Long
Private Declare Function TextOut Lib "gdi32" Alias "TextOutA" (ByVal hDC As Long, ByVal X As Long, ByVal y As Long, ByVal lpString As String, ByVal nCount As Long) As Long

Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer
Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)

Private Declare Function Sound Lib "Speaker.dll" (ByVal dwFreq As Long, ByVal dwDuration As Long) As Boolean

Private Type POINTAPI
    X As Long
    y As Long
End Type

Private frmViewport  As Form
Private picViewport As PictureBox
Private bCOLORS(0 To 7, 0 To 1) As Long, CI As Long, CP As Long, CB As Long
Private useBright As Boolean, isInverse As Boolean
Private Keys As String, KeysL As String
Private cDATA As New Collection
Private Chars As New VirtualDC, cChar As New VirtualDC
Private gsView As New VirtualDC
Private scChars(0 To 23, 0 To 31) As Byte, atChars(0 To 23, 0 To 31) As Byte
Public vRatio As Byte, fullUPDT As Boolean

'the initialization routines...
Public Property Set viewForm(ByVal theForm As Form)
    Set frmViewport = theForm
End Property

Public Property Set viewPic(ByVal thePic As PictureBox)
    Set picViewport = thePic
End Property

Public Sub InitBasic()
Dim i As Long, j As Long
'we initialize this array with the original colors used in the ZX Spectrum, the second bound is to represent the color when BRIGHT is set to 1
    bCOLORS(0, 0) = &H0
    bCOLORS(0, 1) = &H202020
    bCOLORS(1, 0) = &HC00000
    bCOLORS(1, 1) = &HFF0000
    bCOLORS(2, 0) = &HDF
    bCOLORS(2, 1) = &HFF
    bCOLORS(3, 0) = &H8000FF
    bCOLORS(3, 1) = &HA000FF
    bCOLORS(4, 0) = &HC060&
    bCOLORS(4, 1) = &HFF80&
    bCOLORS(5, 0) = &HFFDF00
    bCOLORS(5, 1) = &HFFF000
    bCOLORS(6, 0) = &HDFDF&
    bCOLORS(6, 1) = &HFFFF&
    bCOLORS(7, 0) = &HDFDFDF
    bCOLORS(7, 1) = &HFFFFFF
    
'and these are the default colors used
    PAPER 7: INK 0: BORDER 7
    
    Chars.Clone LoadPicture("chars.gif"): cChar.Create 8, 8
    gsView.Create 256, 192: If vRatio = 0 Then vRatio = 100
End Sub

Public Sub AddKey(ByVal KeyAscii As Long)
    If KeyAscii = 8 Then 'backspace key pressed...
        If Len(Keys) Then Keys = Left$(Keys, Len(Keys) - 1)
    ElseIf KeyAscii > 12 Then 'we simply skip the first control codes that are useless here
        Keys = Keys & Chr$(KeyAscii)
    End If
End Sub

Public Sub SHUTDOWN()
    Unload frmNim
    Set Chars = Nothing: Set cChar = Nothing
    Set gsView = Nothing: Set cDATA = Nothing
    
    End 'well, had to use it... sue me
End Sub

'*********************************
'*  the replacement routines...  *
'*********************************

Sub BORDER(ByVal Color As Long)
    CB = Color
    frmViewport.BackColor = bCOLORS(Color, Abs(useBright))
End Sub

Sub PAPER(ByVal Color As Long)
Dim i As Long: For i = 0 To 23: FillMemory atChars(i, 0), 32, Color * 8 + Abs(useBright) * 64: Next
    CP = Color
    fullUPDT = True
    picViewport.BackColor = bCOLORS(Color, Abs(useBright))
    gsView.FillBackground bCOLORS(Color, Abs(useBright))
    gsView.PaintTo picViewport.hDC
End Sub

Sub INK(ByVal Color As Long)
    CI = Color
End Sub

Sub BRIGHT(ByVal STATUS As Byte)
    useBright = STATUS
End Sub

Sub PRINT_AT(ByVal y As Byte, ByVal X As Byte, Optional ByVal PAPERCOLOR As Long = -1, Optional ByVal INKCOLOR As Long = -1, Optional ByVal HASBRIGHT As Long = -1, Optional ByVal TEXT As String)
Dim i As Long, tc As Byte, tArray() As Byte, atChar As Byte
Dim PC As Long, IC As Long, P2 As Byte, I2 As Byte, B2 As Byte
Dim UB As Boolean: If HASBRIGHT > -1 Then UB = HASBRIGHT Else UB = useBright
If PAPERCOLOR > -1 Then PC = bCOLORS(PAPERCOLOR, Abs(UB)): GoSub 5000 Else PC = bCOLORS(CP, Abs(useBright)): GoSub 5010
If INKCOLOR > -1 Then IC = bCOLORS(INKCOLOR, Abs(UB)): GoSub 5020 Else IC = bCOLORS(CI, Abs(useBright)): GoSub 5030

atChar = I2 + P2 * 8 + B2 * 64

If Len(TEXT) Then
    tArray = StrConv(TEXT, vbFromUnicode)
    For i = 1 To Len(TEXT)
        tc = tArray(i - 1)
        scChars(y, X + i - 1) = tc: atChars(y, X + i - 1) = atChar
        cChar.CloneHDC Chars.hDC, , (tc - ((tc \ &H20) * 32)) * 8, (tc \ &H20) * 8
        gsView.RepBlt cChar, (X + i - 1) * 8, y * 8, IC, PC
    Next
    Erase tArray
End If

Dim rgn1 As Long, rgn2 As Long, vRatioX As Long
vRatioX = ((8 * vRatio) \ 100)
If Not fullUPDT Then
    rgn1 = CreateRectRgn(X * vRatioX, y * vRatioX, (X + Len(TEXT)) * vRatioX, (y + 1) * vRatioX)
Else
    fullUPDT = False
    rgn1 = CreateRectRgn(0, 0, picViewport.ScaleWidth, picViewport.ScaleHeight)
End If
rgn2 = SelectClipRgn(picViewport.hDC, rgn1)

gsView.PaintZoom picViewport.hDC, 0, 0, vRatio
picViewport.Refresh
Call SelectClipRgn(picViewport.hDC, rgn2)
DeleteObject rgn1

Exit Sub
5000 P2 = PAPERCOLOR: B2 = Abs(UB): Return
5010 P2 = CP: B2 = Abs(useBright): Return
5020 I2 = INKCOLOR: B2 = Abs(UB): Return
5030 I2 = CI: B2 = Abs(useBright): Return
End Sub

Sub REFRESH_()
    'this function does not exist in a real spectrum but
    'i needed it to make the zoom function properly
    gsView.PaintZoom picViewport.hDC, 0, 0, vRatio
End Sub

Sub Cls()
Dim i As Long: For i = 0 To 23: FillMemory atChars(i, 0), 32, Color * 8 + Abs(useBright) * 64: Next
    frmViewport.Cls
    picViewport.Cls
    fullUPDT = True
    Erase scChars, atChars
    gsView.FillBackground bCOLORS(CP, Abs(useBright))
End Sub

Sub INPUTs(ByRef VAR As Variant)
    'clean-up the variables
    VAR = "": Keys = "": PRINT_AT 23, 0, , , , String$(32, " ")
    Do 'we loop until the ENTER key is pressed
        If Len(Keys) Then
            If Asc(Right$(Keys, 1)) = 13 Then
                If Len(Keys) > 1 Then
                    VAR = Left$(Keys, Len(Keys) - 1)
                    Keys = Left$(Keys, Len(Keys) - 1)
                Else
                    VAR = " "
                End If
            End If
        End If
            If KeysL <> Keys Then
                If Len(Keys) Then
                    PRINT_AT 23, 0, , , , Format$(Keys, "!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
                Else
                    PRINT_AT 23, 0, , , , String$(32, " ")
                End If
                KeysL = Keys
            End If
        DoEvents
    Loop While VAR = ""
    PRINT_AT 23, 0, , , , String$(32, " ")
End Sub

Sub PAUSE(ByVal TIME As Long)
    Sleep TIME
End Sub

Function INKEY$()
    Dim VAR As String
    Keys = ""
    Do
        If Len(Keys) Then VAR = UCase$(Keys)
        DoEvents
    Loop While VAR = ""
    INKEY$ = VAR
End Function

Function INKEYS(ByVal Key As String) As Boolean
    If GetAsyncKeyState(Asc(Key)) Then
        DoEvents
        INKEYS = True
    End If
End Function

Sub NEWs()
Dim i As Long, j As Long, K As Long
'add some effects so it appears that we are restarting the spectrum
    PAPER 0: BORDER 0: Cls: PAUSE 500: BEEP 0.001, 750
    InitBasic
    PAPER CP: BORDER CB: INK CI: BRIGHT 0: Cls
    PRINT_AT 8, 0, , 2, , "        GONCHUKI SYSTEMS        "
    PRINT_AT 10, 0, , 1, , " ZX SPECTRUM BASIC CODE PORTER  "
    PRINT_AT 14, 0, , 4, , " COPYRIGHT " & Chr$(184) & " 2002 BY GONCHUKI   "
    PRINT_AT 16, 0, , 3, , "   E-MAIL: GONCHUKI@YAHOO.ES    "
    
    For K = 0 To 1
    For j = 0 To 7
        For i = 0 To 31
            PRINT_AT 23, i, , j, K, "*"
            PAUSE 5
        Next
    Next
    Next
    Cls
End Sub

Sub DATA(ParamArray DATAS())
Dim DATAstr As Variant
For Each DATAstr In DATAS
    cDATA.Add CLng(DATAstr)
Next
End Sub

Sub PLOT(ByVal X As Byte, ByVal y As Byte)
Dim pt As POINTAPI

y = 191 - y
SetPixel gsView.hDC, X, y, bCOLORS(CI, Abs(useBright))
MoveToEx gsView.hDC, X, y, pt

gsView.PaintTo picViewport.hDC: picViewport.Refresh
End Sub

Sub DRAW(ByVal X As Long, ByVal y As Long)
Dim oldPen As Long, hPen As Long
With gsView
    hPen = CreatePen(PS_SOLID, 1, CI)
    oldPen = SelectObject(.hDC, hPen)
    
    LineTo .hDC, X, y
    
    SelectObject .hDC, oldPen
    DeleteObject hPen
    
    .PaintTo picViewport.hDC: picViewport.Refresh
End With
End Sub

Sub BUILDUDG(ByVal AssignedCode As Byte)
'this is not a real Basic function, this is a replacement
'to the series of poke's needed to modify the UDG characters

Dim i As Long, j As Long, bFormat As Byte
Dim pPos As POINTAPI
If cDATA.Count >= 8 Then
pPos.X = (AssignedCode - ((AssignedCode \ &H20) * 32)) * 8: pPos.y = (AssignedCode \ &H20) * 8
    For j = 1 To 8
        bFormat = cDATA.Item(1)
        For i = 0 To 7
            If bFormat And (2 ^ i) Then
                SetPixel Chars.hDC, pPos.X + 7 - i, pPos.y, &H0
            End If
        Next
        pPos.y = pPos.y + 1
        cDATA.Remove 1
    Next
Else
    MsgBox "CAN'T BUILD UDG!" & vbCrLf & vbCrLf & "OUT OF DATAs", vbExclamation
End If
End Sub

Function UDG(ByVal repString As String) As String
'another replacement function, in a real spectrum you would
'use the number keys while in graphics mode... as we don't
'have any way of representing those characters, we must
'use this replacement function.
Dim tArray() As Byte, i As Long

If Len(repString) Then
    tArray = StrConv(UCase$(repString), vbFromUnicode)
    For i = 1 To Len(repString)
        UDG = UDG & Chr$(&H7F + tArray(i - 1))
    Next
    Erase tArray
End If
End Function

Function SCREENs(ByVal y As Byte, ByVal X As Byte) As String
    SCREENs = Chr$(scChars(y, X))
End Function

Function ATTR(ByVal y As Byte, ByVal X As Byte) As Byte
If y > 23 Or X > 31 Then Exit Function
    ATTR = atChars(y, X)
End Function

Function CODE(ByVal refStr As String) As Byte
    CODE = Asc(refStr)
End Function

Sub BEEP(ByVal D As Single, ByVal N As Long)
    Sound N, D * 1000
End Sub

