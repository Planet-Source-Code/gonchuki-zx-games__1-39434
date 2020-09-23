VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "VirtualDC"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

'********************************************************
'* VirtualDC - an easy to use and very fast DC manager  *
'*                                                      *
'* This is an oversimplified version of my great and yet*
'*  unveiled GSDIB graphics engine... use it as you wish*
'*  just don't remove this copyright notice, and give   *
'*  credit where its due. If you plan to charge money   *
'*  for your app, a free license would be apreciated... *
'*                                                      *
'********************************************************

Private Declare Function CreateSolidBrush Lib "gdi32" (ByVal crColor As Long) As Long
Private Declare Function FillRect Lib "user32" (ByVal hDC As Long, lpRect As RECT, ByVal hBrush As Long) As Long
Private Declare Function FrameRect Lib "user32" (ByVal hDC As Long, lpRect As RECT, ByVal hBrush As Long) As Long
Private Declare Function SetRect Lib "user32" (lpRect As RECT, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long

Private Declare Function SelectObject Lib "gdi32" (ByVal hDC As Long, ByVal hObject As Long) As Long
Private Declare Function DeleteObject Lib "gdi32" (ByVal hObject As Long) As Long

Private Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal X As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Private Declare Function StretchBlt Lib "gdi32" (ByVal hDC As Long, ByVal X As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal nSrcWidth As Long, ByVal nSrcHeight As Long, ByVal dwRop As Long) As Long
Private Declare Function CreateCompatibleDC Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function CreateDIBSection Lib "gdi32" (ByVal hDC As Long, pBitmapInfo As BITMAPINFOHEADER, ByVal un As Long, lplpVoid As Long, ByVal handle As Long, ByVal dw As Long) As Long
Private Declare Function DeleteDC Lib "gdi32" (ByVal hDC As Long) As Long
Private Declare Function GetObjectA Lib "gdi32" (ByVal hObject As Long, ByVal nCount As Long, lpObject As Any) As Long

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)
Private Declare Sub ZeroMemory Lib "kernel32" Alias "RtlZeroMemory" (Destination As Any, ByVal Length As Long)

Private Type RECT
        Left As Long
        Top As Long
        Right As Long
        Bottom As Long
End Type

Private Type BITMAPINFOHEADER
        biSize As Long
        biWidth As Long
        biHeight As Long
        biPlanes As Integer
        biBitCount As Integer
        biCompression As Long
        biSizeImage As Long
        biXPelsPerMeter As Long
        biYPelsPerMeter As Long
        biClrUsed As Long
        biClrImportant As Long
End Type

Private Type BITMAP
    bmType As Long
    bmWidth As Long
    bmHeight As Long
    bmWidthBytes As Long
    bmPlanes As Integer
    bmBitsPixel As Integer
    bmBits As Long
End Type

Private Type RGBTRIPLE
        rgbBlue As Byte
        rgbGreen As Byte
        rgbRed As Byte
End Type

Private Type BITMAPINFO
        bmiHeader As BITMAPINFOHEADER
        bmiColors As RGBTRIPLE
End Type

Private Type SAFEARRAY2
    cDims As Integer
    fFeatures As Integer
    cbElements As Long
    cLocks As Long
    pvData As Long
    CE0 As Long
    LB0 As Long
    CE1 As Long
    LB1 As Long
End Type

Private MyhDC As Long, BMI As BITMAPINFO, lpRGB As Long
Private hBMO As Long, hDIB As Long
Private sa As SAFEARRAY2, saPtr As Long

Public Sub Clone(ByVal pic As StdPicture)
'Clones a Picture object, you may use LoadPicture to
'directly create the DC and load it with the picrute
'without previously calling Create()

    Dim bmp As BITMAP, hDCt As Long, hBMOt As Long
    
    GetObjectA pic.handle, Len(bmp), bmp
    Create bmp.bmWidth, bmp.bmHeight
    hDCt = CreateCompatibleDC(MyhDC)
    hBMOt = SelectObject(hDCt, pic.handle)
    BitBlt MyhDC, 0, 0, BMI.bmiHeader.biWidth, BMI.bmiHeader.biHeight, hDCt, 0, 0, vbSrcCopy
    SelectObject hDCt, hBMOt
    DeleteDC hDCt
End Sub

Public Sub Create(ByVal W As Long, ByVal H As Long)
'Creates or resizes the current DIB
    Class_Terminate
    With BMI.bmiHeader
        .biSize = Len(BMI.bmiHeader)
        .biWidth = W
        .biHeight = H
        .biPlanes = 1
        .biBitCount = 24
        .biSizeImage = ((.biWidth * 3 + 3) And &HFFFFFFFC) * .biHeight
    End With
    MyhDC = CreateCompatibleDC(0)
    hDIB = CreateDIBSection(MyhDC, BMI.bmiHeader, 0, lpRGB, 0, 0)
    If hDIB Then
        hBMO = SelectObject(MyhDC, hDIB)
        Cls
    Else
        Err.Raise 825, , "DIB failed to create" & vbCrLf & "Resources may be low." & vbCrLf & "Please restart your system and try again"
    End If
'    SetStretchBltMode MyhDC, 3
End Sub

Public Sub CloneHDC(ByVal theHDC As Long, Optional ByVal dwRop As Long = vbSrcCopy, Optional ByVal X As Long = 0, Optional ByVal y As Long = 0)
'clones an entire DC or a selected area
'useful for image cropping/copying
    BitBlt MyhDC, 0, 0, BMI.bmiHeader.biWidth, BMI.bmiHeader.biHeight, theHDC, X, y, dwRop
End Sub

Private Sub Class_Terminate()
    If MyhDC Then
        If hBMO Then DeleteObject SelectObject(MyhDC, hBMO): hBMO = 0
        DeleteDC MyhDC: MyhDC = 0
    End If
End Sub

Public Sub DrawRectangle(ByVal X As Long, ByVal y As Long, ByVal width As Long, ByVal Height As Long, ByVal Color As Long, Optional ByVal Filled As Boolean = True)
Dim bRect As RECT
Dim hBrush As Long

SetRect bRect, X, y, X + width, y + Height
hBrush = CreateSolidBrush(Color)

If Filled Then
    Call FillRect(MyhDC, bRect, hBrush)
Else
    Call FrameRect(MyhDC, bRect, hBrush)
End If

Call DeleteObject(hBrush)
End Sub

Public Sub Cls()
    DrawRectangle 0, 0, BMI.bmiHeader.biWidth, BMI.bmiHeader.biHeight, 0
End Sub

Public Property Get hDC() As Long
    hDC = MyhDC
End Property

Public Sub PaintTo(ByVal toDC As Long, Optional ByVal atX As Long = 0, Optional ByVal atY As Long = 0, Optional ByVal dwRop As Long = vbSrcCopy)
'paints the entire picture to a given DC
    BitBlt toDC, atX, atY, BMI.bmiHeader.biWidth, BMI.bmiHeader.biHeight, MyhDC, 0, 0, dwRop
End Sub

Public Sub PaintArea(ByVal toDC As Long, ByVal fromX As Long, ByVal fromY As Long, ByVal width As Long, ByVal Height As Long, Optional ByVal atX As Long = 0, Optional ByVal atY As Long = 0, Optional ByVal dwRop As Long = vbSrcCopy)
'paints the selected area to a given DC
    BitBlt toDC, atX, atY, width, Height, MyhDC, fromX, fromY, dwRop
End Sub

'paints a resized image without changing the original picture
Public Sub PaintZoom(ByVal toDC As Long, ByVal atX As Long, ByVal atY As Long, ByVal Ratio As Long, Optional ByVal dwRop As Long = vbSrcCopy)
    StretchBlt toDC, atX, atY, BMI.bmiHeader.biWidth * (Ratio * 0.01), BMI.bmiHeader.biHeight * (Ratio * 0.01), MyhDC, 0, 0, BMI.bmiHeader.biWidth, BMI.bmiHeader.biHeight, dwRop
End Sub

Public Sub RepBlt(ByVal dibToBlend As VirtualDC, ByVal atX As Long, ByVal atY As Long, ByVal ColB As Long, ByVal ColW As Long)
Dim i As Long, j As Long
Dim sB() As Byte, sRB As Long
Dim bSB() As Byte, bRB As Long          'used for the blended dib
Dim FirstPos As Long, LastPos As Long   'precalculated offsets
Dim R1 As Long, G1 As Long, B1 As Long, R2 As Long, G2 As Long, B2 As Long
Dim curLine As Long
Dim jPlusLine As Long, iPlusJ As Long
'get the colors
B1 = (ColB And &HFF0000) \ 65536
G1 = (ColB And &HFF00&) \ 256
R1 = ColB And &HFF

B2 = (ColW And &HFF0000) \ 65536
G2 = (ColW And &HFF00&) \ 256
R2 = ColW And &HFF

'get our linear DIBs
sRB = Me.MapArray(sB)
bRB = dibToBlend.MapArray(bSB)

'set up the variables
FirstPos = BMI.bmiHeader.biSizeImage - (atY + 8) * sRB + atX * 3
LastPos = FirstPos + (8 * sRB) - 3
If FirstPos < 0 Then FirstPos = 0: If LastPos > BMI.bmiHeader.biSizeImage Then LastPos = BMI.bmiHeader.biSizeImage

    For i = FirstPos To LastPos Step sRB
        For j = 0 To 21 Step 3
        jPlusLine = j + curLine: iPlusJ = i + j
        'check if pixel is black or anything else
            If (CLng(bSB(jPlusLine + 2)) + bSB(jPlusLine + 1) + bSB(jPlusLine)) = 0 Then
                sB(iPlusJ) = B1
                sB(iPlusJ + 1) = G1
                sB(iPlusJ + 2) = R1
            Else
                sB(iPlusJ) = B2
                sB(iPlusJ + 1) = G2
                sB(iPlusJ + 2) = R2
            End If
        Next
        curLine = curLine + bRB
    Next

End Sub

Friend Function MapArray(ByRef a As Variant) As Long
    sa.cDims = 1
    sa.cbElements = 1
    sa.pvData = lpRGB
    sa.CE0 = BMI.bmiHeader.biSizeImage

    CopyMemory saPtr, ByVal VarPtr(a) + 8, 4
    CopyMemory ByVal saPtr, VarPtr(sa), 4
    MapArray = BMI.bmiHeader.biSizeImage \ BMI.bmiHeader.biHeight
End Function

Public Sub FillBackground(ByVal Color As Long)
    DrawRectangle 0, 0, BMI.bmiHeader.biWidth, BMI.bmiHeader.biHeight, Color, True
End Sub

