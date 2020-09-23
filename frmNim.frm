VERSION 5.00
Begin VB.Form frmNim 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ZX-GAMES - a VB port from the old Spectrum Versions"
   ClientHeight    =   3360
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4350
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "Lucida Console"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmNim.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   ScaleHeight     =   224
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   290
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox picView 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      FontTransparent =   0   'False
      Height          =   2880
      Left            =   255
      ScaleHeight     =   192
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   256
      TabIndex        =   0
      Top             =   240
      Width           =   3840
   End
End
Attribute VB_Name = "frmNim"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private border_width As Long, title_height As Long

Private Sub Form_KeyPress(KeyAscii As Integer)
'this is the fastest way to implement the keyboard input
    modZXBasic.AddKey KeyAscii
End Sub

Private Sub Form_Resize()
If Me.WindowState = vbMinimized Then Exit Sub

border_width = (ScaleX(Me.width, vbTwips, vbPixels) - Me.ScaleWidth)
title_height = ScaleX(Me.Height, vbTwips, vbPixels) - (border_width \ 2) - Me.ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
Set frmNim = Nothing
End 'bad coding practice but must be done because of a possible pending INPUTs or INKEY$ statement
End Sub

Private Sub picView_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 123 Then
        If vRatio = 100 Then
            Me.Move Me.Left, Me.Top, ScaleX(290 * 2 + border_width, vbPixels, vbTwips), ScaleY(224 * 2 + title_height + (border_width \ 2), vbPixels, vbTwips)
            Me.Move (Screen.width - Me.width) \ 2, (Screen.Height - Me.Height) \ 2
            picView.Move picView.Left * 2, picView.Top * 2, picView.width * 2, picView.Height * 2
            vRatio = 200: fullUPDT = True: PRINT_AT 0, 0: fullUPDT = True
        Else
            Me.Move Me.Left, Me.Top, ScaleX(290 + border_width, vbPixels, vbTwips), ScaleY(224 + title_height + (border_width \ 2), vbPixels, vbTwips)
            Me.Move (Screen.width - Me.width) \ 2, (Screen.Height - Me.Height) \ 2
            picView.Move picView.Left \ 2, picView.Top \ 2, picView.width \ 2, picView.Height \ 2
            vRatio = 100: fullUPDT = True: PRINT_AT 0, 0: fullUPDT = True
        End If
    End If
End Sub

Private Sub picView_KeyPress(KeyAscii As Integer)
    modZXBasic.AddKey KeyAscii
End Sub
