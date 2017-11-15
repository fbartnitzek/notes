VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "clsIdEntry"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Public dbId As String
Public dbName As String
Public color As String
Public Needed As Boolean
Public xPath As String
Public sheetName As String
Public row As Long

'Sub create(sDbId As String, sDbName As String, sColor As String, _
'        bNeeded As Boolean, sXPath As String, sSheetName As String, lRow As Long)
'  dbId = sDbId
'  dbName = sDbName
'  color = sColor
'  Needed = bNeeded
'  xPath = sXPath
'  sheetName = sSheetName
'  row = lRow
'End Sub

'Public Function createIdEntry(sDbId As String, sDbName As String, sColor As String, _
'        bNeeded As Boolean, sXPath As String, sSheetName As String, lRow As Long) As clsIdEntry
'  Dim oId As New clsIdEntry
'  Call oId.create(sDbId, sDbName, sColor, bNeeded, sXPath, sSheetName, lRow)
'  Set createIdEntry = oId
'End Function

