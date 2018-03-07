Attribute VB_Name = "ClassFactory"
Option Explicit

' no constructor in vba: method in class-module cannot create and return new entity...

Public Function createIdEntry(sDbId As String, sDbName As String, sColor As String, _
        bNeeded As Boolean, sXPath As String, sSheetName As String, lRow As Long) As clsIdEntry
  Dim oId As New clsIdEntry
  
  oId.dbId = sDbId
  oId.dbName = sDbName
  oId.color = sColor
  oId.Needed = bNeeded
  oId.xPath = sXPath
  oId.sheetName = sSheetName
  oId.row = lRow
  
  Set createIdEntry = oId
End Function

Public Function createSheetEntry(sName As String, collIds As Collection) As clsSheetEntry
  Dim oId As New clsSheetEntry

  oId.name = sName
  Set oId.cIds = collIds
  
  Set createSheetEntry = oId
End Function

