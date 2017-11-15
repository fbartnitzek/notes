Attribute VB_Name = "CollectionTest"
Option Explicit

Sub testCollection()
  Dim cIds As Collection
  Dim oId As clsIdEntry

  Set cIds = generateIds("someSheet", 2)
  
  Call assertEquals(2, cIds.Count)
  Call assertEquals("myTable", cIds.Item(1).dbName)
  
  For Each oId In cIds
    Call assertEquals("someSheet", oId.sheetName)
  Next oId
End Sub

Sub testComplicatedCollection()
  Dim cSheets As New Collection
  Dim oSheet As clsSheetEntry
  
  cSheets.Add createSheetEntry("someSheet", generateIds("someSheet", 5))
  cSheets.Add createSheetEntry("someSheet2", generateIds("someSheet2", 3))
  cSheets.Add createSheetEntry("someSheet3", generateIds("someSheet3", 7))
  
  Call assertEquals(3, cSheets.Count)
  Call assertEquals(7, cSheets.Item(3).cIds.Count)
  Call assertEquals("someSheet2", cSheets.Item(2).name)
End Sub

Sub testUpdateComplicatedCollection()
  Dim cSheets As New Collection
  Dim oSheet As clsSheetEntry, oIdEntry As clsIdEntry
  
  cSheets.Add createSheetEntry("someSheet", generateIds("someSheet", 5))
  cSheets.Add createSheetEntry("someSheet2", generateIds("someSheet2", 3))
  cSheets.Add createSheetEntry("someSheet3", generateIds("someSheet3", 7))

  For Each oSheet In cSheets
    oSheet.name = oSheet.name & "-other"
    For Each oIdEntry In oSheet.cIds
      oIdEntry.color = "magenta"
    Next oIdEntry
  Next oSheet
  
  Call assertEquals("someSheet2-other", cSheets.Item(2).name)
  Call assertEquals("magenta", cSheets.Item(2).cIds.Item(2).color)
End Sub

Private Function generateIds(ByVal sSheet As String, ByVal lNum As Long) As Collection
  Dim cIds As New Collection
  Dim oId As clsIdEntry
  Dim l As Long
  
  For l = 1 To lNum
    cIds.Add createIdEntry("dbid" & l, "myTable", "red", True, "/some/path", "someSheet", 123 + l)
  Next l
  
  Set generateIds = cIds
End Function
