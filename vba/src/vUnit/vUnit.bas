Attribute VB_Name = "vUnit"
Option Explicit
Dim lFailed As Long, lAll As Long
' simple vba unit testing framework
'add a reference to Microsoft Visual Basic for Applications Extensibility 5.3 (or later)
' excel options: http://stackoverflow.com/questions/5300770/how-to-check-from-net-code-whether-trust-access-to-the-vba-project-object-mode

Sub assertEquals(ByVal sExpected, ByVal sActual)
  If sExpected <> sActual Then
    lFailed = lFailed + 1
    Debug.Print "  => assertEquals failed: should be '" & sExpected & "' but was '" & sActual & "'"
  End If
  lAll = lAll + 1
End Sub

Sub testAll()
  Dim comp As VBComponent, lRow As Long, sName As String, lKind As Long
  
  lAll = 0
  lFailed = 0
  For Each comp In ThisWorkbook.VBProject.VBComponents
    With comp.CodeModule
      lRow = .CountOfDeclarationLines + 1
      Do While lRow < .CountOfLines
        sName = .ProcOfLine(lRow, lKind)
'        Debug.Print " - " & comp.Name & "." & sName & " (kind: " & lKind & ", lines: " & .ProcCountLines(sName, lKind) & ")"
        
        lRow = .ProcStartLine(sName, lKind) + .ProcCountLines(sName, lKind) + 1
        If Left(sName, 4) = "test" And sName <> "testAll" Then
          Call runTest(comp, sName)
        End If
      Loop
    End With
  Next
  
  If lFailed = 0 Then
    Debug.Print "===> all tests done: " & lAll & " / " & lAll
  Else
    Debug.Print "===> failing asserts: " & lFailed & " / " & lAll
  End If

End Sub

Private Function runTest(ByRef comp, ByVal sTestMethodName)
  Debug.Print "- testing " & comp.Name & "." & sTestMethodName
    'some.printf()
  Call Application.Run(comp.Name & "." & sTestMethodName)
End Function
