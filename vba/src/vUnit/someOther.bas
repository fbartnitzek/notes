Attribute VB_Name = "someOther"
Option Explicit

Private Sub testShouldWork()
  Call assertEquals("hello2", "hello2")
End Sub

Private Sub testShouldFail()
  Call assertEquals("hello2", "world2")
End Sub

Private Sub testShouldAlsoWork()
  Call assertEquals("hello3", "hello3")
End Sub
