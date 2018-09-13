﻿Imports System.Drawing
Namespace Szunyi

    Public Class Number
        Public Shared Function GetRanges(vals() As Integer) As List(Of Point)
            If IsNothing(vals) = True Then Return New List(Of Point)
            Dim valsII As List(Of Integer) = vals.ToList
            valsII.Insert(0, 0)
            Dim out As New List(Of Point)
            For i1 = 0 To valsII.Count - 2
                out.Add(New Point(valsII(i1) + 1, valsII(i1 + 1)))
            Next
            Return out
        End Function

        Public Shared Function Which_Range(Ranges As List(Of Point), count As Integer) As String
            If Ranges.Count = 0 Then Return String.Empty
            Dim pt = From x In Ranges Where x.X <= count And x.Y >= count
            If pt.Count <> 0 Then
                Return pt.First.X & "-" & pt.First.Y
            Else
                Return "Mt_" & Ranges.Last.Y
            End If
        End Function
        Public Shared Function Get_Median(numbers As List(Of Double)) As Double
            Dim numberCount As Integer = numbers.Count
            Dim halfIndex As Integer = numbers.Count \ 2
            Dim sortedNumbers = numbers.OrderBy(Function(n) n)
            Dim median As Double
            If (numberCount Mod 2 = 0) Then
                median = (sortedNumbers.ElementAt(halfIndex) + sortedNumbers.ElementAt(halfIndex - 1)) / 2
            Else
                median = sortedNumbers.ElementAt(halfIndex)
            End If
            Return median
        End Function
        Public Shared Function Get_Median(numbers As List(Of Long)) As Double
            If numbers.Count = 0 Then Return -1
            Dim numberCount As Integer = numbers.Count
            Dim halfIndex As Integer = numbers.Count \ 2
            Dim sortedNumbers = numbers.OrderBy(Function(n) n)
            Dim median As Double
            If (numberCount Mod 2 = 0) Then
                median = (sortedNumbers.ElementAt(halfIndex) + sortedNumbers.ElementAt(halfIndex - 1)) / 2
            Else
                median = sortedNumbers.ElementAt(halfIndex)
            End If
            Return median
        End Function
        Public Shared Function Get_Median(numbers As List(Of Integer)) As Double
            Dim numberCount As Integer = numbers.Count
            Dim halfIndex As Integer = numbers.Count \ 2
            Dim sortedNumbers = numbers.OrderBy(Function(n) n)
            Dim median As Double
            If (numberCount Mod 2 = 0) Then
                median = (sortedNumbers.ElementAt(halfIndex) + sortedNumbers.ElementAt(halfIndex - 1)) / 2
            Else
                median = sortedNumbers.ElementAt(halfIndex)
            End If
            Return median
        End Function
    End Class

    Public Class Sum_Avarage
        Public Shared Function Convert_to_Long_List(x As List(Of Integer)) As List(Of Long)
            Dim out As New List(Of Long)
            For Each Item In x
                out.Add(Item)
            Next
            Return out
        End Function
        Public Shared Function StdDev(values As List(Of Integer)) As Double
            Dim ret As Double = 0
            Dim count As Integer = values.Count()
            If count > 1 Then
                'Compute the Average
                Dim avg As Double = values.Average()

                'Perform the Sum of (value-avg)^2
                Dim sum As Double = values.Sum(Function(d) (d - avg) * (d - avg))

                'Put it all together
                ret = System.Math.Sqrt(sum / count)
            End If
            Return ret
        End Function
        Public Shared Function StdDev(values As List(Of Double)) As Double
            Dim ret As Double = 0
            Dim count As Integer = values.Count()
            If count > 1 Then
                'Compute the Average
                Dim avg As Double = values.Average()

                'Perform the Sum of (value-avg)^2
                Dim sum As Double = values.Sum(Function(d) (d - avg) * (d - avg))

                'Put it all together
                ret = System.Math.Sqrt(sum / count)
            End If
            Return ret
        End Function
        Public Shared Function StdDev(values As List(Of Long)) As Double
            Dim ret As Double = 0
            Dim count As Integer = values.Count()
            If count > 1 Then
                'Compute the Average
                Dim avg As Double = values.Average()

                'Perform the Sum of (value-avg)^2
                Dim sum As Double = values.Sum(Function(d) (d - avg) * (d - avg))

                'Put it all together
                ret = System.Math.Sqrt(sum / count)
            End If
            Return ret
        End Function
        Public Shared Function Get_Sum(Values As Double(), Optional Start As Integer = -1, Optional [End] As Integer = -1) As Double
            If Start = -1 Then Start = 0
            If [End] = -1 Then [End] = Values.Count - 1
            Dim d As Double = 0
            For i1 = Start To [End]
                d += Values(i1)
            Next
            Return d
        End Function
        Public Shared Function Get_Sum_Above_ThresHold(Values As Double(), ThresHold As Double, Optional Start As Integer = -1, Optional [End] As Integer = -1) As Double
            If Start = -1 Then Start = 0
            If [End] = -1 Then [End] = Values.Count - 1
            Dim d As Double = 0
            For i1 = Start To [End]
                If Values(i1) >= ThresHold Then
                    d += Values(i1)
                End If

            Next
            Return d
        End Function
        Public Shared Function Get_Sum_Below_ThresHold(Values As Double(), ThresHold As Double, Optional Start As Integer = -1, Optional [End] As Integer = -1) As Double
            If Start = -1 Then Start = 0
            If [End] = -1 Then [End] = Values.Count - 1
            Dim d As Double = 0
            For i1 = Start To [End]
                If Values(i1) <= ThresHold Then
                    d += Values(i1)
                End If

            Next
            Return d
        End Function
        Public Shared Function Get_Trapezoidal_Integration_ThresHold(Values As Double(), ThresHold As Double) As Double
            Dim d As Double = 0
            Dim First As Double = Nothing
            Dim Last As Double
            For i1 = 0 To Values.Count - 1
                If Values(i1) >= ThresHold Then
                    d += Values(i1)
                    If IsNothing(First) = True Then First = Values(i1)
                    Last = Values(i1)
                End If

            Next
            d += First / 2
            d += Last / 2
            Return d
        End Function
        Public Shared Function Get_Trapezoidal_Integration(Values As Double()) As Double
            Dim d As Double = 0
            For i1 = 1 To Values.Count - 2
                d += Values(i1)
            Next
            d += Values(0) / 2
            d += Values(Values.Count - 1) / 2
            Return d
        End Function
        Public Shared Function Get_Trapezoidal_Integration(Values As Double(), Start As Integer, [End] As Integer) As Double
            Dim d As Double = 0
            For i1 = Start + 1 To [End] - 1
                d += Values(i1)
            Next
            d += Values(Start) / 2
            d += Values([End]) / 2
            Return d
        End Function

        Public Shared Function Get_Percents(interests As List(Of Long), totals As List(Of Long)) As List(Of Double)
            Dim out As New List(Of Double)
            For i1 = 0 To interests.Count - 1
                out.Add((interests(i1) / totals(i1) * 100))
            Next
            Return out
        End Function

        Public Shared Function Get_Percents(x() As Integer) As List(Of Double)
            Dim sum = x.Sum
            Dim out As New List(Of Double)
            For Each i In x
                out.Add((i / sum) * 100)
            Next
            Return out
        End Function
    End Class
End Namespace
