Imports Bio.IO.GenBank
Imports Bio.IO.SAM
Namespace Szunyi.Location
    Public Class Common
        Public Shared LociBuilder As New Bio.IO.GenBank.LocationBuilder


#Region "Get_Locations "
        Public Shared Function Get_All_Intron_Location(Loci As ILocation) As List(Of ILocation)
            Dim exons = Get_All_Exon_Location(Loci)
            If exons.Count = 0 Then Return New List(Of ILocation)
            Dim out As New List(Of ILocation)
            For i1 = 0 To exons.Count - 2
                If exons(i1).LocationEnd > exons(i1 + 1).LocationStart Then
                    Dim ald As Int16 = 54
                End If
                If exons(i1).Operator = LocationOperator.Complement Then
                    out.Add(LociBuilder.GetLocation("complement(" & exons(i1).LocationEnd + 1 & ".." & exons(i1 + 1).LocationStart - 1 & ")"))
                    out.Last.Accession = exons.First.Accession
                Else
                    out.Add(LociBuilder.GetLocation(exons(i1).LocationEnd + 1 & ".." & exons(i1 + 1).LocationStart - 1))
                    out.Last.Accession = exons.First.Accession
                End If

            Next

            Return out
        End Function
        Public Shared Function Get_All_Exons_Location(Locis As List(Of Bio.IO.GenBank.ILocation)) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In Locis
                out.AddRange(Get_All_Exon_Location(Loci))
            Next
            Return out
        End Function
        Public Shared Function Get_All_Exons_Location(Locis As List(Of Bio.IO.GenBank.Location)) As List(Of Bio.IO.GenBank.ILocation)
            Dim out As New List(Of Bio.IO.GenBank.ILocation)
            For Each Loci In Locis
                out.AddRange(Get_All_Exon_Location(Loci))
            Next
            Return out
        End Function

        Public Shared Function Get_All_Exon_Location(Loci As Bio.IO.GenBank.ILocation) As List(Of Bio.IO.GenBank.ILocation)
            If IsNothing(Loci) = True Then Return Nothing
            Dim out As New List(Of ILocation)
            If Loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If
            If Loci.Operator <> LocationOperator.Complement Then ' no complement join
                For Each subL In Loci.SubLocations
                    subL.Accession = Loci.Accession
                Next
                Return Loci.SubLocations
            End If
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In Loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                    out.Last.Accession = Loci.Accession
                Next
                Return out
            Else ' complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If

        End Function
        Public Shared Function Get_All_Exon_Location(Loci As Bio.IO.GenBank.Location) As List(Of Bio.IO.GenBank.ILocation)
            If IsNothing(Loci) = True Then Return Nothing
            Dim out As New List(Of ILocation)
            If Loci.SubLocations.Count = 0 Then ' No complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If
            If Loci.Operator <> LocationOperator.Complement Then ' no complement join
                For Each subL In Loci.SubLocations
                    subL.Accession = Loci.Accession
                Next
                Return Loci.SubLocations
            End If
            If Loci.SubLocations.First.Operator = LocationOperator.Join Then ' complement join
                For Each subloci As Bio.IO.GenBank.Location In Loci.SubLocations.First.SubLocations
                    Dim s = "complement(" & LociBuilder.GetLocationString(subloci) & ")"
                    out.Add(LociBuilder.GetLocation(s))
                    out.Last.Accession = Loci.Accession
                Next
                Return out
            Else ' complement no join
                out.Add(Loci)
                out.Last.Accession = Loci.Accession
                Return out
            End If

        End Function

        Public Shared Function GetLocation(SAMs As List(Of SAMAlignedSequence)) As List(Of ILocation)
            Dim Out As New List(Of ILocation)
            Dim x = Szunyi.Alignment.Own_Al_Helper.Get_List(SAMs)
            Return GetLocation(x)
        End Function
        Public Shared Function GetLocation(x As List(Of Alignment.Own_Al)) As List(Of ILocation)
            Dim out As New List(Of ILocation)
            For Each Item In x
                out.Add(GetLocation(Item))
            Next
            Return out
        End Function
        Public Shared Function GetLocation(SAM As SAMAlignedSequence) As ILocation
            Dim x1 As New Alignment.Own_Al(SAM)

            Return GetLocation(x1)
        End Function
        Public Shared Function GetLocation(st As Integer, endy As Integer, IsC As Boolean) As ILocation
            Dim s As Integer = st
            Dim e As Integer = endy
            If e < s Then
                Dim tmp = e
                e = s
                s = tmp
            End If
            If IsC = True Then
                If s < e Then

                End If
                Return LociBuilder.GetLocation("complement(" & s & ".." & e & ")")
            Else
                Return LociBuilder.GetLocation(s & ".." & e)
            End If
        End Function
        Public Shared Function GetLocation(st As Integer, endy As Integer) As ILocation
            Dim s As Integer = st
            Dim e As Integer = endy
            If e < s Then
                Dim tmp = e
                e = s
                s = tmp
            End If

            Return LociBuilder.GetLocation(s & ".." & e)

        End Function
        Public Shared Function GetLocation(Own_AL As Szunyi.Alignment.Own_Al) As ILocation
            Dim Introns = From x In Own_AL.Parts Where x.Type = Own_AL.Type.Intron
            Dim RefStart As Integer = 0
            If Own_AL.Parts.First.Type = Own_AL.Type.Soft_Clip Then
                RefStart = Own_AL.Parts(1).Ref_Start
            Else
                RefStart = Own_AL.Parts.First.Ref_Start
            End If
            Dim RefEnd As Integer = 0
            If Own_AL.Parts.Last.Type = Own_AL.Type.Soft_Clip Then
                RefEnd = Own_AL.Parts(Own_AL.Parts.Count - 2).Ref_End
            Else
                RefEnd = Own_AL.Parts.Last.Ref_End
            End If

            Dim IsReverse As Boolean = False
            If Own_AL.Sam.Flag = SAMFlags.QueryOnReverseStrand Then
                IsReverse = True
            End If

            If Introns.Count = 0 Then
                Dim l = GetLocation(RefStart, RefEnd, IsReverse)
                l.Accession = Own_AL.Sam.QName
                Return l
            Else
                Dim l = GetLocation(RefStart, RefEnd, IsReverse)
                l.Accession = Own_AL.Sam.QName
                Dim Exons As New List(Of ILocation)
                Exons.Add(GetLocation(RefStart, Introns(0).Ref_Start - 1))
                For i1 = 0 To Introns.Count - 2
                    Exons.Add(GetLocation(Introns(i1).Ref_End + 1, Introns(i1 + 1).Ref_Start - 1))
                Next
                Exons.Add(GetLocation(Introns.Last.Ref_End + 1, RefEnd))
                Exons = (From x In Exons Where x.LocationEnd - x.LocationStart > 0).ToList
                If IsReverse = True Then
                    l.SubLocations.First.Operator = LocationOperator.Join
                    l.SubLocations.First.IsComplementer = True
                    l.SubLocations.First.SubLocations.AddRange(Exons)
                Else
                    l.Operator = LocationOperator.Join
                    l.SubLocations.AddRange(Exons)
                End If

                Return l
            End If

        End Function

        Public Shared Function Get_Length(Feats As List(Of ILocation)) As List(Of Integer)
            Dim out As New List(Of Integer)
            For Each f In Feats
                out.Add(Get_Length(f))
            Next
            Return out
        End Function

        Public Shared Function Get_Length(Feat As ILocation) As Integer
            Dim Exons = Get_All_Exon_Location(Feat)
            Dim res = (From x In Exons Select x.LocationEnd - x.LocationStart + 1).Sum
            Return res
        End Function
#End Region

    End Class
End Namespace
