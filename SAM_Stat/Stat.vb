Imports System.IO
Imports Bio.IO.SAM
Imports SAM_Stat.Szunyi.Alignment

Namespace Szunyi.SAM_BAM


    Public Class Stat

        Dim Setting As Stat_Setting
        Dim Bam_Files As List(Of FileInfo)
        Public Property By_Files As New Dictionary(Of FileInfo, List(Of Simple_Stat))
        Public Property By_Org As New Dictionary(Of String, List(Of Simple_Stat))
        Public Property By_Read_Group As New Dictionary(Of String, List(Of Simple_Stat))
        Public Property By_Al_Type As New Dictionary(Of String, List(Of Simple_Stat))
        Public Sub New()

        End Sub
        Public Function DoIt(x As Stat_Setting, Files As List(Of FileInfo)) As String
            If Files.Count = 0 Then Return String.Empty
            Setting = x
            Bam_Files = Files
            Create_Dictionaries()
            Do_It()
            Dim str As New System.Text.StringBuilder
            str.Append(Get_Header).AppendLine()
            If Setting.By_Files.Default_Value = 1 Then
                Dim All_Reads As New List(Of Simple_Stat)
                Dim Genome_Length As Long
                For Each Item In By_Files
                    Genome_Length = Szunyi.SAM_BAM.Headers.Get_Organism_Length(Item.Key)
                    For Each AL In By_Al_Type
                        If AL.Value.Count <> 0 Then

                            Dim res = Combine(Item.Value, AL.Value)
                            All_Reads.AddRange(res)
                            str.Append(Get_Text(Item.Key.FullName, res, AL.Key, Genome_Length))
                            str.AppendLine()
                        End If

                    Next
                    str.AppendLine()
                Next
                For Each AL In By_Al_Type
                    If AL.Value.Count <> 0 Then
                        Dim Cumulative = Combine(AL.Value, All_Reads)
                        str.Append(Get_Text("All_File", Cumulative, AL.Key, Genome_Length))
                        str.AppendLine()
                    End If
                Next
                Genome_Length = Szunyi.SAM_BAM.Headers.Get_Organism_Length(Files.First)
                Dim RGs = Szunyi.SAM_BAM.Headers.Get_Read_Groups(Files)
                For Each Item In Me.By_Read_Group
                    If Item.Value.Count <> 0 Then
                        For Each AL In By_Al_Type
                            If AL.Value.Count <> 0 Then

                                Dim res = Combine(Item.Value, AL.Value)
                                All_Reads.AddRange(res)
                                For Each sg In RGs
                                    Dim tmp = From x1 In sg.Tags Where x1.Tag = "ID" And x1.Value = Item.Key
                                    If tmp.Count > 0 Then
                                        Dim kj = (From x4 In sg.Tags Where x4.Tag = "LB" Select x4.Value).First
                                        str.Append(Get_Text(kj, res, AL.Key, Genome_Length))
                                        str.AppendLine()
                                    End If

                                Next


                            End If

                        Next
                        str.AppendLine()
                    End If
                Next
            End If
            Return str.ToString

        End Function
        Private Function Get_Text(Name As String, res As List(Of Simple_Stat), Key As String, Genome_Length As Long) As String
            Dim str As New System.Text.StringBuilder
            str.Append(Name).Append(vbTab).Append(Key).Append(vbTab)
            str.Append(res.Count).Append(vbTab)
            ' Introns
            str.Append(Szunyi.Alignment.Own_Al_Helper.Properties.Get_Reads_Sum_wIntrons(res)).Append(vbTab)
            Dim I = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Avarage_and_SD_Intron(res) ' Length
            str.Append(I.Avarage).Append(vbTab)
            str.Append(I.SD).Append(vbTab)
            ' Exons
            str.Append(Szunyi.Alignment.Own_Al_Helper.Properties.Get_Exons_Per_Mappings(res)).Append(vbTab)
            Dim E = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Avarage_and_SD_Exon(res)
            str.Append(E.Avarage).Append(vbTab)
            str.Append(E.SD).Append(vbTab)
            'Reads Median, avr, SD
            Dim Read_Lengths = (From x1 In res Select x1.Read_Length).ToList
            str.Append(Szunyi.Number.Get_Median(Read_Lengths)).Append(vbTab)

            Dim Aligned_Read_Lengths = (From x1 In res Select x1.Aligned_Read_Length).ToList
            str.Append(Szunyi.Number.Get_Median(Aligned_Read_Lengths)).Append(vbTab)
            'Read Lengths
            Dim RL = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Avarage_and_SD_Read(res)
            str.Append(RL.Avarage).Append(vbTab)
            str.Append(RL.SD).Append(vbTab)
            str.Append(RL.SEM).Append(vbTab)
            'Aligned Read Lengths
            Dim ARL = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Avarage_and_SD_Aligned_Read(res)
            str.Append(ARL.Avarage).Append(vbTab)
            str.Append(ARL.SD).Append(vbTab)
            str.Append(ARL.SEM).Append(vbTab)
            Dim Dels = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Percents_and_Sd_Deletions(res)
            str.Append(Dels.Avarage).Append(vbTab)
            str.Append(Dels.SD).Append(vbTab)
            str.Append(Dels.SEM).Append(vbTab)

            Dim Ins = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Percents_and_Sd_Insertion(res)
            str.Append(Ins.Avarage).Append(vbTab)
            str.Append(Ins.SD).Append(vbTab)
            str.Append(Ins.SEM).Append(vbTab)

            Dim Match = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Percents_and_Sd_Match(res)
            str.Append(Match.Avarage).Append(vbTab)
            str.Append(Match.sd).Append(vbTab)
            str.Append(Match.SEM).Append(vbTab)


            Dim MisMatch = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Percents_and_Sd_MisMatch(res)
            str.Append(MisMatch.Avarage).Append(vbTab)
            str.Append(MisMatch.SD).Append(vbTab)
            str.Append(MisMatch.SEM).Append(vbTab)

            Dim S = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Percents_and_Sd_S(res)
            str.Append(S.Avarage).Append(vbTab)
            str.Append(S.SD).Append(vbTab)

            str.Append(ARL.SUM / Genome_Length)

            Return str.ToString
        End Function
        Private Function Coverage()

        End Function
        Private Function Combine(l1 As List(Of Simple_Stat), l2 As List(Of Simple_Stat)) As List(Of Simple_Stat)
            Dim c As New Stat_Comparer
            Dim Out As New List(Of Simple_Stat)
            l2.Sort(c)
            For Each Item In l1
                Dim Index = l2.BinarySearch(Item, c)
                If Index >= 0 Then
                    Out.Add(Item)
                Else
                    Dim tt As Int16 = 43
                End If
            Next
            Return Out
        End Function

        Private Function Get_Header()
            Dim str As New System.Text.StringBuilder
            str.Append("FileName").Append(vbTab)
            str.Append("Mapping Type").Append(vbTab)
            str.Append("Nof Mappings").Append(vbTab)

            str.Append("Nof Mappings Has Introns").Append(vbTab)
            str.Append("Avr. Intron Length").Append(vbTab)
            str.Append("Avr. Intron Length SD").Append(vbTab)

            str.Append("Nof Exons Per Mapping").Append(vbTab)
            str.Append("Avr. Exon Length").Append(vbTab)
            str.Append("Avr. Exon Length SD").Append(vbTab)

            str.Append("Median Read Length").Append(vbTab)
            str.Append("Median Aligned Read Length").Append(vbTab)

            str.Append("Avr. Read Length").Append(vbTab)
            str.Append("Avr. Read Length SD").Append(vbTab)
            str.Append("Avr. Read Length SE").Append(vbTab)

            str.Append("Avr. Aligned Read Length").Append(vbTab)
            str.Append("Avr. Aligned Read Length SD").Append(vbTab)
            str.Append("Avr. Aligned Read Length SE").Append(vbTab)

            str.Append("Avr. Deletion %").Append(vbTab)
            str.Append("Avr. Deletion % SD").Append(vbTab)
            str.Append("Avr. Deletion % SE").Append(vbTab)

            str.Append("Avr. Insertion %").Append(vbTab)
            str.Append("Avr. Insertion % SD").Append(vbTab)
            str.Append("Avr. Insertion % SE").Append(vbTab)

            str.Append("Avr. Match %").Append(vbTab)
            str.Append("Avr. Match % SD").Append(vbTab)
            str.Append("Avr. Match % SE").Append(vbTab)

            str.Append("Avr. MisMatch %").Append(vbTab)
            str.Append("Avr. MisMatch % SD").Append(vbTab)
            str.Append("Avr. MisMatch % SE").Append(vbTab)

            str.Append("Avr. Soft Clip Lengths").Append(vbTab)
            str.Append("Avr. Soft Clip Lengths SD").Append(vbTab)
            str.Append("Coverage").Append(vbTab)
            Return str.ToString



            ' Nof Read,Nof Mapping,Median Read Length,Avr. Read Length,Avr. Read Length SD,Median Aligned Read Length,Avr. Aligned Read Length,Avr. Aligned Read Length SD,Deleteion %,Deleteion % SD,Insertion %,Insertion % SD,Match%,Match% SD,MisMatch%,MisMatch% SD,Coverage

        End Function
        Private Sub Do_It()
            Dim Index As Long = 0
            For Each File In Me.Bam_Files
                Dim current As New List(Of Simple_Stat)
                Dim Header = Szunyi.SAM_BAM.Headers.Get_Header(File)
                For Each SAM In Import.Parse(File)

                    If SAM.Flag <> SAMFlags.UnmappedQuery Then
                        Index += 1
                        If IsNothing(SAM.QuerySequence) = False Then
                            Dim x1 As New Simple_Stat(SAM, Index)
                            If Me.Setting.By_Files.Default_Value = 1 Then ' Ha Selected
                                Me.By_Files(File).Add(x1)
                            End If
                            If Me.Setting.By_Org.Default_Value = 1 Then ' Ha Selected
                                Me.By_Org(SAM.RName).Add(x1)
                            End If
                            If Me.Setting.By_Read_Group.Default_Value = 1 Then ' Ha Selected
                                Dim GroupID = Szunyi.SAM_BAM.BAM_Optional_Filed_Manipulation.Get_Read_Group_ID(SAM)
                                If GroupID <> String.Empty Then
                                    If Me.By_Read_Group.ContainsKey(GroupID) = False Then Me.By_Read_Group.Add(GroupID, New List(Of Simple_Stat))
                                    Me.By_Read_Group(GroupID).Add(x1)
                                End If
                            End If
                            If Me.Setting.By_Alignments.Selected_Value = "All" Or Me.Setting.By_Alignments.Selected_Value = "Both" Then
                                By_Al_Type("All").Add(x1)
                            End If
                            current.Add(x1)
                            End If
                        End If
                Next
                If Me.Setting.By_Alignments.Selected_Value = "Best" Or Me.Setting.By_Alignments.Selected_Value = "Both" Then
                    For Each i In ByGroupID(current)
                        Me.By_Al_Type("Best").Add(i.First)
                    Next

                End If
            Next

        End Sub
        Private Iterator Function ByGroupID(current As List(Of Simple_Stat)) As IEnumerable(Of List(Of Simple_Stat))
            Dim r = From x In current Group By x.Read_ID Into Group

            For Each gr In r
                Dim Best = From k In gr.Group Order By k.Match.Sum Descending
                Yield Best.ToList
            Next
        End Function

        Private Sub Create_Dictionaries()
            For Each File In Bam_Files
                By_Files.Add(File, New List(Of Simple_Stat))
            Next
            Dim Ref_Seq_IDs = Szunyi.SAM_BAM.Headers.Get_Reference_SeqIDS(Bam_Files)
            For Each ref_Seq_ID In Ref_Seq_IDs
                By_Org.Add(ref_Seq_ID, New List(Of Simple_Stat))
            Next
            Dim RGs = Szunyi.SAM_BAM.Headers.Get_Read_Groups_IDs(Me.Bam_Files)
            For Each rg In RGs
                By_Read_Group.Add(rg, New List(Of Simple_Stat))
            Next

            By_Al_Type.Add("All", New List(Of Simple_Stat))
            By_Al_Type.Add("Best", New List(Of Simple_Stat))
            By_Al_Type.Add("Both", New List(Of Simple_Stat))

        End Sub
    End Class
    Public Class Stat_Comparer
        Implements IComparer(Of Simple_Stat)

        Public Function Compare(x As Simple_Stat, y As Simple_Stat) As Integer Implements IComparer(Of Simple_Stat).Compare
            Return x.ID.CompareTo(y.ID)
        End Function
    End Class
    Public Class Simple_Stat
        Public Property ID As Long
        Public Property Read_ID As String
        Public Property Read_Length As Long '
        Public Property Aligned_Read_Length As Long ' I + M + MM
        Public Property Nof_Exon As Long
        Public Property Exon_Lengths As New List(Of Long)
        Public Property Nof_Intron As Long
        Public Property Intron_lengths As New List(Of Long)
        Public Property Alignment_Length As Long ' I+D+M+MM
        Public Property Aligned_Ref_Length As Long ' D + M +MM
        Public Property Soft_Clip As Long 'S
        Public Property Insertions As Nof_Sum_Avarage
        Public Property Deletions As Nof_Sum_Avarage
        Public Property MisMatch As Nof_Sum_Avarage
        Public Property Match As Nof_Sum_Avarage

        Public Property Sam As SAMAlignedSequence

        Public Sub New(sam As SAMAlignedSequence, Index As Long)
            Me.Sam = sam
            Me.ID = Index
            Me.Read_ID = sam.QName
            Me.Read_Length = sam.QuerySequence.Count
            Dim x1 As New Szunyi.Alignment.Own_Al(sam)
            Me.Aligned_Read_Length = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Aligned_Read_Length(x1) ' I M MM
            Me.Aligned_Ref_Length = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Aligned_Ref_Length(x1) ' D M MM
            Me.Alignment_Length = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Alignment_Length(x1) ' D, I, M,MM
            Me.Soft_Clip = Szunyi.Alignment.Own_Al_Helper.Properties.Get_S(x1)
            Dim l = Szunyi.Location.Common.GetLocation(x1)

            Dim exons = Szunyi.Location.Common.Get_All_Exon_Location(l)
            Dim Lengths = Szunyi.Location.Common.Get_Length(exons)
            Dim Long_Lengths = Sum_Avarage.Convert_to_Long_List(Lengths)
            Me.Exon_Lengths.AddRange(Long_Lengths)

            Me.Nof_Exon = exons.Count

            Dim Introns = Szunyi.Location.Common.Get_All_Intron_Location(l)


            Lengths = Szunyi.Location.Common.Get_Length(Introns)
            Long_Lengths = Sum_Avarage.Convert_to_Long_List(Lengths)
            Me.Intron_lengths.AddRange(Long_Lengths)

            Me.Nof_Intron = Intron_lengths.Count

            Me.Insertions = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Summary(x1, Szunyi.Alignment.Own_Al.Type.Insertion, Me.Alignment_Length)
            Me.Deletions = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Summary(x1, Szunyi.Alignment.Own_Al.Type.Deletion, Me.Alignment_Length)
            Me.Match = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Summary(x1, Szunyi.Alignment.Own_Al.Type.Match, Me.Alignment_Length)
            Me.MisMatch = Szunyi.Alignment.Own_Al_Helper.Properties.Get_Summary(x1, Szunyi.Alignment.Own_Al.Type.MisMatch, Me.Alignment_Length)
            If Me.Insertions.Sum + Me.MisMatch.Sum + Me.Match.Sum + Me.Deletions.Sum <> Me.Alignment_Length Then
                Dim kj As Int16 = 65
            End If
            Me.Sam = Nothing

        End Sub

    End Class

    Public Class Nof_Sum_Avarage

        Private aligned_Length As Integer

        Public Sub New(type As Own_Al.Type, Items As IEnumerable(Of Own_Al_Single_Part), aligned_Length As Integer)
            Me.Type = type

            Me.aligned_Length = aligned_Length
            Me.Nof = Items.Count
            Me.Sum = (From x In Items Select x.Length).Sum
            Me.Sum_per_Nof = Me.Sum / Me.Nof
            Me.Norm_Nof = (100 / aligned_Length) * Me.Nof
            Me.Norm_Sum = (100 / aligned_Length) * Me.Sum
        End Sub

        Public Property Nof As Integer
        Public Property Sum As Integer
        Public Property Sum_per_Nof As Double
        Public Property Norm_Nof As Double
        Public Property Norm_Sum As Double
        Public Property Type As Szunyi.Alignment.Own_Al.Type

    End Class


End Namespace