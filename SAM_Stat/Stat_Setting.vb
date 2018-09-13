Public Class Stat_Setting
    Public Property By_Files As Input_Description
    Public Property By_Org As Input_Description
    Public Property By_Read_Group As Input_Description
    Public Property By_Alignments As Input_Description

    Public Sub New()
        By_Files = New Input_Description("By Files",
                                                           Input_Description_Type.Boolean,
                                                           "Create Stat for every Files",
                                                           1, 100, 20, 20, 1, "True", "")
        By_Org = New Input_Description("By Organism",
                                                             Input_Description_Type.Boolean,
                                                             "Create Stat for every Organism from different files",
                                                             1, 200000, 1, 200000, 0, "True", "")
        By_Read_Group = New Input_Description("By Read Group",
                                                             Input_Description_Type.Boolean,
                                                             "Create Stat for every Read Group",
                                                             1, 200000, 1, 200000, 0, "True", "")
        By_Alignments = New Input_Description("Use All, only Best Alignments, or create both version",
                                                          Input_Description_Type.Selection,
                                                          "Use All Alignments, only Best Alignments, or create both version",
                                                          1, 100, 1, 100, 0, "Best|All|Both", "")



    End Sub
End Class
