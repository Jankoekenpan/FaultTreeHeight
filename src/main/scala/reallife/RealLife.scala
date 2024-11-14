package reallife // or is this just fantasy?

object RealLife {

    def main(args: Array[String]): Unit = {



    }

}

object AircraftRunwayExcursionAccidents {
    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13 = 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18
    val X19 = 19
    val X20 = 20
    val X21 = 21

    val T = 0

    final val M1 = 22
    final val M2 = 23
    final val M3 = 24
    final val M4 = 25
    final val M5 = 26
    final val M6 = 27
    final val M7 = 28
    final val M8 = 29
    final val M9 = 30
    final val M10 = 31
    final val M11 = 32
    final val M12 = 33

    final val p1 = 6E-4
    final val p2 = 2E-5
    final val p3 = 1E-4
    final val p4 = 1E-4
    final val p5 = 3E-4
    final val p6 = 2E-4
    final val p7 = 2E-4
    final val p8 = 2E-4
    final val p9 = 1E-4
    final val p10 = 2E-4
    final val p11 = 3E-5
    final val p12 = 5E-5
    final val p13 = 1E-4
    final val p14 = 5E-5
    final val p15 = 2E-5
    final val p16 = 2.5E-4
    final val p17 = 2.5E-4
    final val p18 = 3E-4
    final val p19 = 3E-5
    final val p20 = 3E-5
    final val p21 = 3E-5

    val FT: FaultTree = AndEvent(T, Seq(
        OrEvent(M1, Seq(
            OrEvent(M3, Seq(
                BasicEvent(X1, p1),
                BasicEvent(X2, p2)
            )),
            OrEvent(M4, Seq(
                OrEvent(M7, Seq(
                    BasicEvent(X7, p7),
                    BasicEvent(X8, p8),
                    BasicEvent(X9, p9)
                )),
                BasicEvent(X3, p3)
            ))
        )),
        OrEvent(M2, Seq(
            OrEvent(M5, Seq(
                BasicEvent(X4, p4),
                BasicEvent(X5, p5),
                BasicEvent(X6, p6)
            )),
            OrEvent(M6, Seq(
                OrEvent(M8, Seq(
                    OrEvent(M10, Seq(
                        AndEvent(M11, Seq(
                            BasicEvent(X16, p16),
                            BasicEvent(X17, p17),
                            BasicEvent(X18, p18)
                        )),
                        BasicEvent(X14, p14),
                        BasicEvent(X15, p15),
                        AndEvent(M12, Seq(
                            BasicEvent(X19, p19),
                            BasicEvent(X20, p20),
                            BasicEvent(X21, p21)
                        ))
                    )),
                    BasicEvent(X10, p10),
                    BasicEvent(X11, p11)
                )),
                OrEvent(M9, Seq(
                    BasicEvent(X12, p12),
                    BasicEvent(X13, p13)
                ))
            ))
        ))
    ))

}

object ChlorineRelease {
    import minimalcutpathset.FaultTree
    import minimalcutpathset.FaultTree.*
    import minimalcutpathset.TreeNode
    import minimalcutpathset.TreeNode.*
    import minimalcutpathset.Gate
    import minimalcutpathset.Gate.*

    final val B1 = 1
    final val B2 = 2
    final val B3 = 3
    final val B4 = 4
    final val B5 = 5
    final val B6 = 6
    final val B7 = 7
    final val B8 = 8
    final val B9 = 9
    final val B10 = 10
    final val B11 = 11
    final val B12 = 12
    final val B13 = 13
    final val B14 = 14
    final val B15 = 15
    final val B16 = 16
    final val B17 = 17
    final val B18 = 18
    final val B19 = 19
    final val B20 = 20
    final val B21 = 21
    final val B22 = 22
    final val B23 = 23
    final val B24 = 24
    final val B25 = 25
    final val B26 = 26
    final val B27 = 27

    final val ChlorineRelease = 0

    final val LeakBetweenTanksAndAA = 28
    final val PipeRupture = 29
    final val FlangeLeakInValveV2 = 30
    final val FlangeLeakInValveV21 = 31
    final val LeakBetweenAAAndBB = 32
    final val LeaksAfterBBDueToPipeRupture = 33
    final val ReleaseOfChlorineFromInlet = 34
    final val ReleaseOfChlorineFromOutlet = 35
    final val ReleaseOfChlorineFromVessel = 36
    final val ReleaseOfChlorineFromFillingPoint = 37
    final val LeakRuptureDueToOverPressure = 38
    final val SafetyReliefSystem = 39
    final val FailureOfReliefSystem = 40
    final val FailureOf2ndReliefSystem = 41
    final val OverFilling = 42
    final val InletValveFailsToHold = 43
    final val InletValveNotClosedAfterFilling = 44
    final val HumanError = 45
    final val OverTemperature = 46;
    final val FlangeLeakInValveV3 = 47
    final val FlangeLeakInValveV31 = 48

    final val LeakRuptureDueToOverPressureOr = 49 // or gate without name

    final val p1 = exp(2.790, -5)
    final val p2 = exp(2.864, -5)
    final val p3 = exp(4.093, -5)
    final val p4 = exp(2.357, -3)
    final val p5 = exp(2.864, -5)
    final val p6 = exp(2.559, -5)
    final val p7 = exp(8.763, -5)
    final val p8 = exp(2.841, -5)
    final val p9 = exp(9.527, -5)
    final val p10 = exp(1.266, -5)
    final val p11 = exp(4.049, -7)
    final val p12 = exp(3.033, -7)
    final val p13 = exp(4.856, -7)
    final val p14 = exp(3.242, -3)
    final val p15 = exp(3.902, -3)
    final val p16 = exp(3.149, -3)
    final val p17 = exp(2.551, -2)
    final val p18 = exp(4.178, -3)
    final val p19 = exp(2.759, -3)
    final val p20 = exp(2.751, -3)
    final val p21 = exp(1.266, -5)
    final val p22 = exp(2.145, -3)
    final val p23 = exp(2.759, -3)
    final val p24 = exp(3.902, -3)
    final val p25 = exp(5.121, -4)
    final val p26 = exp(1.081, -3)
    final val p27 = exp(7.659, -4)

    val FT: FaultTree = FaultTree(ChlorineRelease, Map(
        B1 -> BasicEvent(B1, p1),
        B2 -> BasicEvent(B2, p2),
        B3 -> BasicEvent(B3, p3),
        B4 -> BasicEvent(B4, p4),
        B5 -> BasicEvent(B5, p5),
        B6 -> BasicEvent(B6, p6),
        B7 -> BasicEvent(B7, p7),
        B8 -> BasicEvent(B8, p8),
        B9 -> BasicEvent(B9, p9),
        B10 -> BasicEvent(B10, p10),
        B11 -> BasicEvent(B11, p11),
        B12 -> BasicEvent(B12, p12),
        B13 -> BasicEvent(B13, p13),
        B14 -> BasicEvent(B14, p14),
        B15 -> BasicEvent(B15, p15),
        B16 -> BasicEvent(B16, p16),
        B17 -> BasicEvent(B17, p17),
        B18 -> BasicEvent(B18, p18),
        B19 -> BasicEvent(B19, p19),
        B20 -> BasicEvent(B20, p20),
        B21 -> BasicEvent(B21, p21),
        B22 -> BasicEvent(B22, p22),
        B23 -> BasicEvent(B23, p23),
        B24 -> BasicEvent(B24, p24),
        B25 -> BasicEvent(B25, p25),
        B26 -> BasicEvent(B26, p26),
        B27 -> BasicEvent(B27, p27),

        ChlorineRelease -> Combination(ChlorineRelease, Or, Set(ReleaseOfChlorineFromInlet, ReleaseOfChlorineFromOutlet, ReleaseOfChlorineFromVessel, ReleaseOfChlorineFromFillingPoint)),

        ReleaseOfChlorineFromInlet -> Combination(ReleaseOfChlorineFromInlet, Or, Set(LeakBetweenTanksAndAA, LeakBetweenAAAndBB, LeaksAfterBBDueToPipeRupture)),
        LeakBetweenTanksAndAA -> Combination(LeakBetweenTanksAndAA, Or, Set(PipeRupture, FlangeLeakInValveV2)),
        PipeRupture -> Combination(PipeRupture, Or, Set(B1, B2)),
        FlangeLeakInValveV2 -> Combination(FlangeLeakInValveV2, Or, Set(B4, B5)),
        LeakBetweenAAAndBB -> Combination(LeakBetweenAAAndBB, Or, Set(FlangeLeakInValveV21, PipeRupture)),
        FlangeLeakInValveV21 -> Combination(FlangeLeakInValveV21, Or, Set(B4, B5)),
        LeaksAfterBBDueToPipeRupture -> Combination(LeaksAfterBBDueToPipeRupture, Or, Set(B1, B2)),

        ReleaseOfChlorineFromOutlet -> Combination(ReleaseOfChlorineFromOutlet, Or, Set(LeakBetweenTanksAndAA, LeakBetweenAAAndBB, LeaksAfterBBDueToPipeRupture)),
        LeakBetweenTanksAndAA -> Combination(LeakBetweenTanksAndAA, Or, Set(PipeRupture, FlangeLeakInValveV3)),
        LeakBetweenAAAndBB -> Combination(LeakBetweenAAAndBB, Or, Set(FlangeLeakInValveV31, PipeRupture)),

        ReleaseOfChlorineFromFillingPoint -> Combination(ReleaseOfChlorineFromFillingPoint, Or, Set(B21, B22, B23, B20, B24)),

        ReleaseOfChlorineFromVessel -> Combination(ReleaseOfChlorineFromVessel, Or, Set(B10, LeakRuptureDueToOverPressure, B6, B25)),
        LeakRuptureDueToOverPressure -> Combination(LeakRuptureDueToOverPressure, And, Set(SafetyReliefSystem, LeakRuptureDueToOverPressureOr)),
        SafetyReliefSystem -> Combination(SafetyReliefSystem, And, Set(FailureOfReliefSystem, FailureOf2ndReliefSystem)),
        FailureOfReliefSystem -> Combination(FailureOfReliefSystem, Or, Set(B7, B8, B3)),
        FailureOf2ndReliefSystem -> Combination(FailureOf2ndReliefSystem, Or, Set(B7, B8, B3)),
        LeakRuptureDueToOverPressureOr -> Combination(LeakRuptureDueToOverPressureOr, Or, Set(OverFilling, OverTemperature)),
        OverTemperature -> Combination(OverTemperature, Or, Set(B11, B13, B12)),
        OverFilling -> Combination(OverFilling, Or, Set(InletValveFailsToHold, InletValveNotClosedAfterFilling)),
        InletValveFailsToHold -> Combination(InletValveFailsToHold, And, Set(B9, B9)),
        InletValveNotClosedAfterFilling -> Combination(InletValveNotClosedAfterFilling, Or, Set(B26, B15, B27, B14, HumanError)),
        HumanError -> Combination(HumanError, Or, Set(B16, B17, B18, B19)),
    ))

    def exp(scalar: Double, exponent: Double) =
        scalar * Math.pow(10, exponent)
}

object MainTrackTrainCollisionsLeadingToFatalitiesAndInjuries {

    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9

    val T = 0

    final val M1 = 10

    final val p1 = 3.7E-4
    final val p2 = 1.1E-3
    final val p3 = 7.4E-4
    final val p4 = 3.7E-4
    final val p5 = 1.5E-3
    final val p6 = 7.4E-4
    final val p7 = 3E-3
    final val p8 = 1.1E-3
    final val p9 = 3.7E-4

    val FT: FaultTree = OrEvent(T, Seq(
        BasicEvent(X1,p1),
        BasicEvent(X9,p9),
        OrEvent(M1, Seq(
            BasicEvent(X2,p2),
            BasicEvent(X3,p3),
            BasicEvent(X4,p4),
            BasicEvent(X5,p5),
            BasicEvent(X6,p6),
            BasicEvent(X7,p7)
        ))
    ))
}

object ATCFailsToResolveTheConflict {
    import faulttree.FaultTree
    import faulttree.FaultTree.*

    val X1 = 1
    val X2 = 2
    val X3 = 3
    val X4 = 4
    val X5 = 5
    val X6 = 6
    val X7 = 7
    val X8 = 8
    val X9 = 9
    val X10 = 10
    val X11 = 11
    val X12 = 12
    val X13= 13
    val X14 = 14
    val X15 = 15
    val X16 = 16
    val X17 = 17
    val X18 = 18

    val T = 0

    final val M1 = 19
    final val M2 = 20
    final val M3 = 21
    final val M4 = 22
    final val M5 = 23
    final val M6 = 24
    final val M7 = 25

    final val p1 = 4.31E-3
    final val p2 =  1.29e-1
    final val p3 =  1.29e-1
    final val p4 =  1.29e-1
    final val p5 = 2.70e-4
    final val p6 = 2.70e-4
    final val p7 = 2.70e-4
    final val p8 = 2.70e-4
    final val p9 = 2.70e-4
    final val p10 = 2.70e-4
    final val p11=  2.70e-4
    final val p12 = 2.70e-4
    final val p13 = 3.79e-2
    final val p14 = 3.79e-2
    final val p15=  3.79e-2
    final val p16 = 3.79e-2
    final val p17 =  7.20e-4
    final val p18 =  7.20e-4

    val FT: FaultTree = OrEvent(T, Seq(
        BasicEvent(X1,p1),
        OrEvent(M1, Seq(
            AndEvent(M2, Seq(
                BasicEvent(X2,p2),
                BasicEvent(X3,p3),
                BasicEvent(X4,p4)
            )),
            OrEvent(M3, Seq(
                BasicEvent(X5,p5),
                BasicEvent(X6,p6),
                BasicEvent(X7,p7),
                BasicEvent(X8,p8),
                BasicEvent(X9,p9),
                BasicEvent(X10,p10),
                BasicEvent(X11,p11),
                BasicEvent(X12,p12)
            ))
        )),
        OrEvent(M4,Seq(
            AndEvent(M5,Seq(
                BasicEvent(X13,p13),
                BasicEvent(X14,p14)
            )),
            AndEvent(M6,Seq(
                BasicEvent(X15,p15),
                BasicEvent(X16,p16)
            )),
            OrEvent(M7,Seq(
                BasicEvent(X17,p17),
                BasicEvent(X18,p18)
            ))
        ))
    ))

}

object T0Chopper {
    import minimalcutpathset.FaultTree
    import minimalcutpathset.FaultTree.*
    import minimalcutpathset.TreeNode
    import minimalcutpathset.TreeNode.*
    import minimalcutpathset.Gate
    import minimalcutpathset.Gate.*

    final val X1 = 1
    final val X2 = 2
    final val X3 = 3
    final val X4 = 4
    final val X5 = 5
    final val X6 = 6
    final val X7 = 7
    final val X8 = 8
    final val X9 = 9
    final val X10 = 10
    final val X11 = 11
    final val X12 = 12
    final val X13 = 13
    final val X14 = 14
    final val X15 = 15
    final val X16 = 16

    final val p1 = 0.000499
    final val p2 = 0.000361
    final val p3 = 0.001789
    final val p4 = 0.000120
    final val p5 = 0.001693
    final val p6 = 0.001290
    final val p7 = 0.000680
    final val p8 = 0.001033
    final val p9 = 0.000233
    final val p10 = 0.000139
    final val p11 = 0.001380
    final val p12 = 0.000467
    final val p13 = 0.000501
    final val p14 = 0.000501
    final val p15 = 0.000534
    final val p16 = 0.000347

    final val T = 0

    final val M1 = 17
    final val M2 = 18
    final val M3 = 19
    final val M4 = 20
    final val M5 = 21
    final val M6 = 22
    final val M7 = 23

    val FT: FaultTree = FaultTree(T, Map(
        X1 -> BasicEvent(X1, p1),
        X2 -> BasicEvent(X2, p2),
        X3 -> BasicEvent(X3, p3),
        X4 -> BasicEvent(X4, p4),
        X5 -> BasicEvent(X5, p5),
        X6 -> BasicEvent(X6, p6),
        X7 -> BasicEvent(X7, p7),
        X8 -> BasicEvent(X8, p8),
        X9 -> BasicEvent(X9, p9),
        X10 -> BasicEvent(X10, p10),
        X11 -> BasicEvent(X11, p11),
        X12 -> BasicEvent(X12, p12),
        X13 -> BasicEvent(X13, p13),
        X14 -> BasicEvent(X14, p14),
        X15 -> BasicEvent(X15, p15),
        X16 -> BasicEvent(X16, p16),

        M1 -> Combination(M1, Or, Set(X1, X2, X3, X4)),
        M2 -> Combination(M2, Or, Set(X5, X6)),
        M3 -> Combination(M3, Or, Set(X7, X8, X6)),
        M4 -> Combination(M4, Or, Set(X9, X10)),
        M5 -> Combination(M5, Or, Set(X11, X12)),
        M6 -> Combination(M6, Or, Set(X13, X14, X6)),
        M7 -> Combination(M7, Or, Set(X15, X16)),

        T -> Combination(T, Or, Set(M1, M2, M3, M4, M5, M6, M7))
    ))
}