FN I fa() = 1,
FN I fb() = call fa() + 1,
FN I fc() = let (I => I) fa=(lambda I x => 7) in (call fb() + 1)
-> call fc()
