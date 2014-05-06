doubleMe x = x + x

doubleSmallNumber x = if x > 100 then x else (doubleMe x)

broken = [[], [], []] !! (succ 2)

