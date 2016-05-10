FN I fa(L x) = if (empty (head x)) then 1 else 2,
FN I fb(L x) = if (empty (tail (tail x))) then 4 else 8 -> (call fa((2:[I]):((1:[I]):[L]))) + ((call fb((2:[I]):((1:[I]):[L]))))
