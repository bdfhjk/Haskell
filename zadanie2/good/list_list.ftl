FN (L L I => I) fa(L L I x) = if (empty (head x)) 
                  then 1 
                  else 2,
FN (L L I => I) fb(L L I x) = if (empty (tail (tail x))) 
                  then 4 
                  else 8 -> (call fa((2:[I]):((1:[I]):[L I]))) + ((call fb((2:[I]):((1:[I]):[L I]))))
