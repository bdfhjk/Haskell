FN (L I => I) length(L I l) = 
    if (empty l) 
        then 1 
        else 1 + call length(tail l)
-> call length (1:2:3:4:11:22:33:99999:[I])
