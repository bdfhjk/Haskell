FN (L I => I) process(L I k) =
  if (empty k)
    then 1
  else (call process (tail k)) + (head k)
-> call process(1:2:3:[I])
