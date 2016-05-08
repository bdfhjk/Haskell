FN process(k) =
  if (empty k)
    then 1
  else (call process (tail k)) + (head k)
-> call process(3:2:1:[])
