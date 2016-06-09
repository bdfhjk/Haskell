FN I a() = 5,
FN I b() = 6,
FN I SF(L I x) = (call (head x)() + call (head (tail x))()) -> call SF(a:b:[I])
