FN I a() = 5,
FN I b() = 6,
FN I SF(L x) = (call (head x)() + call (head (tail x))()) -> call SF(a:b:[FUN])
