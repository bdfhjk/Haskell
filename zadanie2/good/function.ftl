FN fa() = 2,
FN fb() = 3*call fa()
->
let a = 4 in
  let b = 5 in
    (a + b)*call fb()
