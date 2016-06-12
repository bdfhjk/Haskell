FN (I=>(I=>(I=>I))) function(I x, I y, I z) = x + y + z,
-> let (I=>(I=>I)) px = call function (1) in
      let (I=>I) py = call px (2) in
         call py (3)
