import Prelude hiding( (++) )

(++) [] a = a
(++) (h:t) b = h:(t ++ b)  
