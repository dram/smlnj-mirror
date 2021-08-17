fun mt (nil,       nil)       = 0
  | mt (a :: nil,  b :: nil)  = 1
  | mt (c :: cs,   d :: ds)   = 2
  | mt x                      = 3
