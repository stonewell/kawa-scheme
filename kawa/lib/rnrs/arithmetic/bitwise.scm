(module-name (rnrs arithmetic bitwise))
(require <kawa.lib.prim_imports>)
(require <kawa.lib.numbers>)

(module-export bitwise-not
               bitwise-and bitwise-ior bitwise-xor bitwise-if
               bitwise-bit-count bitwise-length bitwise-first-bit-set
               bitwise-bit-set? bitwise-copy-bit
               bitwise-bit-field bitwise-copy-bit-field
               bitwise-arithmetic-shift
               bitwise-arithmetic-shift-left
               bitwise-arithmetic-shift-right
               bitwise-rotate-bit-field
               bitwise-reverse-bit-field)

(import (only gnu.kawa.functions.BitwiseOp
              (ashift arithmetic-shift)
              (ashift bitwise-arithmetic-shift)
              (ashiftl bitwise-arithmetic-shift-left)
              (ashiftr bitwise-arithmetic-shift-right)
              (and bitwise-and)
              (ior bitwise-ior)
              (not bitwise-not)
              (xor bitwise-xor)))
