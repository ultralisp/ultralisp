# Testing internal (%private) functions

To test `%private-fn` from another package do not export the symbol. Use fully qualified name like `my-package::%private-fn`.

The `%` prefix already signals "internal" — exporting is not acceptable.
