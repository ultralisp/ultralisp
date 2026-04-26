# CLOS: late binding of methods

A method defined in both a base library and the project with the same specializers will coexist. CLOS uses the last-loaded method. This means `make-response-message` in the project overrides the base library's version, which can change return types unexpectedly.

Be aware of this when defining methods that shadow library methods.
