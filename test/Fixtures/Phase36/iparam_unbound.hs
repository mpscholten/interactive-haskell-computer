-- Referencing an unbound implicit parameter should produce a runtime error.
main = print ?unbound
