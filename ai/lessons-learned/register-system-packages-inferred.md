# register-system-packages запрещён для package-inferred систем

**Source project:** ultralisp
**Date:** 2026-04-26

## Problem

При добавлении зависимости от `reblocks-ui2` (package-inferred / 40ants-package-inferred система) был добавлен вызов `register-system-packages` в `.asd` файл:

```lisp
;; ❌ НЕ ДЕЛАЙТЕ ТАК
(register-system-packages "reblocks-ui2" '(#:reblocks-ui2/widget
                                            #:reblocks-ui2/themes/tailwind
                                            #:reblocks-ui2/themes/api
                                            #:reblocks-ui2/form))
```

Это вызвало circular dependency при загрузке: ASDF начал циклически пытаться загрузить `reblocks-ui2` → `reblocks-ui2/containers/stack` → `reblocks-ui2` (родитель).

## Solution

`register-system-packages` предназначен **только** для систем, где один `.asd` файл предоставляет несколько пакетов (например `"log4cl"` → `(#:log)`). Для `package-inferred-system` каждый файл = свой пакет = своя подсистема, и ASDF резолвит их автоматически. Перечисление подсистем через `register-system-packages` ломает граф зависимостей.

Нужно просто добавить систему в зависимости:

```lisp
;; ✅ ПРАВИЛЬНО
:depends-on ("reblocks-ui2"
             "reblocks-ui2-tailwind"
             ...)
```

И в исходных файлах использовать `:import-from` с полными путями пакетов — ASDF сам найдёт нужные подсистемы:
```lisp
(:import-from #:reblocks-ui2/widget
              #:render
              #:ui-widget)
```
