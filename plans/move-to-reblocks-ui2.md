# План перевода Ultralisp на reblocks-ui2

## Ключевые отличия текущей и целевой архитектуры

| Аспект | Сейчас (reblocks-ui) | Цель (reblocks-ui2) |
|--------|---------------------|---------------------|
| Маршрутизация | `reblocks-navigation-widget:defroutes` с regex-паттернами | `40ants-routes/defroutes:defroutes` с именованными параметрами (`<int:id>`) и `:name` |
| URL-генерация | Ручной `format` строк (например, `(format nil "/projects/~A" name)`) | `route-url` с именами маршрутов |
| Widget базовый класс | `reblocks/widget:widget` | `reblocks-ui2/widget:ui-widget` |
| render | `(defmethod render ((w <type>)) ...)` — один аргумент | `(defmethod render ((w <type>) (theme tailwind-theme)) ...)` — двойная диспетчеризация |
| Страницы (layout) | `render-body` на app + `reblocks-navigation-widget` для переключения виджетов | `page-constructor` в `defapp` + `page` маршруты |
| CSS-фреймворк | Zurb Foundation (через `*foundation-dependencies*` и `reblocks-lass`) | Tailwind CSS (через тему `reblocks-ui2/themes/tailwind`) |
| Формы | `reblocks-ui/form:with-html-form` | `reblocks-ui2/form:form` с валидаторами |
| Таблицы | Ручной HTML (`(:table (:tr ...))`) | `reblocks-ui2/tables/table:make-table` + `column` |
| Хлебные крошки | Нет (вручную `(:a ...) ">" text`) | Нет в библиотеке, нужно реализовать через `route-url` + иерархию маршрутов |

---

## Оставшиеся задачи

### Зависимости — нужно решить вручную

Необходимо добавить `reblocks-ui2` в qlfile (или настроить ASDF source-registry), чтобы система загружалась без circular dependency с `40ants-asdf-system`. Текущая локальная копия в `libs/reblocks-ui2` вызывает циклическую зависимость при загрузке.

### Замена форм — переход на `reblocks-ui2/form` (п.6)

Файлы, всё ещё использующие `reblocks-ui/form`:
- `src/widgets/dists.lisp` — форма создания нового дистрибутива, форма смены никнейма
- `src/widgets/tags.lisp` — кнопка удаления тега
- `src/widgets/source.lisp` — формы редактирования/удаления/проверки источников
- `src/widgets/search.lisp` — `render-link` ("Load more")
- `src/github/widgets/repositories.lisp` — форма импорта GitHub-репозиториев

Заменить `with-html-form` на `reblocks-ui2/form:form` с `on-submit` колбэками и `reblocks-ui2/inputs/text-input:input` для полей ввода.

### Замена URL-генерации — переход на `route-url` (п.5)

Заменить все `format`/ручные URL на `route-url` из `40ants-routes/route-url`:

| Файл | Было | Станет |
|------|------|--------|
| `src/models/project.lisp` | `(format nil "/projects/~A" name)` | `(route-url "project" :author author :name name)` |
| `src/models/dist.lisp` | `(format nil "/dists/~A" (dist-name dist))` | `(route-url "dist" :name (dist-name dist))` |
| `src/widgets/landing.lisp` | `"/github"`, `"/versions/~A"` | `(route-url "github")`, `(route-url "version" :number n)` |
| `src/widgets/all-tags.lisp` | `(fmt "/tags/~A/" tag-name)` | `(route-url "tag" :tag tag-name)` |
| `src/widgets/projects-by-tag.lisp` | `"/tags/"` | `(route-url "tags")` |
| `src/widgets/landing.lisp` | `"/sponsors"` | `(route-url "sponsors")` |

### Переработка протоколов URL (п.9)

- `src/protocols/url.lisp` — метод `url` для `project2` и `dist` переписать на `route-url`
- `src/protocols/external-url.lisp` — оставить без изменений (это внешние URL)

### Тестирование (п.10)

- Обновить тесты виджетов на использование новой сигнатуры `render` (с theme)
- Проверить, что все маршруты резолвятся корректно через `route-url`
- Визуально проверить все страницы после миграции на Tailwind

---

## План (по шагам)

### 1. ~~Подготовка: обновление зависимостей и настройка темы~~ ✅

- [x] Добавить `reblocks-ui2` и `reblocks-ui2-tailwind` в зависимости (`ultralisp.asd`)
- [x] В `src/server.lisp`: заменить `(setf *foundation-dependencies* ...)` на `(setf (current-theme) (make-tailwind-theme-light))`
- [x] В `src/server.lisp`: убрать импорт `reblocks-ui:*foundation-dependencies*` и метод `get-dependencies` с Foundation
- [x] В `src/server.lisp`: убрать `reblocks-lass` зависимости (`*app-dependencies*`)
- [x] В `ultralisp.asd`: добавить `register-system-packages` для `reblocks-ui2/*`

### 2. ~~Переработка маршрутизации — переход на `defroutes` + `page` с именованными маршрутами~~ ✅

- [x] Заменить `reblocks-navigation-widget:defroutes` (в `src/widgets/main.lisp`) на `reblocks/routes:page` в `src/app.lisp`
- [x] Все маршруты используют именованные параметры (`<string:author>`, `<int:number>`, etc.) и `:name`
- [x] Параметры маршрутов приходят как аргументы конструктора виджетов (устранён `register-groups-bind` в render)
- [x] Добавлены `:title` функции для динамических заголовков страниц

### 3. ~~Создание page-frame (замена render-body)~~ ✅

- [x] Создать `src/widgets/frame.lisp` с классом `page-frame-widget` (наследник `ui-widget`) и функцией `wrap-with-page-frame`
- [x] frame рендерит: header (название, поиск), content area (виджет страницы), footer
- [x] Привязан через `:page-constructor #'wrap-with-page-frame` в `defapp`
- [x] Убран метод `render-body` из `src/server.lisp`
- [x] Поисковая форма перенесена в Tailwind-стиле

### 4. ~~Переработка widget-классов — переход на `ui-widget` и theme-aware render~~ ✅

- [x] Заменён базовый класс с `widget` на `ui-widget` (из `reblocks-ui2/widget`)
- [x] Заменена сигнатура render на `(defmethod render ((widget <type>) (theme tailwind-theme)) ...)`
- [x] `render` импортируется из `reblocks-ui2/widget`
- [x] Дочерние виджеты получают `theme`: `(render child-widget theme)`

**Конвертированные виджеты:**
1. [x] `src/widgets/frame.lisp` — page-frame (новый файл)
2. [x] `src/widgets/landing.lisp` — landing-page
3. [x] `src/widgets/project.lisp` — страница проекта
4. [x] `src/widgets/projects.lisp` — список проектов автора и "my projects"
5. [x] `src/widgets/dist.lisp` — страница дистрибутива
6. [x] `src/widgets/dists.lisp` — "my dists"
7. [x] `src/widgets/search.lisp` — страница поиска
8. [x] `src/widgets/version.lisp` — страница версии
9. [x] `src/widgets/all-tags.lisp` — все теги
10. [x] `src/widgets/projects-by-tag.lisp` — проекты по тегу
11. [x] `src/widgets/tags.lisp` — теги проекта (inline widget)
12. [x] `src/widgets/source.lisp` — убран LASS (render оставлен через reblocks/widget для внутренних подвиджетов)
13. [x] `src/widgets/sponsors.lisp` — страница спонсоров
14. [x] `src/widgets/maintenance.lisp` — режим обслуживания
15. [x] `src/widgets/login-menu.lisp` — меню логина
16. [x] `src/widgets/spinner.lisp` — спиннер загрузки
17. [x] `src/github/widgets/repositories.lisp` — убран LASS

### 5. Замена URL-генерации — переход на `route-url` ⏳

Заменить все `format`/ручные URL на `route-url` из `40ants-routes/route-url`:

| Файл | Было | Станет |
|------|------|--------|
| `src/models/project.lisp` | `(format nil "/projects/~A" name)` | `(route-url "project" :author author :name name)` |
| `src/models/dist.lisp` | `(format nil "/dists/~A" (dist-name dist))` | `(route-url "dist" :name (dist-name dist))` |
| `src/widgets/landing.lisp` | `"/github"`, `"/versions/~A"` | `(route-url "github")`, `(route-url "version" :number n)` |
| `src/widgets/all-tags.lisp` | `(fmt "/tags/~A/" tag-name)` | `(route-url "tag" :tag tag-name)` |
| и т.д. | | |

### 6. Замена форм — переход на `reblocks-ui2/form` ⏳

Файлы, использующие `reblocks-ui/form`:
- `src/widgets/dists.lisp` — форма создания нового дистрибутива, форма смены никнейма
- `src/widgets/tags.lisp` — кнопка удаления тега
- `src/widgets/source.lisp` — формы редактирования/удаления/проверки источников
- `src/widgets/search.lisp` — `render-link` ("Load more")
- `src/github/widgets/repositories.lisp` — форма импорта GitHub-репозиториев

Заменить `with-html-form` на `reblocks-ui2/form:form` с `on-submit` колбэками и `reblocks-ui2/inputs/text-input:input` для полей ввода.

### 7. ~~Замена CSS — с Foundation на Tailwind~~ ✅

- [x] Убраны все `reblocks-lass` зависимости (из `src/server.lisp`, из виджетов)
- [x] Заменены Foundation CSS-классы на Tailwind-эквиваленты во всех виджетах
- [x] Для виджетов, у которых были LASS-стили, стили переписаны на Tailwind utility-классы

### 8. ~~Убрать зависимость от `reblocks-navigation-widget`~~ ✅

- [x] `src/widgets/main.lisp` упрощён — `defroutes main-routes` удалён
- [x] Маршрутизация перенесена в `src/app.lisp`

### 9. Переработка протоколов URL ⏳

- `src/protocols/url.lisp` — метод `url` для `project2` и `dist` переписать на `route-url`
- `src/protocols/external-url.lisp` — оставить без изменений (это внешние URL)

### 10. Тестирование ⏳

- Обновить тесты виджетов на использование новой сигнатуры `render` (с theme)
- Проверить, что все маршруты резолвятся корректно через `route-url`
- Визуально проверить все страницы после миграции на Tailwind

---

## Рекомендуемый порядок выполнения

1. ~~**Этап 1 (инфраструктура)**: п.1 (зависимости + тема) + п.3 (page-frame) + п.8 (убрать navigation-widget)~~ ✅
2. ~~**Этап 2 (маршруты)**: п.2 (переработка маршрутизации) — один раз для всех страниц~~ ✅
3. ~~**Этап 3 (виджеты)**: п.4 (переработка render) — по одному виджету за раз~~ ✅
4. ~~**Этап 4 (стили)**: п.7 (CSS на Tailwind)~~ ✅
5. **Этап 5**: п.5 (URL на `route-url`) + п.6 (формы на `reblocks-ui2/form`) + п.9 (протоколы URL) + п.10 (тестирование)
