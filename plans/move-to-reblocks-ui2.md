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

## План (по шагам)

### 1. Подготовка: обновление зависимостей и настройка темы

- Добавить `reblocks-ui2` и `reblocks-ui2-tailwind` в зависимости (qlfile / .asd)
- В `src/server.lisp`: заменить `(setf *foundation-dependencies* ...)` на `(setf (current-theme) (make-tailwind-theme-light))` (по аналогии с `staticbot/src/server.lisp:101-103`)
- В `src/server.lisp`: убрать импорт `reblocks-ui:*foundation-dependencies*` из `get-dependencies` метода app (строка 292-295)
- В `src/server.lisp`: убрать `reblocks-lass` зависимости (`*app-dependencies*`, строки 222-289) — стили перепишутся на Tailwind

### 2. Переработка маршрутизации — переход на `defroutes` + `page` с именованными маршрутами

Заменить `reblocks-navigation-widget:defroutes` (в `src/widgets/main.lisp`) на `40ants-routes/defroutes:defroutes` + `reblocks/routes:page` прямо в `src/app.lisp` (по аналогии с `staticbot/src/app.lisp` и `reblocks-ui2-demo/app.lisp`):

**Было** (`src/widgets/main.lisp:47-77`):
```lisp
(defroutes main-routes
    ("/" (make-landing-widget))
  ("/projects/.*/.*" (make-project-widget))
  ...)
```

**Станет** (в `src/app.lisp` или новом файле маршрутов):
```lisp
(defroutes (*project-routes* :namespace "projects")
  (page ("/" :name "author" :title #'get-author-title)
    (make-author-projects-widget author))
  (page ("/<string:author>/<string:name>" :name "project" :title #'get-project-title)
    (make-project-widget author name)))

(defapp app
  :prefix "/"
  :routes ((page ("/" :name "index") (make-landing-page))
           (page ("/search/" :name "search") (make-search-page))
           (page ("/tags/" :name "tags") (make-all-tags-page))
           (page ("/tags/<string:tag>/" :name "tag" :title #'get-tag-title)
             (make-projects-by-tag-page tag))
           (page ("/projects/<string:author>/<string:name>" :name "project")
             (make-project-page author name))
           (page ("/projects/<string:author>" :name "author")
             (make-author-projects-page author))
           (page ("/dists/<string:name>" :name "dist")
             (make-dist-page name))
           (page ("/versions/<int:number>" :name "version")
             (make-version-page number))
           ...)
  :page-constructor #'wrap-with-page-frame)
```

Это устранит **regex-паттерны** и **парсинг URL внутри render-методов** (который сейчас делается через `register-groups-bind` в render каждого виджета). Вместо этого параметры будут приходить как аргументы конструктора.

### 3. Создание page-frame (замена render-body)

Заменить `render-body` (строки 327-376 в `src/server.lisp`) на виджет `page-frame-widget` (по аналогии с `reblocks-ui2-demo/demo/pages/frame.lisp`):

- Создать `src/widgets/frame.lisp` с классом `page-frame-widget` (наследник `ui-widget`) и функцией `wrap-with-page-frame`
- frame будет рендерить: header (название, motto, поиск), content area (виджет страницы), footer
- Привязать через `:page-constructor #'wrap-with-page-frame` в `defapp`
- Убрать метод `render-body` из `src/server.lisp`
- Перенести поисковую форму из frame в Tailwind-стиле

### 4. Переработка widget-классов — переход на `ui-widget` и theme-aware render

Для каждого виджета в `src/widgets/`:

- Заменить базовый класс с `widget` на `ui-widget` (из `reblocks-ui2/widget`)
- Заменить сигнатуру render: `(defmethod render ((widget <type>)) ...)` → `(defmethod render ((widget <type>) (theme tailwind-theme)) ...)`
- Импортировать `render` из `reblocks-ui2/widget` вместо `reblocks/widget`
- При рендеринге дочерних виджетов передавать `theme`: `(render child-widget theme)` вместо `(render child-widget)`

**Виджеты для переработки** (в порядке зависимости):
1. `src/widgets/landing.lisp` — landing-page
2. `src/widgets/project.lisp` — страница проекта
3. `src/widgets/projects.lisp` — список проектов автора и "my projects"
4. `src/widgets/dist.lisp` — страница дистрибутива
5. `src/widgets/dists.lisp` — "my dists"
6. `src/widgets/search.lisp` — страница поиска
7. `src/widgets/version.lisp` — страница версии
8. `src/widgets/all-tags.lisp` — все теги
9. `src/widgets/projects-by-tag.lisp` — проекты по тегу
10. `src/widgets/tags.lisp` — теги проекта (inline widget)
11. `src/widgets/source.lisp` — виджеты источников
12. `src/widgets/sponsors.lisp` — страница спонсоров
13. `src/widgets/maintenance.lisp` — режим обслуживания
14. `src/widgets/login-menu.lisp` — меню логина
15. `src/widgets/spinner.lisp` — спиннер загрузки
16. `src/github/widgets/repositories.lisp` — виджет репозиториев GitHub

### 5. Замена URL-генерации — переход на `route-url`

Заменить все `format`/ручные URL на `route-url` из `40ants-routes/route-url`:

| Файл | Было | Станет |
|------|------|--------|
| `src/models/project.lisp:741` | `(format nil "/projects/~A" name)` | `(route-url "project" :namespace '(...) :author author :name name)` |
| `src/models/dist.lisp:243` | `(format nil "/dists/~A" (dist-name dist))` | `(route-url "dist" :namespace '(...) :name (dist-name dist))` |
| `src/widgets/landing.lisp` | `"/github"`, `"/versions/~A"` | `(route-url "github")`, `(route-url "version" :number n)` |
| `src/widgets/all-tags.lisp` | `(fmt "/tags/~A/" tag-name)` | `(route-url "tag" :tag tag-name)` |
| и т.д. | | |

### 6. Замена форм — переход на `reblocks-ui2/form`

Файлы, использующие `reblocks-ui/form`:
- `src/widgets/dists.lisp` — форма создания нового дистрибутива, форма смены никнейма
- `src/widgets/tags.lisp` — кнопка удаления тега
- `src/widgets/source.lisp` — формы редактирования/удаления/проверки источников
- `src/widgets/search.lisp` — `render-link` ("Load more")
- `src/github/widgets/repositories.lisp` — форма импорта GitHub-репозиториев

Заменить `with-html-form` на `reblocks-ui2/form:form` с `on-submit` колбэками и `reblocks-ui2/inputs/text-input:input` для полей ввода.

### 7. Замена CSS — с Foundation на Tailwind

- Убрать все `reblocks-lass` зависимости (из `src/server.lisp`, из виджетов)
- Заменить Foundation CSS-классы (`grid-x`, `cell`, `small-12`, `medium-10`, etc.) на Tailwind-эквиваленты (`flex`, `grid`, `w-full`, `max-w-4xl`, `mx-auto`, etc.)
- Для виджетов, у которых есть свои LASS-стили (`src/widgets/landing.lisp`, `src/widgets/search.lisp`, `src/widgets/tags.lisp`, `src/widgets/source.lisp`, `src/widgets/login-menu.lisp`), переписать стили на Tailwind utility-классы или использовать `reblocks-ui2/html:html` с `:css` параметром

### 8. Убрать зависимость от `reblocks-navigation-widget`

После полного перехода на `defroutes` + `page`, виджет `main-routes` из `src/widgets/main.lisp` перестанет быть нужным. Файл можно будет удалить или сильно упростить (оставив только `login-menu` как часть frame).

### 9. Переработка протоколов URL

- `src/protocols/url.lisp` — метод `url` для `project2` и `dist` переписать на `route-url`
- `src/protocols/external-url.lisp` — оставить без изменений (это внешние URL)

### 10. Тестирование

- Обновить тесты виджетов на использование новой сигнатуры `render` (с theme)
- Проверить, что все маршруты резолвятся корректно через `route-url`
- Визуально проверить все страницы после миграции на Tailwind

---

## Рекомендуемый порядок выполнения

1. **Этап 1 (инфраструктура)**: п.1 (зависимости + тема) + п.3 (page-frame) + п.8 (убрать navigation-widget)
2. **Этап 2 (маршруты)**: п.2 (переработка маршрутизации) — один раз для всех страниц
3. **Этап 3 (виджеты)**: п.4 (переработка render) + п.6 (формы) — по одному виджету за раз, начиная с простых (sponsors, spinner, all-tags)
4. **Этап 4 (стили)**: п.7 (CSS на Tailwind) + п.5 (URL на route-url) — параллельно с этапом 3
5. **Этап 5**: п.9 (протоколы URL) + п.10 (тестирование)
