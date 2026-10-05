# Aurora

The default backend theme of Scipio ERP.

## What it is

A hand-written design system. The theme carries no CSS framework: no Bulma, no
Bootstrap, no Foundation, and no SASS, npm or gulp build. Every rule is in
`webapp/aurora/css/`, and the browser reads those files as they are.

It is a theme, not a screen redesign: it styles what the templating and widget
system renders (page titles, action bars, screenlets with their menus, tabs,
forms, data lists, messages, tiles, a right column). Screens do not change.

## The look

- **Graphite chrome** (`#16171B`) lets the red Scipio logo lead. The logo is
  `webapp/aurora/images/scipio-logo-small.svg`, the round mark.
- **Crimson** (`#D20025`, from the logo) never carries text on graphite. It
  marks the current app, the current menu item and the active tab with thin
  bars, and the notification dot.
- **Main buttons** are graphite (light in the dark scheme). A colour class on a
  button is an outline in the colour of its meaning; action bars and section
  menus are outline buttons. Errors are red-orange with an icon.
- **Type:** Bricolage Grotesque for titles and figures, Geist for text, Geist
  Mono for IDs and code. Page titles are 44 px ExtraBold.
- **Contrast:** every text pair passes WCAG AA (4.5:1) in both schemes; the
  bars pass 3:1.

## The shell

| Part | What it does |
| --- | --- |
| Application rail | Logo (to the launcher at "/"), one icon per application (`app_icon` in `themeStyles.groovy`), the reader's menu. |
| App panel | App name, menu filter ("/" key), fold-out menu: a row with a list gets a fold button; the current list is open; opening one closes its open siblings. |
| Top bar | Menu button, breadcrumb, "Jump to an app or page" (Ctrl K), notifications, light/dark switch. |
| Jump dialog | Ctrl K: every application and every item of the app menu; type, arrows, Enter, Esc. |
| Narrow screens | Below 1024 px the rail and the panel become a drawer with a scrim; the top bar turns graphite. |
| Wide screens | The menu button hides the panel; the `scpSidebar` cookie keeps the choice and the server renders it. |

A page that sets `auNoSideColumn` (the launcher at "/") shows the rail only.

## Screenlets

A section that holds content (no nested section, no tiles) is a card: the title
and the section menu share a header bar, the content sits below a full-width
rule, and a data list runs from edge to edge. A section that only groups other
sections, or tiles, stays on the ground with a large heading.

## Fonts and licences

The three fonts are variable woff2 files (latin subset) in `webapp/aurora/fonts/`,
all under the SIL Open Font License 1.1; the licence texts are next to them
(`OFL-*.txt`). There is no font CDN. The icons are Font Awesome 4.7 from
`base-theme` (font: SIL OFL 1.1).

## The two schemes

`aurora-tokens.css` holds the palette. The scheme arrives three ways, in this
order of authority: `:root` (light), `@media (prefers-color-scheme: dark)`, and
`:root[data-theme="dark"|"light"]` (the switch, which always wins). The server
writes `data-theme` into the `html` tag, so the page never flashes. The switch
saves the choice in the `auroraScheme` cookie and the `AURORA_SCHEME` user
preference.

## Files

| Path | Purpose |
| --- | --- |
| `data/AuroraThemeData.xml` | The `VisualTheme` and its resources. |
| `includes/themeStyles.groovy` | The style map: the class names the macros emit. |
| `includes/themeTemplate.ftl` | The macro overrides, and the `html` tag. |
| `includes/header.ftl` | Shell part 1: head, body, the rail, the app panel up to its menu. |
| `includes/appbarClose.ftl` | Shell part 2: the app menu, the top bar, the jump dialog. |
| `includes/footer.ftl` | Shell part 3: closes the content area, footer scripts. |
| `includes/login.ftl`, `error.ftl` | The sign-in split screen and the error sheet. |
| `webapp/aurora/css/aurora-tokens.css` | Colours (light and dark), type, measures. Nothing else holds a colour. |
| `webapp/aurora/css/aurora-base.css` | Reset, type, form controls, helpers. |
| `webapp/aurora/css/aurora-layout.css` | Shell, menu, jump dialog, drawer, grid, screenlets, tiles, sign-in, launcher. |
| `webapp/aurora/css/aurora-components.css` | Every widget; "Graphite discipline" at the end. |
| `webapp/aurora/css/aurora-vendor.css` | jQuery UI, CodeMirror, jstree, DataTables, Trumbowyg, flatpickr. |
| `webapp/aurora/js/aurora.js` | Shell, drawer, fold-out menu, filters, jump dialog, scheme switch, shims. |

## Speed

Chart.js loads on the first page that draws a chart (`Aurora.withChart`),
moment is the small build plus the reader's locale file, and the tile grid is
CSS (no freetile).

## Labels

The shell labels are in `framework/common/config/CommonUiLabels.xml`. The server
reads them from the `common` component jar: after a change, run
`gradlew :framework:common:jar` and restart.
