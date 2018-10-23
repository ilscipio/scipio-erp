# sass-to-js

[![Build Status](https://travis-ci.org/malyw/sass-to-js.svg?branch=master)](https://travis-ci.org/malyw/sass-to-js)
[![npm version](https://badge.fury.io/js/sass-to-js.svg)](http://badge.fury.io/js/sass-to-js)
[![devDependency Status](https://david-dm.org/malyw/sass-to-js/dev-status.png)](https://david-dm.org/malyw/sass-to-js/dev-status.png)

**sass-to-js** is a Library to easily pass Sass variables via CSS to JavaScript.

It provides Sass methods to save Sass values as JSON to CSS and
JavaScript helpers to read them from CSS to JavaScript objects.

It requires **no dependencies** and has [nice Code Coverage with Tests](https://github.com/malyw/sass-to-js/tree/master/test/jasmine/specs)!

sass-to-js has been tested and works in all modern browsers and IE9+.

## Usage examples

You can use it e.g. for passing data from Sass to JS like:

* media breakpoints maps to reuse in JavaScript/HTML (like in [responsive image solutions](https://css-tricks.com/making-sass-talk-to-javascript-with-json/))
* some variables values (e.g. theme colors, dimensions etc.)
* list of variable values which might be applied in some circumstances (for example, columns count for different types of devices)
* to test your Sass code/framework with JavaScript
* to prevent providing same variables in Sass and JavaScript (as described in [sass-to-js article](http://blog.gospodarets.com/passing_data_from_sass_to_js))

## Install

Library is available via **bower**:

```bash
bower install sass-to-js --save
```

as  **npm module**:

```bash
npm install sass-to-js --save
```

or you can just download it [from Github](https://github.com/malyw/sass-to-js)

## Usage

### Sass

Import `sass/_sass-to-js.scss` library file:

```sass
@import "sass-to-js/sass/sass-to-js";
```

After that you can pass any your Sass maps variables to util function `sassToJs`. Examples:

```sass
.breakpoints-data{
  font-family: sassToJs($mediaBreakPoints);
}

.colors-data{
  &:before{
    content: sassToJs($colorMap);
  }
}
```

Also you can pass "simple" (string/color/bool/null/number/list) or
"complex" (maps) Sass values using the following syntax:

```scss
$zoomStep: 3;

&:after {
  content: sassToJs("maxZoomStep", $zoomStep);
  font-family: sassToJs("colors", $colorMap);
}
```


### JS

### Including

Include `js/dist/sass-to-js.min.js`file to your project.
It might be added via `<script/>` tag:

```html
<script src="sass-to-js/js/dist/sass-to-js.min.js"></script>
```

as **CommonJS module**:

```js
var sassToJs = require('sass-to-js/js/dist/sass-to-js.min.js');
```

or **AMD module**:

```js
require([
    'sass-to-js/js/dist/sass-to-js.min'
], function (sassToJs) {
});
```

### Syntax

Library provides util function `sassToJs` which applies two params:

1) Required `element` - HTMLElement, from which converted Sass JSON will be read;

2) Optional `params`- Object with params.

```js
/**
 * @public
 * @param element [HTML Element]
 *
 * @param [params] {Object}
 * @param [params.cssProperty] {String} CSS property name for CSS to be taken form. 'font-family' is set if not provided.
 * @param [params.pseudoEl] {String} e.g. ':before' or '::after'- if CSS need to be taken from CSS generated element
 * @param [params.debug] {Boolean} If "true"- errors are thrown to console to simplify debug
 *
 * @returns  {Object} JSON object
 */
function sassToJs(element, params) {
    ...
}
```

Variations of usage:

* **Without `params` Object** library reads elements "font-family" CSS property and tries to parse it as JSON.

```js
sassToJs(
    document.querySelector('.helper')
);
```

* **`params.pseudoEl`**- sets that JSON has to read from CSS generated content inside of element:

```js
sassToJs(
    document.querySelector('.helper'),
    {
        pseudoEl: ':before'
    }
);
```

* **`params.cssProperty`** - in this string param you can set from which CSS property has to read JSON:

```js
sassToJs(
    document.querySelector('.helper'),
    {
        pseudoEl: ':before',
        cssProperty: 'content'
    }
);
```

* **`params.debug`**- as expected, adds logging parsing etc. error to developer console.

Otherwise library doesn't trigger errors and just returns empty Object `{}` as result of its call.

### AngularJS/jQuery support

If you use Angular or/and jQuery, library detects it and provides util methods for them.

#### AngularJS

```js
angular.element(htmlEl)
    .sassToJs({pseudoEl: '::before', cssProperty: 'content'});
```

#### jQuery

```js
$(htmlEl)
    .sassToJs({pseudoEl: '::before', cssProperty: 'content'});
```

## Links and demos

### Article with description

[Sass-to-js: Passing data from Sass to JavaScript](http://blog.gospodarets.com/passing_data_from_sass_to_js)

### Demo

[Passing data from Sass to JS demo](http://blog.gospodarets.com/demos/data-from-sass-to-js/)

### Codepen

It's possible to use the library on Codepen when you use [sass-to-js reusable Pen](http://codepen.io/malyw/pen/vOEygJ)
as [External Resource](http://blog.codepen.io/2013/05/28/new-feature-use-pens-as-external-resources/):

* [Sass-to-js demo (Sass and jQuery)](http://codepen.io/malyw/pen/zGxodr)
* [Sass-to-js demo (pure CSS and pure JavaScript)](http://codepen.io/malyw/pen/PqZOBd)

## Run the build, tests and watch

```bash
npm install
grunt
```

## License

MIT

[![logo](http://imgh.us/sass-to-js_1.svg)](http://blog.gospodarets.com/passing_data_from_sass_to_js)