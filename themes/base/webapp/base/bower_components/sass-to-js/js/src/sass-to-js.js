(function (window) {
    /*--- COMMON ---*/
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
        params = params || {};
        if (!element) {
            if (params.debug) logError([element, ' is not an HTML element']);
            return {};
        }

        params.cssProperty = params.cssProperty || 'font-family';

        return _getJson(element, params);
    }

    /**
     * Util function to pass call from jQuery/Angular
     * @public
     * @returns  {Object} JSON object
     */
    function sassToJsForLibrary(params) {
        return sassToJs(this[0], params);
    }

    /**
     * Provides JSON object from CSS property value
     * @param element [HTML Element]
     * @param params {Object}
     *
     * @returns {Object} JSON object
     */
    function _getJson(element, params) {
        var cssValue;
        var json = {};

        // get CSS value
        if (params.pseudoEl) {
            cssValue = window.getComputedStyle(element, params.pseudoEl).getPropertyValue(params.cssProperty);
        } else {
            cssValue = window.getComputedStyle(element).getPropertyValue(params.cssProperty);
        }

        if (cssValue === null) {
            if (params.debug) logError(['CSS value for ', element, ' with params ', params, ' is empty']);
            return json;
        }

        // normalize
        cssValue = _normalizeCssValue(
            cssValue
        );

        try {
            json = JSON.parse(cssValue);
        } catch (err) {
            if (params.debug) logError(['Cannot parse JSON from ', element, ' with params ', params]);
        }
        // return
        return json;
    }

    /*--- UTILS ---*/
    /**
     * @param string {String} CSS value string
     * @returns {String} Normalized for JSON.stringify CSS value string
     */
    function _normalizeCssValue(string) {
        string = string.replace(/^['"]+|\s+|\\|(;\s?})+|['"]$/g, '');

        return string;
    }

    /**
     * Logs an error
     * @param error [Array]
     */
    function logError(error) {
        if (window.console && console.error) {
            console.error(error);
        }
    }

    /*--- EXPORT ---*/
    var exported = false;

    if (typeof window.jQuery === 'function') {
        $.fn.sassToJs = sassToJsForLibrary;
        $.fn.sassToJsOriginal = sassToJs;
        exported = true;
    }

    if (typeof window.angular === 'function') {
        angular.element.prototype.sassToJs = sassToJsForLibrary;
        angular.element.prototype.sassToJsOriginal = sassToJs;
        exported = true;
    }

    if (typeof define === 'function' && define.amd) {
        define(function () {
            return sassToJs;
        });
        exported = true;
    }

    if (typeof module !== 'undefined' && module.exports) {
        module.exports = sassToJs;
        exported = true;
    }

    if (!exported) {
        window.sassToJs = sassToJs;
    }
})(window);