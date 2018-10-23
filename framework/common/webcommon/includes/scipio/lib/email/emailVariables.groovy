/**
 * Master Email templating variables include, standard Scipio markup
 *
 * A set of global variables that define common styling classes, part of standard Scipio Freemarker API.
 * Automatically included at all times, unless overridden by properties or themes.
 * Intended to be swappable.
 *
 * NOTES:
 * * Currently targeted toward Foundation CSS for Emails.
 */

/*
*************************************
* MAIN STYLES *
*************************************
* Scipio automatically recognizes the "styles" hash variable and makes it available.
*/

GroovyUtil.runScriptAtLocation("component://common/webcommon/includes/scipio/lib/standard/htmlVariables.groovy", null, context);

context.styles.putAll([
  /*Email specific*/
    "email_callout_table" : "callout",
    "email_callout_table_cell" : "callout-inner secondary",
    
  /* Always declare last */
    "dummy" : ""
]);

 








