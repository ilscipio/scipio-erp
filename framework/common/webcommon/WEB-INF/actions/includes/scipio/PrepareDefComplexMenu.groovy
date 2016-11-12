/**
 * SCIPIO: Prepares default generic menu.
 * This is a prelude to a call to PrepareAdvMenu.groovy;
 * causes it to ignore any activeSubMenu set by the screen.
 */

//import org.ofbiz.base.util.*;

//final module = "PrepareDefComplexMenuGroovy";

context.activeSubMenu = null;
// NOTE: it's possible to not want to set this to null...
// for now do it. see PrepareComplexMenu.groovy for details.
context.activeSubMenuItem = null; 

