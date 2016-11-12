/**
 * SCIPIO: Prepares default side bar menu.
 * This is a prelude to a call to PrepareSideBarMenu.groovy;
 * causes it to ignore any activeSubMenu set by the screen.
 */

//import org.ofbiz.base.util.*;

//final module = "PrepareDefaultComplexSideBarMenu.groovy";

context.activeSubMenu = null;
// NOTE: it's possible to not want to set this to null...
// for now do it. see PrepareComplexMenu.groovy for details.
context.activeSubMenuItem = null; 

