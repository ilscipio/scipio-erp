/**
 * SCIPIO: Gets the visual theme resources into layoutSettings global using default logic.
 */

// The stock behavior is this:
// <set field="visualThemeId" from-field="userPreferences.VISUAL_THEME" global="true"/>
// <service service-name="getVisualThemeResources">
//     <field-map field-name="visualThemeId"/>
//     <field-map field-name="themeResources" from-field="layoutSettings"/>
// </service>
// <set field="layoutSettings" from-field="themeResources" default-value="${layoutSettings}" global="true"/>
//
// In Scipio, the theme should have been already
// looked up into rendererVisualThemeResources by the renderer, which uses more 
// complex lookup logic than the stock screen userPrefs-based lookup.
// See ScreenRenderer.populateXxx.
 
themeResources = (globalContext.layoutSettings != null) ? globalContext.layoutSettings : [:];
visualThemeId = null;

if (context.rendererVisualThemeResources) {
    themeResources.putAll(context.rendererVisualThemeResources);
    visualThemeId = themeResources.VT_ID[0];
}

context.themeResources = themeResources;
globalContext.visualThemeId = visualThemeId;
globalContext.layoutSettings = themeResources;
