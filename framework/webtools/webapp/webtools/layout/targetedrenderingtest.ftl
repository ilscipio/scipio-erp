
<#-- NOTE: only @section and @container have ID support, very limited
    and functionality not guaranteed at this time -->
<@section title="FTL section" id="tr-ftl-section-1">
  <em>This template is here to test ability to cross FTL boundaries 
    when targeting widget XML elements.
    This is not trivial.</em>
  <@render type="screen" resource="component://webtools/widget/MiscScreens.xml#TargetedRenderingTestDeepWidget1" />
  <em>You should see a hello just above this</em>
</@section>

<@section title="FTL section 2" id="tr-ftl-section-2">
  <em>This is a second section</em>
  <@container id="tr-ftl-container-1">Hello from inside a FTL @container</@container>
</@section>