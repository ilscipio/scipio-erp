
<#-- NOTE: only @section and @container have ID support, very limited
    and functionality not guaranteed at this time -->
<@section title="FTL section" id="tr-ftl-section-1">
  <em>This template is here to test ability to cross FTL boundaries 
    when targeting widget XML elements.
    This is not trivial.</em>
  <@render type="screen" resource="component://webtools/widget/MiscScreens.xml#TargetedRenderingTestDeepWidget1" />
  <em>You should see a hello just above this</em>
  
  <#-- NOTE: @virtualSection is the FTL equivalent of widget "section" element
      (whereas @section is equivalent to "screenlet" element) -->
  <@virtualSection name="TR-FTL-VirtualSection-1">
    <em>Hello from inside an FTL virtual section (1)</em>
  </@virtualSection>
</@section>

<@virtualSection name="TR-FTL-VirtualSection-2">
  <em>Hello from inside an FTL virtual section (2)</em>
  <@section title="FTL section 2" id="tr-ftl-section-2">
    <em>This is a second section</em>
    <@container id="tr-ftl-container-1">Hello from inside a FTL @container</@container>
  </@section>

  <#-- NOTE: Only a few standard FTL elements currently support selection: @section, @container, @form, @table
      We do NOT want to add it to @row or @cell because there are far too many instances on a page
      and performance is unclear. -->
  <@form id="tr-ftl-form-1">
    <@field type="input" name="testinput1" label="Input 1" />
    <@field type="input" name="testinput2" label="Input 2" />
  </@form>
  
  <@render type="form" resource="component://webtools/widget/MiscForms.xml" name="TargetedRenderingTestForm1" />
  
  <@table type="data-list" id="tr-ftl-table-1">
    <@thead>
      <@th>Header 1</@th>
      <@th>Header 2</@th>
    </@thead>
    <@tbody>
      <@tr>
        <@td>Hello from table</@td>
        <@td>Hello from table</@td>
      </@tr>
    </@tbody>
  </@table>
  
  <@menu type="button-dropdown" title="Single dropdown button menu" id="tr-ftl-menu-1">
    <@menuitem type="link" text="Menu Button 1" />
    <@menuitem type="link" text="Menu Button 2" />
    <@menuitem type="link" text="Menu Button 3" />
  </@menu>
  
  <@render type="menu" resource="component://webtools/widget/MiscMenus.xml" name="TargetedRenderingTestMenu1" />

</@virtualSection>
