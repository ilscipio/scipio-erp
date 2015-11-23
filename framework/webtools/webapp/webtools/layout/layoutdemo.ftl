<#--<@nav type="magellan">
    <@mli arrival="breadcrumbs"><a href="#breadcrumbs">Breadcrumbs</a></@mli>
    <@mli arrival="grid"><a href="#grid">Grid</a></@mli>
    <@mli arrival="blockgrid"><a href="#blockgrid">Block-Grid</a></@mli>
    <@mli arrival="buttons"><a href="#buttons">Buttons</a></@mli>
    <@mli arrival="panel"><a href="#panel">Panel</a></@mli>
</@nav>-->

<div ${mtarget("breadcrumbs")} id="breadcrumbs"></div>
<@nav type="breadcrumbs">
    <li><a href="#">Home</a></li>
    <li><a href="#">Features</a></li>
    <li class="${styles.nav_breadcrumbs_disabled!}"><a href="#">Gene Splicing</a></li>
    <li class="${styles.nav_breadcrumbs_active!}">Cloning</li>
</@nav>

<@section>
    <@heading attribs=makeMagTargetAttribMap("grid") id="grid">Grid</@heading>
    <@row class="+${styles.grid_display!}">
        <@cell columns=2>2</@cell>
        <@cell columns=4>4</@cell>
        <@cell columns=6>6</@cell>
    </@row>
    <@row class="+${styles.grid_display!}">
        <@cell columns=3>3</@cell>
        <@cell columns=6>6</@cell>
        <@cell columns=3>3</@cell>
    </@row>
    <@row class="+${styles.grid_display!}">
        <@cell columns=2>2</@cell>
        <@cell columns=8>8</@cell>
        <@cell columns=2>2</@cell>
    </@row>   
    <@row class="+${styles.grid_display!}">
        <@cell columns=3>3</@cell>
        <@cell columns=9>9</@cell>
    </@row>   
    <@row class="+${styles.grid_display!}">
        <@cell columns=4>4</@cell>
        <@cell columns=8>8</@cell>
    </@row>       
    <@row class="+${styles.grid_display!}">
        <@cell columns=6>6</@cell>
        <@cell columns=6>6</@cell>
    </@row>
</@section>

<@section>
    <@heading attribs=makeMagTargetAttribMap("blockgrid") id="blockgrid">Tiles</@heading>
    <@grid type="tiles">
        <@tile type="large" color=3 icon="${styles.icon_prefix!}star">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>
        <@tile type="normal" color=7 title="Test" image="http://placehold.it/150x150"></@tile>
        <@tile type="small" color=6 title="" icon="${styles.icon_prefix!}flag"></@tile>
        <@tile type="small" color=3 title="Test" image="http://placehold.it/70x70"></@tile>
        <@tile type="wide" color=0 title="dasdsadsas dasdas" image="http://placehold.it/310x150"></@tile>
        <@tile type="large" color=6 title="Test" image="http://placehold.it/310x310"></@tile>
        <@tile type="wide" color=2 title="Test 3" icon="${styles.icon_prefix!}heart">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>    
        <@tile type="normal" color=2 icon="${styles.icon_prefix!}layout">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>
        <@tile type="normal" color=5 title="2" icon="${styles.icon_prefix!}music"></@tile>
        <@tile type="small" color=4 title="Test"></@tile>
        <@tile type="small" color=6 title="Test"></@tile>
        <@tile type="small" color=4 title="Test"></@tile>
        <@tile type="small" color=5 title="Test"></@tile>
        <@tile type="normal" color=0 image="http://placehold.it/150x150"></@tile>
        <@tile type="normal" color=1 title="2" icon="${styles.icon_prefix!}like"></@tile>
    </@grid>
</@section>

<@section title="Section Titles and Headings - Auto Leveling">
  <@section title="Nested Section Title A">
    <@section title="Nested Nested Section Title 1">
    </@section>
    <@section title="Nested Nested Section Title 2">
    </@section>
  </@section>
  <@section title="Nested Section Title B">
    <@section title="Nested Nested Section Title 3">
    </@section>
    <@section title="Nested Nested Section Title 4">
    </@section>
  </@section>
  <@heading>Heading</@heading>
  <@heading relLevel=1>Heading - Relative Level +1</@heading>
  <@heading relLevel=2>Heading - Relative Level +2</@heading>
</@section>

<@section title="Broken-up section" openOnly=true />
  <@section title="Broken-up nested section" openOnly=true />
    [inside]
  <@section closeOnly=true />
<@section closeOnly=true />

<@section class="+my-section-headings">
<@heading attribs=makeMagTargetAttribMap("buttons") id="buttons">Buttons</@heading>
<@heading relLevel=+1>Heading</@heading>
<@heading level=1>h1.</@heading>
<@heading level=2>h2.</@heading>
<@heading level=3>h3.</@heading>
<@heading level=4>h4.</@heading>
<@heading level=5>h5.</@heading>
<@heading level=6>h6.</@heading>
<@heading level=7 class="+my-additional-heading-class">Heading level 7</@heading>
<@heading level=8 class="my-replace-default-heading-class-level-8">Heading level 8 (custom class)</@heading>
<@heading level=9>Heading level 9</@heading>
<@heading level=2 containerElemType="div" containerClass="+my-heading-container-class" 
    containerId="my-heading-container-1" class="+my-heading-class" id="my-heading">Heading level 2 with container</@heading>

<@heading relLevel=+1>Shapes</@heading>
<a href="#" class="${styles.button!} ${styles.tiny!} ${styles.button_color_default}">Tiny Button</a>
<a href="#" class="${styles.button!} ${styles.small!} ${styles.button_color_default}">Small Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_default}">Default Button</a>
<a href="#" class="${styles.button!} ${styles.disabled!} ${styles.button_color_default}">Disabled Button</a>
<a href="#" class="${styles.button!} ${styles.large!} ${styles.button_color_default}">Large Button</a>
<a href="#" class="${styles.button!} ${styles.expand!} ${styles.button_color_default}">Expanded Button</a>
<a href="#" class="${styles.button!} ${styles.round!} ${styles.button_color_default}">Round Button</a>
<a href="#" class="${styles.button!} ${styles.radius!} ${styles.button_color_default}">Radius Button</a>

<@heading relLevel=+1>Colors</@heading>
<a href="#" class="${styles.button!} ${styles.button_color_default}">Default Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_success!}">Success Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_primary!}">Secondary Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_alert!}">Alert Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_info!}">Info Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_default} ${styles.disabled}">Disabled Button</a>

<@heading relLevel=+1>Button Groups</@heading>
<@menu type="button" class="+my-button-menu-class" id="my-button-menu">
  <@menuitem type="link" text="Menu Button 1" />
  <@menuitem type="link" text="Menu Button 2" contentClass="+${styles.disabled}"/>
  <@menuitem type="link" text="Menu Button 3" contentClass="+${styles.color_green}">
    <!-- nested menu item comment -->
  </@menuitem>
</@menu>

</@section>                                     

<@section>
    <@heading attribs=makeMagTargetAttribMap("panel") id="panel">Panel</@heading>
    <@row>
        <@cell columns=6>
            <@panel>
               Run a manual sweep of anomalous airborne or electromagnetic readings. Radiation levels in our atmosphere have increased by 3,000 percent. Electromagnetic and subspace wave fronts approaching synchronization. What is the strength of the ship's deflector shields at maximum output? The wormhole's size and short period would make this a local phenomenon. Do you have sufficient data to compile a holographic simulation?
            </@panel>
        </@cell>
        <@cell columns=6>
            <@panel type="callout">
                I have reset the sensors to scan for frequencies outside the usual range. By emitting harmonic vibrations to shatter the lattices. We will monitor and adjust the frequency of the resonators. He has this ability of instantly interpreting and extrapolating any verbal communication he hears. It may be due to the envelope over the structure, causing hydrogen-carbon helix patterns throughout. I'm comparing the molecular integrity of that bubble against our phasers.
            </@panel>
        </@cell>
    </@row>
</@section>

<@section>
    <@heading attribs=makeMagTargetAttribMap("charts") id="charts">Charts</@heading>
    <@row>
        <@cell columns="4">
            <@chart type="pie">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="2" title="Sausage"/> 
                <@chartdata value="19" title="Cheese"/> 
                <@chartdata value="6" title="Chicken"/> 
                <@chartdata value="27" title="Other"/>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <@chart type="bar">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="14" title="Sausage"/> 
                <@chartdata value="8" title="Cheese"/> 
                <@chartdata value="11" title="Chicken"/> 
                <@chartdata value="7" title="Other"/>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <@chart type="line">
                <@chartdata value="36" value2="1" title="Peperoni"/>
                <@chartdata value="2" value2="2" title="Sausage"/> 
                <@chartdata value="19" value2="3" title="Cheese"/> 
                <@chartdata value="6" value2="4" title="Chicken"/> 
                <@chartdata value="27" value2="5" title="Other"/>  
            </@chart>
        </@cell>
    </@row>
</@section>

<@section>
    <@heading attribs=makeMagTargetAttribMap("misc") id="misc">Misc</@heading>
    <@progress value=5 id="test"/>
    <#-- simple animation test -->
    <@script>
      $('#test_meter').css({"width": "78%"});
    </@script>
</@section>

<@section title="Tables">
  <#macro commonTestTableContent numRows=4 rowDividerContent="">
    <@thead>
      <@tr><@th>Column 1</@th><@th>Column 2</@th><@th>Column 3</@th></@tr>
    </@thead>
    <@tbody>
    <#list 1..numRows as rowIndex>
      <@tr><@td>Regular row ${rowIndex}-1</@td><@td>Cell ${rowIndex}-2</@td><@td>Cell ${rowIndex}-3</@td></@tr>
      <#if rowIndex_has_next>
        ${rowDividerContent}
      </#if>
    </#list>
      <#nested>
    </@tbody>
    <@tfoot>
      <@tr><@td colspan=3>Footer</@td></@tr>
    </@tfoot>       
  </#macro>

  <@section title="Default tables">
    <@section title="generic table">
      <@table type="generic" attribs={"extra-table-attrib":"some-value"}>
        <@commonTestTableContent />
      </@table>
    </@section>  
    <@section title="data-list table">
      <@table type="data-list" extraTableAttrib="some-value">
        <@commonTestTableContent />
      </@table>
    </@section>   
    <@section title="data-list-multiform table">
      <@table type="data-list-multiform">
        <@commonTestTableContent />
      </@table>
    </@section>       
    <@section title="data-complex table">
      <@table type="data-complex">
        <#assign rowDividerContent><@tr type="util"><@td colspan="3"><hr /></@td></@tr></#assign>
        <@commonTestTableContent rowDividerContent=rowDividerContent/>
      </@table>
    </@section>     
    <@section title="fields table">
      <@table type="fields">
        <@commonTestTableContent />
      </@table>
    </@section>   
    <@section title="summary table">
      <@table type="summary">
        <@commonTestTableContent numRows=1 />
      </@table>
    </@section>       
  </@section>    

  <@section title="Complex tables">
    <@section title="data-complex table (scrollable only)">
      <@table type="data-complex" scrollable=true>
        <@commonTestTableContent />
      </@table>
    </@section>
    <@section title="data-complex table (responsive - default options)">
      <@table type="data-complex" responsive=true>
        <@commonTestTableContent />
      </@table>
    </@section>  
    <#assign responsiveOptions = {
        "fixedHeader" : true,
        "info" : true,
        "paging" : false,
        "searching" : false,
        "ordering" : true
    }>
    <@section title="data-complex table (responsive - custom responsive)">
      <@table type="data-complex" responsive=true responsiveOptions=responsiveOptions scrollable=false fixedColumnsLeft=1 responsiveDefaults=false>
        <@commonTestTableContent />
      </@table>
    </@section>      

    <@section title="data-complex table (nested tables and alt rows)">
      <@table type="data-complex" autoAltRows=true>
        <@thead>
          <@tr><@th>Column 1</@th><@th>Column 2</@th><@th>Column 3</@th></@tr>
        </@thead>
        <@tbody>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr groupLast=true><@td>Grouped row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr groupLast=true><@td>Grouped row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr><@td>
            <@table type="data-complex" inheritAltRows=true>
              <@tr><@td>Table inheriting alt rows from parent</@td><@td>Cell</@td></@tr>
              <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
              <@tr><@td>Regular row</@td><@td>
                <@table type="data-complex" inheritAltRows=true>
                  <@tr><@td>Table inheriting alt rows from parent</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                </@table>
              </@td></@tr>
              <@tr><@td>Regular row</@td><@td>
                <@table type="data-complex" autoAltRows=true firstRowAlt=true inheritAltRows=false>
                  <@tr><@td>Table using its own alt rows within parent, first row odd</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr groupLast=true><@td>Grouped row</@td><@td>Cell</@td></@tr>
                  <@tr groupLast=true><@td>Grouped row</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr type="meta"><@td colspan=2>Special meta row</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr alt=false><@td>Row forced to even alt</@td><@td>Cell</@td></@tr>
                  <@tr alt=false><@td>Row forced to even alt</@td><@td>Cell</@td></@tr>
                  <@tr alt=true><@td>Row forced to odd alt</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                  <@tr type="util"><@td colspan=2>Special utility row <hr /></@td></@tr>
                  <@tr><@td>Regular row</@td><@td>Cell</@td></@tr>
                </@table>
              </@td></@tr>
            </@table>
          </@td><@td>Regular row</@td><@td>Cell</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
          <@tr type="meta"><@td colspan=3>Special meta row</@td></@tr>
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
  
          <@tr openOnly=true colspan=3 />
              <@td openOnly=true/>Manual open/close rows and table<@td closeOnly=true />
              <@td openOnly=true colspan=2/>
                  <@table type="data-complex" inheritAltRows=true openOnly=true />
                      <@thead openOnly=true /><@tr openOnly=true /><@td openOnly=true/>Header<@td closeOnly=true /><@td openOnly=true/>Cell<@td closeOnly=true /><@tr closeOnly=true /><@thead closeOnly=true />
                      <@tbody openOnly=true /><@tr openOnly=true /><@td openOnly=true/>Body<@td closeOnly=true /><@td openOnly=true/>Cell<@td closeOnly=true /><@tr closeOnly=true /><@tbody closeOnly=true />
                      <@tfoot openOnly=true /><@tr openOnly=true /><@td openOnly=true/>Footer<@td closeOnly=true /><@td openOnly=true/>Cell<@td closeOnly=true /><@tr closeOnly=true /><@tfoot closeOnly=true />
                  <@table closeOnly=true />
              <@td closeOnly=true />
          <@tr closeOnly=true />
  
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
        </@tbody>
        <@tfoot>
          <@tr><@td colspan=3>Footer</@td></@tr>
        </@tfoot>
      </@table>
    </@section>
  </@section>
</@section>

<@section title="Menus">
  <@menu type="tab">
    <@menuitem type="link" text="Menu Button 1" />
    <@menuitem type="link" text="Menu Button 2" contentClass="+${styles.disabled}"/>
    <@menuitem type="link" text="Menu Button 3" contentClass="+${styles.color_green}">
      <!-- nested menu item comment -->
    </@menuitem>
  </@menu>

  <#assign menuItems = [
    {"type":"link", "text":"Menu Tab 2", "disabled":true},
    {"type":"link", "text":"Menu Tab 1", "href":"ofbizUrl://WebtoolsLayoutDemo"},
    {"type":"link", "text":"Menu Tab 4", "contentClass":"+${styles.color_green}", "onClick":"javascript:alert('Clicked menu item!');"},
    {"type":"text", "text":"Menu Tab 3 (text entry)", "nestedContent":"<!-- hidden nested menu item comment -->"}
    {"type":"submit", "text":"Menu Tab 5 (submit)", "disabled":true, "class":"+mymenuitemclass", "contentClass":"+mymenuitemcontentclass"}
    {"type":"link", "text":"Menu Tab 6", "selected":true}
    {"type":"link", "text":"Menu Tab 7", "active":true}
  ]>
  <@menu type="subtab" items=menuItems sort=true sortDesc=true/>

  <#macro menuContent menuArgs>
    <@menu class="+my-menu-class" args=menuArgs>
      <@menuitem type="link" text="Menu Button 1" class="+mymenuitemclass"/>
      <@menuitem type="link" text="Menu Button 2" />
    </@menu>
  </#macro>
  <@section title="Section with macro menu def" menuContent=menuContent>
    [section content]
  </@section>

  <#assign menuContent = {"type":"section", "items":[
    {"type":"link", "text":"Menu Button 1" },
    {"type":"link", "text":"Menu Button 2" }
  ]}>
  <@section title="Section with hash/map menu def" menuContent=menuContent>
    [section content]
  </@section>

  <#assign menuContent>
    <@menu type="section" inlineItems=true>
      <@menuitem type="link" text="Menu Button 1" />
      <@menuitem type="link" text="Menu Button 2" />
    </@menu>
  </#assign>
  <@section title="Section with string/html menu def (old)" menuContent=menuContent>
   [section content]
  </@section>

</@section>

<#-- The titleStyle usage here is a demo of what can be set in <label style="..." /> in screens,
     usually don't need in @section macro. See outputted markup for results. -->
<@section title="Macro Test">
    <@section title="Nested section title" titleClass="heading+2:+test-additional-section-title-class">
    </@section>
    <@row class="+test-additional-row-class">
      <@cell class="+test-additional-cell-class">
        In a cell
      </@cell>
    </@row>
    <@row>
      <@cell class="${styles.grid_large}9">
        In a cell
      </@cell>
    </@row>
    <@row class="+test-additional-row-class-1 test-additional-row-class-2">
      <@cell class="${styles.grid_large}9">
        In a cell
      </@cell>
      <@cell large=3 class="+test-additional-cell-class">
        In a cell
      </@cell>
    </@row>
    <@section title="Another section" class="+test-additional-section-class">
      In a sub-section (auto title level increase)
    </@section>
    <@section title="Another section" titleClass="h6">
      In a sub-section (manual title level to h6)
    </@section>
</@section>

<#-- javascript test; entitymaint should only appear once in output... -->
<@script>
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "ServiceList" />
</@script>


<@section title="Fields">
  <#-- TODO: submitarea -> submit (but not to remove submitarea; still important) -->

  <@section title="Default form fields (with label area)">
    <@form name="form1">
    <@fields type="default"> <#-- note: @fields currently optional for type="default"-->
      <@field type="input" name="input1" label="Input 1" />
      <@field type="input" name="input2" label="Input 2" />
      <@field type="display">Display value</@field>
      <@field type="input" name="input3" label="Input 3" />

      <@field type="radio" name="radio1" label="Radio 1" value="Y"/>
      <@field type="radio" name="radio2" label="Radio 2" value="Y" checked=true/>
      <#assign items = [
        {"value":"val1", "description":"Option 1"}
        {"value":"val2", "description":"Option 2"}
        {"value":"val3", "description":"Option 3", "tooltip":"this is radio option 3"}
      ]>
      <@field type="radio" name="radio3a" label="Radio 3a (multi - inline)" items=items currentValue="val2" inlineItems=true tooltip="these are radios"/>
      <@field type="radio" name="radio3b" label="Radio 3b (multi - one per line)" items=items currentValue="val2" inlineItems=false tooltip="these are radios"/>
      <@field type="radio" name="radio4" label="Radio 4" value="Y" currentValue="Y" />
      <@field type="radio" name="radio5" label="Radio 5" value="Y" currentValue="N" />
      <@field type="radio" name="radio6" label="Radio 6" value="Y" defaultValue="Y" />
      <@field type="radio" name="radio7" label="Radio 7" value="Y" defaultValue="N" />
      <@field type="checkbox" name="checkbox1" label="Checkbox 1" value="Y" />
      <@field type="checkbox" name="checkbox2" label="Checkbox 2" value="Y" checked=true/>
      <#assign items = [
        {"value":"val1", "description":"Option 1"}
        {"value":"val2", "description":"Option 2"}
        {"value":"val3", "description":"Option 3", "tooltip":"this is checkbox option 3"}
      ]>
      <@field type="checkbox" name="checkbox3a" label="Checkbox 3a (multi - inline)" items=items currentValue=["val2", "val3"] inlineItems=true tooltip="these are checkboxes"/>
      <@field type="checkbox" name="checkbox3b" label="Checkbox 3b (multi - one per line)" items=items currentValue=["val2", "val3"] inlineItems=false tooltip="these are checkboxes"/>
      <@field type="checkbox" name="checkbox4" label="Checkbox 4" value="Y" currentValue="Y" />
      <@field type="checkbox" name="checkbox5" label="Checkbox 5" value="Y" currentValue="N" />
      <@field type="checkbox" name="checkbox6" label="Checkbox 6" value="Y" defaultValue="Y" />
      <@field type="checkbox" name="checkbox7" label="Checkbox 7" value="Y" defaultValue="N" />

      <#assign items = [
        {"value":"val1", "description":"Value 1"},
        {"value":"val2", "description":"Value 2"},
        {"value":"val3", "description":"Value 3"},
        {"value":"val4", "description":"Value 4"},
        {"value":"val5", "description":"Value 5"}
      ]>
      <@field type="select" items=items name="select1" label="Select 1" currentValue="val2" />
      <@field type="select" items=items name="select1" label="Select 2" currentValue="val2" currentFirst=true currentDescription="THE FIRST"/>
      <@field type="select" items=items name="select1" label="Select 2" currentValue="val2" currentFirst=true />
      <@field type="select" items=items name="select1" label="Select 3" currentValue="val2" allowEmpty=true />
      <@field type="select" items=items name="select1" label="Select 3" allowEmpty=true />
      <#assign items = [
        {"value":"val1", "description":"Value 1"},
        {"value":"val2", "description":"Value 2"},
        {"value":"val3", "description":"Value 3", "selected":true},
        {"value":"val4", "description":"Value 4"},
        {"value":"val5", "description":"Value 5", "selected":true}
      ]>
      <@field type="select" items=items name="select1" label="Select 4 (multiple)" currentValue="val3" multiple="multiple" />
      <@field type="select" items=items name="select1" label="Select 4 (multiple - asmselect - default)" currentValue="val3" multiple="multiple" asmSelectArgs={"enabled":true} title="Multiple values to choose from"/>
      <@field type="select" items=items name="select1" label="Select 4 (multiple - asmselect - custom - sortable)" currentValue="val3" multiple="multiple" 
        asmSelectArgs={"enabled":true, "title":"Select one of these custom values", "asmSelectOptions":{"addItemTarget":"bottom", "sortable":true}}/>
      <@field type="submit" submitType="submit" text="Submit" onClick="alert('submitted!'); return false;" />
    </@fields>
    </@form>
  </@section>

  <@section title="Default form fields (without label area)">
    <@form name="form2">
    <@fields type="default-nolabels">
      <@field type="input" name="input1"/>
      <@field type="input" name="input2" />
      <@field type="display">Display value</@field>
      <@field type="input" name="input3"/>

      <@field type="radio" name="radio1" label="Radio 1" value="Y"/>
      <@field type="radio" name="radio2" label="Radio 2" value="Y" checked=true/>
      <#assign items = [
        {"value":"val1", "description":"Option 1"}
        {"value":"val2", "description":"Option 2"}
        {"value":"val3", "description":"Option 3"}
      ]>
      <@field type="radio" name="radio3" label="Radio 3 (multi)" items=items currentValue="val1"/>
      <@field type="checkbox" name="checkbox1" label="Checkbox 1" value="Y" />
      <@field type="checkbox" name="checkbox2" label="Checkbox 2" value="Y" checked=true/>
      <@field type="checkbox" name="checkbox3" label="Checkbox 3 (multi)" items=items currentValue=["val2", "val3"]/>
      <#assign items = [
        {"value":"val1", "description":"Value 1"},
        {"value":"val2", "description":"Value 2"},
        {"value":"val3", "description":"Value 3"},
        {"value":"val4", "description":"Value 4"},
        {"value":"val5", "description":"Value 5"}
      ]>
      
      <@field type="submit" submitType="button" text="Submit" events={"click": "alert('submitted!');"} />
    </@fields>
    </@form>
  </@section>

  <@section title="Default form fields (with label area) with parent/child fields">
    <@form name="form3">
      <@field type="generic" label="Multi-fields">
        <@field type="input" name="input1"/>
        <@field type="display">Child display value</@field>
        <@field type="input" name="input2"/>
      </@field>
      <@field type="input" name="input3" label="Regular field" />
      <@field type="display">Regular display field</@field>
      <@field type="submitarea">
        <@field type="submit" submitType="link" text="Save" disabled=true />
        <@field type="submit" submitType="link" text="Cancel" disabled=true />
      </@field>
      <@field type="generic" label="Select one">
        <@field type="radio" name="radio1" label="Radio 1-a" value="val1" />
        <@field type="radio" name="radio1" label="Radio 1-b" value="val2" />
      </@field>
      <@field type="generic" label="Select many">
        <@field type="checkbox" name="checkbox1" label="Checkbox 1-a" value="val1" />
        <@field type="checkbox" name="checkbox1" label="Checkbox 1-b" value="val2" checked=true />
      </@field>
      <@field type="generic">
        <@field type="radio" name="radio2" label="Radio 2-a" value="val1" />
        <@field type="radio" name="radio2" label="Radio 2-b" value="val2" />
      </@field>
    </@form>
  </@section>

  <@section title="Custom arranged form fields">
    <@form name="form4">
    <@fields type="generic">
      <@row>
        <@cell columns=6>
          <@field type="input" name="input1" label="Input 1 and 2" labelArea=true />
        </@cell>
        <@cell columns=6>
          <@field type="input" name="input2" />
        </@cell>
      </@row>
      <@row>
        <@cell columns=6>
          <@field type="display">Display value</@field>
          <@field type="display" labelArea=true>Display value with label area</@field>
        </@cell>
        <@cell columns=6>
          <@field type="input" name="input3" label="Input 3" labelArea=true />
        </@cell>
      </@row>      
      <@row>
        <@cell>
          <@field type="radio" name="radio1" label="Radio 1 (with label area)" value="val1" labelArea=true/>
          <@field type="radio" name="radio2" label="Radio 2 (no label area - inlined label)" value="val2" />
          <@field type="checkbox" name="checkbox1" label="Checkbox 1 (with label area)" value="val1" labelArea=true/>
          <@field type="checkbox" name="checkbox2" label="Checkbox 2 (no label area - inlined label)" value="val2" />
        </@cell>
      </@row>
      <@row>
        <@cell offset=6 columns=6>      
          <@field type="submitarea">
            <@field type="submit" submitType="button" text="Submit" disabled=true />
          </@field>
        </@cell>
      </@row>          
    </@fields>
    </@form>
  </@section>

</@section>

<@section title="Class arguments test">
  <#macro myClassTest class="">
    <#local origClass = class>
    <#local class = addClassArg(class, "macro-required-class-1")>
    <#local class = addClassArgDefault(class, "macro-default-class-1")>
    <#local class = addClassArg(class, "macro-required-class-2")>
    <#local class = addClassArgDefault(class, "macro-default-class-2")>
    <#local classes = compileClassArg(class)>
    <li class="${classes}"><#if origClass?is_string>"${origClass?string}"<#else>${origClass?string}</#if> -> "${classes}"</li>
  </#macro>

  <ul>
    <@myClassTest class="" />
    <@myClassTest class="+" />
    <@myClassTest class="=" />

    <@myClassTest class="+caller-additional-class" />  
    <@myClassTest class="caller-override-class" /> 
    <@myClassTest class="=caller-override-class" /> 
  </ul>
</@section>

<@section title="More grid tests">
  <@row class="+${styles.grid_display!}">
    <@cell offset=6 columns=6>
      offset=6 columns=6
    </@cell>
  </@row>

  <@row class="+${styles.grid_display!}">
    <@cell class="+myclass" smallOffset=3 small=9 largeOffset=7 large=5>
      smallOffset=3 small=9 largeOffset=7 large=5
    </@cell>
  </@row>

  <@row class="+${styles.grid_display!}">
    <@cell class="${styles.grid_large}8" largeOffset=3 small=9 last=true>
      class="${styles.grid_large}8" largeOffset=3 small=9 last=true
    </@cell>
  </@row>

  <@row class="+${styles.grid_display!} myrowclass" alt=true>
    <@cell class="+myclass">
      default
    </@cell>
  </@row>

</@section>

<@section title="FTL request-scope stacks">
  <@section title="Basic stack">
    <#macro printTestStack>
      <@objectAsScript lang="raw" escape=false object=getRequestStackAsList("testStack")!"(none)" />
    </#macro>

    <p>Stack: <@printTestStack /></p>
    <#assign dummy = pushRequestStack("testStack", {"key1":"val1", "key2":"val2"})>
    <p>Stack: <@printTestStack /></p>
    <#assign dummy = pushRequestStack("testStack", {"key3":"val3", "key4":"val4"})>
    <p>Stack: <@printTestStack /></p>
    <#assign dummy = pushRequestStack("testStack", {"key5":"val5", "key6":"val6"})>
    <p>Stack: <@printTestStack /></p>
    <#assign dummy = setLastRequestStack("testStack", {"key7":"val7", "key8":"val8"})>
    <p>Stack with last replaced: <@printTestStack /> </p>
    <#assign dummy = popRequestStack("testStack")>
    <p>Stack: <@printTestStack /></p>
    <#assign dummy = popRequestStack("testStack")>
    <p>Stack: <@printTestStack /></p>
    <#assign dummy = popRequestStack("testStack")>
    <p>Stack: <@printTestStack /></p>
  </@section>
  
  <@section title="Container size stack">
    <#macro printTestContainerSizesAndFactors>
      <p>
        Container sizes: <@objectAsScript lang="raw" escape=false object=getAllContainerSizes()![] /><br/>
        Container factors: <@objectAsScript lang="raw" escape=false object=getAbsContainerSizeFactors()!{} /><br/> 
      </p>
    </#macro>
  
    <@row>
      <@cell class="${styles.grid_large!}10 ${styles.grid_medium!}9 ${styles.grid_small!}8 ${styles.grid_end!}">
        <@printTestContainerSizesAndFactors />
        <@row>
          <@cell class="${styles.grid_large!}7 ${styles.grid_small!}6 ${styles.grid_end!}">
            <@printTestContainerSizesAndFactors />
            <@section class="${styles.grid_small!}11 ${styles.grid_end!}">
              <@printTestContainerSizesAndFactors />
              <@row>
                <@cell class="+testclass">
                  <@printTestContainerSizesAndFactors />
                </@cell>
              </@row>
              <@printTestContainerSizesAndFactors />
            </@section>
            <@printTestContainerSizesAndFactors />
          </@cell>
        </@row>
        <@printTestContainerSizesAndFactors />
      </@cell>
    </@row>
  </@section>
</@section>



