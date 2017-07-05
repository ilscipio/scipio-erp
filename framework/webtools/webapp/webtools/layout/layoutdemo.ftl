
<#-- SCIPIO: TODO: show some cases only if debug mode is on -->
<#if debugMode>
  <@commonMsg type="info">
    Debug mode: ON<br/>
    <small>NOTE: In this mode, for testing purposes, the page intentionally contains elements and styles 
        that may appear broken visually, and provides some examples of things <em>not</em> to do, 
        where noted.</small>
  </@commonMsg>
</#if>

<#--<@nav type="magellan">
    <@mli arrival="breadcrumbs"><a href="#breadcrumbs">Breadcrumbs</a></@mli>
    <@mli arrival="grid"><a href="#grid">Grid</a></@mli>
    <@mli arrival="blockgrid"><a href="#blockgrid">Block-Grid</a></@mli>
    <@mli arrival="buttons"><a href="#buttons">Buttons</a></@mli>
    <@mli arrival="panel"><a href="#panel">Panel</a></@mli>
</@nav>-->

<div ${mtarget("breadcrumbs")} id="breadcrumbs"></div>
<@nav type="breadcrumbs">
    <li class="${styles.nav_breadcrumb!}"><a href="#" class="${styles.nav_breadcrumb_link!}">Home</a></li>
    <li class="${styles.nav_breadcrumb!}"><a href="#" class="${styles.nav_breadcrumb_link!}">Features</a></li>
    <li class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_disabled!}"><a href="#" class="${styles.nav_breadcrumb_link!}">Gene Splicing</a></li>
    <li class="${styles.nav_breadcrumb!} ${styles.nav_breadcrumb_active!}">Cloning</li>
</@nav>

<#-- NOTE: class and cellClass parameters are interchangeable here, but cellClass disambiguates -->
<@section containerClass="+my-container-class" cellClass="${styles.grid_small}12 my-cell-class" containerId="the-id-really-used" id="this-id-is-overridden-but-may-be-used-as-base-for-others"
    contentId="the-inner-div-id" 
    attribs={"my-extra-container-attrib":"test-value"} contentAttribs={"my-extra-content-attrib":attribSpecialVal("none"), "my-extra-content-attrib-2":attribSpecialVal("empty")}>
    <@heading attribs=makeMagTargetAttribMap("grid") id="grid">Grid</@heading>
    <@row class="+${styles.grid_display!}" myExtraRowAttrib="some-value">
        <@cell columns=2 myExtraCellAttrib="some-value">2</@cell>
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

    <!-- Manual open/close rows and cells -->
    <@row open=true close=false class="+${styles.grid_display!}" />
        <@cell open=true close=false columns=6 />6<@cell close=true open=false />
        <@cell open=true close=false columns=6 />6<@cell close=true open=false />
    <@row close=true open=false />

</@section>

<!-- 
<@section title="Special title classes" titleClass="div:+divclass;heading:+extraheadingclass">
</@section>
 -->

<@section id="another-section-id-this-time-used-on-container-and-content">
  <@heading attribs=makeMagTargetAttribMap("blockgrid") id="blockgrid">Tiles</@heading>
  <@section title="Custom per-tile styles (overriding default styles)" relHeadingLevel=+1>
    <@grid type="tiles">  <#-- tilesType="default" -->
        <@tile size="large" color=3 icon="${styles.icon!} ${styles.icon_prefix!}star">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>
        <@tile size="normal" color=7 title="Test" image="https://placehold.it/150x150"></@tile>
        <@tile size="small" color=6 title="" icon="${styles.icon!} ${styles.icon_prefix!}flag"></@tile>
        <@tile size="small" color=3 title="Test" image="https://placehold.it/70x70"></@tile>
        <@tile size="wide" color=0 title="dasdsadsas dasdas" image="https://placehold.it/310x150"></@tile>
        <@tile size="large" color=6 title="Test" image="https://placehold.it/310x310"></@tile>
        <@tile size="wide" color=2 title="Test 3" icon="${styles.icon!} ${styles.icon_prefix!}heart">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>    
        <@tile size="normal" color=2 icon="${styles.icon!} ${styles.icon_prefix!}layout ${styles.icon_prefix!}th-large">My money's in that office, right? If she start giving me some bullshit about it ain't there, and we got to go someplace else and get it, I'm gonna shoot you in the head then and there. Then I'm gonna shoot that bitch in the kneecaps, find out where my goddamn money is. She gonna tell me too. Hey, look at me when I'm talking to you, motherfucker. You listen: we go in there, and that nigga Winston or anybody else is in there, you the first motherfucker to get shot. You understand?</@tile>
        <@tile size="normal" color=5 title="2" icon="${styles.icon!} ${styles.icon_prefix!}music"></@tile>
        <@tile size="small" color=4 title="Test"></@tile>
        <@tile size="small" color=6 title="Test"></@tile>
        <@tile size="small" color=4 title="Test"></@tile>
        <@tile size="small" color=5 title="Test"></@tile>
        <@tile size="normal" color=0 image="https://placehold.it/150x150"></@tile>
        <@tile size="normal" color=1 title="2" icon="${styles.icon!} ${styles.icon_prefix!}like ${styles.icon_prefix!}thumbs-up"></@tile>
    </@grid>
  </@section>
  <@section title="Simple thumbnail image gallery with fixed-size tiles" relHeadingLevel=+1>
    <#-- Simple image gallery using tiles -->
    <@grid type="tiles" tilesType="gallery1">
        <@tile image=makeOfbizContentUrl("/images/products/GZ-1000/small.png") link=makeOfbizContentUrl("/images/products/GZ-1000/large.png")>Image 1 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-1001/small.png") link=makeOfbizContentUrl("/images/products/GZ-1001/large.png") title="Image 2 title">Image 2 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-1004/small.png") link=makeOfbizContentUrl("/images/products/GZ-1004/large.png")>Image 3 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-1005/small.png") link=makeOfbizContentUrl("/images/products/GZ-1005/large.png")>Image 4 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-1006/small.png") link=makeOfbizContentUrl("/images/products/GZ-1006/large.png") title="Image 5 title">Image 5 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-2002/small.png") link=makeOfbizContentUrl("/images/products/GZ-2002/large.png")>Image 6 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-2644/small.png") link=makeOfbizContentUrl("/images/products/GZ-2644/large.png")>Image 7 - click to view full image</@tile>
        <@tile image=makeOfbizContentUrl("/images/products/GZ-5005/small.png") link=makeOfbizContentUrl("/images/products/GZ-5005/large.png")>Image 8 - click to view full image</@tile>
    </@grid>
  </@section>  
</@section>

<@section title="Image container">
    <@row>
        <@cell columns=4><@img src="https://placehold.it/240x800" type="cover" link="#" height="100px" width="100%" />Cover</@cell>
        <@cell columns=4><@img src="https://placehold.it/240x800" type="contain" link="#" height="100px" width="100%" />Contain</@cell>
        <@cell columns=4><@img src="https://placehold.it/240x800" type="none" link="#" height="100px" width="100%" />Automatically adjusted</@cell>
    </@row>    
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
  <@heading extraHeadingAttrib="my-extra-heading-attrib-value">Heading</@heading>
  <@heading relLevel=1>Heading - Relative Level +1</@heading>
  <@heading relLevel=2>Heading - Relative Level +2</@heading>
</@section>

<@section title="Broken-up section" open=true close=false />
  <@section title="Broken-up nested section" open=true close=false />
    [inside]
  <@section close=true open=false />
<@section close=true open=false />

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
<a href="#" class="${styles.button!} ${styles.button_color_primary!}">Primary Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_secondary!}">Secondary Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_alert!}">Alert Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_warning!}">Warning Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_info!}">Info Button</a>
<a href="#" class="${styles.button!} ${styles.button_color_default} ${styles.disabled}">Disabled Button</a>

<@heading relLevel=+1>Button Groups</@heading>
<@menu type="button" class="+my-button-menu-class" id="my-button-menu">
  <@menuitem type="link" text="Menu Button 1" />
  <@menuitem type="link" text="Menu Button 2" contentClass="+${styles.disabled}"/>
  <@menuitem type="link" text="Menu Button 3" contentClass="+${styles.button_color_green}">
    <!-- nested menu item comment -->
  </@menuitem>
  <@menuitem type="generic">
    <!-- NOTE: this will only look right if the outer menu is a button menu of some sort -->
    <@menu type="button-dropdown" title="Sub-menu, as button-dropdown">
      <@menuitem type="link" text="Menu Button 1" />
      <@menuitem type="link" text="Menu Button 2" />
      <@menuitem type="link" text="Menu Button 3" />
    </@menu>
  </@menuitem>
</@menu>

<@menu type="button-dropdown" title="Single dropdown button menu">
  <@menuitem type="link" text="Menu Button 1" />
  <@menuitem type="link" text="Menu Button 2" />
  <@menuitem type="link" text="Menu Button 3" />
</@menu>

<@section title="Step navigation">
    <@nav type="steps">
        <@step completed=true icon="${styles.icon!} ${styles.icon_prefix!}cart">Cart</@step>
        <@step active=true icon="${styles.icon!} ${styles.icon_prefix!}info">Shipping</@step>
        <@step disabled=true icon="${styles.icon!} ${styles.icon_prefix!}heart">Billing</@step>
        <@step disabled=true icon="${styles.icon!} ${styles.icon_prefix!}info">Confirm Order</@step>
    </@nav>
</@section>

<#if debugMode>
<p>Nested sub-menus (markup test only!)</p>
<@menu type="button" class="+my-button-menu-class" id="my-button-menu">
  <@menuitem type="link" text="Menu Button 1" />
  <@menuitem type="link" text="Menu Button 2" contentClass="+${styles.disabled}"/>
  <@menuitem type="link" text="Menu Button 3" contentClass="+${styles.button_color_green}">
    <@menu class="+my-button-submenu-class" id="my-button-menu-sub">
      <@menuitem type="link" text="Menu Button 4" />
      <@menuitem type="link" text="Menu Button 5" contentClass="+${styles.disabled}">
        <@menu class="+my-button-subsubmenu-class" id="my-button-menu-sub-sub">
          <@menuitem type="link" text="Menu Button 7" />
          <@menuitem type="link" text="Menu Button 8" contentClass="+${styles.disabled}"/>
          <@menuitem type="link" text="Menu Button 9" contentClass="+${styles.button_color_green}">
            <!-- nested menu item comment -->
          </@menuitem>
        </@menu>
      </@menuitem>
      <@menuitem type="link" text="Menu Button 6" contentClass="+${styles.button_color_green}"/>
    </@menu>
  </@menuitem>
</@menu>
</#if>
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
    <@heading attribs=makeMagTargetAttribMap("modal") id="modal">Modal</@heading>
    <@row>
        <@cell columns=12>
            <@modal id="uniqueModalId" label="click me">
               Communication is not possible. The shuttle has no power. Using the gravitational pull of a star to slingshot back in time? We are going to Starbase Montgomery for Engineering consultations prompted by minor read-out anomalies. Probes have recorded unusual levels of geological activity in all five planetary systems. Assemble a team. Look at records of the Drema quadrant. Would these scans detect artificial transmissions as well as natural signals?
            </@modal>
        </@cell>
    </@row>
</@section>

<@section>
    <@heading attribs=makeMagTargetAttribMap("slider") id="slider">Slider</@heading>
    <@row>
        <@cell columns=6>
            <@slider id="" class="" controls=true indicator=true>
                <@slide link="#" image="https://placehold.it/800x300">What is the strength of the ship's deflector shields at maximum output? The wormhole's size and short period would make this a local phenomenon.</@slide>
                <@slide title="This is a title" link="#" image="https://placehold.it/800x300"></@slide>
            </@slider>
        </@cell>
        <@cell columns=6></@cell>
    </@row>
</@section>

<@section>
    <@heading attribs=makeMagTargetAttribMap("charts") id="charts" level=2>Charts</@heading>
    <#-- SCIPIO: Deprecated
    <@heading level=3>Foundation</@heading>
    <@row>
        <@cell columns="4">
            <@chart type="pie" library="foundation">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="2" title="Sausage"/> 
                <@chartdata value="19" title="Cheese"/> 
                <@chartdata value="6" title="Chicken"/> 
                <@chartdata value="27" title="Other"/>  
            </@chart>
        </@cell>
        <@cell columns="4">
            <@chart type="bar" library="foundation">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="14" title="Sausage"/> 
                <@chartdata value="8" title="Cheese"/> 
                <@chartdata value="11" title="Chicken"/> 
                <@chartdata value="7" title="Other"/>  
            </@chart>
        </@cell>
        <@cell columns="4">
            <@chart type="line" library="foundation">
                <@chartdata value="36" value2="1" title="Peperoni"/>
                <@chartdata value="2" value2="2" title="Sausage"/> 
                <@chartdata value="19" value2="3" title="Cheese"/> 
                <@chartdata value="6" value2="4" title="Chicken"/> 
                <@chartdata value="27" value2="5" title="Other"/>  
            </@chart>
        </@cell>
    </@row>-->
    <@heading level=3>Chart.js (default)</@heading>
    <@row>
        <@cell columns="4">
            <@chart type="pie" library="chart" label1="Number" label2="Item">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="2" title="Sausage"/> 
                <@chartdata value="19" title="Cheese"/> 
                <@chartdata value="6" title="Chicken"/> 
                <@chartdata value="27" title="Other"/>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <@chart type="bar" library="chart" label1="Number" label2="Item">
                <@chartdata value="36" title="Peperoni"/>
                <@chartdata value="14" title="Sausage"/> 
                <@chartdata value="8" title="Cheese"/> 
                <@chartdata value="11" title="Chicken"/> 
                <@chartdata value="7" title="Other"/>  
            </@chart>
        </@cell>
        
        <@cell columns="4">
            <@chart type="line" library="chart" label1="Number" label2="Item">
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
    <#if debugMode>  
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
    </#if>     
  </@section>    

  <#if debugMode>
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
    
    <#if debugMode>
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
  
          <@tr open=true close=false colspan=3 />
              <@td open=true close=false/>Manual open/close rows and table<@td close=true open=false />
              <@td open=true close=false colspan=2/>
                  <@table type="data-complex" inheritAltRows=true open=true close=false />
                      <@thead open=true close=false /><@tr open=true close=false /><@td open=true close=false/>Header<@td close=true open=false /><@td open=true close=false/>Cell<@td close=true open=false /><@tr close=true open=false /><@thead close=true open=false />
                      <@tbody open=true close=false /><@tr open=true close=false /><@td open=true close=false/>Body<@td close=true open=false /><@td open=true close=false/>Cell<@td close=true open=false /><@tr close=true open=false /><@tbody close=true open=false />
                      <@tfoot open=true close=false /><@tr open=true close=false /><@td open=true close=false/>Footer<@td close=true open=false /><@td open=true close=false/>Cell<@td close=true open=false /><@tr close=true open=false /><@tfoot close=true open=false />
                  <@table close=true open=false />
              <@td close=true open=false />
          <@tr close=true open=false />
  
          <@tr><@td>Regular row</@td><@td>Cell</@td><@td>Cell</@td></@tr>
        </@tbody>
        <@tfoot>
          <@tr><@td colspan=3>Footer</@td></@tr>
        </@tfoot>
      </@table>
    </@section>
    </#if>
  </@section>
  </#if>
</@section>

<@section title="Menus">
  <@menu type="tab">
    <@menuitem type="link" text="Menu Button 1" />
    <@menuitem type="link" text="Menu Button 2" contentClass="+${styles.disabled}"/>
    <@menuitem type="link" text="Menu Button 3" contentClass="+${styles.button_color_green}">
      <!-- nested menu item comment -->
    </@menuitem>
  </@menu>

  <#if debugMode>
  <#assign menuItems = [
    {"type":"link", "text":"Menu Tab 2", "disabled":true},
    {"type":"link", "text":"Menu Tab 1", "href":"ofbizUrl://WebtoolsLayoutDemo"},
    {"type":"link", "text":"Menu Tab 4", "contentClass":"+${styles.button_color_green}", "onClick":"javascript:alert('Clicked menu item!');"},
    {"type":"text", "text":"Menu Tab 3 (text entry)", "nestedContent":"<!-- hidden nested menu item comment -->"},
    {"type":"submit", "text":"Menu Tab 5 (submit)", "disabled":true, "class":"+mymenuitemclass", "contentClass":"+mymenuitemcontentclass"},
    {"type":"link", "text":"Menu Tab 6", "selected":true},
    {"type":"link", "text":"Menu Tab 7", "active":true},
    {"type":"generic", "text":"Menu Tab 8"},
    {"type":"generic", "text":"Menu Tab 9", "contentClass":"+thisclassaddsacontainer"}
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
  </#if>
</@section>

<@section title="Tabs">
    <@row>
        <@cell columns="6">
            <@tabs type="vertical" title="Horizontal">
                <@tab title="Tab 1">Logic is the beginning of wisdom, not the end.</@tab>
                <@tab title="Tab 2">"Do not grieve, Admiral. It was logical. The needs of the many outweigh 'The needs of the few.'", Spock grimaces, nods. "Or the one"</@tab>
                <@tab title="Tab 3">Do you know the old Klingon proverb that revenge is a dish best served cold? It is very cold - in space</@tab>
            </@tabs>
        </@cell>
        <@cell columns="6">
            <@tabs type="horizontal" title="Vertical">
                <@tab title="Tab 1">Logic is the beginning of wisdom, not the end.</@tab>
                <@tab title="Tab 2">"Do not grieve, Admiral. It was logical. The needs of the many outweigh 'The needs of the few.'", Spock grimaces, nods. "Or the one"</@tab>
                <@tab title="Tab 3">Do you know the old Klingon proverb that revenge is a dish best served cold? It is very cold - in space</@tab>
            </@tabs>
        </@cell>
    </@row>
</@section>

<#-- The titleStyle usage here is a demo of what can be set in <label style="..." /> in screens,
     usually don't need in @section macro. See outputted markup for results. -->
<#if debugMode>
<@section title="Macro Test">
    <@section title="Nested section title" titleClass="heading+2:+test-additional-section-title-class">
    </@section>
    <@row class="+test-additional-row-class">
      <@cell class="+test-additional-cell-class">
        In a cell
      </@cell>
    </@row>
    <@row>
      <@cell columns=9>
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
</#if>

<#-- javascript test; entitymaint should only appear once in output... -->
<#if debugMode>
<@script>
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "entitymaint" />
<@requireScriptOfbizUrl "ServiceList" />
</@script>
</#if>

<@section title="Fields">
  <#-- TODO: submitarea -> submit (but not to remove submitarea; still important) -->
  <@section title="Default form fields (with label area) (@fields type=\"default\")">
    <@form name="form1">
    <@fields type="default"> <#-- see styles.fields_default_xxx. note: @fields currently optional for type="default"-->
      <@field type="input" name="input1" label="Input 1" />
      <@field type="input" name="input2" label="Input 2" postfix=true />
      <@field type="input" name="input2b" label="Input 2b" postfix=true postfixColumns=2/>
      <@field type="display">Display value</@field>
      <@field type="input" name="input3" label="Input 3" />
      <@field type="input" name="input3nolabel" />

      <@field type="lookup" name="lookup1" label="Lookup 1 (Geo)" formName="form1" fieldFormName="LookupGeo" tooltip="Lookup geos!"/>
      <#macro custLabelAreaTest args={}>
        <label for="${args.fieldId}"><strong>${args.test1} (required and custom label content)</strong></label><#t>
      </#macro>
      <@field type="input" name="input4" labelContent=custLabelAreaTest labelContentArgs={"test1":"Input 4"} required=true />
      <@field type="input" name="input5" label="Input 5 (required, tooltip force-disabled)" required=true tooltip=false/>

      <@field type="datetime" label="Date 1 (timestamp)" name="date1" value="" size="25" maxlength="30" dateType="date-time" />
      <@field type="datetime" label="Date 2 (short date)" name="date2" value="" size="25" maxlength="30" dateType="date" />
      <@field type="datetime" label="Date 3 (time only)" name="date3" value="" size="25" maxlength="30" dateType="time" />
      <@field type="datetime" label="Date 4 (timestamp internal, displayed as short date)" name="date4" value="" size="25" maxlength="30" dateType="date-time" dateDisplayType="date"/>

      <@field type="datefind" label="Date Find 1 (timestamp)" name="datefind1" value="" size="25" maxlength="30" dateType="date-time" />
      <@field type="datefind" label="Date Find 2 (short date)" name="datefind2" value="" size="25" maxlength="30" dateType="date" />
      <@field type="datefind" label="Date Find 3 (time only)" name="datefind3" value="" size="25" maxlength="30" dateType="time" opValue="greaterThanFromDayStart" />
      <@field type="datefind" label="Date Find 4 (timestamp internal, displayed as short date)" name="datefind4" value="" size="25" maxlength="30" dateType="date-time" dateDisplayType="date" opValue="equals" />

      <@field type="textfind" label="Text Find 1" name="textfind1" value="" size="25" maxlength="30" opValue="like" />
      <@field type="textfind" label="Text Find 2" name="textfind2" value="" size="25" maxlength="30" hideOptions=true ignoreCaseValue=false tooltip="this is a tooltip!" />
      <@field type="textfind" label="Text Find 3" name="textfind3" id="mytextfind3" value="" hideIgnoreCase=true />

      <@field type="rangefind" label="Range Find 1" name="rangefind1" value="" size="25" maxlength="30" opValue="contains" />
      <@field type="rangefind" label="Range Find 2" name="rangefind2" value="" size="25" maxlength="30" opFromValue="greaterThan" opThruValue="lessThanEqualTo" tooltip="this is a tooltip!" />
      <@field type="rangefind" label="Range Find 3" name="rangefind3" id="myrangefind3" value="test" />

      <@field type="radio" name="radio1" label="Radio 1" value="Y"/>
      <@field type="radio" name="radio1disabled" label="Radio 1 (disabled)" value="Y" disabled=true/>
      <@field type="radio" name="radio2" label="Radio 2" value="Y" checked=true/>
      <#assign items = [
        {"value":"val1", "description":"Option 1"}
        {"value":"val2", "description":"Option 2"}
        {"value":"val3", "description":"Option 3", "tooltip":"this is radio option 3"}
      ]>
      <@field type="radio" name="radio3a" label="Radio 3a (multi - inline)" items=items currentValue="val2" tooltip="these are radios"/>
      <@field type="radio" name="radio3b" label="Radio 3b (multi - one per line)" items=items currentValue="val2" inlineItems=false tooltip="these are also radios"/>
      <@field type="radio" name="radio4" label="Radio 4" value="Y" currentValue="Y" />
      <@field type="radio" name="radio5" label="Radio 5" value="Y" currentValue="N" />
      <@field type="radio" name="radio6" label="Radio 6" value="Y" defaultValue="Y" />
      <@field type="radio" name="radio7" label="Radio 7" value="Y" defaultValue="N" />
      <@field type="checkbox" name="checkbox1" label="Checkbox 1" value="Y" />
      <@field type="checkbox" name="checkbox1disabled" label="Checkbox 1 (disabled)" value="Y" disabled=true/>
      <@field type="checkbox" name="checkboxInd1b" label="Indicator checkbox (full Y/N value support)" value="Y" altValue="N" />
      <@field type="checkbox" name="checkboxInd1c" label="Indicator checkbox 2 (full Y/N value support)" valueType="indicator" />
      <@field type="checkbox" name="checkbox2" label="Checkbox 2" value="Y" checked=true/>
      <#assign items = [
        {"value":"val1", "description":"Option 1"}
        {"value":"val2", "description":"Option 2"}
        {"value":"val3", "description":"Option 3", "tooltip":"this is checkbox option 3"}
      ]>
      <@field type="checkbox" name="checkbox3a" label="Checkbox 3a (multi - inline)" items=items currentValue=["val2", "val3"] inlineItems=true tooltip="these are checkboxes"/>
      <@field type="checkbox" name="checkbox3b" label="Checkbox 3b (multi - one per line)" items=items currentValue=["val2", "val3"] inlineItems=false tooltip="these are checkboxes"/>
      <@field type="checkbox" name="checkbox3a1" label="Checkbox 3a1 (multi - inline - forced to simple-standard look)" items=items currentValue=["val2", "val3"] inlineItems=true tooltip="these are checkboxes" checkboxType="simple-standard"/>

      <@field type="checkbox" name="checkbox4" label="Checkbox 4" value="Y" currentValue="Y" />
      <@field type="checkbox" name="checkbox5" label="Checkbox 5" value="Y" currentValue="N" tooltip="this is checkbox option 5"/>
      <@field type="checkbox" name="checkbox6" label="Checkbox 6" value="Y" defaultValue="Y" />
      <@field type="checkbox" name="checkbox7" label="Checkbox 7" value="Y" defaultValue="N" />

      <@field type="generic" label="Radios in generic field">
        <@fields inlineItems=false>
        <@field type="radio" name="radio22" label="Radio 22" value="TEST1" />
        <#if debugMode>
        <@field type="radio" name="radio22" label="Radio 23" value="TEST2" />
        <#assign veryLongLabel>
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
        </#assign>
        <@field type="radio" name="radio22" label=veryLongLabel value="TEST3" />
        <@field type="radio" name="radio22" label="Radio 24" value="TEST4" />
        </#if>
        </@fields>
      </@field>
  
      <@field type="generic" label="Checkboxes (default markup, one per line) in generic field">
        <@fields inlineItems=false>
        <@field type="checkbox" name="checkbox22" label="Checkbox 22" value="TEST1" />
        <#if debugMode>
        <@field type="checkbox" name="checkbox23" label="Checkbox 23" value="TEST2" />
        <#assign veryLongLabel>
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
        </#assign>
        <@field type="checkbox" name="checkbox24" label=veryLongLabel value="TEST3" />
        <@field type="checkbox" name="checkbox24" label="Checkbox 24" value="TEST4" />
        </#if>
        </@fields>
      </@field>
      
      <@field type="generic" label="Checkboxes (simple-standard - forced, one per line) in generic field">
        <@fields inlineItems=false checkboxType="simple-standard">
        <@field type="checkbox" name="checkbox32" label="Checkbox 32" value="TEST1" />
        <#if debugMode>
        <@field type="checkbox" name="checkbox33" label="Checkbox 33" value="TEST2" />
        <#assign veryLongLabel>
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
        </#assign>
        <@field type="checkbox" name="checkbox34" label=veryLongLabel value="TEST3" />
        <@field type="checkbox" name="checkbox34" label="Checkbox 34" value="TEST4" />
        </#if>
        </@fields>
      </@field>

      <@field type="generic" label="Checkboxes (simple - forced, one per line) in generic field">
        <@fields inlineItems=false checkboxType="simple">
        <@field type="checkbox" name="checkbox52" label="Checkbox 52" value="TEST1" />
        <#if debugMode>
        <@field type="checkbox" name="checkbox53" label="Checkbox 53" value="TEST2" />
        <#assign veryLongLabel>
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
            This is a very long label. This is a very long label. This is a very long label. This is a very long label. 
        </#assign>
        <@field type="checkbox" name="checkbox54" label=veryLongLabel value="TEST3" />
        <@field type="checkbox" name="checkbox54" label="Checkbox 54" value="TEST4" />
        </#if>
        </@fields>
      </@field>

      <@field type="textarea" name="textarea1" label="Textarea 1" />
      <@field type="textarea" name="textarea2" label="Textarea 2 (readonly)" readonly=true text="test"/>

      <#assign items = [
        {"value":"val1", "description":"Value 1"},
        {"value":"val2", "description":"Value 2"},
        {"value":"val3", "description":"Value 3"},
        {"value":"val4", "description":"Value 4"},
        {"value":"val5", "description":"Value 5"}
      ]>
      <@field type="select" items=items name="select1" label="Select 1" currentValue="val2" />
      <@field type="select" items=items name="select1" label="Select 2" currentValue="val2" currentFirst=true currentDescription="THE FIRST"/>
      <@field type="select" items=items name="select1" label="Select 2 (current first)" currentValue="val2" currentFirst=true />
      <@field type="select" items=items name="select1" label="Select 3" currentValue="val2" allowEmpty=true />
      <@field type="select" items=items name="select1" label="Select 3 (allow empty)" allowEmpty=true />
      <#assign items = [
        {"value":"val1", "description":"Value 1"},
        {"value":"val2", "description":"Value 2"},
        {"value":"val3", "description":"Value 3", "selected":true},
        {"value":"val4", "description":"Value 4"},
        {"value":"val5", "description":"Value 5", "selected":true}
      ]>
      <@field type="select" items=items name="select1" label="Select 4 (multiple)" currentValue="val3" multiple=true />
      <@field type="select" items=items name="select1" label="Select 4 (multiple - dynamic select - default)" currentValue="val3" multiple=true dynSelectArgs={"enabled":true} title="Multiple values to choose from"/>
      <@field type="select" items=items name="select1" label="Select 4 (multiple - dynamic select - custom - sortable)" currentValue="val3" multiple=true 
        dynSelectArgs={"enabled":true, "title":"Select one of these custom values", "sortable":true}/>
     
      <@field type="display" label="Display Field 1" tooltip="This is a tooltip text" formatText=true>
        This 
        is 
        text 
        with 
        newlines
      </@field>
      <@field type="display" label="Display Field 2" formatText=false>
        This 
        is 
        text 
        with 
        no
        newlines
      </@field>
      <@field type="display" label="Display Field 3">
        Text with <b>markup</b>
      </@field>
      <@field type="display" label="Display Field 4" value="This text <b>should be</b> html-escaped"/>
     
      <@field type="generic" label="Generic 1" tooltip="This is a tooltip text">
        This is <p>random text</p> and 
        <div>
            <span>elements</span>. There is an inlined input
            <@field type="input" inline=true size=5 name="input99" />
            in this sentence. 
            There is also a tooltip on the whole generic field.
        </div>
      </@field>
      <@field type="generic" label="Generic 2" value="This text <b>should be</b> html-escaped"/>

      <@field type="submit" submitType="submit" text="Submit" onClick="alert('submitted!'); return false;" />
    </@fields>
    </@form>
  </@section>

  <#if debugMode>
  <@section title="Default form fields (without label area) (@fields type=\"default-nolabelarea\")">
    <@form name="form2">
    <@fields type="default-nolabelarea"> <#-- see styles.fields_default_nolabelarea_xxx -->
      <@field type="input" name="input1"/>
      <@field type="input" name="input2" postfix=true />
      <@field type="display">Display value</@field>
      <#assign postfixContent><span>postfix!</span></#assign>
      <@field type="input" name="input3" postfix=true postfixContent=postfixContent />

      <@field type="datetime" name="date1" value="" size="25" maxlength="30" dateType="date-time" />
      <@field type="datetime" name="date2" label="Date 2" value="" size="25" maxlength="30" dateType="date-time" />

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
  </#if>

  <#if debugMode>
  <@section title="Default form fields (with label area) with parent/child fields (@fields type=\"default\")">
    <@form name="form3"> <#-- see styles.fields_default_xxx -->
      <@field type="generic" label="Multi-fields">
        <@field type="input" name="input1"/>
        <@field type="display">Child display value</@field>
        <@field type="input" name="input2"/>
      </@field>
      <@field type="input" name="input3" label="Regular field" />
      <@field type="display">Regular display field</@field>

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
      <@field type="generic" label="Dates">
        <@field type="datetime" label="Date 1" name="date1" value="" size="25" maxlength="30" dateType="date-time" />
        <@field type="datetime" label="Date 2" name="date2" value="" size="25" maxlength="30" dateType="date-time" />
      </@field>
      <@field type="submitarea">
        <@field type="submit" submitType="link" text="Save" disabled=true />
        <@field type="submit" submitType="link" text="Cancel" disabled=true />
      </@field>
    </@form>
  </@section>
  </#if>

  <#if debugMode>
  <@section title="Default compact form fields (label area at top, for cramped forms) (@fields type=\"default-compact\")">
    <@row>
      <@cell small=4 last=true>
    <@form name="form4">
    <@fields type="default-compact"> <#-- see styles.fields_default_compact_xxx -->
      <@field type="input" name="input1" label="Input 1" />
      <@field type="input" name="input2" label="Input 2" postfix=true />
      <@field type="display">Display value</@field>
      <@field type="input" name="input3" label="Input 3" />
      <@field type="input" name="input4" label="Input 4" labelDetail="[extra label detail]" />
      <@field type="radio" name="radio1" label="Radio 1" value="val2" />
      <@field type="checkbox" name="checkbox1" label="Checkbox 1" value="val2" />
      <@field type="checkbox" name="checkbox1" label="Checkbox 2" value="val2" />
      <@field type="submit" submitType="button" text="Submit" events={"click": "alert('submitted!');"} />
      <@field type="datetime" label="Date 1" name="date1" value="" size="25" maxlength="30" dateType="date-time" />
      <@field type="datetime" label="Date 2" name="date2" value="" size="25" maxlength="30" dateType="date-time" />
    </@fields>
    </@form>
      </@cell>
    </@row>
  </@section>
  </#if>

  <#if debugMode>
  <@section title="Custom arranged form fields (@fields type=\"default-manual\")">
    <#-- NOTE: To get labels on custom (@fields group type "generic") fields, 
        must enable label area manually using labelArea=true. presence of label argument is not enough
        because label argument may be intended as an inline label (for default arrangements, this is
        controlled in styles hash as needed, but for generic fields, it should always be left to caller). -->
    <@form name="form5">
    <@fields type="default-manual"> <#-- see styles.fields_generic_xxx -->
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
          <span>Radios:</span>
          <@field type="radio" name="radio1" label="Radio 1 (with label area)" value="val1" labelArea=true/>
          <@field type="radio" name="radio2" label="Radio 2 (no label area - inlined label)" value="val2" />
          <#-- Radio with auto-collapsing "pseudo" inline label (rare) - here the inline label, underneath, is auto-implemented
              using the label area (so radio widget does not receive an inline label): -->
          <span>Intentionally ugly radio with pseudo-inline label (rare):</span>
          <@field type="radio" name="radio3" label="Radio 3" collapsedInlineLabel=true value="val3" />
          <br/>
          <span>Checkboxes:</span>
          <@field type="checkbox" name="checkbox1" label="Checkbox 1 (with label area)" value="val1" labelArea=true/>
          <@field type="checkbox" name="checkbox2" label="Checkbox 2 (no label area - inlined label)" value="val2" />
          <#-- Checkbox with auto-collapsing "pseudo" inline label (rare): -->
          <span>Intentionally ugly checkbox with pseudo-inline label (rare):</span>
          <@field type="checkbox" name="checkbox3" label="Checkbox 3" collapsedInlineLabel=true value="val3" />
        </@cell>
      </@row>
      <@row>
        <@cell offset=6 columns=6>     
          <@field type="datetime" name="date1" value="" size="25" maxlength="30" dateType="date-time" />
          <@field type="datetime" name="date2" labelArea=true value="" size="25" maxlength="30" dateType="date-time" />
          <@field type="datetime" name="date3" labelArea=true label="Date 3" value="" size="25" maxlength="30" dateType="date-time" />
          <#-- for "generic" @fields, needs explicit collapse (see styles hash) -->
          <span>Date with collapsed label:</span>
          <@field type="datetime" name="date4" labelArea=true label="Date 4" value="" size="25" maxlength="30" dateType="date-time" collapse=true />
          <span>Date with inlined label - auto-implements inline label with collapsed label area (should look same as previous):</span>
          <@field type="datetime" name="date5" labelArea=false collapsedInlineLabel=true label="Date 5" value="" size="25" maxlength="30" dateType="date-time" collapse=true />
          <span>Date with forced inlined label (should look same as previous):</span>
          <@field type="datetime" name="date6" inlineLabelArea=true inlineLabel="Date 6" collapsedInlineLabel=true value="" size="25" maxlength="30" dateType="date-time" collapse=true />
        </@cell>
      </@row>  
      
      <@row class="+form-field-row"> <#-- ofbiz widget-like markup -->
        <@cell>    
          <@row>
            <@cell columns=4 offset=4>      
              <@field type="input" name="input4" label="Input 4" labelArea=true />
            </@cell>
            <@cell columns=4 last=true>      
              <@field type="submit" submitType="link" text="Submit" href=false onClick="alert('submitted!');" />
            </@cell>
          </@row>   
        </@cell>
      </@row>      
    </@fields>
    </@form>
  </@section>
  </#if>
  <#if debugMode>
  <@section title="Widget-only custom form fields, no containers (@fields type=\"default-manual-widgetonly\")">
    <@form name="form6">
    <@fields type="default-manual-widgetonly">
      <@row>
        <@cell columns=6>
          <@field type="input" name="input1" />
        </@cell>
        <@cell columns=6>
          <@field type="input" name="input2" />
        </@cell>
      </@row>
      <@row>
        <@cell>
          <span>Radios:</span>
          <@field type="radio" name="radio1" label="Radio 1" value="val1"/>
          <br/>
          <span>Checkboxes:</span>
          <@field type="checkbox" name="checkbox1" label="Checkbox 1" value="val1" />
        </@cell>
      </@row>
    </@fields>
      <@row>
        <@cell>
          <@field type="checkbox" name="checkbox2" label="Single checkbox with container false" value="val1" widgetOnly=true />
        </@cell>
      </@row>
      <@row>
        <@cell>
          <@field type="checkbox" name="checkbox3" label="Single checkbox with container false but label area force back on (this should look wrong)" value="val1" widgetOnly=true labelArea=true />
        </@cell>
      </@row>
      <@row>
        <@cell>
          <@field type="checkbox" fieldsType="default-manual-widgetonly" name="checkbox4" label="Single checkbox with own fields type (convenience arg)" value="val1" />
        </@cell>
      </@row>
    </@form>
  </@section>
  </#if>
  <#if debugMode>
  <@section title="Variable-size fields">
    <@form name="form7">
    <@fields type="default">
      <@field type="input" name="input1" label="Input" value="val1" />
      <@field type="input" name="input2" label="Input" value="val1" totalColumns=11 />
      <@field type="input" name="input3" label="Input" value="val1" labelColumns=4 />
      <@field type="input" name="input4" label="Input" value="val1" labelColumns=3 totalColumns=11 />
 
      <@field type="input" name="input5" label="Input" value="val1" postfix=true />
      <@field type="input" name="input6" label="Input" value="val1" totalColumns=11 postfix=true />
      <@field type="input" name="input7" label="Input" value="val1" labelColumns=4 postfix=true />
      <@field type="input" name="input8" label="Input" value="val1" labelColumns=3 totalColumns=11 postfix=true />

      <#-- NOTE: widgetPostfixCombined is an override and usually should not be specified; here for testing -->
      <p>NOTE: The next fields are for testing only, and may not look right (widget and postfix don't have a combined container)</p>
      <@field type="input" name="input9" label="Input" value="val1" postfix=true widgetPostfixCombined=false />
      <@field type="input" name="input10" label="Input" value="val1" totalColumns=11 postfix=true widgetPostfixCombined=false />
      <@field type="input" name="input11" label="Input" value="val1" labelColumns=4 postfix=true widgetPostfixCombined=false />
      <@field type="input" name="input12" label="Input" value="val1" labelColumns=3 totalColumns=11 postfix=true widgetPostfixCombined=false />

    <@fields fieldArgs={"totalColumns":11}>
      <p>Group of fields with fewer than 12 total columns</p>
      <@field type="input" name="input13" label="Input" value="val1" />
      <@field type="input" name="input14" label="Input" value="val1" />
      <@field type="input" name="input15" label="Input" value="val1" />
      <@field type="input" name="input16" label="Input" value="val1" />
    </@fields>

    </@fields>
    </@form>
  </@section>
  </#if>
  <#if debugMode>
  <@section title="@fields container inheritance test (NOTE: default is inherit-all)">
    <@form name="form8">
      <@fields>
        <@field type="input" name="input1" label="inherit-all, no effect" />
      </@fields>
      <@fields>
        <@fields type="inherit-all"> 
          <@field type="input" name="input2" label="inherit-all, no effect" />
        </@fields>
      </@fields>

      <@fields type="default-compact">
        <@fields type="inherit-all"> 
          <@field type="input" name="input3" label="default-compact + inherit-all" />
        </@fields>
      </@fields>
      <@fields type="default-compact" fieldArgs={"required":true}>
        <@fields type="inherit-all" fieldArgs={"tooltip":"extra tooltip"}> 
          <@field type="input" name="input3" label="required + extra tooltip" />
        </@fields>
      </@fields>
      <@fields type="default-compact" fieldArgs={"required":true}>
        <@fields type="inherit" fieldArgs={"tooltip":"extra tooltip"}> 
          <@field type="input" name="input3" label="inherit type only (required off)" />
        </@fields>
      </@fields>
    </@form>
  </@section>
  </#if>
</@section>

<#if debugMode>
<a name="AutoValueFormFields"></a>
<@section title="Auto-Value Form Fields">
  <#assign autoformAction><@ofbizUrl>WebtoolsLayoutDemo<#if debugMode>?debugMode=true</#if></@ofbizUrl></#assign>
  <#macro simulateErrorField>
    <#-- NOTE: this field is for demo usage only, bypasses auto-value -->
    <@field type="checkbox" autoValue=false name="simulateError" label="Simulate Error" value="Y" checked=((parameters.simulateError!"") == "Y") />
  </#macro>

  <a name="AutoValueForm1"></a>
  <@section title="Default (params-or-record), with record 1">
    <p>On success, this form forgets what you just submitted. If Simulate Error is checked, 
        it should retain your parameters.</p>
    <form name="autoform1" method="post" action="${autoformAction}#AutoValueForm1">
      <#assign record = {
        "input1":353,
        "input2":"record value 2",
        "input3":"record value 3",
        "input5":"record value 5 (you should never see this)"
      }>
      <#assign defaults = {
        "input3":"default value 3 (you should never see this)",
        "input4":"default value 4"
      }>
      <#assign overrides = {
        "input5":"override value 5 (will always override what you entered)"
      }>
      <#assign autoValueArgs = {"record":record, "defaults":defaults, "overrides":overrides}>
      <#if (parameters.simulateError!"") == "Y">
        <#assign autoValueArgs = autoValueArgs + {"submitError":true}>
      </#if>
      <@fields autoValue=autoValueArgs>
        <@simulateErrorField />
        <@field type="input" name="input1" label="Input 1" />
        <@field type="input" name="input2" label="Input 2" />
        <@field type="input" name="input3" label="Input 3" />
        <@field type="input" name="input4" label="Input 4 (with default/fallback)" />
        <@field type="input" name="input5" label="Input 5 (forced override, always)" />
        <@field type="submit" />
      </@fields>
    </form>
  </@section>
</@section>
</#if>

<#if debugMode>
<a name="ajax-render-test"></a>
<@section title="AJAX Render Test">
<#assign ajaxRenderTestPresets = {
    "MANUAL": {
        "title": "Manual",
        "defaultPreset": true
    },
    "FULL1": { 
        "title": "Full page",
        "requestUri": makeOfbizUrl("ajaxRender"), 
        "view": "runService" 
    },
    "PARTSECT1": {
        "title": "Partial page, widget section",
        "requestUri": makeOfbizUrl("ajaxRender"), 
        "view": "runService", 
        "scpRenderTargetExpr": "$Global-Column-Main"
    },  
    "PARTCONTAINERNONOPT1": {
        "title": "Partial page, widget container, NON-optimized",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "runService",
        "scpRenderTargetExpr": "#main-content"
    },  
    "PARTCONTAINERCOMPLEX1": {
        "title": "Partial page, widget container, with section execution optimization",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "runService",
        "scpRenderTargetExpr": "$Global-Column-Main #main-content" 
    },  
    "PARTCONTAINERCOMPLEXJS1": {
        "title": "Partial page, widget container, plus jQuery sub-element extraction",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "runService",
        "scpRenderTargetExpr": "$Global-Column-Main #main-content",
        "jQueryElemExpr": "#screenlet_1"
    },
    "PARTFTLBOUNDARY1": {
        "title": "Partial page, non-optimized, deep section across FTL boundary test",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$TR-Widget-Deep-Section-2" 
    },
    "PARTFTLBOUNDARY2": {
        "title": "Partial page, part-optimized, deep section across FTL boundary test",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main $TR-SubDec-Section-2 #tr-widget-container-3" 
    },
    "PARTFTLSEL1": {
        "title": "Partial page, LIMITED FTL macro selection test (@container)",
        "description": "NOTE: %screenlet[id=tr-ftl-section-2] also matches Freemarker @section invocations (platform-agnostic)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main $TR-Widget-Section-1 %screenlet[id=tr-ftl-section-2] #tr-ftl-container-1" 
    },
    "PARTFTLSEL2": {
        "title": "Partial page, LIMITED FTL macro selection test (@virtualSection)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main $TR-Widget-Section-1 $TR-FTL-VirtualSection-1" 
    },
    "PARTFTLSEL2b": {
        "title": "Partial page, LIMITED FTL macro selection test 2 (@virtualSection)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main $TR-Widget-Section-1 %section[name='TR-FTL-VirtualSection-1']" 
    },
    "PARTFTLSEL3": {
        "title": "Partial page, LIMITED FTL macro selection test (@form)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main #tr-ftl-form-1" 
    },
    "PARTFTLSEL4": {
        "title": "Partial page, LIMITED FTL macro selection test (widget form within FTL)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main #TargetedRenderingTestForm1" 
    },
    "PARTFTLSEL5": {
        "title": "Partial page, LIMITED FTL macro selection test (@table)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main #tr-ftl-table-1" 
    },
    "PARTFTLSEL6": {
        "title": "Partial page, LIMITED FTL macro selection test (@menu)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main #tr-ftl-menu-1" 
    },
    "PARTFTLSEL7": {
        "title": "Partial page, LIMITED FTL macro selection test (widget menu within FTL)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "$Global-Column-Main #TargetedRenderingTestMenu1" 
    },
    "PARTDECSEL1": {
        "title": "Partial page, special decorator selection test 1",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "^decorator-screen $Global-Column-Main ^/body" 
    },
    "PARTDECSEL2": {
        "title": "Partial page, special decorator selection test 2 (complex)",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "^decorator-screen[name=CommonWebtoolsAppDecorator] ^decorator-screen[name=main-decorator] $Global-Column-Main ^/body ^/body ^decorator-screen[name=TargetedRenderingTestSubDecorator] ^/body #tr-widget-container-3" 
    },
    "PARTREQURI1": {
        "title": "Request URI test, partial page",
        "requestUri": makeOfbizUrl("TargetedRenderingTest"),
        "scpRenderTargetExpr": "$Global-Column-Main" 
    },
    "PARTREQURI2": {
        "title": "Request URI test, partial page, but FULL page for login and error pages",
        "requestUri": makeOfbizUrl("TargetedRenderingTest"),
        "scpRenderTargetExpr": "$Global-Column-Main",
        "scpLoginRenderTargetExpr": "%screen", <#-- get the full login page -->
        "scpErrorRenderTargetExpr": "%screen"  <#-- get the full error page -->
    },
    "PARTREQURI3": {
        "title": "Request URI test, partial page, but FULL page for login or error pages, and test event throws exception (NOTE: does NOT give error page)",
        "requestUri": makeOfbizUrl("TargetedRenderingTest"),
        "scpRenderTargetExpr": "$Global-Column-Main",
        "scpLoginRenderTargetExpr": "%screen", <#-- get the full login page -->
        "scpErrorRenderTargetExpr": "%screen",  <#-- get the full error page -->
        "extraParams": "testEventResult=exception"
    },
    "PARTREQURI4": {
        "title": "Request URI test, partial page, but FULL page for login or error pages, and we call invalid request which gives error page",
        "requestUri": makeOfbizUrl("TargetedRenderingTest")?replace("TargetedRenderingTest", "TargetedRenderingInvalidRequestTest"),
        "scpRenderTargetExpr": "$Global-Column-Main",
        "scpLoginRenderTargetExpr": "%screen", <#-- get the full login page -->
        "scpErrorRenderTargetExpr": "%screen"  <#-- get the full error page -->
    },
    "PARTMULTI1": {
        "title": "Partial page, multi-select test: left & main columns",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "+multi:main-column:$Global-Column-Main,left-column:$Global-Column-Left" 
    },
    "PARTMULTI2": {
        "title": "Partial page, multi-select test: left & main columns, with menu extract by CommonSideBarMenu section name",
        "description": "NOTE: WE CANNOT SET ID on the main <menu> def to reference here ($Global-Column-Left #some-menu-id)
            because metro theme includes it TWICE causing duplicate ID!!!",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "+multi:main-column:$Global-Column-Main,common-sidebar-menu:$Global-Column-Left $Global-CommonSideBarMenu"
    },
    "PARTMULTI2b": {
        "title": "Partial page, multi-select test: left & main columns, with menu extract by element name",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "+multi:main-column:$Global-Column-Main,common-sidebar-menu:$Global-Column-Left $Global-CommonSideBarMenu %menu"
    },
    "PARTMULTI3": {
        "title": "Partial page, multi-select test: left & main columns, with jQuery extract of main menu",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "+multi:main-column:$Global-Column-Main,left-column:$Global-Column-Left",
        "jQueryElemExpr": "+multi:left-column:ul.menu-level-1"
    },
    "PARTMULTI4": {
        "title": "Partial page, multi-select test: nesting support",
        "requestUri": makeOfbizUrl("ajaxRender"),
        "view": "TargetedRenderingTest",
        "scpRenderTargetExpr": "+multi: parent-container: $TR-SubDec-Section-Top, sub-container-1 : $TR-SubDec-Section-Top $tr-subdec-ftl-virtual-1, sub-container-2 : $TR-SubDec-Section-Top $tr-subdec-ftl-virtual-2" 
    }
}>
  <@script>
    function getAjaxRenderOut(renderOut) {
        return renderOut;
    }
    function getAjaxRenderOutWithJQuerySub(renderOut, jQueryElemExpr) {
        var out = jQuery(jQueryElemExpr, jQuery(renderOut));
        if (out.length) {
            return out.wrap('<div/>').parent().html();
        } else {
            return "ERROR: element '" + jQueryElemExpr + "' not found.\n\n-------------------------------\nReturned output:\n-------------------------------\n" + renderOut;
        }
    }
    
    function runAjaxRenderTest(url, params, outputElemId, renderOutProcessCb, renderOutProcessCbMap) {
        var outElem = jQuery('#'+outputElemId);
        outElem.val("(LOADING...)");
        jQuery.ajax({
            url: url,
            type: 'POST',
            data: params,
            success: function(data) {
                var outElem = jQuery('#'+outputElemId);
                
                var processRenderOut = function(renderOut) {
                    if (jQuery.type(renderOut) === 'object') {
                        if (jQuery.isEmptyObject(renderOut)) {
                            return "NOTHING MATCHED OR UNRECOGNIZED ERROR: MULTI RENDEROUT OBJECT WAS EMPTY";
                        }
                        var out = "MULTI TARGET MATCH STATUS: " + Object.keys(renderOut).length + " matching targets found.";
                        var count = 1;
                        jQuery.each(renderOut, function(k, v) {
                            out += "\n\n---------------------------------------------------------------\n";
                            out += "MULTI TARGET MATCH " + count + ": " + k + "\n";
                            out += "---------------------------------------------------------------\n";
                            if (renderOutProcessCbMap && typeof renderOutProcessCbMap[k] !== 'undefined') {
                                out += renderOutProcessCbMap[k](v);
                            } else {
                                out += renderOutProcessCb(v);
                            }
                            count++;
                        });
                        return out;
                    } else {
                        return renderOutProcessCb(renderOut);
                    }
                };

                var out = "";
                if (data._ERROR_MESSAGE_ || data._ERROR_MESSAGE_LIST_) {
                    // TODO: (data._ERROR_MESSAGE_LIST_) 
                    if (data._ERROR_MESSAGE_) {
                        out = "ERROR MESSAGE: " + data._ERROR_MESSAGE_;
                    } else {
                        out = "ERROR MESSAGE (first from list): " + data._ERROR_MESSAGE_LIST_[0];
                    }
                    if (data.renderOut) {
                        out += "\n\n---------------------------------------------------------------\n\n";
                        out += processRenderOut(data.renderOut);
                    }
                } else if (data.renderOut) {
                    out = processRenderOut(data.renderOut);
                } else { 
                    out = "NOTHING MATCHED OR UNRECOGNIZED ERROR";
                }
                outElem.val(out);
            }
        });
    };
    
    function runAjaxRenderTestFromForm(formId, outputElemId) {
        var form = jQuery('#'+formId);
        var requestUri = jQuery('select[name=requestUri]', form).val();
        var url = requestUri;
        
        var view = jQuery('select[name=view]', form).val();

        var scpRenderTargetExpr = jQuery('input[name=scpRenderTargetExpr]', form).val();
        var scpLoginRenderTargetExpr = jQuery('input[name=scpLoginRenderTargetExpr]', form).val();
        var scpErrorRenderTargetExpr = jQuery('input[name=scpErrorRenderTargetExpr]', form).val();
        var extraParams = jQuery('input[name=extraParams]', form).val(); // TODO
        var jQueryElemExpr = jQuery('input[name=jQueryElemExpr]', form).val();
        
        var params = {};
        if (requestUri.indexOf('/ajaxRender') !== -1) {
            if (!view) {
                alert("Please select a view");
                return;
            }
            params.view = view;
        } else {
            params.scpViewAsJson = 'true';
        }
        params.scpRenderTargetExpr = scpRenderTargetExpr;
        if (scpLoginRenderTargetExpr) {
            params.scpLoginRenderTargetExpr = scpLoginRenderTargetExpr;
        }
        if (scpErrorRenderTargetExpr) {
            params.scpErrorRenderTargetExpr = scpErrorRenderTargetExpr;
        }
        if (extraParams) {
            var ep = JSON.parse('{"' + extraParams.replace(/"/g, '\\"').replace(/&/g, '","').replace(/=/g,'":"') + '"}');
            params = jQuery.extend({}, params, ep);
        }
        
        var cb = getAjaxRenderOut;
        var cbmap = null;
        if (jQueryElemExpr.trim().length) {
            if (jQueryElemExpr.startsWith('+multi:')) {
                var jsex = jQueryElemExpr.substring('+multi:'.length);
                cbmap = {};
                var spl = jsex.split(',');
                jQuery.each(spl, function(i, e) {
                    var parts = e.trim().split(':', 2);
                    cbmap[parts[0].trim()] = function(renderOut) {
                        return getAjaxRenderOutWithJQuerySub(renderOut, parts[1].trim());
                    };
                });
            } else {
                cb = function(renderOut) {
                    return getAjaxRenderOutWithJQuerySub(renderOut, jQueryElemExpr);
                };
            }
        }
        
        runAjaxRenderTest(url, params, outputElemId, cb, cbmap);
    }
    
    var ajaxRenderTestPresets = <@objectAsScript lang='js' object=ajaxRenderTestPresets />
    function loadAjaxRenderTestPreset(selElem, formId) {
        var preset = jQuery(selElem).val();
        var values = {};
        if (preset && ajaxRenderTestPresets[preset]) {
            values = ajaxRenderTestPresets[preset]
        }
        setAjaxRenderTestFormInputs(formId, values);
    }
    function setAjaxRenderTestFormInputs(formId, values) {
        var form = jQuery('#'+formId);
        jQuery('select[name=requestUri]', form).val(values.requestUri || "");
        jQuery('select[name=view]', form).val(values.view || "");
        jQuery('input[name=scpRenderTargetExpr]', form).val(values.scpRenderTargetExpr || "");
        jQuery('input[name=scpLoginRenderTargetExpr]', form).val(values.scpLoginRenderTargetExpr || "");
        jQuery('input[name=scpErrorRenderTargetExpr]', form).val(values.scpErrorRenderTargetExpr || "");
        jQuery('input[name=extraParams]', form).val(values.extraParams || "");
        jQuery('input[name=description]', form).val(values.description || "");
        jQuery('input[name=jQueryElemExpr]', form).val(values.jQueryElemExpr || "");
        ajaxRenderRequestUriOnChange(jQuery('select[name=requestUri]', form));
    }
    function ajaxRenderRequestUriOnChange(selElem) {
        selElem = jQuery(selElem);
        if (selElem.val().indexOf('ajaxRender') !== -1) {
            jQuery('select[name=view]', selElem.closest('form')).prop('disabled', false);
        } else {
            jQuery('select[name=view]', selElem.closest('form')).prop('disabled', true);
        }
    }
    jQuery(document).ready(function() {
        ajaxRenderRequestUriOnChange(jQuery('#ajax-render-test-form select[name=requestUri]'));
    });
  </@script>
  
  <@form id="ajax-render-test-form">
    <#assign controllerConfig = Static["org.ofbiz.webapp.control.RequestHandler"].getRequestHandler(request).getControllerConfig()>
    <#assign requestMapMap = toSimpleMap(controllerConfig.getRequestMapMap())>
    <#assign viewMapMap = toSimpleMap(controllerConfig.getViewMapMap())>
    
    <@field type="select" label="Test Preset" onChange="loadAjaxRenderTestPreset(this, 'ajax-render-test-form');">
      <#list ajaxRenderTestPresets?keys as preset>
        <#assign presetValues = ajaxRenderTestPresets[preset]>
        
        <#assign presetTitle = presetValues.title!preset>
        <#assign presetArgStr = "">
        <#assign requestName = presetValues.requestUri!>
        <#if requestName?has_content>
          <#if (requestName?index_of("/control/") >= 0)><#-- FIXME: horrible kludge -->
            <#assign requestName = requestName?substring(requestName?index_of("/control/") + "/control/"?length)>
          </#if>
        </#if>
        <#if requestName?has_content>
          <#assign presetArgStr = presetArgStr + (presetArgStr?has_content?string(", ", "")) + "uri: " + requestName>
        </#if>
        <#if presetValues.view?has_content>
          <#assign presetArgStr = presetArgStr + (presetArgStr?has_content?string(", ", "")) + "view: " + presetValues.view>
        </#if>
        <#if presetValues.scpRenderTargetExpr?has_content>
          <#assign presetArgStr = presetArgStr + (presetArgStr?has_content?string(", ", "")) + "expr: " + presetValues.scpRenderTargetExpr>
        </#if>
        <#if presetValues.jQueryElemExpr?has_content>
          <#assign presetArgStr = presetArgStr + (presetArgStr?has_content?string(", ", "")) + "jQuery: " + presetValues.jQueryElemExpr>
        </#if>
        <#if presetArgStr?has_content>
          <#assign presetTitle = presetTitle + " [" + presetArgStr + "]">
        </#if>

        <@field type="option" selected=(presetValues.defaultPreset!false) value=preset>${presetTitle}</@field>
      </#list>
    </@field>
    
    <@field type="input" name="description" label="Notes" value=""/>
    
    <#assign defaultRequestUri = "ajaxRender">
    <@field type="select" name="requestUri" label="Request URI" value="" onChange="ajaxRenderRequestUriOnChange(this);">
      <@field type="option" selected=(!defaultRequestUri?has_content) value=""></@field>
      <#list requestMapMap?keys as requestName>
        <@field type="option" value=makeOfbizUrl(requestName) selected=(requestName?contains(defaultRequestUri))>${requestName}</@field>
      </#list>
        <@field type="option" value=makeOfbizUrl("TargetedRenderingTest")?replace("TargetedRenderingTest", "TargetedRenderingInvalidRequestTest") selected=(defaultRequestUri?contains("TargetedRenderingInvalidRequestTest"))>TargetedRenderingInvalidRequestTest</@field>
    </@field>
    <@field type="select" name="view" label="View Name" value="">
      <@field type="option" selected=true value=""></@field>
      <#list viewMapMap?keys as viewName>
        <#assign viewMap = viewMapMap[viewName]>  
        <@field type="option" value=viewName>${viewName} (${(viewMap.page)!""})</@field>
      </#list>
    </@field>
    <@field type="input" name="scpRenderTargetExpr" label="Target Widget Element Expression" value=""/>
    <@field type="input" name="scpLoginRenderTargetExpr" label="Expression for Login page" value=""/>
    <@field type="input" name="scpErrorRenderTargetExpr" label="Expression for Error page" value=""
        tooltip="NOTE: this is only used when the errorpage is shown - NOT when an event returns error"/>
    <@field type="input" name="extraParams" label="Extra Parameters (POST)" value=""/>
    <@field type="input" name="jQueryElemExpr" label="Extra jQuery Filter" value=""/>

    <@field type="submit" submitType="link" href="javascript:runAjaxRenderTestFromForm('ajax-render-test-form', 'ajax-render-test-out');" text=uiLabelMap.CommonRun/>
  </@form>
  
  <@field type="textarea" id="ajax-render-test-out" fieldsType="default-compact" label="Output:" rows=20>(click Run to execute test)</@field>

</@section>
</#if>

<#if debugMode>
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
</#if>

<#if debugMode>
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
</#if>

<#if debugMode>
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
</#if>

<#if debugMode>
<@section title="FTL macro flexible args demo">
  <#macro argsTestInner args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, {"innerArg1":"some-value-macro-default", "innerArg2":"some-value-macro-default", "innerArg3":"some-value-macro-default"}, {"innerArg4", "some-value-macro-override"})>
    <p>Inner macro args: <@objectAsScript lang="raw" escape=false object=args /></p>
  </#macro>

  <#-- this macro serves no purpose but to delegate to argsTestInner -->
  <#macro argsTestOuter args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, {"outerArg1":"some-value-macro-default", "outerArg2":"some-value-macro-default", "outerArg3":"some-value-macro-default",
      "innerArg2":"some-value-macro-default-from-delegating-macro"}, {"outerArg4", "some-value-macro-override"})>
    <@argsTestInner args=args />
  </#macro>

  <#assign args = {"innerArg3":"some-value-from-caller", "extraAttrib1":"some-value-from-caller"}>
  <@argsTestOuter args=args outerArg2="some-value-from-caller" extraAttrib2="some-value-from-caller"/>

  <@section title="Macro inspection">
    <p>@field macro default args: <@objectAsScript lang="raw" escape=false object=getScipioMacroDefaultArgs("field") /></p>
  </@section>
</@section>

<@section title="Utilities library demo">
  <#assign mySet = toSet(["val1", "val2", "val2", "val3", "val4", "val3"])>
  <#assign mySet = toSet(mySet)>
  <p>Basic set: <@objectAsScript lang="raw" escape=false object=mySet /></p>
  
  <#assign myMap = {"test1":"a string", "test2": wrapRawScript("a non-quoted script")}>
  
  <p>Map with non-quoted script values: <@objectAsScript lang="raw" escape=false object=myMap /></p>
</@section>

<@section title="Common messages">
  <@commonMsg type="generic">Generic message</@commonMsg>
  <@commonMsg type="result">Result message</@commonMsg>
  <@commonMsg type="result-norecord" />
  <@commonMsg type="error">Fatal error message</@commonMsg>
  <@commonMsg type="fail">Non-fatal error message (fail)</@commonMsg>
</@section>

<@section title="Containers">
  <@container class="+myclass">
    <@container open=true close=false elem="p"/>
      <@container elem="span">
        [Inside three containers]
      </@container>
    <@container close=true open=false /> <#-- this should remember the "p" -->
  </@container>
</@section>

<@section title="URL generation">
  <@section title="Current webapp/context info">
    <ul>
      <li>
        <strong>webSiteId</strong>: ${Static["org.ofbiz.webapp.website.WebSiteWorker"].getWebSiteId(request)!"(none)"}
        <em>(NOTE: In stock Ofbiz there is no webSiteId assigned to admin (webtools) in web.xml, so should be none here! 
             Do not add one to the admin webapp, because it would likely conflict with some screen implementations)</em>
      </li>
    </ul>
  </@section>
  
  <@section title="Standard navigation URLs">
    <ul>
      <li><@ofbizUrl uri="WebtoolsLayoutDemo?param1=val1&param2=val2" escapeAs='html'/> <em>(html post-escaping - <strong>strongly preferred</strong> to pre-escaping)</em></li>
      <li><@ofbizUrl uri="WebtoolsLayoutDemo?param1=val1&amp;param2=val2" /> <em>(html pre-escaping - legacy ofbiz mode - escaping done by caller before passing to macro)</em></li>
      <li><@ofbizUrl>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li>${makeOfbizUrl("WebtoolsLayoutDemo?param1=val1&amp;param2=val2")}</li>
      <li><@ofbizUrl fullPath=true>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl fullPath="true">WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl secure=true>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl secure="true">WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl secure=false>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl secure="false">WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl fullPath=true secure=true>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl fullPath=true secure="true">WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl fullPath=true secure=false>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl fullPath=true secure="false">WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl fullPath=true encode=false>WebtoolsLayoutDemo?param1=val1&amp;param2=val2</@ofbizUrl></li>
      <li><@ofbizUrl uri="main" webSiteId="ScipioWebStore"/></li>
      <li><@ofbizWebappUrl uri="/control/WebtoolsLayoutDemo?param1=val1&amp;param2=val2" /></li>
      <li><@ofbizInterWebappUrl uri="/shop/control/main" /></li>
      <li><@ofbizInterWebappUrl uri="/shop/control/main" fullPath=true/></li>
      <#-- Explicitly allow downgrading to HTTP -->
      <li><@ofbizInterWebappUrl uri="/shop/control/main" secure=false/></li>
      <#-- Explicitly allow downgrading to HTTP -->
      <li><@ofbizInterWebappUrl uri="/shop/control/main" fullPath=true secure=false/></li>
      <li><@ofbizInterWebappUrl uri="main" webSiteId="ScipioWebStore" /></li>
      <li>${makeOfbizInterWebappUrl("/shop/control/main")}</li>
      <li>${makeOfbizInterWebappUrl("main", "ScipioWebStore")}</li>
      <li>${makeOfbizInterWebappUrl({"uri":"main", "webSiteId":"ScipioWebStore", "extLoginKey": true})}</li>
      <li>${makeOfbizInterWebappUrl({"uri":"main?param1=val1&amp;param2=val2", "webSiteId":"ScipioWebStore", "extLoginKey": true})}</li>
    </ul>
  </@section>
  
  <@section title="Non-standard navigation URLs">
    <p><em>NOTE: These are not invalid, but needlessly obscure; for testing.</em></p>
    <ul>
      <li><@ofbizInterWebappUrl uri="/shop/control/main" webSiteId="ScipioWebStore" absPath=true /></li>
      <li><@ofbizInterWebappUrl uri="/shop/control/main" controller=true /></li>
      <li><@ofbizInterWebappUrl uri="/shop/control/main" controller=false /></li>
      <#-- NOTE: if controller false, can't detect some cases of fullPath requirements -->
      <li><@ofbizInterWebappUrl uri="/shop/control/main" controller=false fullPath=true/></li>
      <#-- Allow downgrade -->
      <li><@ofbizInterWebappUrl uri="/shop/control/main" controller=false fullPath=true secure=false/></li>
      <li><@ofbizInterWebappUrl uri="/shop/control/main" controller=false secure=true/></li>
      <li><@ofbizInterWebappUrl uri="/control/main" webSiteId="ScipioWebStore" controller=false /></li>
      <li><@ofbizInterWebappUrl uri="main" webSiteId="ScipioWebStore" controller=true /></li>
      <li><@ofbizUrl absPath=true interWebapp=false controller=true uri="/admin/control/main" /></li>
      <li><@ofbizUrl absPath=true interWebapp=true controller=true uri="/admin/control/main" /></li>
      <li><@ofbizUrl absPath=true interWebapp=false controller=false uri="/admin/control/main" /></li>
      <li><@ofbizUrl absPath=true interWebapp=true controller=false uri="/admin/control/main" /></li>
    </ul>
  </@section>
  
  <@section title="Inter-webapp catalog URLs">
    <p><em>NOTE: These should only reference a webapp configured to handle these in its web.xml file.</em></p>
    <ul>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" productId="PH-1000" /></li>
      <li><@ofbizCatalogUrl prefix="/shop" productId="PH-1000" /></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productId="PH-1000" /></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productId="PH-1000" /></li>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" productId="PH-1000" fullPath=true/></li>
      <li><@ofbizCatalogUrl prefix="/shop" productId="PH-1000" fullPath=true/></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productId="PH-1000" fullPath=true /></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productId="PH-1000" fullPath=true /></li>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" productId="PH-1000" fullPath=true secure=true/></li>
      <li><@ofbizCatalogUrl prefix="/shop" productId="PH-1000" fullPath=true secure=true/></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productId="PH-1000" fullPath=true secure=true params="?test1=val1&test2=val2"?html/></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productId="PH-1000" fullPath=true secure=true params="test1=val1&test2=val2"?html /></li>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" productId="PH-1000" secure=false/></li>
      <li><@ofbizCatalogUrl prefix="/shop" productId="PH-1000" secure=false/></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productId="PH-1000" secure=false /></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productId="PH-1000" secure=false /></li>
    
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" currentCategoryId="EL-PHN-101" /></li>
      <li><@ofbizCatalogUrl prefix="/shop" currentCategoryId="EL-PHN-101" /></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productCategoryId="EL-PHN-101" /></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productCategoryId="EL-PHN-101" /></li>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" currentCategoryId="EL-PHN-101" fullPath=true/></li>
      <li><@ofbizCatalogUrl prefix="/shop" currentCategoryId="EL-PHN-101" fullPath=true/></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productCategoryId="EL-PHN-101" fullPath=true /></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productCategoryId="EL-PHN-101" fullPath=true /></li>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" currentCategoryId="EL-PHN-101" fullPath=true secure=true/></li>
      <li><@ofbizCatalogUrl prefix="/shop" currentCategoryId="EL-PHN-101" fullPath=true secure=true/></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productCategoryId="EL-PHN-101" fullPath=true secure=true /></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productCategoryId="EL-PHN-101" fullPath=true secure=true /></li>
      <li><@ofbizCatalogUrl webSiteId="ScipioWebStore" currentCategoryId="EL-PHN-101" fullPath=true secure=false/></li>
      <li><@ofbizCatalogUrl prefix="/shop" currentCategoryId="EL-PHN-101" fullPath=true secure=false/></li>
      <li><@ofbizCatalogAltUrl webSiteId="ScipioWebStore" productCategoryId="EL-PHN-101" fullPath=true secure=false params="test1=val1&test2=val2"?html/></li>
      <li><@ofbizCatalogAltUrl prefix="/shop" productCategoryId="EL-PHN-101" fullPath=true secure=false /></li>
    </ul>
  </@section>
  
  <@section title="Content URLs">
    <p><em><strong>NOTE:</strong> Some of these are poor-to-terrible use of ofbizContentUrl/makeOfbizContentUrl; post-escaping is always best where possible.
       Other macro implementations new to Scipio (part of Scipio standard library) all do post-escaping.<br/>
       These examples are complicated not only for testing purposes, but also because @ofbizContentUrl (legacy Ofbiz macro, modified in Scipio) is forced to support
       pre-escaping, which was frequently used in legacy Ofbiz templates (<em>even though</em> the macro suffered from implementation problems because of it),
       usually done by screen html auto-escaping.
    </em></p>
    <ul>
      <li>${escapeVal(makeOfbizContentUrl(demoScreenContentUri), 'html')} <em>(no pre-escaping (rawString implicit), html post-escaping - <strong>NOTE: this is the best way (post-escaping)</strong>, compared to others below that do pre-escaping)</em></li>
      <li><@ofbizContentUrl uri=demoScreenContentUri escapeAs='html'/> <em>(no pre-escaping (rawString implicit), html post-escaping - this is equivalent to the previous, but slightly shorter.</em></li>
      <li>
        <#assign urlContent><@ofbizContentUrl strict=true>${rawString(demoScreenContentUri)}</@ofbizContentUrl></#assign>
        ${escapeVal(urlContent, 'html')} <em>(no pre-escaping (rawString explicit, strict true explicit), html post-escaping - NOTE: This is a more verbose and clumsy version (but still correct and strict) of the previous, but technically valid)</em>
      </li>
      <li><@ofbizContentUrl>${demoScreenContentUri}</@ofbizContentUrl> <em>(has html pre-escaping)</em></li>
      <li><@ofbizContentUrl uri=demoScreenContentUri /> <em>(has html pre-escaping)</em></li>
      <li>${escapeVal(makeOfbizContentUrl(demoScreenContentUri), 'html')} <em>(no pre-escaping (rawString implicit), html post-escaping)</em></li>
      <li>${escapeVal(makeOfbizContentUrl({"uri":demoScreenContentUri}), 'html')} <em>(no pre-escaping (rawString implicit), html post-escaping)</em></li>
      <li><@ofbizContentUrl ctxPrefix=true>${demoScreenContentUri}</@ofbizContentUrl> <em>(partial html pre-escaping)</em></li>
      <li><@ofbizContentUrl uri=demoScreenContentUri ctxPrefix=true/> <em>(partial html pre-escaping)</em></li>
      <li>${escapeVal(makeOfbizContentCtxPrefixUrl(demoScreenContentUri), 'html')} (no pre-escaping <em>(rawString implicit), html post-escaping)</em></li>
      <li>${escapeVal(makeOfbizContentUrl({"uri":demoScreenContentUri, "ctxPrefix":true}), 'html')} <em>(no pre-escaping (rawString implicit), html post-escaping)</em></li>
      <#assign manualPrefix = "https://ilscipio.com/images/"><#-- extra slash -->
      <li><@ofbizContentUrl ctxPrefix=manualPrefix>${demoScreenContentUri}</@ofbizContentUrl> <em>(partial html pre-escaping)</em></li>
      <li><@ofbizContentUrl uri=demoScreenContentUri ctxPrefix=manualPrefix/> <em>(partial html pre-escaping)</em></li>
      <li>${escapeVal(makeOfbizContentUrl({"uri":demoScreenContentUri, "ctxPrefix":manualPrefix}), 'html')} <em>(no pre-escaping (rawString implicit), html post-escaping)</em></li>
      <li><@ofbizContentUrl uri=escapeVal(demoScreenContentUri,'js-html') ctxPrefix=escapeVal(contentPathPrefix, 'js-html')/> <em>(js-html pre-escaping)</em></li>
      <li><@ofbizContentUrl uri=escapeVal(demoScreenContentUri,'js-html') ctxPrefix=escapeVal(manualPrefix, 'js-html')/> <em>(js-html pre-escaping)</em></li>
      <li><@ofbizContentUrl uri=escapeVal(demoScreenContentUri,'js-html') ctxPrefix=escapeVal(manualPrefix, 'js-html') strict=true/> <em>(<strong>USAGE ERROR:</strong> js-html pre-escaping - should contain an error (doubled slash), because we passed strict true which means the js-html pre-escaping doesn't get handled properly)</em></li>
      <li>${escapeVal(makeOfbizContentUrl({"uri":escapeVal(demoScreenContentUri,'js'), "ctxPrefix":manualPrefix}), 'html')} <em>(<strong>USAGE ERROR:</strong> js pre-escaping - should contain an error (doubled slash), because this doesn't handle pre-encodings because strict true by default on function)</em></li>
      <li>
        <#assign urlContent><@ofbizContentUrl uri="suffix_without_a_starting_slash/something/extra.jpg" ctxPrefix=(manualPrefix+"<") /></#assign>
        ${escapeVal(urlContent, 'html')} <em>(<strong>USAGE ERROR - DANGEROUS:</strong> js pre-escaping - unsafe string passed to ctxPrefix - should produce log warning that JS string possibly unsafe)</em>
      </li>
    </ul>
    <p>Variant filename (only) tests:</p>
    <ul>
      <li><@ofbizContentUrl uri="/image/some-thing/test.jpg" variant="detail"/> (append)</li>
      <li><@ofbizContentUrl uri="/image/some-thing/test-original.jpg" variant="detail"/> (should replace original)</li>
      <li><@ofbizContentUrl uri="/image/some-thing/original.jpg" variant="detail"/> (should replace original)</li>
      <li><@ofbizContentUrl uri=rewrapString("/image/some-thing/original.jpg") variant="detail"/> (should replace original) (non-strict, tries to work even if html-escaped)</li>
      <li><@ofbizContentUrl uri=rewrapString("/image/some-thing/original.jpg") variant="-detail"/> (force-append)</li>
    </ul>
  </@section>
  
  <@section title="Misc URL tests">
    <ul>
      <li>${addExtLoginKey("main")}</li>
    </ul>
  </@section>
  
  <@section title="Freemarker built-in ?url escaping (NOTE: escapeVal and escapeFullUrl are preferred in Scipio)">
    <ul>
      <li>Explicit encoding: someUri?param1=${"start''#%'end"?url("UTF-8")}</li>
      <li>Implicit encoding (added 2017-01-27): someUri?param1=${"start''#%'end"?url}</li>
    </ul>
  </@section>
</@section>

<@section title="Escaping">
  <@section title="Common escaping">
      <ul>
        <li>complexCharString (screen auto-escaping): ${complexCharString}</li>
        <li>complexCharString (escapeVal): ${escapeVal(complexCharString, 'html')}</li>
        <li>complexCharString (double-escaping failure): ${complexCharString?html}</li>
      </ul>
  </@section>
  <@section title="Filtered/validating/partial escaping">
      <#assign testMarkup>This is <span class="somespanclass">"test"</span> <em class="someclass">markup</em>!</#assign>
      <#assign testMarkup = rewrapString(testMarkup)><#-- make sure the rawString works -->
      <ul>
        <li>htmlmarkup allow none: ${escapeVal(testMarkup, 'htmlmarkup', {'allow':'none'})}</li>
        <li>htmlmarkup allow external: ${escapeVal(testMarkup, 'htmlmarkup', {'allow':'external'})}</li>
        <li>htmlmarkup allow internal: ${escapeVal(testMarkup, 'htmlmarkup', {'allow':'internal'})}</li>
        <li>htmlmarkup allow any-valid: ${escapeVal(testMarkup, 'htmlmarkup', {'allow':'any-valid'})}</li>
        <li>htmlmarkup allow any: ${escapeVal(testMarkup, 'htmlmarkup', {'allow':'any'})}</li>
      </ul>
  </@section>
  <@section title="objectAsScript">
      <ul>
        <li>objectAsScript (js-escaped, with bypass): <@objectAsScript lang="js" object={
            "map key 1, with \"apostrophe\", escaped" : "map value with \"apostrophe\", escaped, enclosed in quotes",
            "map key 2, with \"apostrophe\", escaped" : wrapRawScript("map value with \"apostrophe\" and no enclosing quotes (invalid js), escape bypass")
        }/></li>
      </ul>
  </@section>
  <@section title="Screen html auto-escaping bypass (rawString)">
      <p><em>The current renderer implementation automatically html-escapes strings as soon as they
        are output or interpolated using $\{} or ?string. The function #rawString prevents this.
        The advanced function #rewrapString can be seen as re-enabling the escaping (undoing the
        #rawString bypass).</em></p>
      <ul>
        <#assign autoEscString1 = rewrapString("<em>1234^&;\"'343</em>")>
        <#assign autoEscString2 = rewrapString("<i>123532^&;\"'3443343</i>")>
        <li>Auto-escaped string: "${autoEscString1} - ${autoEscString2}"</li>
        <li>Auto-escaped string: "${autoEscString1 + " - " + autoEscString2}"</li>
        <li>rawString: "${rawString(autoEscString1)} - ${rawString(autoEscString2)}"</li>
        <li>rawString: "${rawString(autoEscString1) + " - " + rawString(autoEscString2)}"</li>
        <li>rawString: "${rawString(autoEscString1, " - ", autoEscString2)}"</li>
        
        <!-- Whole maps bypassing the auto-escaping by rewrapping them in non-escaping models
            NOTE: at current time (2016-10-20) these may be inefficient -->
        <#assign autoEscMapInner1 = {"autoEscString1":autoEscString1, "autoEscString2":autoEscString2}>
        <#assign autoEscMap1 = rewrapMap(autoEscMapInner1)>
        <li>Auto-escaped map strings: "${autoEscMap1.autoEscString1} - ${autoEscMap1.autoEscString2}"</li>
        <li>Auto-escaped map strings: "${autoEscMapInner1.autoEscString1} - ${autoEscMapInner1.autoEscString2}"</li>
        <li>Number of map keys (complex/simple, should be >2/2): ${autoEscMap1?keys?size}/${autoEscMapInner1?keys?size}</li>
        <#assign rawMap1 = rewrapMap(autoEscMap1, "raw")>
        <#assign rawMapInner1 = rewrapMap(autoEscMapInner1, "raw")>
        <li>Raw map strings: "${rawMap1.autoEscString1} - ${rawMap1.autoEscString2}"</li>
        <li>Raw map strings: "${rawMapInner1.autoEscString1} - ${rawMapInner1.autoEscString2}"</li>
        <li>Number of map keys (complex/complex, should be >2/>2): ${rawMap1?keys?size}/${rawMapInner1?keys?size}</li>
        <#assign rawMap1 = rewrapMap(autoEscMap1, "raw-simple")>
        <#assign rawMapInner1 = rewrapMap(autoEscMapInner1, "raw-simple")>
        <li>Raw map strings: "${rawMap1.autoEscString1} - ${rawMap1.autoEscString2}"</li>
        <li>Raw map strings: "${rawMapInner1.autoEscString1} - ${rawMapInner1.autoEscString2}"</li>
        <li>Number of map keys (simple/simple, should be 2/2): ${rawMap1?keys?size}/${rawMapInner1?keys?size}</li>
      </ul>
  </@section>
  <@section title="Pre-escaping and macro escaping bypass">
      <ul>
        <li>Normal html escaping: ${escapeVal('<em>text, not emphasized because html-escaped</em>', 'htmlmarkup')}</li>
        <li>Bypass html escaping: ${escapeVal(wrapAsRaw('<em>text emphasized with html</em>', 'htmlmarkup'), 'htmlmarkup')}</li>
        <li>Normal js escaping: ${escapeVal('These "apostrophes" are js-escaped', 'js')}</li>
        <li>Bypass js escaping: ${escapeVal(wrapAsRaw('These "apostrophes" are not js-escaped', 'js'), 'js')}</li>
        <li>Normal js-html escaping: ${escapeVal('<em>text, not emphasized because html-escaped, plus "apostrophes" are js-escaped</em>', 'js-html')}</li>
        <li>Bypass js-html escaping: ${escapeVal(wrapAsRaw('<em>text emphasized, plus "apostrophes" are not escaped</em>', 'js-html'), 'js-html')}</li>
        <li>Partial bypass js in js-html escaping: ${escapeVal(wrapAsRaw('<em>text, not emphasized because html-escaped, plus "apostrophes" not escaped because of js bypass</em>', 'js'), 'js-html')}</li>
        <li>Failed bypass (wrong language): ${escapeVal(wrapAsRaw('<em>text, not emphasized because html-escaped, because we accidentally wrapped as js</em>', 'js'), 'htmlmarkup')}</li>
        <li>html filling in for htmlmarkup in htmlmarkup: ${escapeVal(wrapAsRaw('text <em>not html-escaped</em> WARN: bad usage! using <> is an error here, testing only, don\'t do this!!!', 'html'), 'htmlmarkup')}</li>
        <li>html filling in for htmlmarkup in htmlmarkup-js: ${escapeVal(wrapAsRaw('text <em>not html-escaped</em> WARN: bad usage! using <> is an error here, testing only, don\'t do this!!!', 'html'), 'htmlmarkup-js')}</em></li>
        <li>htmlmarkup NOT filling in for html in html: ${escapeVal(wrapAsRaw('text <em>html-escaped</em>', 'htmlmarkup'), 'html')}</li>
        <li>htmlmarkup NOT filling in for html in html-js: ${escapeVal(wrapAsRaw('text <em>html-escaped</em>', 'htmlmarkup'), 'html-js')}</em></li>
        <li>Multi-bypass (only js and htmlmarkup specified):<br/>
            <#assign value = wrapAsRaw({'htmlmarkup':'<em>html text with em tags</em>', 'js':'js text with "apostrophes"'})>
            <ul>
              <li>htmlmarkup (emphasized): ${escapeVal(value, 'htmlmarkup')}</li>
              <li>js (unescaped apostrophes): ${escapeVal(value, 'js')}</li>
              <li>js-html (here the js is selected because is prefix of js-html): ${escapeVal(value, 'js-html')}</li>
              <li>htmlmarkup-js (here the html is selected because is prefix of htmlmarkup-js): ${escapeVal(value, 'htmlmarkup-js')}</em></li>
              <li>raw (arbitrary text selected here, because raw was not specified, which it usually should): ${escapeVal(value, 'raw')}</li>
            </ul>     
        </li>
        <li>Multi-bypass (only htmlmarkup and raw specified):<br/>
            <#assign value = wrapAsRaw({'htmlmarkup':'<em>html text with em tags</em>', 'raw':'this is the "raw" text, as if no wrapAsRaw used'})>
            <ul>
              <li>html (emphasized): ${escapeVal(value, 'htmlmarkup')}</li>
              <li>js (escaped apostrophes): ${escapeVal(value, 'js')}</li>
              <li>js-html (here raw text is used because html is not a prefix of js): ${escapeVal(value, 'js-html')}</li>
              <li>htmlmarkup-js (here htmlmarkup is used because is prefix of htmlmarkup-js): ${escapeVal(value, 'htmlmarkup-js')}</em></li>
              <li>raw (just prints the raw text): ${escapeVal(value, 'raw')}</li>
            </ul>     
        </li>
        <li>Multi-bypass (htmlmarkup and html specified):<br/>
            <#assign value = wrapAsRaw({'htmlmarkup':'<em>html text with em tags meant for markup</em>', 'html':'this is generic "html" meant <mainly> for attributes'})>
            <ul>
              <li>htmlmarkup: ${escapeVal(value, 'htmlmarkup')}</li>
              <li>html (for attributes): ${escapeVal(value, 'html')}</li>
              <li>htmlmarkup-js: ${escapeVal(value, 'htmlmarkup-js')}</em></li>
              <li>html-js: ${escapeVal(value, 'html-js')}</em></li>
            </ul>     
        </li>
        <li>Multi-bypass (only html and raw specified):<br/>
            <#assign value = wrapAsRaw({'html':'this is generic "html" meant <em>mainly</em> for attributes WARN: contains bad usage of <>, for demo only! don\'t do this!', 'raw':'some <raw> text'})>
            <ul>
              <li>html (for attributes): ${escapeVal(value, 'html')}</li>
              <li>htmlmarkup (html used here because also works in markup): ${escapeVal(value, 'htmlmarkup')}</li>
              <li>htmlmarkup-js (html used here because also works in markup): ${escapeVal(value, 'htmlmarkup-js')}</em></li>
            </ul>     
        </li>
      </ul>
  </@section>
</@section>

<@section title="Date formatting (formatDate/formatDateTime/formatTime)">
  <#assign testDate = nowTimestamp>
  <ul>
    <li>date-time (locale, timeZone from context): ${formatDateTime(testDate)}</li>
    <li>date (locale, timeZone from context): ${formatDate(testDate)}</li>
    <li>time (locale, timeZone from context): ${formatTime(testDate)}</li>
    <li>date-time (locale, timeZone explicit): ${formatDateTime(testDate, "", locale, timeZone)}</li>
    <li>date (locale, timeZone explicit): ${formatDate(testDate, "", locale, timeZone)}</li>
    <li>time (locale, timeZone explicit): ${formatTime(testDate, "", locale, timeZone)}</li>
    <li>date-time (locale and timezone null, WARN: bad): ${formatDateTime(testDate, "", "", "")}</li>
  </ul>
</@section>

<@section title="Label functions">
  <p><em>NOTE: For uiLabelMap and getLabel, label arguments </em></p>
  <ul>
    <li>Locale stored in uiLabelMap: ${(uiLabelMap.getInitialLocale())!"(missing)"}</li>
    <li>getLabel (resource exists in uiLabelMap): "${getLabel("CommonYes")}"</li>
    <li>getLabel (resource not present in uiLabelMap; uses getPropertyMsg): "${getLabel("ContactListType.description.ANNOUNCEMENT", "MarketingEntityLabels")}"</li>

    <li>uiLabelMap: "${uiLabelMap.CommonDatabaseProblem}"</li>
    <li>getLabel: "${getLabel("CommonDatabaseProblem")}"</li>
    <li>getLabel: "${getLabel("CommonDatabaseProblem", "CommonUiLabels")}"</li>
    <li>getPropertyMsg (no args): "${getPropertyMsg("CommonUiLabels", "CommonDatabaseProblem")}"</li>
    <li>getPropertyMsg (crush locale): "${getPropertyMsg("CommonUiLabels", "CommonDatabaseProblem", false, false)}"</li>
    <li>getPropertyMsg (map args): "${getPropertyMsg("CommonUiLabels", "CommonDatabaseProblem", {"errMessage":"INSERTED-ERRMESSAGE"})}"</li>
    <#assign errMessage = "MAINNS-ERRMESSAGE">
    <li>getPropertyMsg (namespace as args): "${getPropertyMsg("CommonUiLabels", "CommonDatabaseProblem", .namespace)}"</li>

    <#-- NOTE: only globalContext works here because the uiLabelMap's context is not the same as our context -->

    <#assign dummy = setGlobalContextField("errMessage", "GLOBALCONTEXT-ERRMESSAGE")>
    <li>uiLabelMap (with arg in context): "${uiLabelMap.CommonDatabaseProblem}"</li>
    <li>getLabel (with arg in context): "${getLabel("CommonDatabaseProblem")}"</li>
    <li>getLabel (with arg in context): "${getLabel("CommonDatabaseProblem", "CommonUiLabels")}"</li>
    <#assign dummy = setGlobalContextField("errMessage", "")>
    <li>getLabel (args explicit off): "${getLabel("CommonDatabaseProblem", "", false)}"</li>
    <li>getLabel (explicit args): "${getLabel("CommonDatabaseProblem", "", {"errMessage":"INSERTED-ERRMESSAGE"})}"</li>
    <li>getLabel (explicit args, shorthand): "${getLabel("CommonDatabaseProblem", {"errMessage":"INSERTED-ERRMESSAGE"})}"</li>
    <li>rawLabel (explicit args, shorthand): "${rawLabel("CommonDatabaseProblem", {"errMessage":"INSERTED-ERRMESSAGE"})}"</li>

    <#assign prevPartyId = globalContext.partyId!"">
    <#assign dummy = setGlobalContextField("partyId", "GLOBALCONTEXT-PARTYID")>
    <#assign dummy = setGlobalContextField("paymentMethodId", "GLOBALCONTEXT-PAYMENTMETHODID")>
    <li>getLabel (resource not present in uiLabelMap + global args): "${getLabel("AccountingEftPartyNotAuthorized", "AccountingErrorUiLabels")}"</li>
    <#assign dummy = setGlobalContextField("partyId", prevPartyId)><#-- try not to break demo... -->
    <#assign dummy = setGlobalContextField("paymentMethodId", "")>
    <li>getLabel (resource not present in uiLabelMap + args explicit off): "${getLabel("AccountingEftPartyNotAuthorized", "AccountingErrorUiLabels", false)}"</li>
    <li>getLabel (resource not present in uiLabelMap + explicit args): "${getLabel("AccountingEftPartyNotAuthorized", "AccountingErrorUiLabels", {"partyId":"INSERTED-PARTYID", "paymentMethodId":"INSERTED-PAYMENTMETHODID"})}"</li>
    
    <li>getLabel with orderId: ${getLabel('ProductErrorOrderIdNotFound', 'ProductUiLabels', {"orderId":'WS10000'})}</li>
  </ul>
</@section>

<@section title="Menu highlighting">
  <ul>
    <li>context.activeSubMenu: ${context.activeSubMenu!"(none)"}</li>
    <li>context.activeSubMenuItem: ${context.activeSubMenuItem!"(none)"}</li>
        
    <li>context.activeMainMenuItem: ${context.activeMainMenuItem!"(none)"}</li>
    <li>globalContext.activeMainMenuItem: ${globalContext.activeMainMenuItem!"(none)"}</li>

    <li>context.activeMainMenuItem_auto: ${context.activeMainMenuItem_auto!"(none)"}</li>
    <li>globalContext.activeMainMenuItem_auto: ${globalContext.activeMainMenuItem_auto!"(none)"}</li>
  </ul>

</@section>

<@section title="Tree menu">
  <@section title="Multi-level">
    <@treemenu type="lib-basic">
        <@treeitem text="Some item">
            <@treeitem text="Some item" />
            <@treeitem text="Some item" />
            <@treeitem text="Some item">
                <@treeitem text="Some item" />
                <@treeitem text="Some item" />
                <@treeitem text="Some item" />
            </@treeitem>
            <@treeitem text="Some item" />
            <@treeitem text="Some item">
                <@treeitem text="Some item" />
                <@treeitem text="Some item" />
                <@treeitem text="Some item" />
            </@treeitem>
            <@treeitem text="Some item" />
        </@treeitem>
        <@treeitem text="Some item" />
        <@treeitem text="Some item" />
        <@treeitem text="Some item" />
        <@treeitem text="Some item" />
        <@treeitem text="Some item" />
    </@treemenu>
  </@section>

  <@section title="With event">
    <@treemenu type="lib-basic" events={"click":"alert('It is beautiful');"}>
        <@treeitem text="Some item" attribs={"type":"my-custom-type-attrib", "someAttrib":"my-custom-other-attrib", "attribs":"my-custom-attribs-attrib"}/>
        <@treeitem text="Some item">
            <@treeitem text="Some item" />
            <@treeitem text="Some item" />
        </@treeitem>
        <@treeitem text="Some item" />
    </@treemenu>
  </@section>

  <@section title="Doc example">
    <@treemenu type="lib-basic" items=[
        {"text":"Some item"},
        {"text":"Some item", "items":[
            {"text":"Some item"},
            {"text":"Some item"}
        ]},
        {"text":"Some item"}
    ]/>
  </@section>

  <@section title="Doc example with flipped icons">
    <#-- swapped icons -->
    <@treemenu type="lib-basic" defaultNodeIcon=(styles.treemenu_icon_dirnode!"") defaultDirIcon=(styles.treemenu_icon_node!"") items=[
        {"text":"Some item"},
        {"text":"Some item", "items":[
            {"text":"Some item"},
            {"text":"Some item"}
        ]},
        {"text":"Some item"}
    ]/>
  </@section>
  
  <@section title="Flat hierarchy">
    <@treemenu type="lib-basic">
        <@treeitem text="Some item" parent="#" id="treeitem_3_root1"/>
        <@treeitem text="Some item" parent="#" id="treeitem_3_root2"/>
        <@treeitem text="Some item" parent="treeitem_3_root2" id="treeitem_3_child1"/>
        <@treeitem text="Some item" parent="treeitem_3_root2" id="treeitem_3_child2"/>
        <@treeitem text="Some item" parent="#" id="treeitem_3_root3"/>
    </@treemenu>
  </@section>

  <@section title="Flat hierarchy">
    <@treemenu type="lib-basic" items=[
        {"text":"Some item", "isRoot":true, "id":"treeitem_4_root1"},
        {"text":"Some item", "isRoot":true, "id":"treeitem_4_root2"},
        {"text":"Some item", "parent":"treeitem_4_root2", "id":"treeitem_4_child1"},
        {"text":"Some item", "parent":"treeitem_4_root2", "id":"treeitem_4_child2"},
        {"text":"Some item", "isRoot":true, "id":"treeitem_4_root3"}
    ]/>
  </@section>

</@section>


<#-- NOTE: the rewrapString are just here to test the escape bypass -->
<@section title="FTL template interpret (#interpretStd)">
  <#assign dummy = setContextField("parentContextMyVar1", "parent context var")>
  <#assign parentFtlVar1 = "parent ftl var">

  <@section title="#interpretStd inline, defaults, as scalar">
    <#assign compiledTmpl = interpretStd(r"[#ftl][#-- comment --][#assign test = 3][@heading level=6]Hello from <em>interpreted</em>![/@heading]. parentContextMyVar1: ${parentContextMyVar1!'missing (ERROR)'}. parentFtlVar1: ${parentFtlVar1!'missing (good)'}. url: [@ofbizUrl fullPath=true]main[/@ofbizUrl]. rewrapped (escaping) special chars: ${rewrapString('//\\//\\//')}")>
    ${compiledTmpl}
  </@section>
    
  <@section title="#interpretStd from location, as directive (mimic ?interpret, but standalone render)">
    <#assign compiledTmpl = interpretStd({
        "location": rewrapString("component://webtools/webapp/webtools/showDateTime.ftl"),
        "model": "directive"
    })>
    <@compiledTmpl/>
  </@section>
  
  <@section title="#interpretStd inline, as hybrid (mimic ?interpret), with nested render (implicit ScipioObjectWrapper test)">
    <#assign dummy = setContextField("nestedCompiledTmpl", compiledTmpl)>
    <#assign compiledTmpl = interpretStd({
        "body": r"<@heading level=6>Hello from <em>interpreted ${myVar!'missing var (ERROR)'}</em></@heading> with extra context vars. parentContextMyVar1: ${parentContextMyVar1!'missing (ERROR)'}. nestedCompiledTmpl: <@nestedCompiledTmpl/>",
        "model": "hybrid",
        "ctxVars": {"myVar":"my variable"}
    })>
    <@section title="As string:">
      ${compiledTmpl?string}
    </@section>
    <@section title="As directive:">
      <@compiledTmpl/>
    </@section>
  </@section>
  
  <@section title="#interpretStd, non-standard context">
    <p><em>NOTE: Custom context object means most API don't work in this example.</em></p>
    <#assign compiledTmpl = interpretStd({
        "body": rewrapString(r"[#ftl][@heading level=6]Hello from <em>interpreted ${myVar!'missing var (ERROR)'}</em>[/@heading] with non-standard context (non-inherited). parentContextMyVar1: ${parentContextMyVar1!'missing (good)'}. http://www.ilscipio.com"),
        "model": "scalar",
        "invokeCtx": {"myVar":"my variable"}
    })>
    ${compiledTmpl}
  </@section>
    
  <@section title="?interpret (ftl built-in, reference)">
    <p><em>NOTE: For reference only. This builtin is usually inappropriate for use in Scipio, 
        because it runs from parent environment (instead of standalone/isolated), 
        bypasses ofbiz caching, and requires special syntax to evaluate the template. 
        The #interpretStd function will solve these issues and more.</em></p>
    <#assign compiledTmpl = r"[#ftl][#-- comment --][#assign test = 3][@heading level=6]Hello from <em>interpreted</em>![/@heading]. parentContextMyVar1: ${parentContextMyVar1!'missing (ERROR)'}. parentFtlVar1: ${parentFtlVar1!'missing (ERROR)'}. url: [@ofbizUrl fullPath=true]main[/@ofbizUrl]. rewrapped (escaping) special chars: ${rewrapString('//\\//\\//')}"?interpret>
    <@compiledTmpl/>
  </@section>
    
</@section>

<@section title="makeSectionsRenderer">
    <@section title="ftl">
      <#macro ftlSectionTestMacro>
        <p>hello from test macro!</p>
      </#macro>
      <#assign ftlSections = makeSectionsRenderer("ftl", {
        "left-column" : interpretStd('<p>hello from test interpretStd!</p>'),
        "right-column" : ftlSectionTestMacro,
        "main-column" : "<p>hello from test string!</p>",
        "top-column" : "<p>hello from test ?interpret</p>"?interpret
      })>
      
      <@section title="Regular output:">
        ${ftlSections.render("left-column")}
        ${ftlSections.render("right-column")}
        ${ftlSections.render("main-column")}
        ${ftlSections.render("top-column")}
      </@section>
      <@section title="asString output:">
        ${rawString(ftlSections.render("left-column", true))}
        ${rawString(ftlSections.render("right-column", true))}
        ${rawString(ftlSections.render("main-column", true))}
        ${rawString(ftlSections.render("top-column", true))}
      </@section>
    </@section>
</@section>

<#-- NOTE: keep last -->
<hr />
<#-- put this in a @section; it provides extra test for the request-scope section/title levels -->
<@section title="Ofbiz Widgets Layout Tests"> 
  <@render resource=ofbizWidgetsLayoutScreenLocation />

  <@section title="Direct inclusions">
    <@section title="Button Menu">
      <@render type="menu" resource="component://webtools/widget/MiscMenus.xml#LayoutDemoButton2" />
    </@section>
    <@section title="Button Menu (sub-menus filtered out)">
      <@render type="menu" resource="component://webtools/widget/MiscMenus.xml#LayoutDemoButton2NoSubMenus" />
    </@section>
    <@section title="Test form">
      <@render type="form" resource="component://webtools/widget/MiscForms.xml" name="LayoutDemoForm"
        ctxVars={"myFormPassedVar1":"Hello from a scope-protected context var"}
        globalCtxVars={"myFormPassedGlobalVar1":"Hello from a scope-protected global context var"}/>
    </@section>
    <@section title="Test form (alt include method)">
      <@render type="include-form" resource="component://webtools/widget/MiscForms.xml" name="LayoutDemoForm" 
        ctxVars={"myFormPassedVar1":"Hello from a scope-protected context var (alt)"}
        globalCtxVars={"myFormPassedGlobalVar1":"Hello from a scope-protected global context var (alt)"}/>
      <p>globalContext.myFormPassedGlobalVar1 (this should say 'missing', should have been automatically unset): ${globalContext.myFormPassedGlobalVar1!"missing"} ${context.myFormPassedGlobalVar1!"missing"}</p>
    </@section>
    
    <@section title="Max depth arg test">
      <@render type="menu" resource="component://webtools/widget/MiscMenus.xml#LayoutDemoTest3" />
      <@render type="menu" resource="component://webtools/widget/MiscMenus.xml#LayoutDemoTest3" maxDepth="1" 
          ctxVars={"myMenuPassedVar1":"PassedScopedContextVar"}
          globalCtxVars={"myMenuPassedGlobalVar1":"PassedGlobalContextVar"}
          reqAttribs={"myMenuPassedReqAttrib1":"PassedReqAttrib"} />
      <@render type="include-menu" resource="component://webtools/widget/MiscMenus.xml#LayoutDemoTest3" subMenus="none"
          ctxVars={"myMenuPassedVar1":"PassedScopedContextVar (alt)"}
          globalCtxVars={"myMenuPassedGlobalVar1":"PassedGlobalContextVar (alt)"}
          reqAttribs={"myMenuPassedReqAttrib1":"PassedReqAttrib (alt)"} />
      <p>globalContext.myMenuPassedGlobalVar1 (this should say 'missing', should have been automatically unset): ${globalContext.myMenuPassedGlobalVar1!"missing"} ${context.myMenuPassedGlobalVar1!"missing"}</p>
      <p>myMenuPassedReqAttrib1 (this should say 'missing', should have been automatically unset): ${request.getAttribute("myMenuPassedReqAttrib1")!"missing"}</p>
    </@section>
    
    <@section title="Standalone (isolated) FTL include">
      <@render type="ftl" resource="component://webtools/webapp/webtools/layout/layoutdemo_test1.ftl" ctxVars={"layoutdemoftltestvar1":"and hello from layout demo passed var"}/>
      <p>layoutdemoftltestvar1 (should say "missing"): ${layoutdemoftltestvar1!"missing"}</p>
    </@section>
    
    <@section title="Render nothing (empty)">
      <@render resource="" name="" />
    </@section>
    
    <#assign captureWarn><em>NOTE: 2017-03-10: This code is considered <strong>EXPERIMENTAL</strong> for Scipio - 
        stock Ofbiz facilities do not support capturing properly. If you must capture a @render call in production code,
        always pass <code>@render asString=true</code>. The tests below attempt to capture without
        <code>asString=true</code>, which is currently not guaranteed to work in all cases.</em><br/></#assign>
    <@section title="Simple Widget CAPTURED">
      ${captureWarn}
      <#assign capturedInclude><@render type="screen" resource="component://webtools/widget/MiscScreens.xml#DemoSimpleLabelWidget" /></#assign>
      Captured: "${capturedInclude}"
    </@section>
    <@section title="Standalone (isolated) FTL include CAPTURED">
      ${captureWarn}
      <#assign capturedInclude><@render type="ftl" resource="component://webtools/webapp/webtools/layout/layoutdemo_test1.ftl" /></#assign>
      Captured: "${capturedInclude}"
    </@section>
    <@section title="Render nothing (empty) CAPTURED">
      ${captureWarn}
      <#assign capturedInclude><@render resource="" name="" /></#assign>
      Captured: "${capturedInclude}"
    </@section>
    <@section title="Standalone (isolated) FTL include with nested @render CAPTURED">
      ${captureWarn}
      <#assign capturedInclude><@render type="ftl" resource="component://webtools/webapp/webtools/layout/layoutdemo_test2.ftl" /></#assign>
      Captured: "${capturedInclude}"
    </@section>
  </@section>

  <@section title="Admin plain site-map/tree">
  <@row>
      <@cell columns=6>
        <@render type="include-menu" resource="component://webtools/widget/MiscMenus.xml#WebtoolsPlainSiteMapDemo" />
      </@cell>
      <@cell columns=6>
        <@render type="include-menu" resource="component://webtools/widget/MiscMenus.xml#WebtoolsPlainSiteMapDemo3" />
      </@cell>
    </@row>
  </@section>

</@section>
</#if>
