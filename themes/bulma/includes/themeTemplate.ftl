<#--
* 
* A set of HTML templating macros, part of standard Scipio Freemarker API.
* Automatically included at all times.
* Intended to be swappable.
*
* Overrides the default SCIPIO styles located in 
* htmlTemplate.ftl - ofbiz_foundation/framework/common/webcommon/scipio/lib/standard/
* 
-->

<#-- Master include: Includes the default macros and allows overrides -->
<#include "component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl">

<#-- 
*************
* MACRO OVERRIDES
************
See component://common/webcommon/includes/scipio/lib/standard/htmlTemplate.ftl for documentation about
the macro interfaces used in the standard macros and these overrides.
 -->

<#-- @modal step markup - theme override -->
<#macro step_markup class="" icon="" completed=false disabled=false active=false href="" origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.nav_step!)>
    <#if active>
        <#local class = addClassArg(class, styles.nav_step_active!)>
    </#if>
    <#if disabled>
        <#local class = addClassArg(class, styles.nav_step_disabled!)>
    </#if>
    <li<@compiledClassAttribStr class=class />>
        <#local showLink = href?has_content && !disabled><#-- allow link to active for clean page refresh: && !active -->
        <#if showLink>
            <a href="${escapeFullUrl(href, 'html')}" class="steps-marker">
        <#else>
            <span class="steps-marker">
        </#if>
        <#if icon?has_content><i class="<#if completed>${styles.nav_step_completed!} ${styles.nav_step_icon_completed!}<#else>${escapeVal(icon, 'html')}</#if>"></i></#if>
        <#if showLink>
            </a>
        <#else>
            </span>
        </#if>
        <div class="steps-content">
            <#if showLink>
                <a href="${escapeFullUrl(href, 'html')}">
            </#if>
            <#nested>
            <#if showLink>
                </a>
            </#if>
        </div>
    </li>
</#macro>

<#macro alert_markup type="info" class="" typeClass="" id="" style="" closable=true containerId=""
containerClass="" containerStyle="" origArgs={} passArgs={} catchArgs...>
    <#local containerClass = addClassArg(containerClass,'container') />
    <div<@compiledClassAttribStr class=containerClass /><#if containerId?has_content> id="${escapeVal(containerId, 'html')}"</#if><#if containerStyle?has_content> style="${escapeVal(containerStyle, 'html')}"</#if>>
        <div data-alert class="${styles.alert_wrap!} ${styles[raw(typeClass)]!}">
            <#if closable><button class="delete"></button></#if><#t/>
            <div<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if>><#rt/>
                <#-- DEV NOTE: Watch out not to create extra whitespace before nested but ensure there is one after the closable icon -->

                <#nested><#t/>
            </div><#lt/>

        </div>
    </div>
</#macro>

<#-- @modal main markup - theme override -->
<#macro modal_markup id="" modalId="" linkId="" label="" href="" class="" icon="" title="" footer="" linkClass="" origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, "modal-card")>
    <#local linkClass = addClassArg(linkClass, "js-modal-trigger")>
    <a data-href="${escapeFullUrl(href, 'html')}"<@compiledClassAttribStr class=linkClass/><#if linkId?has_content> id="${linkId}"</#if> data-toggle="modal" data-target="${escapeVal(modalId, 'html')}"><#if icon?has_content><i class="${escapeVal(icon, 'html')}"></i> </#if>${escapeVal(label, 'htmlmarkup')}</a>
    <div id="${escapeVal(modalId, 'html')}" role="dialog" aria-hidden="true" class="${styles.modal_wrap!""}">
        <div class="modal-background"></div>
        <#-- Modal content-->
            <#if title?has_content || footer?has_content>
                <div <@compiledClassAttribStr class=class/>>
                    <div class="modal-card-head">
                        <p class="modal-card-title">${title!""}</p>
                    </div>
                    <div class="modal-card-body">
                        <#nested>
                    </div>
                    <div class="modal-card-foot">
                        ${footer!""}
                    </div>
                </div>
            <#else>
                <div class="modal-content">
                    <div class="box"><#nested></div>
                </div>
            </#if>
        <button class="modal-close is-large" aria-label="close"></button>
    </div>
</#macro>

<#function modalControl element type="open">
    <#local call="" />
    <#switch type?lower_case>
        <#case "close">
            <#local call>
                if(${element!""} !== undefined){
                    if (${element!""} instanceof jQuery){
                        ${element!""}.removeClass('is-active');
                    }else{
                        ${element!""}.classList.remove('is-active');
                    }
                }else{
                    document.getElementById('${element!""}').classList.remove('is-active');
                }
            </#local>
            <#break>
        <#case "open">
        <#default>
            <#local call>
                if(${element!""} !== undefined){
                    if (obj instanceof jQuery){
                        ${element!""}.addClass('is-active');
                    }else{
                        ${element!""}.classList.add('is-active');
                    }
                }else{
                    document.getElementById('${element!""}').classList.add('is-active');
                }
            </#local>
            <#break>
    </#switch>
    <#return call/>
</#function>

<#-- @nav main markup - theme override -->
<#macro nav_markup type="" id="" class="" style="" activeElem="" origArgs={} passArgs={} catchArgs...>
    <#switch type>
        <#case "magellan">
        <nav class="magellan" data-spy="scroll">
            <div class="container-fluid">
                <ul class="nav nav-tabs" role="tablist">
                    <#nested>
                </ul>
            </div>
        </nav>
        <#break>
        <#case "breadcrumbs">
        <nav class="${styles.nav_breadcrumbs!}" aria-label="breadcrumbs">
        <ul>
            <#nested>
        </ul>
        </nav>
        <#break>
        <#case "steps">
        <#local class = addClassArg(class, styles.nav_steps!)>
        <ul<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}</#if><#if style?has_content> style="${escapeVal(style, 'html')}</#if>>
            <#nested>
        </ul>
        <#break>
        <#default>
        <ul class="${styles.list_inline!} ${styles.nav_subnav!}">
            <#nested>
        </ul>
        <#break>
    </#switch>
</#macro>

<#macro menu_markup type="" specialType="" name="" class="" id="" style="" attribs={} excludeAttribs=[]
inlineItems=false titleClass="" title="" htmlwrap="ul" isNestedMenu=false parentMenuType="" parentMenuSpecialType=""
active=false activeTarget="" menuLevel=1 sepMenu={} itemCount=""
origArgs={} passArgs={} catchArgs...>
    <#if !inlineItems && htmlwrap?has_content>
        <#-- NOTE: here we always test specialType and never type, so that many (custom) menu types may reuse the same
        existing specialType special handling without having to modify this code -->
        <#if specialType == "main">
            <#-- WARN: isNestedMenu check here would not be logical -->
            <li class="${styles.menu_main_wrap!}"><a href="#" class="${styles.menu_main_item_link!}"
        <#if (styles.framework!"") == "bootstrap"> data-toggle="dropdown"</#if>>${escapeVal(title, 'htmlmarkup')}<#if (styles.framework!"") == "bootstrap"> <i class="fa fa-fw fa-caret-down"></i></#if></a>
        <#elseif specialType == "sidebar" && !isNestedMenu>
            <#-- WARN: isNestedMenu check here is flawed, but it's all we need for now -->
            <aside class="menu">
            <#-- FIXME: this "navigation" variable is way too generic name! is it even still valid? -->
            <#if navigation?has_content><heading>${escapeVal(navigation, 'htmlmarkup')}</heading></#if>
        <#elseif specialType == "button-dropdown">
            <button href="#" data-dropdown="${escapeVal(id, 'html')}" aria-controls="${escapeVal(id, 'html')}" data-toggle="dropdown" aria-expanded="false"<@compiledClassAttribStr class=titleClass />>${escapeVal(title, 'htmlmarkup')}</button><br>
            <#local attribs = attribs + {"data-dropdown-content":"true", "aria-hidden":"true"}>
        </#if>
    </#if>
    <#-- Logo -->
    <#if (sepMenu.layout!) == "before-inline">
        <#if htmlwrap?has_content><${htmlwrap} class="menu-list"<#if id?has_content> id="menu_logo"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
            <li class="nav-title">
                <#if layoutSettings.headerImageLinkUrl??>
                    <#assign logoLinkURL = "${layoutSettings.headerImageLinkUrl}">
                <#else>
                    <#assign logoLinkURL = "${layoutSettings.commonHeaderImageLinkUrl}">
                </#if>
                <a href="<@ofbizUrl>${logoLinkURL!""}</@ofbizUrl>" class="navbar-item brand-text">
                    <img alt="${layoutSettings.companyName}" src="<@contentUrl>/bulmatheme/images/scipio-logo-small.svg</@contentUrl>" />
                </a>
            </li>
        <#if htmlwrap?has_content></${htmlwrap}></#if>
    </#if>
    <#-- Action Menu -->
    <#if sepMenu.hasMenu>
        <#if (sepMenu.layout!) == "before-inline">
            <#local sepClass=class/>
            <#local sepClass = addClassArg(sepClass,"is-open")/>
            <#if htmlwrap?has_content><${htmlwrap}<@compiledClassAttribStr class=sepClass /><#if id?has_content> id="${escapeVal(id, 'html')}_action"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
                <li class="nav-title" id="menu_1_action_title"><a href="javascript:openSidebarTab('menu_1_action');"><i class="fa fa-bolt" aria-hidden="true"></i><span class="menu-label is-sr-only">${getLabel("CommonActions")}</span></a></li>
                <@sepMenu.render sepMenu=sepMenu menuOnly=true></@sepMenu.render>
            <#if htmlwrap?has_content></${htmlwrap}></#if>
        </#if>
    <#else>
        <#local class = addClassArg(class,"is-open")/>
    </#if>
    <#-- Menu -->
    <#if htmlwrap?has_content><${htmlwrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if>></#if>
    <#if (sepMenu.layout!) == "before-inline">
        <li class="nav-title" id="menu_1_title"><a href="javascript:openSidebarTab('menu_1');"><i class="fa fa-sitemap" aria-hidden="true"></i><span class="menu-label is-sr-only">${getLabel("CommonNavigation")}</span></a></li>
        <#--<@menuitem type="text" class=sepMenu.nonsepTitleItemClass contentClass="is-sr-only" text=getLabel("CommonNavigation")/>--></#if>
    <#nested>
    <#if !inlineItems && htmlwrap?has_content>
        <#if specialType == "main">
            <#if htmlwrap?has_content></${htmlwrap}></#if>
            </li>
        <#elseif specialType == "sidebar" && !isNestedMenu>
            <#if htmlwrap?has_content></${htmlwrap}></#if>
            </aside>
        <#else>
            <#if htmlwrap?has_content></${htmlwrap}></#if>
        </#if>
    </#if>
</#macro>

<#macro menuitem_markup type="" menuType="" menuSpecialType="" name="" menuName="" class="" id="" style="" attribs={}
excludeAttribs=[] inlineItem=false htmlwrap="li" disabled=false selected=false active=false activeTarget=""
isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0 itemCount="" origArgs={} passArgs={} catchArgs...>
    <#if !inlineItem && htmlwrap?has_content>
        <${htmlwrap}<@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=["class", "id", "style"]/></#if>><#rt>
    </#if>
    <#nested><#t>
    <#if !inlineItem && htmlwrap?has_content>
        </${htmlwrap}><#lt>
    </#if>
</#macro>

<#macro menuitem_link_markup itemType="" menuType="" menuSpecialType="" itemName="" menuName="" class="" id="" style="" href="" name="" onClick="" target="" title=""
attribs={} excludeAttribs=[] disabled=false selected=false active=false activeTarget="" isNestedMenu=false menuLevel=1 parentMenuType="" parentMenuSpecialType="" itemIndex=0
origArgs={} passArgs={} catchArgs...>
    <#if active><#local class = addClassArg(class, "is-active")></#if>
    <#t><a href="${escapeFullUrl(href, 'html')}"<#if onClick?has_content> onclick="${escapeVal(onClick, 'html')}"</#if><@compiledClassAttribStr class=class /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if attribs?has_content><@commonElemAttribStr attribs=attribs exclude=excludeAttribs/></#if><#if target?has_content> target="${escapeVal(target, 'html')}"</#if><#if title?has_content> title="${escapeVal(title, 'html')}"</#if>><#nested></a>
</#macro>

<#macro table_markup open=true close=true type="" styleName="" class="" id="" cellspacing="" useResponsive=false responsiveArgs={}
autoAltRows="" firstRowAlt="" inheritAltRows=false useFootAltRows=false tableIdNum=0 attribs={} excludeAttribs=[] origArgs={} passArgs={} catchArgs...>
    <#if open><div class="table-container"></#if>
     <@scipioStdTmplLib.table_markup open=open close=close type=type styleName=styleName class=class id=id cellspacing=cellspacing useResponsive=useResponsive
        responsiveArgs=responsiveArgs autoAltRows=autoAltRows firstRowAlt=firstRowAlt inheritAltRows=inheritAltRows useFootAltRows=useFootAltRows tableIdNum=tableIdNum
        attribs=attribs excludeAttribs=excludeAttribs origArgs=origArgs passArgs=passArgs catchArgs=catchArgs><#nested></@scipioStdTmplLib.table_markup>
    <#if close></div></#if>
</#macro>

<#macro tabs_markup type="" title="" id="" class=""
origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.tabs_wrap!"")>
    <#local class = addClassArg(class, type)>

    <#local tabsIdNum = getRequestVar("scipioTabsIdNum")!0>
    <#local tabsIdNum = tabsIdNum + 1 />
    <#local dummy = setRequestVar("scipioTabsIdNum", tabsIdNum)>

    <#local dummy = setRequestVar("scipioTabLength", tabLength)>

    <#local dummy = setRequestVar("scipioTabInfoStack", {})>
    <#local nestedContent><#nested /></#local>
    <#local globalTabInfo=getRequestVar("scipioTabInfoStack"!{})>

    <#if title?has_content><@heading class="${styles.tabs_title!}">${escapeVal(title, 'htmlmarkup')}</@heading></#if>
    <div class="tabs is-fullwidth" id="tab-${tabsIdNum!}">
        <ul <@compiledClassAttribStr class=class /> data-tab role="tablist">
            <#--<li class="tab-title active"><a href="#panel1" data-toggle="tab" role="tab">Tab 1</a></li>-->
            <#list globalTabInfo?keys as localTab>
                <#local tab = globalTabInfo[localTab]/>
                <li class="${styles.tabs_item_title}<#if tab['tabLength']?has_content && tab['tabLength']==1> ${styles.tabs_item_title_active}</#if>">
                    <a href="#tab-${tabsIdNum!}" class="${styles.tabs_item_title_link}<#if tab['tabLength']?has_content && tab['tabLength']==1> ${styles.tabs_item_title_link_active}</#if>"
                    data-toggle="tab" role="tab">${tab['title']!tab['id']!tab['tabLength']}</a>
                </li>
            </#list>
        </ul>
    </div>
    <div<#if styles.tabs_item_container?has_content> class="${styles.tabs_item_container}"</#if>>
        ${nestedContent!}
    </div>
</#macro>


<#-- @chart main markup - theme override -->
<#macro chart_markup type="" chartLibrary="" title="" id="" xlabel="" ylabel="" label1="" label2="" labelUom1="" labelUom2="" chartIdNum=0 renderSeqNumber=0 origArgs={} passArgs={} catchArgs...>
  <#-- WARN/FIXME?: ids and type are not escaped, currently assumed to come from internal only... -->
  <#local nestedContent><#nested /></#local>
  <#if chartLibrary=="foundation">
    <#if nestedContent?has_content>
    <@row>
      <@cell columns=3>
        <ul data-${escapeVal(type, 'html')}-id="${escapeVal(id, 'html')}" class="${styles.chart_legend!}">
            <#nested/>
        </ul>
      </@cell>
      <@cell columns=9><div id="${escapeVal(id, 'html')}" style="height:300px;"></div></@cell>
    </@row>
    <#else>
        <#-- Default to chart.js chart for now, as this is capable of rendering an empty chart -->
        <@chart type=type library="chart" xlabel=xlabel ylabel=ylabel label1=label1 label2=label2>
            <#nested>
        </@chart>
    </#if>
  <#else>
    <#-- Get the number of datasets by inspecting the nested content (chartjs addData function values) -->
    <#if nestedContent?has_content>
        <#assign chartDatasets=chart_get_number_of_datasets(nestedContent, chartLibrary) />
        <#else>
        <#assign chartDatasets=0/>
    </#if>
    <#if (chartDatasets < 1)><#local chartDatasets = 1 /></#if>
    <#-- THEME Specific changes -->
      <div class="chart-wrapper">
      <span class="chart-data">&nbsp;</span>
        <canvas id="${escapeVal(id, 'html')}"></canvas>
    </div>
    <#-- End of THEME Specific changes -->
    <@script>
        $(function(){
            var chartDataEl = $('.chart-data').first();
            var chartData = chartDataEl.sassToJs({pseudoEl:"::before", cssProperty: "content"});
            var options =  {
                    responsive: true,
                    responsiveAnimationDuration: 0,
                    animation: {
                        duration: 1000
                    },
                    maintainAspectRatio: true,
                    tooltips: {
                        mode: <#if type=="line" || type=="bar">'label'<#else>'single'</#if><#if (labelUom1?has_content ||labelUom2?has_content) >,
                        callbacks: {
                            label: function(tooltipItem, data) {
                                <#if labelUom1?has_content>
                                    if(tooltipItem.datasetIndex == 0) {
                                        var datasetLabel = '';
                                        <#if type=="line" || type=="bar">
                                             datasetLabel = data.datasets[tooltipItem.datasetIndex].label;
                                             return datasetLabel + ': ' + tooltipItem.yLabel + ' ${escapeVal(labelUom1, 'js')}';
                                        <#elseif type="pie">
                                             datasetLabel = data.labels[tooltipItem.index] + ': ' + data.datasets[tooltipItem.datasetIndex].data[tooltipItem.index];
                                             return datasetLabel + ' ${escapeVal(labelUom1, 'js')}';
                                        <#else>
                                            return datasetLabel;
                                        </#if>
                                    }
                                </#if>
                                <#if labelUom2?has_content>
                                    if(tooltipItem.datasetIndex == 1) {
                                        var datasetLabel = data.datasets[tooltipItem.datasetIndex].label || '';
                                        return datasetLabel + ': ' + tooltipItem.yLabel + ' ${escapeVal(labelUom2, 'js')}';
                                    }
                                </#if>
                            }
                        }
                        </#if>
                    },
                    hover: {
                        mode: <#if type=="line" || type=="bar">'label'<#else>'single'</#if>
                    },
                    legend: {
                        position: 'bottom',
                        labels: {
                            boxWidth: 30
                        }
                    },
                    title: {
                        <#if title?has_content>
                            display: true,
                            text: '${escapeVal(title, 'js')}',
                        <#else>
                            display: false,
                        </#if>
                        fontColor: chartData.scaleLabelFontColor,
                        fontFamily: chartData.scaleLabelFontFamily,
                        fontSize: chartData.scaleLabelFontSize
                    }
                    <#if type=="line" || type=="bar">,
                        <#if type=="line"> animation: {
                        duration: 0
                        },</#if>
                        scales: {
                            x: {
                                gridLines: {
                                    color: chartData.scaleGridLineColor
                                },
                                scaleLabel : {
                                    display: chartData.scaleLabelDisplay,
                                    <#if xlabel?has_content>labelString: '${escapeVal(xlabel, 'js')}',</#if>
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                },
                                ticks: {
                                    display: true,
                                    autoSkip: true,
                                    padding:10,
                                    maxRotation:30,
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                }
                              },
                            y: {
                                scaleLabel : {
                                    display: chartData.scaleLabelDisplay,
                                    <#if ylabel?has_content>scaleLabel: '${escapeVal(xlabel, 'js')}',</#if>
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                },
                                ticks: {
                                    display: true,
                                    autoSkip: true,
                                    fontColor: chartData.scaleLabelFontColor,
                                    fontFamily: chartData.scaleLabelFontFamily,
                                    fontSize: chartData.scaleLabelFontSize
                                }
                            }
                        }
                    <#elseif type=="pie">,
                        scales: {

                        }
                    </#if>
                };
            var canvasElem = $('#${escapeVal(id, 'js')}');
            var ctx = canvasElem.get(0).getContext("2d");
            var data = {
                labels :[],
                datasets: [
                    {
                      <#if type=="line" || type=="bar">
                      label: '${escapeVal(label1, 'js')}',
                      fill: true,
                      backgroundColor: chartData.primaryFillColor,
                      borderColor: chartData.primaryStrokeColor,
                      pointBackgroundColor: chartData.pointColor,
                      pointBorderColor: chartData.primaryPointStrokeColor,
                      pointHoverBackgroundColor: chartData.pointHighlightFill,
                      pointHoverBorderColor: chartData.pointHighlightStroke,
                      <#else>
                      backgroundColor: [
                            chartData.pieFillColor1,
                            chartData.pieFillColor2,
                            chartData.pieFillColor3,
                            chartData.pieFillColor4,
                            chartData.pieFillColor5,
                            chartData.pieFillColor6
                        ],
                        hoverBackgroundColor: [
                            chartData.pieHighlightColor1,
                            chartData.pieHighlightColor2,
                            chartData.pieHighlightColor3,
                            chartData.pieHighlightColor4,
                            chartData.pieHighlightColor5,
                            chartData.pieHighlightColor6
                        ],
                      </#if>
                      data: []
                    }
                    <#if (chartDatasets > 1)>
                    ,{
                      <#if (type=="line" || type=="bar")>
                      label: '${escapeVal(label2, 'js')}',
                      fill: true,
                      backgroundColor: chartData.secondaryFillColor,
                      borderColor: chartData.secondaryStrokeColor,
                      pointBackgroundColor: chartData.pointColor,
                      pointBorderColor: chartData.secondaryPointStrokeColor,
                      pointHoverBackgroundColor: chartData.pointHighlightFill,
                      pointHoverBorderColor: chartData.pointHighlightStroke,
                      <#else>
                       backgroundColor: [
                            chartData.pieFillColor1,
                            chartData.pieFillColor2,
                            chartData.pieFillColor3,
                            chartData.pieFillColor4,
                            chartData.pieFillColor5,
                            chartData.pieFillColor6
                        ],
                      </#if>
                      data: []
                    }
                    </#if>
                    ]
                };
            var config = {
                <#switch type>
                    <#case "bar">type: 'bar'<#break>
                    <#case "pie">type: 'pie'<#break>
                    <#default>type: '${type!"line"}'
                </#switch>,
                data: data,
                options: options
            };

            var chart = new Chart(ctx, config);
            canvasElem.data("chart", chart);
            var newChart = chart; <#-- backward-compat -->
            var ${escapeVal(id, 'js')} = chart; <#-- backward-compat: TODO: REMOVE: escapeVal is not valid for use outside quotes (see freemarker docs for ?js) -->
            <#-- escapeVal unsafe outside quotes and other issues
            ${escapeVal(id, 'js')} = chart; -->
            ${nestedContent}
            chart.update();
        });
    </@script>
  </#if>
</#macro>

<#-- @slider main markup - theme override -->
<#macro slider_markup title="" id="" sliderIdNum=0 class="" library="" controls=true indicator=true
jsOptions="" origArgs={} passArgs={} catchArgs...>
    <#if langDir?has_content && langDir=="rtl"><#local jsOptions="rtl:true,"+jsOptions/></#if>
    <#if title?has_content><@heading>${escapeVal(title, 'htmlmarkup')}</@heading></#if>
    <#local library="bulma"/>
    <#switch library>
        <#case "owl">
        <#local class = addClassArg(class, "owl-carousel")>
        <div<@compiledClassAttribStr class=class /> id="${escapeVal(id, 'html')}">
            <#nested/>
        </div>
        <script type="text/javascript">
            $(document).ready(function(){
        $("#${escapeVal(id, 'js')}").owlCarousel({${jsOptions}});
                });
            </script>
        <#break>
        <#case "slick">
        <div class="slider-parent">
            <div<@compiledClassAttribStr class=class /> id="${escapeVal(id, 'html')}">
                <#nested/>
            </div>
        </div>
        <script type="text/javascript">
            $(document).ready(function(){
                $("#${escapeVal(id, 'js')}").slick({${jsOptions}});
                });
        </script>
        <#break>
        <#default>
            <div class="${styles.slider_container!}">
                <#local sliderLength = getRequestVar("scipioSliderLength")!0>
                <div class="${styles.slider_wrap!}" id="${escapeVal(id, 'html')}" >
                    <#nested/>
                </div>
            </div>
            <@script>
                document.addEventListener('DOMContentLoaded', () => {
                    bulmaCarousel.attach('#${escapeVal(id, 'js')}', {
                        ${jsOptions}
                    });
                });
            </@script>
        <#break>
    </#switch>
</#macro>

<#-- @slide main markup - theme override -->
<#macro slide_markup id="" sliderId="" class="" library="" image="" link="" linkTarget="" title="" slideIdNum=0 sliderLength=1 renderSeqNumber="" origArgs={} passArgs={} catchArgs...>
    <#local library="bulma"/>
    <#if library=="owl" || library=="slick">
        <div id="${escapeVal(id, 'html')}" class="item">
            <#if link?has_content><a href="${escapeFullUrl(link, 'html')}"<#if linkTarget?has_content> target="${escapeVal(linkTarget, 'html')}"</#if>></#if>
            <div>
                <#if title?has_content><h2>${escapeVal(title, 'htmlmarkup')}</h2></#if>
                <#if image?has_content>
                    <img src="${escapeFullUrl(image, 'html')}"  class="${styles.slide_image!}"/>
                </#if>
                <#local nestedContent><#nested></#local>
                <#if nestedContent?has_content><div class="">${nestedContent}</div></#if>
            </div>
            <#if link?has_content></a></#if>
        </div>
    <#else>
        <div class="item-${slideIdNum} <#if slideIdNum==1>active</#if>">
            <#if link?has_content><a href="${escapeFullUrl(link, 'html')}"<#if linkTarget?has_content> target="${escapeVal(linkTarget, 'html')}"</#if>></#if>
            <#if title?has_content><h2  class="title is-2">${escapeVal(title, 'htmlmarkup')}</h2></#if>
            <#if image?has_content>
                <img src="${escapeFullUrl(image, 'html')}" class="${styles.slide_image!}"/>
            </#if>
            <#local nestedContent><#nested></#local>
            <#if nestedContent?has_content><div class="${styles.slide_content!}">${nestedContent}</div></#if>
            <#if link?has_content></a></#if>
        </div>
    </#if>
</#macro>

<#macro pul_markup title="" origArgs={} passArgs={} catchArgs...>
    <div class="${styles.pricing_wrap!}">
        <#if title?has_content><@pli type="title">${escapeVal(title, 'htmlmarkup')}</@pli></#if>
        <#nested>
    </div>
</#macro>

<#-- @pli main markup - theme override -->
<#macro pli_markup type="" class="" origArgs={} passArgs={} catchArgs...>
    <#switch type>
        <#case "price">
        <#local class = addClassArg(class, "panel-block "+ styles.pricing_price!)>
        <div <@compiledClassAttribStr class=class />>
            <div class="control">
                <#nested>
            </div>
        </div>
        <#break>
        <#case "description">
        <#local class = addClassArg(class, "panel-block "+styles.pricing_description!)>
        <div <@compiledClassAttribStr class=class />><div class="control">
            <#nested>
        </div></div>
        <#break>
        <#case "title">
        <#local class = addClassArg(class, styles.pricing_title!)>
        <div <@compiledClassAttribStr class=class />><#nested></div>
        <#break>
        <#case "button">
        <#local class = addClassArg(class, "panel-block "+styles.pricing_cta!)>
        <div <@compiledClassAttribStr class=class />>
            <#nested>
        </div>
        <#break>
        <#default>
        <#local class = addClassArg(class, styles.pricing_bullet!)>
        <div <@compiledClassAttribStr class=class />><div class="control">
            <#nested>
        </div></div>
        <#break>
    </#switch>
</#macro>

<#--
FORM MACROS
-->
<#-- @fieldset main markup - theme override -->
<#macro fieldset_markup open=true close=true class="" containerClass="" id="" containerId="" title="" collapsed=false collapsibleAreaId="" expandToolTip="" collapseToolTip="" collapsible=false origArgs={} passArgs={} catchArgs...>
    <#if open>
        <#local containerClass = addClassArg(containerClass, "fieldgroup")>
        <#if collapsible || collapsed>
            <#local containerClass = addClassArg(containerClass, "toggleField")>
            <#if collapsed>
                <#local containerClass = addClassArg(containerClass, styles.collapsed)>
            </#if>
        </#if>
        <#local classes = compileClassArg(class)>
        <#local containerClasses = compileClassArg(containerClass, "${styles.grid_large!}12")>
        <@row open=true close=false />
        <@cell open=true close=false class=containerClasses id=containerId />
        <fieldset<#if classes?has_content> class="${escapeVal(classes, 'html')}"</#if><#if id?has_content> id="${escapeVal(id, 'html')}"</#if>>
        <#if title?has_content><legend><#if collapsible || collapsed>[ <i class="${styles.icon!} ${styles.icon_arrow!}"></i> ] </#if>${escapeVal(title, 'htmlmarkup')}</legend></#if>
        <div id="${escapeVal(collapsibleAreaId, 'html')}" class="fieldgroup-body"<#if collapsed> style="display: none;"</#if>>
    </#if>

        <#nested>
        <#if close>
            </div>
            </fieldset>
            <@cell close=true open=false />
            <@row close=true open=false />
        </#if>
</#macro>

<#macro field args={} inlineArgs...>
    <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.field_defaultArgs)>
    <#local dummy = localsPutAll(args)>
    <#local overrideDefaultClass = {} >
    <#switch type>
        <#case "generic">
        <#case "submit">
        <#case "display">
        <#break>
        <#case "file">
        <#break>
        <#case "textarea">
            <#local class = addClassArg(class,"textarea") >
        <#break>
        <#case "select">
            <#local class = addClassArg(class,"select") >
        <#break>
        <#case "checkbox">
        <#case "radio">
            <#local class = addClassArg(class,"is-checkradio") >
        <#break>
        <#case "file">
            <#local class = addClassArg(class,"file-input") >
        <#break>
        <#case "text">
        <#case "input">
        <#case "email">
        <#case "tel">
        <#default>
            <#local class = addClassArg(class,"input") >
        <#break>
    </#switch>
    <#local overrideDefaultClass = mergeArgMaps(overrideDefaultClass,{"class":class!""}) >
    <#local args = mergeArgMaps(args, overrideDefaultClass)>
    <@scipioStdTmplLib.field args=args ><#nested></@scipioStdTmplLib.field>
</#macro>

<#-- @field label area markup - theme override -->
<#macro field_markup_labelarea labelType="" labelPosition="" label="" labelContent=false labelDetail=false fieldType="" fieldsType="" fieldId="" collapse=""
required=false labelContentArgs={} norows=false nocells=false container=true origArgs={} passArgs={} catchArgs...>
    <#local label = escapeVal(label, 'htmlmarkup')?trim>
    <#if !labelContent?is_boolean>
        <@contentArgRender content=labelContent args=labelContentArgs doTrim=true />
    <#elseif label?has_content>
        <label class="label"<#if fieldId?has_content> for="${escapeVal(fieldId, 'html')}"</#if>>${label}<#if required> *</#if></label>
    </#if>
    <#if !labelDetail?is_boolean><@contentArgRender content=labelDetail args=labelContentArgs doTrim=true /></#if>
</#macro>

<#macro field_file_markup_widget class="" alert="" name="" value="" size="" style="" maxlength="" autocomplete="" id="" title="" tooltip="" fieldTitleBlank=false inlineLabel=false
required=false attribs={} origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.field_file_default!"")>
    <#if tooltip?has_content>
        <#local class = addClassArg(class, styles.field_file_tooltip!styles.field_default_tooltip!"")>
        <#local attribs = (styles.field_file_tooltip_attribs!styles.field_default_tooltip_attribs!{}) + attribs>
        <#local title = tooltip>
    </#if>
    <div class="file <#if title?has_content>has-name</#if>">
        <label class="file-label">
            <input type="file"<@fieldElemAttribStr attribs=attribs /><@fieldClassAttribStr class=class alert=alert /><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#rt/>
            <#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#if value?has_content> value="${escapeVal(value, 'html')}"</#if><#if size?has_content> size="${size}"</#if><#if title?has_content> title="${escapeVal(title, 'html')}"</#if><#t/>
            <#if required> required="required"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if autocomplete?has_content> autocomplete="off"</#if>/><#t/>
            <span class="file-cta">
                <span class="file-icon">
                    <i class="fa fa-upload"></i>
                </span>
                <span class="file-label">
                    ${getLabel("ContentDataResourceUpload")}
                </span>
            </span>
            <#if title?has_content>
                <span class="file-name">${escapeVal(title, 'html')}"</span>
            </#if>
        </label>
    </div>
</#macro>


<#-- @field container markup - theme override
    #nested is the actual field widget (<input>, <select>, etc.). -->
<#macro field_markup_container type="" fieldsType="" defaultGridArgs={} gridArgs={}
postfix=false postfixContent=true labelArea=true labelType="" labelPosition="" labelAreaContent="" collapse=""
collapseLabel="" collapsePostfix="" norows=true nocells=true container=true containerId="" containerClass="" containerStyle=""
preWidgetContent=false postWidgetContent=false preLabelContent=false postLabelContent=false prePostfixContent=false postPostfixContent=false
labelAreaContentArgs={} postfixContentArgs={} prePostContentArgs={}
widgetAreaClass="" labelAreaClass="form-label" postfixAreaClass="" widgetPostfixAreaClass="" inverted=false labelSmallDiffColumns=""
origArgs={} passArgs={} required=false noLabelCell=true noInputCell=true catchArgs...>
    <#local rowClass = containerClass>
    <#local isSwitch = passArgs.isSwitch!false>
    <#local fieldEntryTypeClass = "">
    <#local labelInRow = (labelType != "vertical")>
    <#local iftaLabel = passArgs.iftaLabel!false>

    <#local widgetPostfixCombined = gridArgs.widgetPostfixCombined!defaultGridArgs.widgetPostfixCombined!"">
    <#if !widgetPostfixCombined?is_boolean>
        <#if postfix && !collapse> <#-- previously: ((postfix && collapsePostfix) && !collapse) -->
            <#local widgetPostfixCombined = styles["fields_" + fieldsType + "_widgetpostfixcombined"]!styles["fields_default_widgetpostfixcombined"]!true>
        <#else>
            <#local widgetPostfixCombined = false>
        </#if>
    </#if>
    <#local defaultGridStyles = getDefaultFieldGridStyles(defaultGridArgs + {"labelInRow": labelInRow,
    "widgetPostfixCombined":widgetPostfixCombined} + gridArgs)>

    <#if container>
        <#local containerClass = "field">
        <#local widgetAreaClass = "control">
    </#if>

    <#if (postfix?has_content && postfix)|| (prefix?has_content && prefix)>
        <#if container>
            <#if ((labelArea && labelPosition == "left") || (labelArea && labelPosition == "right"))>
                <#local containerClass = addClassArg(containerClass,"has-addons")>
            </#if>
        </#if>
        <#local noInputCell=false/>
    <#else>
        <#if container>
            <#if !containerClass?has_content>
                <#local containerClass = "field">
            </#if>
        </#if>
        <#local noInputCell=false/>
    </#if>

    <#if (labelArea && labelPosition == "left") || (labelArea && labelPosition == "right")>
        <#local containerClass = addClassArg(containerClass,"is-horizontal")>
        <#local labelAreaClass = addClassArg(labelAreaClass,"field-label")>
        <#local widgetAreaClass = addClassArg(widgetAreaClass,"is-expanded")>
    </#if>
    <#if containerClass?has_content><div<@fieldClassAttribStr class=containerClass/>></#if>
    <#if (labelArea && labelPosition == "left") || (labelArea && labelPosition == "top")>
        <#if labelAreaClass?has_content><div<@fieldClassAttribStr class=labelAreaClass/>></#if>
        <#if !preLabelContent?is_boolean><@contentArgRender content=preLabelContent args=prePostContentArgs /></#if>
        <#if !labelAreaContent?is_boolean><@contentArgRender content=labelAreaContent args=labelAreaContentArgs /></#if>
        <#if !postLabelContent?is_boolean><@contentArgRender content=postLabelContent args=prePostContentArgs /></#if>
        <#if labelAreaClass?has_content></div></#if>
    </#if>
    <#if (labelArea && labelPosition == "left") || (labelArea && labelPosition == "right")>
        <div class="field-body">
        <div class="field <#if (labelArea && labelPosition == "left") || (labelArea && labelPosition == "right")>has-addons</#if>">
    </#if>
    <#if widgetAreaClass?has_content><div<@fieldClassAttribStr class=widgetAreaClass/>></#if>
    <#if !preWidgetContent?is_boolean><@contentArgRender content=preWidgetContent args=prePostContentArgs /></#if>
    <#nested>
    <#if !postWidgetContent?is_boolean><@contentArgRender content=postWidgetContent args=prePostContentArgs /></#if>
    <#if widgetAreaClass?has_content></div></#if>
    <#if postfix>
        <#if !prePostfixContent?is_boolean><@contentArgRender content=prePostfixContent args=prePostContentArgs /></#if>
        <#if (postfixContent?is_boolean && postfixContent == true) || !postfixContent?has_content>
            <div class="control">
                <#--<input type="submit" class="${styles.icon!} ${styles.icon_button!}" value="${styles.icon_button_value!}"/>-->
                <button type="submit" class="button">Submit</button>
            </div>
        <#elseif !postfixContent?is_boolean> <#-- boolean false means prevent markup -->
            <#if !postfixContent?is_boolean><div class="control"><@contentArgRender content=postfixContent args=postfixContentArgs /></div></#if>
        </#if>
        <#if !postPostfixContent?is_boolean><div class="control"><@contentArgRender content=postPostfixContent args=prePostContentArgs /></div></#if>
    </#if>
    <#if (labelArea && labelPosition == "left") || (labelArea && labelPosition == "right")>
        </div>
        </div>
    </#if>
    <#if containerClass?has_content></div></#if>
</#macro>


<#macro field_datetime_markup_widget name="" class="" style="" title="" value="" size="" maxlength="" id="" dateType="" dateDisplayType=""
timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString=""
hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""
alert=false mask="" events={} step="" timeValues="" tooltip="" postfix="" postfixColumns="" inlinePostfix=false manualInput=true collapse=false fieldTitleBlank=false origLabel="" inlineLabel=false
dateDisplayFormat="" required=false attribs={} origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.field_datetime_default!"")>
    <#if !dateDisplayFormat?has_content>
        <#local dateDisplayFormat = getFieldDateDispFmtStd(dateDisplayType)>
    </#if>
    <#local displayInputId = "">
    <#if !postfixColumns?is_number>
        <#local postfixColumns = 1>
    </#if>
    <#local inputId = "">
    <#if id?has_content>
        <#local displayInputId = raw(id) + "_i18n">
        <#local inputId = id>
    </#if>
    <#local displayInputName = "">
    <#local inputName = "">
    <#if name?has_content>
        <#local displayInputName = raw(name) + "_i18n">
        <#local inputName = name>
    </#if>
    <#if !postfix?is_boolean>
        <#local postfix = true>
    </#if>

    <#if tooltip?has_content>
        <#local class = addClassArg(class, styles.field_datetime_tooltip!styles.field_default_tooltip!"")>
        <#-- tooltip supplants title -->
        <#local title = tooltip>
        <#local attribs = (styles.field_datetime_tooltip_attribs!styles.field_default_tooltip_attribs!{}) + attribs>
    </#if>
    <#if title?is_boolean && title == false>
        <#local title = "">
    <#else>
        <#if title?is_boolean && title == true>
            <#local title = "">
        </#if>
        <#-- 2018-02-16: title/tooltip can now start with "+" to indicate prepend to default -->
        <#local titlePrefix = "">
        <#if raw(title)?starts_with("+")>
            <#local titlePrefix = raw(title)[1..]>
            <#local title = "">
        </#if>
        <#if !title?has_content>
            <#local title = styles.field_datetime_default_title!>
        </#if>
        <#if title?has_content>
            <#local dateFormatString = raw(uiLabelMap.CommonFormat) + ": " + raw(dateDisplayFormat)>
            <#if title == "FORMAT">
                <#local title = dateFormatString>
            <#elseif title == "LABEL">
                <#local title = origLabel>
            <#elseif title == "LABEL+FORMAT">
                <#if origLabel?has_content>
                    <#local title = raw(origLabel) + " (" + raw(dateFormatString) + ")">
                <#else>
                    <#local title = dateFormatString>
                </#if>
            <#else>
                <#local title = getTextLabelFromExpr(title, {"dateLabel":origLabel, "dateFormatString":dateFormatString})!"">
            </#if>
        </#if>
        <#if titlePrefix?has_content>
            <#if title?has_content>
                <#local title = titlePrefix + (styles.field_datetime_title_sep!" - ") + title>
            <#else>
                <#local title = titlePrefix>
            </#if>
        </#if>
    </#if>
    <input type="date" name="${escapeVal(displayInputName, 'html')}"<@fieldClassAttribStr class=class alert=alert /><#rt/>
    <@fieldElemAttribStr attribs=attribs /><#t/>
    <#if events?has_content><@commonElemEventAttribStr events=events /></#if><#t/>
    <#if title?has_content> title="${escapeVal(title, 'html')}"</#if><#t/>
    <#if value?has_content> value="${escapeVal(value, 'html')}"</#if><#t/>
    <#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#t/>
    <#if size?has_content> size="${size}"</#if><#t/>
    <#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
    <#if !manualInput> readonly="readonly"</#if><#t/>
    <#if displayInputId?has_content> id="${escapeVal(displayInputId, 'html')}"</#if><#t/>
    <#if required> required="required"</#if><#t/>
    <#--don't set this stuff here anymore, because fdatepicker has bugs with this and also it's better to factor out the fdatepicker-specific stuff into the script macro:
            data-date="" data-date-format="${escapeVal(datePickerFmt, 'html')}"<#if dateDisplayType == "month"> data-start-view="year" data-min-view="year"</#if>--> /><#t/>
    <input type="hidden"<#if inputName?has_content> name="${escapeVal(inputName, 'html')}"</#if><#if inputId?has_content> id="${escapeVal(inputId, 'html')}"</#if><#if value?has_content> value="${escapeVal(value, 'html')}"</#if> />
    <#-- TODO?: we don't handle this case yet - only show date (renderer already passed dateType="date" for this). but we must submit zero values for the event to pass. -->
    <#if timeDropdown == "time-dropdown">
        <#if timeHourName?has_content>
            <input type="hidden" name="${escapeVal(timeHourName, 'html')}" value="0"/>
        </#if>
        <#if timeMinutesName?has_content>
            <input type="hidden" name="${escapeVal(timeMinutesName, 'html')}" value="0"/>
        </#if>
    </#if>
    <#-- NOTE: this is still needed in general (but mostly for timeDropdown). only one value supported by events currenlty (Timestamp). -->
    <#if compositeType?has_content>
        <input type="hidden" name="${escapeVal(compositeType, 'html')}" value="Timestamp"/>
    </#if>
    <@field_datetime_markup_script inputId=inputId inputName=inputName displayInputId=displayInputId displayInputName=displayInputName
    dateType=dateType dateDisplayType=dateDisplayType required=required attribs=toSimpleMap(attribs) origArgs=origArgs passArgs=passArgs />
</#macro>

<#assign field_datetime_disptypefmts = {
"timestamp": "yyyy-MM-dd HH:mm:ss.SSS",
"date": "yyyy-MM-dd",
"month": "yyyy-MM",
"time": "HH:mm:ss.SSS"
}>

<#macro field_datetime_markup_script inputId="" inputName="" displayInputId="" displayInputName="" dateType=""
dateDisplayType="" htmlwrap=true required=false origArgs={} passArgs={} catchArgs...>
    <#-- Display behavior flags -->
    <#-- NOTE: displayCorrect && useFillDate are needed for fdatepicker, otherwise the YYYY-MM-DD selection constantly resets the dates -->
    <#local displayCorrect = true><#-- if true, display input is re-assigned value on every change; if false, lets datepicker choose -->
    <#local useFillDate = true && displayCorrect><#-- if true, the digits that picker can't set are preserved from last value - only works for simple dateDisplayConvFmt (YYYY-MM-DD) -->

    <#local dateConvFmt = field_datetime_typefmts[dateType]!><#-- Real date format -->
    <#local dateDisplayConvFmt = field_datetime_disptypefmts[dateDisplayType]!>
    <#-- Effective display format for when displayCorrect==true (bypass for picker display format)
      (field_datetime_disptypefmts: friendly; field_datetime_typefmts: internal; custom hash possible) -->
    <#local dateEffDispConvFmt = field_datetime_typefmts[dateDisplayType]!>

    <#switch dateDisplayType>
        <#case "timestamp">
        <#local fdpExtraOpts>,dateFormat:"${field_datetime_disptypefmts.date}", timeFormat:"${field_datetime_disptypefmts.time}", type:"datetime"</#local>
        <#break>
        <#case "date">
        <#local fdpExtraOpts>,dateFormat:"${field_datetime_disptypefmts.date}",  type:"date"</#local>
        <#break>
        <#case "month">
        <#local fdpExtraOpts>,dateFormat:"${field_datetime_disptypefmts.month}", type:"date"</#local>
        <#break>
        <#case "time">
        <#local fdpExtraOpts>,timeFormat:"${field_datetime_disptypefmts.time}", type:"time"</#local>
        <#break>
    </#switch>

    <#local displayInputIdJs = escapeVal(displayInputId, 'js')>
    <#local inputIdJs = escapeVal(inputId, 'js')>

    <#local fdatepickerOptions>{showTodayButton:true${fdpExtraOpts}}</#local><#lt/>
    <@script htmlwrap=htmlwrap>
        $(function() {
        var sfdh = new ScpFieldDateHelper({ <#-- see selectall.js -->
            displayInputId: "${displayInputIdJs}",
            inputId: "${inputIdJs}",
            displayCorrect: ${displayCorrect?string},
            useFillDate: ${useFillDate?string},
            dateFmt: "${dateConvFmt}",
            dateDisplayFmt: "${dateDisplayConvFmt}",
            dateEffDispFmt: "${dateEffDispConvFmt}"
        });

        jQuery("#${displayInputIdJs}").change(function() {
            sfdh.updateNormDateInputFromI18n(this.value);
        });

        var onDatePopup = function(ev) {
            sfdh.saveOldDateFromI18n();
        };

        var onDateChange = function(ev) {
            <#-- for fdatepicker, the default value lookup on displayInputId input is enough here. -->
            sfdh.updateAllDateInputs(sfdh.getDefUpAllInputArgs());
        };

        bulmaCalendar.attach('#${displayInputIdJs}', ${fdatepickerOptions});
    });
  </@script>
</#macro>

<#macro field_datefind_markup_widget id="" class="" style="" alert="" name="" localizedInputTitle="" value="" value2="" size="" maxlength="" dateType="" dateDisplayType="" dateDisplayFormat=""
formName="" defaultDateTimeString="" imgSrc="" localizedIconTitle="" titleClass="" defaultOptionFrom="" defaultOptionThru=""
opEquals="" opSameDay="" opGreaterThanFromDayStart="" opGreaterThan="" opLessThan="" opUpToDay="" opUpThruDay="" opIsEmpty=""
title="" tooltip="" inlineLabel=false inlinePostfix=false origLabel=origLabel required=false attribs={} origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.field_datefind_default!"")>
    <#local selectClass = styles.field_datefind_select_default!"">
    <#-- NOTE: values of localizedInputTitle are: uiLabelMap.CommonFormatDate/Time/DateTime -->
    <#-- Reuse the template-derived formats -->
    <#if !dateDisplayFormat?has_content>
        <#local dateDisplayFormat = getFieldDateDispFmtStd(dateDisplayType)>
    </#if>
    <#local displayInputId = "">
    <#local inputId = "">
    <#if id?has_content>
        <#local displayInputId = raw(id) + "_i18n">
        <#local inputId = id>
    </#if>
    <#local displayInputName = "">
    <#local inputName = "">
    <#if name?has_content>
        <#local displayInputName = raw(name) + "_fld0_i18n">
        <#local inputName = raw(name) + "_fld0_value">
    </#if>
    <#local opSelectName = "">
    <#if name?has_content>
        <#local opSelectName = raw(name) + "_fld0_op">
    </#if>
    <#if tooltip?has_content>
        <#local class = addClassArg(class, styles.field_datefind_tooltip!styles.field_default_tooltip!"")>
        <#local title = tooltip>
        <#local attribs = (styles.field_datefind_tooltip_attribs!styles.field_default_tooltip_attribs!{}) + attribs>
    <#elseif !title?has_content>
        <#local title = localizedInputTitle>
    </#if>
    <div class="field has-addons" data-date="">
        <div class="control is-expanded">
            <input type="text"<@fieldElemAttribStr attribs=attribs /><#if displayInputId?has_content> id="${escapeVal(displayInputId, 'html')}"</#if><#if displayInputName?has_content> name="${escapeVal(displayInputName, 'html')}"</#if><@fieldClassAttribStr class=class alert=alert /><#rt/>
            <#if title?has_content> title="${escapeVal(title, 'html')}"</#if><#if value?has_content> value="${escapeVal(value, 'html')}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#rt/>/><#lt/>
            <input type="hidden"<#if inputId?has_content> id="${escapeVal(inputId, 'html')}"</#if><#if inputName?has_content> name="${escapeVal(inputName, 'html')}"</#if><#if value?has_content> value="${escapeVal(value, 'html')}"</#if>/>
        </div>
        <div class="control">
            <div class="select">
                <select<#if opSelectName?has_content> name="${escapeVal(opSelectName, 'html')}"</#if><#-- class="selectBox"-->>
                    <option value="equals"<#if defaultOptionFrom == "equals"> selected="selected"</#if>>${escapeVal(opEquals, 'htmlmarkup')}</option>
                    <option value="sameDay"<#if defaultOptionFrom == "sameDay"> selected="selected"</#if>>${escapeVal(opSameDay, 'htmlmarkup')}</option>
                    <option value="greaterThanFromDayStart"<#if defaultOptionFrom == "greaterThanFromDayStart"> selected="selected"</#if>>${escapeVal(opGreaterThanFromDayStart, 'htmlmarkup')}</option>
                    <option value="greaterThan"<#if defaultOptionFrom == "greaterThan"> selected="selected"</#if>>${escapeVal(opGreaterThan, 'htmlmarkup')}</option>
                </select>
            </div>
        </div>
    </div>
    <@field_datetime_markup_script inputId=inputId inputName=inputName displayInputId=displayInputId displayInputName=displayInputName dateType=dateType dateDisplayType=dateDisplayType
    required=required attribs=toSimpleMap(attribs) origArgs=origArgs passArgs=passArgs />
</#macro>

<#macro field_select_widget args={} inlineArgs...>
    <#local overrideDefaultClass = {}>
    <#local args = mergeArgMaps(args, inlineArgs, scipioStdTmplLib.field_select_widget_defaultArgs,overrideDefaultClass)>
    <#local dummy = localsPutAll(args)>
    <#if (!multiple?is_boolean && (multiple == "true" || multiple == "multiple" || multiple == "Y")) || (multiple?is_boolean && multiple)>
        <div class="select is-multiple">
    <#else>
        <div class="select">
    </#if>
        <@scipioStdTmplLib.field_select_widget args=args><#nested></@scipioStdTmplLib.field_select_widget>
    </div>
</#macro>

<#macro field_lookup_markup_widget name="" formName="" fieldFormName="" class="" alert="false" value="" size="" style=""
    maxlength="" id="" events={} readonly=false autocomplete="" descriptionFieldName=""
    targetParameterIter="" imgSrc="" ajaxUrl="" ajaxEnabled=false presentation="layer" width=""
    height="" position="" fadeBackground="true" clearText="" showDescription="" initiallyCollapsed=""
    lastViewName="main" title="" fieldTitleBlank=false inlineLabel=false inlinePostfix=false tooltip="" required=false attribs={} origArgs={} passArgs={} catchArgs...>
  <#--<#if Static["org.ofbiz.widget.model.ModelWidget"].widgetBoundaryCommentsEnabled(context)>
  </#if>-->
  <#local class = addClassArg(class, styles.field_lookup_default!"")>
  <#if tooltip?has_content>
    <#local class = addClassArg(class, styles.field_lookup_tooltip!styles.field_default_tooltip!"")>
    <#local title = tooltip>
    <#local attribs = (styles.field_lookup_tooltip_attribs!styles.field_default_tooltip_attribs!{}) + attribs>
  </#if>
  <#if (!ajaxUrl?has_content) && ajaxEnabled>
    <#local ajaxUrl = requestAttributes._REQUEST_HANDLER_.makeLink(request, response, raw(fieldFormName))/>
    <#local ajaxUrl = raw(id) + "," + raw(ajaxUrl) + ",ajaxLookup=Y" />
  </#if>
  <#if (!showDescription?has_content)>
    <#local showDescriptionProp = getPropertyValue("widget", "widget.lookup.showDescription")!"N">
    <#if "Y" == showDescriptionProp>
      <#local showDescription = "true" />
    <#else>
      <#local showDescription = "false" />
    </#if>
  </#if>
  <#if ajaxEnabled>
    <@script>
      jQuery(document).ready(function(){
        if (!jQuery('form[name="${escapeVal(formName, 'js')}"]').length) {
          alert("Developer: for lookups to work you must provide a form name!")
        }
      });
    </@script>
  </#if>
        <#if size?has_content && size=="0">
          <input type="hidden"<#if name?has_content> name="${escapeVal(name, 'html')}"/></#if>
        <#else>
          <div class="field has-addons">
            <div class="control is-expanded">
              <input type="text"<@fieldElemAttribStr attribs=attribs /><@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${escapeVal(name, 'html')}"</#if><#if value?has_content> value="${escapeVal(value, 'html')}"</#if><#if style?has_content> style="${escapeVal(style, 'html')}"</#if><#rt/>
                <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if id?has_content> id="${escapeVal(id, 'html')}"</#if><#t/>
                <#if readonly?has_content && readonly> readonly="readonly"</#if><#if events?has_content><@commonElemEventAttribStr events=events /></#if><#t/>
                <#if autocomplete?has_content> autocomplete="off"</#if><#t/>
                <#if title?has_content> title="${escapeVal(title, 'html')}"</#if><#t/>
                <#if required> required="required"</#if><#t/>
                /></#if><#t/>
            </div>
            <div class="control">
                <#if presentation?has_content && descriptionFieldName?has_content && presentation == "window">
                      <a href="javascript:call_fieldlookup3(document['${escapeVal(formName, 'js-html')}']['${escapeVal(name, 'js-html')}'], <#rt/>
                          document['${escapeVal(formName, 'js-html')}']['${escapeVal(descriptionFieldName, 'js-html')}'], '${escapeVal(fieldFormName, 'js-html')}', '${escapeVal(presentation, 'js-html')}'<#t/>
                      <#if targetParameterIter?has_content>
                        <#list targetParameterIter as item>
                          ,document['${escapeVal(formName, 'js-html')}']['${escapeVal(item, 'js-html')}'].value <#t/>
                        </#list>
                      </#if>
                      );" class="button"></a><#rt/>
                <#elseif presentation?has_content && presentation == "window">
                        <a href="javascript:call_fieldlookup2(document['${escapeVal(formName, 'js-html')}']['${escapeVal(name, 'js-html')}'], '${escapeVal(fieldFormName, 'js-html')}', '${escapeVal(presentation, 'js-html')}'<#rt/>
                          <#if targetParameterIter?has_content>
                            <#list targetParameterIter as item>
                              ,document['${escapeVal(formName, 'js-html')}']['${escapeVal(item, 'js-html')}'].value<#t/>
                            </#list>
                          </#if>
                          );"  class="button"></a><#rt/>
                <#else>

                    <#local defaultMinLength = getPropertyValue("widget", "widget.autocompleter.defaultMinLength")!2>
                    <#local defaultDelay = getPropertyValue("widget", "widget.autocompleter.defaultDelay")!300>
                    <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
                    <#if !ajaxUrl?contains("searchValueFieldName=")>
                      <#if descriptionFieldName?has_content && showDescription == "true">
                        <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + descriptionFieldName />
                      <#else>
                        <#local ajaxUrl = ajaxUrl + "&amp;searchValueFieldName=" + name />
                      </#if>
                    </#if>

                    <@script>
                    jQuery(document).ready(function(){
                      var options = {
                        requestUrl : "${escapeVal(fieldFormName, 'js')}",
                        inputFieldId : "${escapeVal(id, 'js')}",
                        dialogTarget : document['${escapeVal(formName, 'js')}']['${escapeVal(name, 'js')}'],
                        dialogOptionalTarget : <#if descriptionFieldName?has_content>document['${escapeVal(formName, 'js')}']['${escapeVal(descriptionFieldName, 'js')}']<#else>null</#if>,
                        formName : "${escapeVal(formName, 'js')}",
                        lookupId: '${escapeVal(id, 'html')}',
                        width : "${width}",
                        height : "${height}",
                        position : "topright",
                        modal : "true",
                        ajaxUrl : <#if ajaxEnabled>"${escapeFullUrl(ajaxUrl, 'js')}"<#else>""</#if>,
                        showDescription : <#if ajaxEnabled>"${showDescription}"<#else>false</#if>,
                        presentation : "${escapeVal(presentation, 'js')}",
                        defaultMinLength : "${defaultMinLength}",
                        defaultDelay : "${defaultDelay}",
                        args :
                            <#if targetParameterIter?has_content>
                              <#local isFirst = true>
                              [<#t/>
                              <#list targetParameterIter as item>
                                <#if isFirst>
                                  document['${escapeVal(formName, 'js')}']['${escapeVal(item, 'js')}']<#t/>
                                  <#local isFirst = false>
                                <#else>
                                  ,document['${escapeVal(formName, 'js')}']['${escapeVal(item, 'js')}']<#t/>
                                </#if>
                              </#list>
                              ]<#t/>
                            <#else>[]
                            </#if>
                      };
                      new Lookup(options).init();
                    });
                    </@script>
                    <a href="javascript:void(0);" id="${escapeVal(id, 'html')}_button"  class="button"><i class="fa fa-search"></i></a>
              </#if>
            </div>
            <#if readonly?has_content && readonly>
                <div class="control">
                    <a id="${escapeVal(id, 'html')}_clear"
                    style="background:none;margin-left:5px;margin-right:15px;"
                    class="clearField"
                    href="javascript:void(0);"
                    onclick="javascript:document['${escapeVal(formName, 'js-html')}']['${escapeVal(name, 'js-html')}'].value='';
                      jQuery('#' + jQuery('#${escapeVal(id, 'js-html')}_clear').next().attr('id').replace('_button','') + '_${escapeVal(id, 'js-html')}_lookupDescription').html('');
                      <#if descriptionFieldName?has_content>document['${escapeVal(formName, 'js-html')}']['${escapeVal(descriptionFieldName, 'js-html')}'].value='';</#if>">
                      <#if clearText?has_content>${escapeVal(clearText, 'htmlmarkup')}<#else>${escapeVal(uiLabelMap.CommonClear, 'htmlmarkup')}</#if>
                  </a>
                </div>
            </#if>
        </div>

      <#if (presentation?has_content && presentation == "window")>
        <#local defaultMinLength = getPropertyValue("widget", "widget.autocompleter.defaultMinLength")!2>
        <#local defaultDelay = getPropertyValue("widget", "widget.autocompleter.defaultDelay")!300>
        <#if ajaxUrl?index_of("_LAST_VIEW_NAME_") < 0>
          <#local ajaxUrl = ajaxUrl + "&amp;_LAST_VIEW_NAME_=" + lastViewName />
        </#if>
        <@script>ajaxAutoCompleter('${escapeFullUrl(ajaxUrl, 'js')}', ${showDescription}, ${defaultMinLength}, ${defaultDelay});</@script><#t/>
      </#if>
</#macro>

<#macro field_textfind_markup_widget name="" formName="" value="" defaultOption="" opEquals="" opBeginsWith="" opContains=""
opIsEmpty="" opNotEqual="" opLike="" class="" id="" style="" alert="" size="" maxlength="" autocomplete=true titleClass=""
hideIgnoreCase="" ignoreCase=false ignoreCaseMsg="" title="" tooltip="" fieldTitleBlank=false hideOptions=false inlineLabel=false
origLabel="" collapse=false required=false attribs={} origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.field_textfind_default!"")>
    <#local selectClass = styles.field_textfind_select_default!"">
    <#if tooltip?has_content>
        <#local class = addClassArg(class, styles.field_textfind_tooltip!styles.field_default_tooltip!"")>
        <#local title = tooltip>
        <#local attribs = (styles.field_textfind_tooltip_attribs!styles.field_default_tooltip_attribs!{}) + attribs>
    </#if>
    <#-- Get heuristic-based current container sizes -->
    <#local absColSizes = getAbsContainerSizeFactors()>
    <#-- we say the parent is large if the ratio is greater than 6 -->
    <#local isLargeParent = (absColSizes.large > 6)>
    <#-- NOTE: all the code below assumes that the parent containers are using only
      large-x classes to split the page or form into columns (e.g., large-6, not medium-6) -->
        <div class="field has-addons">
                <#if !hideOptions>
                    <#local newName = name/>
                        <div class="control">
                            <div class="select">
                                <select<#if name?has_content> name="${escapeVal(name, 'html')}_op"</#if><#if selectClass?has_content> class="${selectClass}"</#if>><#-- class="selectBox"-->
                                    <option value="equals"<#if defaultOption == "equals"> selected="selected"</#if>>${escapeVal(opEquals, 'htmlmarkup')}</option>
                                    <option value="contains"<#if defaultOption == "contains"> selected="selected"</#if>>${escapeVal(opContains, 'htmlmarkup')}</option>
                                    <option value="empty"<#if defaultOption == "empty"> selected="selected"</#if>>${escapeVal(opIsEmpty, 'htmlmarkup')}</option>
                                    <option value="notEqual"<#if defaultOption == "notEqual"> selected="selected"</#if>>${escapeVal(opNotEqual, 'htmlmarkup')}</option>
                                    <option value="like"<#if defaultOption == "like"> selected="selected"</#if>>${escapeVal(opBeginsWith, 'htmlmarkup')} (${escapeVal(opLike, 'htmlmarkup')})</option>
                                </select>
                            </div>
                        </div>
                <#else>
                    <input type="hidden"<#if name?has_content> name="${escapeVal(name, 'html')}_op"</#if> value="${escapeVal(defaultOption, 'html')}"/><#rt/>
                </#if>
                    <div class="control is-expanded">
                        <input type="text"<@fieldClassAttribStr class=class alert=alert /> name="${escapeVal(name, 'html')}"<#if value?has_content> value="${escapeVal(value, 'html')}"</#if><#rt/>
                        <#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#if !autocomplete> autocomplete="off"</#if><#t/>
                        <@fieldElemAttribStr attribs=attribs /><#t/>
                        <#if title?has_content> title="${escapeVal(title, 'html')}"</#if>/><#t/>
                    </div>
            <#if hideIgnoreCase>
                <input type="hidden" name="${escapeVal(name, 'html')}_ic" value="<#if ignoreCase>Y<#else></#if>"/><#rt/>
            <#else>
                <div class="control">
                    <div class="field-label is-normal">
                        <label class="label " for="${escapeVal(name, 'html')}_ic"><input type="checkbox" id="${escapeVal(name, 'html')}_ic" name="${escapeVal(name, 'html')}_ic" value="Y"<#if ignoreCase> checked="checked"</#if>/>
                            ${escapeVal(ignoreCaseMsg, 'htmlmarkup')}</label>
                        <#rt/>
                    </div>
                </div>
            </#if>
        </div>
</#macro>

<#macro field_rangefind_markup_widget class="" id="" style="" alert="" name="" formName="" value="" size="" maxlength="" autocomplete=true titleClass=""
defaultOptionFrom="" opEquals="" opGreaterThan="" opGreaterThanEquals="" opLessThan="" opLessThanEquals="" opIsEmpty="" value2=""
defaultOptionThru="" title="" tooltip="" inlineLabel=false origLabel="" collapse=false required=false attribs={} origArgs={} passArgs={} catchArgs...>
    <#local class = addClassArg(class, styles.field_rangefind_default!"")>
    <#local selectClass = styles.field_rangefind_select_default!"">
    <#if tooltip?has_content>
        <#local class = addClassArg(class, styles.field_rangefind_tooltip!styles.field_default_tooltip!"")>
        <#local title = tooltip>
        <#local attribs = (styles.field_rangefind_tooltip_attribs!styles.field_default_tooltip_attribs!{}) + attribs>
    </#if>
    <#local class1="${styles.grid_small!}9 ${styles.grid_large!}9"/>
    <#local class2="${styles.grid_small!}3 ${styles.grid_large!}3"/>
    <div class="field has-addons">
        <div class="control is-expanded">
            <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${escapeVal(name, 'html')}_fld0_value"</#if><#rt/>
            <#if value?has_content> value="${escapeVal(value, 'html')}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
            <#if !autocomplete> autocomplete="off"</#if><#t/>
            <@fieldElemAttribStr attribs=attribs /><#t/>
            <#if title?has_content> title="${escapeVal(title, 'html')}"</#if>/><#t/>

        </div>
         <div class="control">
            <div class="select">
            <select<#if name?has_content> name="${escapeVal(name, 'html')}_fld0_op"</#if><#if selectClass?has_content> class="${selectClass}"</#if>><#-- class="selectBox"-->
                <option value="equals"<#if defaultOptionFrom=="equals"> selected="selected"</#if>>${escapeVal(opEquals, 'htmlmarkup')}</option>
                <option value="greaterThan"<#if defaultOptionFrom=="greaterThan"> selected="selected"</#if>>${escapeVal(opGreaterThan, 'htmlmarkup')}</option>
                <option value="greaterThanEqualTo"<#if defaultOptionFrom=="greaterThanEqualTo"> selected="selected"</#if>>${escapeVal(opGreaterThanEquals, 'htmlmarkup')}</option>
            </select><#rt/>
            </div>
        </div>
            <#rt/>
        <div class="control is-expanded">
            <input type="text"<@fieldClassAttribStr class=class alert=alert /><#if name?has_content> name="${escapeVal(name, 'html')}_fld1_value"</#if><#rt/>
            <#if value2?has_content> value="${escapeVal(value2, 'html')}"</#if><#if size?has_content> size="${size}"</#if><#if maxlength?has_content> maxlength="${maxlength}"</#if><#t/>
            <#if !autocomplete> autocomplete="off"</#if><#t/>
            <@fieldElemAttribStr attribs=attribs /><#t/>
            <#if title?has_content> title="${escapeVal(title, 'html')}"</#if>/><#t/>
        </div>
        <div class="control">
            <div class="select">
                <select<#if name?has_content> name="${escapeVal(name, 'html')}_fld1_op"</#if><#if selectClass?has_content> class="${selectClass}"</#if>><#-- class="selectBox"-->
                    <option value="lessThan"<#if defaultOptionThru == "lessThan"> selected="selected"</#if>>${escapeVal(opLessThan, 'htmlmarkup')}</option>
                    <option value="lessThanEqualTo"<#if defaultOptionThru == "lessThanEqualTo"> selected="selected"</#if>>${escapeVal(opLessThanEquals, 'htmlmarkup')}</option>
                </select>
            </div>
        </div>
    </div>
</#macro>

<#-- save copy of this namespace so that our macros are able to access its own definitions without overrides (sometimes needed) -->
<#assign scipioMetroTmplLib = copyObject(.namespace)>
