<#ftl ns_prefixes={
    "D":"http://docbook.org/ns/docbook",
    "xl":"http://www.w3.org/1999/xlink"
    }>  
<#--^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ must be at the top of the file........-->
<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#macro text text>
  <#list text?children as child>
    <#if child?node_type == "text">
      ${child}
    <#elseif child?node_type == 'element' && child?node_name == "link">
      <a href="${child["@xl:href"]}">${child}</a>
    <#elseif child?node_type == 'element' && child?node_name == "orderedlist">
     <@orderedlist node=child/>
    <#elseif child?node_type == 'element' && child?node_name == "itemizedlist">
     <@itemizedlist node=child/>
    <#elseif child?node_type == 'element' && child?node_name == "mediaobject">
      <@mediaobject node=child/>
    <#elseif child?node_type == 'element' && child?node_name == "emphasis">
      <span class="${child["@role"]}">${child}</span>
    <#elseif child?node_type == 'element' && child?node_name == "programlisting">
      <pre>${child}</pre>
    </#if>
  </#list>
</#macro>

<#-- SCIPIO: FIXME: hides scipio macro of same name -->
<#macro section inSection level first="no">
  <#list inSection.* as subSection>
    <#if subSection?node_name == "title">
      <#list subSection?children as subTitle>
        <#if subTitle?node_type == "text">
          <#if first == "yes">
            <h1>${subTitle}</h1>
          <#else>
            <br /><h${level}>${subTitle}</h${level}>
          </#if>
        <#else>
          <#if subTitle?node_name == "anchor">
            <span id="${subTitle["@xml:id"]}" />
          </#if>
        </#if>
      </#list>
    <#elseif subSection?node_name == "para">
        <p><@para para=subSection/></p>
    <#elseif subSection?node_name == "section">
        <#assign levelPlus=level?number +1/>
        <@section inSection=subSection level="${levelPlus}"/>
    <#elseif subSection?node_name == "orderedlist">
        <@orderedlist node=subSection/>
    <#elseif subSection?node_name == "itemizedlist">
        <@itemizedlist node=subSection/>
    <#elseif subSection?node_name == "caution">
        <span class="caution"><@admonition node=subSection/></span>
    <#elseif subSection?node_name == "important">
        <span class="important"><@admonition node=subSection/></span>
    <#elseif subSection?node_name == "note">
        <span class="note"><@admonition node=subSection/></span>
    <#elseif subSection?node_name == "tip">
        <span class="tip"><@admonition node=subSection/></span>
    <#elseif subSection?node_name == "warning">
        <span class="warning"><@admonition node=subSection/></span>
    </#if>
  </#list>
</#macro>

<#macro listItems node>
  <#list node?children as item>
    <#if item?node_type == "element" && item?node_name == "listitem">
      <#list item.* as subpara>
        <li><@para para=subpara/></li>
      </#list>  
    </#if>
  </#list>
</#macro>

<#macro orderedlist node>
  <ol class="numbers"><@listItems node=node/></ol>
</#macro>

<#macro itemizedlist node>
  <ul class="dots"><@listItems node=node/></ul>
</#macro>

<#macro mediaobject node>
  <#list node?children as item>
    <#if item?node_type == "element" && item?node_name == "imageobject">
        <#assign fileref = item.imagedata["@fileref"]/>
        <#assign depth = item.imagedata["@depth"]/>
        <#assign width = item.imagedata["@width"]/>
    <#elseif item?node_type == "element" && item?node_name == "textobject">
        <#assign alt = item.phrase/>
    <#elseif item?node_type == "element" && item?node_name == "caption">
        <#assign caption = item/>
    </#if>
  </#list>
  <img src="${fileref}" <#if depth?has_content> height="${depth}"</#if> <#if width?has_content> width="${width}"</#if> alt="<#if alt?has_content>${alt}</#if>" />
  <#if caption?has_content><div>${caption}</div></#if>
</#macro>

<#macro para para>
  <@text text=para/>
</#macro>

<#macro admonition node>
  <#list node.* as subSection>
    <#if subSection?node_name == "title">
      <h3>${subSection}</h3>
    <#elseif subSection?node_name == "para">
      <p><@para para=subSection/></p>
    </#if>
  </#list>
</#macro>

<div class="contentarea">
  <div id="column-container">
    <div id="content-main-section">
    <@section inSection=doc.section first="yes" level=1/>
    </div>
  </div>
</div>

