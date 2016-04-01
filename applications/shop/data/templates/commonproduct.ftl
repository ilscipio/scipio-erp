<?xml version="1.0" encoding="UTF-8"?>
<#--
Cato: Template for generic Cato Shop products.

FIXME: Can't use globals, so limited
TODO: Should group entities together so faster data load, but can't without globals
-->
<entity-engine-xml>
<#recurse doc>
</entity-engine-xml>

<#macro products>
<#recurse .node>
</#macro>

<#macro product>
    <#-- WARN: productId must be short (7 chars or less) -->
    <#local productId = .node.@productId[0]>
    <#local productName = .node.@productName[0]>
    <#local productCategoryId = .node.@productCategoryId[0]>

    <#local extraCategoryId1 = (.node.@extraCategoryId1[0])!"">
    <#local extraCategoryId2 = (.node.@extraCategoryId2[0])!"">
    
    <#local productTypeId = (.node.@productTypeId[0])!"">
    <#if !productTypeId?has_content>
      <#local productTypeId = "FINISHED_GOOD">
    </#if>

    <#local urlName = (.node.@urlName[0])!"">
    <#if !urlName?has_content>
      <#local urlName = productName?lower_case?replace(r"\s+", "-", "r")?replace(r"[^a-z0-9-]", "", "r")>
    </#if>

    <#local description = (.node.@description[0])!"">
    <#if !description?has_content>
      <#local description = productName>
    </#if>
    <#local longDescription = (.node.@longDescription[0])!"">
    <#if !longDescription?has_content>
      <#local longDescription = description>
    </#if>

    <#local listPrice = (.node.@listPrice[0])!"">
    <#local defaultPrice = (.node.@defaultPrice[0])!"">
    <#if !listPrice?has_content>
      <#local listPrice = defaultPrice>
    <#elseif !defaultPrice?has_content>
      <#local defaultPrice = listPrice>
    </#if>

    <Product productId="${productId}" productTypeId="${productTypeId}" primaryProductCategoryId="${productCategoryId}" productName="${productName?xml}" internalName="${productName?xml}" description="${description?xml}" longDescription="${longDescription?xml}" taxable="Y" chargeShipping="Y" autoCreateKeywords="Y" isVirtual="N" isVariant="N" createdDate="2001-05-13 12:00:00.0" createdByUserLogin="admin" lastModifiedDate="2001-05-13 12:00:00.0" lastModifiedByUserLogin="admin"/>

    <DataResource dataResourceTypeId="ELECTRONIC_TEXT" dataResourceId="${productId}-ALT" localeString="en"/>
    <DataResource dataResourceTypeId="ELECTRONIC_TEXT" dataResourceId="DR${productId}-ALTEN" localeString="en_US"/>

    <ElectronicText dataResourceId="${productId}-ALT" textData="${urlName}"/>
    <ElectronicText dataResourceId="DR${productId}-ALTEN" textData="${urlName}"/>

    <Content contentId="${productId}-ALT" contentTypeId="DOCUMENT" dataResourceId="${productId}-ALT" localeString="en"/>
    <Content contentId="C${productId}-ALTEN" contentTypeId="DOCUMENT" dataResourceId="DR${productId}-ALTEN" localeString="en_US"/>

    <ContentAssoc contentId="${productId}-ALT" contentIdTo="C${productId}-ALTEN" contentAssocTypeId="ALTERNATE_LOCALE" fromDate="2011-04-26 12:00:00.0"/>

    <ProductContent productId="${productId}" contentId="${productId}-ALT" productContentTypeId="ALTERNATIVE_URL" fromDate="2001-05-13 12:00:00.0"/>

    <ProductPrice productId="${productId}" productPricePurposeId="PURCHASE" productPriceTypeId="DEFAULT_PRICE" currencyUomId="USD" productStoreGroupId="_NA_" fromDate="2001-05-13 12:00:00.0" price="${defaultPrice}" createdDate="2001-05-13 12:00:00.0" createdByUserLogin="admin" lastModifiedDate="2001-05-13 12:00:00.0" lastModifiedByUserLogin="admin"/>
    <ProductPrice productId="${productId}" productPricePurposeId="PURCHASE" productPriceTypeId="LIST_PRICE" currencyUomId="USD" productStoreGroupId="_NA_" fromDate="2001-05-13 12:00:00.0" price="${listPrice}" createdDate="2001-05-13 12:00:00.0" createdByUserLogin="admin" lastModifiedDate="2001-05-13 12:00:00.0" lastModifiedByUserLogin="admin"/>

    <ProductCategoryMember productCategoryId="${productCategoryId}" productId="${productId}" fromDate="2001-05-13 12:00:00.0"/>
  <#if extraCategoryId1?has_content>
    <ProductCategoryMember productCategoryId="${extraCategoryId1}" productId="${productId}" fromDate="2001-05-13 12:00:00.0"/>
  </#if>
  <#if extraCategoryId2?has_content>
    <ProductCategoryMember productCategoryId="${extraCategoryId2}" productId="${productId}" fromDate="2001-05-13 12:00:00.0"/>
  </#if>

</#macro>

<#macro @element>
</#macro>
