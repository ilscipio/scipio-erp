<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#if productCategoryId?? && productCategory??>
<@section title="${uiLabelMap.PageTitleEditCategoryProductCatalogs}">

        <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
            <@thead>
            <@tr class="header-row">
                <@th>${uiLabelMap.ProductCatalogNameId}</@th>
                <@th>${uiLabelMap.CommonType}</@th>
                <@th>${uiLabelMap.CommonFromDateTime}</@th>
                <@th align="center">${uiLabelMap.ProductThruDateTimeSequence}</@th>
                <@th>&nbsp;</@th>
            </@tr>
            </@thead>
            <#assign line = 0>
            <#list prodCatalogCategories as prodCatalogCategory>
            <#assign line = line + 1>
            <#assign prodCatalog = prodCatalogCategory.getRelatedOne("ProdCatalog", false)>
            <#assign curProdCatalogCategoryType = prodCatalogCategory.getRelatedOne("ProdCatalogCategoryType", true)>
            <@tr valign="middle">
                <@td><a href="<@ofbizUrl>EditProdCatalog?prodCatalogId=${(prodCatalogCategory.prodCatalogId)!}</@ofbizUrl>" class="${styles.button_default!}"><#if prodCatalog??>${(prodCatalog.catalogName)!}</#if> [${(prodCatalogCategory.prodCatalogId)!}]</a></@td>
                <@td>
                    ${(curProdCatalogCategoryType.get("description",locale))?default(prodCatalogCategory.prodCatalogCategoryTypeId)}
                </@td>
                <#assign hasntStarted = false>
                <#if (prodCatalogCategory.getTimestamp("fromDate"))?? && nowTimestamp.before(prodCatalogCategory.getTimestamp("fromDate"))> <#assign hasntStarted = true></#if>
                <#assign colorStyle><#if hasntStarted> style="color: red;"</#if></#assign>
                <@td style=colorStyle>${(prodCatalogCategory.fromDate)!}</@td>
                <@td align="center">
                    <form method="post" action="<@ofbizUrl>category_updateProductCategoryToProdCatalog</@ofbizUrl>" name="lineForm_update${line}">
                        <#assign hasExpired = false>
                        <#if (prodCatalogCategory.getTimestamp("thruDate"))?? && nowTimestamp.after(prodCatalogCategory.getTimestamp("thruDate"))> <#assign hasExpired = true></#if>
                        <input type="hidden" name="prodCatalogId" value="${(prodCatalogCategory.prodCatalogId)!}"/>
                        <input type="hidden" name="productCategoryId" value="${(prodCatalogCategory.productCategoryId)!}"/>
                        <input type="hidden" name="prodCatalogCategoryTypeId" value="${prodCatalogCategory.prodCatalogCategoryTypeId}"/>
                        <input type="hidden" name="fromDate" value="${(prodCatalogCategory.fromDate)!}"/>
                        <#if hasExpired><#assign class="alert"></#if>
                        <@htmlTemplate.renderDateTimeField name="thruDate" event="" action="" className="${class!''}"  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${(prodCatalogCategory.thruDate)!}" size="25" maxlength="30" id="thruDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                        <input type="text" size="5" name="sequenceNum" value="${(prodCatalogCategory.sequenceNum)!}"/>
                        <#-- the prodCatalogCategoryTypeId field is now part of the PK, so it can't be changed, must be re-created
                        <select name="prodCatalogCategoryTypeId" size="1">
                            <#if (prodCatalogCategory.prodCatalogCategoryTypeId)??>
                                <option value="${prodCatalogCategory.prodCatalogCategoryTypeId}"><#if curProdCatalogCategoryType??>${(curProdCatalogCategoryType.description)!}<#else> [${(prodCatalogCategory.prodCatalogCategoryTypeId)}]</#if></option>
                                <option value="${prodCatalogCategory.prodCatalogCategoryTypeId}"></option>
                            <#else>
                                <option value="">&nbsp;</option>
                            </#if>
                            <#list prodCatalogCategoryTypes as prodCatalogCategoryType>
                            <option value="${(prodCatalogCategoryType.prodCatalogCategoryTypeId)!}">${(prodCatalogCategoryType.get("description",locale))!}</option>
                            </#list>
                        </select> -->
                        <input type="submit" value="${uiLabelMap.CommonUpdate}"/>
                    </form>
                </@td>
                <@td align="center">
                  <form method="post" action="<@ofbizUrl>category_removeProductCategoryFromProdCatalog</@ofbizUrl>" name="lineForm_delete${line}">
                    <input type="hidden" name="prodCatalogId" value="${(prodCatalogCategory.prodCatalogId)!}"/>
                    <input type="hidden" name="productCategoryId" value="${(prodCatalogCategory.productCategoryId)!}"/>
                    <input type="hidden" name="prodCatalogCategoryTypeId" value="${prodCatalogCategory.prodCatalogCategoryTypeId}"/>
                    <input type="hidden" name="fromDate" value="${(prodCatalogCategory.fromDate)!}"/>
                    <a href="javascript:document.lineForm_delete${line}.submit()" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
                  </form>
                </@td>
            </@tr>
            </#list>
        </@table>

    <@section title="${uiLabelMap.ProductAddCatalogProductCategory}">
            <@table type="fields" cellspacing="0" class="basic-table">
                <@tr><@td>
                    <form method="post" action="<@ofbizUrl>category_addProductCategoryToProdCatalog</@ofbizUrl>" style="margin: 0;" name="addNewForm">
                        <input type="hidden" name="productCategoryId" value="${productCategoryId!}"/>
                        <select name="prodCatalogId">
                        <#list prodCatalogs as prodCatalog>
                            <option value="${(prodCatalog.prodCatalogId)!}">${(prodCatalog.catalogName)!} [${(prodCatalog.prodCatalogId)!}]</option>
                        </#list>
                        </select>
                        <select name="prodCatalogCategoryTypeId" size="1">
                        <#list prodCatalogCategoryTypes as prodCatalogCategoryType>
                            <option value="${(prodCatalogCategoryType.prodCatalogCategoryTypeId)!}">${(prodCatalogCategoryType.get("description",locale))!}</option>
                        </#list>
                        </select>
                        <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                        <input type="submit" value="${uiLabelMap.CommonAdd}"/>
                    </form>
                </@td></@tr>
            </@table>
    </@section>
</@section>
</#if>
