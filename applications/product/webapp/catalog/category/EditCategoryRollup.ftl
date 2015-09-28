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

<#if productCategoryId?has_content>
    <@section title="${uiLabelMap.ProductCategoryRollupParentCategories}">
        <#if (currentProductCategoryRollups.size() <= 0)>
            <@resultMsg>${uiLabelMap.ProductNoParentCategoriesFound}.</@resultMsg>
        <#else>        
            <form method="post" action="<@ofbizUrl>updateProductCategoryToCategory</@ofbizUrl>" name="updateProductCategoryForm">
              <input type="hidden" name="showProductCategoryId" value="${productCategoryId}" />
            <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
                 <@thead>
                   <@tr class="header-row">
                        <@th>${uiLabelMap.ProductParentCategoryId}</@th>
                        <@th>${uiLabelMap.CommonFromDate}</@th>
                        <@th align="center">${uiLabelMap.ProductThruDateTimeSequence}</@th>
                        <@th>&nbsp;</@th>
                   </@tr>
                 </@thead>
                 <@tbody>
                    <#list currentProductCategoryRollups as productCategoryRollup>
                    <#assign suffix = "_o_" + productCategoryRollup_index>
                    <#assign curCategory = productCategoryRollup.getRelatedOne("ParentProductCategory", false)>
                    <#assign hasntStarted = false>
                    <#if productCategoryRollup.fromDate?? && nowTimestamp.before(productCategoryRollup.getTimestamp("fromDate"))><#assign hasntStarted = true></#if>
                    <#assign hasExpired = false>
                    <#if productCategoryRollup.thruDate?? && nowTimestamp.after(productCategoryRollup.getTimestamp("thruDate"))><#assign hasExpired = true></#if>
                    <@tr valign="middle">
                        <@td><#if curCategory?has_content>
                                <a href="<@ofbizUrl>EditCategory?productCategoryId=${curCategory.productCategoryId}</@ofbizUrl>" >
                                    <#assign catContentWrapper = Static["org.ofbiz.product.category.CategoryContentWrapper"].makeCategoryContentWrapper(curCategory, request)!>
                                    <#if catContentWrapper?has_content>
                                        ${catContentWrapper.get("CATEGORY_NAME", "html")!catContentWrapper.get("DESCRIPTION", "html")!curCategory.categoryName!curCategory.description!}
                                    <#else>
                                        ${curCategory.categoryName!curCategory.description!}
                                    </#if>
                                    [${curCategory.productCategoryId}]
                                </a>
                            </#if>
                        </@td>
                        <#assign colorStyle><#if hasntStarted>style="color: red;"</#if></#assign>
                        <@td style=colorStyle>${productCategoryRollup.fromDate}</@td>
                        <@td align="center">
                            <input type="hidden" name="showProductCategoryId${suffix}" value="${productCategoryRollup.productCategoryId}" />
                            <input type="hidden" name="productCategoryId${suffix}" value="${productCategoryRollup.productCategoryId}" />
                            <input type="hidden" name="parentProductCategoryId${suffix}" value="${productCategoryRollup.parentProductCategoryId}" />
                            <input type="hidden" name="fromDate${suffix}" value="${productCategoryRollup.fromDate}" />
                            <@htmlTemplate.renderDateTimeField name="thruDate${suffix}" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="${productCategoryRollup.thruDate!''}" size="25" maxlength="30" id="thruDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
                            <input type="text" size="5" name="sequenceNum${suffix}" value="${productCategoryRollup.sequenceNum!}" />
                        </@td>
                        <@td>
                            <a href="javascript:document.removeProductCategoryFromCategory_${productCategoryRollup_index}.submit();" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
                        </@td>
                    </@tr>
                    </#list>
                  </@tbody>
                  <@tfoot>
                    <@tr valign="middle">
                        <@td colspan="4" align="center">
                            <input type="submit" value="${uiLabelMap.CommonUpdate}" style="font-size: x-small;" />
                            <input type="hidden" value="${currentProductCategoryRollups.size()}" name="_rowCount" />
                        </@td>
                    </@tr>
                  </@tfoot>
            </@table>
            </form>
            
          <#list currentProductCategoryRollups as productCategoryRollup>
            <form name="removeProductCategoryFromCategory_${productCategoryRollup_index}" method="post" action="<@ofbizUrl>removeProductCategoryFromCategory</@ofbizUrl>">
                <input type="hidden" name="showProductCategoryId" value="${productCategoryId}"/>
                <input type="hidden" name="productCategoryId" value="${productCategoryRollup.productCategoryId}"/>
                <input type="hidden" name="parentProductCategoryId" value="${productCategoryRollup.parentProductCategoryId}"/>
                <input type="hidden" name="fromDate" value="${productCategoryRollup.fromDate}"/>
            </form>
          </#list>
        </#if>      
    </@section>

    <@section title="${uiLabelMap.ProductAddCategoryParent} ${uiLabelMap.ProductCategorySelectCategoryAndEnterFromDate}">
        <form method="post" action="<@ofbizUrl>addProductCategoryToCategory</@ofbizUrl>" name="addParentForm">
          <@fields labelArea=false>
            <input type="hidden" name="productCategoryId" value="${productCategoryId}" />
            <input type="hidden" name="showProductCategoryId" value="${productCategoryId}" />
            <@field type="generic">
                <@htmlTemplate.lookupField value="${requestParameters.SEARCH_CATEGORY_ID!}" formName="addParentForm" name="parentProductCategoryId" id="parentProductCategoryId" fieldFormName="LookupProductCategory"/>
            </@field>
            <@field type="generic">
                <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className=""  title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate_1" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
            <@field type="submitarea">
                <input type="submit" value="${uiLabelMap.CommonAdd}" />
            </@field>
          </@fields>
        </form>
    </@section>

    <@section title="${uiLabelMap.ProductCategoryRollupChildCategories}">
        <#if parentProductCategoryRollups.size() == 0>
            <@resultMsg>${uiLabelMap.ProductNoChildCategoriesFound}.</@resultMsg>
        <#else>
            <form method="post" action="<@ofbizUrl>updateProductCategoryToCategory</@ofbizUrl>" name="updateProductCategoryToCategoryChild">
            <input type="hidden" name="showProductCategoryId" value="${productCategoryId}" />
            <@table type="data-list" autoAltRows=true cellspacing="0" class="basic-table">
              <@thead>
                <@tr class="header-row">
                    <@th>${uiLabelMap.ProductChildCategoryId}</@th>
                    <@th>${uiLabelMap.CommonFromDate}</@th>
                    <@th align="center">${uiLabelMap.ProductThruDateTimeSequence}</@th>
                    <@th>&nbsp;</@th>
                </@tr>           
               </@thead>
                    <@tbody>
                    <#assign lineChild = 0>
                    <#list parentProductCategoryRollups as productCategoryRollup>
                    <#assign suffix = "_o_" + lineChild>
                    <#assign lineChild = lineChild + 1>
                    <#assign curCategory = productCategoryRollup.getRelatedOne("CurrentProductCategory", false)>
                    <#assign hasntStarted = false>
                    <#if productCategoryRollup.fromDate?? && nowTimestamp.before(productCategoryRollup.getTimestamp("fromDate"))><#assign hasntStarted = true></#if>
                    <#assign hasExpired = false>
                    <#if productCategoryRollup.thruDate?? && nowTimestamp.after(productCategoryRollup.getTimestamp("thruDate"))><#assign hasExpired = true></#if>
                        <@tr valign="middle">
                            <@td><#if curCategory?has_content>
                                    <a href="<@ofbizUrl>EditCategory?productCategoryId=${curCategory.productCategoryId}</@ofbizUrl>" >
                                        <#assign catContentWrapper = Static["org.ofbiz.product.category.CategoryContentWrapper"].makeCategoryContentWrapper(curCategory, request)!>
                                        <#if catContentWrapper?has_content>
                                            ${catContentWrapper.get("CATEGORY_NAME", "html")!catContentWrapper.get("DESCRIPTION", "html")!curCategory.categoryName!curCategory.description!}
                                        <#else>
                                            ${curCategory.categoryName!curCategory.description!}
                                        </#if>
                                        [${curCategory.productCategoryId}]
                                    </a>
                                </#if>
                            </@td>
                            <#assign colorStyle><#if hasntStarted>style="color: red;"</#if></#assign>
                            <@td style=colorStyle>${productCategoryRollup.fromDate}</@td>
                            <@td align="center">
                                <input type="hidden" name="productCategoryId${suffix}" value="${productCategoryRollup.productCategoryId}" />
                                <input type="hidden" name="parentProductCategoryId${suffix}" value="${productCategoryRollup.parentProductCategoryId}" />
                                <input type="hidden" name="fromDate${suffix}" value="${productCategoryRollup.fromDate}" />
                                <@htmlTemplate.renderDateTimeField name="thruDate${suffix}" event="" action="" className=""  title="Thru Date" value="${productCategoryRollup.thruDate!''}" size="25" maxlength="50" id="thruDatefromDate${suffix}" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>

                                <input type="text" size="5" name="sequenceNum${suffix}" value="${productCategoryRollup.sequenceNum!}" />
                            </@td>
                            <@td>
                                <a href="javascript:document.removeProductCategoryFromCategory_1_${productCategoryRollup_index}.submit();" class="${styles.button_default!}">${uiLabelMap.CommonDelete}</a>
                            </@td>
                        </@tr>
                    </#list>
                    </@tbody>
                    <@tfoot>
                      <@tr valign="middle">
                        <@td colspan="4" align="center">
                            <input type="submit" value="${uiLabelMap.CommonUpdate}" style="font-size: x-small;" />
                            <input type="hidden" value="${lineChild}" name="_rowCount" />
                        </@td>
                      </@tr>
                    </@tfoot>
            </@table>
            </form>
            
          <#list parentProductCategoryRollups as productCategoryRollup>
             <form name="removeProductCategoryFromCategory_1_${productCategoryRollup_index}" method="post" action="<@ofbizUrl>removeProductCategoryFromCategory</@ofbizUrl>">
                 <input type="hidden" name="showProductCategoryId" value="${productCategoryId}"/>
                 <input type="hidden" name="productCategoryId" value="${productCategoryRollup.productCategoryId}"/>
                 <input type="hidden" name="parentProductCategoryId" value="${productCategoryRollup.parentProductCategoryId}"/>
                 <input type="hidden" name="fromDate" value="${productCategoryRollup.fromDate}"/>
             </form>
          </#list>
        </#if>
    </@section>

    <@section title="${uiLabelMap.ProductAddCategoryChild} ${uiLabelMap.ProductCategorySelectCategoryAndEnterFromDate}">
        <form method="post" action="<@ofbizUrl>addProductCategoryToCategory</@ofbizUrl>" name="addChildForm">
          <@fields labelArea=false>
            <input type="hidden" name="showProductCategoryId" value="${productCategoryId}" />
            <input type="hidden" name="parentProductCategoryId" value="${productCategoryId}" />
            <@field type="generic">
                <@htmlTemplate.lookupField value="${requestParameters.SEARCH_CATEGORY_ID!}" formName="addChildForm" name="productCategoryId" id="productCategoryId" fieldFormName="LookupProductCategory"/>
            </@field>
            <@field type="generic">
                <@htmlTemplate.renderDateTimeField name="fromDate" event="" action="" className="" title="Format: yyyy-MM-dd HH:mm:ss.SSS" value="" size="25" maxlength="30" id="fromDate_2" dateType="date" shortDateInput=false timeDropdownParamName="" defaultDateTimeString="" localizedIconTitle="" timeDropdown="" timeHourName="" classString="" hour1="" hour2="" timeMinutesName="" minutes="" isTwelveHour="" ampmName="" amSelected="" pmSelected="" compositeType="" formName=""/>
            </@field>
            <@field type="submitarea">
                <input type="submit" value="${uiLabelMap.CommonAdd}" />
            </@field>
          </@fields>
        </form>
    </@section>
</#if>
