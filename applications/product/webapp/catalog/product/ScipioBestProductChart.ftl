<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#assign chartType=chartType!"pie"/>    <#-- (line|bar|pie) default: line -->
<#assign library=chartLibrary!"chart"/>
<#assign datasets=(chartDatasets!1)?number />

<#if bestSellingProducts?has_content>
    <#if chartType == "pie" || chartType == "bar">
        <@section title=uiLabelMap.ProductBestSellingProducts>        
            <#list mapKeys(bestSellingProducts) as dateIntervals>
                <#assign dateBeginText = dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()) />
                <#assign dateEndText = dateIntervals.getDateFormatter().format(dateIntervals.getDateEnd()) />
                <@chart title="${raw(dateBeginText)} - ${raw(dateEndText)}" type=chartType library=library xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"")>                
                    <#assign currData = bestSellingProducts.get(dateIntervals) />                
                    <#if currData?has_content> 
                        <#if datasets == 1>             
                            <#list currData as bestSellingProduct>     
                                <@chartdata value=(bestSellingProduct.qtyOrdered!0) title=(bestSellingProduct.productName!bestSellingProduct.productId)/>
                            </#list>                           
                        </#if>
                    </#if>
                </@chart>
            </#list>
        </@section>
    <#elseif chartType == "line">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#else>
    <@commonMsg type="result-norecord"/>            
</#if>