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
<#-- SCIPIO -->
<#if finAccountTrans?has_content> <#-- FIXME: Ugly workaround, because variables set by entity-condition do not validate correctly on screen condition -->
<@section title=uiLabelMap.AccountingFinAccountTransaction>
    <@table type="fields" >
        <#if finAccountTrans.finAccountTransId?has_content>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.FormFieldTitle_finAccountTransId}</@td>
              <@td colspan="3">
                ${finAccountTrans.finAccountTransId!}
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.statusId?has_content>
            <#assign currentStatus = finAccountTrans.getRelatedOne("StatusItem", false)/>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonStatus}</@td>
              <@td colspan="3">
                ${currentStatus.get('description',locale)}
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.finAccountTransTypeId?has_content>
            <#assign currType = finAccountTrans.getRelatedOne("FinAccountTransType", false)/>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonType}</@td>
              <@td colspan="3">
                ${currType.get('description',locale)}
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.glReconciliationId?has_content>
            <@tr>
              <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.FormFieldTitle_glReconciliationId}</@td>
              <@td colspan="3">
                <a href="<@pageUrl>ViewGlReconciliationWithTransaction?glReconciliationId=${finAccountTrans.glReconciliationId!}&finAccountId=${finAccountTrans.finAccountId!}</@pageUrl>">${finAccountTrans.glReconciliationId!}</a>
              </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.transactionDate?has_content>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.FormFieldTitle_transactionDate}</@td>
                <@td colspan="3">
                  <@formattedDateTime date=finAccountTrans.transactionDate />             
                </@td>
            </@tr>
        </#if>

        <#if finAccountTrans.amount?has_content>
            <@tr>
                <@td scope="row" class="${styles.grid_large!}3">${uiLabelMap.CommonAmount}</@td>
                <@td colspan="3">
                    <@ofbizCurrency isoCode=payment.currencyUomId amount=(finAccountTrans.amount!)/>    
                </@td>
            </@tr>
        </#if>
    </@table>
</@section>
</#if>