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
<#assign rowCount = 0>
<fo:table table-layout="fixed" border-spacing="3pt">
  <fo:table-column column-width="0.8in"/>
  <fo:table-column column-width="0.8in"/>
  <fo:table-column column-width="1.2in"/>
  <fo:table-column column-width="0.5in"/>
  <fo:table-column column-width="0.5in"/>
  <fo:table-column column-width="1.2in"/>
  <fo:table-column column-width="0.5in"/>
  <fo:table-column column-width="0.5in"/>
  <fo:table-column column-width="1in"/>
  <fo:table-column column-width="1in"/>
  <fo:table-header>
    <fo:table-row>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductProductId}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductProduct}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductFromLocation}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductQoh}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductAtp}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductToLocation}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductQoh}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductAtp}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductMinimumStock}</fo:block>
      </fo:table-cell>
      <fo:table-cell>
        <fo:block font-weight="bold">${uiLabelMap.ProductMoveQuantity}</fo:block>
      </fo:table-cell>
    </fo:table-row>
  </fo:table-header>
  <fo:table-body>
  <#if moveByOisgirInfoList?has_content || moveByPflInfoList?has_content>
      <#assign alt_row = false>
      <#list moveByOisgirInfoList! as moveByOisgirInfo>
          <#assign product = moveByOisgirInfo.product>
          <#assign facilityLocationFrom = moveByOisgirInfo.facilityLocationFrom>
          <#assign facilityLocationTypeEnumFrom = (facilityLocationFrom.getRelatedOne("TypeEnumeration", true))!>
          <#assign facilityLocationTo = moveByOisgirInfo.facilityLocationTo>
          <#assign targetProductFacilityLocation = moveByOisgirInfo.targetProductFacilityLocation>
          <#assign facilityLocationTypeEnumTo = (facilityLocationTo.getRelatedOne("TypeEnumeration", true))!>
          <#assign totalQuantity = moveByOisgirInfo.totalQuantity>
          <fo:table-row>
          <#-- <tr id="moveInfoId_tableRow_${rowCount}" valign="middle"<@tableRowClassAttribStr alt=alt_row />> -->
              <fo:table-cell>
                <fo:block>${product.productId}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${product.internalName!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${facilityLocationFrom.areaId!}:${facilityLocationFrom.aisleId!}:${facilityLocationFrom.sectionId!}:${facilityLocationFrom.levelId!}:${facilityLocationFrom.positionId!}<#if facilityLocationTypeEnumFrom?has_content>(${facilityLocationTypeEnumFrom.description})</#if>[${facilityLocationFrom.locationSeqId}]</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByOisgirInfo.quantityOnHandTotalFrom!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByOisgirInfo.availableToPromiseTotalFrom!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${facilityLocationTo.areaId!}:${facilityLocationTo.aisleId!}:${facilityLocationTo.sectionId!}:${facilityLocationTo.levelId!}:${facilityLocationTo.positionId!}<#if facilityLocationTypeEnumTo?has_content>(${facilityLocationTypeEnumTo.description})</#if>[${facilityLocationTo.locationSeqId}]</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByOisgirInfo.quantityOnHandTotalTo!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByOisgirInfo.availableToPromiseTotalTo!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${targetProductFacilityLocation.minimumStock!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${targetProductFacilityLocation.moveQuantity!}</fo:block>
              </fo:table-cell>
          <#-- </@tr> -->
          </fo:table-row>
          <#assign rowCount = rowCount + 1>
          <#-- toggle the row color -->
          <#assign alt_row = !alt_row>
      </#list>
      <#list moveByPflInfoList! as moveByPflInfo>
          <#assign product = moveByPflInfo.product>
          <#assign facilityLocationFrom = moveByPflInfo.facilityLocationFrom>
          <#assign facilityLocationTypeEnumFrom = (facilityLocationFrom.getRelatedOne("TypeEnumeration", true))!>
          <#assign facilityLocationTo = moveByPflInfo.facilityLocationTo>
          <#assign targetProductFacilityLocation = moveByPflInfo.targetProductFacilityLocation>
          <#assign facilityLocationTypeEnumTo = (facilityLocationTo.getRelatedOne("TypeEnumeration", true))!>
          <#assign totalQuantity = moveByPflInfo.totalQuantity>
          <#-- <tr id="moveInfoId_tableRow_${rowCount}" valign="middle"<@tableRowClassAttribStr alt=alt_row />> -->
          <fo:table-row>
              <fo:table-cell>
                <fo:block>${product.productId}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${product.internalName!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${facilityLocationFrom.areaId!}:${facilityLocationFrom.aisleId!}:${facilityLocationFrom.sectionId!}:${facilityLocationFrom.levelId!}:${facilityLocationFrom.positionId!}<#if facilityLocationTypeEnumFrom?has_content>(${facilityLocationTypeEnumFrom.description})</#if>[${facilityLocationFrom.locationSeqId}]</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByPflInfo.quantityOnHandTotalFrom!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByPflInfo.availableToPromiseTotalFrom!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${facilityLocationTo.areaId!}:${facilityLocationTo.aisleId!}:${facilityLocationTo.sectionId!}:${facilityLocationTo.levelId!}:${facilityLocationTo.positionId!}<#if facilityLocationTypeEnumTo?has_content>(${facilityLocationTypeEnumTo.description})</#if>[${facilityLocationTo.locationSeqId}]</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByPflInfo.quantityOnHandTotalTo!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${moveByPflInfo.availableToPromiseTotalTo!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${targetProductFacilityLocation.minimumStock!}</fo:block>
              </fo:table-cell>
              <fo:table-cell>
                <fo:block>${targetProductFacilityLocation.moveQuantity!}</fo:block>
              </fo:table-cell>
          </fo:table-row>
          <#-- </@tr> -->
          <#assign rowCount = rowCount + 1>
      </#list>
  </#if>
  <#assign messageCount = 0>
  <#list pflWarningMessageList! as pflWarningMessage>
      <#assign messageCount = messageCount + 1>
      <fo:table-row>
        <fo:table-cell>
          <fo:block>${messageCount}:${pflWarningMessage}.</fo:block>
        </fo:table-cell>
      </fo:table-row>
  </#list>
  </fo:table-body>
</fo:table>