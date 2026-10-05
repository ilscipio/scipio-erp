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
<#include "component://shop/webapp/shop/customer/customercommon.ftl">

<#if reviews?has_content>
  <@section title=uiLabelMap.ProductReviews>
    <@table type="data-list">
      <@tr>
        <@th>${uiLabelMap.EcommerceSentDate}</@th>
        <@th>${uiLabelMap.ProductProductId}</@th>
        <@th>${uiLabelMap.ProductReviews}</@th>
        <@th>${uiLabelMap.ProductRating}</@th>
        <@th>${uiLabelMap.CommonIsAnonymous}</@th>
        <@th>${uiLabelMap.CommonStatus}</@th>
      </@tr>
      <#list reviews as review>
        <@tr>
          <@td>${review.postedDateTime!}</@td>
          <@td><a href="<@catalogAltUrl productId=review.productId/>" style="${styles.link_nav_info_id!}>${review.productId}</a></@td>
          <@td>${review.productReview!}</@td>
          <@td>${review.productRating}</@td>
          <@td>${review.postedAnonymous!}</@td>
          <@td>${review.getRelatedOne("StatusItem", false).get("description", locale)}</@td>
        </@tr>
      </#list>
    </@table>
  </@section>
</#if>
