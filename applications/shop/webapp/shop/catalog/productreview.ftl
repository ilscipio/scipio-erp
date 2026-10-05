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
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">
<#if (requestParameters?has_content && requestParameters.product_id??) || productId?has_content>
  <@section title=uiLabelMap.OrderCustomerReviews>
          <@row>
            <@cell small=4>
                <@panel>
                    <div class="${styles.text_center}">
                        <@heading>${uiLabelMap.OrderAverageRating}</@heading>
                        <div>
                            <b>${(averageRating!0)?string("0.#")}</b> / 5
                        </div>
                        <div>
                            <@ratingAsStars rating=averageRating!0 />
                        </div>
                        <small>(${uiLabelMap.CommonFrom} ${numRatings!0} ${uiLabelMap.OrderRatings})</small>
                    </div>
                </@panel>
            </@cell>
          </@row>
      
      
      <#if productReviews?has_content>
        <@section>
        <#list productReviews[0..*5] as productReview>
          <#assign postedUserLogin = productReview.getRelatedOne("UserLogin", false)>
          <#assign postedPerson = postedUserLogin.getRelatedOne("Person", false)!>
          <@row class="+review">
            <@cell small=2 class="+${styles.text_center}">
                    <div class="avatar">
                        <i class="${styles.icon} ${styles.icon_prefix}user" style="font-size:5em;"></i>
                    </div>
                    <div class="time"><small><@formattedDateTime date=productReview.postedDateTime!productReview.createdStamp! defaultVal="0000-00-00 00:00"/></small></div>
            </@cell>
            <@cell small=10>
                    <blockquote class="blockquote">
                        <div class="rating">
                            <strong>${uiLabelMap.OrderRanking}: <@ratingAsStars rating=productReview.productRating!0 /> (${productReview.productRating!?string} / 5)</strong>
                            
                        </div>
                        <#if productReview.productReview?has_content>
                            <p>${productReview.productReview!}</p>
                        </#if>
                        <footer class="blockquote-footer"><#if (productReview.postedAnonymous!("N")) == "Y">${uiLabelMap.OrderAnonymous}<#else>${postedPerson.firstName} ${postedPerson.lastName}</#if></footer>
                    </blockquote>
                </@cell>
            </@row>
        </#list>
        </@section>
        <hr/>
      </#if>
      <@row>
        <@cell>
          <#if userLogin?has_content>
              <#--<@heading>${uiLabelMap.ProductReviewThisProduct}<@heading> -->
              
              <@form id="reviewProduct" method="post" action=makePageUrl("createProductReview")>
                <@fieldset class="inline">
                  <input type="hidden" name="productStoreId" value="${productStore.productStoreId}" />
                  <input type="hidden" name="productId" value="${productId!requestParameters.product_id!""}" />
                  <input type="hidden" name="categoryId" value="${categoryId!requestParameters.category_id!""}" />
                  
                  <#assign ratingItems = [
                    {"value":"1", "description":"1"}
                    {"value":"2", "description":"2"}
                    {"value":"3", "description":"3"}
                    {"value":"4", "description":"4"}
                    {"value":"5", "description":"5"}
                  ]>
                  <@field type="radio" name="productRating" label=uiLabelMap.EcommerceRating items=ratingItems currentValue="3.0"/>
                  <@field type="checkbox" name="postedAnonymous" label=uiLabelMap.EcommercePostAnonymous value="Y" currentValue="N" defaultValue="N"/>
                  <@field type="textarea" name="productReview" label=uiLabelMap.CommonReview cols="40"/>
            
                    <a href="javascript:document.getElementById('reviewProduct').submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a>
                    <#if requestParameters.product_id?has_content><a href="<@pageUrl>product?product_id=${requestParameters.product_id}</@pageUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonCancel}</a></#if>
                    
                </@fieldset>
              </@form>
                <#-- <@heading>${uiLabelMap.ProductCannotReviewUnKnownProduct}.</@heading>-->
                <#else>
                ${uiLabelMap.EcommerceLoggedToPost}
            </#if>
        </@cell>
    </@row>
  </@section>
</#if>