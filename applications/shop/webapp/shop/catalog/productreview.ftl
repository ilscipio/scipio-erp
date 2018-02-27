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
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">
<#if (requestParameters?has_content && requestParameters.product_id??) || productId?has_content>
  <@section title=uiLabelMap.OrderCustomerReviews>
      <#if averageRating?? && (averageRating > 0) && numRatings?? && (numRatings > 1)>
          <@row>
            <@cell small=4>
                <@panel>
                <@heading>${uiLabelMap.OrderAverageRating}</@heading>
                <div>
                    <b>${averageRating?string["0.#"]}</b> / 5
                </div>
                <div>
                </div>
                <#if numRatings??><small>(${uiLabelMap.CommonFrom} ${numRatings} ${uiLabelMap.OrderRatings})</small></#if>
                </@panel>
            </@cell>
            <hr/>
          </@row>
      </#if>
      
      <#if productReviews?has_content>
        <#list productReviews as productReview>
          <#assign postedUserLogin = productReview.getRelatedOne("UserLogin", false)>
          <#assign postedPerson = postedUserLogin.getRelatedOne("Person", false)!>
          <@row class="review">
            <@cell small=2 class="${styles.text_center}">
                    <div class="avatar">
                        <#if (productReview.postedAnonymous!("N")) == "Y">${uiLabelMap.OrderAnonymous}<#else>${postedPerson.firstName} ${postedPerson.lastName}</#if>
                    </div>
                    <div class="time"><small>${productReview.postedDateTime!}</small></div>
            </@cell>
            <@cell small=10>
                    <div class="rating">
                        <em>${uiLabelMap.OrderRanking}: ${productReview.productRating!?string}</em>
                    </div>
                    <#if productReview.productReview?has_content>
                        <p>${productReview.productReview!}</p>
                    </#if>
                </@cell>
            </@row>
        </#list>
        <hr/>
      </#if>
      <#if userLogin?has_content>
          <@row>
            <@cell>
      
              <#--<@heading>${uiLabelMap.ProductReviewThisProduct}<@heading> -->
              
              <@form id="reviewProduct" method="post" action=makeOfbizUrl("createProductReview")>
                <@fieldset class="inline">
                  <input type="hidden" name="productStoreId" value="${productStore.productStoreId}" />
                  <input type="hidden" name="productId" value="${productId!requestParameters.product_id!""}" />
                  <input type="hidden" name="categoryId" value="${categoryId!requestParameters.category_id!""}" />
                  
                  <#assign ratingItems = [
                    {"value":"1.0", "description":"1"}
                    {"value":"2.0", "description":"2"}
                    {"value":"3.0", "description":"3"}
                    {"value":"4.0", "description":"4"}
                    {"value":"5.0", "description":"5"}
                  ]>
                  <@field type="radio" name="productRating" label=uiLabelMap.EcommerceRating items=ratingItems currentValue=""/>
                  <@field type="checkbox" name="postedAnonymous" label=uiLabelMap.EcommercePostAnonymous value="Y" currentValue="N" defaultValue="N"/>
                  <@field type="textarea" name="productReview" label=uiLabelMap.CommonReview cols="40"/>
            
                    <a href="javascript:document.getElementById('reviewProduct').submit();" class="${styles.link_run_sys!} ${styles.action_update!}">${uiLabelMap.CommonSave}</a>
                    <#if requestParameters.product_id?has_content><a href="<@ofbizUrl>product?product_id=${requestParameters.product_id}</@ofbizUrl>" class="${styles.link_nav_cancel!}">${uiLabelMap.CommonCancel}</a></#if>
                    
                </@fieldset>
              </@form>
            <#-- <@heading>${uiLabelMap.ProductCannotReviewUnKnownProduct}.</@heading>-->
            </@cell>
        </@row>
    </#if>
  </@section>
</#if>