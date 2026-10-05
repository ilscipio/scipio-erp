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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->

<#if productImageList?has_content>
  <#if product?has_content>
      <@heading>${product.productId}</@heading>
  </#if>
  <#-- SCIPIO: NOTE: no need for handling rows when have tiles -->
  <@grid type="tiles" tilesType="gallery1">
        <#-- <#assign productName = productTextData >
        <#assign seoUrl = productName.replaceAll(" ", "-") > -->
        <#list productImageList as productImage>
              <#assign imgLink = makeContentUrl((productImage.productImage)!)/>
              <#assign thumbSrc = makeContentUrl((productImage.productImageThumb)!)/>
              <@tile size="normal" image=thumbSrc> <#-- can't use this, breaks Share button: link=imgLink so use View button instead -->
                  <@container class="+${styles.text_center!}">
                      <#--<a href="<@serverUrl>/catalog/images/${seoUrl}-${product.productId}/${seoUrl}-${contentName}</@serverUrl>" target="_blank"><img src="<@contentUrl>${(contentDataResourceView.drObjectInfo)!}</@contentUrl>" vspace="5" hspace="5" alt=""/></a>
                      <a href="<@contentUrl>${(productImage.productImage)!}</@contentUrl>" target="_blank"><img src="<@contentUrl>${(productImage.productImageThumb)!}</@contentUrl>" vspace="5" hspace="5" alt=""/></a>-->
                      <a href="<@contentUrl>${(productImage.productImage)!}</@contentUrl>" target="_blank" class="${styles.link_run_sys!} ${styles.action_view!}">${uiLabelMap.CommonView}</a>
                  </@container>
                  <@container class="+${styles.text_center!}">
                       <#--<a href="javascript:call_fieldlookup('','<@pageUrl>ImageShare?contentId=${productContentAndInfo.contentId}&amp;dataResourceId=${productContentAndInfo.dataResourceId}&amp;seoUrl=/catalog/images/${seoUrl}-${product.productId}/${seoUrl}-${contentName}</@pageUrl>','',${styles.gallery_share_view_width!},${styles.gallery_share_view_height!});" class="${styles.link_nav!} ${styles.action_send!}">${uiLabelMap.ImageManagementShare}</a>-->
                       <a href="javascript:call_fieldlookup('','<@pageUrl>ImageShare?contentId=${productImage.contentId}&amp;dataResourceId=${productImage.dataResourceId}</@pageUrl>','',${styles.gallery_share_view_width!},${styles.gallery_share_view_height!});" class="${styles.link_nav!} ${styles.action_send!}">${uiLabelMap.ImageManagementShare}</a>
                  </@container>
              </@tile>
        </#list>
  </@grid>
<#else>
  <@commonMsg type="result-norecord"/>
</#if>
