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
        
<#-- SCIPIO: this needs an explanation: 
     on stock this page grouped products by date and for each date showed a list of products,
     but no actual images.
     we want sample images at least, so instead of grouping by date we just make a tile
     for each product for each date group, let them arrangement themselves, and for each
     product it will show one sample image.
     TODO?: maybe could re-group the product images per-date using tile "imagesets" (many images per tile), 
        but they'll be really small -->
<@grid type="tiles" tilesType="gallery1">    
    <#list productContentEntries as productContentEntry>
    <#if productContentEntry?has_content> <#-- SCIPIO: WARN: entries may be null! -->
      <#if productContentEntry.approved?has_content>
        <#list productContentEntry.approved as show>
          <#assign thumbSrc = (productContentEntry.sampleImageList[show_index].productImageThumb)!"">
          <#assign targetLink><@ofbizUrl>ListImageRecentlyApproved?productId=${rawString(show.productId)}&date1=${rawString(productContentEntry.timeStampDate1)}&date2=${rawString(productContentEntry.timeStampDate2)}&showDate=${rawString(productContentEntry.date)}</@ofbizUrl></#assign>
          <#-- SCIPIO: NOTE: the productContentEntry.date used to be in a wrapper around the entries (outside
               <#list productContentEntry.approved as show>). but we'll just integrate it into the tiles as title. -->
          <@tile size="normal" image=thumbSrc title="${productContentEntry.date} - ${rawString(show.productId)}" link=targetLink>
            <#--link whole tile instead and put product ID in title
            <a href="${targetLink}" class="${styles.link_nav_info_id!}">${show.productId}</a> - ${productContentEntry.time[show_index]}-->
            ${productContentEntry.time[show_index]}
          </@tile>
        </#list>
      </#if>
    </#if>
    </#list>
</@grid>
