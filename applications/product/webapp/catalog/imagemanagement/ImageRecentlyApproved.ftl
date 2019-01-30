<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
          <#assign targetLink><@pageUrl>ListImageRecentlyApproved?productId=${rawString(show.productId)}&date1=${rawString(productContentEntry.timeStampDate1)}&date2=${rawString(productContentEntry.timeStampDate2)}&showDate=${rawString(productContentEntry.date)}</@pageUrl></#assign>
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
