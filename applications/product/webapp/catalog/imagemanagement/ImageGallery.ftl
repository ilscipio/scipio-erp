<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
