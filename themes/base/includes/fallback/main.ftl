<#if layoutSettings.headerImageUrl??>
    <#assign headerImageUrl = layoutSettings.headerImageUrl>
<#elseif layoutSettings.commonHeaderImageUrl??>
    <#assign headerImageUrl = layoutSettings.commonHeaderImageUrl>
<#elseif layoutSettings.VT_HDR_IMAGE_URL??>
    <#assign headerImageUrl = layoutSettings.VT_HDR_IMAGE_URL.get(0)>
</#if>
<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>

<#assign logo><img src="<@ofbizContentUrl escapeAs='html'><#if headerImageUrl?has_content>${rawString(headerImageUrl)}<#else>/images/scipio/scipio-logo-small.png</#if></@ofbizContentUrl>"/></#assign>


<@row>
	<#-- NOTE: login_wrap contains grid size -->
	<@cell class="${styles.grid_large!}6 ${styles.grid_large!}centered" last=true id="login" style="margin-top: 200px">
	  <div id="login-box-title" class="${styles.login_header!}">
	    <@heading level=1>${logo} ${uiLabelMap.CommonSystemNotConfigured!}</@heading>
	  </div>
	
	  <@section id="system-box-content">
	    <#if uiLabelMap.WebtoolsForSomethingInteresting?has_content 
	       && uiLabelMap.WebtoolsForSomethingInteresting != "WebtoolsForSomethingInteresting">
	      <@alert type="error">
	        ${uiLabelMap.WebtoolsForSomethingInteresting}
	      </@alert>
	    </#if>
	
	    <div class="${styles.login_body!}">
	    	The system is not properly configured. In order to set it up go to...[TODO]
	    </div>
	
	  </@section>
	</@cell>
</@row>