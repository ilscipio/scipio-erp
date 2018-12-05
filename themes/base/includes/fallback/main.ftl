<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>

<#assign logo><img src="<@ofbizContentUrl escapeAs='html'>/images/scipio/scipio-logo-small.png</@ofbizContentUrl>"/></#assign>

<@row>
	<#-- NOTE: login_wrap contains grid size -->
	<@cell class="${styles.grid_large!}12 ${styles.grid_large!} ${styles.login_wrap!}" last=true id="login" style="margin-top: 100px">
	  <div id="system-box-title">
	    <@heading level=1>${logo} ${uiLabelMap.CommonScipioERP!}</@heading>
	  </div>
	
	  <@section id="login-box-content">	    
	    
	
	    <div class="${styles.login_body!}">
	    	<p>
		    	We have detected that there is no WebSite record matching the webSiteId defined as a context-param in web.xml for <strong>${webSiteIdNotFound!""}</strong>. 
		    	This is due the system not being properly configured. In order to solve this, follow our setup wizard to fullfil all the missing gaps. <br/>
		    	<a href="/setup/control/main" class="${styles.link_run_session!} ${styles.action_login!}">${uiLabelMap.CommonGoToSetup}</a>
	    	</p>
	    	
	    	<#-- <@ofbizInterWebappUrl uri="main" webSiteId="setup" /> -->
	    	
	    	<p>Alternatively you can load our demo data in order to have the system properly setup without requiring any further action.</p>
	    	
	    	<p>Check our guide for further information: </p>
	    	
	    	[TODO]
	    </div>
	
	  </@section>
	</@cell>
</@row>