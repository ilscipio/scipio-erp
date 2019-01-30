<#if requestAttributes.uiLabelMap??><#assign uiLabelMap = requestAttributes.uiLabelMap></#if>

<#assign logo><img src="<@contentUrl escapeAs='html'>/images/scipio/scipio-logo-small.png</@contentUrl>"/></#assign>

<@row>
	<#-- NOTE: login_wrap contains grid size -->
	<@cell class="${styles.grid_large!}12 ${styles.grid_large!} ${styles.login_wrap!}" last=true id="login" style="margin-top: 50px">
	  <div id="system-box-title">
	    <@heading level=1>${logo} ${uiLabelMap.CommonScipioERP!}</@heading>
	  </div>
	
	  <@section id="login-box-content">	    
	    
	
	    <div class="${styles.login_body!}">
	    	<p>
		    	We have detected that there is no <i>WebSite</i> record matching the webSiteId defined as a context-param in this webapp web.xml descriptor for <strong>${webSiteIdNotFound!""}</strong>. 
		    	This is due the system not being properly configured. In order to solve this, follow our <a href="${scipioSetupUrl!}" class="${styles.link_run_local_inline!} ${styles.text_color_alert!}">setup wizard</a> to fullfil all the missing gaps.		    	 
	    	</p>	    	
	    	<#-- <@serverUrl uri="main" webSiteId="setup" /> -->	    	
	    	<p class="${styles.text_left}">
	    		Alternatively you can load our demo data in order to have the system properly setup without requiring any further action. You must issue this command to do so:
	    		<pre>
	    			cd &lt;your-working-copy&gt;
	    			./ant load-demo
	    		</pre>  	
	    	</p>	    	
	    	<p>Check our public <a href="/setup/control/main" class="${styles.link_run_local_inline!} ${styles.text_color_alert!}">guide</a> for further information.</p>
	    </div>
	
	  </@section>
	</@cell>
</@row>