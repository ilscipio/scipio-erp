
<#list dashboardGrid as grid>
	<div class="row">
		<#list grid as section>	
			<div class="columns large">
				${Static["org.ofbiz.base.util.Debug"].log("section =====> " + section)}

			</div>
		</#list>
	</div>
</#list>

${sections.render("topLeft")}
