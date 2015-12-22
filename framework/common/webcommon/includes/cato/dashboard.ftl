<#list dashboardGrid as grid>
	<div class="row">
		<#assign columnSize = Static["java.lang.Math"].round(dashboardColumns / columns) * Static["java.lang.Math"].round(12 / dashboardColumns) />
		${Static["org.ofbiz.base.util.Debug"].log("columnSize =======> " + columnSize)}
		<#list grid as s>			
			<#assign totalColumnSize = totalColumnSize!0 + columnSize />			
			<div class="columns large-${columnSize}">				
				<#if s?has_content>
					${Static["org.ofbiz.base.util.Debug"].log("grid index =======> " + grid_index + "   section =====> " + s)}
					${sections.render(s)}
				<#else>
					${Static["org.ofbiz.base.util.Debug"].log("grid index =======> " + grid_index + "   no section to be rendered")}
				</#if>
			</div>
		</#list>
	</div>
</#list>