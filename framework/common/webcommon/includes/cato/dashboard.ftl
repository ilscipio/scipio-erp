<#list dashboardGrid as grid>
	<div class="row">
		<#assign columnSize = Static["java.lang.Math"].round(dashboardColumns / columns) * Static["java.lang.Math"].round(12 / dashboardColumns) />
		${Static["org.ofbiz.base.util.Debug"].log("columnSize =======> " + columnSize)}
		<#list grid as s>			
			<#assign totalColumnSize = totalColumnSize!0 + columnSize />			
			<div class="columns large-${columnSize}">
				${Static["org.ofbiz.base.util.Debug"].log("grid index =======> " + grid_index + "   section =====> " + s)}
				${sections.render(s)}
			</div>
		</#list>
	</div>
</#list>