import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilValidate

final DASHBOARD_MAX_COLUMNS = UtilProperties.getPropertyAsInteger("framework/widget/config/widgetContextScripts.properties", "widget.cato.dashboard.layout.max.column", 6);
final DASHBOARD_MIN_COLUMNS = UtilProperties.getPropertyAsInteger("framework/widget/config/widgetContextScripts.properties", "widget.cato.dashboard.layout.min.column", 2);

columns = (parameters.columns) ? parameters.columns : DASHBOARD_MIN_COLUMNS;
if (columns > DASHBOARD_MAX_COLUMNS)
	columns =  DASHBOARD_MAX_COLUMNS;
	
rows = Math.round(sections.size() / 2);

Debug.log("columns ==========> " + columns);
Debug.log("rows ==========> " + rows);

sections = new ArrayList(context.sections.keySet());

rowsList = FastList.newInstance();
columnsList = FastList.newInstance();

for (i = 0; i <= rows; i++) {	
	for (x = 0; x <= columns; x++) {		
		if (UtilValidate.isNotEmpty(sections.getAt(x))) {
			columnsList.add(sections.getAt(x));
			sections.remove(x);
		}		
	}
	rowsList.add(columnsList);
	columnsList = FastList.newInstance();
}

context.dashboardGrid = rowsList;
