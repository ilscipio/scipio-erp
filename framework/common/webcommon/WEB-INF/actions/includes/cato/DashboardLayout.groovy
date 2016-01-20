import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties


final DASHBOARD_MAX_COLUMNS = UtilProperties.getPropertyAsInteger("framework/widget/config/widget.properties", "widget.cato.dashboard.layout.max.column", 6);
final DASHBOARD_MIN_COLUMNS = UtilProperties.getPropertyAsInteger("framework/widget/config/widget.properties", "widget.cato.dashboard.layout.min.column", 2);

columns = (parameters.columns) ? parameters.columns : DASHBOARD_MIN_COLUMNS;
if (columns > DASHBOARD_MAX_COLUMNS)
    columns =  DASHBOARD_MAX_COLUMNS;
    
rows = Math.round(sections.size() / 2);

//Debug.log("columns ==========> " + columns);
//Debug.log("rows ==========> " + rows);

sections = new LinkedList(context.sections.keySet());
//for (section in sections) {
//    Debug.log("section keySet name ======> " + section);
//}
//
//for (section in context.sections) {
//    Debug.log("section context name ======> " + section);
//}

dashboardGrid = new LinkedList<LinkedList<String>>();
columnsList = new LinkedList<String>();

sectionIndex = 0;
for (i = 0; i < rows; i++) {
    for (x = 0; x < columns; x++) {
//        Debug.log("sectionIndex ======> " + sectionIndex);
        if (sectionIndex < sections.size()) {
            columnsList.add(sections.get(sectionIndex));
        } else {
            columnsList.add(null);
        }
        sectionIndex++;
    }
    dashboardGrid.add(columnsList);
    columnsList = new LinkedList<String>();
}

context.columns = columns;
context.dashboardColumns = DASHBOARD_MAX_COLUMNS;

//Debug.log("dashboardGrid size ==========> " + dashboardGrid.size());

context.dashboardGrid = dashboardGrid;
