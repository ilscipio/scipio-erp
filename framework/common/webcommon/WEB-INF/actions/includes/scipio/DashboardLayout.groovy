
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilProperties


final DASHBOARD_MAX_COLUMNS = UtilProperties.getPropertyAsInteger("framework/widget/config/widget.properties", "widget.scipio.dashboard.layout.max.column", 6);
final DASHBOARD_MIN_COLUMNS = UtilProperties.getPropertyAsInteger("framework/widget/config/widget.properties", "widget.scipio.dashboard.layout.min.column", 2);

columns = (parameters.columns) ? parameters.columns : DASHBOARD_MIN_COLUMNS;
if (columns > DASHBOARD_MAX_COLUMNS)
    columns =  DASHBOARD_MAX_COLUMNS;
    
rows = Math.round(sections.size() / 2);

sections = new LinkedList(context.sections.keySet());

dashboardGrid = new LinkedList<LinkedList<String>>();
columnsList = new LinkedList<String>();

sectionIndex = 0;
for (i = 0; i < rows; i++) {
    for (x = 0; x < columns; x++) {
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

context.dashboardGrid = dashboardGrid;