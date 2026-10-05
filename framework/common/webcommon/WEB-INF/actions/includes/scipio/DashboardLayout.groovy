/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */

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