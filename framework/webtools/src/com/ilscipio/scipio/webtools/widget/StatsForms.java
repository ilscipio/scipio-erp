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
package com.ilscipio.scipio.webtools.widget;

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class StatsForms {

    @Form(
        name = "ListStats",
        location = "component://webtools/widget/StatsForms.xml",
        type = FormType.LIST,
        paginateTarget = "StatsSinceStart",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "requestId", title = "${uiLabelMap.WebtoolsStatsRequestId}", display = @DisplayField),
            @FormField(name = "startTime", title = "${uiLabelMap.WebtoolsStatsStart}", display = @DisplayField),
            @FormField(name = "endTime", title = "${uiLabelMap.WebtoolsStatsStop}", display = @DisplayField),
            @FormField(name = "lengthMins", title = "${uiLabelMap.WebtoolsStatsMinutes}", display = @DisplayField),
            @FormField(name = "numberHits", title = "${uiLabelMap.WebtoolsStatsHits}", display = @DisplayField),
            @FormField(name = "minTime", title = "${uiLabelMap.WebtoolsStatsMin}", display = @DisplayField),
            @FormField(name = "avgTime", title = "${uiLabelMap.WebtoolsStatsAvg}", display = @DisplayField),
            @FormField(name = "maxTime", title = "${uiLabelMap.WebtoolsStatsMax}", display = @DisplayField),
            @FormField(name = "hitsPerMin", title = "${uiLabelMap.WebtoolsStatsHitsPerMin}", display = @DisplayField),
            @FormField(name = "viewBins", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_view}", widgetAreaStyle = "button-col", hyperlink = @HyperlinkField(target = "StatBinsHistory", description = "${uiLabelMap.WebtoolsStatsViewBins}", parameters = {@ParameterDef(paramName = "statsId", fromField = "requestId"), @ParameterDef(paramName = "type", fromField = "requestType")}))
        }
    )
    public interface ListStats {}

    @Form(
        name = "ListRequestStats",
        location = "component://webtools/widget/StatsForms.xml",
        listName = "requestList",
        extendsForm = "ListStats"
    )
    public interface ListRequestStats {}

    @Form(
        name = "ListEventStats",
        location = "component://webtools/widget/StatsForms.xml",
        listName = "eventList",
        extendsForm = "ListStats"
    )
    public interface ListEventStats {}

    @Form(
        name = "ListViewStats",
        location = "component://webtools/widget/StatsForms.xml",
        listName = "viewList",
        extendsForm = "ListStats"
    )
    public interface ListViewStats {}

    @Form(
        name = "ListRequestBins",
        location = "component://webtools/widget/StatsForms.xml",
        listName = "requestList",
        extendsForm = "ListStats",
        fields = {
            @FormField(name = "viewBins", hidden = @HiddenField)
        }
    )
    public interface ListRequestBins {}

    @Form(
        name = "ListMetrics",
        location = "component://webtools/widget/StatsForms.xml",
        type = FormType.LIST,
        listName = "metricsList",
        paginateTarget = "ViewMetrics",
        headerRowStyle = "header-row-2",
        defaultTableStyle = "${styles.table_data_list} light-grid",
        fields = {
            @FormField(name = "name", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "serviceRate", title = "${uiLabelMap.WebtoolsMetricsRate}", display = @DisplayField),
            @FormField(name = "threshold", title = "${uiLabelMap.WebtoolsMetricsThreshold}", display = @DisplayField),
            @FormField(name = "totalEvents", title = "${uiLabelMap.WebtoolsMetricsTotalEvents}", display = @DisplayField),
            @FormField(name = "resetMetric", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_reset}", widgetAreaStyle = "button-col", hyperlink = @HyperlinkField(target = "ResetMetric", description = "${uiLabelMap.CommonReset}", parameters = {@ParameterDef(paramName = "name")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "getAllMetrics")})
    )
    public interface ListMetrics {}

}
