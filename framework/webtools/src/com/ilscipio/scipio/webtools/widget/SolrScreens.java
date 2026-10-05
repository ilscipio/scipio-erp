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

import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SolrScreens {

    @Screen(name = "postRunSolrServiceActions", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/postRunSolrServiceActions_script1.groovy")
    public interface postRunSolrServiceActions {}

    @Screen(name = "SolrStatus", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SolrSolrStatus")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SolrStatus")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "postRunSolrServiceActions")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "isSolrAdmin", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"SOLRADM", "_ADMIN"})}))
    @DecoratorScreen(
        name = "CommonSolrDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "solrStatusDetails", location = "component://webtools/widget/SolrScreens.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = False.class, params = {"isSolrAdmin"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SolrNonAdminWarning}", style = "common-msg-warning"
                    )}), position = 0)})
        }
    )
    public interface SolrStatus {}

    @Screen(name = "SolrServices", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "SolrSolrServices")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SolrServices")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "postRunSolrServiceActions")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "isSolrAdmin", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"SOLRADM", "_ADMIN"})}))
    @DecoratorScreen(
        name = "CommonSolrDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "rebuildSolrIndexService", location = "component://webtools/widget/SolrScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "updateToSolrService", location = "component://webtools/widget/SolrScreens.xml"
            ),
            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "markSolrDataDirtyService", location = "component://webtools/widget/SolrScreens.xml"
            )}, sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = False.class, params = {"isSolrAdmin"})}), widgets = @InlineWidgets(value = {
                        @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.SolrNonAdminWarning}", style = "common-msg-warning"
                    )}), position = 0),
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                        @Condition(type = True.class, params = {"isSolrAdmin"})}), widgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "reloadSolrSecurityAuthorizationsService"
                        )}), position = 4)})
        }
    )
    public interface SolrServices {}

    @Screen(name = "SolrDoc", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "Solr README.txt")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "SolrDoc")
    @DecoratorScreen(
        name = "CommonSolrDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, content = "<@section>\n                            <p><i>applications/solr/README.txt</i></p>\n                            <pre><#include \"component://solr/README.txt\" parse=false></pre>\n                          </@section>"
            )})
        }
    )
    public interface SolrDoc {}

    @Screen(name = "solrStatusDetails", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://solr/script/com/ilscipio/scipio/solr/GetSolrStatusDetails.groovy")
    @Action(type = ActionType.SET, field = "rebuildIndexCoreWidgetLoc", value = "component://webtools/widget/SolrScreens.xml#rebuildSolrIndexServiceCore")
    @Action(type = ActionType.SET, field = "markSolrDataDirtyWidgetLoc", value = "component://webtools/widget/SolrScreens.xml#markSolrDataDirtyServiceCore")
    @Action(type = ActionType.SET, field = "runServiceTarget", value = "runSolrServiceForStatus")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/solr/solrStatusDetails.ftl")}))
    public interface solrStatusDetails {}

    @Screen(name = "rebuildSolrIndexService", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SET, field = "rebuildIndexCoreWidgetLoc", value = "component://webtools/widget/SolrScreens.xml#rebuildSolrIndexServiceCore")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#macro menuContent menuArgs={}>\n                        <@menu args=menuArgs>\n                            <@menuitem type=\"link\" href=makePageUrl(\"scheduleJob?SERVICE_NAME=rebuildSolrIndex\") text=\"${rawLabel('WebtoolsScheduleJob')}: rebuildSolrIndex\" class=\"+${styles.action_nav!} ${styles.action_begin!}\"/>\n                            <@menuitem type=\"link\" href=makePageUrl(\"scheduleJob?SERVICE_NAME=rebuildSolrIndexAuto\") text=\"${rawLabel('WebtoolsScheduleJob')}: rebuildSolrIndexAuto\" class=\"+${styles.action_nav!} ${styles.action_begin!}\"/>\n                            <@menuitem type=\"link\" href=makePageUrl(\"FindJob?noConditionFind=Y&serviceName_op=like&serviceName=rebuildSolrIndex\"+\"%\"?url) text=uiLabelMap.PageTitleFindJob class=\"+${styles.action_nav!} ${styles.action_find!}\"/>\n                        </@menu>\n                    </#macro>\n                    <@section title=\"rebuildSolrIndex\" class=\"scpadmservlist-service\" menuContent=menuContent>\n                      <@render resource=rebuildIndexCoreWidgetLoc/>\n                    </@section>")}))
    public interface rebuildSolrIndexService {}

    @Screen(name = "rebuildSolrIndexServiceCore", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/rebuildSolrIndexServiceCore_script1.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#include \"component://webtools/webapp/webtools/solr/solrcommon.ftl\">\n                    \n                    <p class=\"scpadmservlist-servdesc\">${uiLabelMap.SolrRebuildIndexServiceDesc}</p>\n                    \n                    <@solrServiceForm>\n                      <@solrServiceFields initParams={} defaultSyncMode=\"async\"/>\n                    </@solrServiceForm>\n                      \n                    <p class=\"scpadmservlist-servtip\">\n                      <small>\n                        ${uiLabelMap.CommonNote}: ${uiLabelMap.SolrRebuildIndexStartupInfo} ${uiLabelMap.SolrRebuildIndexStartupTip}:\n                          <code>./ant start-reindex-solr</code> || <code>./ant start-debug-reindex-solr</code> (Linux),\n                          <code>ant start-reindex-solr</code> || <code>ant start-debug-reindex-solr</code> (Windows)\n                      </small>\n                    </p>")}))
    public interface rebuildSolrIndexServiceCore {}

    @Screen(name = "updateToSolrService", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/updateToSolrService_script1.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#include \"component://webtools/webapp/webtools/solr/solrcommon.ftl\">\n                    \n                    <#-- nothing for this\n                    <#macro menuContent menuArgs={}>\n                        <@menu args=menuArgs>\n                        </@menu>\n                    </#macro>-->\n                    <@section title=\"updateToSolr\" class=\"scpadmservlist-service\"><#-- menuContent=menuContent -->\n                    \n                      <p class=\"scpadmservlist-servdesc\">${uiLabelMap.SolrUpdateToSolrServiceDesc}</p>\n                    \n                      <@solrServiceForm>\n                        <@solrServiceFields initParams={\"manual\":true} exclude={\"instance\":true}/>\n                      </@solrServiceForm>\n                    \n                    </@section>")}))
    public interface updateToSolrService {}

    @Screen(name = "markSolrDataDirtyService", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SET, field = "markSolrDataDirtyWidgetLoc", value = "component://webtools/widget/SolrScreens.xml#markSolrDataDirtyServiceCore")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<@section title=\"markSolrDataDirty\" class=\"scpadmservlist-service\">\n                      <@render resource=markSolrDataDirtyWidgetLoc/>\n                    </@section>")}))
    public interface markSolrDataDirtyService {}

    @Screen(name = "markSolrDataDirtyServiceCore", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/markSolrDataDirtyServiceCore_script1.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#include \"component://webtools/webapp/webtools/solr/solrcommon.ftl\">\n                    \n                    <p class=\"scpadmservlist-servdesc\">${uiLabelMap.SolrMarkSolrDataDirtyServiceDesc}</p>\n                    \n                    <@solrServiceForm>\n                      <@solrServiceFields initParams={}/>\n                    </@solrServiceForm>")}))
    public interface markSolrDataDirtyServiceCore {}

    @Screen(name = "reloadSolrSecurityAuthorizationsService", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SET, field = "reloadSolrSecurityAuthorizationsWidgetLoc", value = "component://webtools/widget/SolrScreens.xml#reloadSolrSecurityAuthorizationsServiceCore")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<@section title=\"reloadSolrSecurityAuthorizations\" class=\"scpadmservlist-service\">\n                      <@render resource=reloadSolrSecurityAuthorizationsWidgetLoc/>\n                    </@section>")}))
    public interface reloadSolrSecurityAuthorizationsService {}

    @Screen(name = "reloadSolrSecurityAuthorizationsServiceCore", location = "component://webtools/widget/SolrScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/reloadSolrSecurityAuthorizationsServiceCore_script1.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<#include \"component://webtools/webapp/webtools/solr/solrcommon.ftl\">\n                    \n                    <@alert type=\"warning\"><b>Warning (2018-05-17):</b> This service may not currently work reliably due\n                        to a reported Solr bug. Until it is fixed upstream, you may be forced to restart the server instead of running\n                        this service (after you modify SOLRADM_* permissions).</@alert>\n                    \n                    <p class=\"scpadmservlist-servdesc\">${uiLabelMap.SolrReloadSolrSecurityAuthorizationsServiceDesc}</p>\n                    \n                    <@solrServiceForm>\n                      <@solrServiceFields initParams={}/>\n                    </@solrServiceForm>")}))
    public interface reloadSolrSecurityAuthorizationsServiceCore {}

}
