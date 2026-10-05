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
public class ServiceScreens {

    @Screen(name = "ServiceList", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleServiceList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "serviceList")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/AvailableServices.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/service/availableservices.ftl"
            )})
        }
    )
    public interface ServiceList {}

    @Screen(name = "FindJob", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleJobList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findJob")
    @Action(type = ActionType.SET, field = "jobCtx", fromField = "parameters")
    @Action(type = ActionType.SET, field = "dummy", value = "${groovy: if ('SERVICE_PENDING'.equals(jobCtx.statusId)) jobCtx.jobId = ''}")
    @Action(type = ActionType.SET, field = "clockField", value = "FindJobs_clock_title")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "${styles.grid_row}", widgets = {
                            @Widget(type = WidgetType.INCLUDE_SCREEN, name = "FindJob-part1", location = "component://webtools/widget/ServiceScreens.xml", shareScope = true
                        ),
                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "FindJob-part2", location = "component://webtools/widget/ServiceScreens.xml", shareScope = true
                    )})})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListJobs", location = "component://webtools/widget/ServiceForms.xml"
                    )}))})})
        }
    )
    public interface FindJob {}

    @Screen(name = "FindJob-part1", location = "component://webtools/widget/ServiceScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(style = "${styles.grid_medium}9 ${styles.grid_cell}", includeForms = {@IncludeForm(name = "FindJobs", location = "component://webtools/widget/ServiceForms.xml")})}))
    public interface FindJob_part1 {}

    @Screen(name = "FindJob-part2", location = "component://webtools/widget/ServiceScreens.xml")
    @Section(widgets = @Widgets(containers = {@Container(style = "${styles.grid_medium}3 ${styles.grid_cell} ${styles.text_right}", includeScreens = {@IncludeScreen(name = "serverTimeClock", location = "component://webtools/widget/ServiceScreens.xml")})}))
    public interface FindJob_part2 {}

    @Screen(name = "JobDetails", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleJobDetails")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "findJob")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/JobDetails.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "JobDetails", location = "component://webtools/widget/ServiceForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.WebtoolsRunTimeDataInfo}", includeForms = {
                    @IncludeForm(name = "JobRuntimeDataInfo", location = "component://webtools/widget/ServiceForms.xml"
                )})})
        }
    )
    public interface JobDetails {}

    @Screen(name = "serverTimeClock", location = "component://webtools/widget/ServiceScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://common/webcommon/includes/ServerHour24HourFormatJs.ftl"), @Widget(type = WidgetType.HTML_TEMPLATE, content = "<label for=\"${clockField}\" class=\"serverTimeClock-label\">${uiLabelMap.CommonServerHour}:</label> <#t/>\n                    <span id=\"${clockField}\" style=\"white-space: nowrap;\" class=\"serverTimeClock-value\">...</span><#t/>")}))
    public interface serverTimeClock {}

    @Screen(name = "ThreadList", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleThreadList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "threadList")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/Threads.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.WebtoolsServiceEngineThreads}", includeForms = {
                    @IncludeForm(name = "PoolState", location = "component://webtools/widget/ServiceForms.xml"
                ),
                @IncludeForm(name = "ListJavaThread", location = "component://webtools/widget/ServiceForms.xml"
            )}),
            @Screenlet(title = "${uiLabelMap.WebtoolsGeneralJavaThreads}", htmlTemplates = {
                @HtmlTemplate(location = "component://webtools/webapp/webtools/service/threads.ftl"
            )})})
        }
    )
    public interface ThreadList {}

    @Screen(name = "ScheduleJob", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleScheduleJob")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "scheduleJob")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.WebtoolsStep1ServiceAndRecurrenceInfo}", actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "serviceNameInputIdExpr", value = "#scheduleJob input[name=SERVICE_NAME]"
                )}), includeForms = {
                    @IncludeForm(name = "scheduleJob", location = "component://webtools/widget/ServiceForms.xml"
                )}, includeScreens = {
                    @IncludeScreen(name = "serviceNameAutocomplete", location = "component://webtools/widget/ServiceScreens.xml"
                )})})
        }
    )
    public interface ScheduleJob {}

    @Screen(name = "RunService", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleRunService")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "runService")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "serviceNameInputIdExpr", value = "#runService input[name=SERVICE_NAME]"
                )}), includeForms = {
                    @IncludeForm(name = "runService", location = "component://webtools/widget/ServiceForms.xml"
                )}, includeScreens = {
                    @IncludeScreen(name = "serviceNameAutocomplete", location = "component://webtools/widget/ServiceScreens.xml"
                )})})
        }
    )
    public interface RunService {}

    @Screen(name = "serviceNameAutocomplete", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/serviceNameAutocomplete_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, content = "<@script>\n                          $(function() {\n                            var serviceNames = <@objectAsScript object=(serviceNames![]) lang=\"js\"/>;\n\n                            var matchBestName = function(sourceList, term, mode, maxCount) {\n                                var i = 0, t = term, tlc = t.toLowerCase(), sw = [], swci = [], cnt = [], cntci = [];\n                                var c = 0;\n                                for (i = 0; (i < serviceNames.length && (maxCount <= 0 || c < maxCount)); i++) {\n                                    var s = serviceNames[i];\n                                    if (s.startsWith(t)) {\n                                        sw.push(s);\n                                        c++;\n                                    } else {\n                                        var slc = s.toLowerCase();\n                                        if (slc.startsWith(tlc)) {\n                                            swci.push(s);\n                                            c++;\n                                        } else if (s.includes(t)) {\n                                            cnt.push(s);\n                                            c++;\n                                        } else if (slc.includes(tlc)) {\n                                            cntci.push(s);\n                                            c++;\n                                        }\n                                    }\n                                }\n                                return sw.concat(swci).concat(cnt).concat(cntci);\n                            };\n\n                            $(\"${raw(serviceNameInputIdExpr)}\").autocomplete({\n                              <#if serviceNamesMatchMode == \"best\">\n                                source: function(request, response) {\n                                    response(matchBestName(serviceNames, request.term, \"best\", ${serviceNamesMaxMatch!-1}));\n                                }\n                              <#else><#--<#elseif serviceNamesMatchMode == \"contains\">-->\n                                source: serviceNames\n                              </#if>\n                            });\n                          });\n                        </@script>")}))
    public interface serviceNameAutocomplete {}

    @Screen(name = "setServiceParameters", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleScheduleJob")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "scheduleJob")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.WebtoolsStep2ServiceParameters}", htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/service/setServiceParameter.ftl"
                )})})
        }
    )
    public interface setServiceParameters {}

    @Screen(name = "setSyncServiceParameters", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleScheduleJob")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "runService")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/service/setServiceParameterSync.ftl"
                )})})
        }
    )
    public interface setSyncServiceParameters {}

    @Screen(name = "ServiceResult", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleScheduleJob")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "runService")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ServiceResult.groovy")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/service/serviceResult.ftl"
            )})
        }
    )
    public interface ServiceResult {}

    @Screen(name = "FindJobManagerLock", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleJobManagerLockList")
    @Action(type = ActionType.SET, field = "tabButtonItem", value = "FindJobManagerLock")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap[titleProperty]}")}, position = 0)
                }, screenlets = {
                    @Screenlet(title = "${uiLabelMap.CommonSearchOptions}", name = "findScreenlet", collapsible = true, containers = {
                        @Container(id = "search-options", includeForms = {
                            @IncludeForm(name = "FindJobManagerLock", location = "component://webtools/widget/ServiceForms.xml"
                        )})}, position = 2),
                        @Screenlet(labels = {
                            @Label(text = "${uiLabelMap.CommonSearchResults}", style = "h3"
                        )}, containers = {
                            @Container(id = "search-results", includeForms = {
                                @IncludeForm(name = "ListJobManagerLock", location = "component://webtools/widget/ServiceForms.xml"
                            )})}, position = 3)}, sections = {
                                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                                    @Condition(type = HasPermission.class, params = {"SERVICE_JM_LOCK"
                                })}), widgets = @InlineWidgets(value = {
                                    @Widget(type = WidgetType.CONTAINER, style = "button-bar")}), position = 1
                                )})
        }
    )
    public interface FindJobManagerLock {}

    @Screen(name = "AddJobManagerLock", location = "component://webtools/widget/ServiceScreens.xml")
    @DecoratorScreen(
        name = "PopUpDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "AddJobManagerLock", location = "component://webtools/widget/ServiceForms.xml"
            )})
        }
    )
    public interface AddJobManagerLock {}

    @Screen(name = "JobManagerLockEnable", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobManagerLock", list = "currentJobManagerLocks", filterByDate = true, orderBy = {"fromDate"})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "JobManagerLock", list = "nextJobManagerLocks", conditions = {@ConditionExpr(fieldName = "fromDate", operator = "greater", fromField = "nowTimestamp")}, orderBy = {"fromDate"})
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifNotEmpty = {"currentJobManagerLocks", "nextJobManagerLocks"})}), widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.WebtoolsJobManagerLockEnable}", sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"currentJobManagerLocks"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "jobManagerLocks", fromField = "currentJobManagerLocks")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsCurrentJobManagerLock}", style = "h3"), @Widget(type = WidgetType.INCLUDE_GRID, name = "JobManagerLockEnable", location = "component://webtools/widget/ServiceForms.xml")})), @SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"nextJobManagerLocks"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "jobManagerLocks", fromField = "nextJobManagerLocks")}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsNextJobManagerLock}", style = "h3"), @Widget(type = WidgetType.INCLUDE_GRID, name = "JobManagerLockEnable", location = "component://webtools/widget/ServiceForms.xml")}))})}))
    public interface JobManagerLockEnable {}

    @Screen(name = "JobStats", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleJobStats")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "jobStats")
    @Action(type = ActionType.SET, field = "jobCtx", fromField = "parameters")
    @Action(type = ActionType.SET, field = "clockField", value = "FindJobs_clock_title")
    @DecoratorScreen(
        name = "CommonServiceDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_medium}9 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "currentJobStats", location = "component://webtools/widget/ServiceScreens.xml"
                    )}),
                    @Container2(style = "${styles.grid_medium}3 ${styles.grid_cell} ${styles.text_right}", includeScreens = {
                        @IncludeScreen(name = "serverTimeClock", location = "component://webtools/widget/ServiceScreens.xml"
                    )})}),
                    @Container(style = "${styles.grid_row}", containers = {
                        @Container2(style = "${styles.grid_medium}12 ${styles.grid_cell}", htmlTemplates = {
                            @HtmlTemplate(location = "", content = "<form name=\"\" action=\"<@pageUrl uri='clearGlobalJobStats'/>\" method=\"post\">\n                                            <input type=\"submit\" value=\"Clear Global Stats\"/>\n                                        </form>"
                        )})}),
                        @Container(style = "${styles.grid_row}", containers = {
                            @Container2(style = "${styles.grid_medium}12 ${styles.grid_cell}", sections = {
                                @SectionNested2(actions = @Actions(value = {
                                    @Action(type = ActionType.SET, field = "jobType", value = "generic"
                                ),
                                @Action(type = ActionType.SET, field = "title", value = "Generic Jobs (non-persisted)"
                            )}), widgets = @WidgetsForContainer2(value = {
                                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "globalJobStats"
                            )}))})}),
                            @Container(style = "${styles.grid_row}", containers = {
                                @Container2(style = "${styles.grid_medium}12 ${styles.grid_cell}", sections = {
                                    @SectionNested2(actions = @Actions(value = {
                                        @Action(type = ActionType.SET, field = "jobType", value = "persist"
                                    ),
                                    @Action(type = ActionType.SET, field = "title", value = "Persisted Jobs"
                                )}), widgets = @WidgetsForContainer2(value = {
                                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "globalJobStats"
                                )}))})}),
                                @Container(style = "${styles.grid_row}", containers = {
                                    @Container2(style = "${styles.grid_medium}12 ${styles.grid_cell}", sections = {
                                        @SectionNested2(actions = @Actions(value = {
                                            @Action(type = ActionType.SET, field = "jobType", value = "purge"
                                        ),
                                        @Action(type = ActionType.SET, field = "title", value = "Purge Jobs"
                                    )}), widgets = @WidgetsForContainer2(value = {
                                        @Widget(type = WidgetType.INCLUDE_SCREEN, name = "globalJobStats"
                                    )}))})})})
        }
    )
    public interface JobStats {}

    @Screen(name = "currentJobStats", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SET, field = "title", value = "Current Jobs")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/currentJobStats_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/service/currentJobStats.ftl")}))
    public interface currentJobStats {}

    @Screen(name = "globalJobStats", location = "component://webtools/widget/ServiceScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/globalJobStats_script1.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/service/globalJobStats.ftl")}))
    public interface globalJobStats {}

}
