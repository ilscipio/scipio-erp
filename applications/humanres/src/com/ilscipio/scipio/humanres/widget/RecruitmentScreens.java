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
package com.ilscipio.scipio.humanres.widget;

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
public class RecruitmentScreens {

    @Screen(name = "FindJobRequisitions", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindJobRequisition")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "JobRequisition")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @DecoratorScreen(
        name = "CommonRecruitmentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(sections = {
                        @SectionLeaf(condition = @Condition(type = HasPermission.class, params = {"HUMANRES", "_ADMIN"
                    }), widgets = @WidgetsLeaf(containers = {
                        @ContainerLeaf(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewJobRequisition}", style = "${styles.link_nav} ${styles.action_add}", target = "EditJobRequisition"
                        )})}))})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindJobRequisitions", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListJobRequisitions", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )}))})})
        }
    )
    public interface FindJobRequisitions {}

    @Screen(name = "EditJobRequisition", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditJobRequisition")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "JobRequisition")
    @Action(type = ActionType.SET, field = "jobRequisitionId", fromField = "parameters.jobRequisitionId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "JobRequisition", valueField = "jobRequisition")
    @DecoratorScreen(
        name = "CommonRecruitmentDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"jobRequisition.jobRequisitionId"
                })}), widgets = @InlineWidgets(screenlets = {
                    @Screenlet(title = "${uiLabelMap.CommonAdd} ${uiLabelMap.HumanResJobRequisition}", includeForms = {
                        @IncludeForm(name = "EditJobRequisition", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                    )})}), failWidgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "EditJobRequisition", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )})}))})
        }
    )
    public interface EditJobRequisition {}

    @Screen(name = "FindInternalJobPosting", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindInternalJobPosting")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "InternalJobPosting")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.userLogin.partyId")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewInternalJobPosting}", style = "${styles.link_nav} ${styles.action_add}", target = "EditInternalJobPosting"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindInternalJobPosting", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListInternalJobPosting", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )}))})})
        }
    )
    public interface FindInternalJobPosting {}

    @Screen(name = "EditInternalJobPosting", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "InternalJobPosting")
    @Action(type = ActionType.SET, field = "applicationId", fromField = "parameters.applicationId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmploymentApp", valueField = "employmentApp")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.userLogin.partyId")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.employmentApp ? 'PageTitleEditInternalJobPosting' : 'HumanResNewInternalJobPosting'}")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "EditInternalJobPosting", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditInternalJobPosting", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                )})})
        }
    )
    public interface EditInternalJobPosting {}

    @Screen(name = "FindJobInterview", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindJobInterviewDetails")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "JobInterview")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "menu-bar", widgets = @WidgetsForContainer4(containers = {
                        @Container4(style = "button-bar", widgets = {
                            @Widget(type = WidgetType.LINK, text = "${uiLabelMap.HumanResNewJobInterview}", style = "${styles.link_nav} ${styles.action_add}", target = "EditJobInterview"
                        )})})),
                        @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "FindJobInterview", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )})),
                        @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListInterview", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                        )}))})})
        }
    )
    public interface FindJobInterview {}

    @Screen(name = "EditJobInterview", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditJobInterview")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "JobInterview")
    @Action(type = ActionType.SET, field = "jobInterviewId", fromField = "parameters.jobInterviewId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "JobInterview", valueField = "JobInterview")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "EditJobInterview", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                )})})
        }
    )
    public interface EditJobInterview {}

    @Screen(name = "FindApprovals", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindApprovals")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Approval")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.userLogin.partyId")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindApprovals", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListApprovals", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                    )}))})})
        }
    )
    public interface FindApprovals {}

    @Screen(name = "EditApprovalStatus", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditApprovalStatus")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Approval")
    @Action(type = ActionType.SET, field = "candidateRequestId", fromField = "parameters.candidateRequestId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "EmploymentApp", valueField = "employmentApp")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(name = "EditApprovalStatus", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditApprovalStatus", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                )})})
        }
    )
    public interface EditApprovalStatus {}

    @Screen(name = "FindRelocation", location = "component://humanres/widget/RecruitmentScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindRelocationDetails")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Relocation")
    @DecoratorScreen(
        name = "CommonInternalJobPostingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindRelocation", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRelocation", location = "component://humanres/widget/forms/RecruitmentForms.xml"
                    )}))})})
        }
    )
    public interface FindRelocation {}

}
