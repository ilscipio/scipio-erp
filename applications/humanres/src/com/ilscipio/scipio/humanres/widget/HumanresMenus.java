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

import com.ilscipio.scipio.widget.def.menu.*;
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
public class HumanresMenus {

    @Menu(
        name = "HumanResAppBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        title = "${uiLabelMap.HumanResManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Employees", title = "${uiLabelMap.HumanResEmployeesApplicants}", link = @MenuLink(target = "findEmployees")),
            @MenuItem(name = "EmplPosition", title = "${uiLabelMap.HumanResPositions}", link = @MenuLink(target = "FindEmplPositions")),
            @MenuItem(name = "Recruitment", title = "${uiLabelMap.HumanResRecruitment}", link = @MenuLink(target = "FindJobRequisitions")),
            @MenuItem(name = "EmploymentApp", title = "${uiLabelMap.HumanResApplications}", link = @MenuLink(target = "FindEmploymentApps")),
            @MenuItem(name = "Leave", title = "${uiLabelMap.HumanResEmplLeave}", link = @MenuLink(target = "FindEmplLeaves")),
            @MenuItem(name = "GlobalHRSettings", title = "${uiLabelMap.CommonSettings}", link = @MenuLink(target = "globalHRSettings"))
        }
    )
    public interface HumanResAppBar {}

    @Menu(
        name = "HumanResAppSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        title = "${uiLabelMap.HumanResManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "HumanResAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "Employees", subMenus = {@SubMenu(name = "EmployeeProfile", include = "component://humanres/widget/HumanresMenus.xml#EmployeeProfileSideBar")}),
            @MenuItem(name = "EmplPosition", subMenus = {@SubMenu(name = "EmplPosition", include = "component://humanres/widget/HumanresMenus.xml#EmplPositionSideBar")}),
            @MenuItem(name = "Recruitment", subMenus = {@SubMenu(name = "RecruitmentType", include = "component://humanres/widget/HumanresMenus.xml#RecruitmentTypeSideBar")}),
            @MenuItem(name = "Leave", subMenus = {@SubMenu(name = "EmplLeave", include = "component://humanres/widget/HumanresMenus.xml#EmplLeaveSideBar")}),
            @MenuItem(name = "GlobalHRSettings", subMenus = {@SubMenu(name = "GlobalHRSetting", include = "component://humanres/widget/HumanresMenus.xml#GlobalHRSettingSideBar")})
        }
    )
    public interface HumanResAppSideBar {}

    @Menu(
        name = "EmploymentBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditPartyBenefit",
        items = {
            @MenuItem(name = "EditEmployment", title = "${uiLabelMap.HumanResEmployment}", link = @MenuLink(target = "EditEmployment", parameters = {@MenuParameter(paramName = "roleTypeIdFrom", fromField = "roleTypeIdFrom"), @MenuParameter(paramName = "roleTypeIdTo", fromField = "roleTypeIdTo"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyIdFrom"), @MenuParameter(paramName = "partyIdTo", fromField = "partyIdTo"), @MenuParameter(paramName = "fromDate", fromField = "fromDate")})),
            @MenuItem(name = "EditPartyBenefit", title = "${uiLabelMap.HumanResEditPartyBenefit}", link = @MenuLink(target = "EditPartyBenefits", parameters = {@MenuParameter(paramName = "roleTypeIdFrom", fromField = "roleTypeIdFrom"), @MenuParameter(paramName = "roleTypeIdTo", fromField = "roleTypeIdTo"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyIdFrom"), @MenuParameter(paramName = "partyIdTo", fromField = "partyIdTo"), @MenuParameter(paramName = "fromDate", fromField = "fromDate")})),
            @MenuItem(name = "EditPayrollPreference", title = "${uiLabelMap.HumanResEditPayrollPreference}", link = @MenuLink(target = "EditPayrollPreferences", parameters = {@MenuParameter(paramName = "roleTypeIdFrom", fromField = "roleTypeIdFrom"), @MenuParameter(paramName = "roleTypeIdTo", fromField = "roleTypeIdTo"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyIdFrom"), @MenuParameter(paramName = "partyIdTo", fromField = "partyIdTo"), @MenuParameter(paramName = "fromDate", fromField = "fromDate")})),
            @MenuItem(name = "EditPayHistory", title = "${uiLabelMap.HumanResEditPayHistory}", link = @MenuLink(target = "ListPayHistories", parameters = {@MenuParameter(paramName = "roleTypeIdFrom", fromField = "roleTypeIdFrom"), @MenuParameter(paramName = "roleTypeIdTo", fromField = "roleTypeIdTo"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyIdFrom"), @MenuParameter(paramName = "partyIdTo", fromField = "partyIdTo"), @MenuParameter(paramName = "fromDate", fromField = "fromDate")})),
            @MenuItem(name = "EditUnemploymentClaims", title = "${uiLabelMap.HumanResEditUnemploymentClaim}", link = @MenuLink(target = "EditUnemploymentClaims", parameters = {@MenuParameter(paramName = "roleTypeIdFrom", fromField = "roleTypeIdFrom"), @MenuParameter(paramName = "roleTypeIdTo", fromField = "roleTypeIdTo"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyIdFrom"), @MenuParameter(paramName = "partyIdTo", fromField = "partyIdTo"), @MenuParameter(paramName = "fromDate", fromField = "fromDate")})),
            @MenuItem(name = "EditAgreementEmploymentAppls", title = "${uiLabelMap.HumanResAgreementEmploymentAppl}", link = @MenuLink(target = "EditAgreementEmploymentAppls", parameters = {@MenuParameter(paramName = "agreementId", fromField = "agreementId"), @MenuParameter(paramName = "agreementItemSeqId", fromField = "agreementItemSeqId"), @MenuParameter(paramName = "roleTypeIdFrom", fromField = "roleTypeIdFrom"), @MenuParameter(paramName = "roleTypeIdTo", fromField = "roleTypeIdTo"), @MenuParameter(paramName = "partyIdFrom", fromField = "partyIdFrom"), @MenuParameter(paramName = "partyIdTo", fromField = "partyIdTo"), @MenuParameter(paramName = "fromDate", fromField = "fromDate")}))
        }
    )
    public interface EmploymentBar {}

    @Menu(
        name = "EmploymentSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "EmploymentBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EditPartyBenefit"
    )
    public interface EmploymentSideBar {}

    @Menu(
        name = "EmplPositionBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EmplPositionView",
        items = {
            @MenuItem(name = "EmplPositionView", title = "${uiLabelMap.CommonSummary}", link = @MenuLink(target = "emplPositionView", parameters = {@MenuParameter(paramName = "emplPositionId", fromField = "emplPositionId")})),
            @MenuItem(name = "EditEmplPosition", title = "${uiLabelMap.HumanResEmployeePosition}", link = @MenuLink(target = "EditEmplPosition", parameters = {@MenuParameter(paramName = "emplPositionId", fromField = "emplPositionId")})),
            @MenuItem(name = "EditEmplPositionFulfillments", title = "${uiLabelMap.HumanResPositionFulfillments}", link = @MenuLink(target = "EditEmplPositionFulfillments", parameters = {@MenuParameter(paramName = "emplPositionId", fromField = "emplPositionId")})),
            @MenuItem(name = "EditEmplPositionResponsibilities", title = "${uiLabelMap.HumanResEmplPositionResponsibilities}", link = @MenuLink(target = "EditEmplPositionResponsibilities", parameters = {@MenuParameter(paramName = "emplPositionId", fromField = "emplPositionId")})),
            @MenuItem(name = "EditEmplPositionReportingStructs", title = "${uiLabelMap.HumanResEmplPositionReportingStruct}", link = @MenuLink(target = "EditEmplPositionReportingStructs", parameters = {@MenuParameter(paramName = "emplPositionId", fromField = "emplPositionId")}))
        }
    )
    public interface EmplPositionBar {}

    @Menu(
        name = "EmplPositionSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "EmplPositionBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EmplPositionView"
    )
    public interface EmplPositionSideBar {}

    @Menu(
        name = "PerfReviewBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Find", title = "${uiLabelMap.CommonFind}", link = @MenuLink(target = "FindPerfReviews")),
            @MenuItem(name = "EditPerfReview", title = "${uiLabelMap.HumanResPerfReview}", link = @MenuLink(target = "EditPerfReview", parameters = {@MenuParameter(paramName = "employeePartyId", fromField = "employeePartyId"), @MenuParameter(paramName = "employeeRoleTypeId", fromField = "employeeRoleTypeId"), @MenuParameter(paramName = "perfReviewId", fromField = "perfReviewId")})),
            @MenuItem(name = "EditPerfReviewItems", title = "${uiLabelMap.HumanResEditPerfReviewItems}", link = @MenuLink(target = "EditPerfReviewItems", parameters = {@MenuParameter(paramName = "employeePartyId", fromField = "employeePartyId"), @MenuParameter(paramName = "employeeRoleTypeId", fromField = "employeeRoleTypeId"), @MenuParameter(paramName = "perfReviewId", fromField = "perfReviewId")}))
        }
    )
    public interface PerfReviewBar {}

    @Menu(
        name = "SalaryBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditSalaryStep",
        selectedMenuItemContextFieldName = "activeSubMenu2Item",
        items = {
            @MenuItem(name = "EditPayGrade", title = "${uiLabelMap.HumanResPayGrade}", link = @MenuLink(target = "EditPayGrade", parameters = {@MenuParameter(paramName = "payGradeId", fromField = "payGradeId")})),
            @MenuItem(name = "EditSalaryStep", title = "${uiLabelMap.HumanResEditSalaryStep}", link = @MenuLink(target = "EditSalarySteps", parameters = {@MenuParameter(paramName = "payGradeId", fromField = "payGradeId")}))
        }
    )
    public interface SalaryBar {}

    @Menu(
        name = "SkillType",
        location = "component://humanres/widget/HumanresMenus.xml",
        id = "app-navigation",
        defaultSelectedStyle = "${styles.menu_default_itemactive}",
        selectedMenuItemContextFieldName = "activeSubMenuItem"
    )
    public interface SkillType {}

    @Menu(
        name = "GlobalHRSettingMenus",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "SkillType", title = "${uiLabelMap.HumanResSkillType}", link = @MenuLink(target = "EditSkillTypes")),
            @MenuItem(name = "ResponsibilityType", title = "${uiLabelMap.HumanResResponsibilityType}", link = @MenuLink(target = "EditResponsibilityTypes")),
            @MenuItem(name = "TerminationReason", title = "${uiLabelMap.HumanResTerminationReason}", link = @MenuLink(target = "EditTerminationReasons")),
            @MenuItem(name = "TerminationType", title = "${uiLabelMap.HumanResTerminationTypes}", link = @MenuLink(target = "EditTerminationTypes")),
            @MenuItem(name = "EmplPositionTypes", title = "${uiLabelMap.HumanResEmplPositionType}", link = @MenuLink(target = "FindEmplPositionTypes")),
            @MenuItem(name = "EmplLeaveType", title = "${uiLabelMap.HumanResEmplLeaveType}", link = @MenuLink(target = "EditEmplLeaveTypes")),
            @MenuItem(name = "PayGrade", title = "${uiLabelMap.HumanResPayGrade}", link = @MenuLink(target = "FindPayGrades")),
            @MenuItem(name = "JobInterviewType", title = "${uiLabelMap.HumanResJobInterviewType}", link = @MenuLink(target = "EditJobInterviewType")),
            @MenuItem(name = "EditTrainingTypes", title = "${uiLabelMap.HumanResTrainingClassType}", link = @MenuLink(target = "EditTrainingTypes")),
            @MenuItem(name = "publicHoliday", title = "${uiLabelMap.PageTitlePublicHoliday}", link = @MenuLink(target = "PublicHoliday"))
        }
    )
    public interface GlobalHRSettingMenus {}

    @Menu(
        name = "GlobalHRSettingSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "GlobalHRSettingMenus", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "SkillType"
    )
    public interface GlobalHRSettingSideBar {}

    @Menu(
        name = "EmployeeProfileTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditEmployeeSkills",
        items = {
            @MenuItem(name = "EmployeeProfile", title = "${uiLabelMap.PartyProfile}", link = @MenuLink(target = "EmployeeProfile", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "ListEmployment", title = "${uiLabelMap.HumanResEmployment}", link = @MenuLink(target = "ListEmployments", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "ListEmplPositions", title = "${uiLabelMap.HumanResEmployeePosition}", link = @MenuLink(target = "ListEmplPositions", parameters = {@MenuParameter(paramName = "partyId", fromField = "parameters.partyId")})),
            @MenuItem(name = "EditEmployeeSkills", title = "${uiLabelMap.HumanResSkills}", link = @MenuLink(target = "EditEmployeeSkills", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditEmployeeQuals", title = "${uiLabelMap.HumanResPartyQualification}", link = @MenuLink(target = "EditEmployeeQuals", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditEmployeeTrainings", title = "${uiLabelMap.HumanResTraining}", link = @MenuLink(target = "EditEmployeeTrainings", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditEmployeeContent", title = "${uiLabelMap.HumanResPartyContentAndResumes}", link = @MenuLink(target = "EditPartyContents", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "EditEmployeeLeaves", title = "${uiLabelMap.HumanResEmplLeave}", link = @MenuLink(target = "EditEmployeeLeaves", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "PayrollHistory", title = "${uiLabelMap.HumanResPayRollHistory}", link = @MenuLink(target = "PayrollHistory", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")}))
        }
    )
    public interface EmployeeProfileTabBar {}

    @Menu(
        name = "EmployeeProfileSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "EmployeeProfileTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EditEmployeeSkills"
    )
    public interface EmployeeProfileSideBar {}

    @Menu(
        name = "EmplPositionTypeTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditEmplPositionType",
        selectedMenuItemContextFieldName = "activeSubMenuItem2",
        items = {
            @MenuItem(name = "EditEmplPositionType", title = "${uiLabelMap.HumanResEmplPositionType}", link = @MenuLink(target = "EditEmplPositionTypes", parameters = {@MenuParameter(paramName = "emplPositionTypeId", fromField = "emplPositionTypeId")})),
            @MenuItem(name = "EditEmplPositionTypeRate", title = "${uiLabelMap.HumanResEmplPositionTypeRate}", link = @MenuLink(target = "EditEmplPositionTypeRates", parameters = {@MenuParameter(paramName = "emplPositionTypeId", fromField = "emplPositionTypeId")}))
        }
    )
    public interface EmplPositionTypeTabBar {}

    @Menu(
        name = "RecruitmentTypeMenu",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "JobRequisition",
        items = {
            @MenuItem(name = "JobRequisition", title = "${uiLabelMap.HumanResJobRequisition}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_VIEW"})}), link = @MenuLink(target = "FindJobRequisitions")),
            @MenuItem(name = "InternalJobPosting", title = "${uiLabelMap.HumanResInternalJobPosting}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_VIEW"})}), link = @MenuLink(target = "FindInternalJobPosting"))
        }
    )
    public interface RecruitmentTypeMenu {}

    @Menu(
        name = "RecruitmentTypeSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RecruitmentTypeMenu", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "JobRequisition",
        items = {
            @MenuItem(name = "InternalJobPosting", subMenus = {@SubMenu(name = "InternalJobPosting", include = "component://humanres/widget/HumanresMenus.xml#InternalJobPostingSideBar")})
        }
    )
    public interface RecruitmentTypeSideBar {}

    @Menu(
        name = "InternalJobPostingTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "InternalJobPosting",
        items = {
            @MenuItem(name = "InternalJobPosting", title = "${uiLabelMap.HumanResInternalJobPosting} ${uiLabelMap.CommonApplications}", link = @MenuLink(target = "FindInternalJobPosting")),
            @MenuItem(name = "JobInterview", title = "${uiLabelMap.HumanResJobInterview}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_ADMIN"})}), link = @MenuLink(target = "FindJobInterview")),
            @MenuItem(name = "Approval", title = "${uiLabelMap.HumanResApproval}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_APPROVE"})}), link = @MenuLink(target = "FindApprovals")),
            @MenuItem(name = "Relocation", title = "${uiLabelMap.HumanResRelocation}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_ADMIN"})}), link = @MenuLink(target = "FindRelocation"))
        }
    )
    public interface InternalJobPostingTabBar {}

    @Menu(
        name = "InternalJobPostingSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "InternalJobPostingTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "FindTrainings"
    )
    public interface InternalJobPostingSideBar {}

    @Menu(
        name = "TrainingTypeMenu",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "FindTrainings",
        items = {
            @MenuItem(name = "TrainingCalendar", title = "${uiLabelMap.HumanResTraining} ${uiLabelMap.WorkEffortCalendar}", link = @MenuLink(target = "TrainingCalendar")),
            @MenuItem(name = "FindTrainingStatus", title = "${uiLabelMap.HumanResTrainingStatus}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"EMPLOYEE", "_VIEW"})}), link = @MenuLink(target = "FindTrainingStatus")),
            @MenuItem(name = "FindTrainingApprovals", title = "${uiLabelMap.HumanResTrainingApprovals}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_ADMIN"})}), link = @MenuLink(target = "FindTrainingApprovals"))
        }
    )
    public interface TrainingTypeMenu {}

    @Menu(
        name = "TrainingTypeSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "TrainingTypeMenu", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "FindTrainings"
    )
    public interface TrainingTypeSideBar {}

    @Menu(
        name = "EmplLeaveReasonTypeTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "Employee Leave Reason Type",
        items = {
            @MenuItem(name = "EmplLeaveType", title = "${uiLabelMap.HumanResEmployeeLeaveType}", link = @MenuLink(target = "EditEmplLeaveTypes")),
            @MenuItem(name = "EmplLeaveReasonType", title = "${uiLabelMap.HumanResEmployeeType}", link = @MenuLink(target = "EditEmplLeaveReasonTypes"))
        }
    )
    public interface EmplLeaveReasonTypeTabBar {}

    @Menu(
        name = "EmplLeaveTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EmployeeLeave",
        items = {
            @MenuItem(name = "EmployeeLeave", title = "${uiLabelMap.HumanResEmployeeLeave}", link = @MenuLink(target = "FindEmplLeaves")),
            @MenuItem(name = "Approval", title = "${uiLabelMap.HumanResLeaveApproval}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"HUMANRES", "_APPROVE"})}), link = @MenuLink(target = "FindLeaveApprovals"))
        }
    )
    public interface EmplLeaveTabBar {}

    @Menu(
        name = "EmplLeaveSideBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "EmplLeaveTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EmployeeLeave"
    )
    public interface EmplLeaveSideBar {}

    @Menu(
        name = "EmploymentAppGeneralSubTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditEmploymentApp", title = "${uiLabelMap.HumanResNewEmploymentApp}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "not-equals", "EditEmploymentApp"})}), link = @MenuLink(target = "EditEmploymentApp")),
            @MenuItem(name = "NewEmployee", title = "${uiLabelMap.HumanResNewEmployeeApplicant}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "NewEmployee"))
        }
    )
    public interface EmploymentAppGeneralSubTabBar {}

    @Menu(
        name = "EmploymentAppEditSubTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditEmploymentApp", title = "${uiLabelMap.CommonEdit}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "not-equals", "EditEmploymentApp"})}), link = @MenuLink(target = "EditEmploymentApp", parameters = {@MenuParameter(paramName = "applicationId", fromField = "applicationId")})),
            @MenuItem(name = "ViewEmploymentApp", title = "${uiLabelMap.CommonView}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "equals", "EditEmploymentApp"})}), link = @MenuLink(target = "ViewEmploymentApp", parameters = {@MenuParameter(paramName = "applicationId", fromField = "applicationId")}))
        }
    )
    public interface EmploymentAppEditSubTabBar {}

    @Menu(
        name = "ResumeListSubTabBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditResumes", title = "${uiLabelMap.CommonManage}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditPartyContents", parameters = {@MenuParameter(paramName = "partyId", fromField = "resumePartyId")}))
        }
    )
    public interface ResumeListSubTabBar {}

    @Menu(
        name = "EmplPositionViewSubTarBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditEmploymentApp", title = "${uiLabelMap.HumanResNewEmploymentApp}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditEmploymentApp", parameters = {@MenuParameter(paramName = "emplPositionId", fromField = "emplPositionId")}))
        }
    )
    public interface EmplPositionViewSubTarBar {}

    @Menu(
        name = "EmployeeProfileSubTarBar",
        location = "component://humanres/widget/HumanresMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditEmploymentApp", title = "${uiLabelMap.HumanResNewEmploymentApp}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditEmploymentApp", parameters = {@MenuParameter(paramName = "applyingPartyId", fromField = "partyId")}))
        }
    )
    public interface EmployeeProfileSubTarBar {}

}
