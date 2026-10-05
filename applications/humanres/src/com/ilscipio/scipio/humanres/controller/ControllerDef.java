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
package com.ilscipio.scipio.humanres.controller;

import com.ilscipio.scipio.ce.webapp.control.def.View;
import com.ilscipio.scipio.ce.webapp.control.def.Request;
import com.ilscipio.scipio.ce.webapp.control.def.Response;
import com.ilscipio.scipio.ce.webapp.control.def.Responses;
import com.ilscipio.scipio.ce.webapp.control.def.Event;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.humanres.HumanResEvents;

/**
 * Auto-generated annotation-based controller definitions.
 *
 * <p>Generated from controller.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ControllerDef {
    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPartyName",
        type = "screen",
        page = "component://party/widget/partymgr/LookupScreens.xml#LookupPartyName",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPPARTYNAME = "LookupPartyName";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPayment",
        type = "screen",
        page = "component://accounting/widget/LookupScreens.xml#LookupPayment",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPPAYMENT = "LookupPayment";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupBudget",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupBudget",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPBUDGET = "LookupBudget";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupBudgetItem",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupBudgetItem",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPBUDGETITEM = "LookupBudgetItem";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupEmplPosition",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupEmplPosition",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPEMPLPOSITION = "LookupEmplPosition";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupTerminationReason",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupTerminationReason",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPTERMINATIONREASON = "LookupTerminationReason";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupSalaryStep",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupSalaryStep",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPSALARYSTEP = "LookupSalaryStep";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPayGrade",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupPayGrade",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPPAYGRADE = "LookupPayGrade";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPayRollPreference",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupPayRollPreference",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPPAYROLLPREFERENCE = "LookupPayRollPreference";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupUnemploymentClaim",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupUnemploymentClaim",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPUNEMPLOYMENTCLAIM = "LookupUnemploymentClaim";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupAgreementEmploymentAppl",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupAgreementEmploymentAppl",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPAGREEMENTEMPLOYMENTAPPL = "LookupAgreementEmploymentAppl";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPerfReview",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupPerfReview",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPPERFREVIEW = "LookupPerfReview";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupPartyResume",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupPartyResume",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPPARTYRESUME = "LookupPartyResume";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupEmploymentApp",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupEmploymentApp",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPEMPLOYMENTAPP = "LookupEmploymentApp";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupAgreement",
        type = "screen",
        page = "component://accounting/widget/LookupScreens.xml#LookupAgreement",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPAGREEMENT = "LookupAgreement";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "LookupJobRequisition",
        type = "screen",
        page = "component://humanres/widget/LookupScreens.xml#LookupJobRequisition",
        controller = "humanres"
    )
    public static final String VIEW_LOOKUPJOBREQUISITION = "LookupJobRequisition";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "main",
        type = "screen",
        page = "component://humanres/widget/CommonScreens.xml#main",
        controller = "humanres"
    )
    public static final String VIEW_MAIN = "main";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "FindEmployments",
        type = "screen",
        page = "component://humanres/widget/EmploymentScreens.xml#FindEmployments",
        controller = "humanres"
    )
    public static final String VIEW_FINDEMPLOYMENTS = "FindEmployments";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "EditEmployment",
        type = "screen",
        page = "component://humanres/widget/EmploymentScreens.xml#EditEmployment",
        controller = "humanres"
    )
    public static final String VIEW_EDITEMPLOYMENT = "EditEmployment";

    @com.ilscipio.scipio.ce.webapp.control.def.View(
        name = "ListEmployments",
        type = "screen",
        page = "component://humanres/widget/EmploymentScreens.xml#ListEmployments",
        controller = "humanres"
    )
    public static final String VIEW_LISTEMPLOYMENTS = "ListEmployments";


    // Auto-generated split (Part 2)
    public static class Part2 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyBenefits",
            type = "screen",
            page = "component://humanres/widget/EmploymentScreens.xml#EditPartyBenefits",
            controller = "humanres"
        )
        public static final String VIEW_EDITPARTYBENEFITS = "EditPartyBenefits";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPayrollPreferences",
            type = "screen",
            page = "component://humanres/widget/EmploymentScreens.xml#EditPayrollPreferences",
            controller = "humanres"
        )
        public static final String VIEW_EDITPAYROLLPREFERENCES = "EditPayrollPreferences";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListPayHistories",
            type = "screen",
            page = "component://humanres/widget/EmploymentScreens.xml#ListPayHistories",
            controller = "humanres"
        )
        public static final String VIEW_LISTPAYHISTORIES = "ListPayHistories";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSalarySteps",
            type = "screen",
            page = "component://humanres/widget/PayGradeScreens.xml#EditSalarySteps",
            controller = "humanres"
        )
        public static final String VIEW_EDITSALARYSTEPS = "EditSalarySteps";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditUnemploymentClaims",
            type = "screen",
            page = "component://humanres/widget/EmploymentScreens.xml#EditUnemploymentClaims",
            controller = "humanres"
        )
        public static final String VIEW_EDITUNEMPLOYMENTCLAIMS = "EditUnemploymentClaims";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditAgreementEmploymentAppls",
            type = "screen",
            page = "component://humanres/widget/EmploymentScreens.xml#EditAgreementEmploymentAppls",
            controller = "humanres"
        )
        public static final String VIEW_EDITAGREEMENTEMPLOYMENTAPPLS = "EditAgreementEmploymentAppls";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindEmployee",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#FindEmployee",
            controller = "humanres"
        )
        public static final String VIEW_FINDEMPLOYEE = "FindEmployee";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewEmployee",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#NewEmployee",
            controller = "humanres"
        )
        public static final String VIEW_NEWEMPLOYEE = "NewEmployee";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EmployeeProfile",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EmployeeProfile",
            controller = "humanres"
        )
        public static final String VIEW_EMPLOYEEPROFILE = "EmployeeProfile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeeSkills",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeeSkills",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEESKILLS = "EditEmployeeSkills";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeeTrainings",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeeTrainings",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEETRAININGS = "EditEmployeeTrainings";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeeQuals",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeeQuals",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEEQUALS = "EditEmployeeQuals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeeEmploymentApps",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeeEmploymentApps",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEEEMPLOYMENTAPPS = "EditEmployeeEmploymentApps";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeeResumes",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeeResumes",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEERESUMES = "EditEmployeeResumes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeePerformanceNotes",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeePerformanceNotes",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEEPERFORMANCENOTES = "EditEmployeePerformanceNotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmployeeLeaves",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditEmployeeLeaves",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYEELEAVES = "EditEmployeeLeaves";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindEmplPositions",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#FindEmplPositions",
            controller = "humanres"
        )
        public static final String VIEW_FINDEMPLPOSITIONS = "FindEmplPositions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplPosition",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EditEmplPosition",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLPOSITION = "EditEmplPosition";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListEmplPositions",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#ListEmplPositionsParty",
            controller = "humanres"
        )
        public static final String VIEW_LISTEMPLPOSITIONS = "ListEmplPositions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplPositionFulfillments",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EditEmplPositionFulfillments",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLPOSITIONFULFILLMENTS = "EditEmplPositionFulfillments";

    }

    // Auto-generated split (Part 3)
    public static class Part3 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplPositionResponsibilities",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EditEmplPositionResponsibilities",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLPOSITIONRESPONSIBILITIES = "EditEmplPositionResponsibilities";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplPositionReportingStructs",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EditEmplPositionReportingStructs",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLPOSITIONREPORTINGSTRUCTS = "EditEmplPositionReportingStructs";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ListValidResponsibilities",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#ListValidResponsibilities",
            controller = "humanres"
        )
        public static final String VIEW_LISTVALIDRESPONSIBILITIES = "ListValidResponsibilities";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditValidResponsibility",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EditValidResponsibility",
            controller = "humanres"
        )
        public static final String VIEW_EDITVALIDRESPONSIBILITY = "EditValidResponsibility";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "viewprofile",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#viewprofile",
            controller = "humanres"
        )
        public static final String VIEW_VIEWPROFILE = "viewprofile";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EmplPositionView",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EmplPositionView",
            controller = "humanres"
        )
        public static final String VIEW_EMPLPOSITIONVIEW = "EmplPositionView";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPartySkills",
            type = "screen",
            page = "component://humanres/widget/PartySkillScreens.xml#FindPartySkills",
            controller = "humanres"
        )
        public static final String VIEW_FINDPARTYSKILLS = "FindPartySkills";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewPartySkill",
            type = "screen",
            page = "component://humanres/widget/PartySkillScreens.xml#NewPartySkill",
            controller = "humanres"
        )
        public static final String VIEW_NEWPARTYSKILL = "NewPartySkill";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPartyQuals",
            type = "screen",
            page = "component://humanres/widget/PartyQualScreens.xml#FindPartyQuals",
            controller = "humanres"
        )
        public static final String VIEW_FINDPARTYQUALS = "FindPartyQuals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewPartyQual",
            type = "screen",
            page = "component://humanres/widget/PartyQualScreens.xml#NewPartyQual",
            controller = "humanres"
        )
        public static final String VIEW_NEWPARTYQUAL = "NewPartyQual";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyQuals",
            type = "screen",
            page = "component://humanres/widget/PartyQualScreens.xml#EditPartyQuals",
            controller = "humanres"
        )
        public static final String VIEW_EDITPARTYQUALS = "EditPartyQuals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindEmploymentApps",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#FindEmploymentApps",
            controller = "humanres"
        )
        public static final String VIEW_FINDEMPLOYMENTAPPS = "FindEmploymentApps";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "NewEmploymentApp",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#NewEmploymentApp",
            controller = "humanres"
        )
        public static final String VIEW_NEWEMPLOYMENTAPP = "NewEmploymentApp";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "ViewEmploymentApp",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#ViewEmploymentApp",
            controller = "humanres"
        )
        public static final String VIEW_VIEWEMPLOYMENTAPP = "ViewEmploymentApp";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmploymentApp",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#EditEmploymentApp",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYMENTAPP = "EditEmploymentApp";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPartyResumes",
            type = "screen",
            page = "component://humanres/widget/PartyResumeScreens.xml#FindPartyResumes",
            controller = "humanres"
        )
        public static final String VIEW_FINDPARTYRESUMES = "FindPartyResumes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyResume",
            type = "screen",
            page = "component://humanres/widget/PartyResumeScreens.xml#EditPartyResume",
            controller = "humanres"
        )
        public static final String VIEW_EDITPARTYRESUME = "EditPartyResume";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPayGrades",
            type = "screen",
            page = "component://humanres/widget/PayGradeScreens.xml#FindPayGrades",
            controller = "humanres"
        )
        public static final String VIEW_FINDPAYGRADES = "FindPayGrades";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPayGrade",
            type = "screen",
            page = "component://humanres/widget/PayGradeScreens.xml#EditPayGrade",
            controller = "humanres"
        )
        public static final String VIEW_EDITPAYGRADE = "EditPayGrade";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindPerfReviews",
            type = "screen",
            page = "component://humanres/widget/PerfReviewScreens.xml#FindPerfReviews",
            controller = "humanres"
        )
        public static final String VIEW_FINDPERFREVIEWS = "FindPerfReviews";

    }

    // Auto-generated split (Part 4)
    public static class Part4 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPerfReviews",
            type = "screen",
            page = "component://humanres/widget/PerfReviewScreens.xml#EditPerfReviews",
            controller = "humanres"
        )
        public static final String VIEW_EDITPERFREVIEWS = "EditPerfReviews";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPerfReviewItems",
            type = "screen",
            page = "component://humanres/widget/PerfReviewScreens.xml#EditPerfReviewItems",
            controller = "humanres"
        )
        public static final String VIEW_EDITPERFREVIEWITEMS = "EditPerfReviewItems";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPerformanceNotes",
            type = "screen",
            page = "component://humanres/widget/EmploymentScreens.xml#EditPerformanceNotes",
            controller = "humanres"
        )
        public static final String VIEW_EDITPERFORMANCENOTES = "EditPerformanceNotes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditSkillTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditSkillTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITSKILLTYPES = "EditSkillTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindEmplPositionTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#FindEmplPositionTypes",
            controller = "humanres"
        )
        public static final String VIEW_FINDEMPLPOSITIONTYPES = "FindEmplPositionTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplPositionTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditEmplPositionTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLPOSITIONTYPES = "EditEmplPositionTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplPositionTypeRates",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditEmplPositionTypeRates",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLPOSITIONTYPERATES = "EditEmplPositionTypeRates";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditResponsibilityTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditResponsibilityTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITRESPONSIBILITYTYPES = "EditResponsibilityTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTerminationTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditTerminationTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITTERMINATIONTYPES = "EditTerminationTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTerminationReasons",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditTerminationReasons",
            controller = "humanres"
        )
        public static final String VIEW_EDITTERMINATIONREASONS = "EditTerminationReasons";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTrainingTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditTrainingTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITTRAININGTYPES = "EditTrainingTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PublicHoliday",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#PublicHoliday",
            controller = "humanres"
        )
        public static final String VIEW_PUBLICHOLIDAY = "PublicHoliday";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyResumes",
            type = "screen",
            page = "component://party/widget/partymgr/PartyScreens.xml#EditPartyResumes",
            controller = "humanres"
        )
        public static final String VIEW_EDITPARTYRESUMES = "EditPartyResumes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmploymentApps",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#EditEmploymentApps",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLOYMENTAPPS = "EditEmploymentApps";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindEmplLeaves",
            type = "screen",
            page = "component://humanres/widget/EmplLeaveScreens.xml#FindEmplLeaves",
            controller = "humanres"
        )
        public static final String VIEW_FINDEMPLLEAVES = "FindEmplLeaves";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplLeave",
            type = "screen",
            page = "component://humanres/widget/EmplLeaveScreens.xml#EditEmplLeave",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLLEAVE = "EditEmplLeave";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplLeaveTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditEmplLeaveTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLLEAVETYPES = "EditEmplLeaveTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplLeaveReasonTypes",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditEmplLeaveReasonTypes",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLLEAVEREASONTYPES = "EditEmplLeaveReasonTypes";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindLeaveApprovals",
            type = "screen",
            page = "component://humanres/widget/EmplLeaveScreens.xml#FindLeaveApprovals",
            controller = "humanres"
        )
        public static final String VIEW_FINDLEAVEAPPROVALS = "FindLeaveApprovals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditEmplLeaveStatus",
            type = "screen",
            page = "component://humanres/widget/EmplLeaveScreens.xml#EditEmplLeaveStatus",
            controller = "humanres"
        )
        public static final String VIEW_EDITEMPLLEAVESTATUS = "EditEmplLeaveStatus";

    }

    // Auto-generated split (Part 5)
    public static class Part5 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditInternalOrgFtl",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#EditInternalOrgFtl",
            controller = "humanres"
        )
        public static final String VIEW_EDITINTERNALORGFTL = "EditInternalOrgFtl";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "RemoveInternalOrgFtl",
            type = "screen",
            page = "component://humanres/widget/EmplPositionScreens.xml#RemoveInternalOrgFtl",
            controller = "humanres"
        )
        public static final String VIEW_REMOVEINTERNALORGFTL = "RemoveInternalOrgFtl";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindJobRequisitions",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#FindJobRequisitions",
            controller = "humanres"
        )
        public static final String VIEW_FINDJOBREQUISITIONS = "FindJobRequisitions";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditJobRequisition",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#EditJobRequisition",
            controller = "humanres"
        )
        public static final String VIEW_EDITJOBREQUISITION = "EditJobRequisition";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindInternalJobPosting",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#FindInternalJobPosting",
            controller = "humanres"
        )
        public static final String VIEW_FINDINTERNALJOBPOSTING = "FindInternalJobPosting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditInternalJobPosting",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#EditInternalJobPosting",
            controller = "humanres"
        )
        public static final String VIEW_EDITINTERNALJOBPOSTING = "EditInternalJobPosting";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindJobInterview",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#FindJobInterview",
            controller = "humanres"
        )
        public static final String VIEW_FINDJOBINTERVIEW = "FindJobInterview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditJobInterview",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#EditJobInterview",
            controller = "humanres"
        )
        public static final String VIEW_EDITJOBINTERVIEW = "EditJobInterview";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindApprovals",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#FindApprovals",
            controller = "humanres"
        )
        public static final String VIEW_FINDAPPROVALS = "FindApprovals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditApprovalStatus",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#EditApprovalStatus",
            controller = "humanres"
        )
        public static final String VIEW_EDITAPPROVALSTATUS = "EditApprovalStatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindRelocation",
            type = "screen",
            page = "component://humanres/widget/RecruitmentScreens.xml#FindRelocation",
            controller = "humanres"
        )
        public static final String VIEW_FINDRELOCATION = "FindRelocation";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditJobInterviewType",
            type = "screen",
            page = "component://humanres/widget/GlobalHRSettingScreens.xml#EditJobInterviewType",
            controller = "humanres"
        )
        public static final String VIEW_EDITJOBINTERVIEWTYPE = "EditJobInterviewType";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "TrainingCalendar",
            type = "screen",
            page = "component://humanres/widget/PersonTrainingScreens.xml#TrainingCalendarWithDecorator",
            controller = "humanres"
        )
        public static final String VIEW_TRAININGCALENDAR = "TrainingCalendar";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindTrainingStatus",
            type = "screen",
            page = "component://humanres/widget/PersonTrainingScreens.xml#FindTrainingStatus",
            controller = "humanres"
        )
        public static final String VIEW_FINDTRAININGSTATUS = "FindTrainingStatus";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "FindTrainingApprovals",
            type = "screen",
            page = "component://humanres/widget/PersonTrainingScreens.xml#FindTrainingApprovals",
            controller = "humanres"
        )
        public static final String VIEW_FINDTRAININGAPPROVALS = "FindTrainingApprovals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditTrainingApprovals",
            type = "screen",
            page = "component://humanres/widget/PersonTrainingScreens.xml#EditTrainingApprovals",
            controller = "humanres"
        )
        public static final String VIEW_EDITTRAININGAPPROVALS = "EditTrainingApprovals";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "LookupTraining",
            type = "screen",
            page = "component://humanres/widget/LookupScreens.xml#LookupTraining",
            controller = "humanres"
        )
        public static final String VIEW_LOOKUPTRAINING = "LookupTraining";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "PayrollHistory",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#PayrollHistory",
            controller = "humanres"
        )
        public static final String VIEW_PAYROLLHISTORY = "PayrollHistory";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "EditPartyContents",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EditPartyContents",
            controller = "humanres"
        )
        public static final String VIEW_EDITPARTYCONTENTS = "EditPartyContents";

        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "employeeContentList",
            type = "screen",
            page = "component://humanres/widget/EmployeeScreens.xml#EmployeeContentList",
            controller = "humanres"
        )
        public static final String VIEW_EMPLOYEECONTENTLIST = "employeeContentList";

    }

    // Auto-generated split (Part 6)
    public static class Part6 {
        @com.ilscipio.scipio.ce.webapp.control.def.View(
            name = "emplAppResumeList",
            type = "screen",
            page = "component://humanres/widget/EmploymentAppScreens.xml#EmploymentAppResumeList",
            controller = "humanres"
        )
        public static final String VIEW_EMPLAPPRESUMELIST = "emplAppResumeList";

        @Request(
            uri = "view",
            controller = "humanres",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "main")
        public interface View {}

        @Request(
            uri = "main",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        public interface Main {}

        @Request(
            uri = "FindPartyQuals",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyQuals")
        public interface FindPartyQuals {}

        @Request(
            uri = "NewPartyQual",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewPartyQual")
        public interface NewPartyQual {}

        @Request(
            uri = "createPartyQual",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewPartyQual")
        @Event(type = "service", invoke = "createPartyQual")
        public static String createPartyQual(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyQual",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyQuals")
        @Event(type = "service-multi", invoke = "updatePartyQual")
        public static String updatePartyQual(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyQual",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyQuals")
        @Event(type = "service", invoke = "deletePartyQual")
        public static String deletePartyQual(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPartyResumes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyResumes")
        public interface FindPartyResumes {}

        @Request(
            uri = "EditPartyResume",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResume")
        public interface EditPartyResume {}

        @Request(
            uri = "createPartyResume",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResume")
        @Event(type = "service", invoke = "createPartyResume")
        public static String createPartyResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyResume",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResume")
        @Event(type = "service", invoke = "updatePartyResume")
        public static String updatePartyResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyResume",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartyResumes")
        @Event(type = "service", invoke = "deletePartyResume")
        public static String deletePartyResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPartySkills",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartySkills")
        public interface FindPartySkills {}

        @Request(
            uri = "NewPartySkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewPartySkill")
        public interface NewPartySkill {}

        @Request(
            uri = "createPartySkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartySkills")
        @Response(name = "error", type = "view", value = "NewPartySkill")
        @Event(type = "service", invoke = "createPartySkill")
        public static String createPartySkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartySkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartySkills")
        @Response(name = "error", type = "view", value = "FindPartySkills")
        @Event(type = "service", invoke = "updatePartySkill")
        public static String updatePartySkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartySkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPartySkills")
        @Event(type = "service", invoke = "deletePartySkill")
        public static String deletePartySkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPerfReviews",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPerfReviews")
        public interface FindPerfReviews {}

        @Request(
            uri = "EditPerfReview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerfReviews")
        public interface EditPerfReview {}

    }

    // Auto-generated split (Part 7)
    public static class Part7 {
        @Request(
            uri = "createPerfReview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "error", type = "view", value = "EditPerfReviews")
        @Response(name = "success", type = "request-redirect", value = "EditPerfReview")
        @Event(type = "service", invoke = "createPerfReview")
        public static String createPerfReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePerfReview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "error", type = "view", value = "EditPerfReviews")
        @Response(name = "success", type = "request-redirect", value = "EditPerfReview")
        @Event(type = "service", invoke = "updatePerfReview")
        public static String updatePerfReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePerfReview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerfReviews")
        @Event(type = "service", invoke = "deletePerfReview")
        public static String deletePerfReview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditPerfReviewItems",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerfReviewItems")
        public interface EditPerfReviewItems {}

        @Request(
            uri = "createPerfReviewItem",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerfReviewItems")
        @Event(type = "service", invoke = "createPerfReviewItem")
        public static String createPerfReviewItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePerfReviewItem",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerfReviewItems")
        @Event(type = "service", invoke = "updatePerfReviewItem")
        public static String updatePerfReviewItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePerfReviewItem",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerfReviewItems")
        @Event(type = "service", invoke = "deletePerfReviewItem")
        public static String deletePerfReviewItem(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditPerformanceNotes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerformanceNotes")
        public interface EditPerformanceNotes {}

        @Request(
            uri = "createPerformanceNote",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPerformanceNotes")
        @Response(name = "error", type = "view", value = "EditPerformanceNotes")
        @Event(type = "service", invoke = "createPerformanceNote")
        public static String createPerformanceNote(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindEmployments",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmployments")
        public interface FindEmployments {}

        @Request(
            uri = "EditEmployment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployment")
        public interface EditEmployment {}

        @Request(
            uri = "ListEmployments",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListEmployments")
        public interface ListEmployments {}

        @Request(
            uri = "createEmployment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployment")
        @Event(type = "service", invoke = "createEmployment")
        public static String createEmployment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmployment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployment")
        @Event(type = "service", invoke = "updateEmployment")
        public static String updateEmployment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmployment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmployments")
        @Event(type = "service", invoke = "deleteEmployment")
        public static String deleteEmployment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindEmploymentApps",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmploymentApps")
        public interface FindEmploymentApps {}

        @Request(
            uri = "NewEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewEmploymentApp")
        public interface NewEmploymentApp {}

        @Request(
            uri = "ViewEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ViewEmploymentApp")
        public interface ViewEmploymentApp {}

        @Request(
            uri = "EditEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmploymentApp")
        public interface EditEmploymentApp {}

        @Request(
            uri = "createEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmploymentApp")
        @Response(name = "error", type = "view", value = "EditEmploymentApp")
        @Event(type = "service", invoke = "createEmploymentApp")
        public static String createEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 8)
    public static class Part8 {
        @Request(
            uri = "updateEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "FindEmploymentApps")
        @Response(name = "error", type = "view", value = "FindEmploymentApps")
        @Event(type = "service-multi", invoke = "updateEmploymentApp")
        public static String updateEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmploymentAppSingle",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditEmploymentApp")
        @Response(name = "error", type = "view", value = "EditEmploymentApp")
        @Event(type = "service", invoke = "updateEmploymentApp")
        public static String updateEmploymentAppSingle(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmploymentApps")
        @Event(type = "service", invoke = "deleteEmploymentApp")
        public static String deleteEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadEmployeeContent",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "employeeContentList")
        @Response(name = "error", type = "view", value = "EventMessages")
        @Event(type = "service", invoke = "uploadPartyContentFile")
        public static String uploadEmployeeContent(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "uploadEmplAppResume",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "emplAppResumeList")
        @Response(name = "error", type = "view", value = "EventMessages")
        @Event(type = "service", invoke = "uploadPartyContentFile")
        public static String uploadEmplAppResume(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "ListPayHistories",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPayHistories")
        public interface ListPayHistories {}

        @Request(
            uri = "updatePayHistory",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPayHistories")
        @Event(type = "service", invoke = "updatePayHistory")
        public static String updatePayHistory(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePayHistory",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListPayHistories")
        @Event(type = "service", invoke = "deletePayHistory")
        public static String deletePayHistory(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findPartyBenefits",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyBenefits")
        public interface FindPartyBenefits {}

        @Request(
            uri = "EditPartyBenefits",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyBenefits")
        public interface EditPartyBenefits {}

        @Request(
            uri = "createPartyBenefit",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyBenefits")
        @Event(type = "service", invoke = "createPartyBenefit")
        public static String createPartyBenefit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyBenefit",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyBenefits")
        @Event(type = "service-multi", invoke = "updatePartyBenefit")
        public static String updatePartyBenefit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePartyBenefit",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyBenefits")
        @Event(type = "service", invoke = "deletePartyBenefit")
        public static String deletePartyBenefit(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findPayRollPreferences",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayrollPreferences")
        public interface FindPayRollPreferences {}

        @Request(
            uri = "EditPayrollPreferences",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayrollPreferences")
        public interface EditPayrollPreferences {}

        @Request(
            uri = "createPayrollPreference",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayrollPreferences")
        @Event(type = "service", invoke = "createPayrollPreference")
        public static String createPayrollPreference(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePayrollPreference",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayrollPreferences")
        @Event(type = "service-multi", invoke = "updatePayrollPreference")
        public static String updatePayrollPreference(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePayrollPreference",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayrollPreferences")
        @Event(type = "service", invoke = "deletePayrollPreference")
        public static String deletePayrollPreference(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindPayGrades",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPayGrades")
        public interface FindPayGrades {}

        @Request(
            uri = "EditPayGrade",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayGrade")
        public interface EditPayGrade {}

    }

    // Auto-generated split (Part 9)
    public static class Part9 {
        @Request(
            uri = "createPayGrade",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayGrade")
        @Response(name = "error", type = "view", value = "EditPayGrade")
        @Event(type = "service", invoke = "createPayGrade")
        public static String createPayGrade(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePayGrade",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPayGrade")
        @Response(name = "error", type = "view", value = "EditPayGrade")
        @Event(type = "service", invoke = "updatePayGrade")
        public static String updatePayGrade(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePayGrade",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindPayGrades")
        @Response(name = "error", type = "view", value = "FindPayGrades")
        @Event(type = "service", invoke = "deletePayGrade")
        public static String deletePayGrade(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditSalarySteps",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalarySteps")
        public interface EditSalarySteps {}

        @Request(
            uri = "createSalaryStep",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalarySteps")
        @Event(type = "service", invoke = "createSalaryStep")
        public static String createSalaryStep(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSalaryStep",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalarySteps")
        @Event(type = "service-multi", invoke = "updateSalaryStep")
        public static String updateSalaryStep(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSalaryStep",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSalarySteps")
        @Event(type = "service", invoke = "deleteSalaryStep")
        public static String deleteSalaryStep(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTerminationReasons",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationReasons")
        public interface EditTerminationReasons {}

        @Request(
            uri = "createTerminationReason",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationReasons")
        @Event(type = "service", invoke = "createTerminationReason")
        public static String createTerminationReason(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTerminationReason",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationReasons")
        @Event(type = "service-multi", invoke = "updateTerminationReason")
        public static String updateTerminationReason(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTerminationReason",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationReasons")
        @Event(type = "service", invoke = "deleteTerminationReason")
        public static String deleteTerminationReason(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindUnemploymentClaim",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUnemploymentClaims")
        public interface FindUnemploymentClaim {}

        @Request(
            uri = "EditUnemploymentClaims",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUnemploymentClaims")
        public interface EditUnemploymentClaims {}

        @Request(
            uri = "createUnemploymentClaim",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUnemploymentClaims")
        @Event(type = "service", invoke = "createUnemploymentClaim")
        public static String createUnemploymentClaim(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateUnemploymentClaim",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUnemploymentClaims")
        @Event(type = "service-multi", invoke = "updateUnemploymentClaim")
        public static String updateUnemploymentClaim(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteUnemploymentClaim",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditUnemploymentClaims")
        @Event(type = "service", invoke = "deleteUnemploymentClaim")
        public static String deleteUnemploymentClaim(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindEmplLeaves",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmplLeaves")
        public interface FindEmplLeaves {}

        @Request(
            uri = "EditEmplLeave",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeave")
        public interface EditEmplLeave {}

        @Request(
            uri = "deleteEmplLeave",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmplLeaves")
        @Response(name = "error", type = "view", value = "FindEmplLeaves")
        @Event(type = "service", invoke = "deleteEmplLeave")
        public static String deleteEmplLeave(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmplLeaveTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveTypes")
        public interface EditEmplLeaveTypes {}

    }

    // Auto-generated split (Part 10)
    public static class Part10 {
        @Request(
            uri = "createEmplLeaveType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveTypes")
        @Response(name = "error", type = "view", value = "EditEmplLeaveTypes")
        @Event(type = "service", invoke = "createEmplLeaveType")
        public static String createEmplLeaveType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplLeaveType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveTypes")
        @Response(name = "error", type = "view", value = "EditEmplLeaveTypes")
        @Event(type = "service-multi", invoke = "updateEmplLeaveType")
        public static String updateEmplLeaveType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplLeaveType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveTypes")
        @Response(name = "error", type = "view", value = "EditEmplLeaveTypes")
        @Event(type = "service", invoke = "deleteEmplLeaveType")
        public static String deleteEmplLeaveType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindEmplPositions",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmplPositions")
        public interface FindEmplPositions {}

        @Request(
            uri = "EditEmplPosition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPosition")
        public interface EditEmplPosition {}

        @Request(
            uri = "ListEmplPositions",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListEmplPositions")
        public interface ListEmplPositions {}

        @Request(
            uri = "createEmplPosition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPosition")
        @Response(name = "error", type = "view", value = "EditEmplPosition")
        @Event(type = "service", invoke = "createEmplPosition")
        public static String createEmplPosition(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplPosition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPosition")
        @Event(type = "service", invoke = "updateEmplPosition")
        public static String updateEmplPosition(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplPosition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmplPositions")
        @Event(type = "service", invoke = "deleteEmplPosition")
        public static String deleteEmplPosition(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmplPositionFulfillments",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionFulfillments")
        public interface EditEmplPositionFulfillments {}

        @Request(
            uri = "createEmplPositionFulfillment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionFulfillments")
        @Event(type = "service", invoke = "createEmplPositionFulfillment")
        public static String createEmplPositionFulfillment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplPositionFulfillment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionFulfillments")
        @Event(type = "service", invoke = "updateEmplPositionFulfillment")
        public static String updateEmplPositionFulfillment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplPositionFulfillment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionFulfillments")
        @Event(type = "service", invoke = "deleteEmplPositionFulfillment")
        public static String deleteEmplPositionFulfillment(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmplPositionResponsibilities",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionResponsibilities")
        public interface EditEmplPositionResponsibilities {}

        @Request(
            uri = "createEmplPositionResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionResponsibilities")
        @Event(type = "service", invoke = "createEmplPositionResponsibility")
        public static String createEmplPositionResponsibility(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplPositionResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionResponsibilities")
        @Event(type = "service", invoke = "updateEmplPositionResponsibility")
        public static String updateEmplPositionResponsibility(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplPositionResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionResponsibilities")
        @Event(type = "service", invoke = "deleteEmplPositionResponsibility")
        public static String deleteEmplPositionResponsibility(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmplPositionReportingStructs",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionReportingStructs")
        public interface EditEmplPositionReportingStructs {}

        @Request(
            uri = "createEmplPositionReportingStruct",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionReportingStructs")
        @Response(name = "error", type = "view", value = "EditEmplPositionReportingStructs")
        @Event(type = "service", invoke = "createEmplPositionReportingStruct")
        public static String createEmplPositionReportingStruct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplPositionReportingStruct",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionReportingStructs")
        @Response(name = "error", type = "view", value = "EditEmplPositionReportingStructs")
        @Event(type = "service", invoke = "updateEmplPositionReportingStruct")
        public static String updateEmplPositionReportingStruct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 11)
    public static class Part11 {
        @Request(
            uri = "deleteEmplPositionReportingStruct",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionReportingStructs")
        @Response(name = "error", type = "view", value = "EditEmplPositionReportingStructs")
        @Event(type = "service", invoke = "deleteEmplPositionReportingStruct")
        public static String deleteEmplPositionReportingStruct(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findValidResponsibilities",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListValidResponsibilities")
        public interface FindValidResponsibilities {}

        @Request(
            uri = "EditValidResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditValidResponsibility")
        public interface EditValidResponsibility {}

        @Request(
            uri = "createValidResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditValidResponsibility")
        @Event(type = "service", invoke = "createValidResponsibility")
        public static String createValidResponsibility(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateValidResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditValidResponsibility")
        @Event(type = "service", invoke = "updateValidResponsibility")
        public static String updateValidResponsibility(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteValidResponsibility",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "ListValidResponsibilities")
        @Event(type = "service", invoke = "deleteValidResponsibility")
        public static String deleteValidResponsibility(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EmployeeProfile",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EmployeeProfile", saveHomeView = "true")
        public interface EmployeeProfile {}

        @Request(
            uri = "findSkillTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSkillTypes")
        public interface FindSkillTypes {}

        @Request(
            uri = "EditSkillTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSkillTypes")
        public interface EditSkillTypes {}

        @Request(
            uri = "createSkillType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSkillTypes")
        @Event(type = "service", invoke = "createSkillType")
        public static String createSkillType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateSkillType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSkillTypes")
        @Event(type = "service-multi", invoke = "updateSkillType")
        public static String updateSkillType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteSkillType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSkillTypes")
        @Event(type = "service", invoke = "deleteSkillType")
        public static String deleteSkillType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "findEmployees",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmployee")
        public interface FindEmployees {}

        @Request(
            uri = "NewEmployee",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "NewEmployee")
        public interface NewEmployee {}

        @Request(
            uri = "createEmployee",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EmployeeProfile")
        @Response(name = "error", type = "view", value = "NewEmployee")
        @Event(type = "service", invoke = "createEmployee")
        public static String createEmployee(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmployeeSkills",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeSkills")
        public interface EditEmployeeSkills {}

        @Request(
            uri = "createEmployeeSkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeSkills")
        @Response(name = "error", type = "view", value = "EditEmployeeSkills")
        @Event(type = "service", invoke = "createPartySkill")
        public static String createEmployeeSkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmployeeSkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeSkills")
        @Response(name = "error", type = "view", value = "EditEmployeeSkills")
        @Event(type = "service", invoke = "updatePartySkill")
        public static String updateEmployeeSkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmployeeSkill",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeSkills")
        @Response(name = "error", type = "view", value = "EditEmployeeSkills")
        @Event(type = "service", invoke = "deletePartySkill")
        public static String deleteEmployeeSkill(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmployeeQuals",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeQuals")
        public interface EditEmployeeQuals {}

    }

    // Auto-generated split (Part 12)
    public static class Part12 {
        @Request(
            uri = "createEmployeeQualification",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeQuals")
        @Response(name = "error", type = "view", value = "EditEmployeeQuals")
        @Event(type = "service", invoke = "createPartyQual")
        public static String createEmployeeQualification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmployeeQualification",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeQuals")
        @Response(name = "error", type = "view", value = "EditEmployeeQuals")
        @Event(type = "service", invoke = "updatePartyQual")
        public static String updateEmployeeQualification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmployeeQualification",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeQuals")
        @Response(name = "error", type = "view", value = "EditEmployeeQuals")
        @Event(type = "service", invoke = "deletePartyQual")
        public static String deleteEmployeeQualification(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmployeeTrainings",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeTrainings")
        public interface EditEmployeeTrainings {}

        @Request(
            uri = "EditEmployeeResumes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeResumes")
        public interface EditEmployeeResumes {}

        @Request(
            uri = "EditEmployeePerformanceNotes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeePerformanceNotes")
        public interface EditEmployeePerformanceNotes {}

        @Request(
            uri = "EditEmployeeLeaves",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeLeaves")
        public interface EditEmployeeLeaves {}

        @Request(
            uri = "createEmplLeave",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeLeaves")
        @Response(name = "error", type = "view", value = "EditEmployeeLeaves")
        @Event(type = "service", invoke = "createEmplLeave")
        public static String createEmplLeave(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplLeave",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeLeaves")
        @Response(name = "error", type = "view", value = "EditEmployeeLeaves")
        @Event(type = "service", invoke = "updateEmplLeave")
        public static String updateEmplLeave(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditResponsibilityTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditResponsibilityTypes")
        public interface EditResponsibilityTypes {}

        @Request(
            uri = "createResponsibilityType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditResponsibilityTypes")
        @Response(name = "error", type = "view", value = "EditResponsibilityTypes")
        @Event(type = "service", invoke = "createResponsibilityType")
        public static String createResponsibilityType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateResponsibilityType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditResponsibilityTypes")
        @Response(name = "error", type = "view", value = "EditResponsibilityTypes")
        @Event(type = "service-multi", invoke = "updateResponsibilityType")
        public static String updateResponsibilityType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteResponsibilityType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditResponsibilityTypes")
        @Response(name = "error", type = "view", value = "EditResponsibilityTypes")
        @Event(type = "service", invoke = "deleteResponsibilityType")
        public static String deleteResponsibilityType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "emplPositionView",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EmplPositionView")
        public interface EmplPositionView {}

        @Request(
            uri = "globalHRSettings",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditSkillTypes")
        public interface GlobalHRSettings {}

        @Request(
            uri = "EditTerminationTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationTypes")
        public interface EditTerminationTypes {}

        @Request(
            uri = "createTerminationType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationTypes")
        @Event(type = "service", invoke = "createTerminationType")
        public static String createTerminationType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTerminationType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationTypes")
        @Event(type = "service-multi", invoke = "updateTerminationType")
        public static String updateTerminationType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTerminationType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTerminationTypes")
        @Event(type = "service", invoke = "deleteTerminationType")
        public static String deleteTerminationType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindEmplPositionTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindEmplPositionTypes")
        public interface FindEmplPositionTypes {}

    }

    // Auto-generated split (Part 13)
    public static class Part13 {
        @Request(
            uri = "EditEmplPositionTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionTypes")
        public interface EditEmplPositionTypes {}

        @Request(
            uri = "createEmplPositionType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionTypes")
        @Event(type = "service", invoke = "createEmplPositionType")
        public static String createEmplPositionType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplPositionType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionTypes")
        @Event(type = "service", invoke = "updateEmplPositionType")
        public static String updateEmplPositionType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplPositionType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "FindEmplPositionTypes")
        @Event(type = "service", invoke = "deleteEmplPositionType")
        public static String deleteEmplPositionType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditEmplPositionTypeRates",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionTypeRates")
        public interface EditEmplPositionTypeRates {}

        @Request(
            uri = "updateEmplPositionTypeRate",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionTypeRates")
        @Event(type = "service", invoke = "updateEmplPositionTypeRate")
        public static String updateEmplPositionTypeRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplPositionTypeRate",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplPositionTypeRates")
        @Event(type = "service", invoke = "deleteEmplPositionTypeRate")
        public static String deleteEmplPositionTypeRate(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditAgreementEmploymentAppls",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementEmploymentAppls")
        public interface EditAgreementEmploymentAppls {}

        @Request(
            uri = "createAgreementEmploymentAppl",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementEmploymentAppls")
        @Response(name = "error", type = "view", value = "EditAgreementEmploymentAppls")
        @Event(type = "service", invoke = "createAgreementEmploymentAppl")
        public static String createAgreementEmploymentAppl(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateAgreementEmploymentAppl",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementEmploymentAppls")
        @Event(type = "service-multi", invoke = "updateAgreementEmploymentAppl")
        public static String updateAgreementEmploymentAppl(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteAgreementEmploymentAppl",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditAgreementEmploymentAppls")
        @Event(type = "service", invoke = "deleteAgreementEmploymentAppl")
        public static String deleteAgreementEmploymentAppl(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditPartySkillsExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        public interface EditPartySkillsExt {}

        @Request(
            uri = "EditPartyResumes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResumes")
        public interface EditPartyResumes {}

        @Request(
            uri = "EditPartyResumesExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyResumes")
        public interface EditPartyResumesExt {}

        @Request(
            uri = "EditEmployeeEmploymentApps",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmployeeEmploymentApps")
        public interface EditEmployeeEmploymentApps {}

        @Request(
            uri = "EditEmploymentAppsExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmploymentApps")
        public interface EditEmploymentAppsExt {}

        @Request(
            uri = "createPartySkillExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        @Response(name = "error", type = "view", value = "EditPartySkills")
        @Event(type = "service", invoke = "createPartySkill")
        public static String createPartySkillExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartySkillExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartySkills")
        @Response(name = "error", type = "view", value = "EditPartySkills")
        @Event(type = "service", invoke = "updatePartySkill")
        public static String updatePartySkillExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createPartyQualExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyQuals")
        @Response(name = "error", type = "view", value = "EditPartyQuals")
        @Event(type = "service", invoke = "createPartyQual")
        public static String createPartyQualExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePartyQualExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditPartyQuals")
        @Response(name = "error", type = "view", value = "EditPartyQuals")
        @Event(type = "service-multi", invoke = "updatePartyQual")
        public static String updatePartyQualExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 14)
    public static class Part14 {
        @Request(
            uri = "createEmploymentAppExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmploymentApp")
        @Response(name = "error", type = "view", value = "EditEmploymentApp")
        @Event(type = "service", invoke = "createEmploymentApp")
        public static String createEmploymentAppExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmploymentAppExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditEmploymentApps")
        @Response(name = "error", type = "view", value = "EditEmploymentApps")
        @Event(type = "service-multi", invoke = "updateEmploymentApp")
        public static String updateEmploymentAppExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmploymentAppExtSingle",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditEmploymentApp")
        @Response(name = "error", type = "view", value = "EditEmploymentApp")
        @Event(type = "service", invoke = "updateEmploymentApp")
        public static String updateEmploymentAppExtSingle(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "createEmplLeaveExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeave")
        @Response(name = "error", type = "view", value = "EditEmplLeave")
        @Event(type = "service", invoke = "createEmplLeave")
        public static String createEmplLeaveExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplLeaveExt",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeave")
        @Response(name = "error", type = "view", value = "EditEmplLeave")
        @Event(type = "service", invoke = "updateEmplLeave")
        public static String updateEmplLeaveExt(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindJobRequisitions",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJobRequisitions")
        public interface FindJobRequisitions {}

        @Request(
            uri = "EditJobRequisition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditJobRequisition")
        public interface EditJobRequisition {}

        @Request(
            uri = "createJobRequisition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditJobRequisition")
        @Response(name = "error", type = "view", value = "EditJobRequisition")
        @Event(type = "service", invoke = "createJobRequisition")
        public static String createJobRequisition(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateJobRequisition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditJobRequisition")
        @Response(name = "error", type = "view", value = "EditJobRequisition")
        @Event(type = "service", invoke = "updateJobRequisition")
        public static String updateJobRequisition(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteJobRequisition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJobRequisitions")
        @Event(type = "service", invoke = "deleteJobRequisition")
        public static String deleteJobRequisition(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindInternalJobPosting",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindInternalJobPosting")
        public interface FindInternalJobPosting {}

        @Request(
            uri = "EditInternalJobPosting",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInternalJobPosting")
        public interface EditInternalJobPosting {}

        @Request(
            uri = "createInternalJobPosting",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInternalJobPosting")
        @Response(name = "error", type = "view", value = "EditInternalJobPosting")
        @Event(type = "service", invoke = "createInternalJobPosting")
        public static String createInternalJobPosting(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateInternalJobPosting",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInternalJobPosting")
        @Response(name = "error", type = "view", value = "EditInternalJobPosting")
        @Event(type = "service", invoke = "updateInternalJobPosting")
        public static String updateInternalJobPosting(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteInternalJobPosting",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindInternalJobPosting")
        @Event(type = "service", invoke = "deleteInternalJobPosting")
        public static String deleteInternalJobPosting(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindJobInterview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJobInterview")
        public interface FindJobInterview {}

        @Request(
            uri = "EditJobInterview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditJobInterview")
        public interface EditJobInterview {}

        @Request(
            uri = "createJobInterview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditJobInterview")
        @Response(name = "error", type = "view", value = "EditJobInterview")
        @Event(type = "service", invoke = "createJobInterview")
        public static String createJobInterview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateJobInterview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditJobInterview")
        @Event(type = "service", invoke = "updateJobInterview")
        public static String updateJobInterview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteJobInterview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindJobInterview")
        @Event(type = "service", invoke = "deleteJobInterview")
        public static String deleteJobInterview(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

    }

    // Auto-generated split (Part 15)
    public static class Part15 {
        @Request(
            uri = "FindApprovals",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindApprovals")
        public interface FindApprovals {}

        @Request(
            uri = "EditApprovalStatus",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditApprovalStatus")
        public interface EditApprovalStatus {}

        @Request(
            uri = "updateApprovalStatus",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditApprovalStatus")
        @Response(name = "error", type = "view", value = "EditApprovalStatus")
        @Event(type = "service", invoke = "updateApprovalStatus")
        public static String updateApprovalStatus(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindRelocation",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindRelocation")
        public interface FindRelocation {}

        @Request(
            uri = "EditJobInterviewType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditJobInterviewType")
        public interface EditJobInterviewType {}

        @Request(
            uri = "createJobInterviewType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditJobInterviewType")
        @Event(type = "service", invoke = "createJobInterviewType")
        public static String createJobInterviewType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateJobInterviewType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditJobInterviewType")
        @Event(type = "service-multi", invoke = "updateJobInterviewType")
        public static String updateJobInterviewType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteJobInterviewType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request", value = "EditJobInterviewType")
        @Event(type = "service", invoke = "deleteJobInterviewType")
        public static String deleteJobInterviewType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "TrainingCalendar",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrainingCalendar")
        public interface TrainingCalendar {}

        @Request(
            uri = "createTrainingCalendar",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrainingCalendar")
        @Response(name = "error", type = "view", value = "TrainingCalendar")
        @Event(type = "service", invoke = "createWorkEffortAndPartyAssign")
        public static String createTrainingCalendar(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateTrainingCalendar",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view-home", value = "TrainingCalendar")
        @Response(name = "error", type = "view", value = "TrainingCalendar")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updateTrainingCalendar(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditWorkEffort",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "TrainingCalendar")
        public interface EditWorkEffort {}

        @Request(
            uri = "createTrainingTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrainingTypes")
        @Response(name = "error", type = "view", value = "EditTrainingTypes")
        @Event(type = "service", invoke = "createTrainingTypes")
        public static String createTrainingTypes(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditTrainingTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrainingTypes")
        public interface EditTrainingTypes {}

        @Request(
            uri = "updateTrainingTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrainingTypes")
        @Response(name = "error", type = "view", value = "EditTrainingTypes")
        @Event(type = "service-multi", invoke = "updateTrainingTypes")
        public static String updateTrainingTypes(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteTrainingTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrainingTypes")
        @Response(name = "error", type = "view", value = "EditTrainingTypes")
        @Event(type = "service", invoke = "deleteTrainingTypes")
        public static String deleteTrainingTypes(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindTrainingStatus",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrainingStatus")
        public interface FindTrainingStatus {}

        @Request(
            uri = "updateTrainingStatus",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrainingApprovals")
        @Response(name = "error", type = "view", value = "EditTrainingApprovals")
        @Event(type = "service", invoke = "updateTrainingStatus")
        public static String updateTrainingStatus(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindTrainingApprovals",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrainingApprovals")
        public interface FindTrainingApprovals {}

        @Request(
            uri = "EditTrainingApprovals",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditTrainingApprovals")
        public interface EditTrainingApprovals {}

    }

    // Auto-generated split (Part 16)
    public static class Part16 {
        @Request(
            uri = "applyTraining",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrainingStatus")
        @Response(name = "error", type = "view", value = "TrainingCalendar")
        @Event(type = "service", invoke = "applyTraining")
        public static String applyTraining(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "assignTraining",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindTrainingApprovals")
        @Response(name = "error", type = "view", value = "TrainingCalendar")
        @Event(type = "service", invoke = "assignTraining")
        public static String assignTraining(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "LookupTraining",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupTraining")
        public interface LookupTraining {}

        @Request(
            uri = "EditEmplLeaveReasonTypes",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveReasonTypes")
        public interface EditEmplLeaveReasonTypes {}

        @Request(
            uri = "createEmplLeaveReasonType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveReasonTypes")
        @Response(name = "error", type = "view", value = "EditEmplLeaveReasonTypes")
        @Event(type = "service", invoke = "createEmplLeaveReasonType")
        public static String createEmplLeaveReasonType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updateEmplLeaveReasonType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveReasonTypes")
        @Response(name = "error", type = "view", value = "EditEmplLeaveReasonTypes")
        @Event(type = "service-multi", invoke = "updateEmplLeaveReasonType")
        public static String updateEmplLeaveReasonType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deleteEmplLeaveReasonType",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveReasonTypes")
        @Response(name = "error", type = "view", value = "EditEmplLeaveReasonTypes")
        @Event(type = "service", invoke = "deleteEmplLeaveReasonType")
        public static String deleteEmplLeaveReasonType(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "FindLeaveApprovals",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "FindLeaveApprovals")
        public interface FindLeaveApprovals {}

        @Request(
            uri = "EditEmplLeaveStatus",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveStatus")
        public interface EditEmplLeaveStatus {}

        @Request(
            uri = "updateEmplLeaveStatus",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditEmplLeaveStatus")
        @Response(name = "error", type = "view", value = "EditEmplLeaveStatus")
        @Event(type = "service", invoke = "updateEmplLeaveStatus")
        public static String updateEmplLeaveStatus(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "getHRChild",
            controller = "humanres",
            secure = "true"
        )
        @Response(name = "success", type = "request", value = "json")
        @Response(name = "error", type = "request", value = "json")
        public static String getHRChild(HttpServletRequest request, HttpServletResponse response) {
            // Delegates to: org.ofbiz.humanres.HumanResEvents.getChildHRCategoryTree
            return HumanResEvents.getChildHRCategoryTree(request, response);
        }

        @Request(
            uri = "createInternalOrg",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        @Event(type = "simple", path = "component://humanres/script/org/ofbiz/humanres/HumanResEvents.xml", invoke = "createInternalOrg")
        public static String createInternalOrg(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "removeInternalOrg",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "main")
        @Response(name = "error", type = "view", value = "main")
        @Event(type = "simple", path = "component://humanres/script/org/ofbiz/humanres/HumanResEvents.xml", invoke = "removeInternalOrg")
        public static String removeInternalOrg(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "EditInternalOrgFtl",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "EditInternalOrgFtl")
        public interface EditInternalOrgFtl {}

        @Request(
            uri = "RemoveInternalOrgFtl",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "RemoveInternalOrgFtl")
        public interface RemoveInternalOrgFtl {}

        @Request(
            uri = "PublicHoliday",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PublicHoliday")
        @Response(name = "error", type = "view", value = "PublicHoliday")
        public interface PublicHoliday {}

        @Request(
            uri = "createPublicHoliday",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "PublicHoliday")
        @Response(name = "error", type = "view", value = "PublicHoliday")
        @Event(type = "simple", path = "component://humanres/script/org/ofbiz/humanres/HumanResEvents.xml", invoke = "createPublicHoliday")
        public static String createPublicHoliday(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "updatePublicHoliday",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "request-redirect-noparam", value = "PublicHoliday")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "updateWorkEffort")
        public static String updatePublicHoliday(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "deletePublicHoliday",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PublicHoliday")
        @Response(name = "error", type = "view-last")
        @Event(type = "service", invoke = "deleteWorkEffort")
        public static String deletePublicHoliday(HttpServletRequest request, HttpServletResponse response) {
            return "success"; // Event handled by @Event annotation
        }

        @Request(
            uri = "PayrollHistory",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "PayrollHistory")
        public interface PayrollHistory {}

    }

    // Auto-generated split (Part 17)
    public static class Part17 {
        @Request(
            uri = "LookupPartyName",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyName")
        public interface LookupPartyName {}

        @Request(
            uri = "LookupPayment",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPayment")
        public interface LookupPayment {}

        @Request(
            uri = "LookupBudget",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupBudget")
        public interface LookupBudget {}

        @Request(
            uri = "LookupBudgetItem",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupBudgetItem")
        public interface LookupBudgetItem {}

        @Request(
            uri = "LookupEmplPosition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupEmplPosition")
        public interface LookupEmplPosition {}

        @Request(
            uri = "LookupTerminationReason",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupTerminationReason")
        public interface LookupTerminationReason {}

        @Request(
            uri = "LookupSalaryStep",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupSalaryStep")
        public interface LookupSalaryStep {}

        @Request(
            uri = "LookupPayGrade",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPayGrade")
        public interface LookupPayGrade {}

        @Request(
            uri = "LookupPayRollPreference",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPayRollPreference")
        public interface LookupPayRollPreference {}

        @Request(
            uri = "LookupUnemploymentClaim",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupUnemploymentClaim")
        public interface LookupUnemploymentClaim {}

        @Request(
            uri = "LookupAgreementEmploymentAppl",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAgreementEmploymentAppl")
        public interface LookupAgreementEmploymentAppl {}

        @Request(
            uri = "LookupPerfReview",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPerfReview")
        public interface LookupPerfReview {}

        @Request(
            uri = "LookupPartyResume",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupPartyResume")
        public interface LookupPartyResume {}

        @Request(
            uri = "LookupEmploymentApp",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupEmploymentApp")
        public interface LookupEmploymentApp {}

        @Request(
            uri = "LookupAgreement",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupAgreement")
        public interface LookupAgreement {}

        @Request(
            uri = "LookupJobRequisition",
            controller = "humanres",
            secure = "true",
            auth = "true"
        )
        @Response(name = "success", type = "view", value = "LookupJobRequisition")
        public interface LookupJobRequisition {}


    }
}
