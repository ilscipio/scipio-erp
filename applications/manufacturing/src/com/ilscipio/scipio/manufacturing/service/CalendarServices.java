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
package com.ilscipio.scipio.manufacturing.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CalendarServices {

    /**
     * Create a calendar
     */
    @Service(
        name = "createCalendar",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "createCalendar",
        description = "Create a calendar",
        defaultEntityName = "TechDataCalendar",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCalendar {}

    /**
     * Update a calendar
     */
    @Service(
        name = "updateCalendar",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "updateCalendar",
        description = "Update a calendar",
        defaultEntityName = "TechDataCalendar",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCalendar {}

    /**
     * Remove a calendar
     */
    @Service(
        name = "removeCalendar",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "removeCalendar",
        description = "Remove a calendar",
        defaultEntityName = "TechDataCalendar",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveCalendar {}

    /**
     * Create a Calendar Week
     */
    @Service(
        name = "createCalendarWeek",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "createCalendarWeek",
        description = "Create a Calendar Week",
        defaultEntityName = "TechDataCalendarWeek",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCalendarWeek {}

    /**
     * Update a Calendar Week
     */
    @Service(
        name = "updateCalendarWeek",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "updateCalendarWeek",
        description = "Update a Calendar Week",
        defaultEntityName = "TechDataCalendarWeek",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCalendarWeek {}

    /**
     * Remove a Calendar Week
     */
    @Service(
        name = "removeCalendarWeek",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "removeCalendarWeek",
        description = "Remove a Calendar Week",
        defaultEntityName = "TechDataCalendarWeek",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveCalendarWeek {}

    /**
     * Create a calendar ExceptionDay
     */
    @Service(
        name = "createCalendarExceptionDay",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "createCalendarExceptionDay",
        description = "Create a calendar ExceptionDay",
        defaultEntityName = "TechDataCalendarExcDay",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCalendarExceptionDay {}

    /**
     * Update a calendar ExceptionDay
     */
    @Service(
        name = "updateCalendarExceptionDay",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "updateCalendarExceptionDay",
        description = "Update a calendar ExceptionDay",
        defaultEntityName = "TechDataCalendarExcDay",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCalendarExceptionDay {}

    /**
     * Update a calendar ExceptionDay
     */
    @Service(
        name = "removeCalendarExceptionDay",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "removeCalendarExceptionDay",
        description = "Update a calendar ExceptionDay",
        defaultEntityName = "TechDataCalendarExcDay",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveCalendarExceptionDay {}

    /**
     * Create a Calendar Exception Week
     */
    @Service(
        name = "createCalendarExceptionWeek",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "createCalendarExceptionWeek",
        description = "Create a Calendar Exception Week",
        defaultEntityName = "TechDataCalendarExcWeek",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCalendarExceptionWeek {}

    /**
     * Update a Calendar Exception Week
     */
    @Service(
        name = "updateCalendarExceptionWeek",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "updateCalendarExceptionWeek",
        description = "Update a Calendar Exception Week",
        defaultEntityName = "TechDataCalendarExcWeek",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCalendarExceptionWeek {}

    /**
     * Remove a Calendar Exception Week
     */
    @Service(
        name = "removeCalendarExceptionWeek",
        engine = "java",
        location = "com.ilscipio.scipio.manufacturing.event.RoutingSimpleServices",
        invoke = "removeCalendarExceptionWeek",
        description = "Remove a Calendar Exception Week",
        defaultEntityName = "TechDataCalendarExcWeek",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveCalendarExceptionWeek {}

}
