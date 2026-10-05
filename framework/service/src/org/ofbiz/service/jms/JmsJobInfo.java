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
package org.ofbiz.service.jms;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.JobInfo;
import org.ofbiz.service.job.JobPriority;

import java.util.Date;

public class JmsJobInfo implements JobInfo {

    private final String serviceName;
    private final long startTime;

    public JmsJobInfo(ModelService modelService, long startTime) {
        this.serviceName = modelService.name;
        this.startTime = startTime;
    }

    @Override
    public String getJobId() {
        return null;
    }

    @Override
    public String getJobName() {
        return null;
    }

    @Override
    public Date getStartTime() {
        return new Date(startTime);
    }

    @Override
    public long getPriority() {
        return JobPriority.NORMAL;
    }

    @Override
    public String getServiceName() {
        return serviceName;
    }

    @Override
    public String getJobType() {
        return "jms";
    }

    @Override
    public boolean isPersist() {
        return false;
    }

    @Override
    public GenericValue getJobValue() {
        return null;
    }

    @Override
    public String getJobPool() { return null; } // TODO?
}
