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
