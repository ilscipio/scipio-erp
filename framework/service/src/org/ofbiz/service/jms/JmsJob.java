package org.ofbiz.service.jms;

import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.job.JobInfo;

import java.util.Date;

public class JmsJob implements JobInfo {

    private final String serviceName;
    private final long startTime;

    public JmsJob(ModelService modelService, long startTime) {
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
    public String getServiceName() {
        return serviceName;
    }

    @Override
    public String getJobType() {
        return "jms";
    }

    @Override
    public boolean isPersisted() {
        return false;
    }

    @Override
    public GenericValue getJobValue() {
        return null;
    }
}
