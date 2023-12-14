package com.ilscipio.scipio.common;

public enum SolicitationTypeEnum {

    EMAIL(new String[]{"CUSTOMER_EMAIL_ALLOW_SOL", "USER_EMAIL_ALLOW_SOL", "MAILEON_EMAIL_SOLICITATION"}),
    ADDRESS(new String[]{"CUSTOMER_ADDRESS_ALLOW_SOL", "USER_ADDRESS_ALLOW_SOL"}),
    WORK_PHONE(new String[]{"CUSTOMER_WORK_ALLOW_SOL", "USER_WORK_ALLOW_SOL"}),
    HOME_PHONE(new String[]{"CUSTOMER_HOME_ALLOW_SOL", "USER_HOME_ALLOW_SOL"}),
    MOBILE_PHONE(new String[]{"CUSTOMER_MOBILE_ALLOW_SOL", "USER_MOBILE_ALLOW_SOL"}),
    FAX(new String[]{"CUSTOMER_FAX_ALLOW_SOL", "USER_FAX_ALLOW_SOL"});

    private String[] parameterNames;

    SolicitationTypeEnum(String[] parameterNames) {
        this.parameterNames = parameterNames;
    }

    public String[] getParameterNames() {
        return parameterNames;
    }

}
