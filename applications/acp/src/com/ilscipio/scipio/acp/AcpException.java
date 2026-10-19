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
package com.ilscipio.scipio.acp;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * SCIPIO: 4.0.0: An ACP error answer: the HTTP status and the body {@code {type, code, message, param}}.
 */
@SuppressWarnings("serial")
public class AcpException extends Exception {

    public static final String INVALID_REQUEST = "invalid_request";
    public static final String PROCESSING_ERROR = "processing_error";
    public static final String SERVICE_UNAVAILABLE = "service_unavailable";

    private final int status;
    private final String type;
    private final String code;
    private final String param;

    public AcpException(int status, String type, String code, String message, String param) {
        super(message);
        this.status = status;
        this.type = type;
        this.code = code;
        this.param = param;
    }

    public static AcpException invalid(String code, String message, String param) {
        return new AcpException(400, INVALID_REQUEST, code, message, param);
    }

    public static AcpException notFound(String message) {
        return new AcpException(404, INVALID_REQUEST, "not_found", message, null);
    }

    public static AcpException conflict(String code, String message) {
        return new AcpException(409, INVALID_REQUEST, code, message, null);
    }

    public static AcpException unprocessable(String code, String message, String param) {
        return new AcpException(422, INVALID_REQUEST, code, message, param);
    }

    public static AcpException processing(String message) {
        return new AcpException(500, PROCESSING_ERROR, "processing_error", message, null);
    }

    public static AcpException unavailable(String message) {
        return new AcpException(503, SERVICE_UNAVAILABLE, "service_unavailable", message, null);
    }

    public int getStatus() { return status; }
    public String getType() { return type; }
    public String getCode() { return code; }
    public String getParam() { return param; }

    public Map<String, Object> toBody() {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", type);
        m.put("code", code);
        m.put("message", getMessage());
        if (param != null) m.put("param", param);
        return m;
    }
}
