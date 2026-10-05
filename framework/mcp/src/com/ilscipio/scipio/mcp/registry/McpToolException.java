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
package com.ilscipio.scipio.mcp.registry;

/**
 * SCIPIO: 4.0.0: A tool-level failure. The message is safe to show to the agent (no internals).
 */
public class McpToolException extends Exception {
    private static final long serialVersionUID = 1L;

    private final boolean denied;

    public McpToolException(String message) {
        this(message, false);
    }

    public McpToolException(String message, boolean denied) {
        super(message);
        this.denied = denied;
    }

    /** True when the failure is a permission or policy denial (audited as DENIED). */
    public boolean isDenied() {
        return denied;
    }

    public static McpToolException denied(String message) {
        return new McpToolException(message, true);
    }
}
