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
package org.ofbiz.widget.renderer;

import java.io.IOException;
import java.util.Map;

import org.ofbiz.widget.model.ModelTree;

/**
 * Widget Library - Tree String Renderer interface
 */
public interface TreeStringRenderer extends StringRenderer { // SCIPIO: StringRenderer

    public void renderNodeBegin(Appendable writer, Map<String, Object> context, ModelTree.ModelNode node, int depth) throws IOException;
    public void renderNodeEnd(Appendable writer, Map<String, Object> context, ModelTree.ModelNode node) throws IOException;
    public void renderLabel(Appendable writer, Map<String, Object> context, ModelTree.ModelNode.Label label) throws IOException;
    public void renderLink(Appendable writer, Map<String, Object> context, ModelTree.ModelNode.Link link) throws IOException;
    public void renderImage(Appendable writer, Map<String, Object> context, ModelTree.ModelNode.Image image) throws IOException;
    public void renderLastElement(Appendable writer, Map<String, Object> context, ModelTree.ModelNode node) throws IOException;
    public ScreenStringRenderer getScreenStringRenderer(Map<String, Object> context);

}
