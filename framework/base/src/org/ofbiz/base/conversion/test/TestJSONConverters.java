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
package org.ofbiz.base.conversion.test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.conversion.Converter;
import org.ofbiz.base.conversion.ConverterLoader;
import org.ofbiz.base.conversion.Converters;
import org.ofbiz.base.conversion.JSONConverters;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.UtilGenerics;

import junit.framework.TestCase;

public class TestJSONConverters  extends TestCase {
    public TestJSONConverters(String name) {
        super(name);
        ConverterLoader loader = new JSONConverters();
        loader.loadConverters();
    }

    public void testJSONToMap() throws Exception {
        Converter<JSON, Map<String,String>> converter = UtilGenerics.cast(Converters.getConverter(JSON.class, Map.class));
        Map<String,String> map, convertedMap;
        map = new HashMap<String,String>();
        map.put("field1", "value1");
        JSON json = JSON.from(map);
        convertedMap = UtilGenerics.toMap(converter.convert(json));
        assertEquals("JSON to Map", map, convertedMap);
    }

    public void testJSONToList() throws Exception {
        Converter<JSON, List<Object>> converter = UtilGenerics.cast(Converters.getConverter(JSON.class, List.class));
        List<Object> list, convertedList;
        list = new ArrayList<Object>();
        list.add("field1");
        list.add("field2");
        JSON json = JSON.from(list);
        convertedList = UtilGenerics.toList(converter.convert(json));
        assertEquals("JSON to List", list, convertedList);
    }

    public void testMapToJSON() throws Exception {
        Converter<Map<String,Object>, JSON> converter = UtilGenerics.cast(Converters.getConverter(Map.class, JSON.class));
        JSON json;
        Map<String,Object> map = new LinkedHashMap<String,Object>();
        map.put("field1", "value1");
        map.put("field2", new BigDecimal("3.7"));
        json = converter.convert(map);
        assertEquals("Map to JSON", "{\"field1\":\"value1\",\"field2\":3.7}", json.toString());
    }

    public void testListToJSON() throws Exception {
        Converter<List<String>, JSON> converter = UtilGenerics.cast(Converters.getConverter(List.class, JSON.class));
        JSON json;
        List<String> list = new ArrayList<String>();
        list.add("field1");
        list.add("field2");
        json = converter.convert(list);
        assertEquals("List to JSON", "[\"field1\",\"field2\"]", json.toString());
    }
}

