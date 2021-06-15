package com.ilscipio.scipio.ce.util.servlet;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.w3c.dom.Element;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

public class FieldFilter implements Serializable {
    public static final FieldFilter EMPTY = new FieldFilter(SectionFilter.EMPTY, SectionFilter.EMPTY);

    protected final SectionFilter inputFilter;
    protected final SectionFilter outputFilter;

    public FieldFilter(Element parentElement) {
        Element inputOutputElem = UtilMisc.firstSafe(UtilXml.childElementList(parentElement, "input-output"));
        SectionFilter inputOutputFilter = null;
        if (inputOutputElem != null) {
            inputOutputFilter = new SectionFilter(inputOutputElem);
        }
        Element inputSectionElem = UtilMisc.firstSafe(UtilXml.childElementList(parentElement, "input"));
        SectionFilter inputFilter = (inputSectionElem != null) ? new SectionFilter(inputSectionElem) : SectionFilter.EMPTY;
        if (inputOutputFilter != null) {
            inputFilter = new SectionFilter(inputOutputFilter, inputFilter, false);
        }
        Element outputSectionElem = UtilMisc.firstSafe(UtilXml.childElementList(parentElement, "output"));
        SectionFilter outputFilter = (outputSectionElem != null) ? new SectionFilter(outputSectionElem) : SectionFilter.EMPTY;
        if (inputOutputFilter != null) {
            outputFilter = new SectionFilter(inputOutputFilter, outputFilter, false);
        }
        this.inputFilter = inputFilter;
        this.outputFilter = outputFilter;
    }

    public FieldFilter(SectionFilter inputFilter, SectionFilter outputFilter) {
        this.inputFilter = inputFilter;
        this.outputFilter = outputFilter;
    }

    public FieldFilter(FieldFilter first, FieldFilter second) {
        this.inputFilter = new SectionFilter(first.getInputFilter(), second.getInputFilter());
        this.outputFilter = new SectionFilter(first.getOutputFilter(), second.getOutputFilter());
    }

    public FieldFilter merge(FieldFilter other) {
        if (this == other) {
            return this;
        } else if (this == EMPTY) {
            return other;
        } else if (other == EMPTY) {
            return this;
        } else {
            return new FieldFilter(this, other);
        }
    }

    public SectionFilter getInputFilter() {
        return inputFilter;
    }

    public SectionFilter getOutputFilter() {
        return outputFilter;
    }

    public static class SectionFilter implements Serializable {
        public static final SectionFilter EMPTY = new SectionFilter((String) null);

        protected final String defaultAccess;
        protected final Set<String> allowNames;
        protected final Set<String> denyNames;
        protected final Map<String, Pattern> allowRegex;
        protected final Map<String, Pattern> denyRegex;
        protected final List<Pattern> allowRegexList;
        protected final List<Pattern> denyRegexList;

        public SectionFilter(Element sectionElement) {
            this.defaultAccess = UtilValidate.nullIfEmpty(sectionElement.getAttribute("default-access"));
            Set<String> allowNames = new LinkedHashSet<>();
            Set<String> denyNames = new LinkedHashSet<>();
            Map<String, Pattern> allowRegex = new LinkedHashMap<>();
            Map<String, Pattern> denyRegex = new LinkedHashMap<>();
            List<? extends Element> fieldElementList = UtilXml.childElementList(sectionElement, "field");
            if (fieldElementList != null) {
                for (Element fieldElement : fieldElementList) {
                    String name = UtilValidate.nullIfEmpty(fieldElement.getAttribute("name"));
                    String nameRegex = UtilValidate.nullIfEmpty(fieldElement.getAttribute("name-regex"));
                    String access = UtilValidate.nullIfEmpty(fieldElement.getAttribute("access"));
                    if ("allow".equals(access)) {
                        if (nameRegex != null) {
                            allowRegex.put(name, Pattern.compile(nameRegex));
                        } else if (name != null) {
                            allowNames.add(name);
                        }
                    } else if ("deny".equals(access)) {
                        if (nameRegex != null) {
                            denyRegex.put(name, Pattern.compile(nameRegex));
                        } else if (name != null) {
                            denyNames.add(name);
                        }
                    }
                }
            }
            this.allowNames = allowNames.isEmpty() ? Collections.emptySet() : Collections.unmodifiableSet(allowNames);
            this.denyNames = denyNames.isEmpty() ? Collections.emptySet() : Collections.unmodifiableSet(denyNames);
            this.allowRegex = allowRegex.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(allowRegex);
            this.denyRegex = denyRegex.isEmpty() ? Collections.emptyMap() : Collections.unmodifiableMap(denyRegex);
            this.allowRegexList = this.allowRegex.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(this.allowRegex.values()));
            this.denyRegexList = this.denyRegex.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(this.denyRegex.values()));
        }

        public SectionFilter(String defaultAccess) {
            this.defaultAccess = defaultAccess;
            this.allowNames = Collections.emptySet();
            this.denyNames = Collections.emptySet();
            this.allowRegex = Collections.emptyMap();
            this.denyRegex = Collections.emptyMap();
            this.allowRegexList = Collections.emptyList();
            this.denyRegexList = Collections.emptyList();
        }

        public SectionFilter(SectionFilter first, SectionFilter second, boolean ignoreAccess) {
            this.defaultAccess = (!ignoreAccess && second.getDefaultAccess() != null) ? second.getDefaultAccess() : first.getDefaultAccess();
            this.allowNames = Collections.unmodifiableSet(mergeNames(first.getAllowNames(), second.getAllowNames()));
            this.denyNames = Collections.unmodifiableSet(mergeNames(first.getDenyNames(), second.getDenyNames()));
            this.allowRegex = Collections.unmodifiableMap(mergeRegex(first.getAllowRegex(), second.getAllowRegex()));
            this.denyRegex = Collections.unmodifiableMap(mergeRegex(first.getDenyRegex(), second.getDenyRegex()));
            this.allowRegexList = this.allowRegex.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(this.allowRegex.values()));
            this.denyRegexList = this.denyRegex.isEmpty() ? Collections.emptyList() : Collections.unmodifiableList(new ArrayList<>(this.denyRegex.values()));
        }

        public SectionFilter(SectionFilter first, SectionFilter second) {
            this(first, second, false);
        }

        public SectionFilter merge(SectionFilter other) {
            if (this == other) {
                return this;
            } else if (this == EMPTY) {
                return other;
            } else if (other == EMPTY) {
                return this;
            } else {
                return new SectionFilter(this, other);
            }
        }

        private static Set<String> mergeNames(Collection<String> first, Collection<String> second) {
            Set<String> res = new LinkedHashSet<>(first);
            res.addAll(second);
            return res;
        }

        private static Map<String, Pattern> mergeRegex(Map<String, Pattern> first, Map<String, Pattern> second) {
            Map<String, Pattern> res = new LinkedHashMap<>(first);
            res.putAll(second);
            return res;
        }

        public String getDefaultAccess() {
            return defaultAccess;
        }

        public Set<String> getAllowNames() {
            return allowNames;
        }

        public Set<String> getDenyNames() {
            return denyNames;
        }

        public Map<String, Pattern> getAllowRegex() {
            return allowRegex;
        }

        public Map<String, Pattern> getDenyRegex() {
            return denyRegex;
        }

        public List<Pattern> getAllowRegexList() {
            return allowRegexList;
        }

        public List<Pattern> getDenyRegexList() {
            return denyRegexList;
        }

        public boolean allowsExplicit(String name) {
            if (getAllowNames().contains(name)) {
                return true;
            }
            for (Pattern pat : getAllowRegexList()) {
                if (pat.matcher(name).matches()) {
                    return true;
                }
            }
            return false;
        }

        public boolean deniesExplicit(String name) {
            if (getDenyNames().contains(name)) {
                return true;
            }
            for (Pattern pat : getDenyRegexList()) {
                if (pat.matcher(name).matches()) {
                    return true;
                }
            }
            return false;
        }

        public Boolean allows(String name, Boolean defaultValue) {
            if (deniesExplicit(name)) {
                return false;
            } else if (allowsExplicit(name)) {
                return true;
            }
            if ("deny".equals(getDefaultAccess())) {
                return false;
            } else if ("allow".equals(getDefaultAccess())) {
                return true; // NOTE: legacy system default is true (blacklist mode)
            } else {
                return defaultValue;
            }
        }

        public boolean allows(String name) {
            return allows(name, true);
        }
    }
}
