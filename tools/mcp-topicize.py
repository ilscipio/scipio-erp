#!/usr/bin/env python3
# Scipio Commerce
# Copyright (C) Ilscipio GmbH
#
# This file is part of Scipio Commerce. Scipio Commerce is free software: you
# can redistribute it and modify it under the terms of the GNU Affero General
# Public License, version 3, as published by the Free Software Foundation.
# Scipio Commerce is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
# for more details. You should have received a copy of the license with this
# work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
# A commercial license is available from Ilscipio GmbH.
#
# SPDX-License-Identifier: AGPL-3.0-only
"""Rewrite the MCP annotations of one *Mcp.java file into topic tools.

Usage: python tools/mcp-topicize.py <JavaFile> <mapping.json>

mapping.json:
{
  "topics": [ {"name": "invoice", "title": "Invoices", "description": "One sentence.", "order": 10, "featured": true} ],
  "tools":  { "old_tool_name": ["topic", "action", "One-sentence description."] },
  "drop":   [ "old_tool_name" ],
  "serverDescription": "optional one-sentence @McpServer description",
  "add":    [ {"service": "createPerson", "topic": "party", "name": "create_person", "description": "...",
               "readOnly": false, "destructive": "false", "requiresConfirmation": false, "exclude": [], "order": 60} ]
}
Every @McpTool / @McpServiceTool must be in "tools" or "drop"; the script fails otherwise.
"""
import io
import json
import re
import sys

DESC_RE = re.compile(r'description\s*=\s*"(?:[^"\\]|\\.)*"(?:\s*\+\s*"(?:[^"\\]|\\.)*")*')
NAME_RE = re.compile(r'\bname\s*=\s*"([^"]*)"')
FEATURED_RE = re.compile(r'\s*featured\s*=\s*(true|false)\s*,')
FEATURED_TAIL_RE = re.compile(r',\s*featured\s*=\s*(true|false)\s*')


def jstr(s):
    return '"' + s.replace('\\', '\\\\').replace('"', '\\"') + '"'


def span_of_annotation(src, start):
    """start points at '@'; returns (open_paren_index, close_paren_index) with balanced parens, string-aware."""
    i = src.index('(', start)
    depth = 0
    j = i
    in_str = False
    while j < len(src):
        c = src[j]
        if in_str:
            if c == '\\':
                j += 2
                continue
            if c == '"':
                in_str = False
        else:
            if c == '"':
                in_str = True
            elif c == '(':
                depth += 1
            elif c == ')':
                depth -= 1
                if depth == 0:
                    return i, j
        j += 1
    raise SystemExit('unbalanced annotation at ' + str(start))


def rewrite_entry(body, mapping, drop, kind):
    m = NAME_RE.search(body)
    if not m:
        raise SystemExit(kind + ' without name: ' + body[:80])
    old = m.group(1)
    if old in drop:
        return None
    if old not in mapping:
        raise SystemExit('no mapping for ' + kind + ' ' + old)
    topic, action, desc = mapping[old][:3]
    body = body[:m.start()] + 'topic = ' + jstr(topic) + ', name = ' + jstr(action) + body[m.end():]
    if DESC_RE.search(body):
        body = DESC_RE.sub(lambda _: 'description = ' + jstr(desc), body, count=1)
    else:
        body = body.rstrip() + ', description = ' + jstr(desc)
    body = FEATURED_RE.sub(',', body, count=1) if FEATURED_RE.search(body) else FEATURED_TAIL_RE.sub('', body, count=1)
    body = re.sub(r',\s*,', ',', body)
    body = re.sub(r'\(\s*,', '(', body)
    return body


def add_entry(a):
    parts = ['service = ' + jstr(a['service']), 'topic = ' + jstr(a['topic']), 'name = ' + jstr(a['name'])]
    parts.append('description = ' + jstr(a.get('description', '')))
    parts.append('readOnly = ' + ('true' if a.get('readOnly') else 'false'))
    if a.get('destructive') not in (None, ''):
        parts.append('destructive = ' + jstr(str(a['destructive']).lower()))
    if a.get('requiresConfirmation'):
        parts.append('requiresConfirmation = true')
    if a.get('permission'):
        parts.append('permission = ' + jstr(a['permission']))
    if a.get('exclude'):
        parts.append('exclude = {' + ', '.join(jstr(x) for x in a['exclude']) + '}')
    if a.get('fixed'):
        parts.append('fixed = {' + ', '.join(jstr(x) for x in a['fixed']) + '}')
    if 'order' in a:
        parts.append('order = ' + str(int(a['order'])))
    return '            @McpServiceTool(' + ',\n                    '.join(parts) + ')'


def topic_entry(t):
    parts = ['name = ' + jstr(t['name'])]
    if t.get('title'):
        parts.append('title = ' + jstr(t['title']))
    if 'order' in t:
        parts.append('order = ' + str(int(t['order'])))
    if t.get('featured'):
        parts.append('featured = true')
    parts.append('description = ' + jstr(t.get('description', '')))
    return '            @McpTopic(' + ', '.join(parts[:-1]) + ',\n                    ' + parts[-1] + ')'


def main():
    path, mapping_path = sys.argv[1], sys.argv[2]
    src = io.open(path, encoding='utf-8').read()
    cfg = json.load(io.open(mapping_path, encoding='utf-8'))
    tools = cfg.get('tools', {})
    drop = set(cfg.get('drop', []))
    seen = set()

    # 1. rewrite every @McpServiceTool and @McpTool annotation
    out = []
    pos = 0
    for m in re.finditer(r'@McpServiceTool\(|@McpTool\(', src):
        if m.start() < pos:
            continue
        kind = m.group(0)[:-1]
        i, j = span_of_annotation(src, m.start())
        body = src[i + 1:j]
        name = NAME_RE.search(body).group(1)
        seen.add(name)
        new = rewrite_entry(body, tools, drop, kind)
        out.append(src[pos:m.start()])
        if new is None:
            # drop the whole entry: for service tools also the trailing comma / line; for methods only the annotation line(s)
            tail = j + 1
            if kind == '@McpServiceTool':
                mm = re.match(r'\s*,', src[tail:])
                if mm:
                    tail += mm.end()
                # remove leading indentation of the entry
                while out and out[-1].endswith((' ', '\t')):
                    out[-1] = out[-1].rstrip(' \t')
            else:
                raise SystemExit('cannot drop a @McpTool method automatically: ' + name + ' (remove the method by hand)')
            pos = tail
            continue
        out.append(kind + '(' + new + ')')
        pos = j + 1
    out.append(src[pos:])
    src = ''.join(out)
    missing = [t for t in tools if t not in seen]
    if missing:
        raise SystemExit('mapping names unknown tools: ' + ', '.join(missing))

    # 2. topics and added service actions in @McpServer / @McpServerExtension
    sm = re.search(r'@McpServer(?:Extension)?\(', src)
    if not sm:
        raise SystemExit('no @McpServer annotation')
    i, j = span_of_annotation(src, sm.start())
    body = src[i + 1:j]
    body = re.sub(r'\s*topics\s*=\s*\{.*?\}\s*,?', '', body, flags=re.S) if 'topics =' in body and '@McpTopic' not in body else body
    if '@McpTopic' in body:
        raise SystemExit('file already has topics; run once only')
    topics_block = 'topics = {\n' + ',\n'.join(topic_entry(t) for t in cfg.get('topics', [])) + '\n        }'
    adds = cfg.get('add', [])
    st = re.search(r'serviceTools\s*=\s*\{', body)
    if adds:
        entries = ',\n'.join(add_entry(a) for a in adds)
        if st:
            k = st.end()
            body = body[:k] + '\n' + entries + ',' + body[k:]
        else:
            body = body.rstrip() + ',\n        serviceTools = {\n' + entries + '\n        }'
    # empty serviceTools = { } after drops
    body = re.sub(r'serviceTools\s*=\s*\{\s*,?\s*\}\s*,?', '', body)
    body = re.sub(r',\s*,', ',', body)
    body = body.rstrip()
    body = body.rstrip(',')
    body = body + ',\n        ' + topics_block
    src = src[:i + 1] + body + src[j:]

    # 2b. optional server description
    if cfg.get('serverDescription'):
        i, j = span_of_annotation(src, re.search(r'@McpServer(?:Extension)?\(', src).start())
        body = src[i + 1:j]
        if DESC_RE.search(body):
            body = DESC_RE.sub(lambda _: 'description = ' + jstr(cfg['serverDescription']), body, count=1)
            src = src[:i + 1] + body + src[j:]

    # 3. imports
    if 'import com.ilscipio.scipio.mcp.def.McpTopic;' not in src:
        src = src.replace('import com.ilscipio.scipio.mcp.def.McpTool;', 'import com.ilscipio.scipio.mcp.def.McpTool;\nimport com.ilscipio.scipio.mcp.def.McpTopic;', 1)
    if '@McpServiceTool(' in src and 'import com.ilscipio.scipio.mcp.def.McpServiceTool;' not in src:
        src = src.replace('import com.ilscipio.scipio.mcp.def.McpTool;', 'import com.ilscipio.scipio.mcp.def.McpServiceTool;\nimport com.ilscipio.scipio.mcp.def.McpTool;', 1)
    io.open(path, 'w', encoding='utf-8').write(src)
    print('rewrote', path, '-', len(seen), 'annotations,', len(adds), 'added,', len(drop), 'dropped')


if __name__ == '__main__':
    main()
