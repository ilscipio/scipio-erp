
Templates for adding an Agent MCP profile, a server extension and an Agent Skill to a Scipio component.

ExampleMcp.java
    A complete @McpServer class with one @McpTool, one @McpServiceTool, and one @McpResource.
    Copy it into your component's src/.../mcp/ package, rename the class, replace the
    @component-package@ placeholder with your component's package, and edit the annotations.
    No build file change is needed: the scipio-component Gradle plugin adds framework:mcp to
    every application, addon and hot-deploy component (set scipioComponent { agentTools.set(false) }
    to opt out).

ExampleMcpExtension.java
    A @McpServerExtension class that adds tools, service tools, featured services and entities
    to an EXISTING server (for example "order") from your own component, with no edit to the
    server's source. Copy, rename, set server = "<name>", and add methods.

SKILL.md
    A skeleton Agent Skill file. Copy it into applications/<app>/skills/<name>/SKILL.md (or the
    matching path under framework/, addons/, or hot-deploy/), fill in the frontmatter and every
    bracketed section, and keep it 60-120 lines, in short, direct sentences. Every backticked
    tool name is checked against the registry at load time; a wrong name shows as a warning in
    the log and on Webtools > Agent Access > Skills.

After you add or change a profile, an extension or a skill on a running server, click "Reload
agent registry" on Webtools > Agent Access > Skills (or call the scipio_reload_registry tool).

See docs/EXTENDING-AGENTIC.md at the repository root for the full guide, docs/AGENT-QUICKSTART.md
for the client setup, and docs/AGENT-SECURITY.md for the permission model and the operations
checklist.
