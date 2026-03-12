Synchronize project documentation with the current codebase. Do all of the following:

1. **Detect code changes** (run in parallel):
   - Run `git diff --name-only HEAD~1` to see recently changed files
   - Run `git diff --stat HEAD~1` for change summary
   - Run `git status` for any uncommitted changes

2. **Read all documentation files** (in parallel):
   - `CLAUDE.md`
   - `README.md`
   - `docs/index.qmd`
   - `docs/01-architecture.qmd`
   - `docs/02-data-lineage.qmd`
   - `docs/03-transforms-reference.qmd`
   - `docs/04-dimension-tables.qmd`
   - `docs/05-quality-gates.qmd`
   - `docs/06-configuration.qmd`
   - `docs/07-developer-guide.qmd`
   - `docs/08-lookup-tables.qmd`
   - `docs/_quarto.yml`
   - `data/s3-readme/README.md`

3. **Read changed source files** to understand what's new or modified

4. **For each documentation file**, check if it accurately reflects the current code:
   - **CLAUDE.md**: Key Files list, Architecture section, Pipeline Phases, Output paths (local + S3), Commands, Conventions
   - **01-architecture.qmd**: Phase list/flowchart, file organization tree, checkpoint table, output comparison
   - **02-data-lineage.qmd**: Source-to-target mappings, column lineage matrix
   - **03-transforms-reference.qmd**: Function signatures, parameters, validation rules
   - **05-quality-gates.qmd**: Pre/post validation checks
   - **06-configuration.qmd**: Config variables, S3 settings, upload functions
   - **07-developer-guide.qmd**: Code patterns, contribution guidelines
   - **08-lookup-tables.qmd**: Lookup table references
   - **index.qmd**: Overview flowchart, repository structure tree
   - **_quarto.yml**: Chapter list (if new chapters needed)
   - **`data/s3-readme/README.md`**: S3 folder structure, data flow diagram, prefix table, file naming conventions, "which dataset to use" guidance

5. **Make targeted edits** to each file that needs updating. Preserve existing style, formatting, and Mermaid diagram conventions. Do NOT rewrite sections that are already accurate.

6. **Print a summary** of all changes made, organized by file.

If no documentation updates are needed, say so and explain why.
