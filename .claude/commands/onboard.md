Bootstrap session context by reading key documentation and recent changes. Do all of the following:

1. **Read core domain documentation** (in parallel):
   - `docs/01-architecture.qmd` - Pipeline architecture and phases
   - `docs/02-data-lineage.qmd` - Data flow and column mappings
   - `docs/03-transforms-reference.qmd` - All transformation functions
   - `docs/08-lookup-tables.qmd` - Code definitions and lookup references
   - `docs/reference/irm_25_007_001.md` - IRS IRM 25.7.1: authoritative BMF field definitions, code values, and validation rules
   - `docs/reference/nccs_data_guide.md` - NCCS Data Guide: BMF data quality caveats, NTEE classification, organizational categories, and practical research context

2. **Read the pipeline orchestration**:
   - `R/run_pipeline.R` - Main pipeline with all phases

3. **Check recent changes**:
   - Run `git log --oneline -15` to see recent commits
   - Run `git diff --stat HEAD~5` to see what files changed recently

4. **Check for any in-progress work**:
   - Run `git status` to see uncommitted changes
   - Run `git branch` to see current branch context

5. **Review auto-memory** for previously learned patterns:
   - Read any files in the auto-memory directory

After reading everything, provide a **brief session briefing** that includes:
- Current state of the codebase (branch, recent changes, any uncommitted work)
- Key architectural context relevant to recent work
- Any domain knowledge from memory that may be useful
- Ask what task the user wants to work on this session
