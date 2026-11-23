# Security & Adversarial Hardening (`statci`)

This document describes the threat model, v0.2.0 red-team audit findings, and concrete mitigations implemented in `statci` (`R/security.R` and across all verification engines).

## Reporting a Vulnerability

Please report security issues privately via the repository issue tracker or email (`marcus.harris.uconn@gmail.com`).

---

## v0.2.0 Red-Team Audit Findings & Mitigations

### 1. GitHub Actions Workflow, Shell & R Code Injection (`use_statci_github_action()`)
- **Vulnerability found in v0.1.0:** Unvalidated user strings (`r_version`, `lint_paths`, `manuscript`, `models_path`, `audit_script`, `report_dir`, `file`) were interpolated directly into generated YAML scalars and `run:` blocks. Payloads such as `r_version = "release\n      - run: curl ..."` could inject new workflow steps, `lint_paths = 'R"); system("id"); c("R'` could inject arbitrary R execution, and `file = "../../outside.yaml"` permitted path traversal outside the project root.
- **Mitigation in v0.2.0 (`R/security.R`, `R/github-action.R`):**
  - Strict allowlist validators (`validate_r_version()`, `validate_branch_names()`, `validate_safe_rel_path()`, `validate_contained_dest()`, `safe_yaml_scalar()`) reject control characters, newlines, quotes, backslashes, `..` traversal segments, absolute/home paths, and shell/YAML/R metacharacters.
  - **Zero untrusted string interpolation into `run:` blocks:** All validated parameters are passed via workflow `env:` variables (`STATCI_LINT_PATHS`, `STATCI_MANUSCRIPT`, `STATCI_MODELS_PATH`, `STATCI_REPORT_DIR`, `STATCI_FAIL_ON`, `STATCI_AUDIT_SCRIPT`) and read via `Sys.getenv()` inside static R code (or `Rscript --vanilla "$STATCI_AUDIT_SCRIPT"`).
  - Workflow actions are pinned to explicit patch versions (`actions/checkout@v4.2.2` with `persist-credentials: false`, `r-lib/actions/setup-r@v2.11.2`, `r-lib/actions/setup-r-dependencies@v2.11.2`, `actions/upload-artifact@v4.6.2`) under minimal `permissions: contents: read`.

### 2. HTML Report XSS & Markdown HTML Injection (`render_statci_report()`)
- **Audit scope:** Malicious filenames, rule IDs, lint messages, code snippets, model names, term names, sensitivity details, and report titles containing `<script>`, `<img onerror=...>`, `<svg onload=...>`, or control characters.
- **Mitigation in v0.2.0 (`R/security.R`, `R/report.R`):**
  - Centralized `html_escape_strict()` strips non-printable ASCII control bytes and escapes `&`, `<`, `>`, `"`, and `'` via `htmltools::htmlEscape(..., attribute = TRUE)` across every rendered field.
  - Added a strict Content-Security-Policy header (`<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline';">`) to standalone HTML5 reports as defense-in-depth against script execution.
  - Centralized `md_escape_cell()` escapes `<`, `>`, `&`, `|`, newlines, and control bytes in Markdown table cells so untrusted inputs cannot inject raw HTML into `$GITHUB_STEP_SUMMARY` or PR comments.

### 3. File Path & Symlink Traversal (`stat_lint()`, `stat_lint_dir()`, `audit_manuscript()`)
- **Audit scope:** Control characters, directories passed where files are expected, dangling symlinks, and symlinks inside scanned directories pointing outside the project or model directory.
- **Mitigation in v0.2.0 (`R/security.R`, `R/lint-engine.R`, `R/audit-manuscript.R`):**
  - `validate_existing_file()` and `validate_existing_dir()` reject control bytes, broken symlinks, and mismatched file/directory types.
  - `is_safe_contained_file()` verifies resolved canonical paths stay strictly inside scanned directories (`stat_lint_dir()` skips symlink escapes; `audit_manuscript()` aborts if a `.rds` directory contains a symlink escaping the model directory).

### 4. ReDoS Defense in Manuscript Prose Parser (`audit_manuscript()`)
- **Audit scope:** Pathological long prose strings (e.g., 25,000+ repeated digits, spaces, `*`, `$`, or unclosed `<!-- statci:` / `CI [` delimiters) causing catastrophic PCRE backtracking.
- **Mitigation in v0.2.0 (`R/audit-manuscript.R`):**
  - Replaced unbounded nested quantifiers in `parse_tag_attrs()`, `normalize_prose_line()`, `span_pat`, `comment_pat`, and all claim patterns (`ci_brack_pat`, `ci_to_pat`, etc.) with strictly linear-time bounded-quantifier regexes plus a 20,000-byte per-line scan cap.
  - Verified sub-50ms execution on adversarial ReDoS inputs in `tests/testthat/test-adversarial-security.R`.

### 5. AST Linter Anti-Evasion Hardening (`R/lint-parser.R`, `R/lint-rules.R`)
- **Evasions found & fixed in v0.2.0:**
  - **Fake `# statci-ignore` inside string literals** (`x <- rnorm(5); s <- "# statci-ignore"`) bypassed linting because v0.1.0 scanned raw lines with regex. Now `parse_suppressions_ast()` extracts directives strictly from parsed XML `<COMMENT>` AST tokens.
  - **R 4.1+ shorthand lambdas `\(x)`** bypassed `global_seed_override_in_fn` and false-positived in `unseeded_randomness` because `xmlparsedata` emits `<OP-LAMBDA>` rather than `<FUNCTION>`. All function-scope queries now match `expr[FUNCTION or OP-LAMBDA]`.
  - **Comments/strings spoofing convergence or seed restoration** (`# TODO: check isSingular`, `# $convergence`, `x <- ".Random.seed"`) bypassed `missing_convergence_check` and `global_seed_override_in_fn`. Both rules now inspect AST nodes (`SYMBOL_FUNCTION_CALL`, `SLOT`, `OP-DOLLAR`, `LBB`, and `on.exit()` AST symbols) exclusively.
  - **Non-evaluated Quarto/Rmd pseudocode chunks** (`eval: false` / `eval = FALSE`) no longer produce spurious `parse_error` findings if they contain illustrative non-R syntax.
  - Added built-in rule #14 **`duplicate_parallel_seed`** catching constant `set.seed(42)` inside loops and parallel workers.
