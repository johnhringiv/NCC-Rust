use glob::glob;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;

static CHAPTER_COMPLETED: i32 = 13;
static EXTRA_COMPLETED: i32 = 13;

#[derive(Debug, PartialEq, Clone)]
enum ProgramOutput {
    Error(i32),
    Result {
        code: i32,
        stdout: Option<String>,
        stderr: Option<String>,
    },
}

#[derive(Debug)]
struct TestCase {
    c_file: String,
    extra_files: Vec<String>, // Library files or assembly files
    output: ProgramOutput,
    requires_mathlib: bool, // needs `-lm` at link time (libm)
    lib_deps: Vec<String>,  // helper-lib .c files (gcc-built) to link in (from the `libs` mapping)
}

/// Accumulated pass/fail tallies for a single case (a case may run several sub-tests).
#[derive(Debug, Default)]
struct CaseResult {
    passed: usize,
    failed: usize,
    failures: Vec<String>,
}

impl CaseResult {
    fn merge(&mut self, other: CaseResult) {
        self.passed += other.passed;
        self.failed += other.failed;
        self.failures.extend(other.failures);
    }
}

/// Load assembly_libs from test_properties.json
fn load_assembly_libs() -> HashMap<String, Vec<String>> {
    let json_content = fs::read_to_string("writing-a-c-compiler-tests/test_properties.json").unwrap_or_default();
    let parsed: serde_json::Value = serde_json::from_str(&json_content).unwrap_or_default();

    let mut result = HashMap::new();
    if let Some(assembly_libs) = parsed.get("assembly_libs").and_then(|v| v.as_object()) {
        for (test_path, libs) in assembly_libs {
            if let Some(libs_arr) = libs.as_array() {
                let lib_paths: Vec<String> = libs_arr
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| {
                        // Add platform-specific suffix
                        let suffix = if cfg!(target_os = "macos") {
                            "_osx.s"
                        } else {
                            "_linux.s"
                        };
                        format!("writing-a-c-compiler-tests/tests/{}{}", s, suffix)
                    })
                    .collect();
                result.insert(test_path.clone(), lib_paths);
            }
        }
    }
    result
}

/// Load `requires_mathlib` (relative paths of tests that must link libm) from test_properties.json.
/// Mirrors the book framework's REQUIRES_MATHLIB; keyed the same way (the `_client` suffix stripped).
fn load_requires_mathlib() -> HashSet<String> {
    let json_content = fs::read_to_string("writing-a-c-compiler-tests/test_properties.json").unwrap_or_default();
    let parsed: serde_json::Value = serde_json::from_str(&json_content).unwrap_or_default();
    parsed
        .get("requires_mathlib")
        .and_then(|v| v.as_array())
        .map(|arr| arr.iter().filter_map(|v| v.as_str()).map(String::from).collect())
        .unwrap_or_default()
}

/// Load the `libs` mapping from test_properties.json: test path -> helper-lib `.c` dependencies
/// (e.g. `helper_libs/nan.c`). These are gcc-built and linked into the test, since they `#include`
/// system headers NCC can't preprocess. Values are resolved to full paths under the tests dir.
fn load_libs() -> HashMap<String, Vec<String>> {
    let json_content = fs::read_to_string("writing-a-c-compiler-tests/test_properties.json").unwrap_or_default();
    let parsed: serde_json::Value = serde_json::from_str(&json_content).unwrap_or_default();

    let mut result = HashMap::new();
    if let Some(libs) = parsed.get("libs").and_then(|v| v.as_object()) {
        for (test_path, deps) in libs {
            if let Some(deps_arr) = deps.as_array() {
                let dep_paths: Vec<String> = deps_arr
                    .iter()
                    .filter_map(|v| v.as_str())
                    .map(|s| format!("writing-a-c-compiler-tests/tests/{}", s))
                    .collect();
                result.insert(test_path.clone(), dep_paths);
            }
        }
    }
    result
}

// we want a mapping of test path to program output
fn get_sandler_cases() -> Vec<TestCase> {
    // Load expected results for valid tests
    let expected_results =
        load_expected("writing-a-c-compiler-tests/expected_results.json").unwrap_or_else(|_| HashMap::new());

    // Load assembly libs configuration
    let assembly_libs = load_assembly_libs();

    // Load the set of tests that must link libm
    let requires_mathlib = load_requires_mathlib();

    // Load helper-lib dependencies (test -> [helper .c files], gcc-built and linked in)
    let libs = load_libs();

    let mut cases = vec![];

    for entry in glob("writing-a-c-compiler-tests/tests/**/*.c")
        .expect("glob err")
        .flatten()
    {
        let chapter = entry
            .to_str()
            .and_then(|s| s.split("chapter_").nth(1))
            .and_then(|s| s.split('/').next())
            .and_then(|s| s.parse::<i32>().ok())
            .unwrap_or(0);
        let extra_credit = entry.to_str().map(|s| s.contains("extra_credit")).unwrap_or(false);

        let path_str = entry.to_str().unwrap_or("");

        // Skip non-client library files (they get compiled with the _client.c file)
        if path_str.contains("/libraries/") && !path_str.ends_with("_client.c") {
            continue;
        }

        // Skip helper libraries: these are dependencies linked into other tests (via the `libs`
        // mapping), not standalone tests. They `#include` system headers NCC can't preprocess and
        // are always built by gcc, then linked into the test that depends on them.
        if path_str.contains("/helper_libs/") {
            continue;
        }

        // Determine the ProgramOutput based on the test type
        let output = if path_str.contains("invalid_lex") {
            ProgramOutput::Error(10)
        } else if path_str.contains("invalid_parse") {
            ProgramOutput::Error(20)
        } else if path_str.contains("invalid_semantics")
            || path_str.contains("invalid_declarations")
            || path_str.contains("invalid_types")
            || path_str.contains("invalid_labels")
            || path_str.contains("invalid_struct_tags")
        {
            ProgramOutput::Error(30)
        } else {
            // Valid test - get expected return code from map
            let relative_path = path_str
                .strip_prefix("writing-a-c-compiler-tests/tests/")
                .unwrap_or(path_str);
            // For library tests, look up by library name (without _client suffix)
            let lookup_path = if relative_path.ends_with("_client.c") {
                relative_path.replace("_client.c", ".c")
            } else {
                relative_path.to_string()
            };
            let expected = expected_results.get(&lookup_path);
            ProgramOutput::Result {
                code: expected.map(|e| e.return_code).unwrap_or(0),
                stdout: expected.and_then(|e| e.stdout.clone()),
                stderr: None, // Expected stderr is always empty
            }
        };

        // Determine extra files (library files or assembly files)
        let mut extra_files = Vec::new();

        // Handle library tests - find the corresponding library file
        if path_str.contains("/libraries/") && path_str.ends_with("_client.c") {
            let lib_file = path_str.replace("_client.c", ".c");
            if std::path::Path::new(&lib_file).exists() {
                extra_files.push(lib_file);
            }
        }

        // Handle assembly libs
        let relative_path = path_str
            .strip_prefix("writing-a-c-compiler-tests/tests/")
            .unwrap_or(path_str);
        if let Some(asm_files) = assembly_libs.get(relative_path) {
            extra_files.extend(asm_files.clone());
        }

        // mathlib and lib deps are keyed like the book's props: a `_client.c` test maps to its
        // `.c` library name
        let props_key = relative_path.replace("_client.c", ".c");
        let needs_mathlib = requires_mathlib.contains(&props_key);
        let lib_deps = libs.get(&props_key).cloned().unwrap_or_default();

        if chapter <= CHAPTER_COMPLETED && (!extra_credit || (chapter <= EXTRA_COMPLETED)) {
            cases.push(TestCase {
                c_file: path_str.to_string(),
                extra_files,
                output,
                requires_mathlib: needs_mathlib,
                lib_deps,
            })
        }
    }
    cases
}

fn get_custom_cases() -> Vec<TestCase> {
    // Load expected results for custom tests
    let expected_results = load_expected("tests/c_programs/expected_results.json").unwrap_or_else(|_| HashMap::new());
    let mut cases = vec![];

    for entry in glob("tests/c_programs/**/*.c").expect("glob err").flatten() {
        let path_str = entry.to_str().unwrap_or("");

        // Determine the ProgramOutput based on the test type
        let output = if path_str.contains("invalid_parse") {
            ProgramOutput::Error(20)
        } else if path_str.contains("invalid_lex") {
            ProgramOutput::Error(10)
        } else if path_str.contains("invalid_semantics") {
            ProgramOutput::Error(30)
        } else {
            // Valid test - get expected return code from map
            let relative_path = path_str.strip_prefix("tests/c_programs/").unwrap_or(path_str);
            let expected = expected_results.get(relative_path);
            ProgramOutput::Result {
                code: expected.map(|e| e.return_code).unwrap_or(0),
                stdout: expected.and_then(|e| e.stdout.clone()),
                stderr: None, // Expected stderr is always empty
            }
        };

        cases.push(TestCase {
            c_file: path_str.to_string(),
            extra_files: vec![],
            output,
            requires_mathlib: false,
            lib_deps: vec![],
        })
    }
    cases
}

/// Determines the correct path to the ncc binary based on the build environment
fn get_ncc_binary_path() -> PathBuf {
    // Check environment variables that cargo llvm-cov sets
    if std::env::var("CARGO_LLVM_COV").is_ok()
        || std::env::var("LLVM_COV_TARGET").is_ok()
        || std::env::var("CARGO_LLVM_COV_TARGET_DIR").is_ok()
    {
        // We're running under cargo llvm-cov
        return PathBuf::from("target/llvm-cov-target/debug/ncc");
    }

    // Check for CARGO_TARGET_DIR environment variable
    if let Ok(target_dir) = std::env::var("CARGO_TARGET_DIR") {
        let profile = if cfg!(debug_assertions) { "debug" } else { "release" };
        return PathBuf::from(target_dir).join(profile).join("ncc");
    }

    // Find the most recently modified ncc binary
    let possible_paths = [
        "target/llvm-cov-target/debug/ncc",
        "target/debug/ncc",
        "target/release/ncc",
    ];

    let mut best_path = PathBuf::from("target/debug/ncc");
    let mut best_time = std::time::SystemTime::UNIX_EPOCH;

    for path_str in &possible_paths {
        let path = PathBuf::from(path_str);
        if let Ok(modified) = fs::metadata(&path).and_then(|m| m.modified())
            && modified > best_time
        {
            best_time = modified;
            best_path = path;
        }
    }

    best_path
}

struct ExpectedResult {
    return_code: i32,
    stdout: Option<String>,
}

fn load_expected(json_path: &str) -> Result<HashMap<String, ExpectedResult>, Box<dyn std::error::Error>> {
    let json_content = fs::read_to_string(json_path)?;

    let parsed: serde_json::Value = serde_json::from_str(&json_content)?;

    let mut results = HashMap::new();
    if let serde_json::Value::Object(obj) = parsed {
        for (file_path, value) in obj {
            if let Some(return_code) = value.get("return_code").and_then(|v| v.as_i64()) {
                let stdout = value.get("stdout").and_then(|v| v.as_str()).map(|s| s.to_string());
                results.insert(
                    file_path,
                    ExpectedResult {
                        return_code: return_code as i32,
                        stdout,
                    },
                );
            }
        }
    }
    Ok(results)
}

fn run_test(case: &TestCase, result: &mut CaseResult, extra_args: &[String], test_label: &str) {
    let path = std::path::Path::new(&case.c_file);
    let binary_path = path.with_extension("");
    let binary_path_str = binary_path.to_str().unwrap();

    let ncc_path = get_ncc_binary_path();
    let mut cmd = std::process::Command::new(&ncc_path);
    cmd.arg(&case.c_file);

    // Add extra files (library files, assembly files)
    for extra in &case.extra_files {
        cmd.arg(extra);
    }

    cmd.arg("-o").arg(binary_path_str);

    // libm is needed only on Linux; macOS provides the math symbols via libSystem (matches the
    // book framework, which skips -lm on OSX).
    if case.requires_mathlib && !cfg!(target_os = "macos") {
        cmd.arg("-lm");
    }

    for arg in extra_args {
        cmd.arg(arg);
    }

    let compile_output = cmd.output().unwrap();

    let actual = if compile_output.status.success() {
        // compilation succeed
        let run_output = std::process::Command::new(binary_path_str).output().unwrap();

        let return_code = run_output.status.code().unwrap_or(-1);
        let stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&run_output.stderr).to_string();
        std::fs::remove_file(&binary_path).ok();
        ProgramOutput::Result {
            code: return_code,
            stdout: if stdout.is_empty() { None } else { Some(stdout) },
            stderr: if stderr.is_empty() { None } else { Some(stderr) },
        }
    } else {
        ProgramOutput::Error(compile_output.status.code().unwrap_or(-1))
    };

    record(case, actual, result, test_label);
}

/// Compares a test's actual outcome against its expected `output` and records pass/fail.
fn record(case: &TestCase, actual: ProgramOutput, result: &mut CaseResult, test_label: &str) {
    let passed = match (&actual, &case.output) {
        (ProgramOutput::Error(a), ProgramOutput::Error(b)) => a == b,
        (
            ProgramOutput::Result {
                code: a_code,
                stdout: a_stdout,
                stderr: a_stderr,
            },
            ProgramOutput::Result {
                code: b_code,
                stdout: b_stdout,
                stderr: b_stderr,
            },
        ) => {
            let code_match = a_code == b_code;
            let stdout_match = match (a_stdout, b_stdout) {
                (_, None) => true, // No expected stdout means we don't check
                (Some(a), Some(b)) => a == b,
                (None, Some(_)) => false,
            };
            // stderr should always be empty (b_stderr is always None)
            let stderr_match = match (a_stderr, b_stderr) {
                (None, None) => true,
                (Some(_), None) => false, // Unexpected stderr output
                (_, Some(_)) => unreachable!("Expected stderr should always be None"),
            };
            code_match && stdout_match && stderr_match
        }
        _ => false,
    };

    if passed {
        result.passed += 1;
    } else {
        result.failed += 1;
        let label = if test_label.is_empty() {
            case.c_file.clone()
        } else {
            format!("{} [{}]", case.c_file, test_label)
        };
        result
            .failures
            .push(format!("{} (expected {:?}, got {:?})", label, case.output, actual));
    }
}

/// Runs a test that depends on helper libraries (the `libs` mapping). Each helper `.c` is compiled
/// by gcc (it `#include`s system headers NCC can't preprocess); then NCC compiles the test and
/// **links** it against those helper objects (exercising NCC's own linker with external objects).
/// Helper objects are named per-test so parallel cases sharing a helper (e.g. several NaN tests)
/// don't collide, and NCC leaves them in place (caller-owned `.o` inputs) for us to clean up.
fn run_test_with_libs(case: &TestCase, result: &mut CaseResult) {
    let ncc_path = get_ncc_binary_path();
    let test_path = std::path::Path::new(&case.c_file);
    let binary_path = test_path.with_extension("");
    let binary_str = binary_path.to_str().unwrap();
    let test_stem = test_path.file_stem().unwrap().to_string_lossy().to_string();

    // gcc compiles each helper to a per-test object (avoids collisions across parallel cases)
    let mut helper_objs: Vec<std::path::PathBuf> = Vec::new();
    for lib in &case.lib_deps {
        let lib_stem = std::path::Path::new(lib).file_stem().unwrap().to_string_lossy();
        let lib_obj = test_path.with_file_name(format!("{test_stem}__{lib_stem}.o"));
        let mut cmd = std::process::Command::new("gcc");
        // On arm64 hosts gcc must cross-target x86_64 to match ncc's output.
        #[cfg(target_arch = "aarch64")]
        cmd.args(["-arch", "x86_64"]);
        let st = cmd.arg("-c").arg(lib).arg("-o").arg(&lib_obj).status();
        if st.is_err() || !st.unwrap().success() {
            for o in &helper_objs {
                std::fs::remove_file(o).ok();
            }
            result.failed += 1;
            result
                .failures
                .push(format!("{} [with-libs] (helper {} compile failed)", case.c_file, lib));
            return;
        }
        helper_objs.push(lib_obj);
    }

    // ncc compiles the test and links it with the helper objects (+ -lm on Linux when needed).
    let mut cmd = std::process::Command::new(&ncc_path);
    cmd.arg(&case.c_file);
    for o in &helper_objs {
        cmd.arg(o);
    }
    cmd.arg("-o").arg(binary_str);
    if case.requires_mathlib && !cfg!(target_os = "macos") {
        cmd.arg("-lm");
    }
    let compile_output = cmd.output().unwrap();

    // helper objects are ours (ncc leaves caller-supplied .o inputs in place)
    for o in &helper_objs {
        std::fs::remove_file(o).ok();
    }

    let actual = if compile_output.status.success() {
        let run_output = std::process::Command::new(binary_str).output().unwrap();
        std::fs::remove_file(&binary_path).ok();
        let stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
        let stderr = String::from_utf8_lossy(&run_output.stderr).to_string();
        ProgramOutput::Result {
            code: run_output.status.code().unwrap_or(-1),
            stdout: if stdout.is_empty() { None } else { Some(stdout) },
            stderr: if stderr.is_empty() { None } else { Some(stderr) },
        }
    } else {
        ProgramOutput::Error(compile_output.status.code().unwrap_or(-1))
    };
    record(case, actual, result, "with-libs");
}

/// Runs a cross-compilation test for library files
/// Compiles client with one compiler and library with the other, then links
fn run_library_cross_test(
    client_file: &str,
    library_file: &str,
    expected: &ProgramOutput,
    result: &mut CaseResult,
    ncc_compiles_client: bool,
    requires_mathlib: bool,
) {
    let ncc_path = get_ncc_binary_path();
    let path = std::path::Path::new(client_file);
    let binary_path = path.with_extension("");
    let binary_path_str = binary_path.to_str().unwrap();

    let client_obj = path.with_extension("o");
    let lib_path = std::path::Path::new(library_file);
    let library_obj = lib_path.with_extension("o");

    let test_label = if ncc_compiles_client {
        "ncc client + gcc lib"
    } else {
        "gcc client + ncc lib"
    };

    // Compile client
    let client_status = if ncc_compiles_client {
        std::process::Command::new(&ncc_path)
            .arg("-c")
            .arg(client_file)
            .arg("-o")
            .arg(&client_obj)
            .status()
    } else {
        let mut cmd = std::process::Command::new("gcc");
        // On arm64 hosts gcc must cross-target x86_64 to match ncc's output.
        #[cfg(target_arch = "aarch64")]
        cmd.args(["-arch", "x86_64"]);
        cmd.arg("-c").arg(client_file).arg("-o").arg(&client_obj).status()
    };

    if client_status.is_err() || !client_status.unwrap().success() {
        result.failed += 1;
        result
            .failures
            .push(format!("{} [{}] (client compilation failed)", client_file, test_label));
        return;
    }

    // Compile library
    let library_status = if ncc_compiles_client {
        let mut cmd = std::process::Command::new("gcc");
        // On arm64 hosts gcc must cross-target x86_64 to match ncc's output.
        #[cfg(target_arch = "aarch64")]
        cmd.args(["-arch", "x86_64"]);
        cmd.arg("-c").arg(library_file).arg("-o").arg(&library_obj).status()
    } else {
        std::process::Command::new(&ncc_path)
            .arg("-c")
            .arg(library_file)
            .arg("-o")
            .arg(&library_obj)
            .status()
    };

    if library_status.is_err() || !library_status.unwrap().success() {
        std::fs::remove_file(&client_obj).ok();
        result.failed += 1;
        result
            .failures
            .push(format!("{} [{}] (library compilation failed)", client_file, test_label));
        return;
    }

    // Link with gcc
    let mut link_cmd = std::process::Command::new("gcc");
    // On arm64 hosts gcc must cross-target x86_64 to match ncc's output.
    #[cfg(target_arch = "aarch64")]
    link_cmd.args(["-arch", "x86_64"]);
    link_cmd
        .arg(&client_obj)
        .arg(&library_obj)
        .arg("-o")
        .arg(binary_path_str);
    if requires_mathlib && !cfg!(target_os = "macos") {
        link_cmd.arg("-lm");
    }
    let link_status = link_cmd.status();

    std::fs::remove_file(&client_obj).ok();
    std::fs::remove_file(&library_obj).ok();

    if link_status.is_err() || !link_status.unwrap().success() {
        result.failed += 1;
        result
            .failures
            .push(format!("{} [{}] (linking failed)", client_file, test_label));
        return;
    }

    // Run the binary
    let run_output = std::process::Command::new(binary_path_str).output().unwrap();
    std::fs::remove_file(&binary_path).ok();

    let return_code = run_output.status.code().unwrap_or(-1);
    let stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&run_output.stderr).to_string();
    let actual = ProgramOutput::Result {
        code: return_code,
        stdout: if stdout.is_empty() { None } else { Some(stdout) },
        stderr: if stderr.is_empty() { None } else { Some(stderr) },
    };

    let passed = match (&actual, expected) {
        (
            ProgramOutput::Result {
                code: a_code,
                stdout: a_stdout,
                stderr: a_stderr,
            },
            ProgramOutput::Result {
                code: b_code,
                stdout: b_stdout,
                stderr: b_stderr,
            },
        ) => {
            let code_match = a_code == b_code;
            let stdout_match = match (a_stdout, b_stdout) {
                (_, None) => true,
                (Some(a), Some(b)) => a == b,
                (None, Some(_)) => false,
            };
            // stderr should always be empty
            let stderr_match = match (a_stderr, b_stderr) {
                (None, None) => true,
                (Some(_), None) => false,
                (_, Some(_)) => unreachable!("Expected stderr should always be None"),
            };
            code_match && stdout_match && stderr_match
        }
        _ => false,
    };

    if passed {
        result.passed += 1;
    } else {
        result.failed += 1;
        result.failures.push(format!(
            "{} [{}] (expected {:?}, got {:?})",
            client_file, test_label, expected, actual
        ));
    }
}

/// Runs all sub-tests for a single case and returns its tally. Pure with respect to shared
/// state (output files are derived from the case's own source path, so cases don't collide),
/// which lets `run_cases` evaluate cases concurrently.
fn run_one_case(case: &TestCase) -> CaseResult {
    let mut result = CaseResult::default();
    let is_library_test = case.c_file.contains("/libraries/") && case.c_file.ends_with("_client.c");

    match &case.output {
        ProgramOutput::Error(_) => {
            run_test(case, &mut result, &[], "");
        }
        ProgramOutput::Result { .. } => {
            // Tests with helper-lib dependencies: ncc compiles the test, gcc builds the helpers, link.
            if !case.lib_deps.is_empty() {
                run_test_with_libs(case, &mut result);
            } else if is_library_test {
                if let Some(library_file) = case.extra_files.first() {
                    // Test 1: ncc compiles client, gcc compiles library (validates ncc as caller)
                    run_library_cross_test(
                        &case.c_file,
                        library_file,
                        &case.output,
                        &mut result,
                        true,
                        case.requires_mathlib,
                    );
                    // Test 2: gcc compiles client, ncc compiles library (validates ncc as callee)
                    run_library_cross_test(
                        &case.c_file,
                        library_file,
                        &case.output,
                        &mut result,
                        false,
                        case.requires_mathlib,
                    );
                }
            } else {
                // Standard test: compile everything with ncc
                run_test(case, &mut result, &[], "");
            }
        }
    }

    result
}

fn run_cases(cases: Vec<TestCase>) {
    use rayon::prelude::*;

    // Evaluate cases in parallel. rayon's global pool bounds total concurrency across all
    // test functions. `collect` preserves input order, so the sequential merge below lists
    // failures deterministically regardless of completion order.
    let per_case: Vec<CaseResult> = cases.par_iter().map(run_one_case).collect();
    let mut tally = CaseResult::default();
    for r in per_case {
        tally.merge(r);
    }

    // takes too long to do this for all cases.
    // On macOS the default linker is already external `ld` (libwild is linux-only), so this
    // case is a redundant no-op there — only exercise it on Linux.
    #[cfg(target_os = "linux")]
    run_test(
        cases.first().unwrap(),
        &mut tally,
        &["--external-linker".to_string()],
        "external-linker",
    );

    assert_eq!(
        tally.failed,
        0,
        "{} of {} sub-tests failed:\n{}",
        tally.failed,
        tally.passed + tally.failed,
        tally
            .failures
            .iter()
            .map(|f| format!("  - {f}"))
            .collect::<Vec<_>>()
            .join("\n")
    );
    assert!(tally.passed > 0, "no test cases ran");
}

#[test]
fn test_all_sandler() {
    let cases = get_sandler_cases();
    run_cases(cases)
}

#[test]
fn test_custom_programs() {
    let cases = get_custom_cases();
    run_cases(cases)
}

#[test]
fn test_label_printing_with_iced() {
    // Test with a file that should generate labels
    let test_file = "writing-a-c-compiler-tests/tests/chapter_6/valid/extra_credit/compound_if_expression.c";

    // Get the path to the ncc binary
    let ncc_path = get_ncc_binary_path();

    // Run ncc with -S flag to get assembly output
    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("-S")
        .output()
        .expect("Failed to execute ncc");

    assert!(output.status.success(), "ncc failed to compile {}", test_file);

    // Convert output to string
    let asm_output = String::from_utf8_lossy(&output.stdout);

    // Check that the output contains labels (labels are not indented and end with :)
    let has_labels = asm_output
        .lines()
        .any(|line| !line.starts_with(char::is_whitespace) && line.ends_with(":"));

    assert!(has_labels, "No label definitions found in assembly output");

    // Check that jump instructions reference labels, not numeric addresses
    let mut found_jump_with_label = false;
    for line in asm_output.lines() {
        let trimmed = line.trim();

        // Check various jump instructions
        if trimmed.starts_with("je ")
            || trimmed.starts_with("jne ")
            || trimmed.starts_with("jmp ")
            || trimmed.starts_with("jg ")
            || trimmed.starts_with("jl ")
            || trimmed.starts_with("jge ")
            || trimmed.starts_with("jle ")
        {
            // Get the jump target
            if let Some(target) = trimmed.split_whitespace().nth(1) {
                // Check if target is a label (contains .L or L prefix) and not just a number
                if !target.chars().all(|c| c.is_numeric() || c == '-') {
                    found_jump_with_label = true;
                }

                // Ensure it's not just a numeric address
                assert!(
                    !target.chars().all(|c| c.is_numeric() || c == '-' || c == 'x'),
                    "Jump instruction '{}' uses numeric address instead of label",
                    trimmed
                );
            }
        }
    }

    assert!(
        found_jump_with_label,
        "No jump instructions with label references found in assembly output"
    );

    // Count the labels for informational purposes (labels are not indented and end with :)
    let label_count = asm_output
        .lines()
        .filter(|line| !line.starts_with(char::is_whitespace) && line.ends_with(":"))
        .count();

    println!(
        "✓ Label printing test passed: found {} labels in assembly output",
        label_count
    );
}

#[test]
fn test_data_symbol_resolution_with_iced() {
    // Test that -S output shows data symbol names for RIP-relative accesses
    let test_file = "writing-a-c-compiler-tests/tests/chapter_10/valid/multiple_static_file_scope_vars.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("-S")
        .output()
        .expect("Failed to execute ncc");

    assert!(output.status.success(), "ncc failed to compile {}", test_file);

    let asm_output = String::from_utf8_lossy(&output.stdout);

    // Check that data symbol names appear in RIP-relative addressing (e.g., "foo(%rip)")
    let has_data_symbol = asm_output
        .lines()
        .any(|line| line.contains("(%rip)") && !line.trim().starts_with('.'));

    assert!(
        has_data_symbol,
        "No data symbol references found in assembly output. Expected symbol(%rip) format.\nOutput:\n{}",
        asm_output
    );

    // Verify the symbol name is used, not just a numeric displacement
    // Look for pattern like "foo(%rip)" where foo is an identifier
    let has_named_symbol = asm_output.lines().any(|line| {
        if let Some(idx) = line.find("(%rip)") {
            // Get the part before (%rip)
            let before = &line[..idx];
            // Check if it ends with an identifier (letters, digits, underscore, dot)
            if let Some(last_word) = before.split_whitespace().last() {
                // Should contain at least one letter (not just numbers like "-8")
                last_word.chars().any(|c| c.is_alphabetic())
            } else {
                false
            }
        } else {
            false
        }
    });

    assert!(
        has_named_symbol,
        "Data references use numeric displacements instead of symbol names.\nOutput:\n{}",
        asm_output
    );

    // Check that static variables are shown in .data or .bss section
    let has_data_section = asm_output.contains(".data") || asm_output.contains(".bss");
    assert!(has_data_section, "No .data or .bss section found in assembly output");

    println!("✓ Data symbol resolution test passed");
}

#[test]
fn test_parser_printing() {
    // Test with a file that should generate labels
    let test_file = "writing-a-c-compiler-tests/tests/chapter_6/valid/extra_credit/compound_if_expression.c";

    // Get the path to the ncc binary
    let ncc_path = get_ncc_binary_path();

    // Run ncc with -S flag to get assembly output
    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--parse")
        .output()
        .expect("Failed to execute ncc");

    assert!(output.status.success(), "ncc failed to compile {}", test_file);

    // Convert output to string
    let asm_output = String::from_utf8_lossy(&output.stdout);

    let check = asm_output.lines().any(|line| line.contains("CompoundAssignment (Add)"));

    assert!(check);
}

#[test]
fn test_wshadow_warning() {
    // Test that WShadow warning is displayed for hidden_variable.c
    let test_file = "writing-a-c-compiler-tests/tests/chapter_7/valid/hidden_variable.c";

    // Get the path to the ncc binary
    let ncc_path = get_ncc_binary_path();

    // Run ncc to compile the file and capture stderr for warnings
    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    // Convert stderr to string to check for warnings
    let stderr_output = String::from_utf8_lossy(&output.stderr);

    // Check that WShadow warning is present
    assert!(
        stderr_output.contains("Wshadow"),
        "Expected WShadow warning for variable shadowing, but stderr was: {}",
        stderr_output
    );

    // Check that it specifically mentions the variable being shadowed
    assert!(
        stderr_output.contains("variable 'a'"),
        "Warning should mention the shadowed variable, but stderr was: {}",
        stderr_output
    );

    println!("✓ WShadow warning test passed: variable shadowing warning detected");
}

fn test_warning(test_file: &str, warning: &str) {
    let path = std::path::Path::new(test_file);
    let binary_path = path.with_extension("");

    // Get the path to the ncc binary
    let ncc_path = get_ncc_binary_path();

    // Run ncc to compile the file and capture stderr for warnings
    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .output()
        .expect("Failed to execute ncc");

    std::fs::remove_file(&binary_path).ok();

    // Convert stderr to string to check for warnings
    let stderr_output = String::from_utf8_lossy(&output.stderr);

    // Check that WShadow warning is present
    assert!(
        stderr_output.contains(warning),
        "Expected {} warning, but stderr was: {}",
        warning,
        stderr_output
    );
}

#[test]
fn test_switch_unreachable() {
    let files = [
        "writing-a-c-compiler-tests/tests/chapter_8/valid/extra_credit/switch_no_case.c",
        "writing-a-c-compiler-tests/tests/chapter_8/valid/extra_credit/switch_nested_cases.c",
    ];

    for file in files {
        test_warning(file, "-Wswitch-unreachable")
    }
}

#[test]
fn test_unused_parameter_warning() {
    let test_file = "tests/c_programs/warnings/unused_parameter.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);

    assert!(
        stderr_output.contains("-Wunused-parameter"),
        "Expected -Wunused-parameter warning, but stderr was: {}",
        stderr_output
    );

    assert!(
        stderr_output.contains("unused parameter 'y'"),
        "Warning should mention the unused parameter 'y', but stderr was: {}",
        stderr_output
    );

    // Ensure we don't warn about 'x' which IS used
    assert!(
        !stderr_output.contains("unused parameter 'x'"),
        "Should not warn about parameter 'x' which is used, but stderr was: {}",
        stderr_output
    );

    println!("✓ Unused parameter warning test passed");
}

#[test]
fn test_no_warning_on_declaration() {
    let test_file = "tests/c_programs/warnings/no_warning_on_declaration.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);

    // Function declarations (no body) should NOT trigger unused parameter warnings
    assert!(
        !stderr_output.contains("-Wunused-parameter"),
        "Declaration-only functions should not trigger unused parameter warnings, but got: {}",
        stderr_output
    );

    println!("✓ No warning on declaration test passed");
}

#[test]
fn test_division_by_zero_warning() {
    let test_file = "tests/c_programs/warnings/division_by_zero.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);

    assert!(
        stderr_output.contains("-Wdiv-by-zero"),
        "Expected -Wdiv-by-zero warning, but stderr was: {}",
        stderr_output
    );

    // Both the `/` and `%` cases should be reported with their distinct wording
    assert!(
        stderr_output.contains("division by zero"),
        "Warning should mention division by zero, but stderr was: {}",
        stderr_output
    );
    assert!(
        stderr_output.contains("remainder by zero"),
        "Warning should mention remainder by zero, but stderr was: {}",
        stderr_output
    );

    println!("✓ Division-by-zero warning test passed");
}

#[test]
fn test_no_division_by_zero_on_nonconstant() {
    let test_file = "tests/c_programs/warnings/no_division_by_zero.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);

    // Non-constant and non-zero constant divisors must not warn
    assert!(
        !stderr_output.contains("-Wdiv-by-zero"),
        "Non-constant / non-zero divisors should not warn, but stderr was: {}",
        stderr_output
    );

    println!("✓ No division-by-zero on non-constant divisor test passed");
}

#[test]
fn test_shift_count_warning() {
    let test_file = "tests/c_programs/warnings/shift_count.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);

    // Both the overflow (count >= width) and negative-count cases should be reported
    assert!(
        stderr_output.contains("-Wshift-count-overflow"),
        "Expected -Wshift-count-overflow warning, but stderr was: {}",
        stderr_output
    );
    assert!(
        stderr_output.contains("-Wshift-count-negative"),
        "Expected -Wshift-count-negative warning, but stderr was: {}",
        stderr_output
    );

    println!("✓ Shift-count warning test passed");
}

#[test]
fn test_no_shift_count_on_valid_shifts() {
    let test_file = "tests/c_programs/warnings/no_shift_count.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);

    // In-range and non-constant shift counts must not warn
    assert!(
        !stderr_output.contains("-Wshift-count"),
        "In-range / non-constant shift counts should not warn, but stderr was: {}",
        stderr_output
    );

    println!("✓ No shift-count warning on valid shifts test passed");
}

#[test]
fn test_div_by_zero_in_constant_context_error() {
    // A constant zero divisor in a context requiring a constant expression is a hard error
    // (exit 30) reported specifically as "division by zero in constant expression", not the
    // generic "not a constant expression" / "not an integer constant expression".
    let test_file = "tests/c_programs/switch/invalid_semantics/case_div_by_zero.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    assert_eq!(
        output.status.code(),
        Some(30),
        "Expected exit code 30 for a div-by-zero case label"
    );

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr_output.contains("division by zero in constant expression"),
        "Expected a specific division-by-zero message, but stderr was: {}",
        stderr_output
    );

    println!("✓ Division-by-zero in constant context error test passed");
}

#[test]
fn test_overflow_warning() {
    // Folds that leave the result type warn with -Woverflow. The `-INT_MIN`, `INT_MIN / -1`,
    // and `INT_MIN % -1` cases in this fixture previously *panicked* the compiler, so asserting a
    // clean exit (not 101) is also the no-panic regression check.
    let test_file = "tests/c_programs/warnings/overflow.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    assert_ne!(
        output.status.code(),
        Some(101),
        "ncc panicked on a constant overflow fold (regression)"
    );
    assert_eq!(output.status.code(), Some(0), "expected --validate to succeed");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr_output.contains("-Woverflow"),
        "Expected -Woverflow warning, but stderr was: {}",
        stderr_output
    );

    println!("✓ Overflow warning + no-panic regression test passed");
}

#[test]
fn test_no_overflow_on_in_range() {
    let test_file = "tests/c_programs/warnings/no_overflow.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr_output.contains("-Woverflow"),
        "In-range constant folds should not warn, but stderr was: {}",
        stderr_output
    );

    println!("✓ No overflow warning on in-range folds test passed");
}

#[test]
fn test_no_overflow_on_unsigned() {
    // Unsigned wraparound is well-defined (modular arithmetic), so -Woverflow must NOT fire,
    // matching gcc/clang. The overflowing_* flag from constant folding is suppressed when the
    // operand type is unsigned; only signed overflow (UB) warns — see test_overflow_warning.
    let test_file = "tests/c_programs/warnings/no_overflow_unsigned.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr_output.contains("-Woverflow"),
        "Unsigned wraparound is defined and must not warn, but stderr was: {}",
        stderr_output
    );

    println!("✓ No overflow warning on unsigned wraparound test passed");
}

#[test]
fn test_constant_conversion_warning() {
    let test_file = "tests/c_programs/warnings/constant_conversion.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr_output.contains("-Wconstant-conversion"),
        "Expected -Wconstant-conversion warning, but stderr was: {}",
        stderr_output
    );

    println!("✓ Constant-conversion warning test passed");
}

#[test]
fn test_no_constant_conversion_on_cast_or_fit() {
    let test_file = "tests/c_programs/warnings/no_constant_conversion.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    // Explicit casts and conversions that don't lose value must not warn
    assert!(
        !stderr_output.contains("-Wconstant-conversion"),
        "Explicit casts / in-range conversions should not warn, but stderr was: {}",
        stderr_output
    );

    println!("✓ No constant-conversion on cast/fit test passed");
}

#[test]
fn test_sequence_point_warning() {
    let test_file = "tests/c_programs/warnings/sequence_point.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr_output.contains("-Wsequence-point"),
        "Expected -Wsequence-point warning, but stderr was: {}",
        stderr_output
    );

    println!("✓ Sequence-point warning test passed");
}

#[test]
fn test_no_sequence_point_on_sequenced() {
    let test_file = "tests/c_programs/warnings/no_sequence_point.c";

    let ncc_path = get_ncc_binary_path();

    let output = std::process::Command::new(&ncc_path)
        .arg(test_file)
        .arg("--validate")
        .output()
        .expect("Failed to execute ncc");

    let stderr_output = String::from_utf8_lossy(&output.stderr);
    // Single modifications and sequenced operators (?:, &&, separate statements) must not warn
    assert!(
        !stderr_output.contains("-Wsequence-point"),
        "Sequenced / single modifications should not warn, but stderr was: {}",
        stderr_output
    );

    println!("✓ No sequence-point on sequenced expressions test passed");
}
