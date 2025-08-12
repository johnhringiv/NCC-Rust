use glob::glob;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

static CHAPTER_COMPLETED: i32 = 6;
static EXTRA_COMPLETED: i32 = 6;

#[derive(Debug, PartialEq)]
enum ProgramOutput {
    Error(i32),
    Result(i32),
}

#[derive(Debug)]
struct TestCase {
    c_file: String,
    output: ProgramOutput,
}

// we want a mapping of test path to program output
fn get_sandler_cases() -> Vec<TestCase> {
    // Load expected results for valid tests
    let expected_results =
        load_expected("../writing-a-c-compiler-tests/expected_results.json").unwrap_or_else(|_| HashMap::new());
    let mut cases = vec![];

    for entry in glob("../writing-a-c-compiler-tests/tests/**/*.c")
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

        // Determine the ProgramOutput based on the test type
        let output = if path_str.contains("invalid_lex") {
            ProgramOutput::Error(10)
        } else if path_str.contains("invalid_parse") {
            ProgramOutput::Error(20)
        } else if path_str.contains("invalid_semantics") {
            ProgramOutput::Error(30)
        } else {
            // Valid test - get expected return code from map
            let relative_path = path_str
                .strip_prefix("../writing-a-c-compiler-tests/tests/")
                .unwrap_or(path_str);
            let expected_value = *expected_results.get(relative_path).unwrap_or(&0);
            ProgramOutput::Result(expected_value)
        };

        if chapter <= CHAPTER_COMPLETED && (!extra_credit || (chapter <= EXTRA_COMPLETED)) {
            cases.push(TestCase {
                c_file: path_str.to_string(),
                output,
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
            let expected_value = *expected_results.get(relative_path).unwrap_or(&0);
            ProgramOutput::Result(expected_value)
        };

        cases.push(TestCase {
            c_file: path_str.to_string(),
            output,
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
        if let Ok(metadata) = fs::metadata(&path) {
            if let Ok(modified) = metadata.modified() {
                if modified > best_time {
                    best_time = modified;
                    best_path = path;
                }
            }
        }
    }

    best_path
}

fn load_expected(json_path: &str) -> Result<HashMap<String, i32>, Box<dyn std::error::Error>> {
    let json_content = fs::read_to_string(json_path)?;

    let parsed: serde_json::Value = serde_json::from_str(&json_content)?;

    let mut results = HashMap::new();
    if let serde_json::Value::Object(obj) = parsed {
        for (file_path, value) in obj {
            if let Some(return_code) = value.get("return_code").and_then(|v| v.as_i64()) {
                results.insert(file_path, return_code as i32);
            }
        }
    }
    Ok(results)
}

fn run_test(
    case: &TestCase,
    total_passed: &mut usize,
    total_failed: &mut usize,
    failed_tests: &mut Vec<String>,
    iced: bool,
) {
    let path = std::path::Path::new(&case.c_file);
    let binary_path = path.with_extension("");
    let binary_path_str = binary_path.to_str().unwrap();

    let ncc_path = get_ncc_binary_path();
    let mut cmd = std::process::Command::new(&ncc_path);
    cmd.arg(case.c_file.clone()).arg("-o").arg(binary_path_str);

    if !iced {
        cmd.arg("--no-iced");
    }

    let compile_output = cmd.output().unwrap();

    let actual = if compile_output.status.success() {
        // compilation succeed
        let run_output = std::process::Command::new(binary_path_str).output().unwrap();

        let return_code = run_output.status.code().unwrap_or(-1);
        std::fs::remove_file(&binary_path).ok();
        ProgramOutput::Result(return_code)
    } else {
        ProgramOutput::Error(compile_output.status.code().unwrap_or(-1))
    };

    if actual == case.output {
        *total_passed += 1;
    } else {
        *total_failed += 1;
        failed_tests.push(format!(
            "{} (expected {:?}, got {:?})",
            case.c_file, case.output, actual
        ));
    }
}

fn run_cases(cases: Vec<TestCase>) {
    let mut total_passed = 0;
    let mut total_failed = 0;
    let mut failed_tests = Vec::new();

    for case in cases {
        match case.output {
            ProgramOutput::Error(_) => run_test(&case, &mut total_passed, &mut total_failed, &mut failed_tests, true),
            ProgramOutput::Result(_) => {
                run_test(&case, &mut total_passed, &mut total_failed, &mut failed_tests, true);
                run_test(&case, &mut total_passed, &mut total_failed, &mut failed_tests, false);
            }
        }
    }

    println!("\n=== C Programs Test Results ===");
    println!("Passed: {total_passed}");
    println!("Failed: {total_failed}");

    if !failed_tests.is_empty() {
        println!("\nFailed tests:");
        for test in &failed_tests {
            println!("  - {test}");
        }
    }

    if total_failed > 0 {
        println!("{total_failed} tests failed")
    }
    assert_eq!(total_failed, 0)
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
