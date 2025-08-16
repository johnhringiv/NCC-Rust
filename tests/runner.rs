use glob::glob;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

static CHAPTER_COMPLETED: i32 = 8;
static EXTRA_COMPLETED: i32 = 7;

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
        load_expected("writing-a-c-compiler-tests/expected_results.json").unwrap_or_else(|_| HashMap::new());
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
                .strip_prefix("writing-a-c-compiler-tests/tests/")
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
        if let Ok(modified) = fs::metadata(&path).and_then(|m| m.modified())
            && modified > best_time
        {
            best_time = modified;
            best_path = path;
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
    extra_args: &[String],
) {
    let path = std::path::Path::new(&case.c_file);
    let binary_path = path.with_extension("");
    let binary_path_str = binary_path.to_str().unwrap();

    let ncc_path = get_ncc_binary_path();
    let mut cmd = std::process::Command::new(&ncc_path);
    cmd.arg(case.c_file.clone()).arg("-o").arg(binary_path_str);

    for arg in extra_args {
        cmd.arg(arg);
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

    for case in &cases {
        match case.output {
            ProgramOutput::Error(_) => run_test(case, &mut total_passed, &mut total_failed, &mut failed_tests, &[]),
            ProgramOutput::Result(_) => {
                run_test(case, &mut total_passed, &mut total_failed, &mut failed_tests, &[]);
                run_test(
                    case,
                    &mut total_passed,
                    &mut total_failed,
                    &mut failed_tests,
                    &["--no-iced".to_string()],
                );
            }
        }
    }
    // takes too long to do this for all cases
    run_test(
        cases.first().unwrap(),
        &mut total_passed,
        &mut total_failed,
        &mut failed_tests,
        &["--gcc".to_string()],
    );

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
    assert_eq!(total_failed, 0);
    assert!(total_passed > 0)
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

    let check = asm_output
        .lines()
        .any(|line| line.contains("CompoundAssignment (Add)"));

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
