use std::process::Command;

#[test]
fn reports_the_project_version() -> Result<(), Box<dyn std::error::Error>> {
    let output = Command::new(env!("CARGO_BIN_EXE_project")).output()?;

    assert!(output.status.success());
    assert_eq!(
        String::from_utf8(output.stdout)?,
        format!("project {}\n", env!("CARGO_PKG_VERSION"))
    );

    Ok(())
}
