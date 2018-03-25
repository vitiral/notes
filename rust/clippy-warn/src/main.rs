
/// #REQ-data-family.lint_partof_exists
/// Lint against partofs that do not exist but should (ERROR)
///
/// warns if I don't tick lint_partof_dne, but doesn't warn
/// if I don't tick lint_other_something
pub(crate) fn lint_partof_dne() {
}

fn main() {
    println!("Hello, world!");
}
