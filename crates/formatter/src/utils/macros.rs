#[macro_export]
macro_rules! slice {
    ($code:expr, $start:expr, $end:expr) => {
        &$code[$start as usize..$end as usize]
    };
}
