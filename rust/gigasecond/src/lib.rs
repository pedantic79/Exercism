extern crate chrono;
use chrono::{DateTime, Utc, Duration};

// Returns a Utc DateTime one billion seconds after start.
pub fn after(start: DateTime<Utc>) -> DateTime<Utc> {
    let billion = Duration::seconds(1_000_000_000);
    start.checked_add_signed(billion).unwrap()
}
